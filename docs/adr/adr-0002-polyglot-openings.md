
### PR Description

**Title:** `feat: Add PolyGlot opening book reader and Zobrist key generator`

**Summary:**
This PR introduces full support for PolyGlot `.bin` opening books. It implements the standard PolyGlot pseudo-random number generator (PRNG) to generate the 781 64-bit Zobrist keys dynamically at runtime, computes position hashes according to the PolyGlot specification, and handles 16-byte Big-Endian binary file lookups.

**Changes Included:**

1. **Zobrist Key Initialization:** Implements PolyGlot's PRNG (`seed * 2862933555777941757 + 3037000493`) to generate the deterministic 781-element hash table.
2. **PolyGlot Hash Computation:** Hashes piece positions, castling rights, en-passant files (only when legal pawn captures exist), and turn state.
3. **Binary File Parser:** Opens `.bin` files in unformatted direct stream access mode and converts Big-Endian integers to native Fortran layout.
4. **Move Lookup API:** Returns a list of candidate moves with weights/learn counts for a given board state.

---

### Implementation Code

Create a new file `PolyglotM.f90`:

```fortran
module PolyglotM
    use iso_fortran_env, only: int8, int16, int32, int64, real64
    implicit none
    private

    public :: polyglot_init
    public :: polyglot_get_key
    public :: polyglot_entry_t
    public :: polyglot_find_moves

    ! PolyGlot entry representation matching the 16-byte record layout
    type :: polyglot_entry_t
        integer(kind=int64) :: key
        integer(kind=int16) :: move
        integer(kind=int16) :: weight
        integer(kind=int32) :: learn
        ! Unpacked move details
        integer :: from_sq
        integer :: to_sq
        integer :: promotion  ! 0: None, 1: Knight, 2: Bishop, 3: Rook, 4: Queen
    end type polyglot_entry_t

    ! Standard PolyGlot Zobrist Keys Array (781 values)
    integer(kind=int64), save :: polyglot_keys(0:780)
    logical, save :: is_initialized = .false.

    ! Piece indexing mapping: Polyglot expects 0..11
    ! 0: BP, 1: WP, 2: BN, 3: WN, 4: BB, 5: WB, 6: BR, 7: WR, 8: BQ, 9: WQ, 10: BK, 11: WK
    integer, parameter :: PG_BP = 0, PG_WP = 1
    integer, parameter :: PG_BN = 2, PG_WN = 3
    integer, parameter :: PG_BB = 4, PG_WB = 5
    integer, parameter :: PG_BR = 6, PG_WR = 7
    integer, parameter :: PG_BQ = 8, PG_WQ = 9
    integer, parameter :: PG_BK = 10, PG_WK = 11

contains

    !! Initializes the 781 Zobrist keys using the standard PolyGlot LCG PRNG
    subroutine polyglot_init()
        integer(kind=int64) :: seed
        integer :: i
        
        if (is_initialized) return

        ! PolyGlot initial seed = 0x123456789ABCDEF1
        seed = int(z'123456789ABCDEF1', int64)

        do i = 0, 780
            ! LCG sequence: seed = seed * 2862933555777941757 + 3037000493
            seed = seed * int(2862933555777941757_int64, int64) + int(3037000493_int64, int64)
            polyglot_keys(i) = seed
        end do

        is_initialized = .true.
    end subroutine polyglot_init

    !! Computes PolyGlot Zobrist Key for a given position
    !! Parameters:
    !!   board: 8x8 array containing piece codes (0 if empty, or PG_* constants)
    !!   castling: logical array [White Short, White Long, Black Short, Black Long]
    !!   ep_file: En passant target file (1..8 / A..H), 0 if no legal EP target exists
    !!   is_white_turn: .true. if White is to move
    function polyglot_get_key(board, castling, ep_file, is_white_turn) result(key)
        integer, intent(in) :: board(0:63)         ! Board array indexed 0 (A1) to 63 (H8)
        logical, intent(in) :: castling(4)        ! [K, Q, k, q]
        integer, intent(in) :: ep_file            ! 1..8 for A..H, 0 if no legal capture
        logical, intent(in) :: is_white_turn
        integer(kind=int64) :: key
        integer :: sq, piece_type, idx

        if (.not. is_initialized) call polyglot_init()

        key = 0_int64

        ! 1. Board Pieces (0 .. 767)
        do sq = 0, 63
            piece_type = board(sq)
            if (piece_type >= 0 .and. piece_type <= 11) then
                idx = 64 * piece_type + sq
                key = ieor(key, polyglot_keys(idx))
            end if
        end do

        ! 2. Castling Rights (768 .. 771)
        if (castling(1)) key = ieor(key, polyglot_keys(768)) ! White Kingside
        if (castling(2)) key = ieor(key, polyglot_keys(769)) ! White Queenside
        if (castling(3)) key = ieor(key, polyglot_keys(770)) ! Black Kingside
        if (castling(4)) key = ieor(key, polyglot_keys(771)) ! Black Queenside

        ! 3. En Passant File (772 .. 779)
        ! Note: Must only be applied if an EN PASSANT capture is legally possible!
        if (ep_file >= 1 .and. ep_file <= 8) then
            key = ieor(key, polyglot_keys(772 + (ep_file - 1)))
        end if

        ! 4. Side to Move (780) - PolyGlot XORs when White is to move
        if (is_white_turn) then
            key = ieor(key, polyglot_keys(780))
        end if

    end function polyglot_get_key

    !! Searches a PolyGlot binary book file for moves matching the target key
    subroutine polyglot_find_moves(filename, search_key, entries, num_found)
        character(len=*), intent(in) :: filename
        integer(kind=int64), intent(in) :: search_key
        type(polyglot_entry_t), intent(out) :: entries(:)
        integer, intent(out) :: num_found

        integer :: u, io_stat
        integer(kind=int64) :: file_size, num_records, low, high, mid
        integer(kind=int64) :: current_key
        character(len=16) :: raw_record
        type(polyglot_entry_t) :: record

        num_found = 0
        
        open(newunit=u, file=filename, access='stream', status='old', &
             action='read', iostat=io_stat)
        if (io_stat /= 0) return

        ! Determine number of 16-byte entries in book
        inquire(unit=u, size=file_size)
        num_records = file_size / 16

        if (num_records == 0) then
            close(u)
            return
        end if

        ! Binary Search for the key
        low = 1
        high = num_records
        mid = -1

        do while (low <= high)
            mid = (low + high) / 2
            read(unit=u, pos=(mid - 1)*16 + 1) raw_record
            record = unpack_entry(raw_record)

            ! Compare Unsigned 64-bit Keys
            if (record%key == search_key) then
                exit
            else if (compare_unsigned_64(record%key, search_key) < 0) then
                low = mid + 1
            else
                high = mid - 1
            end if
        end do

        ! If entry found, rewind to first matching entry (PolyGlot allows multiple moves per key)
        if (low <= high) then
            do while (mid > 1)
                read(unit=u, pos=(mid - 2)*16 + 1) raw_record
                current_key = unpack_key(raw_record)
                if (current_key /= search_key) exit
                mid = mid - 1
            end do

            ! Collect all matching entries
            do while (mid <= num_records)
                read(unit=u, pos=(mid - 1)*16 + 1) raw_record
                record = unpack_entry(raw_record)
                if (record%key /= search_key) exit
                
                num_found = num_found + 1
                if (num_found <= size(entries)) then
                    entries(num_found) = record
                end if
                mid = mid + 1
            end do
        end if

        close(u)
    end subroutine polyglot_find_moves

    !! Unpacks a 16-byte Big-Endian buffer into a polyglot_entry_t struct
    function unpack_entry(buffer) result(entry)
        character(len=16), intent(in) :: buffer
        type(polyglot_entry_t) :: entry

        entry%key = unpack_key(buffer)
        entry%move = int(ior(ishft(iand(ichar(buffer(9:9)), 255), 8), &
                             iand(ichar(buffer(10:10)), 255)), int16)
        entry%weight = int(ior(ishft(iand(ichar(buffer(11:11)), 255), 8), &
                               iand(ichar(buffer(12:12)), 255)), int16)
        entry%learn = int(ior(ishft(iand(int(ichar(buffer(13:13)), int32), 255), 24), &
                          ior(ishft(iand(int(ichar(buffer(14:14)), int32), 255), 16), &
                          ior(ishft(iand(int(ichar(buffer(15:15)), int32), 255), 8), &
                              iand(int(ichar(buffer(16:16)), int32), 255)))), int32)

        ! Unpack move components
        entry%to_sq = ibits(entry%move, 0, 6)
        entry%from_sq = ibits(entry%move, 6, 6)
        entry%promotion = ibits(entry%move, 12, 3)
    end function unpack_entry

    !! Unpacks 8-byte Big-Endian Key
    function unpack_key(buffer) result(key)
        character(len=16), intent(in) :: buffer
        integer(kind=int64) :: key
        integer :: i

        key = 0_int64
        do i = 1, 8
            key = ior(ishft(key, 8), int(iand(ichar(buffer(i:i)), 255), int64))
        end do
    end function unpack_key

    !! Performs Unsigned 64-bit integer comparison (Returns -1 if a < b, 0 if equal, 1 if a > b)
    function compare_unsigned_64(a, b) result(res)
        integer(kind=int64), intent(in) :: a, b
        integer :: res
        
        if (a == b) then
            res = 0
        else if (ieor(a, int(z'8000000000000000', int64)) < ieor(b, int(z'8000000000000000', int64))) then
            res = -1
        else
            res = 1
        end if
    end function compare_unsigned_64

end module PolyglotM

```

---

### Example Unit Test / Usage Driver

`tests/test_polyglot.f90`:

```fortran
program test_polyglot
    use PolyglotM
    use iso_fortran_env, only: int64
    implicit none

    integer :: board(0:63)
    logical :: castling(4)
    integer :: ep_file
    logical :: is_white
    integer(kind=int64) :: key
    type(polyglot_entry_t) :: matches(10)
    integer :: num_found, i

    ! 1. Initialize PolyGlot Keys
    call polyglot_init()

    ! 2. Setup Standard Start Position Board
    board = -1 ! Empty
    
    ! White pieces (A1..H1 = 0..7, A2..H2 = 8..15)
    board(0) = 7; board(1) = 3; board(2) = 5; board(3) = 9
    board(4) = 11; board(5) = 5; board(6) = 3; board(7) = 7
    board(8:15) = 1

    ! Black pieces (A7..H7 = 48..55, A8..H8 = 56..63)
    board(48:55) = 0
    board(56) = 6; board(57) = 2; board(58) = 4; board(59) = 8
    board(60) = 10; board(61) = 4; board(62) = 2; board(63) = 6

    castling = [.true., .true., .true., .true.]
    ep_file = 0
    is_white = .true.

    ! 3. Compute Hash Key
    key = polyglot_get_key(board, castling, ep_file, is_white)
    
    ! Expected Start Position Key: 0x463B961EA6D7420D
    write(*, '(A, Z16.16)') "Start Position Key: 0x", key

    ! 4. Search Opening Book (e.g., Performance.bin)
    call polyglot_find_moves("Performance.bin", key, matches, num_found)
    
    write(*, '(A, I0)') "Book moves found: ", num_found
    do i = 1, min(num_found, size(matches))
        write(*, '(A, I2, A, I2, A, I1, A, I5)') &
            "Move: Sq ", matches(i)%from_sq, " -> Sq ", matches(i)%to_sq, &
            " | Promo: ", matches(i)%promotion, " | Weight: ", matches(i)%weight
    end do

end program test_polyglot

```
