! ruhe — A UCI chess engine written in Fortran.
! Copyright (C) 2026 Michael White
!
! This program is free software: you can redistribute it and/or modify
! it under the terms of the GNU General Public License as published by
! the Free Software Foundation, either version 3 of the License, or
! (at your option) any later version.
!
! This program is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
! GNU General Public License for more details.
!
! You should have received a copy of the GNU General Public License
! along with this program.  If not, see <https://www.gnu.org/licenses/>.
!-
!! @brief basic chessboard actions on piece-type--color bitboards
module BaseM

  use StringUtilsM
  use PieceM, only: PCK, EMPTY, COLOR_MASK
  use SquareM, only: SQK, SQ_NONE, SQUARE_MASK
  use BitboardM, only: BBK, BB_EMPTY
  use EvaluationM, only: EVK
  implicit none
  private

  !! ~ python-chess routines
  public :: &
    attacks_mask, &        !! 9
    attackers_mask, &      !! 10
    base_fen, &            !! 16
    clear_base, &          !!     (clear)
    color_at, &            !! 7
    init_base, &           !! 1
    is_attacked_by, &      !! 11
    is_pinned, &           !! 13
    king_sq, &             !! 8
    piece_at, &            !! 5
    piece_type_at, &       !! 6
    pieces_mask, &         !! 4
    pin_mask, &            !! 12
    remove_piece_at, &     !! 14
    reset_base, &          !! 2   (reset)
    set_base_fen, &        !! 17
    set_piece_at           !! 15

  public :: &
    attackers_xray_mask, &
    dump_base, &
    full_score, &
    score_pawn_structure, &
    get_mailbox_array, &
    get_pawn_attacks, &
    get_mailbox_str, &
    is_square_attacked, &
    pinner, &
    print_mailbox

  !! @brief piece bitmasks Least-Endian-File-Rank LEFR
  type, public :: BaseT

    integer(BBK), dimension(0:COLOR_MASK) :: occupied_co = BB_EMPTY
    integer(BBK), dimension(0:COLOR_MASK) :: pawns_co = BB_EMPTY
    integer(BBK), dimension(0:COLOR_MASK) :: knights_co = BB_EMPTY
    integer(BBK), dimension(0:COLOR_MASK) :: bishops_co = BB_EMPTY
    integer(BBK), dimension(0:COLOR_MASK) :: rooks_co = BB_EMPTY
    integer(BBK), dimension(0:COLOR_MASK) :: queens_co = BB_EMPTY
    integer(BBK), dimension(0:COLOR_MASK) :: kings_co = BB_EMPTY

    ! --- Incremental Evaluation (Tapered) ---
    integer(EVK) :: score_mg !! Running Midgame score (White POV)
    integer(EVK) :: score_eg !! Running Endgame score (White POV)
    integer(EVK) :: phase !! Total material value (24 -> 0)
    integer(EVK) :: score !! Final tapered sum (Midgame + EG_Boost)
  contains
    procedure :: attacks_mask
    procedure :: attackers_mask
    procedure :: base_fen
    procedure :: clear_base
    procedure :: color_at
    procedure :: init_base
    procedure :: is_attacked_by
    procedure :: is_pinned
    procedure :: is_square_attacked
    procedure :: king_sq
    procedure :: piece_at
    procedure :: piece_type_at
    procedure :: pieces_mask
    procedure :: pin_mask
    procedure :: remove_piece_at
    procedure :: reset_base
    procedure :: set_base_fen
    procedure :: set_piece_at

    procedure :: get_pawn_attacks
    procedure :: print_mailbox
  end type BaseT

  integer, parameter, public :: &
    MAX_EPD = 20, &
    MAX_PARAM_LEN = 10, &
    MAX_ARG_LEN = 100
  character(len=MAX_PARAM_LEN), dimension(MAX_EPD), public :: epd_params
  character(len=MAX_ARG_LEN), dimension(MAX_EPD), public :: epd_args

  character(len=43), parameter, public :: &
    STARTING_BOARD_FEN = 'rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR'

  integer, parameter, public :: &
    ERR_NONE = 0, &
    ERR_FEN_EXPECTED_SEPARATOR = 1, &
    ERR_FEN_INVALID_SYNTAX = 2, &
    ERR_FEN_EXTRA_CHARS = 3

contains

  !! @brief squares attacked by given square
  pure function attacks_mask(self, sq) result(mask)

    use PieceM, only: BLACK, WHITE
    use SquareM, only: V_E, V_N, V_S, V_W, V_NE, V_NW, V_SE, V_SW
    use BitboardM, only: get_ray_attacks, &
              BB_SQUARES, BB_PAWN_ATTACKS_CO, BB_KING_ATTACKS, BB_KNIGHT_ATTACKS

    class(BaseT), intent(in) :: self
    integer(SQK), intent(in) :: sq
    integer(BBK) :: mask

    integer(BBK) :: bb_sq, occ
    integer(PCK) :: co

    bb_sq = BB_SQUARES(sq)
    mask = BB_EMPTY
    occ = ior(self%occupied_co(WHITE), self%occupied_co(BLACK))

    !! handle empty square (no attacks)
    if (iand(bb_sq, occ) == BB_EMPTY) return

    !! branchless color
    co = merge(WHITE, BLACK, btest(self%occupied_co(WHITE), sq))

    if (iand(bb_sq, self%pawns_co(co)) /= BB_EMPTY) then
      mask = BB_PAWN_ATTACKS_CO(co, sq)

    else if (iand(bb_sq, self%knights_co(co)) /= BB_EMPTY) then
      mask = BB_KNIGHT_ATTACKS(sq)

    else if (iand(bb_sq, self%kings_co(co)) /= BB_EMPTY) then
      mask = BB_KING_ATTACKS(sq)

    else
      if (iand(bb_sq, ior(self%bishops_co(co), &
                          self%queens_co(co))) /= BB_EMPTY) then
        mask = ior(mask, &
                   ior(get_ray_attacks(sq, V_NE, occ), &
                       ior(get_ray_attacks(sq, V_NW, occ), &
                           ior(get_ray_attacks(sq, V_SW, occ), &
                               get_ray_attacks(sq, V_SE, occ)))))
      end if
      if (iand(bb_sq, ior(self%rooks_co(co), &
                          self%queens_co(co))) /= BB_EMPTY) then
        mask = ior(mask, &
                   ior(get_ray_attacks(sq, V_N, occ), &
                       ior(get_ray_attacks(sq, V_S, occ), &
                           ior(get_ray_attacks(sq, V_E, occ), &
                               get_ray_attacks(sq, V_W, occ)))))
      end if
    end if
  end function attacks_mask

  !! @brief attackers by by_co, onto given sq, optional occ
  pure function attackers_mask(self, sq, by_co, occ_opt) result(mask)

    use PieceM, only: WHITE, BLACK, XCOLOR
    use SquareM, only: &
      V_N, V_E, V_W, V_S, &
      V_NE, V_SE, V_SW, V_NW
    use BitboardM, only: get_ray_attacks, &
                         BB_PAWN_ATTACKS_CO, BB_KNIGHT_ATTACKS, BB_KING_ATTACKS

    class(BaseT), intent(in) :: self
    integer(PCK), intent(in) :: by_co
    integer(SQK), intent(in) :: sq
    integer(BBK), intent(in), optional :: occ_opt
    integer(BBK) :: mask

    integer(BBK) :: t_occ, sliders

    mask = BB_EMPTY

    if (present(occ_opt)) then
      t_occ = occ_opt
    else
      t_occ = ior(self%occupied_co(WHITE), self%occupied_co(BLACK))
    end if

    !! leaps; pawns from another color, knights, kings
    mask = &
      ior(iand(self%pawns_co(by_co), BB_PAWN_ATTACKS_CO(XCOLOR(by_co), sq)), &
          ior(iand(self%knights_co(by_co), BB_KNIGHT_ATTACKS(sq)), &
              iand(self%kings_co(by_co), BB_KING_ATTACKS(sq))))

    ! diagonal sliders
    sliders = ior(self%bishops_co(by_co), self%queens_co(by_co))
    if (sliders /= BB_EMPTY) then
      mask = ior(mask, &
                 iand(sliders, &
                      ior(get_ray_attacks(sq, V_NE, t_occ), &
                          ior(get_ray_attacks(sq, V_NW, t_occ), &
                              ior(get_ray_attacks(sq, V_SW, t_occ), &
                                  get_ray_attacks(sq, V_SE, t_occ))))))
    end if

    ! cardinal sliders
    sliders = ior(self%rooks_co(by_co), self%queens_co(by_co))
    if (sliders /= BB_EMPTY) then
      mask = ior(mask, &
                 iand(sliders, &
                      ior(get_ray_attacks(sq, V_N, t_occ), &
                          ior(get_ray_attacks(sq, V_S, t_occ), &
                              ior(get_ray_attacks(sq, V_E, t_occ), &
                                  get_ray_attacks(sq, V_W, t_occ))))))
    end if
  end function attackers_mask

  !! @brief fen board str, e.g. 'rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR'
  pure function base_fen(self) result(str)

    use PieceM, only: PIECE_CHAR, PIECE_LAST
    use SquareM, only: V_E, V_S, FILE_A, FILE_H, RANK_1_OFFSET, RANK_8_OFFSET

    class(BaseT), intent(in) :: self
    character(len=64 + 7) :: str
    integer(SQK) :: file, rank_offset, sq
    integer(PCK) :: pc
    integer :: nempty
    character(len=1), dimension(1:8), parameter :: &
      digit_str = ['1', '2', '3', '4', '5', '6', '7', '8']

    str = ''
    do rank_offset = RANK_8_OFFSET, RANK_1_OFFSET, V_S
      nempty = 0
      do file = FILE_A, FILE_H, V_E
        sq = rank_offset + file
        pc = piece_at(self, sq)
        if (pc == EMPTY) then
          nempty = nempty + 1
        else
          if (nempty > 0) str = trim(str)//digit_str(nempty)
          if (pc < 0 .or. pc > PIECE_LAST) then
            str = trim(str)//"#"
          else
            str = trim(str)//PIECE_CHAR(pc)
          end if
          nempty = 0
        end if
      end do
      if (nempty > 0) str = trim(str)//digit_str(nempty)
      if (rank_offset /= RANK_1_OFFSET) str = trim(str)//'/'
    end do
  end function base_fen

  !! @brief reset to the starting position
  subroutine clear_base(self)

    class(BaseT), intent(inout) :: self

    self%pawns_co = BB_EMPTY
    self%knights_co = BB_EMPTY
    self%bishops_co = BB_EMPTY
    self%rooks_co = BB_EMPTY
    self%queens_co = BB_EMPTY
    self%kings_co = BB_EMPTY
    self%occupied_co = BB_EMPTY

    call full_score(self, self%score, self%phase, self%score_mg)

  end subroutine clear_base

  !! @brief piece color at the given sq
  pure function color_at(self, sq) result(co)

    use PieceM, only: PCK, WHITE, BLACK, PC_NONE
    use BitboardM, only: BB_SQUARES

    class(BaseT), intent(in) :: self
    integer(PCK) :: co

    integer(SQK), intent(in) :: sq
    integer(BBK) :: mask

    mask = BB_SQUARES(sq)
    if (iand(self%occupied_co(WHITE), mask) /= BB_EMPTY) then
      co = WHITE
    else if (iand(self%occupied_co(BLACK), mask) /= BB_EMPTY) then
      co = BLACK
    else
      co = PC_NONE
    end if
  end function color_at

  !! @brief initialize the base
  subroutine init_base(self, base_fen_option)

    class(BaseT), intent(inout) :: self
    character(len=100), intent(in), optional :: base_fen_option

    character(len=100) :: t_fen
    integer :: status

    self%occupied_co = [BB_EMPTY, BB_EMPTY]
    t_fen = STARTING_BOARD_FEN
    if (present(base_fen_option)) t_fen = base_fen_option
    if (t_fen == "") then
      call self%clear_base()
    else if (t_fen == STARTING_BOARD_FEN) then
      call self%reset_base()
    else
      call self%set_base_fen(trim(t_fen), status)
      if (status /= ERR_NONE) then
        print *, 'Error: set_base_fen, status = ', status
      end if
    end if
  end subroutine init_base

  subroutine dump_base(base)

    use PieceM, only: WHITE, BLACK
    use BitboardM, only: print_stamp

    type(BaseT), intent(inout) :: base

    print *, "occupied_co(WHITE):"
    call print_stamp(base%occupied_co(WHITE))

    print *, "pawns_co(WHITE):"
    call print_stamp(base%pawns_co(WHITE))
    print *, "knights_co(WHITE):"
    call print_stamp(base%knights_co(WHITE))
    print *, "bishops_co(WHITE):"
    call print_stamp(base%bishops_co(WHITE))
    print *, "rooks_co(WHITE):"
    call print_stamp(base%rooks_co(WHITE))
    print *, "queens_co(WHITE):"
    call print_stamp(base%queens_co(WHITE))
    print *, "kings_co(WHITE):"
    call print_stamp(base%kings_co(WHITE))

    print *, "occupied_co(BLACK):"
    call print_stamp(base%occupied_co(BLACK))

    print *, "pawns_co(BLACK):"
    call print_stamp(base%pawns_co(BLACK))
    print *, "knights_co(BLACK):"
    call print_stamp(base%knights_co(BLACK))
    print *, "bishops_co(BLACK):"
    call print_stamp(base%bishops_co(BLACK))
    print *, "rooks_co(BLACK):"
    call print_stamp(base%rooks_co(BLACK))
    print *, "queens_co(BLACK):"
    call print_stamp(base%queens_co(BLACK))
    print *, "kings_co(BLACK):"
    call print_stamp(base%kings_co(BLACK))
  end subroutine dump_base

  !! @brief t/f whether given co attacks given sq, with optional occ
  pure function is_attacked_by(self, sq, co, occ_opt) result(tf)

    use PieceM, only: PCK, BLACK, WHITE
    use SquareM, only: SQK
    use BitboardM, only: BBK

    class(BaseT), intent(in) :: self
    integer(SQK), intent(in) :: sq
    integer(PCK), intent(in) :: co
    integer(BBK), intent(in), optional :: occ_opt
    logical :: tf

    integer(BBK) :: occ

    if (present(occ_opt)) then
      occ = occ_opt
    else
      occ = ior(self%occupied_co(WHITE), self%occupied_co(BLACK))
    end if

    tf = (self%attackers_mask(sq, co, occ) /= BB_EMPTY)
  end function is_attacked_by

  !! @brief whether given square is pinned to king of given color
#ifndef DEBUG
  pure &
#endif
    function is_pinned(self, co, sq) result(tf)

    use BitboardM, only: BB_ALL

    class(BaseT), intent(in) :: self
    integer(PCK), intent(in) :: co
    integer(SQK), intent(in) :: sq
    logical :: tf

    tf = (self%pin_mask(co, sq) /= BB_ALL)
  end function is_pinned

  !! @brief determine king_sq given co
#ifndef DEBUG
  pure &
#endif
    function king_sq(self, co) result(sq)

    use BitboardM, only: lsb

    class(BaseT), intent(in) :: self
    integer(PCK), intent(in) :: co
    integer(SQK) :: sq

#ifdef DEBUG
    if (self%kings_co(co) == BB_EMPTY) &
      error stop "Error: the king is missing!"
#endif
    sq = lsb(self%kings_co(co))
  end function king_sq

  !! @brief piece given square
  pure function piece_at(self, sq) result(pc)

    use PieceM, only: PCK, WHITE, BLACK
    use SquareM, only: SQK
    use BitboardM, only: BB_SQUARES

    class(BaseT), intent(in) :: self
    integer(SQK), intent(in) :: sq
    integer(PCK) :: pc

    integer(PCK) :: co
    integer(BBK) :: occ

    occ = ior(self%occupied_co(WHITE), self%occupied_co(BLACK))
    if (iand(occ, BB_SQUARES(sq)) == BB_EMPTY) then
      pc = EMPTY
      return
    end if

    if (iand(self%occupied_co(WHITE), BB_SQUARES(sq)) /= BB_EMPTY) then
      co = WHITE
    else
      co = BLACK
    end if
    pc = ior(co, piece_type_at(self, sq))
  end function piece_at

  !! @brief piece type at given square
  pure function piece_type_at(self, sq) result(ty)

    use PieceM, only: PC_NONE, PAWN, KNIGHT, BISHOP, ROOK, QUEEN, KING
    use BitboardM, only: BB_SQUARES

    class(BaseT), intent(in) :: self
    integer(SQK), intent(in) :: sq
    integer(PCK) :: ty

    integer(PCK) :: co
    integer(BBK) :: mask

    co = color_at(self, sq)
    if (co == PC_NONE) then
      ty = EMPTY
      return
    end if

    mask = BB_SQUARES(sq)
    if (iand(self%pawns_co(co), mask) /= BB_EMPTY) then
      ty = PAWN
    else if (iand(self%knights_co(co), mask) /= BB_EMPTY) then
      ty = KNIGHT
    else if (iand(self%bishops_co(co), mask) /= BB_EMPTY) then
      ty = BISHOP
    else if (iand(self%rooks_co(co), mask) /= BB_EMPTY) then
      ty = ROOK
    else if (iand(self%queens_co(co), mask) /= BB_EMPTY) then
      ty = QUEEN
    else if (iand(self%kings_co(co), mask) /= BB_EMPTY) then
      ty = KING
    else
      ty = PC_NONE
    end if
  end function piece_type_at

  !! @brief bitmask of given piece type and color
  pure function pieces_mask(self, ty, co) result(mask)

    use PieceM, only: PCK, PIECE_TYPE, &
                      PAWN, KNIGHT, BISHOP, ROOK, QUEEN, KING
    use BitboardM, only: BBK, BB_ALL

    class(BaseT), intent(in) :: self
    integer(PCK), intent(in) :: ty, co
    integer(BBK) :: mask

    integer(PCK) :: pc

    pc = ty + co

    select case (PIECE_TYPE(pc))
    case (PAWN)
      mask = self%pawns_co(pc)
    case (KNIGHT)
      mask = self%knights_co(pc)
    case (BISHOP)
      mask = self%bishops_co(pc)
    case (ROOK)
      mask = self%rooks_co(pc)
    case (QUEEN)
      mask = self%queens_co(pc)
    case (KING)
      mask = self%kings_co(pc)
    case default
      mask = BB_ALL
    end select
  end function pieces_mask

  !! @brief pinned single friendly piece between attacker and xco king
#ifndef DEBUG
  pure &
#endif
    function pin_mask(self, co, sq) result(mask)

    use PieceM, only: PCK, XCOLOR, BLACK, WHITE
    use SquareM, only: SQK, V_E, V_N, V_NE, V_NW
    use BitboardM, only: &
      BB_ALL, BB_BETWEEN, BB_LINE, BB_SQUARES, BB_VECTOR, msb

    class(BaseT), intent(in) :: self
    integer(PCK), intent(in) :: co
    integer(SQK), intent(in) :: sq
    integer(BBK) :: mask

    integer(PCK) :: xco
    integer(BBK) :: line_snipers, rays, snipers
    integer(SQK) :: k_sq, pinner_sq, v
    integer :: i

    !! our (co) king square
    k_sq = king_sq(self, co)
    if (k_sq == SQ_NONE .or. sq == k_sq) then ! king missing or king himself?
      mask = BB_ALL
      return
    end if

    !! king-to-sq alignment
    v = BB_VECTOR(k_sq, sq)
    if (v == BB_EMPTY) then
      mask = BB_ALL
      return
    end if

    !! other team
    xco = XCOLOR(co)

    do i = 1, 2
      if (i == 1) then
        !! rooks and queens (V_E, V_W, V_N, V_S)
        if (abs(v) /= V_E .and. abs(v) /= V_N) cycle
        snipers = ior(self%rooks_co(xco), self%queens_co(xco))
      else
        if (abs(v) /= V_NW .and. abs(v) /= V_NE) cycle
        snipers = ior(self%bishops_co(xco), self%queens_co(xco))
      end if

      !! attacks[king][0]
      rays = BB_LINE(k_sq, sq)
      line_snipers = iand(rays, snipers)

      if (line_snipers /= BB_EMPTY) then
        !! find the sniper on the side of 'sq' (relative to the king)
        pinner_sq = pinner(k_sq, sq, line_snipers)

        if (pinner_sq /= -1) then
          if (iand(BB_BETWEEN(k_sq, pinner_sq), &
                   ior(self%occupied_co(WHITE), &
                       self%occupied_co(BLACK))) == BB_SQUARES(sq)) then
            mask = BB_LINE(k_sq, pinner_sq)
            return
          end if
        end if
      end if
    end do

    mask = BB_ALL
  end function pin_mask

  !! @brief emptying given sq, optionally returning pc
  subroutine remove_piece_at(self, sq, pc)

    use PieceM, only: &
      EMPTY, WHITE, BLACK, PAWN, KNIGHT, BISHOP, ROOK, QUEEN, KING
    use BitboardM, only: &
      BB_EMPTY, BB_SQUARES

    class(BaseT), intent(inout) :: self
    integer(SQK), intent(in) :: sq
    integer(PCK), intent(out), optional :: pc
    integer(PCK) :: co, ty
    integer(BBK) :: bb

    bb = BB_SQUARES(sq)
    if (iand(self%occupied_co(WHITE), bb) /= BB_EMPTY) then
      co = WHITE
    else if (iand(self%occupied_co(BLACK), bb) /= BB_EMPTY) then
      co = BLACK
    else
      if (present(pc)) pc = EMPTY
      return
    end if
    self%occupied_co(co) = ibclr(self%occupied_co(co), sq)

    if (iand(self%pawns_co(co), bb) /= BB_EMPTY) then
      self%pawns_co(co) = ibclr(self%pawns_co(co), sq)
      ty = PAWN
    else if (iand(self%knights_co(co), bb) /= BB_EMPTY) then
      self%knights_co(co) = ibclr(self%knights_co(co), sq)
      ty = KNIGHT
    else if (iand(self%bishops_co(co), bb) /= BB_EMPTY) then
      self%bishops_co(co) = ibclr(self%bishops_co(co), sq)
      ty = BISHOP
    else if (iand(self%rooks_co(co), bb) /= BB_EMPTY) then
      self%rooks_co(co) = ibclr(self%rooks_co(co), sq)
      ty = ROOK
    else if (iand(self%queens_co(co), bb) /= BB_EMPTY) then
      self%queens_co(co) = ibclr(self%queens_co(co), sq)
      ty = QUEEN
    else if (iand(self%kings_co(co), bb) /= BB_EMPTY) then
      self%kings_co(co) = ibclr(self%kings_co(co), sq)
      ty = KING
    else
      error stop "Error: remove_piece_at sq EMPTY"
    end if
    if (present(pc)) pc = ty + co
  end subroutine remove_piece_at

  !! @brief reset to the starting position
  subroutine reset_base(self)

    use BitboardM, only: &
      BB_RANK_1, BB_RANK_2, BB_RANK_7, BB_RANK_8, &
      BB_A1, BB_B1, BB_C1, BB_D1, BB_E1, BB_F1, BB_G1, BB_H1, &
      BB_A8, BB_B8, BB_C8, BB_D8, BB_E8, BB_F8, BB_G8, BB_H8

    class(BaseT), intent(inout) :: self

    ! init bit planes
    self%pawns_co = [BB_RANK_2, BB_RANK_7]
    self%knights_co = [ior(BB_B1, BB_G1), ior(BB_B8, BB_G8)]
    self%bishops_co = [ior(BB_C1, BB_F1), ior(BB_C8, BB_F8)]
    self%rooks_co = [ior(BB_A1, BB_H1), ior(BB_A8, BB_H8)]
    self%queens_co = [BB_D1, BB_D8]
    self%kings_co = [BB_E1, BB_E8]
    self%occupied_co = [ior(BB_RANK_1, BB_RANK_2), ior(BB_RANK_7, BB_RANK_8)]

    ! init game phase and material count
    call full_score(self, self%score, self%phase, self%score_mg)

  end subroutine reset_base

  !! @brief parse FEN board field to place pieces
  subroutine set_base_fen(self, fen, status_opt)

    use PieceM, only: is_piece_char, parse_piece
    use SquareM, only: is_rank_digit, V_E, V_S, FILE_A, FILE_H, &
                       RANK_1_OFFSET, RANK_8_OFFSET

    class(BaseT), intent(out) :: self
    character(len=*), intent(in) :: fen
    integer, intent(out), optional :: status_opt
    character(len=1) :: char
    integer(PCK) :: pc
    integer(SQK) :: file, rank_offset, sq, empty_count
    integer :: iempty, istr
    character(len=*), parameter :: VALID_CHARS = "rnbqkpRNBQKP12345678/"

    if (present(status_opt)) status_opt = ERR_NONE

    ! pre-validation
    do istr = 1, len_trim(fen)
      if (index(VALID_CHARS, fen(istr:istr)) == 0) then
        if (present(status_opt)) then
          status_opt = ERR_FEN_INVALID_SYNTAX
        else
          print *, 'ERROR: Invalid char in FEN string: '//fen(istr:istr)
          stop
        end if
        return
      end if
    end do

    ! initialization
    call self%clear_base()

    rank_offset = RANK_8_OFFSET
    file = FILE_A

    ! parsing
    do istr = 1, len_trim(fen)
      char = fen(istr:istr)
      if (char == '/') then ! then end of rank
        rank_offset = rank_offset + V_S
        file = FILE_A

      else if (is_piece_char(char)) then ! then a validated piece
        ! place piece char
        sq = rank_offset + file
        pc = parse_piece(char)
        call self%set_piece_at(sq, pc)
        file = file + V_E ! next square

      else if (is_rank_digit(char)) then ! then empty squares
        ! clear empty squares
        read (char, fmt='(i1)') empty_count
        do iempty = 1, empty_count
          sq = rank_offset + file
          call self%set_piece_at(sq, EMPTY)
          file = file + V_E ! next square
        end do
      end if

      if (file > FILE_H + 1_SQK) then
        if (.not. present(status_opt)) then
          print '(a,i2,a,i2)', "ERROR: FEN expected ""/""; "// &
            "rank_offset(0:7)=", rank_offset, "file(0:7)=", file
          error stop
        end if
        status_opt = ERR_FEN_EXPECTED_SEPARATOR
        exit
      end if

      if (rank_offset < RANK_1_OFFSET) then
        if (.not. present(status_opt)) then
          if (istr /= len_trim(fen)) &
            print '(a,i2,a,i2)', "ERROR: FEN has extra chars: "// &
            fen(istr + 1:len_trim(fen))
          error stop
        end if
        status_opt = ERR_FEN_EXTRA_CHARS
        exit
      end if
    end do

    ! init the game phase and score
    call full_score(self, self%score, self%phase, self%score_mg)

  end subroutine set_base_fen

  !! @brief set a piece on a given square, ensuring piece_at, king, and occ
  pure subroutine set_piece_at(self, sq, pc)

    use PieceM, only: PIECE_COLOR, PIECE_TYPE, &
                      PAWN, KNIGHT, BISHOP, ROOK, QUEEN, KING

    class(BaseT), intent(inout) :: self
    integer(SQK), intent(in) :: sq
    integer(PCK), intent(in) :: pc
    integer(PCK) :: co, captured

    captured = piece_at(self, sq)

    ! remove any non-none piece from the bbs
    if (captured /= EMPTY) then
      co = PIECE_COLOR(captured)
      self%occupied_co(co) = ibclr(self%occupied_co(co), sq)
      select case (PIECE_TYPE(captured))
      case (PAWN)
        self%pawns_co(co) = ibclr(self%pawns_co(co), sq)
      case (KNIGHT)
        self%knights_co(co) = ibclr(self%knights_co(co), sq)
      case (BISHOP)
        self%bishops_co(co) = ibclr(self%bishops_co(co), sq)
      case (ROOK)
        self%rooks_co(co) = ibclr(self%rooks_co(co), sq)
      case (QUEEN)
        self%queens_co(co) = ibclr(self%queens_co(co), sq)
      case (KING)
        self%kings_co(co) = BB_EMPTY
      end select
    end if

    ! place the non-empty piece into the bbs
    if (pc /= EMPTY) then
      co = PIECE_COLOR(pc)
      self%occupied_co(co) = ibset(self%occupied_co(co), sq)
      select case (PIECE_TYPE(pc))
      case (PAWN)
        self%pawns_co(co) = ibset(self%pawns_co(co), sq)
      case (KNIGHT)
        self%knights_co(co) = ibset(self%knights_co(co), sq)
      case (BISHOP)
        self%bishops_co(co) = ibset(self%bishops_co(co), sq)
      case (ROOK)
        self%rooks_co(co) = ibset(self%rooks_co(co), sq)
      case (QUEEN)
        self%queens_co(co) = ibset(self%queens_co(co), sq)
      case (KING)
        self%kings_co(co) = ibset(self%kings_co(co), sq)
      end select
    end if
  end subroutine set_piece_at

  !! @brief potential slider attackers with x-ray view of sq
  pure function attackers_xray_mask(base, sq, attacker_co) result(mask)

    use PieceM, only: PCK, XCOLOR
    use SquareM, only: SQK
    use BitboardM, only: &
      BB_PAWN_ATTACKS_CO, BB_KNIGHT_ATTACKS, BB_BISHOP_RAYS, BB_ROOK_RAYS

    type(BaseT), intent(in) :: base
    integer(SQK), intent(in) :: sq
    integer(PCK), intent(in) :: attacker_co
    integer(BBK) :: mask

    ! for leapers, normal attackers mask
    mask = &
      iand(base%pawns_co(attacker_co), &
           BB_PAWN_ATTACKS_CO(XCOLOR(attacker_co), sq))
    mask = &
      ior(mask, &
          iand(base%knights_co(attacker_co), BB_KNIGHT_ATTACKS(sq)))

    ! for sliders choose BB_EMPTY to acquire the full rays
    mask = &
      ior(mask, &
          iand(ior(base%bishops_co(attacker_co), &
                   base%queens_co(attacker_co)), &
               BB_BISHOP_RAYS(sq)))
    mask = &
      ior(mask, &
          iand(ior(base%rooks_co(attacker_co), &
                   base%queens_co(attacker_co)), &
               BB_ROOK_RAYS(sq)))
  end function attackers_xray_mask

  !! @brief mailbox array (piece-square-table)
  function get_mailbox_array(base) result(pst)

    use PieceM, only: EMPTY, WHITE, BLACK, &
                      PAWN, KNIGHT, BISHOP, ROOK, QUEEN, KING
    use BitboardM, only: pop_lsb

    type(BaseT), intent(in) :: base
    integer(PCK), dimension(0:SQUARE_MASK) :: pst

    integer(BBK) :: temp_bb
    integer(SQK) :: sq
    integer(PCK) :: co

    pst = EMPTY

    do co = WHITE, BLACK, (BLACK - WHITE)
      temp_bb = base%pawns_co(co)
      do while (temp_bb /= BB_EMPTY)
        call pop_lsb(temp_bb, sq)
        pst(sq) = PAWN + co
      end do

      temp_bb = base%knights_co(co)
      do while (temp_bb /= BB_EMPTY)
        call pop_lsb(temp_bb, sq)
        pst(sq) = KNIGHT + co
      end do

      temp_bb = base%bishops_co(co)
      do while (temp_bb /= BB_EMPTY)
        call pop_lsb(temp_bb, sq)
        pst(sq) = BISHOP + co
      end do

      temp_bb = base%rooks_co(co)
      do while (temp_bb /= BB_EMPTY)
        call pop_lsb(temp_bb, sq)
        pst(sq) = ROOK + co
      end do

      temp_bb = base%queens_co(co)
      do while (temp_bb /= BB_EMPTY)
        call pop_lsb(temp_bb, sq)
        pst(sq) = QUEEN + co
      end do

      temp_bb = base%kings_co(co)
      do while (temp_bb /= BB_EMPTY)
        call pop_lsb(temp_bb, sq)
        pst(sq) = KING + co
      end do
    end do
  end function get_mailbox_array

  !! @brief pawn attacks
  pure function get_pawn_attacks(self, co) result(bb)

    use PieceM, only: PCK, WHITE, XCOLOR
    use SquareM, only: V_NE, V_NW, V_SW, V_SE
    use BitboardM, only: BBK, BB_FILE_A, BB_FILE_H

    class(BaseT), intent(in) :: self
    integer(PCK), intent(in) :: co
    integer(BBK) :: bb

    integer(PCK) :: xco

    xco = XCOLOR(co)

    if (co == WHITE) then
      !! White: NW(+7) must avoid A-file, and NE(+9) must avoid H-file
      bb = ior( &
           iand(ishft(iand(self%pawns_co(co), not(BB_FILE_A)), V_NW), &
                self%occupied_co(xco)), &
           iand(ishft(iand(self%pawns_co(co), not(BB_FILE_H)), V_NE), &
                self%occupied_co(xco)))
    else
      !! Black: SW(-9) must avoid A-file, and SE (-7) must avoid H-file
      bb = ior( &
           iand(ishft(iand(self%pawns_co(co), not(BB_FILE_A)), V_SW), &
                self%occupied_co(xco)), &
           iand(ishft(iand(self%pawns_co(co), not(BB_FILE_H)), V_SE), &
                self%occupied_co(xco)))
    end if
  end function get_pawn_attacks

  !! @brief ret stamp-like str given base
  pure function get_mailbox_str(base) result(str)

    use PieceM, only: PIECE_CHAR
    use SquareM, only: SQUARE_FILE, V_E, V_S, FILE_A, FILE_H, &
                       RANK_1_OFFSET, RANK_8_OFFSET

    type(BaseT), intent(in) :: base
    character(len=8*17) :: str
    integer(SQK) :: rank_offset, file, sq
    integer(PCK) :: pc
    character(len=1), parameter :: NL = new_line('A'), SP = ' '

    str = ''
    do rank_offset = RANK_8_OFFSET, RANK_1_OFFSET, V_S
      do file = FILE_A, FILE_H, V_E
        sq = rank_offset + file
        pc = piece_at(base, sq)
        str = trim(str)//SP//PIECE_CHAR(pc)
        if (SQUARE_FILE(sq) == FILE_H .and. sq /= FILE_H) str = trim(str)//NL
      end do
    end do
  end function get_mailbox_str

  !! @brief determine whether given square is attacked by given color
#ifndef DEBUG
  pure &
#endif
    function is_square_attacked(self, sq, by_co, occ) result(tf)

    use PieceM, only: PCK, XCOLOR
    use SquareM, only: SQK
    use BitboardM, only: &
      BB_PAWN_ATTACKS_CO, BB_KNIGHT_ATTACKS, BB_KING_ATTACKS, &
      BB_ROOK_RAYS, BB_BISHOP_RAYS, BB_BETWEEN, BB_EMPTY, BB_SQUARES

    class(BaseT), intent(in) :: self
    integer(kind=SQK), intent(in) :: sq !! target square
    integer(kind=PCK), intent(in) :: by_co !! enemy color
    integer(kind=BBK), intent(in) :: occ !! current board occupancy
    logical :: tf

    integer(BBK) :: snipers, line_snipers
    integer(SQK) :: s_sq

    tf = .false.

    !! attacks by pawn, by_co pawns that attack sq with: XCOLOR(by_co)
    if (iand(BB_PAWN_ATTACKS_CO(XCOLOR(by_co), sq), &
             self%pawns_co(by_co)) /= BB_EMPTY) then
      tf = .true.

    !! attacks by knight by_co
    else if (iand(BB_KNIGHT_ATTACKS(sq), &
                  self%knights_co(by_co)) /= BB_EMPTY) then
      tf = .true.

    !! attacks by king, by_co
    else if (iand(BB_KING_ATTACKS(sq), &
                  BB_SQUARES(king_sq(self, by_co))) /= BB_EMPTY) then
      tf = .true.

    else
      !! rooks and queens sliders
      snipers = ior(self%rooks_co(by_co), self%queens_co(by_co))
      line_snipers = iand(snipers, BB_ROOK_RAYS(sq))

      do while (line_snipers /= BB_EMPTY)
        s_sq = int(trailz(line_snipers), SQK)
        if (iand(BB_BETWEEN(sq, s_sq), occ) == BB_EMPTY) then
          tf = .true.
          return
        end if
        line_snipers = ibclr(line_snipers, s_sq)
      end do

      !! diag sliders: bishops and queens
      snipers = ior(self%bishops_co(by_co), self%queens_co(by_co))
      line_snipers = iand(snipers, BB_BISHOP_RAYS(sq))

      do while (line_snipers /= BB_EMPTY)
        s_sq = int(trailz(line_snipers), SQK)
        if (iand(BB_BETWEEN(sq, s_sq), occ) == BB_EMPTY) then
          tf = .true.
          return
        end if
        line_snipers = ibclr(line_snipers, s_sq)
      end do
    end if
  end function is_square_attacked

  !! @brief nearest of the far snipers
  pure function pinner(k_sq, sq, snipers) result(pinner_sq)

    use BitboardM, only: BB_VECTOR, get_ray_in_direction

    integer(SQK), intent(in) :: k_sq
    integer(SQK), intent(in) :: sq
    integer(BBK), intent(in) :: snipers
    integer(SQK) :: pinner_sq

    integer(SQK) :: v
    integer(BBK) :: far_snipers

    !! direction from king through sq
    v = BB_VECTOR(k_sq, sq)

    !! isolate far_snipers sq, s.t. K->sq->far_snipers
    far_snipers = iand(snipers, get_ray_in_direction(sq, v))

    if (far_snipers == BB_EMPTY) then
      pinner_sq = -1_SQK
      return
    end if

    !! nearest far_sniper
    !! if vector is positive, want lsb; if negative, want msb
    if (v > 0_SQK) then
      pinner_sq = int(trailz(far_snipers), SQK)
    else
      pinner_sq = int(63 - leadz(far_snipers), SQK)
    end if
  end function pinner

  !! @brief print mailbox
  subroutine print_mailbox(base)

    class(BaseT), intent(in) :: base  ! class can accept BoardT

    write (*, fmt='(a)') get_mailbox_str(base)
  end subroutine print_mailbox

  !! @brief perfom whole-board static score evaluation
  subroutine full_score(self, score, phase_opt, score_mg_opt)

    use EvaluationM, only: &
      EVK, VALUE_PST, PIECE_PHASE, PIECE_VALUE, BOOST_ST_BK, BOOST_ST_WK
    use PieceM, only: WHITE, BLACK
    use SquareM, only: SQK

    class(BaseT), intent(in) :: self
    integer(kind=EVK), intent(out) :: score
    integer(kind=EVK), intent(out), optional :: phase_opt, score_mg_opt
    integer(kind=EVK) :: max_boost, phase, score_mg
    integer(kind=SQK) :: sq

    ! middle-game scoring sum material
    score_mg = sum(PIECE_VALUE(get_mailbox_array(self)))

    ! sum position PST
    do sq = 0, 63
      score_mg = score_mg + VALUE_PST(self%piece_at(sq), sq)
    end do
    ! score_mg = score_mg + sum(VALUE_PST(get_mailbox_array(self), 0:63))

    ! sum pawn structure
    score_mg = score_mg + score_pawn_structure(self)
    if (present(score_mg_opt)) score_mg_opt = score_mg
    ! PRINT *, "score_mg=", score_mg

    ! game phase (0-24) sum piece phase
    phase = sum(PIECE_PHASE(get_mailbox_array(self)))
    if (present(phase_opt)) phase_opt = phase
    ! PRINT *, "phase=", phase

    ! maximum end-game boost, white POV
    if (any(self%kings_co == BB_EMPTY)) then ! protect against missing king!
      max_boost = 0_EVK
    else
      max_boost = sum([BOOST_ST_WK(king_sq(self, WHITE)), &
                       BOOST_ST_BK(king_sq(self, BLACK))])
    end if
    ! PRINT *, "max_boost=", max_boost

    ! sum phased boost
    score = score_mg + int(max_boost*(1 - phase/24.0), kind=EVK)
    ! PRINT *, "score=", score

  end subroutine full_score

  !! @brief TODO score_pawn_structure
  function score_pawn_structure(self) result(score)

    use EvaluationM, only: EVK

    class(BaseT), intent(in) :: self
    integer(EVK) :: score

    if (.false.) score = int(self%pawns_co(0), kind=EVK)
    score = 0_EVK
    ! PRINT *, "base=", self
  end function score_pawn_structure

end module BaseM

