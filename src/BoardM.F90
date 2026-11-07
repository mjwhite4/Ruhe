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
! @brief State, Rules, Game Logic, and FEN Parsing
! @details state inspection, rule-checking, end-of-game conditions,
! FEN conversion, and position evaluation/hashing without board mutation
module BoardM

  use, intrinsic :: iso_fortran_env, only: int8, int16
  use BaseM, only: BaseT, STARTING_BOARD_FEN
  use BitboardM, only: BBK
  use ClockM, only: LimitsT, ControlT
  use EvaluationM, only: EVK
  use MoveStackM, only: MVK, MoveStackT
  use PieceM, only: PCK
  use SquareM, only: SQK, SQUARE_MASK, SQUARE_NAME
  use StateM, only: StateT, get_state_str

  implicit none

  !! @brief game result states
  !! @details termination enum in python-chess
  integer, parameter :: &
    TERM_NONE = 0, &
    TERM_CHECKMATE = 1, &
    TERM_STALEMATE = 2, &
    TERM_INSUFFICIENT_MATERIAL = 3, &
    TERM_SEVENTYFIVE_MOVES = 4, &
    TERM_FIVEFOLD_REPETITION = 5, &
    TERM_FIFTY_MOVES = 6, &
    TERM_THREEFOLD_REPETITION = 7

  character(len=7), parameter :: &
    UNDECIDED = "*", &
    DRAWN = "1/2-1/2", &
    WHITE_WINS = "1-0", &
    BLACK_WINS = "0-1"

  type, public :: OutcomeT
    integer :: termination = TERM_NONE
    character(len=7) :: winner = DRAWN ! -1 for draw, 0 for WHITE, 1 for BLACK
  end type OutcomeT

  character(len=66), parameter, public :: &
    STARTING_FEN = STARTING_BOARD_FEN//' w KQkq - 0 1'

  integer(kind=EVK), dimension(0:SQUARE_MASK, 0:SQUARE_MASK), &
    save, public :: history = 0_EVK

  integer, parameter :: &
    MAX_GAME_PLY = 1000

  type, extends(BaseT), public :: BoardT
    type(StateT) :: state
    type(MoveStackT) :: stack
    type(StateT), dimension(MAX_GAME_PLY) :: state_sequence ! possible moves
    integer(MVK), dimension(MAX_GAME_PLY) :: move_sequence ! possible moves
    integer :: game_ply
    type(LimitsT) :: limits
    type(ControlT) :: control
  contains

    ! state, FEN, and utility methods
    procedure :: clear_state                       !! ** "clear"
    procedure :: set_fen                           !! **
    procedure :: get_fen                           !! ** fen
    procedure, nopass :: get_board
    procedure :: edit_board
    procedure :: reset_board                       !! **
    procedure :: clear_board                       !! **
    procedure :: ply                               !! **
    procedure :: print_move_list

    ! game rules and status queries
    procedure :: can_claim_draw                    !! **
    procedure :: can_claim_fifty_moves             !! **
    procedure :: can_claim_threefold_repetition
    procedure :: is_fifty_moves                    !! **
    procedure :: is_seventyfive_moves              !! **
    procedure :: is_fivefold_repetition            !! **
    procedure :: is_repetition                     !! **
    procedure :: is_halfmoves_
    procedure :: has_insufficient_material         !! **
    procedure :: is_insufficient_material          !! **
    procedure :: is_check                          !! **
    procedure :: is_checkmate                      !! **
    procedure :: is_draw
    procedure :: is_stalemate                      !! **
    procedure :: is_threefold_repetition           !! **
    procedure :: is_game_over                      !! **
    procedure :: outcome                           !! **
    procedure :: get_result                        !! ** "result"
    procedure :: gives_check                       !! **
    procedure :: gives_checkmate                   !! **
    procedure :: was_into_check                    !! **
    procedure :: has_legal_moves                   !! **

    ! hashing and scoring utilities
    procedure :: calculate_base_hash
    procedure :: calculate_full_hash
    procedure :: update_hash_and_score
    procedure :: update_score

#ifdef DEBUG
    procedure :: verify_hash
#endif

    ! AttacksM
    procedure :: is_legal
    procedure :: is_pseudo_legal
    procedure :: is_into_check
    procedure :: is_move_an_evasion
    procedure :: is_valid_castling_move
    procedure :: is_valid_pawn_move
    procedure :: is_ep_discovered_check
    procedure :: stays_on_pin_ray
    procedure :: checkers_mask
    procedure :: get_evasion_mask
    procedure :: get_potential_pinners
    procedure :: slider_blockers_

    ! GeneratorM
    procedure :: generate_pseudo_legal_moves
    procedure :: generate_pseudo_legal_captures
    procedure :: generate_pseudo_legal_ep
    procedure :: generate_legal_moves
    procedure :: generate_legal_castling_moves
    procedure :: generate_legal_ep
    procedure :: generate_evasions
    procedure :: generate_captures_by_pawn
    procedure :: generate_piece_captures
    procedure :: generate_piece_quiets
    procedure :: make_move
    procedure :: unmake_move
    procedure :: move_pieces
    procedure :: unmove_pieces
    procedure :: parse_uci
  end type BoardT

  interface

    module &
      function is_legal(self, mv) result(res_tf)
      class(BoardT), intent(in) :: self
      integer(MVK), intent(in) :: mv
      logical :: res_tf
    end function is_legal
    module &
#ifndef DEBUG
      pure &
#endif
      function is_move_an_evasion(self, mv, k_sq, chkrs) result(res_tf)
      class(BoardT), intent(in) :: self
      integer(MVK), intent(in) :: mv
      integer(SQK), intent(in) :: k_sq
      integer(BBK), intent(in) :: chkrs
      logical :: res_tf
    end function is_move_an_evasion
    module &
      pure &
      function is_valid_castling_move(self, mv) result(res_tf)
      class(BoardT), intent(in) :: self
      integer(MVK), intent(in) :: mv
      logical :: res_tf
    end function is_valid_castling_move
    module &
      pure &
      function is_valid_pawn_move(self, from_sq, to_sq) result(res_tf)
      class(BoardT), intent(in) :: self
      integer(SQK), intent(in) :: from_sq, to_sq
      logical :: res_tf
    end function is_valid_pawn_move
    module &
      pure &
      function is_ep_discovered_check(self, from_sq, to_sq, k_sq) result(res_tf)
      class(BoardT), intent(in) :: self
      integer(SQK), intent(in) :: from_sq, to_sq, k_sq
      logical :: res_tf
    end function is_ep_discovered_check
    module &
      pure &
      function stays_on_pin_ray(self, from_sq, to_sq, k_sq) result(res_tf)
      class(BoardT), intent(in) :: self
      integer(SQK), intent(in) :: from_sq, to_sq, k_sq
      logical :: res_tf
    end function stays_on_pin_ray
    module &
#ifndef DEBUG
      pure &
#endif
      function checkers_mask(self) result(mask)
      class(BoardT), intent(in) :: self
      integer(BBK) :: mask
    end function checkers_mask
    module &
#ifndef DEBUG
      pure &
#endif
      function get_evasion_mask(self, k_sq, chkrs) result(mask)
      class(BoardT), intent(in) :: self
      integer(SQK), intent(in) :: k_sq
      integer(BBK), intent(in) :: chkrs
      integer(BBK) :: mask
    end function get_evasion_mask
    module &
      pure &
      function get_potential_pinners(self, k_sq) result(bb)
      class(BoardT), intent(in) :: self
      integer(SQK), intent(in) :: k_sq
      integer(BBK) :: bb
    end function get_potential_pinners
    module &
#ifndef DEBUG
      pure &
#endif
      function slider_blockers_(self, k_sq) result(pinned)
      class(BoardT), intent(in) :: self
      integer(SQK), intent(in) :: k_sq
      integer(BBK) :: pinned
    end function slider_blockers_

    module &
      pure &
      subroutine is_pseudo_legal(self, mv, res_tf)
      class(BoardT), intent(in) :: self
      integer(MVK), intent(in) :: mv
      logical, intent(out) :: res_tf
    end subroutine is_pseudo_legal
    module &
      subroutine is_into_check(self, mv, res_tf)
      class(BoardT), intent(in) :: self
      integer(MVK), intent(in) :: mv
      logical, intent(out) :: res_tf
    end subroutine is_into_check

    module &
      function generate_legal_castling_moves(self, from_mask_opt, to_mask_opt) &
      result(nmoves)
      class(BoardT), intent(inout) :: self
      integer(BBK), intent(in), optional :: from_mask_opt, to_mask_opt
      integer :: nmoves
    end function generate_legal_castling_moves
    module &
      function generate_legal_ep(self) result(nmoves)
      class(BoardT), intent(inout) :: self
      integer :: nmoves
    end function generate_legal_ep
    module &
      function generate_captures_by_pawn(self) result(nmoves)
      class(BoardT), intent(inout) :: self
      integer :: nmoves
    end function generate_captures_by_pawn

    module &
      subroutine generate_pseudo_legal_moves(self, from_mask_opt, to_mask_opt)
      class(BoardT), intent(inout) :: self
      integer(BBK), intent(in), optional :: from_mask_opt, to_mask_opt
      integer :: nmoves
    end subroutine generate_pseudo_legal_moves
    module &
     subroutine generate_pseudo_legal_captures(self, from_mask_opt, to_mask_opt)
      class(BoardT), intent(inout) :: self
      integer(BBK), intent(in), optional :: from_mask_opt, to_mask_opt
    end subroutine generate_pseudo_legal_captures
    module &
#ifndef DEBUG
      pure &
#endif
      subroutine generate_pseudo_legal_ep(self, from_mask_opt, to_mask_opt)
      class(BoardT), intent(inout) :: self
      integer(BBK), intent(in), optional :: from_mask_opt, to_mask_opt
    end subroutine generate_pseudo_legal_ep
    module &
      subroutine generate_legal_moves(self, nmoves)
      class(BoardT), intent(inout) :: self
      integer, intent(out) :: nmoves
    end subroutine generate_legal_moves
    module &
     subroutine generate_evasions(self, k_sq, chkrs, from_mask_opt, to_mask_opt)
      class(BoardT), intent(inout) :: self
      integer(SQK), intent(in) :: k_sq
      integer(BBK), intent(in) :: chkrs
      integer(BBK), intent(in), optional :: from_mask_opt, to_mask_opt
    end subroutine generate_evasions
    module &
#ifndef DEBUG
      pure &
#endif
      subroutine generate_piece_captures(self, ty)
      class(BoardT), intent(inout) :: self
      integer(PCK), intent(in) :: ty
    end subroutine generate_piece_captures
    module &
#ifndef DEBUG
      pure &
#endif
      subroutine generate_piece_quiets(self, ty)
      class(BoardT), intent(inout) :: self
      integer(PCK), intent(in) :: ty
    end subroutine generate_piece_quiets
    module &
      subroutine make_move(self, mv)
      class(BoardT), intent(inout) :: self
      integer(MVK), intent(in) :: mv
    end subroutine make_move
    module &
      subroutine unmake_move(self, mv)
      class(BoardT), intent(inout) :: self
      integer(MVK), intent(in) :: mv
    end subroutine unmake_move
    module &
      subroutine move_pieces(self, mv)
      class(BoardT), intent(inout) :: self
      integer(MVK), intent(in) :: mv
    end subroutine move_pieces
    module &
      subroutine unmove_pieces(self, mv)
      class(BoardT), intent(inout) :: self
      integer(MVK), intent(in) :: mv
    end subroutine unmove_pieces
    module &
      subroutine parse_uci(self, uci_str, mv)
      class(BoardT), intent(inout) :: self
      character(len=*), intent(in) :: uci_str
      integer(MVK), intent(out) :: mv
    end subroutine parse_uci
    module &
      pure &
      subroutine update_state(state, mv)
      type(StateT), intent(inout) :: state
      integer(MVK), intent(in) :: mv
    end subroutine update_state

  end interface

  !! @brief exactly one instance of the board
  type(BoardT), save, target :: singleton_board

contains

  pure &
    subroutine clear_state(self)

    use PieceM, only: WHITE
    use SquareM, only: EP_NONE
    use StateM, only: CSK

    class(BoardT), intent(inout) :: self

    self%state%turn = WHITE
    self%state%castling_rights = 0_CSK
    self%state%ep_sq = EP_NONE
    self%state%halfmove_clock = 0_int16
    self%state%fullmove_number = 1_int16
  end subroutine clear_state

  !! @brief parse 6-item FEN string
  subroutine set_fen(self, fen, status_opt)

    use StringUtilsM, only: next_token
    use PieceM, only: BLACK, WHITE
    use SquareM, only: EP_NONE, parse_square
    use BaseM, only: clear_base, full_score
    use StateM, only: B_OO_BIT, B_OOO_BIT, W_OO_BIT, W_OOO_BIT

    ! use ZobristM, only: ZHK

    class(BoardT), intent(inout) :: self
    character(len=*), intent(in) :: fen
    integer, intent(out), optional :: status_opt
    character(len=:), allocatable :: token

    character(len=71) :: fen_base
    character(len=1) :: fen_turn
    character(len=4) :: fen_castling
    character(len=2) :: fen_ep
    character(len=2) :: fen_halfmove
    character(len=3) :: fen_fullmove
    integer :: t_status, cursor

    if (present(status_opt)) status_opt = 0

    associate (base => self, state => self%state, stack => self%stack)

      call self%clear_base()
      call self%stack%reset_movestack()

      !! it is an error not to have four fields

      cursor = 1
      call next_token(fen, " ", cursor, token)
      if (len_trim(token) == 0) then
        if (present(status_opt)) then; status_opt = 1; return; end if
        error stop "Error: set_fen: empty"//trim(fen)
      end if
      fen_base = token

      ! parse fen board
      call base%set_base_fen(fen_base, t_status)
      if (t_status /= 0) then
        if (present(status_opt)) then; status_opt = t_status; return; end if
        error stop "Error: set_base_fen fail:"//trim(fen)
      end if

      ! parse turn
      call next_token(fen, " ", cursor, token)
      if (len_trim(token) == 0) then
        if (present(status_opt)) then; status_opt = 2; return; end if
        error stop "Error: set_fen turn:"//trim(fen)
      end if
      fen_turn = token

      ! parse active color: "w" or "b", who goes next.
      if (fen_turn == 'w') then
        state%turn = WHITE
      else if (fen_turn == 'b') then
        state%turn = BLACK
      else
        if (present(status_opt)) then; status_opt = 3; return; end if
        error stop "Error: set_fen ac not 'w' or 'b':"//trim(fen)
      end if

      ! parse castling
      call next_token(fen, " ", cursor, token)
      if (len_trim(token) == 0) then
        if (present(status_opt)) then; status_opt = 4; return; end if
        error stop "Error: set_fen castling fail:"//trim(fen)
      end if
      fen_castling = token

      ! set castling rights: KQkq, Qkq, Qk, etc.
      state%castling_rights = 0_int8
      if (index(fen_castling, 'K') > 0) &
        state%castling_rights = &
        ibset(state%castling_rights, W_OO_BIT) !SQ_H1=1110 (bit0)
      if (index(fen_castling, 'Q') > 0) &
        state%castling_rights = &
        ibset(state%castling_rights, W_OOO_BIT) !SQ_A1=1101 (bit1)
      if (index(fen_castling, 'k') > 0) &
        state%castling_rights = &
        ibset(state%castling_rights, B_OO_BIT) !SQ_H8=1011 (bit2)
      if (index(fen_castling, 'q') > 0) &
        state%castling_rights = &
        ibset(state%castling_rights, B_OOO_BIT) !SQ_A8=0111 (bit3)

      ! parse fen_ep
      call next_token(fen, " ", cursor, token)
      if (len_trim(token) == 0) then
        if (present(status_opt)) then; status_opt = 5; return; end if
        error stop "Error: set_fen: missing ep:"//trim(fen)
      end if
      fen_ep = token

      ! set ep square
      if (fen_ep == '-') then
        state%ep_sq = EP_NONE
      else if (len_trim(fen_ep) == 2 .and. &
               index('abcdefgh', fen_ep(1:1)) > 0 .and. &
               index('36', fen_ep(2:2)) > 0) then
        state%ep_sq = parse_square(fen_ep(1:2))
      else
        if (present(status_opt)) then; status_opt = 6; return; end if
        error stop "Error: set_fen: ep not '(-|[a-h](3|6))'"//trim(fen)
      end if

      ! parse fen_halfmove
      fen_halfmove = '0' ! default if none
      call next_token(fen, " ", cursor, token)
      fen_halfmove = token
      read (fen_halfmove, fmt=*) state%halfmove_clock

      ! parse fen_fullmove
      fen_fullmove = '1'  ! default, if none
      call next_token(fen, " ", cursor, token)
      fen_fullmove = token
      read (fen_fullmove, fmt=*) state%fullmove_number

      ! finalize the score
      call full_score(base, base%score, base%phase, base%score_mg)

      ! finalize the state hash
      self%state%h = calculate_full_hash(self)

      ! finalize the movestack
      call stack%reset_movestack()
    end associate
  end subroutine set_fen

  !! @brief ret fen string given base and state
  pure function get_fen(self) result(fen)

    class(BoardT), intent(in) :: self
    character(len=147) :: fen

    character(len=64 + 7) :: base_str ! up to 64 letters and 7 /
    character(len=16) :: state_str ! e.g.: w KQkq e6 + (3d + 1SP + 2d)
    character(len=1), parameter :: SP = ' '

    base_str = self%base_fen()
    state_str = get_state_str(self%state)

    fen = trim(base_str)//SP//trim(state_str)
  end function get_fen

  !! @brief ret pointer to the singleton board
  function get_board() result(ptr)

    type(BoardT), pointer :: ptr

    ptr => singleton_board
  end function get_board

  !! @brief edit the initial position
  subroutine edit_board(self)

    use PieceM, only: &
      COLOR_MASK, NONEMPTIES, BK, BR, WK, WR, &
      EMPTY, PAWN, BISHOP, QUEEN, ROOK, KING
    use SquareM, only: &
      EP_NONE, SQ_A1, SQ_E1, SQ_A8, SQ_E8, SQ_H8, &
      parse_rank, parse_file
    use StateM, only: &
      CSK, &
      B_OO_BIT, B_OOO_BIT, W_OO_BIT, W_OOO_BIT

    class(BoardT), intent(inout) :: self
    character(len=10) :: str
    integer(kind=PCK) :: pc
    integer(kind=SQK) :: sq

    do
      read (unit=*, fmt=*) str
      str = adjustl(str)
      pc = EMPTY
      select case (str(1:1))
      case ('#')
        call self%clear_board()
        cycle
      case ('c')
        self%state%turn = ieor(self%state%turn, COLOR_MASK)
        cycle
      case ('.', ' ')
        exit
      case ('X', 'x')
        pc = EMPTY
      case ('K', 'k')
        pc = KING + self%state%turn
      case ('Q', 'q')
        pc = QUEEN + self%state%turn
      case ('B', 'b')
        pc = BISHOP + self%state%turn
      case ('R', 'r')
        pc = ROOK + self%state%turn
      case ('P', 'p')
        pc = PAWN + self%state%turn
      end select
      if (parse_file(str(2:2)) >= 0 .and. parse_rank(str(3:3)) >= 0) then
        sq = parse_file(str(2:2)) + parse_rank(str(3:3))
        if (sq >= SQ_A1 .and. sq <= SQ_H8 .and. &
            findloc(NONEMPTIES, pc, dim=1) > 0) then
          call self%set_piece_at(sq, pc)
        else
          write (unit=0, fmt='(a)') &
            'Error parsing pc, col, and rank, pc, sq=', sq, pc
        end if
      else
        write (unit=0, fmt='(a)') 'Error parsing col and rank:'//str
      end if
    end do

    ! assume rights exist when required pieces are present
    !# TODO could edit cas_str!
    self%state%castling_rights = 0_CSK
    if (self%piece_at(SQ_E1) == WK) then
      if (self%piece_at(SQ_H8) == WR) & !SQ_H1=1110 (bit0)
        self%state%castling_rights = &
        ibset(self%state%castling_rights, W_OO_BIT)
      if (self%piece_at(SQ_A1) == WR) & !SQ_A1=1101 (bit1)
        self%state%castling_rights = &
        ibset(self%state%castling_rights, W_OOO_BIT)
    end if
    if (self%piece_at(SQ_E8) == BK) then
      if (self%piece_at(SQ_H8) == BR) & !SQ_H8=1011 (bit2)
        self%state%castling_rights = &
        ibset(self%state%castling_rights, B_OO_BIT)
      if (self%piece_at(SQ_A8) == BR) & !SQ_A8=0111 (bit3)
        self%state%castling_rights = &
        ibset(self%state%castling_rights, B_OOO_BIT)
    end if

    ! assume no ep
    !# TODO could edit the ep_sq!
    self%state%ep_sq = EP_NONE
  end subroutine edit_board

  !! @brief starting position
  pure &
    subroutine reset_state(self)

    use PieceM, only: WHITE
    use SquareM, only: EP_NONE
    use StateM, only: CSK

    type(BoardT), intent(inout) :: self

    self%state%turn = WHITE
    self%state%castling_rights = 15_CSK
    self%state%ep_sq = EP_NONE
    self%state%halfmove_clock = 0_int16
    self%state%fullmove_number = 1_int16
  end subroutine reset_state

  !! @brief full initial board
  subroutine reset_board(self)

    use BitboardM, only: init_ray_tables

    class(BoardT), intent(inout) :: self

    call self%BaseT%reset_base()
    call self%stack%reset_movestack()

    call init_ray_tables()
  end subroutine reset_board

  !! @brief clear (empty) entire board: base, state, movestack
  subroutine clear_board(self)

    class(BoardT), intent(inout) :: self

    call self%BaseT%clear_base()
    call self%clear_state()
    call self%stack%reset_movestack()
  end subroutine clear_board

  !! @brief half moves since the game start,
  !! as indicated by fullmove_number and turn
  pure function ply(self) result(n)

    use PieceM, only: WHITE

    class(BoardT), intent(in) :: self
    integer :: n

    n = 2*(self%state%fullmove_number - 1) + &
        merge(0, 1, self%state%turn == WHITE)
  end function ply

  !! @brief print the move list to stdout.
  subroutine print_move_list(self)

    use MoveM, only: get_uci

    class(BoardT), intent(inout) :: self

    character(len=80) :: ln_str
    character(len=5) :: mv_str
    character(len=1), parameter :: SP = ' '
    integer :: ix, n, p

    p = self%stack%current_ply
    n = self%stack%ixn(p) + 1 - self%stack%ix1(p)
    write (unit=0, fmt="(i0, a)") n, " moves:"

    ln_str = ""
    do ix = self%stack%ix1(p), self%stack%ixn(p)
      mv_str = get_uci(self%stack%move_list(ix))
      if (ix == self%stack%ix1(p)) then
        ln_str = mv_str
      else if (len_trim(ln_str) + len_trim(mv_str) + 1 <= 80) then
        ln_str = trim(ln_str)//SP//trim(mv_str)
      else
        write (unit=0, fmt="(a)") trim(ln_str)
        ln_str = mv_str
      end if
    end do
    if (len_trim(ln_str) > 0) write (unit=0, fmt="(a)") trim(ln_str)
  end subroutine print_move_list

  !! Game Rules and Status Queries
  !!----------------------------

  !! @brief t/f whether position/turn can claim draw
  function can_claim_draw(self) result(res_tf)

    class(BoardT), intent(inout) :: self
    logical :: res_tf

    res_tf = self%can_claim_fifty_moves() .or. &
             self%has_insufficient_material(self%state%turn) .or. &
             self%can_claim_threefold_repetition()
  end function can_claim_draw

  !! @brief t/f whether position/turn can claim fifty move draw
  !! @details also, if a legal move can achieve this condition
  function can_claim_fifty_moves(self) result(res_tf)

    class(BoardT), intent(inout) :: self
    logical :: res_tf

    integer :: ix, p
    integer(MVK) :: mv

    res_tf = .false.
    if (self%is_fifty_moves()) return

    if (self%state%halfmove_clock >= 99) then
      p = self%stack%current_ply
      do ix = self%stack%ix1(p), self%stack%ixn(p)
        mv = self%stack%move_list(ix)
        call move_pieces(self, mv)
        if (self%is_fifty_moves()) res_tf = .true.
        call unmove_pieces(self, mv)
        if (res_tf) exit
      end do
    end if
  end function can_claim_fifty_moves

  !! @brief t/f whether position/turn can claim threefold repetition draw
  ! TODO
  function can_claim_threefold_repetition(self) result(res_tf)

    class(BoardT), intent(inout) :: self
    logical :: res_tf

    integer :: ix, p
    integer(MVK) :: mv

    res_tf = .false.
    if (self%is_fifty_moves()) return

    if (self%state%halfmove_clock >= 99) then
      p = self%stack%current_ply
      do ix = self%stack%ix1(p), self%stack%ixn(p)
        mv = self%stack%move_list(ix)
        call move_pieces(self, mv)
        if (self%is_fifty_moves()) res_tf = .true.
        call unmove_pieces(self, mv)
        if (res_tf) exit
      end do
    end if
  end function can_claim_threefold_repetition

  !! @brief true/false whether fifty moves occur without capture or pawn move
  function is_fifty_moves(self) result(res_tf)

    class(BoardT), intent(inout) :: self
    logical :: res_tf

    res_tf = is_halfmoves_(self, 100)
  end function is_fifty_moves

  !! @brief true/false whether is seventyfive moves since capture or pawn move
  function is_seventyfive_moves(self) result(res_tf)

    class(BoardT), intent(inout) :: self
    logical :: res_tf

    res_tf = is_halfmoves_(self, 150)
  end function is_seventyfive_moves

  !! @brief true/false whether position occurs a fifth time
  pure &
    function is_fivefold_repetition(self) result(res_tf)

    class(BoardT), intent(in) :: self
    logical :: res_tf

    res_tf = self%is_repetition(5)
  end function is_fivefold_repetition

  !! @brief t/f whether current position occurs target_count times
  !! @details scan state_sequence from the last irreversible move
  pure &
    function is_repetition(self, nlimit) result(res_tf)

    use StateM, only: StateT

    class(BoardT), intent(in) :: self
    integer, intent(in), optional :: nlimit
    logical :: res_tf

    integer :: i, nlim, start_search_ply, nmatch

    res_tf = .false.
    nmatch = 1 !! the current position is instance 1

    ! how high shall we count
    nlim = 3
    if (present(nlimit)) nlim = nlimit

    ! locate irreversable floor, guard against negative index
    start_search_ply = &
      max(0, self%stack%current_ply - self%state%halfmove_clock)

    ! backward scan on two-steps (each with same side to move)
    do i = self%stack%current_ply - 2, start_search_ply, -2
      if (self%state_sequence(i)%h == self%state%h) then
        nmatch = nmatch + 1
        if (nmatch >= nlim) then
          res_tf = .true.
          return
        end if
      end if
    end do
  end function is_repetition

  !! @brief auxilliary true/false whether n-many halfmoves and not mated
  function is_halfmoves_(self, n) result(res_tf)

    class(BoardT), intent(inout) :: self
    integer, intent(in) :: n
    logical :: res_tf

    res_tf = ((self%state%halfmove_clock >= n) .and. has_legal_moves(self))
  end function is_halfmoves_

  !! @brief true/false whether position occurs a third time
  pure &
    function is_threefold_repetition(self) result(res_tf)

    class(BoardT), intent(in) :: self
    logical :: res_tf

    res_tf = self%is_repetition(3)
  end function is_threefold_repetition

  !! @brief t/f whether given co has insufficient winning material
#ifndef DEBUG
  pure &
#endif
    function has_insufficient_material(self, co) result(res_tf)

    use PieceM, only: XCOLOR, BLACK, WHITE
    use BitboardM, only: BB_EMPTY, BB_SQUARES, &
                         BB_DARK_SQUARES, BB_LIGHT_SQUARES

    class(BoardT), intent(in) :: self
    integer(PCK), intent(in) :: co
    logical :: res_tf

    integer(BBK) :: bishops, knights, pawns, &
                    co_squares, xco_squares, mask_PRQ
    logical :: is_same_shade

    co_squares = self%occupied_co(co)
    xco_squares = self%occupied_co(XCOLOR(co))

    !! @details pawns, rooks, queens are sufficient
    !! (thus, insufficient = .false.)
    mask_PRQ = ior(self%pawns_co(co), &
                   ior(self%rooks_co(co), &
                       self%queens_co(co)))

    if (mask_PRQ /= BB_EMPTY) then
      ! with P, R, Q, insufficiency cannot be established
      res_tf = .false.

    else if (self%knights_co(co) /= BB_EMPTY) then
      !! @details knights are insufficent if only 1 (2 pieces in all)
      !! and xco has no pawns, knights, bishops, rooks
      res_tf = &
        (popcnt(co_squares) <= 2 .and. &
         iand(xco_squares, not(ior(BB_SQUARES(self%king_sq(co)), &
                                   self%queens_co(co)))) == BB_EMPTY)

    else if (self%bishops_co(co) /= BB_EMPTY) then
      !! @details at this stage, bishops are insufficient only if:
      !! our bishops are all on the same light/dark squares
      !! we have no other pieces
      !! opp has no P, N, or B of the opposite color
      bishops = ior(self%bishops_co(WHITE), self%bishops_co(BLACK))
      knights = ior(self%knights_co(WHITE), self%knights_co(BLACK))
      pawns = ior(self%pawns_co(WHITE), self%pawns_co(BLACK))
      is_same_shade = &
        ((iand(bishops, BB_DARK_SQUARES) == BB_EMPTY) .or. &
         (iand(bishops, BB_LIGHT_SQUARES) == BB_EMPTY))
      res_tf = is_same_shade .and. &
               (pawns == BB_EMPTY) .and. (knights == BB_EMPTY)

    else
      !! @details only the king remains, insufficient
      res_tf = .true.
    end if
  end function has_insufficient_material

  !! @brief t/f whether both sides have insufficient material
#ifndef DEBUG
  pure &
#endif
    function is_insufficient_material(self) result(res_tf)

    use PieceM, only: WHITE, BLACK

    class(BoardT), intent(in) :: self
    logical :: res_tf

    res_tf = self%has_insufficient_material(WHITE) .and. &
             self%has_insufficient_material(BLACK)
  end function is_insufficient_material

  !! @brief whether side to move is checked
  function is_check(self) result(res_tf)

    use BitboardM, only: BB_EMPTY

    class(BoardT), intent(in) :: self
    logical :: res_tf

    ! if an opponent attacks the king square
    res_tf = (checkers_mask(self) /= BB_EMPTY)
  end function is_check

  !! @brief t/f whether side to move is checkmated
  function is_checkmate(self) result(res_tf)

    class(BoardT), intent(inout) :: self
    logical :: res_tf

    integer :: nmoves

    if (.not. is_check(self)) then
      res_tf = .false.
      return
    end if

    call generate_legal_moves(self, nmoves)
    res_tf = (nmoves == 0)
  end function is_checkmate

  !! @brief t/f whether side to move is drawn
  function is_draw(self) result(res_tf)

    class(BoardT), intent(inout) :: self
    logical :: res_tf

    res_tf = &
      self%is_threefold_repetition() .or. &
      self%is_fifty_moves() .or. &
      self%is_stalemate()
  end function is_draw

  !! @brief t/f whether position/turn is stalemate
  function is_stalemate(self) result(res_tf)

    class(BoardT), intent(inout) :: self
    logical :: res_tf

    integer :: nmoves

    if (is_check(self)) then
      res_tf = .false.
      return
    end if

    call generate_legal_moves(self, nmoves)
    res_tf = (nmoves == 0)
  end function is_stalemate

  !! @brief true/false whether game is over
  function is_game_over(self, claim_draw_opt) result(res_tf)

    class(BoardT), intent(inout) :: self
    logical, intent(in), optional :: claim_draw_opt
    logical :: res_tf

    type(OutcomeT) :: term
    logical :: claim_draw

    claim_draw = .false.
    if (present(claim_draw_opt)) claim_draw = claim_draw_opt

    term = self%outcome(claim_draw)
    res_tf = (term%termination /= TERM_NONE)
  end function is_game_over

  !! @brief outcome termination and winner
  function outcome(self, claim_draw_opt) result(res)

    use PieceM, only: WHITE, BLACK, XCOLOR

    class(BoardT), intent(inout) :: self
    logical, optional, intent(in) :: claim_draw_opt
    type(OutcomeT) :: res

    logical :: claim_draw

    res = OutcomeT(TERM_NONE, UNDECIDED)

    claim_draw = .false.
    if (present(claim_draw_opt)) claim_draw = claim_draw_opt

    !! normal game end
    if (self%is_checkmate()) then
      res = OutcomeT(TERM_CHECKMATE, &
                     merge(WHITE_WINS, BLACK_WINS, &
                           XCOLOR(self%state%turn) == WHITE))

    else if (self%has_insufficient_material(WHITE) .and. &
             self%has_insufficient_material(BLACK)) then
      res = OutcomeT(TERM_INSUFFICIENT_MATERIAL, DRAWN)

    else if (.not. has_legal_moves(self)) then
      res = OutcomeT(TERM_STALEMATE, DRAWN)

    else if (self%state%halfmove_clock >= 150) then
      !! automatic draw
      res = OutcomeT(TERM_SEVENTYFIVE_MOVES, DRAWN)

    else if (self%is_fivefold_repetition()) then
      !! automatic draw
      res = OutcomeT(TERM_FIVEFOLD_REPETITION, DRAWN)

    else if (claim_draw) then
      !! claimable draws
      if (self%state%halfmove_clock >= 100) then
        res = OutcomeT(TERM_FIFTY_MOVES, DRAWN)

      else if (self%is_repetition(3)) then
        res = OutcomeT(TERM_THREEFOLD_REPETITION, DRAWN)
      end if
    end if
  end function outcome

  !! @brief result of a game: 1-0, 0-1, 1/2-1/2, or *
  function get_result(self, claim_draw) result(res)

    class(BoardT), intent(inout) :: self
    logical, intent(in), optional :: claim_draw
    type(OutcomeT) :: res

    logical :: t_claim_draw

    t_claim_draw = .false.
    if (present(claim_draw)) t_claim_draw = claim_draw
    res = self%outcome(t_claim_draw)
  end function get_result

  !! @brief t/f whether the move checks the king
  !! @details moves and unmoves pieces
  !! Correctly handles complex scenarios like discovered checks or pins
  !! Performance: it is called for potentially every pseudo legal move
  !! There is a static version of move pieces that can determine check
  !! gives_check can influence the priority in pack_move with CHECK_BONUS
  function gives_check(self, mv) result(res_tf)

    class(BoardT), intent(inout) :: self
    integer(MVK), intent(in) :: mv
    logical :: res_tf

    call move_pieces(self, mv) !! execute move
    res_tf = is_check(self) !! probe for check on opponent's king
    call unmove_pieces(self, mv) !! restore
  end function gives_check

  !! @brief t/f whether given move delivers checkmate to opp
  function gives_checkmate(self, mv) result(res_tf)

    class(BoardT), intent(inout) :: self
    integer(MVK), intent(in) :: mv
    logical :: res_tf

    call move_pieces(self, mv) !! execute
    res_tf = self%is_checkmate() !! probe whether opp is checkmated
    call unmove_pieces(self, mv) !! restore
  end function gives_checkmate

  !! @brief t/f whether previous move was into check
  function was_into_check(self) result(res_tf)

    use PieceM, only: XCOLOR

    class(BoardT), intent(in) :: self
    logical :: res_tf
    integer(SQK) :: opp_k_sq

    opp_k_sq = self%king_sq(XCOLOR(self%state%turn))
    res_tf = self%is_attacked_by(opp_k_sq, self%state%turn)
  end function was_into_check

  !! @brief true/false whether any legal moves exist
  function has_legal_moves(self) result(res_tf)

    class(BoardT), intent(inout) :: self
    logical :: res_tf

    integer(MVK) :: mv
    integer :: ix, p

    res_tf = .false.

    !! TODO suppose generate_legal_moves has not been called, ix1(1) = 0

    p = self%stack%current_ply
    do ix = self%stack%ix1(p), self%stack%ixn(p)
      mv = self%stack%move_list(ix)
      call move_pieces(self, mv) !// try the move.
      if (.not. is_check(self)) res_tf = .true.
      call unmove_pieces(self, mv)
      if (res_tf) exit
    end do
  end function has_legal_moves

  !! Hashing and Scoring
  !!------------------

  !! @brief calculate base hash using PST
  pure &
    function calculate_base_hash(self) result(h)

    use PieceM, only: EMPTY
    use SquareM, only: SQUARE_MASK
    use ZobristM, only: ZHK, ZHASH_PST

    class(BoardT), intent(in) :: self
    integer(kind=ZHK) :: h
    integer(PCK) :: pc
    integer :: sq

    h = 0_ZHK
    do sq = 0, int(SQUARE_MASK) ! needs [sq]=int32 here, or error messages!
      pc = self%piece_at(int(sq, SQK)) ! and only now must have [sq]=int16
      if (pc /= EMPTY) &
        h = ieor(h, ZHASH_PST(pc, sq))  !// pc, sq hash data table.
    end do
  end function calculate_base_hash

  !! @brief calculate ep hash key given ep_square (or ep_none)
  pure function calculate_ep_hash(ep_sq) result(h)

    use SquareM, only: SQUARE_FILE, EP_NONE
    use ZobristM, only: ZHK, ZHASH_EP_FILE

    integer(kind=SQK), intent(in) :: ep_sq
    integer(kind=ZHK) :: h

    if (ep_sq == EP_NONE) then
      h = 0_ZHK
      return
    end if
    h = ZHASH_EP_FILE(SQUARE_FILE(ep_sq))   !# 772->779
  end function calculate_ep_hash

  !! @brief calculate full board hash code
  !! @details mainly for comparison to incremental updates with ieor
  pure function calculate_full_hash(self) result(h)

    use PieceM, only: BLACK
    use ZobristM, only: ZHK, ZHASH_CASTLING, ZHASH_TURN

    class(BoardT), intent(in) :: self
    integer(kind=ZHK) :: h

    h = ieor(calculate_base_hash(self), &
             ieor(ZHASH_CASTLING(self%state%castling_rights), &
                  calculate_ep_hash(self%state%ep_sq)))
    if (self%state%turn == BLACK) &
      h = ieor(h, ZHASH_TURN)
  end function calculate_full_hash

  !! @brief change hash and score given move
#ifndef DEBUG
  pure &
#endif
    subroutine update_hash_and_score(self, state, mv)

    use PieceM, only: PAWN, KING, ROOK, COLOR_MASK, EMPTY
    use SquareM, only: SQUARE_FILE, RANK_OFFSET, &
                       FILE_A, FILE_D, FILE_F, FILE_G, FILE_H
    use BaseM, only: BaseT
    use ZobristM, only: ZHASH_PST
    use MoveM, only: get_from_sq, get_to_sq, get_piece, get_captured, &
                     get_promotion

    class(BoardT), intent(inout) :: self
    type(StateT), intent(inout) :: state
    integer(MVK), intent(in) :: mv

    integer(kind=PCK) :: captured, co, piece, promotion, arrival_pc
    integer(kind=SQK) :: &
      from_sq, to_sq, king_to_col, k_rank, rk_from, rk_to, victim_sq
    logical :: is_ep

    piece = get_piece(mv)
    from_sq = get_from_sq(mv)
    to_sq = get_to_sq(mv)
    captured = get_captured(mv)
    promotion = get_promotion(mv)

    co = iand(piece, COLOR_MASK)

    ! move -- from/ (possible)promotion to -- by the landing piece
    arrival_pc = merge(promotion, piece, promotion /= EMPTY)
    state%h = ieor(state%h, ieor(ZHASH_PST(piece, from_sq), &
                                 ZHASH_PST(arrival_pc, to_sq)))
    call update_score(self, piece, from_sq, arrival_pc, to_sq)

    ! capture (regular/ep), determine victim square based on is_ep
    if (captured /= EMPTY) then
      is_ep = (arrival_pc == PAWN + co) .and. (to_sq == state%ep_sq)
      victim_sq = merge(RANK_OFFSET(from_sq) + SQUARE_FILE(to_sq), to_sq, is_ep)
      state%h = ieor(state%h, ZHASH_PST(captured, victim_sq))
      call update_score(self, captured, victim_sq, EMPTY, victim_sq)
    end if

    ! castling; abs(from-to) == 2 assumes 0:63 LEFR
    if (piece == KING + co .and. abs(from_sq - to_sq) == 2) then

      ! choose king/queen-side based on king_to_col == FILE_G
      k_rank = RANK_OFFSET(from_sq)
      king_to_col = SQUARE_FILE(to_sq)
      rk_from = k_rank + merge(FILE_H, FILE_A, king_to_col == FILE_G)
      rk_to = k_rank + merge(FILE_F, FILE_D, king_to_col == FILE_G)
      state%h = ieor(state%h, ieor(ZHASH_PST(ROOK + co, rk_from), &
                                   ZHASH_PST(ROOK + co, rk_to)))
      call update_score(self, ROOK + co, rk_from, ROOK + co, rk_to)
    end if
  end subroutine update_hash_and_score

  !! @brief perfom score update given from and to
#ifndef DEBUG
  pure &
#endif
    subroutine update_score(self, from_pc, from_sq, to_pc, to_sq)

    use PieceM, only: BLACK, WHITE
    use EvaluationM, only: &
      BOOST_ST_BK, BOOST_ST_WK, EVK, PIECE_PHASE, PIECE_VALUE, VALUE_PST
    use BaseM, only: BaseT, king_sq
    use StateM, only: StateT

    class(BoardT), intent(inout) :: self
    integer(kind=SQK), intent(in) :: from_sq, to_sq
    integer(kind=PCK), intent(in) :: from_pc, to_pc
    integer(kind=EVK) :: max_boost, old_phase, old_score_mg

    old_score_mg = self%score_mg
    self%score_mg = old_score_mg &
                    - PIECE_VALUE(from_pc) - VALUE_PST(from_pc, from_sq) &
                    + PIECE_VALUE(to_pc) + VALUE_PST(to_pc, to_sq)

    ! reduce phase with the from-phase, augment with the to-phase
    old_phase = self%phase
    self%phase = old_phase - PIECE_PHASE(from_pc) + PIECE_PHASE(to_pc)

    ! computing present max_boost is cheaper than updating
    max_boost = &
      BOOST_ST_WK(self%king_sq(WHITE)) + BOOST_ST_BK(self%king_sq(BLACK))

    ! computing present score
    self%score = self%score_mg + &
                 int(max_boost*(1 - self%phase/24.0), kind=EVK)
  end subroutine update_score

#ifdef DEBUG
  pure function verify_hash(self) result(is_valid)

    use, intrinsic :: iso_fortran_env, only: int64

    class(BoardT), intent(in) :: self
    logical :: is_valid
    integer(kind=int64) :: h_full

    h_full = self%calculate_full_hash()

    is_valid = (self%state%h == h_full)

  end function verify_hash
#endif

end module BoardM
