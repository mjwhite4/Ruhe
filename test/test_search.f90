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
program test_search

  use, intrinsic :: iso_fortran_env, only: int64
  use EvaluationM, only: EVK, DRAW, MATE
  use TestUtilsM, only: begin_suite, begin_test, pass, fail
  implicit none

  call begin_suite("test_search")

  call assert_search( &
    test_name="mate in one", &
    fen="7k/8/5KQ1/8/8/8/8/8 w - - 0 1", &
    ref_nmoves=23, &
    ref_bm="g6g7", &
    ref_score=MATE - 1_EVK)

  call assert_search( &
    test_name="only one legal move", &
    fen="R6k/8/5K2/8/8/8/8/8 b - - 0 1", &
    ref_nmoves=1, &
    ref_bm="h8h7")

  call assert_search( &
    test_name="checkmate", &
    fen="7k/6Q1/5K2/8/8/8/8/8 b - - 0 1", &
    ref_nmoves=0, &
    ref_bm="0000", &
    ref_score=-MATE)

  call assert_search( &
    test_name="stalemate", &
    fen="7k/5Q2/6K1/8/8/8/8/8 b - - 0 1", &
    ref_nmoves=0, &
    ref_bm="0000", &
    ref_score=DRAW)

  call assert_search( &
    test_name="hanging queen", &
    fen="7k/8/8/3q4/8/8/8/K2R4 w - - 0 1", &
    ref_nmoves=12, &
    require_moves="d1d5")

  call assert_search( &
    test_name="promotion", &
    fen="7k/P7/8/8/8/8/8/7K w - - 0 1", &
    ref_nmoves=7, &
    require_moves="a7a8q,a7a8r,a7a8n,a7a8b")

  call assert_search( &
    test_name="legal en passant", &
    fen="7k/8/8/3pP3/8/8/8/7K w - d6 0 1", &
    ref_nmoves=5, &
    require_moves="e5d6")

  call assert_search( &
    test_name="illegal en passant", &
    fen="8/8/8/8/k2pP2R/8/8/3K4 b - e3 0 1", &
    ref_nmoves=6, &
    forbidden_moves="d4e3")

  call assert_search( &
    test_name="castling", &
    fen="r3k2r/8/8/8/8/8/8/R3K2R w KQkq - 0 1", &
    ref_nmoves=26, &
    require_moves="e1g1,e1c1")

contains

  subroutine assert_search(test_name, fen, ref_nmoves, &
                           ref_score, ref_bm, require_moves, forbidden_moves)

    use, intrinsic :: iso_fortran_env, only: real64
    use iso_fortran_env, only: error_unit
    use BoardM, only: BoardT, get_board, get_fen, set_fen, generate_legal_moves
    use ClockM, only: LimitsT, init_limits
    use EvaluationM, only: EVK, MATE
    use MoveM, only: MVK, get_uci
    use PieceM, only: WHITE
    use SearchM, only: alphabeta, negamax, search_deeper, SearchResultT
    use StringUtilsM, only: next_token, to_hex, to_str
    use ZobristM, only: ZHK

    character(len=*), intent(in) :: test_name, fen
    integer, intent(in), optional :: ref_nmoves
    integer(EVK), intent(in), optional :: ref_score
    character(len=*), intent(in), optional :: &
      ref_bm, require_moves, forbidden_moves

    type(BoardT), pointer :: board
    integer :: depth, i, ipos, nmoves, p
    character(len=512) :: message_txt
    character(len=:), allocatable :: token
    type(SearchResultT) :: alphabeta_out, negamax_out
    logical :: found
    integer(MVK), dimension(:), allocatable :: moves
    integer(EVK) :: v
    integer(ZHK) :: h

    call begin_test(test_name)

    board => get_board()
    call board%reset_board()
    call set_fen(board, fen)
    call generate_legal_moves(board, nmoves)

    message_txt = ""

    ! save legal moves, ply, evaluation, and hash
    p = board%stack%current_ply
    allocate (moves(nmoves))
    moves = board%stack%move_list(board%stack%ix1(p):board%stack%ixn(p))
    v = board%score
    h = board%state%h

    ! if search nmoves and ref_nmoves are mismatched, fail
    if (present(ref_nmoves)) then
      if (nmoves /= ref_nmoves) then
        write (unit=message_txt, fmt='(a,i0,a,i0)') &
          " nmoves:", nmoves, &
          " ref_nmoves:", ref_nmoves
        call fail((message_txt))
      end if
    end if

    if (fen /= get_fen(board)) then
      error stop "fen /= get_fen(board) before search:"//trim(fen)
    end if

    do depth = 1, 3

      alphabeta_out = SearchResultT()

      board%limits = LimitsT()
      call init_limits(board%limits, board%state%turn == WHITE, board%control)
      board%control%poll_input = .false.

      call alphabeta(board, depth, 0, -MATE, MATE, &
                     alphabeta_out%move, alphabeta_out%score, &
                     alphabeta_out%nnodes)

      if (board%control%is_stopped) then
        call fail("alphabeta stopped before completing the test")
      end if

      ! compare fen, score, and hash before-after restoration
      if (fen /= get_fen(board)) then
        call fail("fen not restored after alphabeta:"//get_fen(board))
      elseif (v /= board%score) then
        call fail("static value not restored after alphabeta:"// &
                  to_str(board%score))
      elseif (h /= board%state%h) then
        call fail("hash not restored after alphabeta:"// &
                  to_hex(board%state%h))
      end if

      ! compare ply, nmoves, moves before-after restoration
      if (p /= board%stack%current_ply) then
        call fail("p/=board%stack%current_ply not restored after alphabeta")
      elseif (nmoves /= board%stack%ixn(p) - board%stack%ix1(p) + 1) then
        call fail("nmoves/=ixn-ix1+1 not restored after alphabeta")
      end if

      ! ensure null move for terminal position
      if (nmoves == 0) then
        if (alphabeta_out%move /= 0_MVK) then
          call fail("alphabeta: expected null move for terminal position")
        end if
      else if (.not. any(moves == alphabeta_out%move)) then
        call fail("alphabeta: returned move is not legal")
      end if

      ! prove that each of moves(i) exists once within move_list(ix1:ixn)
      ! (and with above nmoves == ixn(p) - ix1(p) + 1, the converse)
      do i = 1, nmoves
        if (.not. count(moves(i) == board%stack%move_list( &
                        board%stack%ix1(p):board%stack%ixn(p))) == 1) then
          call fail("move not present after alphabeta:"//get_uci(moves(i)))
        end if
      end do

      negamax_out = SearchResultT()
      call negamax(board, depth, 0, &
                   negamax_out%move, negamax_out%score, negamax_out%nnodes)

      ! compare fen, score, hashi, nmoves before-after restoration
      if (fen /= get_fen(board)) then
        call fail("fen not restored after negamax:"//get_fen(board))
      elseif (v /= board%score) then
        call fail("static value not restored after negamax:"// &
                  to_str(board%score))
      elseif (h /= board%state%h) then
        call fail("hash not restored after negamax:"//to_hex(board%state%h))
      end if

      ! compare ply, nmoves, and moves before-after restoration
      if (p /= board%stack%current_ply) then
        call fail("p/=board%stack%current_ply not restored after negamax")
      elseif (nmoves /= board%stack%ixn(p) - board%stack%ix1(p) + 1) then
        call fail("nmoves/=ixn-ix1+1 not restored after negamax")
      end if

      ! prove that each of moves(i) exists once within move_list(ix1:ixn)
      ! (and with above nmoves == ixn(p) - ix1(p) + 1, the converse)
      do i = 1, nmoves
        if (.not. count(moves(i) == board%stack%move_list( &
                        board%stack%ix1(p):board%stack%ixn(p))) == 1) then
          call fail("move not present after negamax:"//get_uci(moves(i)))
        end if
      end do

      ! compare search scores and ref_score
      if (alphabeta_out%score /= negamax_out%score) then
        write (unit=message_txt, fmt='(a,i0,a,i0)') &
          " alphabeta score:", alphabeta_out%score, &
          " negamax score:", negamax_out%score
        call fail(trim(message_txt))
      else if (present(ref_score)) then
        if (negamax_out%score /= ref_score) then
          write (unit=message_txt, fmt='(a,i0,a,i0)') &
            " negamax score:", negamax_out%score, &
            " ref_score:", ref_score
          call fail(trim(message_txt))
        end if
      end if

      ! ensure null move for terminal position
      if (nmoves == 0) then
        if (negamax_out%move /= 0_MVK) then
          call fail("negamax: expected null move for terminal position")
        end if
      else if (.not. any(moves == negamax_out%move)) then
        call fail("negamax: returned move is not legal")
      end if

      ! if searched move and not one of the ref_bm are matched, fail
      if (present(ref_bm)) then
        ipos = 1
        found = .false.
        do
          call next_token(ref_bm, ",", ipos, token)
          if (token == "") exit
          if (get_uci(alphabeta_out%move) == token) then
            found = .true.
            exit
          end if
        end do
        if (.not. found) then
          call fail(" | bm:"//get_uci(alphabeta_out%move)//" /= "// &
                    "ref_bm:"//trim(ref_bm))
        end if
      end if
    end do

    if (present(forbidden_moves)) then
      associate (unused => forbidden_moves)
      end associate
    end if

    if (present(require_moves)) then
      associate (unused => require_moves)
      end associate
    end if

    call pass()

  end subroutine assert_search

end program test_search
