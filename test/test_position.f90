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
program test_position

  use, intrinsic :: iso_fortran_env, only: int64
  use EvaluationM, only: EVK, DRAW, MATE
  use TestUtilsM, only: begin_suite, begin_test, pass, fail
  implicit none

  call begin_suite("test_position")

  call assert_position( &
    test_name="mate in one", &
    fen="7k/8/5KQ1/8/8/8/8/8 w - - 0 1", &
    ref_nmoves=23, &
    ref_bm="g6g7", &
    ref_score=MATE - 1_EVK)

  call assert_position( &
    test_name="only one legal move", &
    fen="R6k/8/5K2/8/8/8/8/8 b - - 0 1", &
    ref_nmoves=1, &
    ref_bm="h8h7")

  call assert_position( &
    test_name="checkmate", &
    fen="7k/6Q1/5K2/8/8/8/8/8 b - - 0 1", &
    ref_nmoves=0, &
    ref_score=-MATE)

  call assert_position( &
    test_name="stalemate", &
    fen="7k/5Q2/6K1/8/8/8/8/8 b - - 0 1", &
    ref_nmoves=0, &
    ref_score=DRAW)

  call assert_position( &
    test_name="hanging queen", &
    fen="7k/8/8/3q4/8/8/8/K2R4 w - - 0 1", &
    ref_nmoves=12, &
    require_moves="d1d5")

  call assert_position( &
    test_name="promotion", &
    fen="7k/P7/8/8/8/8/8/7K w - - 0 1", &
    ref_nmoves=7, &
    require_moves="a7a8q,a7a8r,a7a8n,a7a8b")

  call assert_position( &
    test_name="legal en passant", &
    fen="7k/8/8/3pP3/8/8/8/7K w - d6 0 1", &
    ref_nmoves=5, &
    require_moves="e5d6")

  call assert_position( &
    test_name="illegal en passant", &
    fen="8/8/8/8/k2pP2R/8/8/3K4 b - e3 0 1", &
    ref_nmoves=6, &
    forbidden_moves="d4e3")

  call assert_position( &
    test_name="castling", &
    fen="r3k2r/8/8/8/8/8/8/R3K2R w KQkq - 0 1", &
    ref_nmoves=26, &
    require_moves="e1g1,e1c1")

contains

  subroutine assert_position(test_name, fen, ref_nmoves, &
                             ref_score, ref_bm, require_moves, forbidden_moves)

    use, intrinsic :: iso_fortran_env, only: real64
    use iso_fortran_env, only: error_unit, output_unit
    use BoardM, only: BoardT, get_board, set_fen, generate_legal_moves
    use ClockM, only: elapsed_time
    use MoveM, only: MVK, get_uci
    use SearchM, only: alphabeta, negamax, search_deeper, SearchResultT
    use StringUtilsM, only: next_token

    character(len=*), intent(in) :: test_name, fen
    integer, intent(in), optional :: ref_nmoves
    integer(EVK), intent(in), optional :: ref_score
    character(len=*), intent(in), optional :: &
      ref_bm, require_moves, forbidden_moves

    type(BoardT), pointer :: board
    integer :: ipos, nmoves, ply
    character(len=:), allocatable :: message_txt, token
    integer(MVK), dimension(:), allocatable :: moves

    call begin_test(test_name)

    board => get_board()
    call board%reset_board()
    message_txt = ""

    call set_fen(board, fen)

    ! print '(a, a)', "position fen ", trim(fen)

    ! write (error_unit, '(a)') "test_position: generating root moves"
    ! flush (error_unit)

    call generate_legal_moves(board, nmoves)

    ! save move_list, ply
    ply = board%stack%current_ply
    allocate (moves(nmoves))
    moves = board%stack%move_list(board%stack%ix1(ply):board%stack%ixn(ply))
    ! print '(a)', get_uci(moves)

    ! count of nmoves must match ref_nmoves
    if (present(ref_nmoves)) then
      if (nmoves /= ref_nmoves) then
        write (unit=message_txt, fmt='(a,i0,a,i0)') &
          " nmoves:", nmoves, &
          " ref_nmoves:", ref_nmoves
        call fail(trim(message_txt))
      end if
    end if

    ! position with no moves is either checkmate or stalemate, so test it
    if (nmoves == 0) then
      if (ref_score == -MATE .and. &
          .not. board%is_check()) call fail("checkmate, expected check")

      if (ref_score == DRAW .and. &
          board%is_check()) call fail("stalemate, expected no check")

      call pass()
      return
    end if

    ! all ref_bm must appear exactly once
    if (present(ref_bm)) then
      ipos = 1
      do
        call next_token(ref_bm, ",", ipos, token)
        token = trim(adjustl(token))
        if (token == "") exit
        if (.not. count(token == get_uci(moves)) == 1) then
          call fail("ref_bm:"//trim(token)//" not found once")
        end if
      end do
    end if

    ! not any forbidden_moves may appear
    if (present(forbidden_moves)) then
      ipos = 1
      do
        call next_token(forbidden_moves, ",", ipos, token)
        token = trim(adjustl(token))
        if (token == "") exit
        if (any(token == get_uci(moves))) then
          call fail("forbidden_moves:"//trim(token)//" was found")
        end if
      end do
    end if

    ! ignore ref_score
    if (present(ref_score)) then
      associate (unused => ref_score)
      end associate
    end if

    call pass()

  end subroutine assert_position

end program test_position
