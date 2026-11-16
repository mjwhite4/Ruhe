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
program test_search_limits

  use, intrinsic :: iso_fortran_env, only: int64
  use TestUtilsM, only: begin_suite, begin_test, pass, fail

  use BoardM, only: STARTING_FEN

  implicit none

  call begin_suite("test_search_limits")

  call assert_search_limits( &
    test_name="start position", &
    fen=STARTING_FEN)

contains

  subroutine assert_search_limits(test_name, fen)

    use, intrinsic :: iso_fortran_env, only: real64, output_unit
    use iso_fortran_env, only: error_unit
    use BaseM, only: &
      print_mailbox
    use BoardM, only: &
      BoardT, get_board, get_fen, set_fen, generate_legal_moves
    use ClockM, only: LimitsT, init_limits
    use EvaluationM, only: MATE
    use MoveM, only: get_uci
    use PieceM, only: WHITE
    use SearchM, only: alphabeta, search_deeper, SearchResultT
    use StringUtilsM, only: next_token, to_hex, to_str

    character(len=*), intent(in) :: test_name, fen

    type(BoardT), pointer :: board
    type(SearchResultT) :: result_out
    integer(int64) :: complete_nodes
    integer :: nmoves

    call begin_test(test_name)

    board => get_board()
    call board%reset_board()

    call set_fen(board, fen)
    if (fen /= get_fen(board)) then
      error stop "fen /= get_fen(board) before search:"//trim(fen)
    end if
    call generate_legal_moves(board, nmoves)

    result_out = SearchResultT()

    !! establish node requirement with unrestricted budget

    board%limits = LimitsT() ! <-- HUGE node budget
    call init_limits(board%limits, board%state%turn == WHITE, board%control)
    board%control%poll_input = .false. ! <-- now non-interactive
    call alphabeta(board, 3, 0, -MATE, MATE, &
                   result_out%move, result_out%score, &
                   result_out%nnodes)
    complete_nodes = board%control%nodes
    call check(.not. board%control%is_stopped, &
               "unrestricted search completed")
    call check(board%control%nodes > 2_int64, &
               "baseline search visits descendants")

    !! test budget of 1_int64

    call set_fen(board, fen)
    call generate_legal_moves(board, nmoves)

    board%limits = LimitsT() ! <-- HUGE node budget
    call init_limits(board%limits, board%state%turn == WHITE, board%control)
    board%control%poll_input = .false. ! <-- now non-interactive
    call alphabeta(board, 3, 0, -MATE, MATE, &
                   result_out%move, result_out%score, &
                   result_out%nnodes)
    complete_nodes = board%control%nodes
    call check(.not. board%control%is_stopped, &
               "unrestricted search completed")
    call check(board%control%nodes > 2_int64, &
               "baseline search visits descendants")

    write (output_unit, *) board%control%nodes, result_out%nnodes
    call flush (output_unit)

    call pass()

  end subroutine assert_search_limits

  subroutine check(condition, description)

    logical, intent(in) :: condition
    character(len=*), intent(in) :: description

    if (.not. condition) call fail(description)
  end subroutine check

end program test_search_limits
