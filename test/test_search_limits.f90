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
  use TestUtilsM, only: begin_suite, begin_test, pass_msg, fail_msg

  use BoardM, only: STARTING_FEN

  implicit none

  call begin_suite("test_search_limits")

  call assert_search_request( &
    test_name="start position", &
    fen=STARTING_FEN)

contains

  subroutine assert_search_request(test_name, fen)

    use BoardM, only: &
      BoardT, get_board, get_fen, set_fen, generate_legal_moves
    use ClockM, only: SearchRequestT, init_search
    use EvaluationM, only: MATE
    use PieceM, only: WHITE
    use SearchM, only: alphabeta, SearchResultT
    use TestUtilsM, only: &
      PositionCheckpointT, check_checkpoint, save_checkpoint

    character(len=*), intent(in) :: test_name, fen

    type(BoardT), pointer :: board
    type(SearchResultT) :: result_out
    integer(int64) :: visit_count, budget
    integer :: nmoves
    type(PositionCheckpointT) :: checkpoint

    call begin_test(test_name)

    ! init board, moves, result_out struct
    board => get_board()
    call board%reset_board()
    call set_fen(board, fen)
    if (fen /= get_fen(board)) then
      error stop "fen /= get_fen(board) before search:"//trim(fen)
    end if
    call generate_legal_moves(board, nmoves)
    result_out = SearchResultT()

    ! save legal moves, ply, evaluation, and hash
    call save_checkpoint(board, checkpoint)

    if (.not. board%verify_hash()) &
      call fail_msg("root hash does not match full computation")

    !! establish node requirement with default unrestricted budget
    board%request = SearchRequestT() ! <-- HUGE default budget
    call init_search(board%request, board%state%turn == WHITE, board%search)
    board%search%poll_input = .false. ! <-- non-interactive for test
    call alphabeta(board, 3, 0, -MATE, MATE, &
                   result_out%move, result_out%score, &
                   result_out%nnodes)

    ! verify search completed uninterrupted, fetch visit_count
    if (board%search%is_stopped) &
      call fail_msg("unrestricted depth 3 was interrupted")
    if (board%search%node_count <= 2_int64) &
      call fail_msg("depth 3 visited too few descendants")
    if (board%search%quit_requested) &
      call fail_msg("unrestricted search unexpectedly requested quit")

    ! verify board is restored post-search
    call check_checkpoint(board, checkpoint)

    visit_count = board%search%node_count

    !! one-node budget (=1)
    call set_fen(board, fen)
    call generate_legal_moves(board, nmoves)

    budget = 1_int64
    board%request = SearchRequestT()
    board%request%node_limit = budget ! interrupt before baseline completion
    call init_search(board%request, board%state%turn == WHITE, board%search)
    board%search%poll_input = .false. ! <-- non-interactive for test

    if (board%search%node_count /= 0_int64) &
      call fail_msg("initial node count /= zero")
    if (board%search%node_limit /= 1_int64) &
      call fail_msg("one-node budget was not installed")
    if (board%search%is_stopped) &
      call fail_msg("stop flag already set before search")

    call save_checkpoint(board, checkpoint)

    call alphabeta(board, 3, 0, -MATE, MATE, &
                   result_out%move, result_out%score, &
                   result_out%nnodes)

    call check_checkpoint(board, checkpoint)

    ! verify after search
    if (board%search%node_count /= budget) &
      call fail_msg("one-node search did not reach the exact budget")
    if (.not. board%search%is_stopped) &
      call fail_msg("one-node exhaustion did not set stop flag")
    if (board%search%quit_requested) &
      call fail_msg("one-node exhaustion unexpectedly requested quit")

    !! recursive interruption
    call set_fen(board, fen)
    call generate_legal_moves(board, nmoves)

    budget = max(2_int64, visit_count/2_int64)
    board%request = SearchRequestT()
    board%request%node_limit = budget
    call init_search(board%request, board%state%turn == WHITE, board%search)
    board%search%poll_input = .false. ! <-- non-interactive for test

    call save_checkpoint(board, checkpoint)

    call alphabeta(board, 3, 0, -MATE, MATE, &
                   result_out%move, result_out%score, &
                   result_out%nnodes)

    call check_checkpoint(board, checkpoint)

    ! verify after search
    if (board%search%node_count /= budget) &
      call fail_msg("recursive interrupt exact node budget not reached")
    if (.not. board%search%is_stopped) &
      call fail_msg("recursive interrupt node exhaustion did not set stop flag")
    if (board%search%quit_requested) &
call fail_msg("recursive interrupt node exhaustion unexpectedly requested quit")

    call pass_msg()

  end subroutine assert_search_request

end program test_search_limits
