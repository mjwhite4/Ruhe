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
program test_searching

  use, intrinsic :: iso_fortran_env, only: int64
  use TestUtilsM, only: begin_suite, begin_test, pass, fail

  use BoardM, only: STARTING_FEN

  implicit none

  call begin_suite("test_searching")

  call assert_search_request( &
    test_name="start position", &
    fen=STARTING_FEN)

contains

  subroutine assert_search_request(test_name, fen)

    use, intrinsic :: iso_fortran_env, only: real64, output_unit
    use iso_fortran_env, only: error_unit, output_unit
    use BaseM, only: &
      print_mailbox
    use BoardM, only: &
      BoardT, get_board, get_fen, set_fen, generate_legal_moves
    use ClockM, only: SearchRequestT, init_searching
    use EvaluationM, only: EVK, MATE
    use MoveM, only: MVK, get_uci
    use PieceM, only: WHITE
    use SearchM, only: alphabeta, search_deeper, SearchResultT
    use StringUtilsM, only: next_token, to_hex, to_str
    use ZobristM, only: ZHK

    character(len=*), intent(in) :: test_name, fen

    type(BoardT), pointer :: board
    type(SearchResultT) :: result_out
    integer(int64) :: visit_count, budget
    integer :: i, nmoves, p
    character(len=512) :: message_txt
    integer(MVK), dimension(:), allocatable :: moves
    integer(EVK) :: v, v_mg, v_phase
    integer(ZHK) :: h

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

    message_txt = ""

    ! save legal moves, ply, evaluation, and hash
    p = board%stack%current_ply
    allocate (moves(nmoves))
    moves = board%stack%move_list(board%stack%ix1(p):board%stack%ixn(p))
    v = board%score
    v_mg = board%score_mg
    v_phase = board%phase
    h = board%state%h
    call check(board%verify_hash(), "root hash matches full computation")

    !! establish node requirement with default unrestricted budget
    board%request = SearchRequestT() ! <-- HUGE default budget
    call init_searching(board%request, board%state%turn == WHITE, &
                        board%searching)
    board%searching%poll_input = .false. ! <-- non-interactive for test
    call alphabeta(board, 3, 0, -MATE, MATE, &
                   result_out%move, result_out%score, &
                   result_out%nnodes)

    ! verify search completed uninterrupted, fetch visit_count
    call check(.not. board%searching%is_stopped, &
               "unrestricted depth 3 completed without stopping")
    call check(board%searching%node_count > 2_int64, &
               "depth 3 visited multiple descendants")
    call check(.not. board%searching%quit_requested, &
               "unrestricted search did not request quit")

    ! compare fen, score, and hash before-after restoration
    if (fen /= get_fen(board)) then
      call fail("fen not restored after alphabeta:"//get_fen(board))
    elseif (v /= board%score) then
      call fail("score not restored:"//to_str(board%score))
    elseif (v_mg /= board%score_mg) then
      call fail("score_mg not restored:"//to_str(board%score_mg))
    elseif (v_phase /= board%phase) then
      call fail("phase not restored:"//to_str(board%phase))
    elseif (h /= board%state%h) then
      call fail("hash not restored after alphabeta:"//to_hex(board%state%h))
    end if

    ! compare ply, nmoves, moves before-after restoration
    if (p /= board%stack%current_ply) then
      call fail("p/=board%stack%current_ply not restored after alphabeta")
    elseif (nmoves /= board%stack%ixn(p) - board%stack%ix1(p) + 1) then
      call fail("nmoves/=ixn-ix1+1 not restored after alphabeta")
    end if
    do i = 1, size(moves)
      call check(count(board%stack%move_list( &
                     board%stack%ix1(p):board%stack%ixn(p)) == moves(i)) == 1, &
                 "root legal move occurs exactly once after search")
    end do

    visit_count = board%searching%node_count

    !! one-node budget (=1)
    call set_fen(board, fen)
    call generate_legal_moves(board, nmoves)

    budget = 1_int64
    board%request = SearchRequestT()
    board%request%node_limit = budget ! interrupt before baseline completion
    call init_searching(board%request, board%state%turn == WHITE, &
                        board%searching)
    board%searching%poll_input = .false. ! <-- non-interactive for test

    call check(board%searching%node_count == 0_int64, &
               "node count initially zero")
    call check(board%searching%node_limit == 1_int64, &
               "one-node budget installed")
    call check(.not. board%searching%is_stopped, &
               "stop flag initially clear")

    call flush (output_unit)
    call alphabeta(board, 3, 0, -MATE, MATE, &
                   result_out%move, result_out%score, &
                   result_out%nnodes)

    ! verify after search
    call check(board%searching%node_count == budget, &
               "one-node budget exactly reached")
    call check(board%searching%is_stopped, &
               "one-node exhaustion set stop flag")
    call check(.not. board%searching%quit_requested, &
               "one-node exhaustion did not request quit")

    ! compare fen, score, and hash before-after restoration
    if (fen /= get_fen(board)) then
      call fail("fen not restored after alphabeta:"//get_fen(board))
    elseif (v /= board%score) then
      call fail("score not restored:"//to_str(board%score))
    elseif (v_mg /= board%score_mg) then
      call fail("score_mg not restored:"//to_str(board%score_mg))
    elseif (v_phase /= board%phase) then
      call fail("phase not restored:"//to_str(board%phase))
    elseif (h /= board%state%h) then
      call fail("hash not restored after alphabeta:"//to_hex(board%state%h))
    end if

    ! compare ply, nmoves, moves before-after restoration
    if (p /= board%stack%current_ply) then
      call fail("p/=board%stack%current_ply not restored after alphabeta")
    elseif (nmoves /= board%stack%ixn(p) - board%stack%ix1(p) + 1) then
      call fail("nmoves/=ixn-ix1+1 not restored after alphabeta")
    end if
    do i = 1, size(moves)
      call check(count(board%stack%move_list( &
                     board%stack%ix1(p):board%stack%ixn(p)) == moves(i)) == 1, &
                 "root legal move occurs exactly once after search")
    end do

    !! recursive interruption
    call set_fen(board, fen)
    call generate_legal_moves(board, nmoves)

    budget = max(2_int64, visit_count/2_int64)
    board%request = SearchRequestT()
    board%request%node_limit = budget ! <-- budget of 1
    call init_searching(board%request, board%state%turn == WHITE, &
                        board%searching)
    board%searching%poll_input = .false. ! <-- non-interactive for test
    call alphabeta(board, 3, 0, -MATE, MATE, &
                   result_out%move, result_out%score, &
                   result_out%nnodes)

    ! verify after search
    call check(board%searching%node_count == budget, &
               "recursive interruption exact node budget reached")
    call check(board%searching%is_stopped, &
               "recursive interruption node exhaustion set stop flag")
    call check(.not. board%searching%quit_requested, &
               "recursive interruption node exhaustion did not request quit")

    ! compare fen, score, and hash before-after restoration
    if (fen /= get_fen(board)) then
      call fail("fen not restored after alphabeta:"//get_fen(board))
    elseif (v /= board%score) then
      call fail("score not restored:"//to_str(board%score))
    elseif (v_mg /= board%score_mg) then
      call fail("score_mg not restored:"//to_str(board%score_mg))
    elseif (v_phase /= board%phase) then
      call fail("phase not restored:"//to_str(board%phase))
    elseif (h /= board%state%h) then
      call fail("hash not restored after alphabeta:"//to_hex(board%state%h))
    end if

    ! compare ply, nmoves, moves before-after restoration
    if (p /= board%stack%current_ply) then
      call fail("p/=board%stack%current_ply not restored after alphabeta")
    elseif (nmoves /= board%stack%ixn(p) - board%stack%ix1(p) + 1) then
      call fail("nmoves/=ixn-ix1+1 not restored after alphabeta")
    end if

    call pass()
    do i = 1, size(moves)
      call check(count(board%stack%move_list( &
                     board%stack%ix1(p):board%stack%ixn(p)) == moves(i)) == 1, &
                 "root legal move occurs exactly once after search")
    end do

  end subroutine assert_search_request

  subroutine check(condition, description)

    logical, intent(in) :: condition
    character(len=*), intent(in) :: description

    if (.not. condition) call fail(description)
  end subroutine check

end program test_searching
