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
program test_search_history

  use TestUtilsM, only: begin_suite

  call begin_suite("test_search_history")

  ! set up startpos, follow prescribed moves, then inspect and test searches

  call assert_moves_and_search( &
    "position startpos moves e2e4 e7e5", &
    "rnbqkbnr/pppp1ppp/8/4p3/4P3/8/PPPP1PPP/RNBQKBNR w KQkq e6 0 2")

  call assert_moves_and_search( &
    "position startpos moves b1c3 g8f6", &
    "rnbqkb1r/pppppppp/5n2/8/8/2N5/PPPPPPPP/R1BQKBNR w KQkq - 2 2")

contains

  subroutine assert_moves_and_search(cmd_str, ref_fen)

    use BoardM, only: &
      BoardT, get_board, get_fen, set_fen, generate_legal_moves, &
      make_move, parse_uci, STARTING_FEN
    use ClockM, only: SearchRequestT, init_search
    use EvaluationM, only: MATE
    use MoveM, only: MVK, get_uci
    use PieceM, only: WHITE
    use SearchM, only: alphabeta, negamax, SearchResultT
    use StateM, only: StateT, equal_state
    use StringUtilsM, only: next_token, to_str
    use TestUtilsM, only: &
      begin_suite, begin_test, fail_msg, check_checkpoint, &
      pass_msg, PositionCheckpointT, save_checkpoint
    implicit none

    character(len=*), intent(in) :: cmd_str, ref_fen

    type(BoardT), pointer :: board
    integer :: depth, ipos, nmoves, ref_ply
    integer(MVK) :: mv
    character(len=:), allocatable :: token
    character(len=512) :: message_txt
    type(SearchResultT) :: alphabeta_out, negamax_out
    type(PositionCheckpointT) :: checkpoint
    type(StateT), dimension(1:10) :: save_state_sequence
    integer(MVK), dimension(1:10) :: save_move_sequence

    call begin_test(cmd_str)

    board => get_board()
    call board%reset_board()

    ! before moves
    ipos = 1

    ! parse "position startpos moves" ("moves" must be present in this example)
    do
      call next_token(cmd_str, " ", ipos, token)
      if (token == "position") cycle
      if (token == "moves") exit
      if (token == "startpos") then
        call set_fen(board, STARTING_FEN)
        call generate_legal_moves(board, nmoves)
        ref_ply = board%stack%current_ply
        cycle
      elseif (token == "") then
        call fail_msg("no moves in position statement")
      else
        call fail_msg("parsing position statement")
      end if
    end do

    ! parse uci moves and make them
    do
      call next_token(cmd_str, " ", ipos, token)
      if (token == "") exit
      call parse_uci(board, token, mv)
      if (mv == 0_MVK) call fail_msg("move not parsed:"//token)

      if (.not. any(mv == board%stack%move_list( &
                    board%stack%ix1(ref_ply): &
                    board%stack%ixn(ref_ply)))) then
        call fail_msg("move "//token//" not available from position")
      end if
      call make_move(board, mv)
      call generate_legal_moves(board, nmoves)
      ref_ply = ref_ply + 1

      ! print *, "ref_ply=", ref_ply
      ! print *, "board%stack%ix1(ref_ply)=", board%stack%ix1(ref_ply)
      ! print *, "board%stack%ixn(ref_ply)=", board%stack%ixn(ref_ply)
    end do

    if (board%stack%current_ply /= ref_ply) then
      call fail_msg("setup did not advance ply as expected")
    end if

    ! save the sequences up to the moment
    save_state_sequence(1:ref_ply - 1) = &
      board%state_sequence(1:ref_ply - 1)
    save_move_sequence(1:ref_ply - 1) = &
      board%move_sequence(1:ref_ply - 1)

    if (get_fen(board) /= ref_fen) then
      call fail_msg("unexpected position after moves")
    end if

    if (.not. board%verify_hash()) then
      call fail_msg("initial hash code differs from full calculation")
    end if

    call save_checkpoint(board, checkpoint)

    message_txt = ""
    do depth = 1, 3

      board%request = SearchRequestT()
      call init_search(board%request, board%state%turn == WHITE, board%search)
      board%search%poll_input = .false.

      ! alphabeta search

      alphabeta_out = SearchResultT()
      call alphabeta(board, depth, 0, -MATE, MATE, &
                     alphabeta_out%move, alphabeta_out%score, &
                     alphabeta_out%nnodes)
      call check_checkpoint(board, checkpoint)

      if (board%search%is_stopped) then
        call fail_msg("alphabeta stopped before completing the test")
      end if

      ! after both moves, assert that setup actually retained two moves
      if (board%stack%current_ply /= ref_ply) then
        call fail_msg("setup did not retain moves 1")
      end if

      ! after moves and search, assert that state_sequence remained
      if (.not. all(equal_state( &
                    save_state_sequence(1:ref_ply - 1), &
                    board%state_sequence(1:ref_ply - 1)))) then
        call fail_msg("setup did not retain board%state_sequence")
      end if

      ! after moves and search assert that move_sequence remained
      if (any(save_move_sequence(1:ref_ply - 1) /= &
              board%move_sequence(1:ref_ply - 1))) then
        call fail_msg("setup did not retain move_sequence")
      end if

      ! ensure null move for terminal position
      if (nmoves == 0) then
        if (alphabeta_out%move /= 0_MVK) then
          call fail_msg("alphabeta: expected null move for terminal position")
        end if
      else if (.not. any(checkpoint%moves == alphabeta_out%move)) then
        call fail_msg("alphabeta: returned move is not legal")
      end if

      ! negamax search

      negamax_out = SearchResultT()
      call negamax(board, depth, 0, &
                   negamax_out%move, negamax_out%score, negamax_out%nnodes)
      call check_checkpoint(board, checkpoint)

      ! compare search scores and ref_score
      if (alphabeta_out%score /= negamax_out%score) then
        write (unit=message_txt, fmt='(a,i0,a,i0)') &
          " alphabeta score:", alphabeta_out%score, &
          " negamax score:", negamax_out%score
        call fail_msg(trim(message_txt))
      end if

      ! after both moves, assert that setup actually retained ply
      if (board%stack%current_ply /= ref_ply) then
        call fail_msg("setup did not retain moves 2")
      end if

      ! after moves and search, assert that state_sequence remained
      if (.not. all(equal_state( &
                    save_state_sequence(1:ref_ply - 1), &
                    board%state_sequence(1:ref_ply - 1)))) then
        call fail_msg("setup did not retain board%state_sequence")
      end if

      ! after moves and search assert that move_sequence remained
      if (any(save_move_sequence(1:ref_ply - 1) /= &
              board%move_sequence(1:ref_ply - 1))) then
        call fail_msg("setup did not retain move_sequence")
      end if

      ! ensure null move for terminal position
      if (nmoves == 0) then
        if (negamax_out%move /= 0_MVK) then
          call fail_msg("negamax: expected null move for terminal position")
        end if
      else if (.not. any(checkpoint%moves == negamax_out%move)) then
        call fail_msg("negamax: returned move is not legal")
      end if

    end do

    call pass_msg()

  end subroutine assert_moves_and_search

end program test_search_history
