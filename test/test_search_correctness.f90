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
program test_search_correctness

  use EvaluationM, only: EVK, DRAW, MATE
  use TestUtilsM, only: begin_suite, begin_test, pass_msg, fail_msg
  implicit none

  call begin_suite("test_search_correctness")

  call assert_search( &
    test_name="mate in one", &
    fen="7k/8/5KQ1/8/8/8/8/8 w - - 0 1", &
    ref_nmoves_opt=23, &
    ref_bm_opt="g6g7", &
    ref_score_opt=MATE - 1_EVK)

  call assert_search( &
    test_name="only one legal move", &
    fen="R6k/8/5K2/8/8/8/8/8 b - - 0 1", &
    ref_nmoves_opt=1, &
    ref_bm_opt="h8h7")

  call assert_search( &
    test_name="checkmate", &
    fen="7k/6Q1/5K2/8/8/8/8/8 b - - 0 1", &
    ref_nmoves_opt=0, &
    ref_bm_opt="0000", &
    ref_score_opt=-MATE)

  call assert_search( &
    test_name="stalemate", &
    fen="7k/5Q2/6K1/8/8/8/8/8 b - - 0 1", &
    ref_nmoves_opt=0, &
    ref_bm_opt="0000", &
    ref_score_opt=DRAW)

  call assert_search( &
    test_name="hanging queen", &
    fen="7k/8/8/3q4/8/8/8/K2R4 w - - 0 1", &
    ref_nmoves_opt=12)

  call assert_search( &
    test_name="promotion", &
    fen="7k/P7/8/8/8/8/8/7K w - - 0 1", &
    ref_nmoves_opt=7)

  call assert_search( &
    test_name="legal en passant", &
    fen="7k/8/8/3pP3/8/8/8/7K w - d6 0 1", &
    ref_nmoves_opt=5)

  call assert_search( &
    test_name="illegal en passant", &
    fen="8/8/8/8/k2pP2R/8/8/3K4 b - e3 0 1", &
    ref_nmoves_opt=6)

  call assert_search( &
    test_name="castling", &
    fen="r3k2r/8/8/8/8/8/8/R3K2R w KQkq - 0 1", &
    ref_nmoves_opt=26)

contains

  subroutine assert_search(test_name, fen, ref_nmoves_opt, ref_score_opt, ref_bm_opt)

    use BoardM, only: BoardT, get_board, get_fen, set_fen, generate_legal_moves
    use ClockM, only: SearchRequestT, init_search
    use EvaluationM, only: EVK, MATE
    use MoveM, only: MVK, get_uci
    use PieceM, only: WHITE
    use SearchM, only: alphabeta, negamax, SearchResultT
    use StringUtilsM, only: next_token, to_str
    use TestUtilsM, only: &
      PositionCheckpointT, save_checkpoint, check_checkpoint

    character(len=*), intent(in) :: test_name, fen
    integer, intent(in), optional :: ref_nmoves_opt
    integer(EVK), intent(in), optional :: ref_score_opt
    character(len=*), intent(in), optional :: ref_bm_opt

    type(BoardT), pointer :: board
    integer :: depth, ipos, nmoves
    character(len=512) :: message_txt
    character(len=:), allocatable :: token
    type(SearchResultT) :: alphabeta_out, negamax_out
    logical :: found
    type(PositionCheckpointT) :: checkpoint

    call begin_test(test_name)

    board => get_board()
    call board%reset_board()
    call set_fen(board, fen)
    call generate_legal_moves(board, nmoves)

    message_txt = ""

    if (fen /= get_fen(board)) then
      call fail_msg("initial FEN differs from requested position")
    elseif (.not. board%verify_hash()) then
      call fail_msg("initial hash code differs from full calculation")
    end if

    call save_checkpoint(board, checkpoint)

    ! if search nmoves and ref_nmoves_opt are mismatched, fail
    if (present(ref_nmoves_opt)) then
      if (nmoves /= ref_nmoves_opt) &
        call fail_msg("nmoves:"//to_str(nmoves)// &
                      " ref_nmoves_opt:"//to_str(ref_nmoves_opt))
    end if

    do depth = 1, 3

      alphabeta_out = SearchResultT()

      board%request = SearchRequestT()
      call init_search(board%request, board%state%turn == WHITE, board%search)
      board%search%poll_input = .false.

      call alphabeta(board, depth, 0, -MATE, MATE, &
                     alphabeta_out%move, alphabeta_out%score, &
                     alphabeta_out%nnodes)

      ! ensure position restoration after search
      call check_checkpoint(board, checkpoint)

      if (board%search%is_stopped) then
        call fail_msg("alphabeta stopped before completing the test")
      end if

      ! ensure null move for terminal root position (without legal moves)
      if (nmoves == 0) then
        if (alphabeta_out%move /= 0_MVK) then
          call fail_msg("alphabeta: expected null move for terminal position")
        end if

      ! check alphabeta move against saved root movelist
      else if (.not. any(checkpoint%moves == alphabeta_out%move)) then
        call fail_msg("alphabeta: returned move is not legal")
      end if

      negamax_out = SearchResultT()
      call negamax(board, depth, 0, &
                   negamax_out%move, negamax_out%score, negamax_out%nnodes)

      ! ensure position restoration after search
      call check_checkpoint(board, checkpoint)

      ! compare search scores and ref_score_opt
      if (alphabeta_out%score /= negamax_out%score) then
        write (unit=message_txt, fmt='(a,i0,a,i0)') &
          " alphabeta score:", alphabeta_out%score, &
          " negamax score:", negamax_out%score
        call fail_msg(trim(message_txt))
      else if (present(ref_score_opt)) then
        if (negamax_out%score /= ref_score_opt) then
          write (unit=message_txt, fmt='(a,i0,a,i0)') &
            " negamax score:", negamax_out%score, &
            " ref_score_opt:", ref_score_opt
          call fail_msg(trim(message_txt))
        end if
      end if

      ! ensure null move for terminal root position (without legal moves)
      if (nmoves == 0) then
        if (negamax_out%move /= 0_MVK) then
          call fail_msg("negamax: expected null move for terminal position")
        end if

      ! check negamax move against saved legal move list
      else if (.not. any(checkpoint%moves == negamax_out%move)) then
        call fail_msg("negamax: returned move is not legal")
      end if

      ! if searched move and not one of the ref_bm_opt are matched, fail
      if (present(ref_bm_opt)) then
        ipos = 1
        found = .false.
        do
          ! accept comma separated list of ref_bm_opt, allowing ties
          call next_token(ref_bm_opt, ",", ipos, token)
          if (token == "") exit
          if (get_uci(alphabeta_out%move) == token) then
            found = .true.
            exit
          end if
        end do
        if (.not. found) then
          call fail_msg(" | bm:"//get_uci(alphabeta_out%move)//" /= "// &
                        "ref_bm_opt:"//trim(ref_bm_opt))
        end if
      end if
    end do

    call pass_msg()

  end subroutine assert_search

end program test_search_correctness
