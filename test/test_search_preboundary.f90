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
program test_search_preboundary

  use EvaluationM, only: EVK, DRAW, MATE
  use TestUtilsM, only: begin_suite, begin_test, pass_msg, fail_msg
  implicit none

  character(len=32) :: fixture_argument

  call get_command_argument(1, fixture_argument)
  if (trim(fixture_argument) == "--infinite-fixture") then
    call run_infinite_fixture()
    stop
  end if

  call begin_suite("test_search_preboundary")

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

! call assert_search( &
!   test_name="hanging queen", &
!   fen="7k/8/8/3q4/8/8/8/K2R4 w - - 0 1", &
!   ref_nmoves_opt=12)

! call assert_search( &
!   test_name="promotion", &
!   fen="7k/P7/8/8/8/8/8/7K w - - 0 1", &
!   ref_nmoves_opt=7)

! call assert_search( &
!   test_name="legal en passant", &
!   fen="7k/8/8/3pP3/8/8/8/7K w - d6 0 1", &
!   ref_nmoves_opt=5)

! call assert_search( &
!   test_name="illegal en passant", &
!   fen="8/8/8/8/k2pP2R/8/8/3K4 b - e3 0 1", &
!   ref_nmoves_opt=6)

! call assert_search( &
!   test_name="castling", &
!   fen="r3k2r/8/8/8/8/8/8/R3K2R w KQkq - 0 1", &
!   ref_nmoves_opt=26)

contains

  ! Opt-in child process controlled by test_uci.py; never waits in normal tests.
  subroutine run_infinite_fixture()

    use, intrinsic :: iso_c_binding, only: c_int
    use, intrinsic :: iso_fortran_env, only: output_unit
    use BoardM, only: BoardT, get_board, set_fen, generate_legal_moves
    use ClockM, only: SearchRequestT
    use MoveM, only: get_uci
    use MoveStackM, only: MAX_PLY, next_ply
    use NonBlockingM, only: uci_input_unit, uci_input_close
    use SearchM, only: SearchResultT, search_deeper
    use TestUtilsM, only: PositionCheckpointT, save_checkpoint, check_checkpoint

    type(BoardT), pointer :: board
    type(PositionCheckpointT) :: checkpoint
    type(SearchResultT) :: result
    integer(c_int) :: status
    integer :: nmoves

    call begin_suite("test_search_preboundary")
    call begin_test("infinite search waits at capacity")
    board => get_board()
    call board%reset_board()
    call set_fen(board, "R6k/8/5K2/8/8/8/8/8 b - - 0 1")
    do while (board%stack%current_ply < MAX_PLY - 1)
      call next_ply(board%stack)
    end do
    call generate_legal_moves(board, nmoves)
    if (nmoves /= 1) call fail_msg("fixture requires one legal root move")
    call save_checkpoint(board, checkpoint)

    status = uci_input_unit()
    if (status /= 0_c_int) call fail_msg("input initialization failed")
    board%request = SearchRequestT()
    board%request%infinite = .true.
    write (output_unit, '(a)') "fixture_ready"
    flush (output_unit)
    call search_deeper(board, result, poll_input_opt=.true.)
    status = uci_input_close()
    if (status /= 0_c_int) call fail_msg("input cleanup failed")

    call check_checkpoint(board, checkpoint)
    if (.not. any(checkpoint%moves == result%move)) &
      call fail_msg("returned move is not legal at root")
    if (get_uci(result%move) /= "h8h7") call fail_msg("wrong retained move")
    if (result%depth /= 1) call fail_msg("wrong completed depth")
    if (result%nnodes /= 2) call fail_msg("wrong retained iteration node count")
    if (board%search%node_count /= 2) call fail_msg("nodes increased while waiting")
    if (.not. board%search%is_stopped) call fail_msg("search did not record stop")
    if (board%search%quit_requested) call fail_msg("stop requested engine shutdown")
    write (output_unit, '(a)') "fixture_result"
    flush (output_unit)
    call pass_msg()
  end subroutine run_infinite_fixture


  subroutine assert_search(test_name, fen, ref_nmoves_opt, ref_score_opt, ref_bm_opt)

    use iso_fortran_env, only: output_unit
    use BoardM, only: BoardT, get_board, get_fen, set_fen, generate_legal_moves
    use ClockM, only: SearchRequestT, init_search
    use EvaluationM, only: EVK, MATE
    use MoveM, only: MVK, get_uci
    use MoveStackM, only: MAX_PLY, next_ply
    use PieceM, only: WHITE
    use SearchM, only: alphabeta, negamax, search_deeper, SearchResultT
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
    type(SearchResultT) :: alphabeta_out, negamax_out, search_out
    type(SearchResultT) :: alphabeta_ply1_out, negamax_ply1_out
    logical :: found
    type(PositionCheckpointT) :: checkpoint

    call begin_test(test_name)

    board => get_board()
    call board%reset_board()
    call set_fen(board, fen)

    call generate_legal_moves(board, nmoves)
    call save_checkpoint(board, checkpoint)
 
    ! run alphabeta and verify position restoration at ply=1, depth=1
    alphabeta_ply1_out = SearchResultT()
    board%request = SearchRequestT()
    call init_search(board%request, board%state%turn == WHITE, board%search)
    board%search%poll_input = .false.
    call alphabeta(board, 1, 0, -MATE, MATE, &
                   alphabeta_ply1_out%move, alphabeta_ply1_out%score, &
                   alphabeta_ply1_out%nnodes)
    call check_checkpoint(board, checkpoint)
 
    ! run negamax and verify position restoration at ply=1, depth=1
    negamax_ply1_out = SearchResultT()
    call negamax(board, 1, 0, &
                 negamax_ply1_out%move, negamax_ply1_out%score, &
                 negamax_ply1_out%nnodes)
    call check_checkpoint(board, checkpoint)


    do while (board%stack%current_ply < MAX_PLY - 1)
      call next_ply(board%stack)
    end do

    call generate_legal_moves(board, nmoves)

    if (board%stack%current_ply /= MAX_PLY - 1) then
      call fail_msg("fixture did not reach MAX_PLY - 1")
    end if

    message_txt = ""

    ! if search fen, hash, or ref_nmoves_opt are mismatched, fail
    if (fen /= get_fen(board)) then
      call fail_msg("initial FEN differs from requested position")
    elseif (.not. board%verify_hash()) then
      call fail_msg("initial hash code differs from full calculation")
    elseif (present(ref_nmoves_opt)) then
      if (nmoves /= ref_nmoves_opt) &
        call fail_msg("nmoves:"//to_str(nmoves)// &
                      " ref_nmoves_opt:"//to_str(ref_nmoves_opt))
    end if

    ! Establish a new baseline after intentionally relocating the stack.
    call save_checkpoint(board, checkpoint)

    do depth = 1, 3, 2

      alphabeta_out = SearchResultT()

      board%request = SearchRequestT()
      call init_search(board%request, board%state%turn == WHITE, board%search)
      board%search%poll_input = .false.

      call alphabeta(board, depth, 0, -MATE, MATE, &
                     alphabeta_out%move, alphabeta_out%score, &
                     alphabeta_out%nnodes)
      call check_checkpoint(board, checkpoint)

      if (nmoves == 0) then
        if (alphabeta_out%move /= 0_MVK) then
          call fail_msg("alphabeta: expected null move for terminal root")
        end if
      else
        if (.not. any(alphabeta_out%move == checkpoint%moves)) then
          call fail_msg("alphabeta: returned move is not legal")
        end if
      end if

      if (alphabeta_out%score /= alphabeta_ply1_out%score) then
        call fail_msg("alphabeta: score differs from ordinary depth=1 reference")
      end if

      if (board%search%is_stopped) then
        call fail_msg("alphabeta: stopped before completing the test")
      end if

      if (alphabeta_out%nnodes /= alphabeta_ply1_out%nnodes) then
        call fail_msg("alphabeta: nodes differ from ordinary depth=1 reference")
      end if
      if (alphabeta_out%nnodes /= board%search%node_count) then
        call fail_msg("alphabeta: node counts disagree")
      end if
      if (alphabeta_out%nnodes /= 1 + nmoves) then
        call fail_msg("alphabeta: node counts disagree with 1 + nmoves")
      end if

      negamax_out = SearchResultT()
      call negamax(board, depth, 0, &
                   negamax_out%move, negamax_out%score, negamax_out%nnodes)
      call check_checkpoint(board, checkpoint)

      if (nmoves == 0) then
        if (negamax_out%move /= 0_MVK) then
          call fail_msg("negamax: expected null move for terminal root")
        end if
      else
        if (.not. any(negamax_out%move == checkpoint%moves)) then
          call fail_msg("negamax: returned move is not legal")
        end if
      end if

      if (negamax_out%score /= negamax_ply1_out%score) then
        call fail_msg("negamax: score differs from ordinary depth=1 reference")
      end if

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

      ! verify that search visited the root and legal children only
      if (negamax_out%nnodes /= negamax_ply1_out%nnodes) then
        call fail_msg("negamax: nodes differ from ordinary depth=1 reference")
      end if
      if (negamax_out%nnodes /= 1 + nmoves) then
        call fail_msg("negamax: node counts disagree with 1 + nmoves")
      end if

      ! if searched move and not one of the ref_bm_opt are matched, fail
      if (present(ref_bm_opt)) then
        ipos = 1
        found = .false.
        do
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

    ! set the driver's requested depth
    board%request = SearchRequestT()
    board%request%depth_limit = 3
    board%request%infinite = .false.

    call search_deeper(board, search_out, poll_input_opt=.false.)
    call check_checkpoint(board, checkpoint)

    ! if terminal root
    if (nmoves == 0) then
      if (search_out%move /= 0_MVK) then
        call fail_msg("search_deeper: expected null move for terminal root")
      end if

    ! if non-terminal root
    else
      if (.not. any(checkpoint%moves == search_out%move)) then
        call fail_msg("search_deeper: returned move is not legal")
      end if

      if (search_out%depth /= 1) then
        call fail_msg("search_out: depth /= 1 for MAX_ply - 1")
      end if

       if (search_out%nnodes /= 1 + nmoves) then
         call fail_msg("search_deeper: expected root (1) plus legal children count")
       end if
    end if

    ! compare search_deeper score and negamax score, (effectively depth=1 for MAX_PLY - 1)
    if (search_out%score /= negamax_out%score) then
      write (unit=message_txt, fmt='(a,i0,a,i0)') &
        " search_deeper score:", search_out%score, &
        " negamax score:", negamax_out%score
      call fail_msg(trim(message_txt))
    end if


    call pass_msg()

  end subroutine assert_search

end program test_search_preboundary
