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
! @bief manage searching
module SearchM

  use, intrinsic :: iso_fortran_env, only: &
    int8, int16, int32, int64, output_unit
  use MoveM, only: MVK
  use EvaluationM, only: EVK
  implicit none

  public :: &
    alphabeta, &
    negamax, &
    search_deeper, &
    init_search

  type, public :: SearchResultT
    integer(MVK) :: move = 0_MVK
    integer(EVK) :: score = 0_EVK
    integer :: depth = 0
    logical :: valid = .false.
    integer(int64) :: nnodes = 0_int64
  end type

contains

  subroutine init_search(board)

    use BoardM, only: BoardT

    class(BoardT), intent(inout) :: board

    board%stack%nodes = 0
    board%stack%qnodes = 0
  end subroutine init_search

  !! @brief iterative deepening from start, retain only completed results
  subroutine search_deeper(board, search_out)

    use, intrinsic :: iso_fortran_env, only: int64, error_unit
    use BoardM, only: BoardT, generate_legal_moves
    use ClockM, only: elapsed_time, init_limits, should_stop
    use EvaluationM, only: EVK, DRAW, MATE
    use MoveM, only: MVK, get_uci
    use PieceM, only: WHITE

    type(BoardT), intent(inout) :: board
    type(SearchResultT), intent(out) :: search_out
    integer :: nmoves

    integer :: depth, ix1, ply
    integer(int64) :: nnodes
    integer(EVK) :: score
    integer(MVK) :: move
    logical :: is_wtm

    is_wtm = (board%state%turn == WHITE)
    call init_limits(board%limits, is_wtm, board%control)
    call generate_legal_moves(board, nmoves)

    search_out = SearchResultT()

    if (nmoves == 0) then
      search_out%depth = 1
      search_out%move = 0_MVK
      search_out%valid = .true.
      return
    end if

    ! Legal fallback if interrupted before completing depth 1.
    ply = board%stack%current_ply
    ix1 = board%stack%ix1(ply)
    move = board%stack%move_list(ix1)
    nnodes = 0
    score = DRAW

    search_out%depth = 0
    search_out%move = move
    search_out%nnodes = nnodes
    search_out%score = score

    depth = 1

    do
      if (should_stop(board%control)) exit

      call alphabeta(board, depth, 0, -MATE, MATE, move, score, nnodes)

      if (board%control%is_stopped) exit ! Discard incomplete iteration.

      search_out%depth = depth
      search_out%move = move
      search_out%nnodes = nnodes
      search_out%score = score
      search_out%valid = .true.

      ! progress report; score/PV reporting separately
      write (output_unit, '(a,i0,a,i0,a,i0,a,a)') &
        "info depth ", depth, " nodes ", board%control%nodes, &
        " time ", elapsed_time(board%control), &
        " pv ", trim(get_uci(move))
      flush (output_unit)

      if (.not. board%limits%infinite) then
        if (depth >= board%limits%depth) exit
        if (elapsed_time(board%control) >= board%control%soft_ms) exit
      end if

      depth = depth + 1
    end do
  end subroutine search_deeper

  !! @brief unpruned, fixed-depth reference for checking alphabeta
  !! @details No clock checks, input polling, transposition table, or ordering.
  !! The caller supplies the same root position and depth as alphabeta.
  !! ply is distance from the search root (normally zero initially), not
  !! the move stack index. nnodes_opt returns visited nodes, including leaves.
  !! This routine does not initialize or modify board%control.
  recursive subroutine negamax( &
    board, depth, ply, best_move, score, nnodes_opt)

    use, intrinsic :: iso_fortran_env, only: int64
    use BoardM, only: BoardT, generate_legal_moves, is_check, is_draw
    use EvaluationM, only: DRAW, MATE
    use PieceM, only: WHITE
#ifdef DEBUG
    use MoveM, only: get_piece, get_from_sq
#endif

    type(BoardT), intent(inout) :: board
    integer, intent(in) :: depth, ply
    integer(MVK), intent(out) :: best_move
    integer(EVK), intent(out) :: score
    integer(int64), intent(out), optional :: nnodes_opt

    integer(MVK), allocatable :: moves(:)
    integer(MVK) :: mv, child_move
    integer(EVK) :: child_score, value
    integer :: i, nmoves, stack_ply
    integer(int64) :: nnodes, child_nnodes

    nnodes = 1_int64
    if (present(nnodes_opt)) nnodes_opt = nnodes
    score = DRAW
    best_move = 0_MVK

    ! Match alphabeta's draw-before-terminal-position precedence.
    if (is_draw(board)) return

    call generate_legal_moves(board, nmoves)
    stack_ply = board%stack%current_ply

    if (nmoves == 0) then
      if (is_check(board)) score = -MATE + int(ply, EVK)
      return
    end if

    ! Static evaluation is expressed from the side-to-move perspective.
    if (depth <= 0) then
      score = board%score
      if (board%state%turn /= WHITE) score = -score
      return
    end if

    ! Own this node's list: descendants regenerate stack move storage.
    allocate (moves(nmoves))
    moves = board%stack%move_list( &
            board%stack%ix1(stack_ply):board%stack%ixn(stack_ply))

    score = -MATE
    do i = 1, nmoves
      mv = moves(i)
#ifdef DEBUG
      if (board%stack%current_ply /= stack_ply) &
        error stop "negamax: wrong stack ply before make_move"
      if (get_piece(mv) /= board%piece_at(get_from_sq(mv))) &
        error stop "negamax: generated move disagrees with board"
#endif

      call board%make_move(mv)
      call negamax(board, depth - 1, ply + 1, &
                   child_move, child_score, child_nnodes)
      call board%unmake_move(mv)

#ifdef DEBUG
      if (board%stack%current_ply /= stack_ply) &
        error stop "negamax: unmake_move did not restore stack ply"
      if (get_piece(mv) /= board%piece_at(get_from_sq(mv))) &
        error stop "negamax: unmake_move did not restore moving piece"
#endif

      nnodes = nnodes + child_nnodes
      value = -child_score
      ! Select the first move even when its score equals the initial bound.
      if (i == 1 .or. value > score) then
        score = value
        best_move = mv
      end if
    end do

    if (present(nnodes_opt)) nnodes_opt = nnodes
  end subroutine negamax

  !! @brief simple alpha-beta negamax
  recursive subroutine alphabeta( &
    board, depth, ply, alpha, beta, best_move, score, nnodes)

    use iso_fortran_env, only: error_unit
    use BoardM, only: BoardT, generate_legal_moves, is_check, is_draw
    use ClockM, only: should_stop
    use EvaluationM, only: EVK, DRAW, MATE
    use MoveM, only: MVK
    use MoveStackM, only: sort_best
    use PieceM, only: WHITE
#ifdef DEBUG
    use MoveM, only: get_piece, get_from_sq, get_uci
#endif

    type(BoardT), intent(inout) :: board
    integer, intent(in) :: depth, ply
    integer(EVK), intent(in) :: alpha, beta
    integer(MVK), intent(out) :: best_move
    integer(EVK), intent(out) :: score
    integer(int64), intent(out) :: nnodes

    integer(MVK), allocatable :: moves(:)
    integer :: i, ix, nmoves, stack_ply
    integer(MVK) :: child_move, mv
    integer(EVK) :: a, val, board_score
    integer(int64) :: child_nnodes

    score = DRAW
    best_move = 0_MVK
    nnodes = 1

    ! when should_stop becomes true, immediately unwind
    if (should_stop(board%control)) return

    board%control%nodes = board%control%nodes + 1_int64

    ! repetition, fifty-move rule, insufficient material
    if (is_draw(board)) return

    call generate_legal_moves(board, nmoves)

    stack_ply = board%stack%current_ply

#ifdef DEBUG

    ! Own this node's list: descendants regenerate stack move storage.
    allocate (moves(nmoves))
    moves = board%stack%move_list( &
            board%stack%ix1(stack_ply):board%stack%ixn(stack_ply))

    do i = 1, nmoves
      mv = moves(i)

      if (get_piece(mv) /= board%piece_at(get_from_sq(mv))) then
        write (error_unit, *) "BAD GENERATED MOVE"
        write (error_unit, *) "depth/search ply/stack ply/index:", &
          depth, ply, stack_ply, i
        write (error_unit, *) "move/raw:", get_uci(mv), mv
        write (error_unit, *) "encoded piece/board piece:", &
          get_piece(mv), board%piece_at(get_from_sq(mv))
        flush (error_unit)
        error stop "Generated move disagrees with board"
      end if
    end do
#endif

    ! if no moves, only two scores are possible
    if (nmoves == 0) then
      if (is_check(board)) then
        score = -MATE + int(ply, int16)
      else
        score = DRAW
      end if
      return
    end if

    ! leaf node positional score with side-to-move perspective
    if (depth <= 0) then
      score = board%score
      if (board%state%turn /= WHITE) score = -score
      return
    end if

    ! lower bound for score
    a = alpha
    score = -MATE

    do ix = board%stack%ix1(stack_ply), board%stack%ixn(stack_ply)
      mv = board%stack%move_list(ix)

      ! when should_stop become true, immediately unwind
      if (should_stop(board%control)) return

      ! swap best mv to top TODO right here??
      ! call sort_best(board%stack)

#ifdef DEBUG
      if (get_piece(mv) /= board%piece_at(get_from_sq(mv))) then
        write (error_unit, *) "BAD MOVE BEFORE MAKE"
        write (error_unit, *) "depth/search ply/stack ply/index:", &
          depth, ply, board%stack%current_ply, ix
        write (error_unit, *) "move/raw:", get_uci(mv), mv
        flush (error_unit)
        error stop "move became inconsistent after generation"
      end if
#endif

      board_score = board%score

      ! try one fewer depth, one more ply, negate & swap a-b, and report bm, ce
      call board%make_move(mv)
      call alphabeta(board, depth - 1, ply + 1, -beta, -a, child_move, val, &
                     child_nnodes)
      call board%unmake_move(mv)

      if (board%score /= board_score) then
#ifdef DEBUG
        write (error_unit, *) "Evaluation not restored after ", get_uci(mv)
#endif
        write (error_unit, *) "expected:", board_score, "actual:", board%score
        error stop "make/unmake evaluation mismatch"
      end if

      nnodes = nnodes + child_nnodes

#ifdef DEBUG
      if (board%stack%current_ply /= stack_ply) &
        error stop "unmake_move did not restore stack ply"

      if (get_piece(mv) /= board%piece_at(get_from_sq(mv))) &
        error stop "unmake_move did not restore the moving piece"

      do i = 1, nmoves
        if (.not. any(moves(i) == board%stack%move_list( &
                   board%stack%ix1(stack_ply):board%stack%ixn(stack_ply)))) then
          error stop "mv missing from move_list after alphabeta:"//get_uci(mv)
        end if

      end do
#endif

      ! if stopped, val is unusable
      if (board%control%is_stopped) return

      ! negate val perspective, if val is better than score, keep bm, ce
      if (-val > score) then
        score = -val
        best_move = mv
      end if

      ! improve the lower bound
      a = max(a, -val)

      ! if temp alpha meets or exceeds beta, it is a "beta cutoff"
      if (a >= beta) exit
    end do
  end subroutine alphabeta

!
! quiescence_search(search, alpha, beta): search only captures until quiet
!
! Inside quiescence_search test input_available() before attempting a read.
! if (modulo(board%stack%nodes, 4096) == 0) then
!   if (input_available()) then
!     ! Standard input has data ready to read without blocking
!     read(unit=*, fmt="(a)", iostat=ios) cmd_str
!     if (cmd_str == "stop") then
!       ! Interrupt search immediately
!       return
!     end if
!   end if
! end if

end module SearchM
