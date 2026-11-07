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
! @brief PERFormance Testing count leaf nodes and elapsed time at ply depth
module PerftM

  use, intrinsic :: iso_fortran_env, only: int64, real64
  use SquareM, only: SQK
  use MoveStackM, only: MVK, MAX_MOVES, get_move
  use MoveM, only: get_uci
  implicit none
  private
  public :: &
    run_perft, &
    perft

contains

  !! enumerate leaf-nodes at depth
  recursive subroutine perft(board, depth, nodes, breakdown_opt)

    use BaseM, only: print_mailbox
    use BoardM, only: BoardT, make_move, unmake_move
#ifdef DEBUG
    use BoardM, only: get_fen
#endif

    ! use BoardM, only: is_check, generate_legal_moves, get_fen

    use iso_fortran_env, only: error_unit, output_unit
    use MoveStackM, only: prepare_ply
    use MoveM, only: get_full_move

    class(BoardT), intent(inout) :: board
    integer, intent(in) :: depth ! remaining search depth (decrements each)
    integer(kind=int64), intent(out) :: nodes
    logical, intent(in), optional :: breakdown_opt

    logical :: do_breakdown
    integer :: i, nmoves, p
    integer(kind=int64) :: subtree_nodes
    integer(kind=MVK) :: mv
    logical :: tf

#ifdef DEBUG
    character(len=147) :: fen_before, fen_after
#endif

    ! reset accumulators
    nodes = 0_int64

    ! base leaf count for depth == 0
    if (depth == 0) then
      nodes = 1_int64
      return
    end if

    ! whether breakdown reporting this call
    do_breakdown = .false.
    if (present(breakdown_opt)) do_breakdown = breakdown_opt

    call board%generate_legal_moves(nmoves)

    ! return on depth == 1 only if no breakdown
    if (depth == 1 .and. .not. do_breakdown) then
      !PRINTt *, "L56: depth == 1 .and. .not. do"
      nodes = int(nmoves, kind=int64)
      return
    end if

    nodes = 0_int64

    !! tally sub-tree move counts
    p = board%stack%current_ply
    do i = board%stack%ix1(p), board%stack%ixn(p)
      mv = board%stack%move_list(i)

#ifdef DEBUG
      ! print *, "fen=", get_fen(board)
      ! call print_board_stamp(board)

      ! print *, "L67: considering move, mv=", get_uci(mv)

      fen_before = get_fen(board)
      if (.not. board%verify_hash()) then
        print '(a)', trim(fen_before)
        error stop "L79: verify_hash fail"
      end if
#endif

      ! if move mv would place king in check
      call board%is_into_check(mv, tf)
      if (tf) then
        PRINT *, "L65: skipping is_into_check: mv= ", get_uci(mv)
        cycle ! skip illegal move
      end if

      ! move on the board
      call make_move(board, mv)

#ifdef DEBUG
      if (.not. board%verify_hash()) then
        print '(a, a, a, a)', "position fen ", trim(fen_before), &
          ' moves ', get_uci(mv)
        error stop "L98: verify_hash fail"
      end if
#endif

      ! recurse into the tree for valid legal move
      call perft(board, depth - 1, subtree_nodes, breakdown_opt=.false.)
      ! Safe alternative without slicing bounds risks:

      ! (always) unmake move to restore board state
      call unmake_move(board, mv)

#ifdef DEBUG
      fen_after = get_fen(board)
      if (fen_before /= fen_after) then
        write (error_unit, "(a,a)") "fen_before=", trim(fen_before)
        write (error_unit, "(a,a)") "fen_after =", trim(fen_after)
        error stop "perft: unmake_move failed to restore original state"
      end if
#endif

      nodes = nodes + subtree_nodes

      ! tally nodes by move
      if (do_breakdown) then
        write (output_unit, "(a,a,i0)") get_full_move(mv), ": ", subtree_nodes
      end if
    end do

  end subroutine perft

  !@brief convert clocks to elapsed time
! pure function elapsed_time(clock_start, clock_rate, clock_max, clock_end) &
!   result(sec)

!   integer(kind=int64), intent(in) :: clock_start, clock_rate, clock_max, &
!                                      clock_end
!   real(kind=real64) :: sec

!   integer(kind=int64) :: clock_elapsed

!   if (clock_end >= clock_start) then
!     clock_elapsed = clock_end - clock_start
!   else
!     ! addition by clock_max prevents clock count wrap-around
!     clock_elapsed = clock_end + clock_max - clock_start
!   end if
!   sec = real(clock_elapsed, real64)/real(clock_rate, real64)
! end function elapsed_time

  !! @brief set up call to recursive perft
  subroutine run_perft(board, depth)

    use BoardM, only: BoardT
    use MoveStackM, only: reset_movestack

    type(BoardT), intent(inout) :: board
    integer, intent(in) :: depth
    integer(kind=int64) :: total_nodes

    integer(kind=int64) :: clock_start, clock_end, clock_rate
    real(kind=real64) :: sec, nps

    total_nodes = 0_int64

    !! log elapsed real-time and NPS
    call system_clock(clock_start, clock_rate)
    call reset_movestack(board%stack)
    call perft(board, depth, total_nodes, breakdown_opt=.true.)
    call system_clock(clock_end)

    sec = real(clock_end - clock_start, real64)/real(clock_rate, real64)
    nps = merge(real(total_nodes, real64)/sec, 0.0_real64, &
                sec > 0.0_real64)

  end subroutine run_perft

end module PerftM
