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
module MoveStackM
  use, intrinsic :: iso_fortran_env, only: int64
  use StateM, only: StateT
  use MoveM, only: MVK
  implicit none

  ! move stack
  public :: &
    count_moves, &
    get_move, &
    next_ply, &
    prev_ply, &
    prepare_ply, &
    print_path, &
    push_move, &
    reset_movestack, &
    sort_best

  !! @brief move stack geometry
  integer, parameter, public :: &
    MAX_PLY = 100, &
    MAX_MOVES = 2000 ! high=130, low=0, avg=36

  !! @brief move stack and ply-tracking
  !! @details ix1, ixn have first & last by ply
  type :: MoveStackT
    integer(MVK), dimension(1:MAX_PLY*MAX_MOVES) :: move_list ! possible moves
    integer(MVK), dimension(1:MAX_PLY) :: path ! move sequence
    integer, dimension(1:MAX_PLY) :: ix1, ixn ! head, tail per ply
    integer :: current_ply = 1 ! absolute ply = game_ply + search_ply
    integer :: free = 1
    integer :: nodes, qnodes
  contains
    procedure :: reset_movestack
  end type MoveStackT

contains

  !! @brief count available pseudo-legal moves
  pure function count_moves(stack) result(nmoves)

    type(MoveStackT), intent(in) :: stack
    integer :: nmoves
    integer :: p

    p = stack%current_ply
    if (stack%ixn(p) >= stack%ix1(p)) then
      nmoves = stack%ixn(p) - stack%ix1(p) + 1
    else
      nmoves = 0
    end if
  end function count_moves

  !! @brief i-th move this ply
  function get_move(stack, i) result(mv)

    type(MoveStackT), intent(in) :: stack
    integer, intent(in) :: i
    integer(MVK) :: mv
    integer :: p, iaddr

    p = stack%current_ply
    iaddr = stack%ix1(p) + i - 1
    if (stack%ixn(p) >= iaddr) then
      mv = stack%move_list(iaddr)
    else
      error stop "ERROR: get_move: attempt to read empty move_list."
    end if
  end function get_move

  !! @brief push outward one ply in the search
  !! @detail by maintaining ixn(p) = ix1(p)-1, do loops skip empty lists
  subroutine next_ply(stack)

    class(MoveStackT), intent(inout) :: stack

    if (stack%current_ply >= MAX_PLY) &
      error stop "next_ply: MAX_PLY exceeded"
    stack%current_ply = stack%current_ply + 1
    call prepare_ply(stack)
  end subroutine next_ply

  !! @brief decrement ply, retain movelist
  subroutine prev_ply(stack)

    class(MoveStackT), intent(inout) :: stack

    integer :: p

    p = stack%current_ply
    if (p > 1) then
      stack%current_ply = p - 1
      stack%free = max(stack%ix1(p - 1), stack%ixn(p - 1) + 1)
    else
      error stop "ERROR: prev_ply: cannot pop ply=1"
    end if
  end subroutine prev_ply

  !! @brief prepare this ply for accepting moves
  subroutine prepare_ply(stack)

    type(MoveStackT), intent(inout) :: stack
    integer :: p

    p = stack%current_ply

    if (p == 1) then
      stack%ix1(1) = 1
      stack%ixn(1) = 0
      stack%free = 1
      stack%path(1) = 0_MVK

    else if (p > 1) then

      ! after highest used index by ply p-1
      stack%ix1(p) = stack%ixn(p - 1) + 1
      stack%ixn(p) = stack%ix1(p) - 1
      stack%free = stack%ix1(p)
      stack%path(p) = 0_MVK

    else
      error stop "prepare_ply: cannot prepare ply < 0"
    end if
  end subroutine prepare_ply

  !! @brief print uci move sequence
  subroutine print_path(stack)

    use, intrinsic :: iso_fortran_env, only: output_unit
    use MoveM, only: get_uci

    type(MoveStackT), intent(in) :: stack
    integer :: p
    character(len=10) :: uci_str

    write (output_unit, fmt="(a)", advance="no") " moves "

    do p = 1, stack%current_ply
      if (stack%path(p) /= 0_MVK) then
        uci_str = get_uci(stack%path(p))
        write (output_unit, fmt="(A,A)", advance="no") trim(uci_str), " "
      end if
    end do
    flush (unit=output_unit) ! Ensure flushes stdout before error stop
  end subroutine print_path

  !! @brief push move mv onto stack
  pure &
    subroutine push_move(stack, mv)

    type(MoveStackT), intent(inout) :: stack
    integer(MVK), intent(in) :: mv
    integer :: next_idx, p

    p = stack%current_ply
    next_idx = stack%free
    if (stack%free > MAX_PLY*MAX_MOVES) then
      error stop "push_move: move stack overflow."
    end if

    stack%move_list(next_idx) = mv
    stack%ixn(p) = next_idx
    stack%free = next_idx + 1
  end subroutine push_move

  !! @brief reset the MoveStack to the very beginning
  pure &
    subroutine reset_movestack(self)

    class(MoveStackT), intent(inout) :: self

    self%current_ply = 1
    self%ix1(1) = 1
    self%ixn(1) = 0
    self%free = 1
    self%path(1) = 0_MVK
  end subroutine reset_movestack

  !# @brief place the best score move at the top
  subroutine sort_best(stack)

    type(MoveStackT), intent(inout) :: stack

    integer(MVK) :: best_mv
    integer :: best_ix, ix, ix1, p

    p = stack%current_ply
    ix1 = stack%ix1(p)
    if (ix1 > stack%ixn(p)) then
      error stop "sort_best: cannot sort 0 moves."
    end if

    best_mv = stack%move_list(ix1)
    best_ix = ix1

    ! pick highest value mv, because priority score occupies high bits
    do ix = ix1 + 1, stack%ixn(p)
      if (stack%move_list(ix) > best_mv) then
        best_mv = stack%move_list(ix)
        best_ix = ix
      end if
    end do

    ! swap better mv to top
    if (best_ix /= ix1) then
      stack%move_list(best_ix) = stack%move_list(ix1)
      stack%move_list(ix1) = best_mv
    end if
  end subroutine sort_best

end module MoveStackM

