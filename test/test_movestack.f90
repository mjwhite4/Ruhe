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
program test_movestack

  use TestUtilsM, only: begin_suite, begin_test, pass, fail
  use MoveStackM
  implicit none

  type(MoveStackT), allocatable :: stack

  call begin_suite("test_movestack")

  ! -------------------------------------------------------------------
  ! Reset and Initial Ply State
  ! -------------------------------------------------------------------
  call begin_test("reset_movestack and initial ply state")

  allocate (stack)
  call stack%reset_movestack()

  if (stack%current_ply /= 1 .or. &
      count_moves(stack) /= 0 .or. &
      stack%free /= 1) then
    call fail("initial movestack pointer or ply state invalid")
  end if

  call pass()

  ! -------------------------------------------------------------------
  ! Move Insertion and Retrieval
  ! -------------------------------------------------------------------
  call begin_test("push_move, count_moves, and get_move")

  call push_move(stack, 100_MVK)
  call push_move(stack, 200_MVK)
  call push_move(stack, 300_MVK)

  if (count_moves(stack) /= 3) call fail("move count mismatch after pushing 3 moves")

  if (get_move(stack, 1) /= 100_MVK .or. &
      get_move(stack, 2) /= 200_MVK .or. &
      get_move(stack, 3) /= 300_MVK) then
    call fail("inserted move value mismatch")
  end if

  call pass()

  ! -------------------------------------------------------------------
  ! Ply Navigation (next_ply / prev_ply)
  ! -------------------------------------------------------------------
  call begin_test("next_ply and prev_ply traversal")

  deallocate (stack)
  allocate (stack)
  call stack%reset_movestack()

  ! Ply 1
  call push_move(stack, 11_MVK)
  call push_move(stack, 12_MVK)

  ! Move to Ply 2
  call next_ply(stack)

  if (stack%current_ply /= 2 .or. count_moves(stack) /= 0) then
    call fail("ply increment failed or ply 2 is not empty")
  end if

  call push_move(stack, 21_MVK)
  call push_move(stack, 22_MVK)
  call push_move(stack, 23_MVK)

  if (count_moves(stack) /= 3 .or. get_move(stack, 1) /= 21_MVK) then
    call fail("ply 2 move stack corrupted")
  end if

  ! Pop back to Ply 1
  call prev_ply(stack)

  if (stack%current_ply /= 1 .or. count_moves(stack) /= 2) then
    call fail("ply decrement failed or ply 1 count changed")
  end if

  if (get_move(stack, 1) /= 11_MVK .or. get_move(stack, 2) /= 12_MVK) then
    call fail("ply 1 move contents corrupted after returning from ply 2")
  end if

  call pass()

  ! -------------------------------------------------------------------
  ! Move Sorting Priority
  ! -------------------------------------------------------------------
  call begin_test("sort_best move ordering")

  deallocate (stack)
  allocate (stack)
  call stack%reset_movestack()

  call push_move(stack, 50_MVK)
  call push_move(stack, 500_MVK)  ! Highest priority move
  call push_move(stack, 250_MVK)

  call sort_best(stack)

  if (get_move(stack, 1) /= 500_MVK) then
    call fail("highest priority move not placed at top after sort_best")
  end if

  call pass()

end program test_movestack
