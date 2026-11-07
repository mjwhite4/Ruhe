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
program test_ttable
  use, intrinsic :: iso_fortran_env, only: int8, int16
  use TestUtilsM, only: begin_suite, begin_test, pass, fail
  use TTableM, only: &
    reset_ttable, probe_ttable, store_ttable, EntryT, TT, TT_MASK, TT_EXACT
  use ZobristM, only: ZHK
  use EvaluationM, only: EVK
  use MoveStackM, only: MVK
  implicit none

  call begin_suite("test_ttable")

  call test_reset_and_allocation()
  call test_store_and_probe_hit_miss()
  call test_replacement_policy()

contains

  !-----------------------------------------------------------------------------
  ! Test initialization, allocation size, and mask mask computation
  !-----------------------------------------------------------------------------
  subroutine test_reset_and_allocation()
    logical :: ok

    call begin_test("reset and allocation")

    ! Initialize table with 2^4 = 16 entries
    call reset_ttable(4, ok)

    if (.not. ok) call fail("Failed to allocate transposition table")
    if (.not. allocated(TT)) call fail("TT array is not allocated")
    if (size(TT) /= 16) call fail("TT size is not equal to 16")
    if (TT_MASK /= 15_ZHK) call fail("TT_MASK mask calculation mismatch")

    call pass()
  end subroutine test_reset_and_allocation

  !-----------------------------------------------------------------------------
  ! Test basic store, probe hit, and probe miss behavior
  !-----------------------------------------------------------------------------
  subroutine test_store_and_probe_hit_miss()
    logical :: ok, found
    integer(kind=ZHK) :: hash_key
    type(EntryT) :: item, result_item

    call begin_test("store and probe hit/miss")

    call reset_ttable(8, ok) !! 256 entries

    ! Probe non-existent hash key
    hash_key = 123456789_ZHK
    call probe_ttable(hash_key, result_item, found)
    if (found) call fail("Found entry before storing")

    ! Prepare entry to store
    item%hash = hash_key
    item%score = 150_EVK
    item%mv = 12_MVK
    item%depth = 4_int8
    item%flag = TT_EXACT
    item%age = 1_int16

    call store_ttable(hash_key, item)

    ! Probe existing key
    call probe_ttable(hash_key, result_item, found)
    if (.not. found) call fail("Stored entry was not found")
    if (result_item%score /= 150_EVK) call fail("Stored score mismatch")
    if (result_item%depth /= 4_int8) call fail("Stored depth mismatch")
    if (result_item%flag /= TT_EXACT) call fail("Stored flag mismatch")

    call pass()
  end subroutine test_store_and_probe_hit_miss

  !-----------------------------------------------------------------------------
  ! Test replacement rules (empty slot, deeper search, staler search)
  !-----------------------------------------------------------------------------
  subroutine test_replacement_policy()
    logical :: ok, found
    integer(kind=ZHK) :: hash1, hash2
    type(EntryT) :: e1, e2, res

    call begin_test("replacement policy")

    call reset_ttable(4, ok) !! 16 entries

    ! Pick two different hashes that collide on index (e.g. 1 and 17 for size 16)
    hash1 = 1_ZHK
    hash2 = 17_ZHK

    ! Setup shallow entry e1
    e1%hash = hash1
    e1%score = 100_EVK
    e1%depth = 2_int8
    e1%age = 1_int16
    call store_ttable(hash1, e1)

    ! Setup deeper colliding entry e2
    e2%hash = hash2
    e2%score = 200_EVK
    e2%depth = 5_int8
    e2%age = 1_int16

    ! Deeper entry (depth 5 >= depth 2) should overwrite
    call store_ttable(hash2, e2)

    call probe_ttable(hash2, res, found)
    if (.not. found) call fail("Deeper entry failed to replace shallower entry")
    if (res%score /= 200_EVK) call fail("Deeper entry score mismatch")

    call pass()
  end subroutine test_replacement_policy

end program test_ttable
