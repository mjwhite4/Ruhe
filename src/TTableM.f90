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
!! @brief maintain an internal memory hash table.

module TTableM

  use, intrinsic :: iso_fortran_env, only: int8, int16, int64
  use EvaluationM, only: EVK
  use ZobristM, only: ZHK
  use MoveStackM, only: MVK
  implicit none
  private

  public :: &
    reset_ttable, &
    probe_ttable, &
    store_ttable

  !! @brief search bound flags
  integer(kind=int8), parameter, public :: &
    TT_NONE = 0_int8, & ! nothing decided
    TT_EXACT = 1_int8, & ! an exact (pv) value
    TT_ALPHA = 2_int8, & ! an alpha (upper) cutoff
    TT_BETA = 3_int8    ! a beta (lower) cutoff

  !! @brief single entry in the hash table (Hash, Score, Depth, Flag)
  type, public :: EntryT
    integer(kind=ZHK)   :: hash = 0_ZHK    !! 8 bytes
    integer(kind=EVK)   :: score = 0_EVK   !! 2 bytes
    integer(kind=MVK)   :: mv = 0_MVK      !! 2 bytes (bm index)
    integer(kind=int8)  :: depth = -1_int8 !! 1 byte
    integer(kind=int8)  :: flag = 0_int8   !! 1 byte (EXACT, ALPHA, BETA)
    integer(kind=int16) :: age = 0_int16   !! 2 bytes (to handle old searches)
  end type

  !! @brief A large array of EntryT
  type(EntryT), dimension(:), allocatable, save, public :: TT
  integer(ZHK), save, public :: TT_MASK = 0_ZHK

contains

  !! @brief initialize the bounds t-table.
  !! @details log2size replaces modulo(h, with iand(h, TT_MASK)
  subroutine reset_ttable(log2size, success)

    integer, intent(in) :: log2size
    logical, intent(out) :: success
    integer(int64) :: n_entries
    integer :: istat

    n_entries = ishft(1_int64, log2size)
    if (allocated(TT)) deallocate (TT)

    allocate (TT(0:n_entries - 1), stat=istat)
    success = (istat == 0) !! STAT=0 is success

    if (success) then
      TT_MASK = int(size(TT, kind=int64) - 1, ZHK)
      TT%hash = 0_ZHK !! init all slots to empty
    end if
  end subroutine reset_ttable

  !! @brief fetch a bound from the table
  subroutine probe_ttable(h, found_item, is_found)

    integer(kind=ZHK), intent(in) :: h
    type(EntryT), intent(out) :: found_item
    logical, intent(out) :: is_found
    integer(ZHK) :: ix

    ! bitwise iand on an arg = 2**n-1 is faster than modulo
    ix = iand(h, TT_MASK)
    found_item = TT(ix)
    is_found = (found_item%hash == h)
  end subroutine probe_ttable

  !! @brief store a bound in the table
  subroutine store_ttable(h, item_to_store)

    integer(kind=ZHK), intent(in) :: h
    type(EntryT), intent(in) :: item_to_store
    integer(kind=ZHK) :: ix

    ix = iand(h, TT_MASK)

    !! @brief replace if empty, OR new is deeper, OR old is stale
    if ( &
      TT(ix)%hash == 0_ZHK .or. &
      item_to_store%depth >= TT(ix)%depth .or. &
      item_to_store%age > TT(ix)%age) then
      TT(ix) = item_to_store
      TT(ix)%hash = h !! Safety: ensure has is linked to index
    end if
  end subroutine store_ttable

end module TTableM
