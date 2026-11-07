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
!---

!! @brief clock and time control
module ClockM

  use, intrinsic :: iso_fortran_env, only: int8, int16, int32, int64
  use iso_fortran_env, only: output_unit

  implicit none

  private
  public :: &
    elapsed_time, &
    init_limits, &
    limits_reached, &
    poll_search_commands, &
    should_stop

  type, public :: LimitsT
    integer :: depth = huge(0)
    integer(int64) :: nodes = huge(0_int64)
    integer(int64) :: movetime = -1  ! milliseconds; -1 means absent
    integer(int64) :: wtime = -1, btime = -1
    integer(int64) :: winc = 0, binc = 0
    integer :: movestogo = 0
    logical :: infinite = .false.
    logical :: ponder = .false.
  end type LimitsT

  type, public :: ControlT
    integer(int64) :: nodes = 0
    integer(int64) :: max_nodes = huge(0_int64)
    integer(int64) :: start_time = 0
    integer(int64) :: clock_rate = 0
    integer(int64) :: soft_ms = huge(0_int64)
    integer(int64) :: hard_ms = huge(0_int64)
    logical :: is_stopped = .false.
    logical :: quit_requested = .false.
    logical :: poll_input = .true.
  end type

contains

  ! @brief elapsed wall time
  function elapsed_time(control) result(ms)

    type(ControlT), intent(in) :: control
    integer(int64) :: ms
    integer(int64) :: now

    call system_clock(now)
    ! Assume the clock increases without wrapping
    ms = 1000*(now - control%start_time)/control%clock_rate
  end function elapsed_time

  ! @brief fix default search Control, limits
  subroutine init_limits(limits, is_wtm, control)

    type(LimitsT), intent(in) :: limits
    logical, intent(in) :: is_wtm
    type(ControlT), intent(out) :: control

    integer(int64) :: &
      remaining_time, increment_time, available_time, target_time
    integer :: moves

    call system_clock(control%start_time, control%clock_rate)
    if (control%clock_rate <= 0) error stop "No system clock"

    control%max_nodes = limits%nodes

    if (limits%infinite) return

    if (limits%movetime >= 0) then
      control%hard_ms = limits%movetime
      control%soft_ms = control%hard_ms
      return
    end if

    if (is_wtm) then
      remaining_time = limits%wtime
      increment_time = limits%winc
    else
      remaining_time = limits%btime
      increment_time = limits%binc
    end if

    if (remaining_time < 0) return  ! No clock supplied.

    moves = limits%movestogo
    if (moves <= 0) moves = 30 ! Heuristic for sudden-death Control.

    ! Reserve 50 ms for scheduling, communication, and returning the move.
    available_time = max(0_int64, remaining_time - 50_int64)

    target_time = available_time/moves + (increment_time*4)/5

    control%soft_ms = min(available_time, target_time)
    control%hard_ms = min(available_time, 3*target_time)
  end subroutine init_limits

  ! @brief helper
  pure logical function limits_reached(control, elapsed_ms) result(reached)

    type(ControlT), intent(in) :: control
    integer(int64), intent(in) :: elapsed_ms

    reached = control%is_stopped .or. &
              control%nodes >= control%max_nodes .or. &
              elapsed_ms >= control%hard_ms
  end function

  ! @brief attend (non-blocking) any command input during search
  subroutine poll_search_commands(control)

    use iso_c_binding, only: c_int
    use iso_fortran_env, only: output_unit
    use NonBlockingM, only: uci_next_command, &
                           READ_EOF, READ_NO_LINE, READ_IOS_ERROR, READ_OVERFLOW

    type(ControlT), intent(inout) :: control
    character(len=8192) :: command
    integer(c_int) :: status
    integer :: i

    ! Bound the work per call so input cannot starve the search.
    do i = 1, 16
      call uci_next_command(command, status)

      select case (status)
      case (READ_NO_LINE)
        return

      case (READ_EOF)
        control%is_stopped = .true.
        control%quit_requested = .true.
        return

      case (READ_IOS_ERROR, READ_OVERFLOW)
        control%is_stopped = .true.
        control%quit_requested = .true.
        ! control%input_failed = .true.
        return
      end select

      command = adjustl(command)

      select case (trim(command))
      case ("")
        cycle

      case ("stop")
        control%is_stopped = .true.
        return

      case ("quit")
        control%is_stopped = .true.
        control%quit_requested = .true.
        return

      case ("isready")
        write (output_unit, '(a)') "readyok"
        flush (output_unit)

      case default
        ! Minimal example: ignore other commands during search.
        ! If supporting deferred commands, enqueue them here.
        ! Do not change the board while negamax is using it.
      end select
    end do
  end subroutine poll_search_commands

  ! @brief shared stop test
  function should_stop(control) result(is_stopped)

    type(ControlT), intent(inout) :: control
    logical :: is_stopped

    if (control%poll_input) call poll_search_commands(control)
    control%is_stopped = limits_reached(control, elapsed_time(control))
    is_stopped = control%is_stopped
  end function should_stop

end module ClockM
