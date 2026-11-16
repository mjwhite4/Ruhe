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
    elapsed_time_ms, &
    init_limits, &
    limits_reached, &
    poll_search_commands, &
    should_stop

  ! requested search constraints for one search
  type, public :: LimitsT

    integer :: depth_limit = huge(0)
      ! max requested search depth in plies from root

    integer(int64) :: node_limit = huge(0_int64)
      ! max cumulative node count across all iterations

    integer(int64) :: movetime_ms = -1_int64   ! milliseconds; -1 means absent
      ! a negative value means absent; zero requests immediate stop
      ! when supplied, takes precedence over game-clock allocation

    integer(int64) :: &
            white_time_ms = -1, &
            black_time_ms = -1
      ! remaning clock time; negative if absent (not in effect)

    integer(int64) :: &
            white_increment_ms = 0, &
            black_increment_ms = 0
      ! time added to clock per move

    integer :: moves_to_go = 0
      ! moves remaining until the next time control
      ! zero means unspecified; allocation currently assumes 30 moves

    logical :: infinite = .false.
      ! (t/f) whether infinite search mode: suppress iterative deepening depth/time exits
      ! do enforce node limit and responds to stop, quit, EOF, and input errors

    logical :: ponder = .false.
      ! record the UCI ponder request
      ! ponder behavior is not presently implemented
  end type LimitsT


  ! runtime state and derived search budget for one search
  type, public :: ControlT

    integer(int64) :: node_count = 0
      ! cumulative alpha-beta admitted after the entry stop check,
      ! including root and leaf nodes, across all deepening iterations.
      ! An entry rejected by the stop check does not increase this count

    integer(int64) :: node_limit = huge(0_int64)
      ! active node budget copied from LimitsT%node_limit
      ! reaching this count causes stopping at the next stop check.

    integer(int64) :: start_tick = 0
      ! SYSTEM_CLOCK count captured when search control is initialized.
      ! A clock reading in ticks, not a duration in milliseconds

    integer(int64) :: clock_ticks_per_second = 0
      ! SYSTEM_CLOCK count rate used to convert ticks to time
      ! must be positive before calling elapsed_time_ms

    integer(int64) :: soft_ms = huge(0_int64)
      ! target duration
      ! check after completing depth increment to decide whether to stop before another iteration

    integer(int64) :: hard_ms = huge(0_int64)
      ! maximum allocated duration
      ! checked within search; reaching it interrupts the iteration
      ! actual time depends on checking frequency and unwinding

    logical :: is_stopped = .false.
      ! search-stop state: set when the budget is exhausted, stop, quit, EOF, or input error
      ! remains true until control is reinitialized

    logical :: quit_requested = .false.
      ! whther to terminate the engine after search unwinds
      ! set by quit, EOF, or input error; ordinary budget exhaustion or stop does not set it

    logical :: poll_input = .true.
      ! (t/f) enable non-blocking command pollling during search stop checks.
      ! set false for internal tests that must not consume STDIN
  end type ControlT

contains

  ! @brief report elapsed wall time
  function elapsed_time_ms(control) result(ms)

    type(ControlT), intent(in) :: control
    integer(int64) :: ms
    integer(int64) :: now_tick

    call system_clock(now_tick)
    ! Assume the clock increases without wrapping
    ms = 1000*(now_tick - control%start_tick)/control%clock_ticks_per_second
  end function elapsed_time_ms

  ! @brief fix default search Control, limits
  subroutine init_limits(limits, is_wtm, control)

    type(LimitsT), intent(in) :: limits
    logical, intent(in) :: is_wtm
    type(ControlT), intent(out) :: control

    integer(int64) :: &
      remaining_ms, increment_ms, available_ms, target_ms
    integer :: moves

    call system_clock(control%start_tick, control%clock_ticks_per_second)
    if (control%clock_ticks_per_second <= 0) error stop "No system clock"

    control%node_limit = limits%node_limit

    if (limits%infinite) return

    if (limits%movetime_ms >= 0) then
      control%hard_ms = limits%movetime_ms
      control%soft_ms = control%hard_ms
      return
    end if

    if (is_wtm) then
      remaining_ms = limits%white_time_ms
      increment_ms = limits%white_increment_ms
    else
      remaining_ms = limits%black_time_ms
      increment_ms = limits%black_increment_ms
    end if

    if (remaining_ms < 0) return  ! No clock supplied.

    moves = limits%moves_to_go
    if (moves <= 0) moves = 30 ! Heuristic for sudden-death Control.

    ! Reserve 50 ms for scheduling, communication, and returning the move.
    available_ms = max(0_int64, remaining_ms - 50_int64)

    target_ms = available_ms/moves + (increment_ms*4)/5

    control%soft_ms = min(available_ms, target_ms)
    control%hard_ms = min(available_ms, 3*target_ms)
  end subroutine init_limits

  ! @brief helper
  pure logical function limits_reached(control, elapsed_ms) result(reached_tf)

    type(ControlT), intent(in) :: control
    integer(int64), intent(in) :: elapsed_ms

    reached_tf = control%is_stopped .or. &
              control%node_count >= control%node_limit .or. &
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
    control%is_stopped = limits_reached(control, elapsed_time_ms(control))
    is_stopped = control%is_stopped
  end function should_stop

end module ClockM
