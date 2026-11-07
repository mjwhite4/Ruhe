program test_clock

  use iso_fortran_env, only: int64, error_unit
  use ClockM, only: LimitsT, ControlT, init_limits, limits_reached
  use TestUtilsM, only: begin_suite, begin_test, fail, pass
  implicit none

  type(LimitsT) :: limits
  type(ControlT) :: control

  call begin_suite("test_clock")

  call begin_test("basic limits")
  control = ControlT()
  control%max_nodes = 100
  control%hard_ms = 500

  control%nodes = 99
  call check(.not. limits_reached(control, 499_int64), "below limits")
  call check(limits_reached(control, 500_int64), "deadline reached")

  control%nodes = 100
  call check(limits_reached(control, 0_int64), "node limit reached")
  call pass()

  ! No supplied time or node limit.
  call begin_test("no supplied time or node limit")
  limits = LimitsT()
  call init_limits(limits, .true., control)
  call check(control%hard_ms == huge(0_int64), "no deadline")
  call check(control%max_nodes == huge(0_int64), "no node limit")
  call check(control%nodes == 0, "node count reset")
  call check(.not. control%is_stopped, "stop flag reset")
  call pass()

  ! Fixed movetime overrides ordinary clock allocation.
  call begin_test("fixed movetime overrides ordinary clock allocation")
  limits%movetime = 1500
  limits%wtime = 60000
  call init_limits(limits, .true., control)
  call check(control%soft_ms == 1500, "movetime soft limit")
  call check(control%hard_ms == 1500, "movetime hard limit")
  call pass()

  ! Node limit.
  call begin_test("node limit")
  limits = LimitsT()
  limits%nodes = 1000
  call init_limits(limits, .true., control)
  call check(control%max_nodes == 1000, "node limit copied")
  call pass()

  ! Expected values for the current allocation policy:
  call begin_test("reserve 50 ms; target = available/moves + 4*increment/5.")
  ! reserve 50 ms; target = available/moves + 4*increment/5.
  limits = LimitsT()
  limits%wtime = 60050
  limits%btime = 30050
  limits%winc = 1000
  limits%binc = 500
  limits%movestogo = 20

  call init_limits(limits, .true., control)
  call check(control%soft_ms == 3800, "white soft budget")
  call check(control%hard_ms == 11400, "white hard budget")

  call init_limits(limits, .false., control)
  call check(control%soft_ms == 1900, "black soft budget")
  call check(control%hard_ms == 5700, "black hard budget")
  call pass()

  ! Missing movestogo uses the current 30-move estimate.
  call begin_test("missing movestogo uses the current 30-move estimate.")
  limits%movestogo = 0
  call init_limits(limits, .true., control)
  call check(control%soft_ms == 2800, "sudden-death budget")
  call pass()

  ! Never allocate more than the available clock.
  call begin_test("never allocate more than the available clock")
  limits%wtime = 40
  call init_limits(limits, .true., control)
  call check(control%hard_ms == 0, "clock below reserve")
  call pass()

  ! Reinitialization clears a previous interruption.
  call begin_test("reinitialization clears a previous interruption")
  control%is_stopped = .true.
  control%quit_requested = .true.
  control%nodes = 999
  limits = LimitsT()
  call init_limits(limits, .true., control)
  call check(.not. control%is_stopped, "stopped reset")
  call check(.not. control%quit_requested, "quit reset")
  call check(control%nodes == 0, "nodes reset again")
  call pass()

contains

  subroutine check(condition, description)
    logical, intent(in) :: condition
    character(len=*), intent(in) :: description

    if (.not. condition) then
      call fail(description)
    end if
  end subroutine

end program
