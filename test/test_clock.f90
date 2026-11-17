program test_clock

  use iso_fortran_env, only: int64, error_unit
  use ClockM, only: SearchRequestT, SearchStateT, init_searching, limits_reached
  use TestUtilsM, only: begin_suite, begin_test, fail, pass
  implicit none

  type(SearchRequestT) :: request
  type(SearchStateT) :: searching

  call begin_suite("test_clock")

  call begin_test("basic request")
  searching = SearchStateT()
  searching%node_limit = 100
  searching%hard_ms = 500

  searching%node_count = 99
  call check(.not. limits_reached(searching, 499_int64), "below request")
  call check(limits_reached(searching, 500_int64), "deadline reached")

  searching%node_count = 100
  call check(limits_reached(searching, 0_int64), "node limit reached")
  call pass()

  ! No supplied time or node limit.
  call begin_test("no supplied time or node limit")
  request = SearchRequestT()
  call init_searching(request, .true., searching)
  call check(searching%hard_ms == huge(0_int64), "no deadline")
  call check(searching%node_limit == huge(0_int64), "no node limit")
  call check(searching%node_count == 0, "node count reset")
  call check(.not. searching%is_stopped, "stop flag reset")
  call pass()

  ! Fixed movetime overrides ordinary clock allocation.
  call begin_test("fixed movetime overrides ordinary clock allocation")
  request%movetime_ms = 1500
  request%white_time_ms = 60000
  call init_searching(request, .true., searching)
  call check(searching%soft_ms == 1500, "movetime soft limit")
  call check(searching%hard_ms == 1500, "movetime hard limit")
  call pass()

  ! Node limit.
  call begin_test("node limit")
  request = SearchRequestT()
  request%node_limit = 1000
  call init_searching(request, .true., searching)
  call check(searching%node_limit == 1000, "node limit copied")
  call pass()

  ! Expected values for the current allocation policy:
  call begin_test("reserve 50 ms; target = available/moves + 4*increment/5.")
  ! reserve 50 ms; target = available/moves + 4*increment/5.
  request = SearchRequestT()
  request%white_time_ms = 60050
  request%black_time_ms = 30050
  request%white_increment_ms = 1000
  request%black_increment_ms = 500
  request%moves_to_go = 20

  call init_searching(request, .true., searching)
  call check(searching%soft_ms == 3800, "white soft budget")
  call check(searching%hard_ms == 11400, "white hard budget")

  call init_searching(request, .false., searching)
  call check(searching%soft_ms == 1900, "black soft budget")
  call check(searching%hard_ms == 5700, "black hard budget")
  call pass()

  ! Missing movestogo uses the current 30-move estimate.
  call begin_test("missing movestogo uses the current 30-move estimate.")
  request%moves_to_go = 0
  call init_searching(request, .true., searching)
  call check(searching%soft_ms == 2800, "sudden-death budget")
  call pass()

  ! Never allocate more than the available clock.
  call begin_test("never allocate more than the available clock")
  request%white_time_ms = 40
  call init_searching(request, .true., searching)
  call check(searching%hard_ms == 0, "clock below reserve")
  call pass()

  ! Reinitialization clears a previous interruption.
  call begin_test("reinitialization clears a previous interruption")
  searching%is_stopped = .true.
  searching%quit_requested = .true.
  searching%node_count = 999
  request = SearchRequestT()
  call init_searching(request, .true., searching)
  call check(.not. searching%is_stopped, "stopped reset")
  call check(.not. searching%quit_requested, "quit reset")
  call check(searching%node_count == 0_int64, "node count reset again")
  call check(searching%node_limit == huge(0_int64), "node limit reset")
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
