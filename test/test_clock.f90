program test_clock

  use iso_fortran_env, only: int64, error_unit
  use ClockM, only: SearchRequestT, SearchStateT, init_search, limits_reached
  use TestUtilsM, only: begin_suite, begin_test, fail_msg, pass_msg
  implicit none

  type(SearchRequestT) :: request
  type(SearchStateT) :: search

  call begin_suite("test_clock")

  call begin_test("basic request")
  search = SearchStateT()
  search%node_limit = 100
  search%hard_ms = 500

  search%node_count = 99
  call check(.not. limits_reached(search, 499_int64), "below request")
  call check(limits_reached(search, 500_int64), "deadline reached")

  search%node_count = 100
  call check(limits_reached(search, 0_int64), "node limit reached")
  call pass_msg()

  ! No supplied time or node limit.
  call begin_test("no supplied time or node limit")
  request = SearchRequestT()
  call init_search(request, .true., search)
  call check(search%hard_ms == huge(0_int64), "no deadline")
  call check(search%node_limit == huge(0_int64), "no node limit")
  call check(search%node_count == 0, "node count reset")
  call check(.not. search%is_stopped, "stop flag reset")
  call pass_msg()

  ! Fixed movetime overrides ordinary clock allocation.
  call begin_test("fixed movetime overrides ordinary clock allocation")
  request%movetime_ms = 1500
  request%white_time_ms = 60000
  call init_search(request, .true., search)
  call check(search%soft_ms == 1500, "movetime soft limit")
  call check(search%hard_ms == 1500, "movetime hard limit")
  call pass_msg()

  ! Node limit.
  call begin_test("node limit")
  request = SearchRequestT()
  request%node_limit = 1000
  call init_search(request, .true., search)
  call check(search%node_limit == 1000, "node limit copied")
  call pass_msg()

  ! Expected values for the current allocation policy:
  call begin_test("reserve 50 ms; target = available/moves + 4*increment/5.")
  ! reserve 50 ms; target = available/moves + 4*increment/5.
  request = SearchRequestT()
  request%white_time_ms = 60050
  request%black_time_ms = 30050
  request%white_increment_ms = 1000
  request%black_increment_ms = 500
  request%moves_to_go = 20

  call init_search(request, .true., search)
  call check(search%soft_ms == 3800, "white soft budget")
  call check(search%hard_ms == 11400, "white hard budget")

  call init_search(request, .false., search)
  call check(search%soft_ms == 1900, "black soft budget")
  call check(search%hard_ms == 5700, "black hard budget")
  call pass_msg()

  ! Missing movestogo uses the current 30-move estimate.
  call begin_test("missing movestogo uses the current 30-move estimate.")
  request%moves_to_go = 0
  call init_search(request, .true., search)
  call check(search%soft_ms == 2800, "sudden-death budget")
  call pass_msg()

  ! Never allocate more than the available clock.
  call begin_test("never allocate more than the available clock")
  request%white_time_ms = 40
  call init_search(request, .true., search)
  call check(search%hard_ms == 0, "clock below reserve")
  call pass_msg()

  ! Reinitialization clears a previous interruption.
  call begin_test("reinitialization clears a previous interruption")
  search%is_stopped = .true.
  search%quit_requested = .true.
  search%node_count = 999
  request = SearchRequestT()
  call init_search(request, .true., search)
  call check(.not. search%is_stopped, "stopped reset")
  call check(.not. search%quit_requested, "quit reset")
  call check(search%node_count == 0_int64, "node count reset again")
  call check(search%node_limit == huge(0_int64), "node limit reset")
  call pass_msg()

contains

  subroutine check(condition, description)
    logical, intent(in) :: condition
    character(len=*), intent(in) :: description

    if (.not. condition) then
      call fail_msg(description)
    end if
  end subroutine

end program
