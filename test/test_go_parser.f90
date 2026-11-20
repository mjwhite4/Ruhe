program test_go_parser

  use iso_fortran_env, only: int64, error_unit
  use BoardM, only: BoardT, get_board
  use UciM, only: set_search
  use TestUtilsM, only: begin_suite, begin_test, fail_msg, pass_msg
  implicit none

  type(BoardT), pointer :: board
  integer :: status

  board => get_board()

  call begin_suite("test_go_parser")

  ! Basic depth.
  call begin_test("basic depth")
  call set_search(board, "go depth 5", status)
  call check(status == 0, "depth accepted")
  call check(board%request%depth_limit == 5, "depth stored")
  call pass_msg()

  ! All clock fields, with different values to catch swapped assignments.
  call begin_test("all clock fields, swapped assignments")
  call set_search(board, &
           "go wtime 60000 btime 45000 winc 1000 binc 500 movestogo 20", status)

  call check(status == 0, "clock command accepted")
  call check(board%request%white_time_ms == 60000_int64, "wtime")
  call check(board%request%black_time_ms == 45000_int64, "btime")
  call check(board%request%white_increment_ms == 1000_int64, "winc")
  call check(board%request%black_increment_ms == 500_int64, "binc")
  call check(board%request%moves_to_go == 20, "movestogo")
  call pass_msg()

  ! Independent request may appear together.
  call begin_test("independent request together")
  call set_search(board, "go depth 8 nodes 100000 movetime 1500", status)
  call check(status == 0, "combined request accepted")
  call check(board%request%depth_limit == 8, "combined depth")
  call check(board%request%node_limit == 100000_int64, "combined nodes")
  call check(board%request%movetime_ms == 1500_int64, "combined movetime")
  call pass_msg()

  ! No stale request from the previous command.
  call begin_test("no stale request from the previous command")
  call set_search(board, "go depth 2", status)
  call check(status == 0, "second command accepted")
  call check(board%request%movetime_ms == -1_int64, "movetime reset")
  call check(board%request%node_limit == huge(0_int64), "nodes reset")
  call check(board%request%white_time_ms == -1_int64, "clock reset")

  call set_search(board, "go infinite", status)
  call check(status == 0, "infinite accepted")
  call check(board%request%infinite, "infinite stored")

  call set_search(board, "go depth 1", status)
  call check(.not. board%request%infinite, "infinite reset")

  call set_search(board, "go winc 0 binc 0", status)
  call check(status == 0, "zero increments accepted")
  call pass_msg()

  ! Bad input must return failure, without ERROR STOP.
  call begin_test("bad inputs must return failure, w/o error stop")
  call expect_bad("go depth")
  call expect_bad("go movestogo")
  call expect_bad("go depth xyz")
  call expect_bad("go depth 0")
  call expect_bad("go depth -1")
  call expect_bad("go movetime 1.5")
  call expect_bad("go movetime 00:01")
  call expect_bad("go nodes 9223372036854775808")
  call pass_msg()

  ! print '(a)', "[PASS] test_go_parser"

contains

  subroutine check(condition, description)
    logical, intent(in) :: condition
    character(len=*), intent(in) :: description

    if (.not. condition) then
      ! write (error_unit, '(a)') "[FAIL] "//description
      call fail_msg(description)
      ! error stop 1
    end if
  end subroutine

  subroutine expect_bad(command)
    character(len=*), intent(in) :: command
    integer :: res

    call set_search(board, command, res)
    call check(res /= 0, "reject "//command)
  end subroutine

end program
