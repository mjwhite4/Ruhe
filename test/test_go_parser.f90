program test_go_parser

  use iso_fortran_env, only: int64, error_unit
  use BoardM, only: BoardT, get_board
  use UciM, only: set_control
  use TestUtilsM, only: begin_suite, begin_test, fail, pass
  implicit none

  type(BoardT), pointer :: board
  integer :: status

  board => get_board()

  call begin_suite("test_go_parser")

  ! Basic depth.
  call begin_test("basic depth")
  call set_control(board, "go depth 5", status)
  call check(status == 0, "depth accepted")
  call check(board%limits%depth == 5, "depth stored")
  call pass()

  ! All clock fields, with different values to catch swapped assignments.
  call begin_test("all clock fields, swapped assignments")
  call set_control(board, &
           "go wtime 60000 btime 45000 winc 1000 binc 500 movestogo 20", status)

  call check(status == 0, "clock command accepted")
  call check(board%limits%wtime == 60000_int64, "wtime")
  call check(board%limits%btime == 45000_int64, "btime")
  call check(board%limits%winc == 1000_int64, "winc")
  call check(board%limits%binc == 500_int64, "binc")
  call check(board%limits%movestogo == 20, "movestogo")
  call pass()

  ! Independent limits may appear together.
  call begin_test("independent limits together")
  call set_control(board, &
                   "go depth 8 nodes 100000 movetime 1500", status)
  call check(status == 0, "combined limits accepted")
  call check(board%limits%depth == 8, "combined depth")
  call check(board%limits%nodes == 100000_int64, "combined nodes")
  call check(board%limits%movetime == 1500_int64, "combined movetime")
  call pass()

  ! No stale limits from the previous command.
  call begin_test("no stale limits from the previous command")
  call set_control(board, "go depth 2", status)
  call check(status == 0, "second command accepted")
  call check(board%limits%movetime == -1_int64, "movetime reset")
  call check(board%limits%nodes == huge(0_int64), "nodes reset")
  call check(board%limits%wtime == -1_int64, "clock reset")

  call set_control(board, "go infinite", status)
  call check(status == 0, "infinite accepted")
  call check(board%limits%infinite, "infinite stored")

  call set_control(board, "go depth 1", status)
  call check(.not. board%limits%infinite, "infinite reset")

  call set_control(board, "go winc 0 binc 0", status)
  call check(status == 0, "zero increments accepted")
  call pass()

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
  call pass()

  ! print '(a)', "[PASS] test_go_parser"

contains

  subroutine check(condition, description)
    logical, intent(in) :: condition
    character(len=*), intent(in) :: description

    if (.not. condition) then
      ! write (error_unit, '(a)') "[FAIL] "//description
      call fail(description)
      ! error stop 1
    end if
  end subroutine

  subroutine expect_bad(command)
    character(len=*), intent(in) :: command
    integer :: res

    call set_control(board, command, res)
    call check(res /= 0, "reject "//command)
  end subroutine

end program
