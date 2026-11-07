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
module TestUtilsM

  use, intrinsic :: iso_fortran_env, only: int64, real64
  use BoardM, only: BoardT, get_board, reset_board, set_fen
  use ClockM, only: ControlT, elapsed_time
  use MoveStackM, only: reset_movestack
  use PerftM, only: perft

  use, intrinsic :: iso_fortran_env, only: error_unit
  implicit none
  private

  public :: assert_perft, begin_suite, begin_test, pass, fail

  ! Module-level variables tracking active context
  character(len=64) :: current_suite = ""
  character(len=64) :: current_test = ""

  ! ANSI color escape sequences
  character(len=*), parameter :: GREEN = char(27)//"[32m"
  character(len=*), parameter :: RED = char(27)//"[31m"
  character(len=*), parameter :: RESET = char(27)//"[0m"

contains

  !-----------------------------------------------------------------------------
  ! Start a new test suite
  !-----------------------------------------------------------------------------
  subroutine begin_suite(suite_name)
    character(len=*), intent(in) :: suite_name
    current_suite = suite_name
    current_test = ""
    print '(A)', "Running "//trim(current_suite)//"..."
  end subroutine begin_suite

  !-----------------------------------------------------------------------------
  ! Track the active test name
  !-----------------------------------------------------------------------------
  subroutine begin_test(test_name)
    character(len=*), intent(in) :: test_name
    current_test = test_name
  end subroutine begin_test

  !-----------------------------------------------------------------------------
  ! Pass current test
  !-----------------------------------------------------------------------------
  subroutine pass(msg_opt)
    character(len=*), intent(in), optional :: msg_opt

    if (present(msg_opt)) then
      print "(a,a)", GREEN//"  [PASS] "//RESET, &
        trim(current_suite)//": "//trim(current_test)//": "//trim(msg_opt)
    else
      print "(a,a)", GREEN//"  [PASS] "//RESET, &
        trim(current_suite)//": "//trim(current_test)
    end if
    current_test = "no active test!"
  end subroutine pass

  !-----------------------------------------------------------------------------
  ! Fail current test with custom message and stop execution
  !-----------------------------------------------------------------------------
  subroutine fail(msg)
    character(len=*), intent(in) :: msg
    write (error_unit, '(A,A,A,A,A,A)') RED//"  [FAIL] "//RESET, &
      trim(current_suite)//": "// &
      trim(current_test)//" - ", trim(msg)
    error stop 1
  end subroutine fail

  subroutine assert_perft(ref_name, ref_fen, ref_depth, ref_nodes, &
                          breakdown_opt)

    character(len=*), intent(in) :: ref_name, ref_fen
    integer, intent(in) :: ref_depth
    integer(kind=int64), intent(in) :: ref_nodes
    logical, intent(in), optional :: breakdown_opt

    type(BoardT), pointer :: board
    integer(kind=int64) :: total_nodes
    real(kind=real64) :: ms, knps
    character(len=128) :: stats_txt
    character(len=64) :: verdict_txt
    logical :: do_breakdown

    call begin_test(ref_name)

    total_nodes = 0_int64
    board => get_board()
    call board%reset_board()

#ifdef DEBUG
    call board%set_fen( &
      "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1")
    if (.not. board%verify_hash()) &
      call fail("Hash verification failed on initial setup!")
#endif

    call set_fen(board, ref_fen)

    do_breakdown = .false.
    if (present(breakdown_opt)) do_breakdown = breakdown_opt

    if (do_breakdown) then
      print '(a, a)', "position fen ", trim(ref_fen)
      print '(a, i0)', "go perft ", ref_depth
    end if

    call system_clock(board%control%start_time, board%control%clock_rate)
    call reset_movestack(board%stack)
    call perft(board, ref_depth, total_nodes, do_breakdown)

    ms = elapsed_time(board%control)

    knps = real(total_nodes, real64)/max(ms, 0.1_real64)

    write (unit=stats_txt, fmt='(A,I0,A,I0,A,I0)') &
      "Depth: ", ref_depth, &
      " | Nodes: ", total_nodes, " / ", ref_nodes

    if (total_nodes == ref_nodes) then
      write (unit=verdict_txt, fmt='(A,G0.3,A)') &
        " | ", (knps/1000_real64), " Mnps"
      call pass(trim(stats_txt)//trim(verdict_txt))
    else
      write (unit=verdict_txt, fmt='(A,I0,A)') &
        " | Diff: ", total_nodes - ref_nodes, " nodes"
      call fail(trim(stats_txt)//trim(verdict_txt))
    end if
  end subroutine assert_perft

end module TestUtilsM
