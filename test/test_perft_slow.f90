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

program test_perft_slow

  use, intrinsic :: iso_fortran_env, only: int64
  use TestUtilsM, only: assert_perft, begin_suite
  implicit none

  character(len=1) :: enabled
  integer :: status

  character(len=*), parameter :: &
    YELLOW = achar(27)//"[33m", &
    RESET = achar(27)//"[0m"

!   GREEN = achar(27)//"[32m", &
!   RED = achar(27)//"[31m", &
!   BLUE = achar(27)//"[34m", &
!   MAGENTA = achar(27)//"[35m", &
!   CYAN= achar(27)//"[36m", &

  call get_environment_variable( &
    "RUHE_SLOW_TESTS", enabled, status=status)

  if (status /= 0 .or. enabled /= "1") then
    print '(a)', "  "//YELLOW//"[SKIP]"//RESET//" "// &
            "test_perft_slow: set RUHE_SLOW_TESTS=1"
    stop
  end if

  call begin_suite("test_perft_slow")

  call assert_perft( &
    "startpos depth 6:", &
    "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1", &
    6, 119060324_int64, breakdown_opt=.false.)

end program test_perft_slow
