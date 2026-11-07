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
module BuildInfoM

  implicit none
  private

  public :: &
    print_build

  !! NOTE processor directives below assume file extension, .F90, uppercase F,
  !! ... for ensuring that the pre-processor runs before compilation

#ifndef V_NAME
#define V_NAME "ruhe"
#endif

#ifndef V_DATE
#define V_DATE "2026-11-07"
#endif

#ifndef V_VERSION
#define V_VERSION "0.1"
#endif

#ifndef V_ARCH
#define V_ARCH "x86_64"
#endif

#ifndef V_COMPILER
#define V_COMPILER "gfortran"
#endif

#ifndef V_FLAGS
#define V_FLAGS "fpm default"
#endif

  character(len=*), parameter, public :: BUILD_NAME = V_NAME
  character(len=*), parameter, public :: BUILD_DATE = V_DATE
  character(len=*), parameter, public :: BUILD_VERSION = V_VERSION
  character(len=*), parameter, public :: BUILD_ARCH = V_ARCH
  character(len=*), parameter, public :: BUILD_COMPILER = V_COMPILER
  character(len=*), parameter, public :: BUILD_FLAGS = V_FLAGS

contains

  !! @brief print build name, date, compiler, flags, arch
  subroutine print_build()

    print '(a,a)', "BUILD NAME = ", BUILD_NAME
    print '(a,a)', "BUILD VERSION = ", BUILD_VERSION
    print '(a,a)', "BUILD DATE = ", BUILD_DATE
    print '(a,a)', "BUILD COMPILER = ", BUILD_COMPILER
    print '(a,a)', "BUILD FLAGS = ", BUILD_FLAGS
    print '(a,a)', "BUILD ARCH = ", BUILD_ARCH
  end subroutine print_build

end module BuildInfoM
