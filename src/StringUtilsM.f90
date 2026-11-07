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
!! @brief convenient string expressions mainly for debugging
module StringUtilsM

  use, intrinsic :: iso_fortran_env, only: int8, int16, int32, int64
  implicit none
  private

  public :: &
    next_token, &
    to_hex, &
    to_oct, &
    to_str, &
    to_int, &
    to_lower, &
    to_upper

  !! @brief define to_oct, to_hex, to_str for formatting ints
  interface to_hex
    module procedure i8_to_hex, i16_to_hex, i32_to_hex, i64_to_hex
  end interface

  interface to_oct
    module procedure i8_to_oct, i16_to_oct, i32_to_oct, i64_to_oct
  end interface

  interface to_str
    module procedure i8_to_dec, i16_to_dec, i32_to_dec, i64_to_dec
  end interface

  interface to_int
    module procedure dec_to_i32, dec_to_i64
  end interface

contains

  !! @brief next token; ipos points to whitespace or EOS
  subroutine next_token(str, delimit, ipos, token)

    character(len=*), intent(in) :: str
    character(len=1), intent(in) :: delimit
    integer, intent(inout) :: ipos
    character(len=:), allocatable, intent(out) :: token
    integer :: first, str_len

    ! no token yet
    token = ""
    str_len = len_trim(str)

    ! scan to first non-whitespace
    do while (ipos <= str_len)
      if (str(ipos:ipos) /= delimit .and. str(ipos:ipos) /= char(9)) exit
      ipos = ipos + 1
    end do
    first = ipos

    ! scan to first space or EOL
    do while (ipos <= str_len)
      if (str(ipos:ipos) == delimit .or. str(ipos:ipos) == char(9)) exit
      ipos = ipos + 1
    end do

    ! may have a token
    if (first <= str_len) then
      token = str(first:ipos - 1) ! empty token signifies the end
    end if
  end subroutine next_token

  !! @brief string-ify int
  pure function i64_to_hex(n) result(str)
    integer(int64), intent(in) :: n
    character(len=18) :: str
    write (str, "(a,z16.16)") "0x", n
  end function i64_to_hex

  pure function i32_to_hex(n) result(str)
    integer(int32), intent(in) :: n
    character(len=10) :: str
    write (str, "(a,z8.8)") "0x", n
  end function i32_to_hex

  pure function i16_to_hex(n) result(str)
    integer(int16), intent(in) :: n
    character(len=6) :: str
    write (str, "(a,z4.4)") "0x", iand(int(n, int32), 65535_int32)
  end function i16_to_hex

  pure function i8_to_hex(n) result(str)
    integer(int8), intent(in) :: n
    character(len=4) :: str
    write (str, "(a,z2.2)") "0x", iand(int(n, int32), 255_int32)
  end function i8_to_hex

  pure function i64_to_dec(n) result(str)
    integer(int64), intent(in) :: n
    character(len=24) :: str
    write (str, "(i0)") n
  end function i64_to_dec

  pure function i32_to_dec(n) result(str)
    integer(int32), intent(in) :: n
    character(len=13) :: str
    write (str, '(i0)') n
  end function i32_to_dec

  pure function i16_to_dec(n) result(str)
    integer(int16), intent(in) :: n
    character(len=8) :: str
    write (str, '(i0)') n
  end function i16_to_dec

  pure function i8_to_dec(n) result(str)
    integer(int8), intent(in) :: n
    character(len=5) :: str
    write (str, '(i0)') n
  end function i8_to_dec

  pure function i64_to_oct(n) result(str)
    integer(int64), intent(in) :: n
    character(len=24) :: str
    write (str, '(a,o22.22)') "0o", n
  end function i64_to_oct

  pure function i32_to_oct(n) result(str)
    integer(int32), intent(in) :: n
    character(len=13) :: str
    write (str, '(a,o11.11)') "0o", n
  end function i32_to_oct

  pure function i16_to_oct(n) result(str)
    integer(int16), intent(in) :: n
    character(len=8) :: str
    write (str, '(a,o6.6)') "0o", n
  end function i16_to_oct

  pure function i8_to_oct(n) result(str)
    integer(int8), intent(in) :: n
    character(len=5) :: str
    write (str, '(a,o3.3)') "0o", n
  end function i8_to_oct

  subroutine dec_to_i32(str, n, io_stat_opt)
    character(len=*), intent(in) :: str
    integer(int32), intent(out) :: n
    integer, intent(out), optional :: io_stat_opt
    integer :: io_stat
    read (unit=str, fmt=*, iostat=io_stat) n
    if (present(io_stat_opt)) then
      io_stat_opt = io_stat
    else if (io_stat /= 0) then
      error stop "invalid int32:"//trim(str)
    end if
  end subroutine dec_to_i32

  subroutine dec_to_i64(str, n, io_stat_opt)
    character(len=*), intent(in) :: str
    integer(int64), intent(out) :: n
    integer, intent(out), optional :: io_stat_opt
    integer :: io_stat
    read (unit=str, fmt=*, iostat=io_stat) n
    if (present(io_stat_opt)) then
      io_stat_opt = io_stat
    else if (io_stat /= 0) then
      error stop "invalid int64:"//trim(str)
    end if
  end subroutine dec_to_i64

  pure function to_lower(str) result(s)
    character(len=*), intent(in) :: str
    integer :: i, c
    integer, parameter :: lower_case = iachar('a') - iachar('A')
    character(len=len(str)) :: s
    do i = 1, len_trim(str)
      c = iachar(str(i:i))
      if (iachar('A') <= c .and. c <= iachar('Z')) then
        s(i:i) = achar(c + lower_case)
      else
        s(i:i) = str(i:i)
      end if
    end do
  end function to_lower

  pure function to_upper(str) result(s)
    character(len=*), intent(in) :: str
    integer :: i, c
    integer, parameter :: lower_case = iachar('a') - iachar('A')
    character(len=len_trim(str)) :: s
    do i = 1, len(str)
      c = iachar(str(i:i))
      if (iachar('a') <= c .and. c <= iachar('z')) then
        s(i:i) = achar(c - lower_case)
      else
        s(i:i) = str(i:i)
      end if
    end do
  end function to_upper

end module

