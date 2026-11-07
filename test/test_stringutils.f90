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
program test_stringutils

  use, intrinsic :: iso_fortran_env, only: int8, int16, int32, int64
  use TestUtilsM, only: begin_suite, begin_test, pass, fail
  use StringUtilsM
  implicit none

  character(len=100) :: line
  integer :: cursor
  character(len=:), allocatable :: token

  call begin_suite("test_stringutils")

  ! -------------------------------------------------------------------
  ! Case Conversion Functions (lower & upper)
  ! -------------------------------------------------------------------

  call begin_test("lower conversion")
  if (trim(to_lower("e2e4 e7e8q KQkq 123!")) /= "e2e4 e7e8q kqkq 123!") then
    call fail("lowercase conversion string mismatch")
  end if
  call pass()

  call begin_test("upper conversion")
  if (trim(to_upper("e2e4 e7e8q KQkq 123!")) /= "E2E4 E7E8Q KQKQ 123!") then
    call fail("uppercase conversion string mismatch")
  end if
  call pass()

  ! -------------------------------------------------------------------
  ! Token Parsing (next_token)
  ! -------------------------------------------------------------------

  call begin_test("next_token parsing")
  line = "position startpos moves e2e4"
  cursor = 1

  call next_token(line, " ", cursor, token)
  if (token /= "position") call fail("token 1 mismatch")

  call next_token(line, " ", cursor, token)
  if (token /= "startpos") call fail("token 2 mismatch")

  call next_token(line, " ", cursor, token)
  if (token /= "moves") call fail("token 3 mismatch")

  call next_token(line, " ", cursor, token)
  if (token /= "e2e4") call fail("token 4 mismatch")

  call next_token(line, " ", cursor, token)
  if (len_trim(token) /= 0) then
    call fail("expected empty token flag at end of line")
  end if
  call pass()

  ! -------------------------------------------------------------------
  ! Integer Formatting (hexstr, octstr, intstr)
  ! -------------------------------------------------------------------

  call begin_test("hexstr formatting")
  if (trim(to_hex(-1_int8)) /= "0xFF" .or. &
      trim(to_hex(255_int16)) /= "0x00FF" .or. &
      trim(to_hex(255_int32)) /= "0x000000FF" .or. &
      trim(to_hex(255_int64)) /= "0x00000000000000FF") then
    call fail("hexadecimal string formatting mismatch")
  end if
  call pass()

  call begin_test("octstr formatting")
  if (trim(to_oct(63_int8)) /= "0o077" .or. &
      trim(to_oct(63_int16)) /= "0o000077" .or. &
      trim(to_oct(63_int32)) /= "0o00000000077" .or. &
      trim(to_oct(63_int64)) /= "0o0000000000000000000077") then
    call fail("octal string formatting mismatch")
  end if
  call pass()

  call begin_test("intstr formatting")
  if (trim(to_str(127_int8)) /= "127" .or. &
      trim(to_str(32767_int16)) /= "32767" .or. &
      trim(to_str(2147483647_int32)) /= "2147483647" .or. &
      trim(to_str(9223372036854775807_int64)) /= "9223372036854775807") then
    call fail("integer string formatting mismatch")
  end if
  call pass()

end program test_stringutils
