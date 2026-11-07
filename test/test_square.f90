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
program test_square

  use TestUtilsM, only: begin_suite, begin_test, pass, fail
  use SquareM
  use PieceM, only: WHITE, BLACK
  implicit none

  integer(kind=SQK) :: file, rank, sq, zsq

  call begin_suite("test_square")

  ! -------------------------------------------------------------------
  ! Parsing Round-Trip Tests
  ! -------------------------------------------------------------------

  call begin_test("parse_file(FILE_CHAR(sq)) == file")
  do sq = 0, SQUARE_MASK
    file = SQUARE_FILE(sq)
    if (parse_file(FILE_CHAR(sq)) /= file) then
      call fail("file character parsing round-trip mismatch")
    end if
  end do
  call pass()

  call begin_test("parse_rank(RANK_CHAR(sq)) == rank")
  do sq = 0, SQUARE_MASK
    rank = RANK_OFFSET(sq)
    if (parse_rank(RANK_CHAR(sq)) /= rank) then
      call fail("rank character parsing round-trip mismatch")
    end if
  end do
  call pass()

  call begin_test("parse_square(SQUARE_NAME(sq)) == sq")
  do sq = 0, SQUARE_MASK
    if (parse_square(SQUARE_NAME(sq)) /= sq) then
      call fail("square string parsing round-trip mismatch")
    end if
  end do
  call pass()

  ! -------------------------------------------------------------------
  ! Parsing Invalid Inputs & Rank Digit Checks
  ! -------------------------------------------------------------------

  call begin_test("invalid inputs return SQ_NONE")
  if (parse_square("z9") /= SQ_NONE .or. &
      parse_file("x") /= SQ_NONE .or. &
      parse_rank("9") /= SQ_NONE) then
    call fail("invalid square/file/rank input did not return SQ_NONE")
  end if
  call pass()

  call begin_test("is_rank_digit logic")
  if (.not. is_rank_digit('1') .or. .not. is_rank_digit('8') .or. &
      is_rank_digit('0') .or. is_rank_digit('9') .or. is_rank_digit('a')) then
    call fail("is_rank_digit classification failed")
  end if
  call pass()

  ! -------------------------------------------------------------------
  ! Board Mirror & Distance Metrics
  ! -------------------------------------------------------------------

  call begin_test("MIRROR flips ranks correctly")
  if (MIRROR(SQ_A1) /= SQ_A8 .or. MIRROR(SQ_E2) /= SQ_E7 .or. &
      MIRROR(SQ_H8) /= SQ_H1) then
    call fail("square rank mirroring mismatch")
  end if
  call pass()

  call begin_test("square_distance king steps")
  if (square_distance(SQ_A1, SQ_H8) /= 7_SQK .or. &
      square_distance(SQ_E4, SQ_E5) /= 1_SQK .or. &
      square_distance(SQ_E4, SQ_F5) /= 1_SQK .or. &
      square_distance(SQ_E4, SQ_E4) /= 0_SQK) then
    call fail("chebyshev distance calculation mismatch")
  end if
  call pass()

  call begin_test("square_manhattan_distance rook steps")
  if (square_manhattan_distance(SQ_A1, SQ_H8) /= 14_SQK .or. &
      square_manhattan_distance(SQ_E4, SQ_E5) /= 1_SQK .or. &
      square_manhattan_distance(SQ_E4, SQ_F5) /= 2_SQK .or. &
      square_manhattan_distance(SQ_E4, SQ_E4) /= 0_SQK) then
    call fail("manhattan distance calculation mismatch")
  end if
  call pass()

  call begin_test("square_knight_distance step checks")
  if (square_knight_distance(SQ_E4, SQ_F4) /= 3_SQK .or. &
      square_knight_distance(SQ_A1, SQ_B2) /= 4_SQK .or. &
      square_knight_distance(SQ_E4, SQ_G6) /= 4_SQK .or. &
      square_knight_distance(SQ_E4, SQ_G5) /= 1_SQK .or. &
      square_knight_distance(SQ_E4, SQ_E4) /= 0_SQK) then
    call fail("knight distance calculation mismatch")
  end if
  call pass()

  ! -------------------------------------------------------------------
  ! 0x88 / 0o77 Board Transformations & Off-board Detection
  ! -------------------------------------------------------------------

  call begin_test("ZO(OZ(sq)) == sq round-trip")
  do sq = 0, SQUARE_MASK
    zsq = OZ(sq)
    if (ZO(zsq) /= sq .or. .not. Z_IS_ON_BOARD(zsq)) then
      call fail("0x88 square coordinate conversion round-trip failed")
    end if
  end do
  call pass()

  call begin_test("0x88 off-board detection")
  do sq = 0, SQUARE_MASK
    zsq = OZ(sq)
    if (iand(zsq, Z88) /= 0_SQK) then
      call fail("valid square has off-board 0x88 bits set")
    end if
    if (SQUARE_FILE(sq) == FILE_H) then
      if (iand(zsq + ZE, Z88) == 0_SQK .or. Z_IS_ON_BOARD(zsq + ZE)) then
        call fail("off-board square east of file H was not detected")
      end if
    end if
  end do
  call pass()

  ! -------------------------------------------------------------------
  ! Color-oriented Rank Arrays
  ! -------------------------------------------------------------------

  call begin_test("IS_RANK_7_CO for WHITE and BLACK")
  if (.not. IS_RANK_7_CO(SQ_E7, WHITE) .or. IS_RANK_7_CO(SQ_E2, WHITE) .or. &
      .not. IS_RANK_7_CO(SQ_E2, BLACK) .or. IS_RANK_7_CO(SQ_E7, BLACK)) then
    call fail("color-oriented 7th rank check mismatch")
  end if
  call pass()

end program test_square
