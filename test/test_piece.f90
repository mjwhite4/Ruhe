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
program test_piece

  use TestUtilsM, only: begin_suite, begin_test, pass, fail
  use PieceM
  implicit none

  integer(PCK) :: pc

  call begin_suite("test_piece")

  ! -------------------------------------------------------------------
  ! Round-Trip and Decomposition Tests
  ! -------------------------------------------------------------------

  call begin_test("parse_piece(PIECE_CHAR(pc)) == pc")
  do pc = 0, PIECE_LAST
    if (parse_piece(PIECE_CHAR(pc)) /= pc) then
      call fail("piece character parsing round-trip mismatch")
    end if
  end do
  call pass()

  call begin_test("PIECE_TYPE(pc) == ibclr(pc, COLOR_BIT)")
  do pc = 0, PIECE_LAST
    if (piece_type(pc) /= ibclr(pc, COLOR_BIT)) then
      call fail("piece_type cleared bit calculation mismatch")
    end if
  end do
  call pass()

  call begin_test("PIECE_TYPE(pc) + PIECE_COLOR(pc) == pc")
  do pc = 0, PIECE_LAST
    if (piece_type(pc) + piece_color(pc) /= pc) then
      call fail("piece type plus color composition mismatch")
    end if
  end do
  call pass()

  ! -------------------------------------------------------------------
  ! Color Constants & XCOLOR Toggling
  ! -------------------------------------------------------------------

  call begin_test("XCOLOR color toggling logic")
  if (WHITE /= 0_PCK .or. BLACK /= 1_PCK) call fail("color constants mismatch")
  if (XCOLOR(WHITE) /= BLACK .or. XCOLOR(BLACK) /= WHITE) then
    call fail("XCOLOR bitwise color inversion mismatch")
  end if
  call pass()

  ! -------------------------------------------------------------------
  ! Parsing Functions & Invalid Inputs
  ! -------------------------------------------------------------------

  call begin_test("is_piece_char valid and invalid characters")
  if (.not. is_piece_char('P') .or. .not. is_piece_char('k') .or. &
      .not. is_piece_char('.') .or. is_piece_char('Z') .or. &
      is_piece_char('1')) then
    call fail("is_piece_char character validation failure")
  end if
  call pass()

  call begin_test("parse_piece invalid input returns PC_NONE")
  if (parse_piece('Z') /= PC_NONE .or. parse_piece('?') /= PC_NONE) then
    call fail("invalid character did not parse to PC_NONE")
  end if
  call pass()

  ! -------------------------------------------------------------------
  ! Piece Classification Boolean Arrays
  ! -------------------------------------------------------------------

  call begin_test("IS_SWEEPER sliding piece checks")
  if (.not. IS_SWEEPER(WB) .or. .not. IS_SWEEPER(BR) .or. &
      .not. IS_SWEEPER(WQ) .or. IS_SWEEPER(WP) .or. IS_SWEEPER(WN) .or. &
      IS_SWEEPER(BK) .or. IS_SWEEPER(EMPTY)) then
    call fail("IS_SWEEPER classification failed")
  end if
  call pass()

  call begin_test("IS_PROMOTION type checks")
  if (.not. IS_PROMOTION(WN) .or. .not. IS_PROMOTION(BB) .or. &
      .not. IS_PROMOTION(WR) .or. .not. IS_PROMOTION(BQ) .or. &
      IS_PROMOTION(WP) .or. IS_PROMOTION(BK) .or. IS_PROMOTION(EMPTY)) then
    call fail("IS_PROMOTION classification failed")
  end if
  call pass()

  call begin_test("IS_NON_PAWN piece checks")
  if (.not. IS_NON_PAWN(WN) .or. .not. IS_NON_PAWN(BK) .or. &
      IS_NON_PAWN(WP) .or. IS_NON_PAWN(BP) .or. IS_NON_PAWN(EMPTY)) then
    call fail("IS_NON_PAWN classification failed")
  end if
  call pass()

  call begin_test("IS_NONEMPTY piece checks")
  if (.not. IS_NONEMPTY(WP) .or. .not. IS_NONEMPTY(BK) .or. &
      IS_NONEMPTY(EMPTY) .or. IS_NONEMPTY(1_PCK)) then
    call fail("IS_NONEMPTY classification failed")
  end if
  call pass()

end program test_piece
