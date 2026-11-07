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
program test_evaluation

  use, intrinsic :: iso_fortran_env, only: int16
  use TestUtilsM, only: begin_suite, begin_test, pass, fail
  use BaseM, only: BaseT, full_score
  use PieceM, only: WHITE, BLACK, PAWN, KNIGHT, BISHOP, ROOK, QUEEN, KING
  use SquareM, only: SQ_E1, SQ_E8, SQ_D4
  use EvaluationM
  implicit none

  type(BaseT) :: base
  integer(kind=EVK) :: score, phase, score_mg

  call begin_suite("test_evaluation")

  ! -------------------------------------------------------------------
  ! Evaluation Constants & Piece Values
  ! -------------------------------------------------------------------
  call begin_test("evaluation constants and piece values")

  if (MATE /= 32000_EVK .or. MATED /= -32000_EVK .or. DRAW /= 0_EVK) then
    call fail("special score constants mismatch")
  end if

  if (PIECE_VALUE(WHITE + PAWN) /= 100_EVK .or. &
      PIECE_VALUE(BLACK + PAWN) /= -100_EVK) then
    call fail("pawn value mismatch")
  end if

  if (PIECE_VALUE(WHITE + KNIGHT) /= 320_EVK .or. &
      PIECE_VALUE(BLACK + KNIGHT) /= -320_EVK .or. &
      PIECE_VALUE(WHITE + BISHOP) /= 330_EVK .or. &
      PIECE_VALUE(BLACK + BISHOP) /= -330_EVK .or. &
      PIECE_VALUE(WHITE + ROOK) /= 500_EVK .or. &
      PIECE_VALUE(BLACK + ROOK) /= -500_EVK .or. &
      PIECE_VALUE(WHITE + QUEEN) /= 900_EVK .or. &
      PIECE_VALUE(BLACK + QUEEN) /= -900_EVK) then
    call fail("piece value constants mismatch")
  end if

  call pass()

  ! -------------------------------------------------------------------
  ! Full Score Symmetry at Startpos
  ! -------------------------------------------------------------------
  call begin_test("full_score symmetry at startpos")

  call base%reset_base()
  call full_score(base, score, phase_opt=phase, score_mg_opt=score_mg)

  if (phase /= 24_EVK) call fail("starting position phase /= 24")
  if (score_mg /= 0_EVK .or. score /= 0_EVK) call fail("starting score not symmetric")

  call pass()

  ! -------------------------------------------------------------------
  ! Material Imbalance Evaluation
  ! -------------------------------------------------------------------
  call begin_test("material advantage evaluation")

  call base%clear_base()
  call base%set_piece_at(SQ_E1, WHITE + KING)
  call base%set_piece_at(SQ_E8, BLACK + KING)
  call base%set_piece_at(SQ_D4, WHITE + KNIGHT)

  call full_score(base, score, phase_opt=phase, score_mg_opt=score_mg)

  if (phase /= 1_EVK) call fail("single knight phase /= 1")
  if (score_mg <= 0_EVK .or. score <= 0_EVK) call fail("white advantage not detected")

  call pass()

  ! -------------------------------------------------------------------
  ! Game Phase & Endgame Interpolation
  ! -------------------------------------------------------------------
  call begin_test("bare kings endgame phase and interpolation")

  call base%clear_base()
  call base%set_piece_at(SQ_E1, WHITE + KING)
  call base%set_piece_at(SQ_E8, BLACK + KING)

  call full_score(base, score, phase_opt=phase, score_mg_opt=score_mg)

  if (phase /= 0_EVK) call fail("bare kings phase /= 0")
  if (score /= 0_EVK) call fail("bare kings evaluation not equal to 0")

  call pass()

end program test_evaluation
