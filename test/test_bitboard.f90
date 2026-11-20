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
program test_bitboard

  use, intrinsic :: iso_fortran_env, only: int8, int16, int32, int64
  use TestUtilsM, only: begin_suite, begin_test, pass_msg, fail_msg
  use SquareM
  use BitboardM
  implicit none

  integer(BBK) :: bb, occ
  integer(SQK) :: sq

  call begin_suite("test_bitboard")

  ! -------------------------------------------------------------------
  ! Bit Counting & Scanning Functions
  ! -------------------------------------------------------------------

  call begin_test("n_bits population count")
  if (n_bits(BB_EMPTY) /= 0 .or. n_bits(BB_ALL) /= 64 .or. &
      n_bits(BB_DARK_SQUARES) /= 32 .or. n_bits(BB_LIGHT_SQUARES) /= 32) then
    call fail_msg("population count calculation mismatch")
  end if
  call pass_msg()

  call begin_test("lsb and msb bit scanning")
  if (lsb(BB_SQUARES(SQ_E4)) /= SQ_E4 .or. &
      msb(BB_SQUARES(SQ_E4)) /= SQ_E4 .or. &
      lsb(BB_CORNERS) /= SQ_A1 .or. msb(BB_CORNERS) /= SQ_H8) then
    call fail_msg("bit scanning lsb/msb result mismatch")
  end if
  call pass_msg()

  call begin_test("pop_lsb sequence")
  bb = ior(BB_SQUARES(SQ_A1), BB_SQUARES(SQ_H8))
  call pop_lsb(bb, sq)
  if (sq /= SQ_A1 .or. bb /= BB_SQUARES(SQ_H8)) &
    call fail_msg("first pop_lsb failed")
  call pop_lsb(bb, sq)
  if (sq /= SQ_H8 .or. bb /= BB_EMPTY) &
    call fail_msg("second pop_lsb failed")
  call pass_msg()

  call begin_test("pop_msb sequence")
  bb = ior(BB_SQUARES(SQ_A1), BB_SQUARES(SQ_H8))
  call pop_msb(bb, sq)
  if (sq /= SQ_H8 .or. bb /= BB_SQUARES(SQ_A1)) &
    call fail_msg("first pop_msb failed")
  call pop_msb(bb, sq)
  if (sq /= SQ_A1 .or. bb /= BB_EMPTY) call fail_msg("second pop_msb failed")
  call pass_msg()

  ! -------------------------------------------------------------------
  ! Constant Bitboards & Board Masks
  ! -------------------------------------------------------------------

  call begin_test("rank and file mask constants")
  if (n_bits(BB_RANK_1) /= 8 .or. n_bits(BB_FILE_A) /= 8 .or. &
      iand(BB_RANK_1, BB_FILE_A) /= BB_SQUARES(SQ_A1) .or. &
      iand(BB_RANK_8, BB_FILE_H) /= BB_SQUARES(SQ_H8)) then
    call fail_msg("rank and file bitboard mask error")
  end if
  call pass_msg()

  call begin_test("corners, center, and backranks")
  if (n_bits(BB_CORNERS) /= 4 .or. n_bits(BB_CENTER) /= 4 .or. &
      n_bits(BB_BACKRANKS) /= 16) then
    call fail_msg("board region constants mismatch")
  end if
  call pass_msg()

  ! -------------------------------------------------------------------
  ! Directional Bit Shifts
  ! -------------------------------------------------------------------

  call begin_test("shift_right and shift_left")
  if (shift_right(1_SQK) /= BB_SQUARES(SQ_B1) .or. &
      shift_left(2_SQK) /= BB_SQUARES(SQ_A1)) then
    call fail_msg("bitboard shift operation mismatch")
  end if
  call pass_msg()

  ! -------------------------------------------------------------------
  ! Ray Generation & Ray Table Initialization
  ! -------------------------------------------------------------------

  call begin_test("shoot_ray")
  if (shoot_ray(SQ_E4, V_N) /= BB_RAY_N(SQ_E4) .or. &
      shoot_ray(SQ_E4, V_SW) /= BB_RAY_SW(SQ_E4)) then
    call fail_msg("shoot_ray directional bitboard mismatch")
  end if
  call pass_msg()

  call begin_test("init_ray_tables, BB_BETWEEN and BB_LINE")
  call init_ray_tables()

  if (BB_VECTOR(SQ_A1, SQ_A8) /= V_N .or. &
      BB_VECTOR(SQ_A1, SQ_H8) /= V_NE .or. &
      BB_VECTOR(SQ_A1, SQ_B3) /= 0_SQK) then
    call fail_msg("ray direction vector lookup mismatch")
  end if

  if (BB_BETWEEN(SQ_A1, SQ_A3) /= BB_SQUARES(SQ_A2) .or. &
      BB_BETWEEN(SQ_A1, SQ_A2) /= BB_EMPTY .or. &
      BB_LINE(SQ_A1, SQ_A8) /= BB_FILE_A) then
    call fail_msg("BB_BETWEEN or BB_LINE table lookup mismatch")
  end if
  call pass_msg()

  call begin_test("get_ray_attacks with occupancy")
  occ = BB_SQUARES(SQ_A4)
  bb = get_ray_attacks(SQ_A1, V_N, occ)
  if (bb /= ior(BB_SQUARES(SQ_A2), ior(BB_SQUARES(SQ_A3), &
                                       BB_SQUARES(SQ_A4)))) then
    call fail_msg("ray attacks calculation with blocker mismatch")
  end if
  call pass_msg()

  ! -------------------------------------------------------------------
  ! Leaper and Sliding Piece Attack Tables
  ! -------------------------------------------------------------------

  call begin_test("knight and king attack masks")
  if (n_bits(BB_KNIGHT_ATTACKS(SQ_E4)) /= 8 .or. &
      n_bits(BB_KNIGHT_ATTACKS(SQ_A1)) /= 2 .or. &
      n_bits(BB_KING_ATTACKS(SQ_E4)) /= 8 .or. &
      n_bits(BB_KING_ATTACKS(SQ_A1)) /= 3) then
    call fail_msg("knight or king attack mask count mismatch")
  end if
  call pass_msg()

  call begin_test("pawn push and attack masks")
  if (n_bits(BB_PAWN_ATTACKS_CO(0, SQ_E4)) /= 2 .or. &
      n_bits(BB_PAWN_PUSH_CO(0, SQ_E4)) /= 1) then
    call fail_msg("pawn push/attack mask count mismatch")
  end if
  call pass_msg()

  call begin_test("sliding_attacks_ generator")
  bb = sliding_attacks_(SQ_D4, BB_EMPTY, [V_N, V_S, V_E, V_W])
  if (bb /= BB_ROOK_RAYS(SQ_D4)) then
    call fail_msg("sliding attack generator output mismatch")
  end if
  call pass_msg()

end program test_bitboard
