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
program test_base

  use, intrinsic :: iso_fortran_env, only: int8, int16, int32, int64
  use TestUtilsM, only: begin_suite, begin_test, pass, fail
  use PieceM
  use SquareM
  use BitboardM
  use StringUtilsM
  use BaseM
  implicit none

  type(BaseT) :: base
  integer(PCK) :: pc
  integer(BBK) :: mask, occ
  integer(PCK), dimension(0:SQUARE_MASK) :: pst
  integer :: status

  call init_ray_tables()
  call begin_suite("test_base")

  ! -------------------------------------------------------------------
  ! 1. Board Initialization, Clearing, and FEN Parsing
  ! -------------------------------------------------------------------

  call begin_test("default init starting position and base_fen")
  call base%init_base()
  if (base%base_fen() /= STARTING_BOARD_FEN) call fail("FEN mismatch from starting board")
  call pass()

  call begin_test("clear_base")
  call base%clear_base()
  if (base%occupied_co(WHITE) /= BB_EMPTY .or. &
      base%occupied_co(BLACK) /= BB_EMPTY .or. &
      base%base_fen() /= "8/8/8/8/8/8/8/8") then
    call fail("board not empty after clear_base")
  end if
  call pass()

  call begin_test("set_base_fen custom FEN")
  call base%set_base_fen("r3k2r/8/8/8/8/8/8/R3K2R", status)
  if (status /= ERR_NONE .or. base%base_fen() /= "r3k2r/8/8/8/8/8/8/R3K2R") then
    call fail("failed setting custom base FEN")
  end if
  call pass()

  ! -------------------------------------------------------------------
  ! 2. Piece Placement and Queries
  ! -------------------------------------------------------------------

  call begin_test("set_piece_at and square queries")
  call base%clear_base()
  call base%set_piece_at(SQ_E4, WHITE + QUEEN)
  call base%set_piece_at(SQ_E1, WHITE + KING)
  call base%set_piece_at(SQ_E8, BLACK + KING)

  if (base%piece_at(SQ_E4) /= WHITE + QUEEN .or. &
      base%piece_type_at(SQ_E4) /= QUEEN .or. &
      base%color_at(SQ_E4) /= WHITE .or. &
      base%piece_at(SQ_A1) /= EMPTY .or. &
      base%color_at(SQ_A1) /= PC_NONE) then
    call fail("square piece/color query mismatch")
  end if
  call pass()

  call begin_test("king_sq")
  if (base%king_sq(WHITE) /= SQ_E1 .or. base%king_sq(BLACK) /= SQ_E8) then
    call fail("king square lookup failed")
  end if
  call pass()

  call begin_test("remove_piece_at")
  call base%remove_piece_at(SQ_E4, pc)
  if (pc /= WHITE + QUEEN .or. base%piece_at(SQ_E4) /= EMPTY) then
    call fail("remove_piece_at did not properly unseat piece")
  end if
  call pass()

  ! -------------------------------------------------------------------
  ! 3. Mailbox Array and Piece Masks
  ! -------------------------------------------------------------------

  call begin_test("get_mailbox_array")
  call base%reset_base()
  pst = get_mailbox_array(base)
  if (pst(SQ_A1) /= WHITE + ROOK .or. pst(SQ_E1) /= WHITE + KING .or. &
      pst(SQ_E8) /= BLACK + KING .or. pst(SQ_E4) /= EMPTY) then
    call fail("mailbox array export incorrect")
  end if
  call pass()

  ! -------------------------------------------------------------------
  ! 4. Attack and Attacker Masks
  ! -------------------------------------------------------------------

  call begin_test("attacks_mask")
  call base%clear_base()
  call base%set_piece_at(SQ_E4, WHITE + ROOK)
  mask = base%attacks_mask(SQ_E4)
  if (mask /= BB_ROOK_RAYS(SQ_E4)) call fail("rook attack mask mismatch")
  call pass()

  call begin_test("attackers_mask and is_square_attacked")
  call base%reset_base()
  occ = ior(base%occupied_co(WHITE), base%occupied_co(BLACK))

  ! White pawns on c2/e2 attacking d3
  mask = base%attackers_mask(SQ_D3, WHITE, occ)
  if (mask /= ior(BB_SQUARES(SQ_C2), BB_SQUARES(SQ_E2))) then
    call fail("white pawn attackers mask mismatch on d3")
  end if

  if (.not. base%is_square_attacked(SQ_D3, WHITE, occ) .or. &
      base%is_square_attacked(SQ_E4, WHITE, occ)) then
    call fail("is_square_attacked failed for white")
  end if

  ! Black pawns on c7/e7 attacking d6
  mask = base%attackers_mask(SQ_D6, BLACK, occ)
  if (mask /= ior(BB_SQUARES(SQ_C7), BB_SQUARES(SQ_E7))) then
    call fail("black pawn attackers mask mismatch on d6")
  end if

  if (.not. base%is_square_attacked(SQ_D6, BLACK, occ) .or. &
      base%is_square_attacked(SQ_E4, BLACK, occ)) then
    call fail("is_square_attacked failed for black")
  end if

  call pass()

  ! -------------------------------------------------------------------
  ! 5. Pins and Pin Masks
  ! -------------------------------------------------------------------

  call begin_test("pin_mask and is_pinned")
  call base%clear_base()
  call base%set_piece_at(SQ_E1, WHITE + KING)
  call base%set_piece_at(SQ_E4, WHITE + ROOK)  ! Pinned piece
  call base%set_piece_at(SQ_E8, BLACK + ROOK)  ! Sniper

if (.not. base%is_pinned(WHITE, SQ_E4)) call fail("expected piece to be pinned")

  mask = base%pin_mask(WHITE, SQ_E4)
  if (iand(mask, BB_SQUARES(SQ_E5)) == BB_EMPTY .or. &
      iand(mask, BB_SQUARES(SQ_D4)) /= BB_EMPTY) then
    call fail("pin_mask directional restriction wrong")
  end if
  call pass()

  ! -------------------------------------------------------------------
  ! 6. Board mailbox Formatting
  ! -------------------------------------------------------------------

  call begin_test("get_mailbox_str")
  call base%clear_base()
  call base%set_piece_at(SQ_E1, WHITE + KING)
  if (index(get_mailbox_str(base), "K") == 0) call fail("formatted string missing king")
  call pass()

end program test_base
