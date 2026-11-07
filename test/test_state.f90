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
program test_state

  use, intrinsic :: iso_fortran_env, only: int8, int16
  use TestUtilsM, only: begin_suite, begin_test, pass, fail
  use PieceM, only: WHITE, BLACK
  use SquareM, only: SQ_A1, SQ_E1, SQ_H1, SQ_A8, SQ_E8, SQ_H8, SQ_E3, SQ_E6, &
                     EP_NONE
  use StateM
  implicit none

  type(StateT) :: st, st1, st2, st_expected
  integer(kind=CSK) :: rights

  call begin_suite("test_state")

  ! -------------------------------------------------------------------
  ! Default State Initialization
  ! -------------------------------------------------------------------
  call begin_test("new_state default values")

  st = new_state()

  if (st%turn /= WHITE .or. &
      st%castling_rights /= WB_CS_MASK .or. &
      st%ep_sq /= EP_NONE .or. &
      st%halfmove_clock /= 0_int16 .or. &
      st%fullmove_number /= 1_int16) then
    call fail("new_state default properties mismatch")
  end if

  call pass()

  ! -------------------------------------------------------------------
  ! Castling Masks & Rights Queries
  ! -------------------------------------------------------------------
  call begin_test("castling masks and rights queries")

  if (W_OO_MASK /= 1_CSK .or. W_OOO_MASK /= 2_CSK .or. &
      B_OO_MASK /= 4_CSK .or. B_OOO_MASK /= 8_CSK) then
    call fail("castling mask constants mismatch")
  end if

 if (W_CS_MASK /= 3_CSK .or. B_CS_MASK /= 12_CSK .or. WB_CS_MASK /= 15_CSK) then
    call fail("combined castling mask constants mismatch")
  end if

  rights = ior(W_OO_MASK, B_OOO_MASK) ! White Kingside + Black Queenside

  if (.not. has_kingside_castling_rights(rights, WHITE) .or. &
      has_queenside_castling_rights(rights, WHITE)) then
    call fail("white castling rights query mismatch")
  end if

  if (has_kingside_castling_rights(rights, BLACK) .or. &
      .not. has_queenside_castling_rights(rights, BLACK)) then
    call fail("black castling rights query mismatch")
  end if

  call pass()

  ! -------------------------------------------------------------------
  ! Castling Update Mask Rules
  ! -------------------------------------------------------------------
  call begin_test("CS_UPDATE_MASK rook/king movements")

  rights = WB_CS_MASK

  ! White King moves (E1) -> clear all White rights
  if (iand(rights, CS_UPDATE_MASK(SQ_E1)) /= B_CS_MASK) then
    call fail("moving white king did not clear all white castling rights")
  end if

  ! White A1 Rook moves -> clear White Queenside
  if (iand(rights, CS_UPDATE_MASK(SQ_A1)) /= (WB_CS_MASK - W_OOO_MASK)) then
    call fail("moving white a1 rook did not clear white queenside rights")
  end if

  ! White H1 Rook moves -> clear White Kingside
  if (iand(rights, CS_UPDATE_MASK(SQ_H1)) /= (WB_CS_MASK - W_OO_MASK)) then
    call fail("moving white h1 rook did not clear white kingside rights")
  end if

  ! Black King moves (E8) -> clear all Black rights
  if (iand(rights, CS_UPDATE_MASK(SQ_E8)) /= W_CS_MASK) then
    call fail("moving black king did not clear all black castling rights")
  end if

  ! Black A8 Rook moves -> clear Black Queenside
  if (iand(rights, CS_UPDATE_MASK(SQ_A8)) /= (WB_CS_MASK - B_OOO_MASK)) then
    call fail("moving black a8 rook did not clear black queenside rights")
  end if

  ! Black H8 Rook moves -> clear Black Kingside
  if (iand(rights, CS_UPDATE_MASK(SQ_H8)) /= (WB_CS_MASK - B_OO_MASK)) then
    call fail("moving black h8 rook did not clear black kingside rights")
  end if

  call pass()

  ! -------------------------------------------------------------------
  ! FEN State String Formatting
  ! -------------------------------------------------------------------
  call begin_test("get_state_str formatting")

  st = new_state()
  if (trim(get_state_str(st)) /= "w KQkq - 0 1") then
    call fail("default state string output mismatch")
  end if

  st = new_state(turn_opt=BLACK, &
                 castling_rights_opt=ior(W_OOO_MASK, B_OO_MASK), &
                 ep_sq_opt=SQ_E6)
  st%halfmove_clock = 3_int16
  st%fullmove_number = 12_int16

  if (trim(get_state_str(st)) /= "b Qk e6 3 12") then
    call fail("custom state string output mismatch")
  end if

  st%castling_rights = 0_CSK
  st%ep_sq = EP_NONE
  if (trim(get_state_str(st)) /= "b - - 3 12") then
    call fail("no-rights state string output mismatch")
  end if

  call pass()

  ! -------------------------------------------------------------------
  ! Zobrist Hash Incremental Updates
  ! -------------------------------------------------------------------
  call begin_test("incremental update_state_hash against fresh state")

  st1 = new_state(turn_opt=WHITE, castling_rights_opt=WB_CS_MASK, ep_sq_opt=EP_NONE)

  st2 = st1
  st2%turn = BLACK
  st2%ep_sq = SQ_E3
  st2%castling_rights = iand(st1%castling_rights, CS_UPDATE_MASK(SQ_A1))

  call update_state_hash(st2, st1)

  st_expected = new_state(turn_opt=BLACK, &
                          castling_rights_opt=st2%castling_rights, &
                          ep_sq_opt=SQ_E3)

  if (st2%h /= st_expected%h) then
    call fail("incremental hash mismatch compared to full hash generation")
  end if

  call pass()

end program test_state
