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
program test_move

  use, intrinsic :: iso_fortran_env, only: int8, int16, int64
  use TestUtilsM, only: begin_suite, begin_test, pass, fail
  use BaseM
  use BoardM
  use EvaluationM
  use MoveM
  use MoveStackM
  use PieceM
  use SquareM
  implicit none

  type(BoardT) :: board
  integer(MVK) :: imv, parsed_imv, mv_pawn_takes_queen, mv_queen_takes_pawn, mv_quiet
  integer(int16) :: pri, u_pri
  integer(kind=int8) :: mtype, u_mtype
  integer(kind=SQK) :: f_sq, t_sq, u_f_sq, u_t_sq
  integer(kind=PCK) :: pc, cap, pro, u_pc, u_cap, u_pro
  integer(EVK) :: score_ptq, score_qtp, score_quiet
  character(len=5) :: uci_str
  character(len=128) :: initial_fen

  call begin_suite("test_move")

  ! -------------------------------------------------------------------
  ! Move Packing and Unpacking
  ! -------------------------------------------------------------------
  call begin_test("pack_move and unpack_move integrity")

  pri = 150_int16
  f_sq = SQ_E2
  t_sq = SQ_E4
  pc = WHITE + PAWN
  cap = EMPTY
  pro = EMPTY
  mtype = DOUBLE_PUSH

  imv = pack_move(pri, int(f_sq, 2), int(t_sq, 2), int(pc, 1), int(cap, 1), int(pro, 1), mtype)

  call unpack_move(imv, u_pri, u_f_sq, u_t_sq, u_pc, u_cap, u_pro, u_mtype)

  if (u_pri /= pri .or. u_f_sq /= f_sq .or. u_t_sq /= t_sq .or. &
      u_pc /= pc .or. u_cap /= cap .or. u_pro /= pro .or. u_mtype /= mtype) then
    call fail("unpacked components do not match packed inputs")
  end if

  if (get_from_sq(imv) /= f_sq .or. get_to_sq(imv) /= t_sq .or. &
      get_piece(imv) /= pc .or. get_captured(imv) /= cap .or. &
      get_promotion(imv) /= pro .or. get_move_type(imv) /= mtype .or. &
      get_priority(imv) /= pri) then
    call fail("getter functions returned incorrect values")
  end if

  call pass()

  ! -------------------------------------------------------------------
  ! UCI Conversions
  ! -------------------------------------------------------------------
  call begin_test("UCI conversion functions (get_uci / from_uci)")

 imv = pack_move(0_int16, SQ_E2, SQ_E4, WHITE + PAWN, EMPTY, EMPTY, DOUBLE_PUSH)
  uci_str = get_uci(imv)
  if (trim(uci_str) /= "e2e4") call fail("get_uci expected e2e4")

  parsed_imv = from_uci("e2e4")
  if (get_from_sq(parsed_imv) /= SQ_E2 .or. get_to_sq(parsed_imv) /= SQ_E4) then
    call fail("from_uci failed to parse e2e4")
  end if

  imv = pack_move(0_int16, SQ_E7, SQ_E5, WHITE + PAWN, EMPTY, WHITE + QUEEN, PROMO_MOVE)
  uci_str = get_uci(imv)
  if (trim(uci_str) /= "e7e5q") call fail("get_uci expected promotion string e7e5q")

  if (get_uci(null_move()) /= "0000") call fail("null move UCI string /= 0000")

  call pass()

  ! -------------------------------------------------------------------
  ! MVV / LVA Move Ordering Scores
  ! -------------------------------------------------------------------
  call begin_test("get_mvv_lva scoring priority")

  mv_pawn_takes_queen = pack_move(0_int16, int(SQ_E4, 2), int(SQ_D5, 2), &
                int(WHITE + PAWN, 1), int(BLACK + QUEEN, 1), EMPTY, NORMAL_MOVE)

  mv_queen_takes_pawn = pack_move(0_int16, int(SQ_D4, 2), int(SQ_E5, 2), &
                int(WHITE + QUEEN, 1), int(BLACK + PAWN, 1), EMPTY, NORMAL_MOVE)

  mv_quiet = pack_move(0_int16, int(SQ_E2, 2), int(SQ_E4, 2), &
                       int(WHITE + PAWN, 1), EMPTY, EMPTY, NORMAL_MOVE)

  score_ptq = get_mvv_lva(mv_pawn_takes_queen)
  score_qtp = get_mvv_lva(mv_queen_takes_pawn)
  score_quiet = get_mvv_lva(mv_quiet)

  if (score_quiet /= 0 .or. score_ptq <= score_qtp) then
    call fail("MVV/LVA priorities calculated out of order")
  end if

  call pass()

  ! -------------------------------------------------------------------
  ! Board Manipulation (move_pieces & unmove_pieces)
  ! -------------------------------------------------------------------
  call begin_test("move_pieces and unmove_pieces board integrity")

  call board%reset_base()
  initial_fen = base_fen(board)

  imv = pack_move(0_int16, int(SQ_E2, 2), int(SQ_E4, 2), &
                  int(WHITE + PAWN, 1), EMPTY, EMPTY, DOUBLE_PUSH)

  call move_pieces(board, imv)

  if (piece_at(board, SQ_E4) /= WHITE + PAWN .or. piece_at(board, SQ_E2) /= EMPTY) then
    call fail("piece placement after move_pieces invalid")
  end if

  call unmove_pieces(board, imv)

  if (piece_at(board, SQ_E2) /= WHITE + PAWN .or. piece_at(board, SQ_E4) /= EMPTY) then
    call fail("piece placement after unmove_pieces invalid")
  end if

if (base_fen(board) /= initial_fen) call fail("board FEN mismatch after unmove")

  call pass()

  ! -------------------------------------------------------------------
  ! Retroactive Score Negation / Distance Inversion
  ! -------------------------------------------------------------------
  call begin_test("retro_score evaluation propagation")

  if (retro_score(150_EVK) /= -150_EVK) call fail("retro_score simple score negation failed")
  if (retro_score(ILLEGAL) /= ILLEGAL) call fail("retro_score ILLEGAL handling failed")
  if (retro_score(MATE) /= -MATE + 1_EVK) call fail("retro_score MATE distance increment failed")
  if (retro_score(MATED) /= -MATED - 1_EVK) call fail("retro_score MATED distance increment failed")

  call pass()

end program test_move
