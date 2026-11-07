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
program test_board

  use TestUtilsM, only: begin_suite, begin_test, pass, fail
  use BoardM, only: BoardT, get_board, reset_board, STARTING_FEN
  use PieceM, only: WHITE
  use SquareM, only: EP_NONE
  implicit none

  type(BoardT), pointer :: board
  type(BoardT) :: local_board
  integer :: status, nmoves

  call begin_suite("test_board")

  !-----------------------------------------------------------------------------
  ! Test 1: Singleton access & reset
  !-----------------------------------------------------------------------------
  call begin_test("test_singleton_access")

  board => get_board()
  if (.not. associated(board)) call fail("pointer not associated")

  call board%reset_board()
  if (board%state%turn /= WHITE) call fail("turn not WHITE")
  if (board%state%ep_sq /= EP_NONE) call fail("ep_sq not EP_NONE")
  if (board%state%halfmove_clock /= 0) call fail("halfmove_clock /= 0")
  if (board%state%fullmove_number /= 1) call fail("fullmove_number /= 1")

  call pass()

  !-----------------------------------------------------------------------------
  ! Test 2: FEN parsing on starting position
  !-----------------------------------------------------------------------------
  call begin_test("test_set_fen_starting_pos")

  call local_board%set_fen(STARTING_FEN, status)
  if (status /= 0) call fail("set_fen status /= 0")
  if (local_board%state%turn /= WHITE) call fail("turn /= WHITE")
  if (local_board%is_check()) call fail("starting pos in check")
  if (local_board%is_checkmate()) call fail("starting pos in checkmate")
  if (local_board%is_stalemate()) call fail("starting pos in stalemate")

  call pass()

  !-----------------------------------------------------------------------------
  ! Test 3: Move generation on starting position
  !-----------------------------------------------------------------------------
  call begin_test("test_legal_moves_start_pos")

  call local_board%set_fen(STARTING_FEN)
  call local_board%generate_legal_moves(nmoves)
  if (nmoves /= 20) call fail("move count /= 20")

  call pass()

  !-----------------------------------------------------------------------------
  ! Test 4: Check & Checkmate detection
  !-----------------------------------------------------------------------------
  call begin_test("test_check_detection")

  call local_board%set_fen("r1bqkb1r/pppp1ppp/2n5/4p3/2B1P3/5Q2/PPPP1PPP/RNB1K1NR b KQkq - 0 4")
  if (local_board%is_check()) call fail("false check detected")

  call local_board%set_fen("r1bqkb1r/pppp1Qpp/2n5/4p3/2B1P3/8/PPPP1PPP/RNB1K1NR b KQkq - 0 4")
  if (.not. local_board%is_check()) call fail("missing check")
  if (.not. local_board%is_checkmate()) call fail("missing checkmate")

  call pass()

end program test_board
