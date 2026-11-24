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
  use TestUtilsM, only: begin_suite, begin_test, pass_msg, fail_msg
  use BaseM, only: BaseT, full_score
  use PieceM, only: WHITE, BLACK, PAWN, KNIGHT, BISHOP, ROOK, QUEEN, KING
  use SquareM, only: SQ_D5, SQ_E1, SQ_E8, SQ_D4
  use EvaluationM
  implicit none

  type(BaseT) :: base
  integer(kind=EVK) :: score, phase, score_mg
  integer(kind=EVK) :: white_score, white_score_mg

  call begin_suite("test_evaluation")

  ! -------------------------------------------------------------------
  ! Evaluation Constants & Piece Values
  ! -------------------------------------------------------------------
  call begin_test("evaluation constants and piece values")

  if (MATE /= 32000_EVK .or. MATED /= -32000_EVK .or. DRAW /= 0_EVK) then
    call fail_msg("special score constants mismatch")
  end if

  if (PIECE_VALUE(WHITE + PAWN) /= 100_EVK .or. &
      PIECE_VALUE(BLACK + PAWN) /= -100_EVK) then
    call fail_msg("pawn value mismatch")
  end if

  if (PIECE_VALUE(WHITE + KNIGHT) /= 320_EVK .or. &
      PIECE_VALUE(BLACK + KNIGHT) /= -320_EVK .or. &
      PIECE_VALUE(WHITE + BISHOP) /= 330_EVK .or. &
      PIECE_VALUE(BLACK + BISHOP) /= -330_EVK .or. &
      PIECE_VALUE(WHITE + ROOK) /= 500_EVK .or. &
      PIECE_VALUE(BLACK + ROOK) /= -500_EVK .or. &
      PIECE_VALUE(WHITE + QUEEN) /= 900_EVK .or. &
      PIECE_VALUE(BLACK + QUEEN) /= -900_EVK) then
    call fail_msg("piece value constants mismatch")
  end if

  call pass_msg()

  ! -------------------------------------------------------------------
  ! Full Score Symmetry at Startpos
  ! -------------------------------------------------------------------
  call begin_test("full_score symmetry at startpos")

  call base%reset_base()
  call full_score(base, score, phase_opt=phase, score_mg_opt=score_mg)

  if (phase /= 24_EVK) call fail_msg("starting position phase /= 24")
  if (score_mg /= 0_EVK .or. score /= 0_EVK) &
    call fail_msg("starting score not symmetric")

  call pass_msg()

  ! -------------------------------------------------------------------
  ! Material Imbalance Evaluation
  ! -------------------------------------------------------------------
  call begin_test("material advantage evaluation")

  call base%clear_base()
  call base%set_piece_at(SQ_E1, WHITE + KING)
  call base%set_piece_at(SQ_E8, BLACK + KING)
  call base%set_piece_at(SQ_D4, WHITE + KNIGHT)

  call full_score(base, score, phase_opt=phase, score_mg_opt=score_mg)

  if (phase /= 1_EVK) call fail_msg("single knight phase /= 1")
  if (score_mg <= 0_EVK .or. score <= 0_EVK) &
    call fail_msg("white advantage not detected")

  white_score = score
  white_score_mg = score_mg

  call pass_msg()


  call begin_test("color-reflected knight evaluation")

  call base%clear_base()
  call base%set_piece_at(SQ_E1, WHITE + KING)
  call base%set_piece_at(SQ_E8, BLACK + KING)
  call base%set_piece_at(SQ_D5, BLACK + KNIGHT)

  call full_score(base, score, phase_opt=phase, score_mg_opt=score_mg)

  if (phase /= 1_EVK) then
    call fail_msg("black knight phase /= 1")
  end if
  if (score /= -white_score) then
    call fail_msg("color-reflected score did not change sign")
  end if
  if (score_mg /= -white_score_mg) then
    call fail_msg("color-reflected score_mg did not change sign")
  end if

  call pass_msg()

  ! -------------------------------------------------------------------
  ! Game Phase & Endgame Interpolation
  ! -------------------------------------------------------------------
  call begin_test("bare kings endgame phase and interpolation")

  call base%clear_base()
  call base%set_piece_at(SQ_E1, WHITE + KING)
  call base%set_piece_at(SQ_E8, BLACK + KING)

  call full_score(base, score, phase_opt=phase, score_mg_opt=score_mg)

  if (phase /= 0_EVK) call fail_msg("bare kings phase /= 0")
  if (score /= 0_EVK) call fail_msg("bare kings evaluation not equal to 0")

  call pass_msg()

  

  call assert_move_cache( &
    "capture evaluation caches", &
    "7k/8/8/3q4/8/8/8/K2R4 w - - 0 1", "d1d5")

  call assert_move_cache( &
    "queen promotion evaluation caches", &
    "7k/P7/8/8/8/8/8/7K w - - 0 1", "a7a8q")

  call assert_move_cache( &
    "knight promotion evaluation caches", &
    "7k/P7/8/8/8/8/8/7K w - - 0 1", "a7a8n")

  call assert_move_cache( &
    "castling evaluation caches", &
    "4k3/8/8/8/8/8/8/4K2R w K - 0 1", "e1g1")

  call assert_move_cache( &
    "en passant evaluation caches", &
    "7k/8/8/3pP3/8/8/8/7K w - d6 0 1", "e5d6")

  !--- special move evaluation cache and undo

  call assert_move_cache( &
    "black capture evaluation caches", &
    "k2r4/8/8/8/3Q4/8/8/7K b - - 0 1", "d8d4")

  call assert_move_cache( &
    "black queen promotion evaluation caches", &
    "7k/8/8/8/8/8/p7/7K b - - 0 1", "a2a1q")

  call assert_move_cache( &
    "black knight promotion evaluation caches", &
    "7k/8/8/8/8/8/p7/7K b - - 0 1", "a2a1n")

  call assert_move_cache( &
    "black castling evaluation caches", &
    "4k2r/8/8/8/8/8/8/4K3 b k - 0 1", "e8g8")

  call assert_move_cache( &
    "black en passant evaluation caches", &
    "7k/8/8/8/3Pp3/8/8/7K b - d3 0 1", "e4d3")


contains

  subroutine assert_move_cache(test_name, fen, move_uci)

    use BoardM, only: BoardT, get_board, set_fen, generate_legal_moves
    use MoveM, only: MVK, get_uci
    use TestUtilsM, only: PositionCheckpointT, &
                         save_checkpoint, check_checkpoint

    character(len=*), intent(in) :: test_name, fen, move_uci

    type(BoardT), pointer :: board
    type(PositionCheckpointT) :: checkpoint
    integer(MVK) :: move
    integer(EVK) :: actual_score, actual_mg, actual_phase
    integer :: nmoves, i

    call begin_test(test_name)

    board => get_board()
    call board%reset_board()

    call set_fen(board, fen)
    call generate_legal_moves(board, nmoves)
    call save_checkpoint(board, checkpoint)

    move = 0_MVK
    do i = 1, checkpoint%nmoves
      if (get_uci(checkpoint%moves(i)) == move_uci) then
        move = checkpoint%moves(i)
        exit
      end if
    end do

    if (move == 0_MVK) then
      call fail_msg("requested move missing from legal moves:"//move_uci)
    end if

    call board%make_move(move)

    call full_score(board, actual_score, &
                    phase_opt=actual_phase, score_mg_opt=actual_mg)

    if (board%score /= actual_score) then
      call fail_msg("cached score differs from full_score")
    end if
    if (board%score_mg /= actual_mg) then
      call fail_msg("cached score_mg differs from full_score")
    end if
    if (board%phase /= actual_phase) then
      call fail_msg("cached phase differs from full_score")
    end if

    call board%unmake_move(move)
    call check_checkpoint(board, checkpoint)

    call pass_msg()

  end subroutine assert_move_cache

end program test_evaluation
