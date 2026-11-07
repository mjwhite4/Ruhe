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
!! @brief Static scoring of base
!! @attrib Tomasz Michniewski, "Simplified Evaluation Function"
!! @details These PST values are highly tuned for human like play.
module EvaluationM

  use, intrinsic :: iso_fortran_env, only: int16
  use PieceM, only: PIECE_LAST
  use SquareM, only: RANK_MASK, SQUARE_MASK, MIRROR
  implicit none
  private

  !! special constants

  integer, parameter, public :: &
    EVK = int16

  integer(kind=EVK), parameter, public :: &
    EV_NONE = 29998_EVK

  integer(kind=EVK), parameter, public :: &
    MATE = 32000_EVK, &
    DRAW = 0_EVK, &
    MATED = -32000_EVK, &
    ILLEGAL = 29999_EVK, &
    BASE_CAPTURE_VALUE = 10000_EVK, &
    BASE_PROMOTION_VALUE = 20000_EVK

  !! @param piece-value table, "SEF" (or "PeSTO") values

  integer(kind=EVK), dimension(0:PIECE_LAST), parameter, public :: &
    PIECE_VALUE = int([ &
                      0, 0, & ! EMPTY, BLACK bit
                      100, -100, & ! PAWN
                      320, -320, & ! KNIGHT
                      330, -330, & ! BISHOP
                      500, -500, & ! ROOK
                      900, -900, & ! QUEEN
                      20000, -20000 & ! KING large to avoid search failure
                      ], EVK)

  !! @brief phases by piece-index (black pieces have the same sign)
  integer(kind=EVK), dimension(0:PIECE_LAST), parameter, public :: &
    PIECE_PHASE = int([ &
                      0, 0, & ! EMPTY, BLACK bit
                      0, 0, & ! PAWN
                      1, 1, & ! KNIGHT
                      1, 1, & ! BISHOP
                      2, 2, & ! ROOK
                      4, 4, & ! QUEEN
                      0, 0 & ! KING
                      ], EVK)

  integer(kind=EVK), dimension(0:SQUARE_MASK), parameter :: &
    ST_ZERO = 0_EVK

  !! @param Pawn value square table ST
  integer(kind=EVK), dimension(0:SQUARE_MASK), parameter :: &
    ST_WP = int([ &
                !   FILE_A  _B  _C  _D  _E  _F  _G  _H
                0, 0, 0, 0, 0, 0, 0, 0, & !! RANK_1
                50, 50, 50, 50, 50, 50, 50, 50, & !! RANK_2
                10, 10, 20, 30, 30, 20, 10, 10, & !! RANK_3
                5, 5, 10, 25, 25, 10, 5, 5, & !! RANK_4
                0, 0, 0, 20, 20, 0, 0, 0, & !! RANK_5
                5, -5, -10, 0, 0, -10, -5, 5, & !! RANK_6
                5, 10, 10, -20, -20, 10, 10, 5, & !! RANK_7
                0, 0, 0, 0, 0, 0, 0, 0 & !! RANK_8 (Promote)
                ], EVK), &
    ST_BP = -ST_WP(MIRROR) ! (/negative/ mirror assignment)

  !! @param King value square ST (Middle-game)
  integer(kind=EVK), dimension(0:SQUARE_MASK), parameter :: &
    ST_WK_MG = int([ &
                   !   COL_A  _B  _C  _D  _E  _F  _G  _H
                   -30, -40, -40, -50, -50, -40, -40, -30, & !! RANK_1
                   -30, -40, -40, -50, -50, -40, -40, -30, & !! RANK_2
                   -30, -40, -40, -50, -50, -40, -40, -30, & !! RANK_3
                   -30, -40, -40, -50, -50, -40, -40, -30, & !! RANK_4
                   -20, -30, -30, -40, -40, -30, -30, -20, & !! RANK_5
                   -10, -20, -20, -20, -20, -20, -20, -10, & !! RANK_6
                   20, 20, 0, 0, 0, 0, 20, 20, & !! RANK_7
                   20, 30, 10, 0, 0, 10, 30, 20 & !! RANK_8 (Promote)
                   ], EVK), &
    ST_BK_MG = -ST_WK_MG(MIRROR) ! (/negative/ mirror assignment)

  !! @param King value square table ST (End-game)
  !! @details when end-game phase = 0, this is the WK PST
  integer(kind=EVK), dimension(0:SQUARE_MASK), parameter :: &
    ST_WK_EG = int([ &
                   !   COL_A  _B  _C  _D  _E  _F  _G  _H
                   -50, -40, -30, -20, -20, -30, -40, -50, & !! RANK_1
                   -30, -20, -10, 0, 0, -10, -20, -30, & !! RANK_2
                   -30, -10, 20, 30, 30, 20, -10, -30, & !! RANK_3
                   -30, -10, 30, 40, 40, 30, -10, -30, & !! RANK_4
                   -30, -10, 30, 40, 40, 30, -10, -30, & !! RANK_5
                   -30, -10, 20, 30, 30, 20, -10, -30, & !! RANK_6
                   -30, -30, 0, 0, 0, 0, -30, -30, & !! RANK_7
                   -50, -30, -30, -30, -30, -30, -30, -50 & !! RANK_8 (Promote)
                   ], EVK), &
    ST_BK_EG = -ST_WK_EG(MIRROR) ! (/negative/ mirror assignment)

  !! middle-game King value square-table ST

  integer(kind=EVK), dimension(0:PIECE_LAST, 0:SQUARE_MASK), parameter, &
    public :: &
    VALUE_PST = transpose(reshape([ & ! transpose to piece-square
                                  ST_ZERO, ST_ZERO, & ! EMPTY
                                  ST_WP, ST_BP, & ! PAWN
                                  ST_ZERO, ST_ZERO, & ! KNIGHT
                                  ST_ZERO, ST_ZERO, & ! BISHOP
                                  ST_ZERO, ST_ZERO, & ! ROOK
                                  ST_ZERO, ST_ZERO, & ! QUEEN
                                  ST_WK_MG, ST_BK_MG & ! KING
                                  ], [1 + SQUARE_MASK, 1 + PIECE_LAST]))

  !! @brief the max boost (EG-MG), due to zero game phase (white's POV)
  integer(kind=EVK), dimension(0:SQUARE_MASK), parameter, public :: &
    BOOST_ST_WK = ST_WK_EG - ST_WK_MG, &
    BOOST_ST_BK = ST_BK_EG - ST_BK_MG

contains

end module EvaluationM
