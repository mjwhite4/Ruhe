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
!! @brief Piece constants, e.g. WHITE, BLACK, PAWN, ... KING
module PieceM

  use, intrinsic :: iso_fortran_env, only: int8
  implicit none
  private

  !! @brief piece data type is int8
  integer, parameter, public :: &
    PCK = int8

  integer(kind=PCK), parameter, public :: &
    PIECE_LAST = 13_PCK, &
    COLOR_MASK = 1_PCK

  integer, parameter, public :: &
    COLOR_BIT = 0

  !! @brief color codes
  integer(kind=PCK), parameter, public :: &
    WHITE = 0_PCK, & ! WHITE==EMPTY, so beware: if/=EMPTY, a WHITE piece!
    BLACK = 1_PCK

  !! @brief other color
  integer(kind=PCK), dimension(0:1), parameter, public :: &
    XCOLOR = [BLACK, WHITE]

  !! @brief piece type codes (0,13)
  integer(kind=PCK), parameter, public :: &
    PC_NONE = -1_PCK, & ! unevaluated/uninitialized/unsafe as index
    EMPTY = 0_PCK, & ! plain empty square
    PAWN = 2_PCK, &
    KNIGHT = 4_PCK, &
    BISHOP = 6_PCK, &
    ROOK = 8_PCK, &
    QUEEN = 10_PCK, &
    KING = 12_PCK

  !! @brief piece type+color codes
  integer(kind=PCK), parameter, public :: &
    WP = PAWN, &
    WN = KNIGHT, &
    WB = BISHOP, &
    WR = ROOK, &
    WQ = QUEEN, &
    WK = KING, &
    BP = ior(BLACK, PAWN), &
    BN = ior(BLACK, KNIGHT), &
    BB = ior(BLACK, BISHOP), &
    BR = ior(BLACK, ROOK), &
    BQ = ior(BLACK, QUEEN), &
    BK = ior(BLACK, KING)

  !! @brief convert piece type or piece type+color to piece name
  character(len=1), dimension(0:PIECE_LAST), parameter, public :: &
    PIECE_CHAR = [ &
    '.', 'x', &
    'P', 'p', &
    'N', 'n', &
    'B', 'b', &
    'R', 'r', &
    'Q', 'q', &
    'K', 'k' &
    ]

  integer :: pc ! default integer type MANDATORY for implied do

  !! @brief piece color, given piece
  integer(kind=PCK), dimension(0:PIECE_LAST), parameter, public :: &
    PIECE_COLOR = [(iand(int(pc, PCK), COLOR_MASK), pc=0, PIECE_LAST)]

  !! @brief piece type, given piece
  integer(kind=PCK), dimension(0:PIECE_LAST), parameter, public :: &
    PIECE_TYPE = [(ibclr(int(pc, PCK), COLOR_BIT), pc=0, int(PIECE_LAST))]

  !! @brief piece list of sliding types (BRQ)
  integer(kind=PCK), dimension(3), parameter, public :: &
    TYPE_BRQ = [BISHOP, ROOK, QUEEN], &
    SWEEPERS = TYPE_BRQ

  !! @brief bool whether a sliding piece (BRQ), given pc
  logical, dimension(0:PIECE_LAST), parameter, public :: &
    IS_TYPE_BRQ = [(findloc(TYPE_BRQ, PIECE_TYPE(pc), dim=1) > 0, &
                    pc=0, PIECE_LAST)], &
    IS_SWEEPER = IS_TYPE_BRQ

  !! @brief piece list (NBR)
  integer(kind=PCK), dimension(3), parameter, public :: &
    TYPE_NBR = [KNIGHT, BISHOP, ROOK]

  !! @brief bool (NBR), given pc
  logical, dimension(0:PIECE_LAST), parameter, public :: &
    IS_TYPE_NBR = [(findloc(TYPE_NBR, PIECE_TYPE(pc), dim=1) > 0, &
                    pc=0, PIECE_LAST)]

  !! @brief piece list promotion possibilities (NBRQ) ... Q first
  integer(kind=PCK), dimension(4), parameter, public :: &
    TYPE_NBRQ = [QUEEN, KNIGHT, BISHOP, ROOK], &
    PROMOTIONS = TYPE_NBRQ

  !! @brief bool whether a promotion possibility (NBRQ), given pc
  logical, dimension(0:PIECE_LAST), parameter, public :: &
    IS_TYPE_NBRQ = [(findloc(TYPE_NBRQ, PIECE_TYPE(pc), dim=1) > 0, &
                     pc=0, PIECE_LAST)], &
    IS_PROMOTION = IS_TYPE_NBRQ

  !! @brief piece list of non-pawn types (NBRQK)
  integer(kind=PCK), dimension(5), parameter, public :: &
    TYPE_NBRQK = [KNIGHT, BISHOP, ROOK, QUEEN, KING], &
    NON_PAWNS = TYPE_NBRQK

  !! @brief bool whether non-pawn type (NBRQK), given pc
  logical, dimension(0:PIECE_LAST), parameter, public :: &
    IS_TYPE_NBRQK = [(findloc(TYPE_NBRQK, PIECE_TYPE(pc), dim=1) > 0, &
                      pc=0, PIECE_LAST)], &
    IS_NON_PAWN = IS_TYPE_NBRQK

  !! @brief piece list of piece types (PNBRQK)
  integer(kind=PCK), dimension(6), parameter, public :: &
    TYPE_PNBRQK = [PAWN, KNIGHT, BISHOP, ROOK, QUEEN, KING], &
    NONEMPTIES = TYPE_PNBRQK

  !! @brief bool whether piece type (PNBRQK), given piece
  logical, dimension(0:PIECE_LAST), parameter, public :: &
    IS_TYPE_PNBRQK = [(findloc(TYPE_PNBRQK, PIECE_TYPE(pc), dim=1) > 0, &
                       pc=0, PIECE_LAST)], &
    IS_NONEMPTY = IS_TYPE_PNBRQK

  public :: &
    is_piece_char, &
    parse_piece

contains

  !! @brief whether the piece is one of: '.', 'x', 'P', 'p', 'N', 'n', ...
  pure logical function is_piece_char(char)
    character(len=1), intent(in) :: char

    ! without dim=1 findloc ==> array, so choose dim=1 for one SLOC
    is_piece_char = (findloc(PIECE_CHAR, char, dim=1) >= 1_PCK)
  end function is_piece_char

  !! @brief ret piece given piece name ('p'->BP, 'P'->WP, '.'->EMPTY, ...)
  pure integer(PCK) function parse_piece(nm)
    character(len=*), intent(in) :: nm

    ! -1_PCK (=PC_NONE) if unfound, signaling parse fail!
    parse_piece = int(findloc(PIECE_CHAR, nm, dim=1), PCK) - 1_PCK
  end function parse_piece

end module PieceM
