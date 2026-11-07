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
!! @brief define square constants, e.g. SQK, and routines, e.g. parse_square
module SquareM

  use, intrinsic :: iso_fortran_env, only: int16
  use PieceM, only: COLOR_MASK, PIECE_LAST
  implicit none

  private
  public :: &
    is_rank_digit, & !# whether character belongs to rank digits
    parse_file, & !# col given col name (e.g. 'a' returns FILE_A=0)
    parse_rank, & !# rank given rank name (e.g. '8' returns RANK_8=56)
    parse_square, & !# square given square name (e.g. 'a8' returns 56)
    square_distance, & !# count king steps from a to b
    square_manhattan_distance, & !# count rook steps from a to b
    square_knight_distance   !# count knight steps from a to b

  !! @brief square data type is int16
  integer, parameter, public :: &
    SQK = int16

  !! @brief cardinal directions
  integer(kind=SQK), parameter, public :: &
    V_E = 1_SQK, &
    V_W = -V_E, &
    V_N = 8_SQK, &
    V_S = -V_N

  !! @brief diagonal directions
  integer(kind=SQK), parameter, public :: &
    V_NE = V_N + V_E, &
    V_SE = V_S + V_E, &
    V_SW = V_S + V_W, &
    V_NW = V_N + V_W

  !! @brief all named squares as square indices
  integer(kind=SQK), parameter, public :: &
    SQ_A1 = 0_SQK, SQ_B1 = 1_SQK, SQ_C1 = 2_SQK, SQ_D1 = 3_SQK, &
    SQ_A2 = 8_SQK, SQ_B2 = 9_SQK, SQ_C2 = 10_SQK, SQ_D2 = 11_SQK, &
    SQ_A3 = 16_SQK, SQ_B3 = 17_SQK, SQ_C3 = 18_SQK, SQ_D3 = 19_SQK, &
    SQ_A4 = 24_SQK, SQ_B4 = 25_SQK, SQ_C4 = 26_SQK, SQ_D4 = 27_SQK, &
    SQ_A5 = 32_SQK, SQ_B5 = 33_SQK, SQ_C5 = 34_SQK, SQ_D5 = 35_SQK, &
    SQ_A6 = 40_SQK, SQ_B6 = 41_SQK, SQ_C6 = 42_SQK, SQ_D6 = 43_SQK, &
    SQ_A7 = 48_SQK, SQ_B7 = 49_SQK, SQ_C7 = 50_SQK, SQ_D7 = 51_SQK, &
    SQ_A8 = 56_SQK, SQ_B8 = 57_SQK, SQ_C8 = 58_SQK, SQ_D8 = 59_SQK

  integer(kind=SQK), parameter, public :: &
    SQ_E1 = 4_SQK, SQ_F1 = 5_SQK, SQ_G1 = 6_SQK, SQ_H1 = 7_SQK, &
    SQ_E2 = 12_SQK, SQ_F2 = 13_SQK, SQ_G2 = 14_SQK, SQ_H2 = 15_SQK, &
    SQ_E3 = 20_SQK, SQ_F3 = 21_SQK, SQ_G3 = 22_SQK, SQ_H3 = 23_SQK, &
    SQ_E4 = 28_SQK, SQ_F4 = 29_SQK, SQ_G4 = 30_SQK, SQ_H4 = 31_SQK, &
    SQ_E5 = 36_SQK, SQ_F5 = 37_SQK, SQ_G5 = 38_SQK, SQ_H5 = 39_SQK, &
    SQ_E6 = 44_SQK, SQ_F6 = 45_SQK, SQ_G6 = 46_SQK, SQ_H6 = 47_SQK, &
    SQ_E7 = 52_SQK, SQ_F7 = 53_SQK, SQ_G7 = 54_SQK, SQ_H7 = 55_SQK, &
    SQ_E8 = 60_SQK, SQ_F8 = 61_SQK, SQ_G8 = 62_SQK, SQ_H8 = 63_SQK

  !! @brief masks, when and-ed with square, yield column and rank
  integer(kind=SQK), parameter, public :: &
    FILE_MASK = 7_SQK, &
    RANK_MASK = 56_SQK, &
    SQUARE_MASK = 63_SQK, &
    SQ_NONE = 64_SQK, &
    EP_NONE = SQ_A1

  !! implied do loop variables
  integer, private :: icol, irank, sq, ico, iz

  !! @brief list of squares from A1 to H8
  integer(kind=SQK), dimension(0:SQUARE_MASK), parameter, public :: &
    SQUARES = int([(sq, sq=0, int(SQUARE_MASK))], SQK)

  !! @brief square col, given square
  integer(kind=SQK), dimension(0:SQUARE_MASK), parameter, public :: &
    SQUARE_FILE = iand(SQUARES, FILE_MASK)

  !! @brief square rank, given square
  integer(kind=SQK), dimension(0:SQUARE_MASK), parameter, public :: &
    RANK_OFFSET = iand(SQUARES, RANK_MASK)

  !! @brief square mirror board
  integer(kind=SQK), dimension(0:SQUARE_MASK), parameter, public :: &
    MIRROR = RANK_MASK - RANK_OFFSET(SQUARES) + SQUARE_FILE(SQUARES)

  !! @brief col = iand(E4, FILE_MASK) = FILE_E
  integer(kind=SQK), parameter, public :: &
    FILE_A = 0_SQK, &
    FILE_B = 1_SQK, &
    FILE_C = 2_SQK, &
    FILE_D = 3_SQK, &
    FILE_E = 4_SQK, &
    FILE_F = 5_SQK, &
    FILE_G = 6_SQK, &
    FILE_H = 7_SQK

  !! @brief rank = iand(E4, RANK_MASK) = RANK_4
  integer(kind=SQK), parameter, public :: &
    RANK_1_OFFSET = 0_SQK*V_N, &
    RANK_2_OFFSET = 1_SQK*V_N, &
    RANK_3_OFFSET = 2_SQK*V_N, &
    RANK_4_OFFSET = 3_SQK*V_N, &
    RANK_5_OFFSET = 4_SQK*V_N, &
    RANK_6_OFFSET = 5_SQK*V_N, &
    RANK_7_OFFSET = 6_SQK*V_N, &
    RANK_8_OFFSET = 7_SQK*V_N

  !! @brief RANK_1_CO(BLACK) = RANK_8
  integer(kind=SQK), dimension(0:COLOR_MASK), parameter, public :: &
    RANK_1_OFFSET_CO = [RANK_1_OFFSET, RANK_8_OFFSET], &
    RANK_2_OFFSET_CO = [RANK_2_OFFSET, RANK_7_OFFSET], &
    RANK_3_OFFSET_CO = [RANK_3_OFFSET, RANK_6_OFFSET], &
    RANK_4_OFFSET_CO = [RANK_4_OFFSET, RANK_5_OFFSET], &
    RANK_5_OFFSET_CO = [RANK_5_OFFSET, RANK_4_OFFSET], &
    RANK_6_OFFSET_CO = [RANK_6_OFFSET, RANK_3_OFFSET], &
    RANK_7_OFFSET_CO = [RANK_7_OFFSET, RANK_2_OFFSET], &
    RANK_8_OFFSET_CO = [RANK_8_OFFSET, RANK_1_OFFSET]

  !! @details is_e7_7th_for_white = IS_7TH_RANK_CO(E7, WHITE) = .true.
  logical, dimension(0:SQUARE_MASK, 0:COLOR_MASK), parameter, public :: &
    IS_RANK_1_CO = reshape([( &
      RANK_OFFSET(SQUARES) == RANK_1_OFFSET_CO(ico), ico=0, int(COLOR_MASK))], &
                           [1 + int(SQUARE_MASK), 1 + int(COLOR_MASK)]), &
    IS_RANK_2_CO = reshape([( &
      RANK_OFFSET(SQUARES) == RANK_2_OFFSET_CO(ico), ico=0, int(COLOR_MASK))], &
                           [1 + int(SQUARE_MASK), 1 + int(COLOR_MASK)]), &
    IS_RANK_7_CO = reshape([( &
      RANK_OFFSET(SQUARES) == RANK_7_OFFSET_CO(ico), ico=0, int(COLOR_MASK))], &
                           [1 + int(SQUARE_MASK), 1 + int(COLOR_MASK)]), &
    IS_RANK_8_CO = reshape([( &
      RANK_OFFSET(SQUARES) == RANK_8_OFFSET_CO(ico), ico=0, int(COLOR_MASK))], &
                           [1 + int(SQUARE_MASK), 1 + int(COLOR_MASK)])

  !! @brief rank_char = RANK_CHAR(E4) = 'e'
  character(len=1), dimension(0:SQUARE_MASK), parameter, public :: &
    RANK_CHAR = [reshape(transpose(reshape([( &
                                     ['1', '2', '3', '4', '5', '6', '7', '8'], &
                                    icol=int(FILE_A), int(FILE_H), int(V_E))], &
                                           [8, 8])), [1 + int(SQUARE_MASK), 1])]

  !! @brief col_char = FILE_CHAR(E4) = '4'
  character(len=1), dimension(0:SQUARE_MASK), parameter, public :: &
    FILE_CHAR = [(['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h'], &
                  irank=RANK_1_OFFSET, RANK_8_OFFSET, int(V_N))]

  !! @brief convert square to square name
  character(len=2), dimension(0:SQUARE_MASK), parameter, public :: &
    SQUARE_NAME = FILE_CHAR//RANK_CHAR

  !! @brief cardinal, diagonal leaps
  integer(kind=SQK), dimension(4), parameter, public :: &
    V_CARDINAL_LEAPS = [V_N, V_E, V_W, V_S], &
    V_DIAGONAL_LEAPS = [V_NE, V_NW, V_SE, V_SW]

  !! @brief knight and king leaps
  integer(kind=SQK), dimension(8), parameter, public :: &
    V_KNIGHT_LEAPS = [V_N + V_NE, V_N + V_NW, V_E + V_NE, V_W + V_NW, &
                      V_E + V_SE, V_W + V_SW, V_S + V_SE, V_S + V_SW], &
    V_KING_LEAPS = [V_N, V_S, V_E, V_W, V_NE, V_SE, V_SW, V_NW]

  !! @details A hybrid 0z88/0o77 scheme is used for moves.
  !! * 'o'-board squares have "octal" indices, i.e. o'00'-o'ff' (a8->h1),
  !!   having 3-bit rank and 3-bit col, or 0:63 LEFR.
  !! * 'z'-board squares have "hexadecimal" indices, i.e. z'00'-z'77' (a8->h1),
  !!   having 4-bit rank and 4 bit col.
  !! * Pieces usually live in the o-board (octal) from_sq and to_sq,
  !!   mapped onto the z-board as zsq, with mapped move vector zv.
  !! * Any attempted moves to destination (zsq + zv) off-board can be
  !!   detected with the "0x88 trick", "iand(zsq+zv, 0z'88') /= 0", and
  !!   those moves may be rejected.
  !! * Successful moves are mapped back to the o-board for analysis.
  !! * The forward mapping to z-space is accomplished with zsq = OZ(sq),
  !!   and its inverse to o-space is done with sq = ZO(zsq).

  !! @brief Z88 has the off-board rank and col bits set
  integer(kind=SQK), parameter, public :: &
    Z88 = int(z'88', SQK)       !// off-board bits

  !! @brief rook hops in z-space
  integer(kind=SQK), parameter, public :: &
    ZE = 1_SQK, &
    ZW = -ZE, &
    ZN = 16_SQK, &
    ZS = -ZN

  !! @brief bishop hops in z-space
  integer(kind=SQK), parameter, public :: &
    ZNE = ZN + ZE, &
    ZSE = ZS + ZE, &
    ZSW = ZS + ZW, &
    ZNW = ZN + ZW

  !! @brief knight hops in z-space
  integer(kind=SQK), parameter, public :: &
    ZNNW = ZN + ZNW, &
    ZNNE = ZN + ZNE, &
    ZENE = ZE + ZNE, &
    ZESE = ZE + ZSE, &
    ZSSE = ZS + ZSE, &
    ZSSW = ZS + ZSW, &
    ZWNW = ZW + ZNW, &
    ZWSW = ZW + ZSW

  !! @brief convert O'oy' to Z'zy' (octal ranks & cols to hexadecimal)
  integer(kind=SQK), dimension(0:64), parameter, public :: &
    OZ = [ishft(RANK_OFFSET(SQUARES), 1) + SQUARE_FILE(SQUARES), SQ_NONE]

  !! @brief Z-space extreme rays are 7*ZSSW ray from A1, 7*ZNNE from H8
  integer(kind=SQK), parameter, public :: &
    MINZ = OZ(SQ_A1) - 7_SQK*(ZN + ZE), &
    MAXZ = OZ(SQ_H8) + 7_SQK*(ZN + ZE)

  !! @brief convert Z'zy' to O'oy' (hex ranks & cols -> octal)
  integer(kind=SQK), dimension(MINZ:MAXZ), parameter, public :: &
    ZO = [(int(merge(findloc(int(OZ), iz, dim=1) - 1, int(SQ_NONE), &
              findloc(int(OZ), iz, dim=1) /= 0), SQK), iz=int(MINZ), int(MAXZ))]

  !! @brief true/false whether z-square is on board
  logical, dimension(MINZ:MAXZ), parameter, public :: &
    Z_IS_ON_BOARD = (ZO /= SQ_NONE)

  !! @brief pawn leaps
  integer(kind=SQK), dimension(2, 0:COLOR_MASK), parameter, public :: &
    ZV_PAWN_LEAPS_CO = reshape([ZNE, ZNW, ZSE, ZSW], [2, 2])

  !! @brief cardinal, diagonal leaps
  integer(kind=SQK), dimension(4), parameter, public :: &
    ZV_CARDINAL_LEAPS = [ZN, ZE, ZW, ZS], &
    ZV_DIAGONAL_LEAPS = [ZNE, ZNW, ZSE, ZSW]

  !! @brief knight and king leaps
  integer(kind=SQK), dimension(8), parameter, public :: &
    ZV_KNIGHT_LEAPS = [ZNNE, ZNNW, ZENE, ZWNW, ZESE, ZWSW, ZSSE, ZSSW], &
    ZV_KING_LEAPS = [ZN, ZS, ZE, ZW, ZNE, ZSE, ZSW, ZNW]

contains

  !! @brief whether plausible col name (e.g. 'e4' or '4' returns .true.)
  pure function is_rank_digit(char) result(tf)

    character(len=1), intent(in) :: char
    logical :: tf

    tf = (index('12345678', char) > 0)
  end function is_rank_digit

  !! @brief col, given square name or col name (e.g. 'a' or 'a3' returns FILE_A)
  pure function parse_file(nm) result(file)

    character(len=*), intent(in) :: nm
    integer :: loc
    integer(kind=SQK) :: file

    loc = findloc(FILE_CHAR, nm(1:1), dim=1)
    if (loc == 0) then
      file = SQ_NONE
      return
    end if
    file = int(loc - 1, SQK)
  end function parse_file

  !! @brief rank given square name or rank name ('7' or 'a7' returns RANK_7_OFFSET)
  pure function parse_rank(nm) result(rank)

    character(len=*), intent(in) :: nm
    integer :: loc
    integer(kind=SQK) :: rank

    loc = findloc(RANK_CHAR, nm(1:1), dim=1)
    if (loc == 0) then
      rank = SQ_NONE
      return
    end if
    rank = int(loc - 1, SQK)
  end function parse_rank

  !! @brief square given square name ('a1' returns A1)
  pure function parse_square(nm) result(sq)

    character(len=*), intent(in) :: nm
    integer(kind=SQK) :: sq
    integer :: loc

    loc = findloc(SQUARE_NAME, nm(1:2), dim=1)
    if (loc == 0) then
      sq = SQ_NONE
      return
    end if
    sq = int(loc - 1, SQK)
  end function parse_square

  !! @brief count king steps from a to b
  pure function square_distance(a, b) result(dist)

    integer(kind=SQK), intent(in) :: a, b
    integer(kind=SQK) :: dist

    dist = max(abs(SQUARE_FILE(b) - SQUARE_FILE(a)), &
               ishft(abs(RANK_OFFSET(b) - RANK_OFFSET(a)), -3))
  end function square_distance

  !! @brief count knight steps from a to b
  pure function square_knight_distance(a, b) result(dist)

    integer(kind=SQK), intent(in) :: a, b
    integer(kind=SQK) :: m, dx, dy
    integer(kind=SQK) :: dist

    dx = abs(SQUARE_FILE(a) - SQUARE_FILE(b))
    dy = abs(RANK_OFFSET(a) - RANK_OFFSET(b))/V_N

    if (dx + dy == 1_SQK) then !! adjacent squares
      dist = 3_SQK
      return
    else if (dx == 2_SQK .and. dy == 2_SQK) then !! two squares diagonal
      dist = 4_SQK
      return
    else if (dx == 1_SQK .and. dy == 1_SQK) then !! one square diagonal
      if (findloc([SQ_A1, SQ_A8, SQ_H1, SQ_H8], a, dim=1) > 0 .or. &
          findloc([SQ_A1, SQ_A8, SQ_H1, SQ_H8], b, dim=1) > 0) then !! corners
        dist = 4_SQK
        return
      end if
    end if

    ! all "integer" divisions
    m = max((dx + 1_SQK)/2_SQK, (dy + 1_SQK)/2_SQK, (dx + dy + 2_SQK)/3_SQK)
    dist = m + mod(m + dx + dy, 2_SQK)
  end function square_knight_distance

  !! @brief count rook steps from a to b
  pure function square_manhattan_distance(a, b) result(dist)

    integer(kind=SQK), intent(in) :: a, b
    integer(kind=SQK) :: dist

    dist = &
      abs(SQUARE_FILE(b) - SQUARE_FILE(a)) + &
      ishft(abs(RANK_OFFSET(b) - RANK_OFFSET(a)), -3)
  end function square_manhattan_distance

end module SquareM
