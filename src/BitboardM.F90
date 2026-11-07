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
!! @brief int64 glimpses of base
module BitboardM
  use, intrinsic :: iso_fortran_env, only: int8, int16, int32, int64
  use SquareM
  use PieceM, only: PIECE_LAST, COLOR_MASK
  implicit none
  private

  public :: &
    lsb, & !# least significant bit
    pop_lsb, & !# (~ scan_forward)
    msb, & !# most significant bit
    pop_msb, & !# (~ scan_reversed)
    shift_down, & !# shift down one rank (V_S)
    shift_2_down, & !# shift down two ranks (2*V_S)
    shift_up, & !# shift up one rank (V_N)
    shift_2_up, & !# shift up two ranks (2*V_N)
    shift_right, & !# shift right one file (V_E)
    shift_2_right, & !# shift right two files (2*V_E)
    shift_left, & !# shift left one file (V_W)
    shift_2_left, & !# shift left two files (2*V_W)
    shift_up_left, & !# shift up one rank and left one file (V_NW)
    shift_up_right, & !# shift up one rank and right one file (V_NE)
    shift_down_left, & !# shift down one rank and left one file (V_SW)
    shift_down_right, & !# shift down one rank and right one file (V_SE)
    sliding_attacks_

  !# flip_vertical,      & !# flip about the vertical
  !# flip_horizontal,    & !# flip about the horizontal
  !# flip_diagonal,      & !# flip about the diagonal
  !# flip_anti_diagonal, & !# flip about the anti-diagonal

  public :: &
    get_ray_attacks, &
    get_ray_in_direction, &
    init_ray_tables, &
    init_vector_table, &
    n_bits, &
    print_stamp, &
    shoot_ray

  !# between,        & !# all between a&b exclusive, on rank, file, or diag
  !# mirror,         & !# return vertically mirrored copy of this Bitboard

  !# union(a, b) -> Bitboard
  !# intersection(a, b) -> Bitboard
  !# difference(a, b) -> Bitboard
  !# difference_update -> a = a-b
  !# symmetric_difference_update a = a^(a&b)

  !# list,           & !# nonempty square index list
  !# copy,           &
  !# update,         &
  !# carry_rippler   & Iterator iterate over subsets

  integer, parameter, public :: &
    BBK = int64

  integer(kind=BBK), dimension(0:SQUARE_MASK, 8), public :: &
    RAY_TABLE

  integer(kind=BBK), parameter, public :: &
    BB_EMPTY = 0_BBK

  integer(kind=BBK), parameter, public :: &
    BB_ALL = int(z'ffffffffffffffff', BBK), &
    BB_DARK_SQUARES = int(z'55aa55aa55aa55aa', BBK), &
    BB_LIGHT_SQUARES = int(z'aa55aa55aa55aa55', BBK)

  integer(kind=BBK), parameter, public :: &
    BB_RANK_1 = ishft(int(z'ff', BBK), RANK_1_OFFSET), &
    BB_RANK_2 = ishft(int(z'ff', BBK), RANK_2_OFFSET), &
    BB_RANK_3 = ishft(int(z'ff', BBK), RANK_3_OFFSET), &
    BB_RANK_4 = ishft(int(z'ff', BBK), RANK_4_OFFSET), &
    BB_RANK_5 = ishft(int(z'ff', BBK), RANK_5_OFFSET), &
    BB_RANK_6 = ishft(int(z'ff', BBK), RANK_6_OFFSET), &
    BB_RANK_7 = ishft(int(z'ff', BBK), RANK_7_OFFSET), &
    BB_RANK_8 = ishft(int(z'ff', BBK), RANK_8_OFFSET)

  integer(kind=BBK), dimension(0:COLOR_MASK), parameter, public :: &
    BB_RANK_1_CO = ishft(int(z'ff', BBK), RANK_1_OFFSET_CO), &
    BB_RANK_2_CO = ishft(int(z'ff', BBK), RANK_2_OFFSET_CO), &
    BB_RANK_3_CO = ishft(int(z'ff', BBK), RANK_3_OFFSET_CO), &
    BB_RANK_4_CO = ishft(int(z'ff', BBK), RANK_4_OFFSET_CO), &
    BB_RANK_5_CO = ishft(int(z'ff', BBK), RANK_5_OFFSET_CO), &
    BB_RANK_6_CO = ishft(int(z'ff', BBK), RANK_6_OFFSET_CO), &
    BB_RANK_7_CO = ishft(int(z'ff', BBK), RANK_7_OFFSET_CO), &
    BB_RANK_8_CO = ishft(int(z'ff', BBK), RANK_8_OFFSET_CO)

  integer(kind=BBK), parameter, public :: &
    BB_FILE_A = int(z'0101010101010101', BBK), &
    BB_FILE_B = ishft(BB_FILE_A, FILE_B), &
    BB_FILE_C = ishft(BB_FILE_A, FILE_C), &
    BB_FILE_D = ishft(BB_FILE_A, FILE_D), &
    BB_FILE_E = ishft(BB_FILE_A, FILE_E), &
    BB_FILE_F = ishft(BB_FILE_A, FILE_F), &
    BB_FILE_G = ishft(BB_FILE_A, FILE_G), &
    BB_FILE_H = ishft(BB_FILE_A, FILE_H)

  !! @brief what is the purpose of BB_FILES?
  integer(kind=BBK), parameter, dimension(0:7), public :: &
    BB_FILES = [BB_FILE_A, BB_FILE_B, BB_FILE_C, BB_FILE_D, &
                BB_FILE_E, BB_FILE_F, BB_FILE_G, BB_FILE_H]

  !! @brief what is the purpose of BB_RANKS?
  integer(kind=BBK), parameter, dimension(0:7), public :: &
    BB_RANKS = [BB_RANK_1, BB_RANK_2, BB_RANK_3, BB_RANK_4, &
                BB_RANK_5, BB_RANK_6, BB_RANK_7, BB_RANK_8]

  integer(kind=BBK), dimension(0:64), parameter, public :: &
    BB_SQUARES = [ibset(BB_EMPTY, SQUARES), BB_EMPTY]

  integer(kind=BBK), dimension(0:SQUARE_MASK), parameter, public :: &
    BB_FLOOD_GT = ishft(BB_ALL, int(SQUARES + V_E)), &
    BB_FLOOD_LT = ishft(BB_ALL, SQUARES - int(V_E + SQUARE_MASK)), &
    BB_FLOOD_N = ishft(BB_ALL, SQUARE_FILE(SQUARES) + V_N), &
    BB_RANK = ishft(int(z'00000000000000ff', BBK), &
                    RANK_OFFSET(SQUARES)), &
    BB_FILE = ishft(int(z'0101010101010101', BBK), &
                    SQUARE_FILE(SQUARES)), &
    BB_DIAG = ishft(int(z'8040201008040201', BBK), &
                    SQUARE_FILE(SQUARES) - ishft(RANK_OFFSET(SQUARES), -3)), &
    BB_ANTIDIAG = ishft(int(z'0102040810204080', BBK), &
             SQUARE_FILE(SQUARES) + ishft(RANK_OFFSET(SQUARES), -3) - FILE_MASK)

  integer(kind=BBK), parameter, public :: &
    BB_CORNERS = ior(BB_SQUARES(SQ_A1), &
                     ior(BB_SQUARES(SQ_A8), &
                         ior(BB_SQUARES(SQ_H1), BB_SQUARES(SQ_H8)))), &
    BB_CENTER = ior(BB_SQUARES(SQ_D4), &
                    ior(BB_SQUARES(SQ_E4), &
                        ior(BB_SQUARES(SQ_D5), BB_SQUARES(SQ_E5)))), &
    BB_BACKRANKS = ior(BB_RANK(RANK_1_OFFSET), BB_RANK(RANK_8_OFFSET))

  integer(kind=BBK), parameter, public :: &
    BB_A1 = BB_SQUARES(SQ_A1), BB_A2 = BB_SQUARES(SQ_A2), &
    BB_B1 = BB_SQUARES(SQ_B1), BB_B2 = BB_SQUARES(SQ_B2), &
    BB_C1 = BB_SQUARES(SQ_C1), BB_C2 = BB_SQUARES(SQ_C2), &
    BB_D1 = BB_SQUARES(SQ_D1), BB_D2 = BB_SQUARES(SQ_D2), &
    BB_E1 = BB_SQUARES(SQ_E1), BB_E2 = BB_SQUARES(SQ_E2), &
    BB_F1 = BB_SQUARES(SQ_F1), BB_F2 = BB_SQUARES(SQ_F2), &
    BB_G1 = BB_SQUARES(SQ_G1), BB_G2 = BB_SQUARES(SQ_G2), &
    BB_H1 = BB_SQUARES(SQ_H1), BB_H2 = BB_SQUARES(SQ_H2)

  integer(kind=BBK), parameter, public :: &
    BB_H3 = BB_SQUARES(SQ_H3), BB_H4 = BB_SQUARES(SQ_H4), &
    BB_A3 = BB_SQUARES(SQ_A3), BB_A4 = BB_SQUARES(SQ_A4), &
    BB_B3 = BB_SQUARES(SQ_B3), BB_B4 = BB_SQUARES(SQ_B4), &
    BB_C3 = BB_SQUARES(SQ_C3), BB_C4 = BB_SQUARES(SQ_C4), &
    BB_D3 = BB_SQUARES(SQ_D3), BB_D4 = BB_SQUARES(SQ_D4), &
    BB_E3 = BB_SQUARES(SQ_E3), BB_E4 = BB_SQUARES(SQ_E4), &
    BB_F3 = BB_SQUARES(SQ_F3), BB_F4 = BB_SQUARES(SQ_F4), &
    BB_G3 = BB_SQUARES(SQ_G3), BB_G4 = BB_SQUARES(SQ_G4)

  integer(kind=BBK), parameter, public :: &
    BB_A5 = BB_SQUARES(SQ_A5), BB_A6 = BB_SQUARES(SQ_A6), &
    BB_B5 = BB_SQUARES(SQ_B5), BB_B6 = BB_SQUARES(SQ_B6), &
    BB_C5 = BB_SQUARES(SQ_C5), BB_C6 = BB_SQUARES(SQ_C6), &
    BB_D5 = BB_SQUARES(SQ_D5), BB_D6 = BB_SQUARES(SQ_D6), &
    BB_E5 = BB_SQUARES(SQ_E5), BB_E6 = BB_SQUARES(SQ_E6), &
    BB_F5 = BB_SQUARES(SQ_F5), BB_F6 = BB_SQUARES(SQ_F6), &
    BB_G5 = BB_SQUARES(SQ_G5), BB_G6 = BB_SQUARES(SQ_G6), &
    BB_H5 = BB_SQUARES(SQ_H5), BB_H6 = BB_SQUARES(SQ_H6)

  integer(kind=BBK), parameter, public :: &
    BB_H7 = BB_SQUARES(SQ_H7), BB_H8 = BB_SQUARES(SQ_H8), &
    BB_A7 = BB_SQUARES(SQ_A7), BB_A8 = BB_SQUARES(SQ_A8), &
    BB_B7 = BB_SQUARES(SQ_B7), BB_B8 = BB_SQUARES(SQ_B8), &
    BB_C7 = BB_SQUARES(SQ_C7), BB_C8 = BB_SQUARES(SQ_C8), &
    BB_D7 = BB_SQUARES(SQ_D7), BB_D8 = BB_SQUARES(SQ_D8), &
    BB_E7 = BB_SQUARES(SQ_E7), BB_E8 = BB_SQUARES(SQ_E8), &
    BB_F7 = BB_SQUARES(SQ_F7), BB_F8 = BB_SQUARES(SQ_F8), &
    BB_G7 = BB_SQUARES(SQ_G7), BB_G8 = BB_SQUARES(SQ_G8)

  !! @brief king shield, 3 squares (2 at edges) centered on K, one rank forward
  !! @details pawn attacks 2 squares on diagonals; pawn leaps 1 square forward
  integer(kind=BBK), dimension(0:COLOR_MASK, 0:SQUARE_MASK), &
    parameter, public :: &
    BB_SHIELD_CO = transpose(reshape([ &
                                   merge(ior(BB_SQUARES(ZO(OZ(SQUARES) + ZN)), &
                                        ior(BB_SQUARES(ZO(OZ(SQUARES) + ZNE)), &
                                BB_SQUARES(ZO(OZ(SQUARES) + ZNW)))), BB_EMPTY, &
                                        (.not. btest(BB_BACKRANKS, SQUARES))), &
                                   merge(ior(BB_SQUARES(ZO(OZ(SQUARES) + ZS)), &
                                        ior(BB_SQUARES(ZO(OZ(SQUARES) + ZSE)), &
                                BB_SQUARES(ZO(OZ(SQUARES) + ZSW)))), BB_EMPTY, &
                                       (.not. btest(BB_BACKRANKS, SQUARES)))], &
                                     [1 + SQUARE_MASK, 1 + COLOR_MASK])), &
    BB_PAWN_ATTACKS_CO = transpose(reshape([ &
                                  merge(ior(BB_SQUARES(ZO(OZ(SQUARES) + ZNE)), &
                                 BB_SQUARES(ZO(OZ(SQUARES) + ZNW))), BB_EMPTY, &
                                           (.not. btest(BB_RANK_8, SQUARES))), &
                                  merge(ior(BB_SQUARES(ZO(OZ(SQUARES) + ZSE)), &
                                 BB_SQUARES(ZO(OZ(SQUARES) + ZSW))), BB_EMPTY, &
                                          (.not. btest(BB_RANK_1, SQUARES)))], &
                                          [1 + SQUARE_MASK, 1 + COLOR_MASK])), &
    BB_PAWN_PUSH_CO = transpose(reshape([ &
                             merge(BB_SQUARES(ZO(OZ(SQUARES) + ZN)), BB_EMPTY, &
                                        (.not. btest(BB_BACKRANKS, SQUARES))), &
                             merge(BB_SQUARES(ZO(OZ(SQUARES) + ZS)), BB_EMPTY, &
                                       (.not. btest(BB_BACKRANKS, SQUARES)))], &
                                        [1 + SQUARE_MASK, 1 + COLOR_MASK]))

  !! @brief knights and kings leap toward eight squares each
  integer(kind=BBK), dimension(0:SQUARE_MASK), parameter, public :: &
    BB_KNIGHT_ATTACKS = &
    ior(BB_SQUARES(ZO(OZ(SQUARES) + ZNNE)), &
        ior(BB_SQUARES(ZO(OZ(SQUARES) + ZNNW)), &
            ior(BB_SQUARES(ZO(OZ(SQUARES) + ZENE)), &
                ior(BB_SQUARES(ZO(OZ(SQUARES) + ZWNW)), &
                    ior(BB_SQUARES(ZO(OZ(SQUARES) + ZESE)), &
                        ior(BB_SQUARES(ZO(OZ(SQUARES) + ZWSW)), &
                            ior(BB_SQUARES(ZO(OZ(SQUARES) + ZSSE)), &
                                BB_SQUARES(ZO(OZ(SQUARES) + ZSSW))))))))), &
    BB_KING_ATTACKS = &
    ior(BB_SQUARES(ZO(OZ(SQUARES) + ZNE)), &
        ior(BB_SQUARES(ZO(OZ(SQUARES) + ZN)), &
            ior(BB_SQUARES(ZO(OZ(SQUARES) + ZNW)), &
                ior(BB_SQUARES(ZO(OZ(SQUARES) + ZE)), &
                    ior(BB_SQUARES(ZO(OZ(SQUARES) + ZW)), &
                        ior(BB_SQUARES(ZO(OZ(SQUARES) + ZSE)), &
                            ior(BB_SQUARES(ZO(OZ(SQUARES) + ZS)), &
                                BB_SQUARES(ZO(OZ(SQUARES) + ZSW)))))))))

  !! @brief rays by sq, each of eight ray vectors 1-7 squares away
  integer(kind=BBK), dimension(0:SQUARE_MASK), parameter, public :: &
    BB_RAY_NE = ior(BB_SQUARES(ZO(OZ(SQUARES) + ZNE)), &
                    ior(BB_SQUARES(ZO(OZ(SQUARES) + 2*ZNE)), &
                        ior(BB_SQUARES(ZO(OZ(SQUARES) + 3*ZNE)), &
                            ior(BB_SQUARES(ZO(OZ(SQUARES) + 4*ZNE)), &
                                ior(BB_SQUARES(ZO(OZ(SQUARES) + 5*ZNE)), &
                                    ior(BB_SQUARES(ZO(OZ(SQUARES) + 6*ZNE)), &
                                    BB_SQUARES(ZO(OZ(SQUARES) + 7*ZNE)))))))), &
    BB_RAY_N = ior(BB_SQUARES(ZO(OZ(SQUARES) + ZN)), &
                   ior(BB_SQUARES(ZO(OZ(SQUARES) + 2*ZN)), &
                       ior(BB_SQUARES(ZO(OZ(SQUARES) + 3*ZN)), &
                           ior(BB_SQUARES(ZO(OZ(SQUARES) + 4*ZN)), &
                               ior(BB_SQUARES(ZO(OZ(SQUARES) + 5*ZN)), &
                                   ior(BB_SQUARES(ZO(OZ(SQUARES) + 6*ZN)), &
                                     BB_SQUARES(ZO(OZ(SQUARES) + 7*ZN)))))))), &
    BB_RAY_NW = ior(BB_SQUARES(ZO(OZ(SQUARES) + ZNW)), &
                    ior(BB_SQUARES(ZO(OZ(SQUARES) + 2*ZNW)), &
                        ior(BB_SQUARES(ZO(OZ(SQUARES) + 3*ZNW)), &
                            ior(BB_SQUARES(ZO(OZ(SQUARES) + 4*ZNW)), &
                                ior(BB_SQUARES(ZO(OZ(SQUARES) + 5*ZNW)), &
                                    ior(BB_SQUARES(ZO(OZ(SQUARES) + 6*ZNW)), &
                                    BB_SQUARES(ZO(OZ(SQUARES) + 7*ZNW)))))))), &
    BB_RAY_E = ior(BB_SQUARES(ZO(OZ(SQUARES) + ZE)), &
                   ior(BB_SQUARES(ZO(OZ(SQUARES) + 2*ZE)), &
                       ior(BB_SQUARES(ZO(OZ(SQUARES) + 3*ZE)), &
                           ior(BB_SQUARES(ZO(OZ(SQUARES) + 4*ZE)), &
                               ior(BB_SQUARES(ZO(OZ(SQUARES) + 5*ZE)), &
                                   ior(BB_SQUARES(ZO(OZ(SQUARES) + 6*ZE)), &
                                     BB_SQUARES(ZO(OZ(SQUARES) + 7*ZE)))))))), &
    BB_RAY_W = ior(BB_SQUARES(ZO(OZ(SQUARES) + ZW)), &
                   ior(BB_SQUARES(ZO(OZ(SQUARES) + 2*ZW)), &
                       ior(BB_SQUARES(ZO(OZ(SQUARES) + 3*ZW)), &
                           ior(BB_SQUARES(ZO(OZ(SQUARES) + 4*ZW)), &
                               ior(BB_SQUARES(ZO(OZ(SQUARES) + 5*ZW)), &
                                   ior(BB_SQUARES(ZO(OZ(SQUARES) + 6*ZW)), &
                                     BB_SQUARES(ZO(OZ(SQUARES) + 7*ZW)))))))), &
    BB_RAY_SE = ior(BB_SQUARES(ZO(OZ(SQUARES) + ZSE)), &
                    ior(BB_SQUARES(ZO(OZ(SQUARES) + 2*ZSE)), &
                        ior(BB_SQUARES(ZO(OZ(SQUARES) + 3*ZSE)), &
                            ior(BB_SQUARES(ZO(OZ(SQUARES) + 4*ZSE)), &
                                ior(BB_SQUARES(ZO(OZ(SQUARES) + 5*ZSE)), &
                                    ior(BB_SQUARES(ZO(OZ(SQUARES) + 6*ZSE)), &
                                    BB_SQUARES(ZO(OZ(SQUARES) + 7*ZSE)))))))), &
    BB_RAY_S = ior(BB_SQUARES(ZO(OZ(SQUARES) + ZS)), &
                   ior(BB_SQUARES(ZO(OZ(SQUARES) + 2*ZS)), &
                       ior(BB_SQUARES(ZO(OZ(SQUARES) + 3*ZS)), &
                           ior(BB_SQUARES(ZO(OZ(SQUARES) + 4*ZS)), &
                               ior(BB_SQUARES(ZO(OZ(SQUARES) + 5*ZS)), &
                                   ior(BB_SQUARES(ZO(OZ(SQUARES) + 6*ZS)), &
                                     BB_SQUARES(ZO(OZ(SQUARES) + 7*ZS)))))))), &
    BB_RAY_SW = ior(BB_SQUARES(ZO(OZ(SQUARES) + ZSW)), &
                    ior(BB_SQUARES(ZO(OZ(SQUARES) + 2*ZSW)), &
                        ior(BB_SQUARES(ZO(OZ(SQUARES) + 3*ZSW)), &
                            ior(BB_SQUARES(ZO(OZ(SQUARES) + 4*ZSW)), &
                                ior(BB_SQUARES(ZO(OZ(SQUARES) + 5*ZSW)), &
                                    ior(BB_SQUARES(ZO(OZ(SQUARES) + 6*ZSW)), &
                                    BB_SQUARES(ZO(OZ(SQUARES) + 7*ZSW)))))))), &
    BB_ROOK_RAYS = ior(BB_RAY_N, ior(BB_RAY_E, ior(BB_RAY_W, BB_RAY_S))), &
    BB_BISHOP_RAYS = ior(BB_RAY_NE, ior(BB_RAY_SE, ior(BB_RAY_SW, BB_RAY_NW)))

  !! @brief delta-(rank/8) and delta-file
  !! @details it takes a long time
  integer, private :: f, t
  integer(SQK), dimension(0:SQUARE_MASK, 0:SQUARE_MASK), parameter, public :: &
    BB_DR = reshape([([(RANK_OFFSET(t)/8_SQK - RANK_OFFSET(f)/8_SQK, &
                        f=0, int(SQUARE_MASK))], t=0, int(SQUARE_MASK))], &
                    [1 + SQUARE_MASK, 1 + SQUARE_MASK]), &
    BB_DF = reshape([([(SQUARE_FILE(t) - SQUARE_FILE(f), &
                        f=0, int(SQUARE_MASK))], t=0, int(SQUARE_MASK))], &
                    [1 + SQUARE_MASK, 1 + SQUARE_MASK])

  integer(SQK), dimension(0:SQUARE_MASK, 0:SQUARE_MASK), save, public :: &
    BB_VECTOR

  integer(BBK), dimension(0:SQUARE_MASK, 0:SQUARE_MASK), save, public :: &
    BB_BETWEEN, BB_LINE

contains

  !! least significant (sq); count of trailing zeros
#ifndef DEBUG
  pure &
#endif
    function lsb(bb) result(sq)

    use SquareM, only: SQK
    integer(BBK), intent(in) :: bb
    integer(SQK) :: sq

#ifdef DEBUG
    if (bb == BB_EMPTY) then
      print *, "FATAL: lsb() received an empty bitboard!"
      error stop "Examine move generator logic"
    end if
#endif

    sq = int(trailz(bb), SQK)
  end function lsb

  !! @brief count of trailing zeros; least significant bit
  !! @details scan_forward(bb) -> Iterator[sq]
#ifndef DEBUG
  pure &
#endif
    subroutine pop_lsb(bb, sq)
    use SquareM, only: SQK
    integer(BBK), intent(inout) :: bb
    integer(SQK), intent(out) :: sq

#ifdef DEBUG
    if (bb == BB_EMPTY) then
      print *, "FATAL: pop_lsb() received an empty bitboard!"
      error stop "Loop termination condition missing."
    end if
#endif

    sq = int(trailz(bb), SQK) ! the square
    bb = iand(bb, bb - 1_BBK) ! BLSR bit-layer subregister trick
  end subroutine pop_lsb

  !! @brief most significant (sq), (0 to 63)
#ifndef DEBUG
  pure &
#endif
    function msb(bb) result(sq)
    use SquareM, only: SQK
    integer(BBK), intent(in) :: bb
    integer(SQK) :: sq
#ifdef DEBUG
    if (bb == BB_EMPTY) then
      print *, "FATAL: msb() received an empty bitboard!"
      error stop "Examine the calling move generator."
    end if
#endif
    sq = int(63 - leadz(bb), SQK)
  end function msb

  !! @brief Pop most significant bit (MSB) with fast parallel mask
  !! @details scan_reversed(bb) -> Iterator[sq]
#ifndef DEBUG
  pure &
#endif
    subroutine pop_msb(bb, sq)
    use SquareM, only: SQK
    integer(BBK), intent(inout) :: bb
    integer(SQK), intent(out) :: sq
#ifdef DEBUG
    if (bb == BB_EMPTY) then
      print *, "FATAL: pop_msb() received an empty bitboard!"
      error stop "Loop termination condition missing."
    end if
#endif
    sq = int(63 - leadz(bb), SQK)
    bb = ieor(bb, ishft(1_BBK, sq))
  end subroutine pop_msb

  !! @brief shift down one rank (V_S)
  pure function shift_down(bb) result(res)

    integer(SQK), intent(in) :: bb
    integer(BBK) :: res

    res = int(ishft(bb, V_S), BBK)
  end function shift_down

  !! @brief shift down two ranks (2*V_S)
  pure function shift_2_down(bb) result(res)
    integer(SQK), intent(in) :: bb
    integer(BBK) :: res

    res = int(ishft(bb, V_S*2_SQK), BBK)
  end function shift_2_down

  !! @brief shift up one rank (V_N)
  pure function shift_up(bb) result(res)
    integer(SQK), intent(in) :: bb
    integer(BBK) :: res

    res = int(ishft(bb, V_N), BBK)
  end function shift_up

  !! @brief shift up two ranks (V_N*2_SQK)
  pure function shift_2_up(bb) result(res)

    integer(SQK), intent(in) :: bb
    integer(BBK) :: res

    res = int(ishft(bb, V_N*2_SQK), BBK)
  end function shift_2_up

  !! @brief shift right one file (V_E)
  pure function shift_right(bb) result(res)
    integer(SQK), intent(in) :: bb
    integer(BBK) :: res

    res = iand(int(ishft(bb, V_E), BBK), not(BB_FILE_A))
  end function shift_right

  !! @brief shift right two files (V_E*2_SQK)
  pure function shift_2_right(bb) result(res)
    integer(SQK), intent(in) :: bb
    integer(BBK) :: res

    res = iand(int(ishft(bb, V_E*2_SQK), BBK), ior(BB_FILE_A, BB_FILE_B))
  end function shift_2_right

  !! @brief shift left one file (V_W)
  pure function shift_left(bb) result(res)
    integer(SQK), intent(in) :: bb
    integer(BBK) :: res

    res = int(ishft(bb, V_W), BBK)
  end function shift_left

  !! @brief shift left two files (V_W*2_SQK)
  pure function shift_2_left(bb) result(res)

    integer(SQK), intent(in) :: bb
    integer(BBK) :: res

    res = iand(int(ishft(bb, V_W*2_SQK), BBK), ior(BB_FILE_G, BB_FILE_B))
  end function shift_2_left

  !! @brief shift up one rank and left one file (V_N+V_W)
  pure function shift_up_left(bb) result(res)

    integer(SQK), intent(in) :: bb
    integer(BBK) :: res

    res = iand(int(ishft(bb, V_N + V_W), BBK), not(BB_FILE_H))
  end function shift_up_left

  !! @brief shift up one rank and right one file (V_NE)
  pure function shift_up_right(bb) result(res)

    integer(SQK), intent(in) :: bb
    integer(BBK) :: res

    res = iand(int(ishft(bb, V_N + V_E), BBK), not(BB_FILE_A))
  end function shift_up_right

  !! @brief shift down one rank and left one file (V_S+V_W)
  pure function shift_down_left(bb) result(res)

    integer(SQK), intent(in) :: bb
    integer(BBK) :: res

    res = iand(int(ishft(bb, V_S + V_W), BBK), not(BB_FILE_H))
  end function shift_down_left

  !! @brief shift down one rank and right one file (V_S+V_E)
  pure function shift_down_right(bb) result(res)

    integer(SQK), intent(in) :: bb
    integer(BBK) :: res

    res = iand(int(ishft(bb, V_S + V_E), BBK), not(BB_FILE_A))
  end function shift_down_right

  !! @brief attacks from given sq, stopping at occ, along given deltas
  pure function sliding_attacks_(sq, occ, deltas) result(attacks)

    use SquareM, only: SQK, square_distance

    integer(SQK), intent(in) :: sq
    integer(BBK), intent(in) :: occ
    integer(SQK), dimension(:), intent(in) :: deltas
    integer(BBK) :: attacks

    integer(SQK) :: prev, target_sq
    integer :: i

    attacks = 0_BBK

    !! iterate on directions
    do i = 1, size(deltas)
      prev = sq
      target_sq = sq + deltas(i)

      do
!! if out of range for 64-square board
        if (target_sq < 0_SQK .or. target_sq > 63_SQK) exit

!! detect wrap-around
        if (square_distance(target_sq, prev) > 1) exit

!! record the attack
        attacks = ibset(attacks, target_sq)

!! if sq occupied, block ray
        if (btest(occ, target_sq)) exit

!! next sq
        prev = target_sq
        target_sq = target_sq + deltas(i)
      end do
    end do
  end function sliding_attacks_

  !! @brief attacks mask given sq, ray vector, and occ
  pure function get_ray_attacks(sq, vector, occ) result(bb)

    integer(kind=SQK), intent(in) :: sq, vector
    integer(kind=BBK), intent(in) :: occ
    integer(BBK) :: bb

    integer(kind=BBK) :: blockers, shadow
    integer(kind=SQK) :: first_blocker

    bb = shoot_ray(sq, vector)
    blockers = iand(bb, occ)

    if (blockers /= BB_EMPTY) then
      !! find the first interception
      if (vector > 0) then
        ! positive find blocker at least significant bit LSB
        first_blocker = int(trailz(blockers), SQK)
      else
        ! negative find blocketr at most significant bit MSB
        first_blocker = int(63 - leadz(blockers), SQK)
      end if

      !! remove shadow cast by first_blocker
      shadow = shoot_ray(first_blocker, vector)
      bb = ieor(bb, shadow)
    end if
  end function get_ray_attacks

  !! @brief count nonzero Bitboard bits (popcnt)
  !! @details target cpu instruction popcnt
  pure function n_bits(bb) result(n)

    integer(BBK), intent(in) :: bb
    integer :: n

    n = popcnt(bb)
  end function n_bits

  !! @brief output "stamp" of bitb 1's and .'s
  subroutine print_stamp(bb)

    use SquareM, only: RANK_CHAR

    integer(BBK), intent(in) :: bb

    integer(kind=SQK) :: file, rank, sq
    character(len=1) :: ch
    character, parameter :: SP = ' '

    !! @details decline rank: 7->0, white advance file 0->7,
    !!          sq = (i ^ RANK_MASK) | (i & FILE_MASK), i=0->63

    do rank = RANK_8_OFFSET, RANK_1_OFFSET, V_S
      do file = FILE_A, FILE_H, V_E
        sq = ior(rank, file)
        ch = merge('1', ':', btest(bb, sq))
        write (*, '(a2)', advance='no') SP//ch
        if (SQUARE_FILE(sq) == FILE_H) write (*, '(a)') SP//RANK_CHAR(sq)
      end do
    end do
    write (*, '(a)') ' a b c d e f g h '
  end subroutine print_stamp

  !! @brief produce ray from given square, direction
  pure function shoot_ray(from_sq, vector) result(bb)

    integer(SQK), intent(in) :: from_sq, vector
    integer(BBK) :: bb

    select case (vector)
    case (V_N)
      bb = BB_RAY_N(from_sq) ! North
    case (V_S)
      bb = BB_RAY_S(from_sq) ! South
    case (V_E)
      bb = BB_RAY_E(from_sq) ! East
    case (V_W)
      bb = BB_RAY_W(from_sq) ! West
    case (V_NE)
      bb = BB_RAY_NE(from_sq) ! North-East
    case (V_NW)
      bb = BB_RAY_NW(from_sq) ! North-West
    case (V_SE)
      bb = BB_RAY_SE(from_sq) ! South-East
    case (V_SW)
      bb = BB_RAY_SW(from_sq) ! South-West
    case default
      bb = BB_EMPTY
    end select
  end function shoot_ray

  !! @brief "between" from f to t, 0 if none, 0 if f == t
  subroutine init_ray_tables()

    use SquareM, only: SQK, SQUARE_MASK
    integer :: fr, to
    integer(SQK) :: v
    integer(BBK) :: ray_fwd, ray_bwd, bit

    call init_vector_table()

    BB_BETWEEN = BB_EMPTY
    BB_LINE = BB_EMPTY

    do to = 0, int(SQUARE_MASK)
      do fr = 0, int(SQUARE_MASK)
        v = BB_VECTOR(fr, to)

        !! calculate only if they are on a line (v /= 0)
        if (v /= 0_SQK) then
          ray_fwd = get_ray_in_direction(int(fr, SQK), v) ! f toward t
          ray_bwd = get_ray_in_direction(int(fr, SQK), -v) ! f away from t
          bit = BB_SQUARES(fr)

          !! intersection of rays from fr and to
          BB_BETWEEN(fr, to) = iand(ray_fwd, &
                                    get_ray_in_direction(int(to, SQK), -v))

          !! entire line forward, backward, origin square
          BB_LINE(fr, to) = ior(ior(ray_fwd, ray_bwd), bit)
        end if
      end do
    end do
  end subroutine init_ray_tables

  !! @brief ray given origin sq, and direction v
  pure function get_ray_in_direction(sq, v) result(bb)

    integer(SQK), intent(in) :: sq, v
    integer(BBK) :: bb

    select case (v)
    case (V_N); bb = BB_RAY_N(sq)
    case (V_S); bb = BB_RAY_S(sq)
    case (V_E); bb = BB_RAY_E(sq)
    case (V_W); bb = BB_RAY_W(sq)
    case (V_NE); bb = BB_RAY_NE(sq)
    case (V_SW); bb = BB_RAY_SW(sq)
    case (V_NW); bb = BB_RAY_NW(sq)
    case (V_SE); bb = BB_RAY_SE(sq)
    case default; bb = BB_EMPTY
    end select
  end function get_ray_in_direction

  !! @brief "vector" from f to t, 0 if none, and 0 if f == t
  subroutine init_vector_table()

    use SquareM, only: SQK, SQUARE_MASK, RANK_OFFSET, SQUARE_FILE, &
                       V_N, V_E, V_S, V_W, V_NE, V_SE, V_SW, V_NW
    integer :: fr, to, df, dr

    BB_VECTOR = 0_SQK

    do to = 0, int(SQUARE_MASK)
      do fr = 0, int(SQUARE_MASK)
        if (fr == to) cycle

        df = SQUARE_FILE(to) - SQUARE_FILE(fr)
        dr = RANK_OFFSET(to) - RANK_OFFSET(fr)

        if (df == 0) then
          BB_VECTOR(fr, to) = merge(V_N, V_S, dr > 0)

        else if (dr == 0) then
          BB_VECTOR(fr, to) = merge(V_E, V_W, df > 0)

        else if (8*abs(df) == abs(dr)) then
          BB_VECTOR(fr, to) = merge( &
                              merge(V_NE, V_NW, df > 0), &
                              merge(V_SE, V_SW, df > 0), dr > 0)

        end if
      end do
    end do
  end subroutine init_vector_table

end module BitboardM
