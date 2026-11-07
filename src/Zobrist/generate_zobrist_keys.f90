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
! ======================================================================
! PROGRAM: generate_zobrist_keys (Complete and Robust Version)
! PURPOSE:
!   Creates a ZobristM.f90 module where Zobrist keys are defined as
!   parameters. It avoids compiler line-limits by building a large
!   array from smaller, private parameter arrays. This is the complete
!   and final version of the generator program.
!
! HOW TO USE:
!   1. Save this code as `generate_zobrist_keys.f90`.
!   2. Save the `RandomUtilsM` module (from previous answers) as
!      `RandomUtilsM.f90` in the same directory.
!   3. Compile both files:
!      `gfortran -o generate_hashes RandomUtilsM.f90 generate_zobrist_keys.f90`
!   4. Run the executable: `./generate_hashes`
!   5. A new `ZobristM.f90` file will be created. Use it in your engine.
! ======================================================================
program generate_zobrist_keys
  use RandomUtilsM, only: generate_random_int64
  use, intrinsic :: iso_fortran_env, only: int64
  implicit none

  character(len=*), parameter :: OUTPUT_FILENAME = "ZobristM.f90"
  integer, parameter :: F_UNIT = 10, SEED_VALUE = 20240503
  integer, parameter :: &
          NUM_SQUARES = 64, &
          NUM_PIECE_CODES = 14, &
          NUM_CASTLING_RIGHTS = 4, &
          NUM_EP_COLS = 8, &
          NUM_SIDES = 2 
  integer :: i, iostat_check
  character(len=31), parameter :: ZERO = "int(z'0000000000000000', ZHK)"
  character(len=5), dimension(0:13) :: PIECE_VAR_NAMES = [ &
      "EMPTY", "BLACK", &
      "WP   ", "BP   ", "WN   ", "BN   ", "WB   ", "BB   ", &
      "WR   ", "BR   ", "WQ   ", "BQ   ", "WK   ", "BK   " ]

  print *, "==============================================="
  print *, "   Zobrist Key Module Generator (Complete)"
  print *, "==============================================="

  open(unit=F_UNIT, file=OUTPUT_FILENAME, status='replace', &
       action='write', iostat=iostat_check)
  if (iostat_check /= 0) then
    stop "FATAL: Could not open output file."
  end if
  print *, " > Opened '", trim(OUTPUT_FILENAME), "' for writing."

  call write_module_header()
  do i = 0, NUM_PIECE_CODES - 1
    call write_single_piece_array(PIECE_VAR_NAMES(i))
  end do
  call write_final_assembly(PIECE_VAR_NAMES)
  call write_castling_keys()
  call write_ep_col_keys()
  call write_turn_key()
  call write_module_footer()

  close(F_UNIT)
  print *, " > '", trim(OUTPUT_FILENAME), "' generated successfully."
  print *, "==============================================="

contains

  !! @brief write header for hash key module
  subroutine write_module_header()

    write(F_UNIT, '(a)') "module ZobristM"
    write(F_UNIT, '(a)') "  use, intrinsic :: iso_fortran_env, only: int64"
    write(F_UNIT, '(a)') "  implicit none"
    write(F_UNIT, '(a)') "  private"
    write(F_UNIT, '(a)') ""
    write(F_UNIT, '(a)') "  ! Define the data type as int64"
    write(F_UNIT, '(a)') "  integer, parameter, public :: ZHK = int64"
    write(F_UNIT, '(a)') ""
  end subroutine write_module_header

  !! @brief write hash keys for a single piece
  subroutine write_single_piece_array(pc_name)

    character(len=*), intent(in) :: pc_name
    integer :: i
    character(len=80) :: lines(NUM_SQUARES)

    write(F_UNIT, '(2x, a)') "! Keys for piece type '" // trim(pc_name) // "'"
    write(F_UNIT, '(2x, a)') "integer(kind=ZHK), dimension(0:63), private, &"
    write(F_UNIT, '(6x, a)') "parameter :: ZHASH_" // trim(pc_name) // " = &"

    if (pc_name /= "EMPTY" .and. pc_name /= "BLACK") then
      write(F_UNIT, '(2x,a)') "[ &"
      do i = 1, NUM_SQUARES

        ! FIX: Write the simple string variable with a simple '(a)' format.
        write(F_UNIT, '(a)') trim("    " // &
              ternary(pc_name == "EMPTY" .or. pc_name == "BLACK", ZERO, &
              trim(get_formatted_random_key())) &
              // ternary(i == NUM_SQUARES, "  &", ", &"))

      end do
      write(F_UNIT, '(2x, a)') "]"
    else
      write(F_UNIT, '(2x, a)') "0_ZHK"
    end if
    write(F_UNIT, '(a)') ""
    print *, "   - Wrote private parameter ZHASH_", trim(pc_name)
  end subroutine write_single_piece_array

  !! @brief write assemble hash keys as piece-square table
  subroutine write_final_assembly(names)

    character(len=*), dimension(0:13), intent(in) :: names
    write(F_UNIT, '(2x, a)') "! Assemble 2D array from private arrays"
    write(F_UNIT, '(2x, a)') "integer(kind=ZHK), dimension(0:13, 0:63), &"
    write(F_UNIT, '(6x, a)') "parameter, public :: ZHASH_PST = &"
    write(F_UNIT, '(6x, a)') "reshape([ &"
    write(F_UNIT, '(6x, a)') "ZHASH_EMPTY, ZHASH_BLACK, ZHASH_WP, ZHASH_BP, &"
    write(F_UNIT, '(6x, a)') "ZHASH_WN, ZHASH_BN, ZHASH_WB, ZHASH_BB, &"
    write(F_UNIT, '(6x, a)') "ZHASH_WR, ZHASH_BR, ZHASH_WQ, ZHASH_BQ, &"
    write(F_UNIT, '(6x, a)') "ZHASH_WK, ZHASH_BK &"
    write(F_UNIT, '(a)') "      ], shape=[14, 64])"
    write(F_UNIT, '(a)') ""
    print *, "   - Wrote ZHASH_PST assembly..."
  end subroutine write_final_assembly

  !! @brief write (4) hash keys for castling bits
  subroutine write_castling_keys()

    integer :: i
    write(F_UNIT, '(2x, a)') "! Keys for each of the castling rights -KQkq"
    write(F_UNIT, '(2x, a)') "integer(kind=ZHK), parameter, public :: &"
    write(F_UNIT, '(6x, a)') "ZH1 = " // &
              trim(get_formatted_random_key()) // ", &"
    write(F_UNIT, '(6x, a)') "ZA1 = " // &
              trim(get_formatted_random_key()) // ", &"
    write(F_UNIT, '(6x, a)') "ZH8 = " // &
              trim(get_formatted_random_key()) // ", &"
    write(F_UNIT, '(6x, a)') "ZA8 = " // &
              trim(get_formatted_random_key())

    write(F_UNIT, '(2x, a)') "integer(kind=ZHK), dimension(0:15), &"
    write(F_UNIT, '(6x, a)') "parameter, public :: ZHASH_CASTLING = [ &"

    write(F_UNIT, '(6x, a)') &
       "0_ZHK,                                  & ! . . . .            ", &
       "                                   ZH1,   & ! . . . O-O          ", &
       "                              ZA1,        & ! . . O-O-O .        ", &
       "                         ieor(ZA1, ZH1),  & ! . . O-O-O O-O      ", &
       "                    ZH8,                  & ! . o-o . .          ", &
       "               ieor(ZH8,           ZH1),  & ! . o-o . O-O        ", &
       "               ieor(ZH8,      ZA1),       & ! . o-o O-O-O .      ", &
       "               ieor(ZH8, ieor(ZA1, ZH1)), & ! . o-o O-O-O O-O    ", &
       "          ZA8,                            & ! o-o-o . . .        ", &
       "     ieor(ZA8,                     ZH1),  & ! o-o-o . . O-O      ", &
       "     ieor(ZA8,                ZA1),       & ! o-o-o . O-O-O .    ", &
       "     ieor(ZA8,           ieor(ZA1, ZH1)), & ! o-o-o . O-O-O O-O  ", &
       "     ieor(ZA8,      ZH8),                 & ! o-o-o o-o . .      ", &
       "     ieor(ZA8, ieor(ZH8,           ZH1)), & ! o-o-o o-o . O-O    ", &
       "ieor(ieor(ZA8, ZH8),          ZA1),       & ! o-o-o o-o O-O-O .  ", &
       "ieor(ieor(ZA8, ZH8),     ieor(ZA1, ZH1))  & ! o-o-o o-o O-O-O O-O"

    write(F_UNIT, '(a)') "  ]"
    write(F_UNIT, '(a)') ""
    print *, "   - Wrote ZHASH_CASTLING table..."
  end subroutine write_castling_keys

  !! @brief write hash keys for ep_col
  subroutine write_ep_col_keys()

    integer :: i

    write(F_UNIT, '(a)') "  ! Keys for each of 8 en-passant file columns"
    write(F_UNIT, '(a)') "  integer(kind=ZHK), dimension(0:7), &"
    write(F_UNIT, '(a)') "      parameter, public :: ZHASH_EP_COL = [ &"
    do i = 1, NUM_EP_COLS
      write(F_UNIT, '(a)') "    " // &
              trim(get_formatted_random_key()) // &
              ternary(i == NUM_EP_COLS, "  &", ", &")
    end do
    write(F_UNIT, '(a)') "  ]"
    write(F_UNIT, '(a)') ""
    print *, "   - Wrote ZHASH_EP_COL table..."
  end subroutine write_ep_col_keys

  !! @brief write the hash key for turn
  subroutine write_turn_key()

    integer :: iturn

    write(F_UNIT, '(2x, a)') "! Key to flip when the side to move is Black"
    write(F_UNIT, '(2x, a)') "integer(kind=ZHK), parameter, public :: &"
    write(F_UNIT, '(6x, a)') "ZHASH_TURN =  &"
    write(F_UNIT, '(4x, a)') trim(get_formatted_random_key())
    write(F_UNIT, '(a)') ""
    print *, "   - Wrote ZHASH_TURN key..."
  end subroutine write_turn_key

  !! @brief write end of hash key module
  subroutine write_module_footer()

    write(F_UNIT, '(a)') "end module ZobristM"
  end subroutine write_module_footer


  !! @brief string-ify a random key for lossless output
  function get_formatted_random_key() result(fmtd_key)

    character(len=40) :: fmtd_key
    character(len=16) :: hex_str

    fmtd_key = ""
    write(hex_str, '(z16.16)') generate_random_int64()
    write(fmtd_key, "(a)") "int(z'" // hex_str // "', ZHK)"
  end function get_formatted_random_key


  pure function ternary(condition, true_val, false_val) result(res)

    logical, intent(in) :: condition
    character(len=*), intent(in) :: true_val, false_val
    character(len=max(len(true_val), len(false_val))) :: res

    if (condition) then
      res = true_val
    else
      res = false_val
    end if
  end function ternary

end program generate_zobrist_keys


