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
!! @brief Game metadata turn, ep-Square, etc. that is not base
module StateM

  use, intrinsic :: iso_fortran_env, only: int8, int16
  use PieceM, only: PCK, WHITE, BLACK
  use SquareM, only: SQK, SQUARE_MASK, EP_NONE, &
                     SQ_A1, SQ_A8, SQ_E1, SQ_E8, SQ_H1, SQ_H8
  use EvaluationM, only: EVK
  use ZobristM, only: ZHK
  implicit none
  private

  public :: &
    new_state, &
    get_state_str, &
    has_kingside_castling_rights, &
    has_queenside_castling_rights, &
    update_state_hash

  integer, parameter, public :: &
    CSK = int8

  !! @brief five items from FEN giving non-positional state
  !! @details this type is stored in StateT at every ply.
  type, public :: StateT

    ! --- Irreversible Rules State ---
    integer(kind=PCK) :: &
      turn = WHITE                !! Side to move (WHITE=0, BLACK=1)
    integer(kind=CSK) :: &
      castling_rights = 0_CSK     !! 4-bit bitfield (0-15) for KQkq
    integer(kind=SQK) :: &
      ep_sq = EP_NONE             !! EP target square (not SQ_NONE)
    integer(kind=int16) :: &
      halfmove_clock = 0_int16, & !! 50-move rule counter
      fullmove_number = 1_int16   !! Incremented after every Black move
    integer(kind=ZHK)  :: &       !! The 64-bit signature of this state
      h = 0_ZHK
  end type

  !! @brief Castling bits for StateT%castling_rights (0-15)
  integer, parameter, public :: &
    W_OO_BIT = 0, &  !! White kingside (bit 0)
    W_OOO_BIT = 1, & !! White queenside (bit 1)
    B_OO_BIT = 2, &  !! Black kingside (bit 2)
    B_OOO_BIT = 3    !! Black queenside (bit 3)

  integer(kind=PCK), parameter, public :: &
    W_OO_MASK = ibset(0_CSK, W_OO_BIT), &   !! White kingside 1_CSK
    W_OOO_MASK = ibset(0_CSK, W_OOO_BIT), & !! White kingside 2_CSK
    B_OO_MASK = ibset(0_CSK, B_OO_BIT), &   !! Black queenside 4_CSK
    B_OOO_MASK = ibset(0_CSK, B_OOO_BIT)    !! Black queenside 8_CSK

  !! @brief Useful combinations for WHITE_CS and BLACK_CS

  integer(kind=PCK), parameter, public :: &
    W_CS_MASK = ior(W_OO_MASK, W_OOO_MASK), & !! = 3_CSK
    B_CS_MASK = ior(B_OO_MASK, B_OOO_MASK), & !! = 12_CSK
    WB_CS_MASK = ior(W_CS_MASK, B_CS_MASK)    !! = 15_CSK

  !! @brief Bitmasks to update cs rights via IAND(rights, mask(sq))
  !! @details SQ_A1=13 (clear W_OOO), SQ_E1=12 (clear both), SQ_H1=14 (clear W_OO)
  !!          SQ_A8=7 (clear B_OOO), SQ_E8=3 (clear both), SQ_H8=11 (clear B_OO)

  integer, private :: sq ! implied do variable

  integer(kind=CSK), dimension(0:63), parameter, public :: &
    CS_UPDATE_MASK = WB_CS_MASK - unpack(vector=[ &
                                         W_OOO_MASK, W_CS_MASK, W_OO_MASK, &
                                         B_OOO_MASK, B_CS_MASK, B_OO_MASK], &
                                         mask=[(any(int(sq, CSK) == [ &
                                                    SQ_A1, SQ_E1, SQ_H1, &
                                                    SQ_A8, SQ_E8, SQ_H8]), &
                                          sq=0, int(SQUARE_MASK))], field=0_CSK)

contains

  !! @brief update hash for castle ep hmcl after pieces move
  pure function calculate_state_hash(state) result(h)

    use SquareM, only: SQUARE_FILE
    use ZobristM, only: ZHASH_TURN, ZHASH_CASTLING, ZHASH_EP_FILE
    type(StateT), intent(in) :: state
    integer(kind=ZHK) :: h

    ! apply turn hash code
    h = 0_ZHK
    if (state%turn == BLACK) h = ZHASH_TURN

    ! apply castling hash code
    h = ieor(h, ZHASH_CASTLING(int(state%castling_rights)))

    ! apply en passant hash code
    if (state%ep_sq /= EP_NONE) &
      h = ieor(h, ZHASH_EP_FILE(SQUARE_FILE(state%ep_sq)))
  end function calculate_state_hash

  !! @brief ret state part of fen string, e.g. "w KQkq e6 3 6"
  pure function get_state_str(state) result(str)

    use SquareM, only: SQUARE_NAME
    type(StateT), intent(in) :: state
    character(len=16) :: str
    character(len=1) :: turn_str
    character(len=4) :: cs_str
    character(len=2) :: ep_str
    character(len=2) :: halfmove_clock_str
    character(len=3) :: fullmove_number_str
    character(len=1), parameter :: SP = ' '

    turn_str = "w"
    if (state%turn == BLACK) turn_str = "b"

    cs_str = ''
    if (btest(state%castling_rights, W_OO_BIT)) cs_str = 'K'
    if (btest(state%castling_rights, W_OOO_BIT)) cs_str = trim(cs_str)//'Q'
    if (btest(state%castling_rights, B_OO_BIT)) cs_str = trim(cs_str)//'k'
    if (btest(state%castling_rights, B_OOO_BIT)) cs_str = trim(cs_str)//'q'
    if (cs_str == '') cs_str = '-'

    ep_str = "-"
    if (state%ep_sq /= EP_NONE) ep_str = SQUARE_NAME(state%ep_sq)

    write (halfmove_clock_str, fmt='(i0)') state%halfmove_clock
    write (fullmove_number_str, fmt='(i0)') state%fullmove_number

    str = turn_str// &
          SP//trim(cs_str)// &
          SP//trim(ep_str)// &
          SP//trim(halfmove_clock_str)// &
          SP//trim(fullmove_number_str)
  end function get_state_str

  !! @brief ret bool whether given side has kingside rights
  pure function has_kingside_castling_rights(rights, co) result(has)

    integer(CSK), intent(in) :: rights
    integer(PCK), intent(in) :: co
    logical :: has

    if (co == BLACK) then
      has = btest(rights, B_OO_BIT) !SQ_H8=1011 (bit2)
    else
      has = btest(rights, W_OO_BIT) !SQ_H1=1110 (bit0)
    end if
  end function has_kingside_castling_rights

  !! @brief ret bool whether given side has queenside rights
  pure function has_queenside_castling_rights(rights, co) result(has)

    integer(CSK), intent(in) :: rights
    integer(PCK), intent(in) :: co
    logical :: has

    if (co == BLACK) then
      has = btest(rights, B_OOO_BIT) !SQ_A8=0111 (bit3)
    else
      has = btest(rights, W_OOO_BIT) !SQ_A1=1101 (bit1)
    end if
  end function has_queenside_castling_rights

  !! @brief Create new StateT with sensible defaults
  pure function new_state(turn_opt, castling_rights_opt, ep_sq_opt) &
    result(state)

    integer(PCK), intent(in), optional :: turn_opt
    integer(CSK), intent(in), optional :: castling_rights_opt
    integer(SQK), intent(in), optional :: ep_sq_opt
    type(StateT) :: state

    state%turn = WHITE
    if (present(turn_opt)) state%turn = turn_opt

    state%castling_rights = WB_CS_MASK
    if (present(castling_rights_opt)) &
      state%castling_rights = castling_rights_opt

    state%ep_sq = EP_NONE
    if (present(ep_sq_opt)) state%ep_sq = ep_sq_opt

    state%halfmove_clock = 0_int16
    state%fullmove_number = 1_int16

    state%h = calculate_state_hash(state)
  end function new_state

  !! @brief update hash for castle ep hmcl after pieces move
  subroutine update_state_hash(new_state, old_state)

    use SquareM, only: SQUARE_FILE
    use ZobristM, only: ZHASH_TURN, ZHASH_CASTLING, ZHASH_EP_FILE
    type(StateT), intent(inout) :: new_state
    type(StateT), intent(in) :: old_state

    associate (h => new_state%h, &
               new_rights => new_state%castling_rights, &
               old_rights => old_state%castling_rights, &
               new_ep_sq => new_state%ep_sq, &
               old_ep_sq => old_state%ep_sq)

      ! always apply turn hash
      h = ieor(h, ZHASH_TURN)

      ! castling (old out, new in)
      if (new_rights /= old_rights) then
        h = ieor(h, ZHASH_CASTLING(int(old_rights)))
        h = ieor(h, ZHASH_CASTLING(int(new_rights)))
      end if

      ! en passant hash
      if (new_ep_sq /= old_ep_sq) then
        if (old_ep_sq /= EP_NONE) &
          h = ieor(h, ZHASH_EP_FILE(SQUARE_FILE(old_ep_sq)))
        if (new_ep_sq /= EP_NONE) &
          h = ieor(h, ZHASH_EP_FILE(SQUARE_FILE(new_ep_sq)))
      end if
    end associate
  end subroutine update_state_hash

end module StateM
