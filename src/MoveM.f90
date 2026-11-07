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
!! @brief Move generation
module MoveM
  use, intrinsic :: iso_fortran_env, only: int8, int16, int64
  use PieceM, only: PCK, EMPTY
  use SquareM, only: SQK, SQ_A1, SQ_NONE
  use StateM, only: StateT
  use EvaluationM, only: EVK
  use ZobristM, only: ZHK
  implicit none
  private

  ! move generation
  public :: &
    add_targets

  ! move
  public :: &
    get_uci, &
    get_full_move, &
    from_uci, &
    null_move, &
    get_from_sq, &
    get_to_sq, &
    get_piece, &
    get_captured, &
    get_move_type, &
    get_priority, &
    get_promotion, &
    get_mvv_lva, &
    move_to_full_pcn, &
    pack_move, &
    retro_score, &
    unpack_move

  ! hash and score updates
  public :: &
    advanced_score

  integer, parameter, public :: MVK = int64

  integer(int8), parameter, public :: &
    NORMAL_MOVE = 0_int8, & ! quiet moves and standard capture
    DOUBLE_PUSH = 1_int8, & ! pawn moves two squares ahead (sets ep_sq)
    CASTLE_MOVE = 2_int8, & ! king moves two squares, triggers rook move
    EP_MOVE = 3_int8, & ! en passant remove pawn from 1-hop square
    PROMO_MOVE = 4_int8    ! general promotion flag, mv%promotion is the pc

  character(len=11), dimension(0:4), parameter, public :: &
    MOVE_TYPE_NAMES = [ &
    "NORMAL_MOVE", &
    "DOUBLE_PUSH", &
    "CASTLE_MOVE", &
    "EP_MOVE    ", &
    "PROMO_MOVE "]

  !! @brief move datum bit-widths
  integer, parameter, public :: &
    NBITS_FROM_SQ = 6, &
    NBITS_TO_SQ = 6, &
    NBITS_PIECE = 4, &
    NBITS_CAPTURED = 4, &
    NBITS_PROMOTION = 4, &
    NBITS_MOVE_TYPE = 4, &
    NBITS_PRIORITY = 16

  !! @brief (put PRIORITY toward MSB)
  integer, parameter, public :: &
    SHIFT_FROM_SQ = 0, &
    SHIFT_TO_SQ = NBITS_FROM_SQ + SHIFT_FROM_SQ, &
    SHIFT_PIECE = NBITS_TO_SQ + SHIFT_TO_SQ, &
    SHIFT_CAPTURED = NBITS_PIECE + SHIFT_PIECE, &
    SHIFT_PROMOTION = NBITS_CAPTURED + SHIFT_CAPTURED, &
    SHIFT_MOVE_TYPE = NBITS_PROMOTION + SHIFT_PROMOTION, &
    SHIFT_PRIORITY = NBITS_MOVE_TYPE + SHIFT_MOVE_TYPE

  !! @brief move datum masks
  integer(MVK), parameter, public :: &
    MASK_MOVE_TYPE = ishft(ishft(1_MVK, NBITS_MOVE_TYPE) - 1, &
                           SHIFT_MOVE_TYPE), &
    MASK_FROM_SQ = ishft(ishft(1_MVK, NBITS_FROM_SQ) - 1, SHIFT_FROM_SQ), &
    MASK_TO_SQ = ishft(ishft(1_MVK, NBITS_TO_SQ) - 1, SHIFT_TO_SQ), &
    MASK_PIECE = ishft(ishft(1_MVK, NBITS_PIECE) - 1, SHIFT_PIECE), &
    MASK_CAPTURED = ishft(ishft(1_MVK, NBITS_CAPTURED) - 1, SHIFT_CAPTURED), &
    MASK_PROMOTION = ishft(ishft(1_MVK, NBITS_PROMOTION) - 1, &
                           SHIFT_PROMOTION), &
    MASK_PRIORITY = ishft(ishft(1_MVK, NBITS_PRIORITY) - 1, SHIFT_PRIORITY)

contains

  !! @brief roll the score backwards by nn ply
  pure elemental function retro_score(g) result(retro)

    use EvaluationM, only: EVK, ILLEGAL, MATE, MATED

    integer(kind=EVK), intent(in) :: g
    integer(kind=EVK) :: retro

    if (g == ILLEGAL) then
      retro = ILLEGAL
    else if (g >= MATE) then
      retro = -g + 1_EVK
    else if (g <= MATED) then
      retro = -g - 1_EVK
    else
      retro = -g
    end if
  end function retro_score

  !! @brief store move compactly, ~40 bits
  pure elemental &
    function pack_move(priority, from_sq, to_sq, piece, captured, promotion, &
                       move_type) result(mv)

    integer(int16), intent(in) :: &
      priority      !! Move ordering score (Priority)
    integer(SQK), intent(in) :: &
      from_sq, &    !! Origin (0-63)
      to_sq         !! Destination (0-63)
    integer(PCK), intent(in) :: &
      piece, &      !! The moving piece (type + color)
      captured, &   !! The victim (type + color, or EMPTY)
      promotion     !! The new piece identity (or EMPTY)
    integer(int8), intent(in) :: &
      move_type     !! Move type ()

    integer(MVK) :: mv

    mv = &
      ior(ishft(int(priority, MVK), SHIFT_PRIORITY), &
          ior(ishft(int(from_sq, MVK), SHIFT_FROM_SQ), &
              ior(ishft(int(to_sq, MVK), SHIFT_TO_SQ), &
                  ior(ishft(int(piece, MVK), SHIFT_PIECE), &
                      ior(ishft(int(captured, MVK), SHIFT_CAPTURED), &
                          ior(ishft(int(promotion, MVK), SHIFT_PROMOTION), &
                              ishft(int(move_type, MVK), SHIFT_MOVE_TYPE)))))))
  end function pack_move

  !! @brief unpack the int32 move into parts: from_sq, to_sq, piece, etc.
  pure elemental &
    subroutine unpack_move(mv, priority, from_sq, to_sq, &
                           piece, captured, promotion, move_type)

    integer(kind=MVK), intent(in) :: mv
    integer(kind=int16), intent(out) :: &
      priority
    integer(kind=SQK), intent(out) :: &
      from_sq, &    !! Origin (0-63)
      to_sq         !! Destination (0-63)
    integer(kind=PCK), intent(out) :: &
      piece, &      !! The moving piece (type + color)
      captured, &   !! The victim (type + color, or EMPTY)
      promotion     !! The new piece identity (or EMPTY)
    integer(kind=int8), intent(out) :: &
      move_type

    priority = int(ishft(iand(mv, MASK_PRIORITY), -SHIFT_PRIORITY), int16)
    from_sq = int(ishft(iand(mv, MASK_FROM_SQ), -SHIFT_FROM_SQ), SQK)
    to_sq = int(ishft(iand(mv, MASK_TO_SQ), -SHIFT_TO_SQ), SQK)
    piece = int(ishft(iand(mv, MASK_PIECE), -SHIFT_PIECE), PCK)
    captured = int(ishft(iand(mv, MASK_CAPTURED), -SHIFT_CAPTURED), PCK)
    promotion = int(ishft(iand(mv, MASK_PROMOTION), -SHIFT_PROMOTION), PCK)
    move_type = int(ishft(iand(mv, MASK_MOVE_TYPE), -SHIFT_MOVE_TYPE), int8)

  end subroutine unpack_move

  !! @brief uci string for move, ^[a-h][1-8][a-h][1-8][qnbr]?$
  !! @details get_uci(self) -> string, python-chess
  pure elemental function get_uci(mv) result(str)

    use PieceM, only: EMPTY, PIECE_CHAR
    use SquareM, only: SQUARE_NAME
    use StringUtilsM, only: to_lower

    integer(MVK), intent(in) :: mv
    character(len=5) :: str

    if (get_promotion(mv) /= EMPTY) then
      str = SQUARE_NAME(get_from_sq(mv))// &
            SQUARE_NAME(get_to_sq(mv))// &
            to_lower(PIECE_CHAR(get_promotion(mv)))
    else if (mv /= 0_MVK) then
      str = trim(SQUARE_NAME(get_from_sq(mv))// &
                 SQUARE_NAME(get_to_sq(mv)))
    else
      str = "0000"
    end if
  end function get_uci

  !! @details get_uci_flag(self) -> string, python-chess
  pure function get_full_move(mv) result(str)

    use PieceM, only: EMPTY, PIECE_CHAR
    use SquareM, only: SQUARE_NAME

    integer(MVK), intent(in) :: mv
    character(len=23) :: str
    character(len=1) :: dash_cross, blank_equal

    dash_cross = merge("-", "x", get_captured(mv) == EMPTY)
    blank_equal = merge(" ", "=", get_promotion(mv) == EMPTY)
    str = &
      PIECE_CHAR(get_piece(mv))// &
      SQUARE_NAME(get_from_sq(mv))// &
      dash_cross// &
      PIECE_CHAR(get_captured(mv))// &
      SQUARE_NAME(get_to_sq(mv))// &
      blank_equal// &
      PIECE_CHAR(get_promotion(mv))//" ("// &
      MOVE_TYPE_NAMES(get_move_type(mv))//")"

  end function get_full_move

  !! @brief parse a UCI string, ^[a-h][1-8][a-h][1-8][qnbr]?$
  !! @details from_get_uci(cls, uci: str) -> Move
  pure function from_uci(str) result(mv)

    use PieceM, only: EMPTY, EMPTY, parse_piece
    use SquareM, only: SQ_NONE, parse_square

    character(len=*), intent(in) :: str
    integer(MVK) :: mv

    integer(SQK) :: f_sq, t_sq
    integer(PCK) :: promo

    mv = -1_MVK
    if (str == "0000") then
      mv = 0_MVK

    else if (len_trim(str) == 4 .or. len_trim(str) == 5) then

      !! standard move ^[a-h][1-8][a-h][1-8]$
      f_sq = parse_square(str(1:2))
      t_sq = parse_square(str(3:4))
      if (f_sq == SQ_NONE .or. t_sq == SQ_NONE) return

      promo = EMPTY
      if (len_trim(str) == 5) then
        promo = parse_piece(str(5:5))
        if (promo == EMPTY) return
      end if

      !! python logic from_sq == to_sq is invalid
      if (f_sq == t_sq) return

      mv = pack_move(priority=0_int16, &
                     from_sq=f_sq, &
                     to_sq=t_sq, &
                     piece=EMPTY, &
                     captured=EMPTY, &
                     promotion=promo, &
                     move_type=0_int8)
    end if

    !! The piece and captured fields are filled later by a validate move
    !! or a make move routine, because form_uci only views the string input
  end function from_uci

  !! @brief supply/represent null move
  pure function null_move() result(mv)

    integer(MVK) :: mv
    mv = 0_MVK
  end function null_move

  !! @brief from square, givem mv
  pure elemental function get_from_sq(mv) result(sq)

    integer(kind=MVK), intent(in) :: mv
    integer(SQK) :: sq

    sq = int(ishft(iand(mv, MASK_FROM_SQ), -SHIFT_FROM_SQ), SQK)
  end function get_from_sq

  !! @brief to square, given mv
  pure elemental function get_to_sq(mv) result(sq)

    integer(kind=MVK), intent(in) :: mv
    integer(SQK) :: sq

    sq = int(ishft(iand(mv, MASK_TO_SQ), -SHIFT_TO_SQ), SQK)
  end function get_to_sq

  !! @brief moving piece, given mv
  pure elemental function get_piece(mv) result(pc)

    integer(kind=MVK), intent(in) :: mv
    integer(PCK) :: pc

    pc = int(ishft(iand(mv, MASK_PIECE), -SHIFT_PIECE), PCK)
  end function get_piece

  !! @brief captured piece, given mv
  pure elemental function get_captured(mv) result(pc)

    integer(kind=MVK), intent(in) :: mv
    integer(PCK) :: pc

    pc = int(ishft(iand(mv, MASK_CAPTURED), -SHIFT_CAPTURED), PCK)
  end function get_captured

  pure elemental function get_promotion(mv) result(pc)

    integer(kind=MVK), intent(in) :: mv
    integer(PCK) :: pc

    pc = int(ishft(iand(mv, MASK_PROMOTION), -SHIFT_PROMOTION), PCK)
  end function get_promotion

  pure elemental function get_priority(mv) result(priority)

    integer(kind=MVK), intent(in) :: mv
    integer(int16) :: priority

    priority = int(ishft(iand(mv, MASK_PRIORITY), -SHIFT_PRIORITY), int16)
  end function get_priority

  !!
  pure elemental function get_move_type(mv) result(mv_ty)

    integer(kind=MVK), intent(in) :: mv
    integer(int8) :: mv_ty

    mv_ty = int(ishft(iand(mv, MASK_MOVE_TYPE), -SHIFT_MOVE_TYPE), int8)
  end function get_move_type

  !! @brief Quick MVV-LVA scoring for move ordering.
  pure elemental function get_mvv_lva(mv) result(ev)

    use PieceM, only: EMPTY
    use EvaluationM, only: EVK, PIECE_VALUE

    integer(MVK), intent(in) :: mv
    integer(EVK) :: ev

    !! Formula: (VictimValue * 10) - AttackerValue
    !! A Pawn (100) capturing a Queen (900) = 9000 - 100 = 8900.
    !! A Queen (900) capturing a Pawn (100) = 1000 - 900 = 100.
    if (get_captured(mv) /= EMPTY) then
      ev = abs(PIECE_VALUE(get_captured(mv)))*5_EVK &
           - abs(PIECE_VALUE(get_piece(mv)))
    else
      ev = 0_EVK
    end if

    !! Boost promotions above regular captures
    if (get_promotion(mv) /= EMPTY) ev = ev + 10000_EVK
  end function get_mvv_lva

  !! @brief convert move to Fully Explicit "Piece-Coordinate Notation"
  !! @details ^([KQRBNP])?([a-h][1-8])(-|x[KQRBNP])([a-h][1-8])(=[QRBN])?$
  pure function move_to_full_pcn(mv) result(pcn)

    use PieceM, only: EMPTY, PIECE_CHAR
    use SquareM, only: SQUARE_NAME

    integer(MVK), intent(in) :: mv
    character(len=12) :: pcn

    character(len=12) :: buffer

    ! moving piece + origin square: [Piece][Square] -> e.g., "Pe2 or "ne7"
    ! if capturing, append: "x[Piece]", if not: "-"

    if (get_captured(mv) /= EMPTY) then
      buffer = &
        PIECE_CHAR(get_piece(mv))// &
        SQUARE_NAME(get_from_sq(mv))// &
        "x"// &
        PIECE_CHAR(get_captured(mv))// &
        SQUARE_NAME(get_to_sq(mv))
    else
      buffer = &
        PIECE_CHAR(get_piece(mv))// &
        SQUARE_NAME(get_from_sq(mv))// &
        "-"// &
        SQUARE_NAME(get_to_sq(mv))
    end if

    ! if promoting, append promotion piece, format: "=[Piece]" -> e.g., "=Q"
    if (get_promotion(mv) /= EMPTY) then
      buffer = &
        trim(buffer)// &
        "="// &
        PIECE_CHAR(get_promotion(mv))
    end if

    ! ensure no leading spaces
    pcn = adjustl(buffer)
  end function move_to_full_pcn

  !! @brief shoot vector from square and trim sq beyond first occ
  subroutine add_targets(from_sq, vector, total_occ, targets)

    use SquareM, only: SQK
    use BitboardM, only: BBK, BB_EMPTY, shoot_ray

    integer(SQK), intent(in) :: from_sq, vector
    integer(kind=BBK), intent(in) :: total_occ
    integer(kind=BBK), intent(inout) :: targets

    integer(kind=SQK) ::  first_blocker
    integer(kind=BBK) :: blockers, ray, reachable

    ! ray shooting logic
    ray = shoot_ray(from_sq, vector)

    ! find strikes
    blockers = iand(ray, total_occ)

    ! if no blockers, adopt the whole ray, and done
    if (blockers == BB_EMPTY) then
      targets = ior(targets, ray)
      return
    end if

    ! advancing vector hits at lsb, declining vector hits at msb
    first_blocker = int(merge(trailz(blockers), &
                              63 - leadz(blockers), vector > 0), SQK)

    ! get moves up to and including the blocker
    reachable = ieor(ray, shoot_ray(first_blocker, vector))
    targets = ior(targets, reachable)
  end subroutine add_targets

  !! --- Update

  !! @brief doubled pawns, rooks on open files, bad bishop
  function advanced_score(base) result(score)

    use PieceM, only: BLACK, WHITE
    use SquareM, only: FILE_MASK, V_N, V_S
    use BitboardM, only: &
      BBK, BB_FILE, BB_DARK_SQUARES, BB_EMPTY, BB_RANK_2, BB_RANK_7
    use BaseM, only: BaseT
    use EvaluationM, only: EVK

    type(BaseT), intent(in) :: base
    integer :: col
    integer(kind=BBK) :: doubled_mask
    integer(kind=EVK) :: score

    score = 0_EVK

    !! @brief doubled pawns evaluation
    !! @details doubled pawns are friendly pawns, one square behind the other.
    !! In 0:63 LEFR, shift (either north or south) by 8 bits

    doubled_mask = iand(base%pawns_co(WHITE), ishft(base%pawns_co(WHITE), V_S))
    score = score - int(popcnt(doubled_mask)*15, EVK)
    doubled_mask = iand(base%pawns_co(BLACK), ishft(base%pawns_co(BLACK), V_N))
    score = score + int(popcnt(doubled_mask)*15, EVK)

    !! @brief rooks on open files evaluation
    !! @details an open file has no pawns of either color
    !! pre-calculated 64-bit bitboards of cols

    do col = 0, int(FILE_MASK)
      if (iand(ior(base%pawns_co(WHITE), &
                   base%pawns_co(BLACK)), BB_FILE(col)) == BB_EMPTY) then
        if (iand(base%rooks_co(WHITE), BB_FILE(col)) /= BB_EMPTY) &
          score = score + 20_EVK
        if (iand(base%rooks_co(BLACK), BB_FILE(col)) /= BB_EMPTY) &
          score = score - 20_EVK
      end if
    end do

    !! @brief bad bishop evaluation
    if (iand(base%bishops_co(WHITE), BB_DARK_SQUARES) /= BB_EMPTY) &
      score = score - &
              int((popcnt(iand(base%pawns_co(WHITE), BB_DARK_SQUARES))*5), EVK)
    if (iand(base%bishops_co(BLACK), BB_DARK_SQUARES) /= BB_EMPTY) &
      score = score - &
              int((popcnt(iand(base%pawns_co(BLACK), BB_DARK_SQUARES))*5), EVK)

    ! king safety
    !
    ! TODO pre-calculate SHIELD_MASKS(sq) for every King square
    !
    !shields = iand(base%pawns_co(WHITE), SHIELD_MASKS(base%king(WHITE)))
    !if(shields /= BB_EMPTY)then
    !  score = score + 10 * popcnt(shields)
    !end if
    !shields = iand(base%pawns_co(BLACK), SHIELD_MASKS(base%king(BLACK)))
    !if(shields /= BB_EMPTY)then
    !  score = score + 10 * popcnt(shields)
    !end if

    ! rook on the seventh rank
    if (iand(base%rooks_co(WHITE), BB_RANK_7) /= BB_EMPTY) &
      score = score + &
              int((20*popcnt(iand(base%rooks_co(WHITE), BB_RANK_7))), EVK)
    if (iand(base%rooks_co(BLACK), BB_RANK_2) /= BB_EMPTY) &
      score = score + &
              int((20*popcnt(iand(base%rooks_co(BLACK), BB_RANK_7))), EVK)
  end function advanced_score

end module MoveM

