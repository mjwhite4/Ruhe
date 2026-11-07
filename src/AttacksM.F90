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

! @brief Pins Checks, and Tactical Ray/Attack Queries
! @detail Isolate low-level tactical computations of checking rays, pins,
! evasions, and move legality checks re. king safety
submodule(BoardM) AttacksM
  implicit none

contains

  !! @brief whether given mv is legal in current position
  module &
    function is_legal(self, mv) result(res_tf)

    use MoveStackM, only: MVK
    use MoveM, only: get_uci

    class(BoardT), intent(in) :: self
    integer(MVK), intent(in) :: mv
    logical :: res_tf
    logical :: is_into_check_tf, is_pseudo_legal_tf

    ! t_or_f =  self%is_pseudo_legal(mv)
    ! print *, "self%is_pseudo_legal(mv)=", t_or_f

    res_tf = .false.
    call self%is_pseudo_legal(mv, is_pseudo_legal_tf)
    if (.not. is_pseudo_legal_tf) return

    call self%is_into_check(mv, is_into_check_tf)
    ! print *, "L972: is_into_check mv=", get_uci(mv), is_into_check_tf
    res_tf = .not. is_into_check_tf
  end function is_legal

  !! @brief true/false whether pos after mv is pseudo legal (may endanger king)
  module &
    pure &
    subroutine is_pseudo_legal(self, mv, res_tf)

    use PieceM, only: PCK, EMPTY, PAWN, KING, PIECE_TYPE
    use SquareM, only: SQK
    use MoveStackM, only: MVK
    use MoveM, only: get_from_sq, get_to_sq, get_piece
    use BitboardM, only: BB_EMPTY, BB_SQUARES

    class(BoardT), intent(in) :: self
    integer(MVK), intent(in) :: mv
    logical, intent(out) :: res_tf

    integer(PCK) :: pc
    integer(SQK) :: from_sq, to_sq
    integer(BBK) :: from_mask, to_mask

    res_tf = .false.

    from_sq = get_from_sq(mv)
    to_sq = get_to_sq(mv)
    pc = get_piece(mv)

    if (pc == EMPTY) return ! source sq must be nonempty

    from_mask = BB_SQUARES(from_sq)
    to_mask = BB_SQUARES(to_sq)

    ! from sq and turn must agree
    if (.not. btest(self%occupied_co(self%state%turn), from_sq)) return

    ! only pawns promote, only on their eighth rank, only to NBRQ
    select case (PIECE_TYPE(pc))
    case (PAWN)
      res_tf = self%is_valid_pawn_move(from_sq, to_sq)
    case (KING)
      ! standard king move or castling
      res_tf = (iand(self%attacks_mask(from_sq), to_mask) /= BB_EMPTY)
      if (.not. res_tf) res_tf = self%is_valid_castling_move(mv)
    case default
      ! leapers and sliders use attack bitboards
      res_tf = (iand(self%attacks_mask(from_sq), to_mask) /= BB_EMPTY)
    end select
  end subroutine is_pseudo_legal

  !! @brief t/f whether move is into check
  subroutine is_into_check(self, mv, res_tf)

    use BitboardM, only: BBK, BB_EMPTY, BB_LINE
    use MoveM, only: get_from_sq, get_to_sq, get_move_type, EP_MOVE, get_uci
    use PieceM, only: XCOLOR, BLACK, WHITE
    use SquareM, only: SQK

    class(BoardT), intent(in) :: self
    integer(MVK), intent(in) :: mv
    logical, intent(out) :: res_tf

    integer(SQK) :: k_sq, from_sq, to_sq
    integer(BBK) :: chkrs, pinned_mask, occ

    res_tf = .false.
    from_sq = get_from_sq(mv)
    to_sq = get_to_sq(mv)
    k_sq = self%king_sq(self%state%turn)

    chkrs = self%attackers_mask(k_sq, XCOLOR(self%state%turn))
    if (chkrs /= BB_EMPTY) then !! if presently in check ! only evasions pass
      res_tf = .not. self%is_move_an_evasion(mv, k_sq, chkrs)
      ! print *, "L882: is .not. evasion=", res_tf
      return
    end if

    !! if moving king, then not to an attacked square
    if (from_sq == k_sq) then
      occ = ior(self%occupied_co(WHITE), self%occupied_co(BLACK))
      occ = ibclr(occ, from_sq) ! permit xray **through** soon vacant from_sq
      res_tf = self%is_square_attacked(to_sq, XCOLOR(self%state%turn), occ)
      ! print *, "L891: moving king to attack sq=", res_tf
      return
    end if

    !! ep reveals check, must take precedence over pinned piece test below
    if (get_move_type(mv) == EP_MOVE) then
      ! call print_board_stamp(self)
      res_tf = self%is_ep_discovered_check(from_sq, to_sq, k_sq)
      ! print *, "L898: is_ep_discovered_check=", res_tf, "mv=", get_uci(mv)
      return
    end if

    pinned_mask = self%slider_blockers_(k_sq)
    if (btest(pinned_mask, from_sq)) then !! else if moving a pinned piece

      !! to reveal attack (not stay on pin ray), check!
      !res_tf = .not. self%stays_on_pin_ray(from_sq, to_sq, k_sq)
      res_tf = .not. btest(BB_LINE(k_sq, from_sq), to_sq)
      ! print *, "L908: is .not. remaining on pin line=", res_tf
      return
    end if
  end subroutine is_into_check

  !! @brief helper for is_into_check
  !! @details generate only evasions to minimize pseudo legal moves
  !! can validate speculative moves (killer moves, history moves)
  module &
#ifndef DEBUG
    pure &
#endif
    function is_move_an_evasion(self, mv, k_sq, chkrs) result(res_tf)

    use PieceM, only: PCK, XCOLOR, WHITE, BLACK
    use SquareM, only: SQK, V_S, V_N
    use MoveM, only: get_from_sq, get_to_sq, get_move_type, get_uci, EP_MOVE
    use MoveStackM, only: MVK
    use BitboardM, only: BBK, BB_LINE

    class(BoardT), intent(in) :: self
    integer(MVK), intent(in) :: mv
    integer(SQK), intent(in) :: k_sq
    integer(BBK), intent(in) :: chkrs
    logical :: res_tf

    integer(SQK) :: from_sq, to_sq, target_sq
    integer(BBK) :: pinned_mask, target_mask, occ
    integer(PCK) :: co, xco

    from_sq = get_from_sq(mv)
    to_sq = get_to_sq(mv)

    co = self%state%turn
    xco = XCOLOR(co)
    occ = ior(self%occupied_co(WHITE), self%occupied_co(BLACK))

    ! king on the move
    if (from_sq == k_sq) then

      ! king cannot move to sq attacked by the enemy
      res_tf = .not. self%is_square_attacked(to_sq, xco, ibclr(occ, k_sq))
      return
    end if

    ! double check: king must move out of double check
    if (popcnt(chkrs) > 1) then
      res_tf = .false.
      return
    end if

    if (get_move_type(mv) == EP_MOVE) then
      target_sq = to_sq + merge(V_S, V_N, co == WHITE)
    else
      target_sq = to_sq
    end if

    ! single check w/ no king move: capture checking piece or block ray
    ! valid to_sq lie between king and one chkrs
    target_mask = self%get_evasion_mask(k_sq, chkrs)
    if (.not. btest(target_mask, target_sq)) then
      ! if the destination is in the target mask, the move is an evasion
      res_tf = .false.
      return
    end if

    ! if moving piece pinned, then verify to_sq lands on the pin ray
    pinned_mask = self%slider_blockers_(k_sq)
    if (btest(pinned_mask, from_sq)) then
      ! t/f: stays on the pin ray
      res_tf = btest(BB_LINE(k_sq, from_sq), to_sq)
      return
    end if

    ! rare: ep evasion masking discovered check on rank/file
    if (get_move_type(mv) == EP_MOVE) then
      res_tf = .not. self%is_ep_discovered_check(from_sq, to_sq, k_sq)
      return
    end if

    res_tf = .true.
  end function is_move_an_evasion

  !! @brief helper for is-pseudo_legal
  module &
    pure &
    function is_valid_castling_move(self, mv) result(res_tf)

    use PieceM, only: PCK, WHITE, BLACK
    use SquareM, only: SQK, FILE_A, FILE_C, FILE_E, FILE_G, FILE_H, &
                       RANK_1_OFFSET_CO
    use BitboardM, only: BBK, BB_EMPTY, BB_BETWEEN
    use MoveM, only: get_to_sq
    use StateM, only: has_kingside_castling_rights, &
                      has_queenside_castling_rights

    class(BoardT), intent(in) :: self
    integer(MVK), intent(in) :: mv
    logical :: res_tf

    integer(PCK) :: co
    integer(SQK) :: to_sq, rank_offset
    integer(BBK) :: path_mask, occ

    res_tf = .false.
    to_sq = get_to_sq(mv)
    co = self%state%turn
    occ = iand(self%occupied_co(WHITE), self%occupied_co(BLACK))
    rank_offset = RANK_1_OFFSET_CO(co)

    ! verify that to_sq is a valid castling destination
    if (to_sq == rank_offset + FILE_G) then

      ! verify that the rights exist
      if (.not. has_kingside_castling_rights( &
          self%state%castling_rights, co)) return

      ! verify that the path between king and rook is empty
      path_mask = BB_BETWEEN(rank_offset + FILE_E, rank_offset + FILE_H)
      res_tf = (iand(occ, path_mask) == BB_EMPTY)

    else if (to_sq == rank_offset + FILE_C) then

      ! verify that the rights exist
      if (.not. has_queenside_castling_rights( &
          self%state%castling_rights, co)) return

      ! verify that the path between king and rook is empty
      path_mask = BB_BETWEEN(rank_offset + FILE_E, rank_offset + FILE_A)
      res_tf = (iand(occ, path_mask) == BB_EMPTY)
    end if
  end function is_valid_castling_move

  !! @brief helper for is_pseudo_legal
  module &
    pure &
    function is_valid_pawn_move(self, from_sq, to_sq) result(res_tf)

    use PieceM, only: PCK, BLACK, WHITE, XCOLOR
    use SquareM, only: SQK, V_N, V_S, IS_RANK_2_CO
    use BitboardM, only: BBK, BB_EMPTY, BB_SQUARES

    class(BoardT), intent(in) :: self
    integer(SQK), intent(in) :: from_sq, to_sq
    logical :: res_tf

    integer(BBK) :: to_bb, empty_squares, enemy_squares
    integer(SQK) :: v
    integer(PCK) :: co, xco

    co = self%state%turn
    xco = XCOLOR(co)

    res_tf = .false.
    to_bb = BB_SQUARES(to_sq)
    empty_squares = not(iand(self%occupied_co(WHITE), self%occupied_co(BLACK)))
    enemy_squares = self%occupied_co(xco)

    v = merge(V_N, V_S, co == WHITE)
    if (to_sq == from_sq + v) then

      ! single push: destination must be empty
      res_tf = (iand(to_bb, empty_squares) /= BB_EMPTY)
      return
    else if (to_sq == from_sq + v*2_SQK) then

      ! double push: must be on the starting rank and both squares empty
      if (IS_RANK_2_CO(from_sq, co)) then
        res_tf = (iand(to_bb, empty_squares) /= BB_EMPTY) .and. &
                 (iand(BB_SQUARES(from_sq + v), empty_squares) /= BB_EMPTY)
      end if
      return
    end if

    ! explore whether to_sq is a diagonal attack from from_sq
    if (iand(self%attacks_mask(from_sq), to_bb) /= BB_EMPTY) then
      ! standard capture
      if (iand(to_bb, enemy_squares) /= BB_EMPTY) then
        res_tf = .true.
        ! en passant destination is ep_square
      else if (to_sq == self%state%ep_sq) then
        res_tf = .true.
      end if
    end if
  end function is_valid_pawn_move

  !! @brief whether ep caused discovered check along the 4th rank
  module &
    pure &
    function is_ep_discovered_check(self, from_sq, to_sq, k_sq) result(res_tf)

    use PieceM, only: PCK, WHITE, BLACK, XCOLOR
    use SquareM, only: SQK, RANK_OFFSET, SQUARE_FILE, &
                       V_N, V_E, V_S, V_W, V_NE, V_SE, V_SW, V_NW
    use BitboardM, only: BBK, BB_EMPTY, get_ray_attacks, &
                         BB_BISHOP_RAYS, BB_ROOK_RAYS

    class(BoardT), intent(in) :: self
    integer(SQK), intent(in) :: from_sq, to_sq, k_sq
    logical :: res_tf

    integer :: cap_sq
    integer(PCK) :: xco
    integer(BBK) :: occ_after, rays, bishop_snipers, rook_snipers

    xco = XCOLOR(self%state%turn)

    ! post-ep occupancy
    cap_sq = RANK_OFFSET(from_sq) + SQUARE_FILE(to_sq)
    occ_after = ior(self%occupied_co(WHITE), self%occupied_co(BLACK))
    occ_after = ibclr(occ_after, from_sq)
    occ_after = ibclr(occ_after, cap_sq)
    occ_after = ibset(occ_after, to_sq)
    ! print *, "L922: post-ep occ:"
    ! call print_stamp(occ_after)

    ! no checks yet
    res_tf = .false.

    ! diag discovered checks (bishops & queens)
    bishop_snipers = ior(self%bishops_co(xco), self%queens_co(xco))
    if (iand(BB_BISHOP_RAYS(k_sq), bishop_snipers) /= BB_EMPTY) then
      rays = ior(ior(get_ray_attacks(k_sq, V_NE, occ_after), &
                     get_ray_attacks(k_sq, V_NW, occ_after)), &
                 ior(get_ray_attacks(k_sq, V_SE, occ_after), &
                     get_ray_attacks(k_sq, V_SW, occ_after)))
      ! print *, "L935: diag striking rays:"
      ! call print_stamp(rays)

      if (iand(rays, bishop_snipers) /= BB_EMPTY) then
        res_tf = .true.
        return
      end if
    end if

    ! ortho discovered checks (rooks & queens)
    rook_snipers = ior(self%rooks_co(xco), self%queens_co(xco))
    if (iand(BB_ROOK_RAYS(k_sq), rook_snipers) /= BB_EMPTY) then
      rays = ior(ior(get_ray_attacks(k_sq, V_N, occ_after), &
                     get_ray_attacks(k_sq, V_S, occ_after)), &
                 ior(get_ray_attacks(k_sq, V_E, occ_after), &
                     get_ray_attacks(k_sq, V_W, occ_after)))
      ! print *, "L951: ortho striking rays:"
      ! call print_stamp(rays)

      if (iand(rays, rook_snipers) /= BB_EMPTY) then
        res_tf = .true.
        return
      end if
    end if

  end function is_ep_discovered_check

  !! @brief helper for is_into_check
  module &
    pure &
    function stays_on_pin_ray(self, from_sq, to_sq, k_sq) result(res_tf)

    use SquareM, only: SQK
    use BitboardM, only: BB_LINE

    class(BoardT), intent(in) :: self
    integer(SQK), intent(in) :: from_sq, to_sq, k_sq
    logical :: res_tf

    integer(BBK) :: pin_ray

    ! identify the connectnig ray
    pin_ray = BB_LINE(k_sq, from_sq)

    ! valid moves only to these destinations
    res_tf = btest(pin_ray, to_sq)
  end function stays_on_pin_ray

  !!@brief get the squares of pieces giving check
  module &
#ifndef DEBUG
    pure &
#endif
    function checkers_mask(self) result(mask)

    use SquareM, only: SQK
    use PieceM, only: XCOLOR
    use BitboardM, only: BBK

    class(BoardT), intent(in) :: self
    integer(BBK) :: mask

    integer(SQK) :: k_sq

    k_sq = self%king_sq(self%state%turn)
    mask = self%attackers_mask(k_sq, XCOLOR(self%state%turn))

  end function checkers_mask

  !! @brief helper for is_move_an_evasion
#ifndef DEBUG
  pure &
#endif
    module &
    function get_evasion_mask(self, k_sq, chkrs) result(mask)

    use PieceM, only: PCK, PIECE_TYPE, BISHOP, ROOK, QUEEN
    use SquareM, only: SQK
    use BitboardM, only: lsb, BBK, BB_EMPTY, BB_SQUARES, BB_BETWEEN

    class(BoardT), intent(in) :: self
    integer(SQK), intent(in) :: k_sq
    integer(BBK), intent(in) :: chkrs
    integer(BBK) :: mask

    integer(PCK) :: ty
    integer(SQK) :: chckr_sq
    integer(BBK) :: chckr_bit

    ! default to empty (used for double-check logic)
    mask = BB_EMPTY

    ! if more than one chckr, no piece can block or capture both at once
    if (popcnt(chkrs) > 1) return

    ! single check logic
    chckr_sq = lsb(chkrs)
    chckr_bit = BB_SQUARES(chckr_sq)

    ! the chckr's square is always a target (capture it!)
    mask = chckr_bit

    ! if the chckr is a slider (RBQ) add the blocking ray
    ! we check the piece type at the chckr_sq
    ty = PIECE_TYPE(self%piece_at(chckr_sq))
    if (ty == ROOK .or. ty == BISHOP .or. ty == QUEEN) then

      ! use precalculated BB_LINE, BB_BETWEEN
      mask = ior(mask, BB_BETWEEN(k_sq, chckr_sq))
    end if
  end function get_evasion_mask

  !! @brief potential pinners are aligned on rank, file, diagonal with aim
  module &
    pure &
    function get_potential_pinners(self, k_sq) result(bb)

    use PieceM, only: PCK, XCOLOR
    use SquareM, only: SQK
    use BitboardM, only: BBK, BB_ROOK_RAYS, BB_BISHOP_RAYS

    class(BoardT), intent(in) :: self
    integer(SQK), intent(in) :: k_sq
    integer(BBK) :: bb

    integer(BBK) :: rook_team, bishop_team
    integer(PCK) :: xco

    xco = XCOLOR(self%state%turn)

    ! (R, Q)
    rook_team = iand(BB_ROOK_RAYS(k_sq), &
                     ior(self%rooks_co(xco), self%queens_co(xco)))

    ! (B, Q)
    bishop_team = iand(BB_BISHOP_RAYS(k_sq), &
                       ior(self%bishops_co(xco), self%queens_co(xco)))

    bb = ior(rook_team, bishop_team)
  end function get_potential_pinners

  !! @brief
  module &
#ifndef DEBUG
    pure &
#endif
    function slider_blockers_(self, k_sq) result(pinned)

    use PieceM, only: BLACK, WHITE
    use SquareM, only: SQK
    use BitboardM, only: BBK, BB_EMPTY, BB_BETWEEN, pop_lsb

    class(BoardT), intent(in) :: self
    integer(SQK), intent(in) :: k_sq
    integer(BBK) :: pinned

    integer(BBK) :: blockers, occ, friendlies, snipers, pinner_ray
    integer(SQK) :: p_sq

    pinned = BB_EMPTY
    occ = ior(self%occupied_co(WHITE), self%occupied_co(BLACK))
    friendlies = self%occupied_co(self%state%turn)

    ! enemy sliders (Q, R, and B) aligned along rays with k_sq
    snipers = self%get_potential_pinners(k_sq)

    do while (snipers /= BB_EMPTY)
      call pop_lsb(snipers, p_sq) ! enemy sniper square

      pinner_ray = BB_BETWEEN(k_sq, p_sq)
      blockers = iand(pinner_ray, occ)

      ! if exactly one blocker between king and sniper, and only if friendly
      if (popcnt(blockers) == 1) then
        pinned = ior(pinned, iand(blockers, friendlies))
      end if
    end do

  end function slider_blockers_

end submodule AttacksM

