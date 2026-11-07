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

! @brief Move Generation & Move Execution
! @detail Generate pseudo-legal moves, parse UCI moves strings, mutate board
! state when making/unmaking moves
submodule(BoardM) GeneratorM

  use, intrinsic :: iso_fortran_env, only: int16
  implicit none

contains

#ifdef DEBUG
  subroutine assert_evaluation(self, where)

    use BaseM, only: full_score
    use iso_fortran_env, only: error_unit

    class(BoardT), intent(in) :: self
    character(len=*), intent(in) :: where
    integer(EVK) :: score, phase, score_mg

    call full_score(self, score, phase, score_mg)

    if (self%score /= score .or. &
        self%phase /= phase .or. &
        self%score_mg /= score_mg) then
      write (error_unit, '(a)') trim(where)
      write (error_unit, *) "score:    cached/full:", self%score, score
      write (error_unit, *) "score_mg: cached/full:", self%score_mg, score_mg
      write (error_unit, *) "phase:    cached/full:", self%phase, phase
      error stop "cached evaluation disagrees with full_score"
    end if
  end subroutine assert_evaluation
#endif

  !! @brief pseudo legal moves may leave the king in check
  module &
    subroutine generate_pseudo_legal_moves(self, from_mask_opt, to_mask_opt)

    use PieceM, only: &
      PCK, XCOLOR, EMPTY, WHITE, BLACK, PAWN, KNIGHT, BISHOP, ROOK, QUEEN
    use SquareM, only: SQK, EP_NONE, V_N, V_S, IS_RANK_8_CO
    use BitboardM, only: &
      pop_msb, BBK, BB_ALL, BB_EMPTY, BB_PAWN_ATTACKS_CO, &
      BB_RANK_4, BB_RANK_5
    use MoveStackM, only: MVK, push_move
    use MoveM, only: pack_move, NORMAL_MOVE, DOUBLE_PUSH, PROMO_MOVE, get_uci

    class(BoardT), intent(inout) :: self
    integer(BBK), intent(in), optional :: from_mask_opt, to_mask_opt
    integer :: nmoves

    integer(BBK) :: from_mask, pawns, non_pawns, to_mask, occ, capturers, &
          moves, single_moves, double_moves, our_squares, enemy_squares, targets
    integer(PCK) :: co, xco, piece, captured_pc
    integer(SQK) :: from_sq, to_sq
    integer(MVK) :: mv

    nmoves = 0
    co = self%state%turn
    xco = XCOLOR(co)

    from_mask = BB_ALL
    if (present(from_mask_opt)) from_mask = from_mask_opt
    to_mask = BB_ALL
    if (present(to_mask_opt)) to_mask = to_mask_opt

    our_squares = self%occupied_co(co)
    enemy_squares = self%occupied_co(xco)

    !! generate piece moves (non_pawns)
    non_pawns = iand(iand(our_squares, not(self%pawns_co(co))), from_mask)

    do while (non_pawns /= BB_EMPTY)
      call pop_msb(non_pawns, from_sq)
      piece = self%piece_at(from_sq)
      moves = iand(iand(self%attacks_mask(from_sq), not(our_squares)), to_mask)
      do while (moves /= BB_EMPTY)
        call pop_msb(moves, to_sq)
        if (btest(enemy_squares, to_sq)) then
          captured_pc = self%piece_at(to_sq)
        else
          captured_pc = EMPTY
        end if
        mv = pack_move(priority=0_int16, &
                       from_sq=from_sq, &
                       to_sq=to_sq, &
                       piece=piece, &
                       captured=captured_pc, &
                       promotion=EMPTY, &
                       move_type=NORMAL_MOVE)
        call push_move(self%stack, mv)
        nmoves = nmoves + 1
      end do
    end do

    !! generate castling moves
    if (iand(from_mask, self%kings_co(co)) /= BB_EMPTY) &
      nmoves = nmoves + self%generate_legal_castling_moves(from_mask, to_mask)

    !! the remaining moves are all pawn moves
    pawns = self%pawns_co(co)
    if (pawns == BB_EMPTY) return

    !! generate pawn captures
    capturers = pawns
    do while (capturers /= BB_EMPTY)
      call pop_msb(capturers, from_sq)
      targets = iand(BB_PAWN_ATTACKS_CO(co, from_sq), &
                     iand(self%occupied_co(xco), to_mask))

      do while (targets /= BB_EMPTY)
        call pop_msb(targets, to_sq)
        captured_pc = self%piece_at(to_sq)
        if (IS_RANK_8_CO(to_sq, co)) then
          mv = pack_move(priority=0_int16, &
                         from_sq=from_sq, &
                         to_sq=to_sq, &
                         piece=PAWN + co, &
                         captured=captured_pc, &
                         promotion=QUEEN + co, &
                         move_type=PROMO_MOVE)
          call push_move(self%stack, mv)
          mv = pack_move(priority=0_int16, &
                         from_sq=from_sq, &
                         to_sq=to_sq, &
                         piece=PAWN + co, &
                         captured=captured_pc, &
                         promotion=ROOK + co, &
                         move_type=PROMO_MOVE)
          call push_move(self%stack, mv)
          mv = pack_move(priority=0_int16, &
                         from_sq=from_sq, &
                         to_sq=to_sq, &
                         piece=PAWN + co, &
                         captured=captured_pc, &
                         promotion=BISHOP + co, &
                         move_type=PROMO_MOVE)
          call push_move(self%stack, mv)
          mv = pack_move(priority=0_int16, &
                         from_sq=from_sq, &
                         to_sq=to_sq, &
                         piece=PAWN + co, &
                         captured=captured_pc, &
                         promotion=KNIGHT + co, &
                         move_type=PROMO_MOVE)
          call push_move(self%stack, mv)
          nmoves = nmoves + 4
        else
          mv = pack_move(priority=0_int16, &
                         from_sq=from_sq, &
                         to_sq=to_sq, &
                         piece=PAWN + co, &
                         captured=captured_pc, &
                         promotion=EMPTY, &
                         move_type=NORMAL_MOVE)
          call push_move(self%stack, mv)
          nmoves = nmoves + 1
        end if
      end do
    end do

    !! prepare pawn advance generation
    occ = ior(self%occupied_co(WHITE), self%occupied_co(BLACK))
    if (co == WHITE) then
      !! move all pawns north, but only if target empty
      single_moves = iand(ishft(pawns, V_N), not(occ))
      !! moves valid single moves north again, only if empty target rank 4
      double_moves = iand(iand(ishft(single_moves, V_N), not(occ)), BB_RANK_4)
    else
      !! move all pawns south, but only if target empty
      single_moves = iand(ishft(pawns, V_S), not(occ))
      !! moves valid single moves south again, only if empty target rank 5
      double_moves = iand(iand(ishft(single_moves, V_S), not(occ)), BB_RANK_5)
    end if

    !! generate single pawn moves
    do while (single_moves /= BB_EMPTY)
      call pop_msb(single_moves, to_sq)
      from_sq = to_sq + merge(V_S, V_N, co == WHITE)

      if (IS_RANK_8_CO(to_sq, co)) then
        mv = pack_move(priority=0_int16, &
                       from_sq=from_sq, &
                       to_sq=to_sq, &
                       piece=PAWN + co, &
                       captured=EMPTY, &
                       promotion=QUEEN + co, &
                       move_type=PROMO_MOVE)
        call push_move(self%stack, mv)
        mv = pack_move(priority=0_int16, &
                       from_sq=from_sq, &
                       to_sq=to_sq, &
                       piece=PAWN + co, &
                       captured=EMPTY, &
                       promotion=ROOK + co, &
                       move_type=PROMO_MOVE)
        call push_move(self%stack, mv)
        mv = pack_move(priority=0_int16, &
                       from_sq=from_sq, &
                       to_sq=to_sq, &
                       piece=PAWN + co, &
                       captured=EMPTY, &
                       promotion=BISHOP + co, &
                       move_type=PROMO_MOVE)
        call push_move(self%stack, mv)
        mv = pack_move(priority=0_int16, &
                       from_sq=from_sq, &
                       to_sq=to_sq, &
                       piece=PAWN + co, &
                       captured=EMPTY, &
                       promotion=KNIGHT + co, &
                       move_type=PROMO_MOVE)
        call push_move(self%stack, mv)
        nmoves = nmoves + 4
      else
        mv = pack_move(priority=0_int16, &
                       from_sq=from_sq, &
                       to_sq=to_sq, &
                       piece=PAWN + co, &
                       captured=EMPTY, &
                       promotion=EMPTY, &
                       move_type=NORMAL_MOVE)
        call push_move(self%stack, mv)
        nmoves = nmoves + 1
      end if
    end do

    !! generate double pawn moves
    do while (double_moves /= BB_EMPTY)
      call pop_msb(double_moves, to_sq)
      from_sq = to_sq - 2_SQK*merge(V_N, V_S, co == WHITE)
      mv = pack_move(priority=0_int16, &
                     from_sq=from_sq, &
                     to_sq=to_sq, &
                     piece=PAWN + co, &
                     captured=EMPTY, &
                     promotion=EMPTY, &
                     move_type=DOUBLE_PUSH)
      call push_move(self%stack, mv)
      nmoves = nmoves + 1
    end do

    !! generate en passant captures
    ! insert ep_sq into to_mask, if capturable pawn sq found in to_mask
    if (self%state%ep_sq /= EP_NONE) then
      if (btest(to_mask, self%state%ep_sq + &
                merge(V_S, V_N, co == WHITE))) then
        to_mask = ibset(to_mask, self%state%ep_sq)
      end if

      call self%generate_pseudo_legal_ep(from_mask, to_mask)
      nmoves = nmoves + 1
    end if
  end subroutine generate_pseudo_legal_moves

  !! @brief generate pseudo legal captures
  module &
    subroutine generate_pseudo_legal_captures(self, from_mask_opt, to_mask_opt)

    use PieceM, only: PCK, XCOLOR
    use BitboardM, only: BB_ALL

    class(BoardT), intent(inout) :: self
    integer(BBK), intent(in), optional :: from_mask_opt, to_mask_opt

    integer(PCK) :: co, xco
    integer(BBK) :: from_mask, to_mask

    co = self%state%turn
    xco = XCOLOR(co)

    from_mask = BB_ALL; if (present(from_mask_opt)) from_mask = from_mask_opt
    to_mask = BB_ALL; if (present(to_mask_opt)) to_mask = to_mask_opt

    call self%generate_pseudo_legal_moves( &
      from_mask, iand(to_mask, self%occupied_co(xco)))

  end subroutine generate_pseudo_legal_captures

  !! @brief generate ep moves without testing post move king safety
  module &
#ifndef DEBUG
    pure &
#endif
    subroutine generate_pseudo_legal_ep(self, from_mask_opt, to_mask_opt)

    use BitboardM, only: &
      pop_msb, BB_RANK_4, BB_RANK_5, BB_ALL, BB_EMPTY, BB_PAWN_ATTACKS_CO
    use MoveM, only: EP_MOVE, pack_move, get_uci
    use MoveStackM, only: push_move
    use PieceM, only: PCK, EMPTY, XCOLOR, PAWN, WHITE, BLACK
    use SquareM, only: SQK, EP_NONE

    class(BoardT), intent(inout) :: self
    integer(BBK), intent(in), optional :: from_mask_opt, to_mask_opt
    integer :: nmoves

    integer(PCK) :: co, xco
    integer(SQK) :: from_sq
    integer(BBK) :: attackers, from_mask, to_mask
    integer(MVK) :: mv

    nmoves = 0
    co = self%state%turn
    xco = XCOLOR(co)

    from_mask = BB_ALL; if (present(from_mask_opt)) from_mask = from_mask_opt
    to_mask = BB_ALL; if (present(to_mask_opt)) to_mask = to_mask_opt

    ! ensure ep_sq is not EP_NONE and its bit is set in to_mask
    if (self%state%ep_sq == EP_NONE .or. &
        .not. btest(to_mask, self%state%ep_sq)) return

#ifdef DEBUG
    ! verify ep target square is not occupied
    if (btest(self%occupied_co(WHITE), self%state%ep_sq) .or. &
        btest(self%occupied_co(BLACK), self%state%ep_sq)) &
      error stop "ep_sq occupied"
#endif

    ! pawns of color co, attack (rev xco from) ep_sq, from w 5th b 4th
    attackers = iand(iand(iand(self%pawns_co(co), from_mask), &
                          BB_PAWN_ATTACKS_CO(xco, self%state%ep_sq)), &
                     merge(BB_RANK_5, BB_RANK_4, co == WHITE))

    ! scan and push
    do while (attackers /= BB_EMPTY)
      call pop_msb(attackers, from_sq)

      ! pack move with en passant flag
      mv = pack_move(priority=0_int16, &
                     from_sq=from_sq, &
                     to_sq=self%state%ep_sq, &
                     piece=PAWN + co, &
                     captured=PAWN + xco, &
                     promotion=EMPTY, &
                     move_type=EP_MOVE)
      call push_move(self%stack, mv)
      nmoves = nmoves + 1
    end do
  end subroutine generate_pseudo_legal_ep

  !! @brief generate and push moves to the stack
  module &
    subroutine generate_legal_moves(self, nmoves)

    use BitboardM, only: BBK, BB_EMPTY
    use SquareM, only: SQK
    use MoveStackM, only: MVK, reset_movestack, prepare_ply
    use MoveM, only: get_uci

    class(BoardT), intent(inout) :: self
    integer, intent(out) :: nmoves

    integer(BBK) :: chkrs
    integer(SQK) :: k_sq
    integer(MVK) :: mv
    integer :: i, k, p
    logical :: res_tf

    call prepare_ply(self%stack)

    chkrs = self%checkers_mask()

    if (chkrs /= BB_EMPTY) then
      ! these evasions are pseudo-legal, so apply is_legal
      k_sq = self%king_sq(self%state%turn)
      call self%generate_evasions(k_sq, chkrs)  ! quick
    else
      ! n_pseudo moves in move_list, index ranging from ix1(p) to ixn(p)
      call self%generate_pseudo_legal_moves()
    end if

    ! perform in-place legality filtering and compaction
    p = self%stack%current_ply
    k = self%stack%ix1(p) - 1 ! initialize (one fewer than ix1 implies empty)

    do i = self%stack%ix1(p), self%stack%ixn(p)
      mv = self%stack%move_list(i)

      ! ensure this legal move is positioned at k + 1 (one beyond cursor)
      res_tf = self%is_legal(mv)
      if (res_tf) then
        k = k + 1 ! advance cursor for every legal move
        if (k < i) self%stack%move_list(k) = mv ! copy if needed (a gap)
      end if
    end do

    ! terminate stack at k
    self%stack%ixn(p) = k
    self%stack%free = k + 1
    nmoves = max(0, k - self%stack%ix1(p) + 1)
  end subroutine generate_legal_moves

  !! @brief generate castling moves into the moves_list
  function generate_legal_castling_moves(self, from_mask_opt, to_mask_opt) &
    result(nmoves)

    use PieceM, only: PCK, BLACK, WHITE, EMPTY, KING, XCOLOR
    use SquareM, only: SQK, RANK_OFFSET, &
                       FILE_B, FILE_C, FILE_D, FILE_E, FILE_F, FILE_G
    use BitboardM, only: BBK, BB_ALL
    use EvaluationM, only: EVK
    use BaseM, only: BaseT, is_square_attacked, king_sq, piece_at
    use StateM, only: has_queenside_castling_rights, &
                      has_kingside_castling_rights
    use MoveM, only: CASTLE_MOVE, pack_move, get_uci
    use MoveStackM, only: MVK, push_move

    class(BoardT), intent(inout) :: self
    integer(BBK), intent(in), optional :: from_mask_opt, to_mask_opt
    integer :: nmoves

    integer(kind=PCK) :: co, xco
    integer(kind=SQK) :: k_rank
    integer(kind=BBK) :: occ, from_mask, to_mask
    integer(MVK) :: mv

    nmoves = 0
    co = self%state%turn
    xco = XCOLOR(co)
    occ = ior(self%occupied_co(WHITE), self%occupied_co(BLACK))

    from_mask = BB_ALL; if (present(from_mask_opt)) from_mask = from_mask_opt
    to_mask = BB_ALL; if (present(to_mask_opt)) to_mask = to_mask_opt

    !! castling when in check? not permitted, so we're done!
    if (self%is_square_attacked(self%king_sq(co), xco, occ)) return

    !! rank of interest
    k_rank = RANK_OFFSET(self%king_sq(co))

    if (.not. btest(from_mask, k_rank + FILE_E)) return

    !! @brief Q-side: O-O-O
    if (btest(to_mask, k_rank + FILE_C) .and. &
        has_queenside_castling_rights(self%state%castling_rights, co) .and. &
        self%piece_at(k_rank + FILE_B) == EMPTY .and. &
        self%piece_at(k_rank + FILE_C) == EMPTY .and. &
        self%piece_at(k_rank + FILE_D) == EMPTY .and. &
        .not. self%is_square_attacked(k_rank + FILE_C, xco, occ) .and. &
        .not. self%is_square_attacked(k_rank + FILE_D, xco, occ)) then
      mv = pack_move(priority=0_EVK, &
                     from_sq=k_rank + FILE_E, &
                     to_sq=k_rank + FILE_C, &
                     piece=KING + co, &
                     captured=EMPTY, &
                     promotion=EMPTY, &
                     move_type=CASTLE_MOVE)
      call push_move(self%stack, mv)
      nmoves = nmoves + 1
    end if

    !! @brief K-side: O-O
    if (btest(to_mask, k_rank + FILE_G) .and. &
        has_kingside_castling_rights(self%state%castling_rights, co) .and. &
        self%piece_at(k_rank + FILE_G) == EMPTY .and. &
        self%piece_at(k_rank + FILE_F) == EMPTY .and. &
        .not. self%is_square_attacked(k_rank + FILE_G, xco, occ) .and. &
        .not. self%is_square_attacked(k_rank + FILE_F, xco, occ)) then
      mv = pack_move(priority=0_EVK, &
                     from_sq=k_rank + FILE_E, &
                     to_sq=k_rank + FILE_G, &
                     piece=KING + co, &
                     captured=EMPTY, &
                     promotion=EMPTY, &
                     move_type=CASTLE_MOVE)
      call push_move(self%stack, mv)
      nmoves = nmoves + 1
    end if
  end function generate_legal_castling_moves

  !! @brief gen ep captures on given ep_square
  function generate_legal_ep(self) result(nmoves)

    use BaseM, only: BaseT, is_square_attacked, king_sq
    use BitboardM, only: pop_msb, BBK, BB_EMPTY, pop_msb
    use EvaluationM, only: EVK
    use MoveM, only: EP_MOVE, pack_move, get_piece, get_uci
    use MoveStackM, only: MVK, push_move
    use PieceM, only: PCK, PC_NONE, BLACK, WHITE, XCOLOR, PAWN
    use SquareM, only: SQK, EP_NONE, V_N, V_S, RANK_6_OFFSET, RANK_OFFSET

    class(BoardT), intent(inout) :: self
    integer :: nmoves

    integer(SQK) :: victim_sq, fr_sq, k_sq
    integer(BBK) :: candidate_pawns, ghost_occ
    integer(PCK) :: co, xco
    integer(MVK) :: mv

    nmoves = 0

    !! perhaps no ep target exists? done!
    if (self%state%ep_sq == EP_NONE) return

    !! ep is at rank 6 for white to move, and at rank 3 for black to move
    co = merge(WHITE, BLACK, RANK_OFFSET(self%state%ep_sq) == RANK_6_OFFSET)
    xco = XCOLOR(co)

    !! victim is at rank 5 for white to move, and rank 4 for black to move
    victim_sq = merge(self%state%ep_sq + V_S, &
                      self%state%ep_sq + V_N, &
                      co == WHITE)
    k_sq = self%king_sq(co)

    !! identify all (0, 1, or 2) friendly pawns capable of capturing ep
    !! if white captures on e6, the pawns must sit at d5 or f5
    !! find those candidate squares by opponent (xco) attacking from ep_sq
    candidate_pawns = iand(self%pawns_co(co), self%get_pawn_attacks(xco))

    do while (candidate_pawns /= BB_EMPTY)
      call pop_msb(candidate_pawns, fr_sq)

      !! create the move
      mv = pack_move(priority=0_EVK, &
                     from_sq=fr_sq, &
                     to_sq=self%state%ep_sq, &
                     piece=PAWN + co, &
                     captured=PAWN + xco, &
                     promotion=PC_NONE, &
                     move_type=EP_MOVE)

      !! legality test TODO potentially this test may be simplified
      call move_pieces(self, mv)
      ghost_occ = &
        ior(ibset(BB_EMPTY, self%state%ep_sq), &
            ieor(ibset(BB_EMPTY, victim_sq), &
                 ieor(ibset(BB_EMPTY, fr_sq), &
                      ior(self%occupied_co(WHITE), &
                          self%occupied_co(BLACK)))))
      if (.not. self%is_square_attacked(k_sq, xco, ghost_occ)) then
        call push_move(self%stack, mv)
        nmoves = nmoves + 1
      end if
      call unmove_pieces(self, mv)
    end do
  end function generate_legal_ep

  !! @brief move the king, capture lone chckr, or block the checking ray
  module &
    subroutine generate_evasions(self, k_sq, chkrs, from_mask_opt, to_mask_opt)

    use SquareM, only: SQK
    use PieceM, only: PCK, EMPTY, WHITE, BLACK, XCOLOR, KING
    use BitboardM, only: lsb, msb, pop_msb, BB_ALL, BB_EMPTY, BB_BETWEEN, &
                         BB_LINE, BB_SQUARES, BB_KING_ATTACKS
    use BaseM, only: BaseT
    use MoveM, only: NORMAL_MOVE, pack_move, get_uci
    use MoveStackM, only: push_move

    class(BoardT), intent(inout) :: self
    integer(SQK), intent(in) :: k_sq
    integer(BBK), intent(in) :: chkrs
    integer(BBK), intent(in), optional :: from_mask_opt, to_mask_opt

    integer(BBK) :: from_mask, to_mask, sliders, attacked, escape, &
                    mv_from, occ, target_mask
    integer(SQK) :: chckr_sq, to_sq
    integer(PCK) :: co, xco, cap
    integer(MVK) :: mv

    from_mask = BB_ALL
    if (present(from_mask_opt)) from_mask = from_mask_opt
    to_mask = BB_ALL
    if (present(to_mask_opt)) to_mask = to_mask_opt

    co = merge(WHITE, BLACK, btest(self%occupied_co(WHITE), k_sq))
    xco = XCOLOR(co)
    occ = ior(self%occupied_co(WHITE), self%occupied_co(BLACK))

    !! identify BRQ attacking rays (the King cannot move along them)
    sliders = iand(chkrs, &
                   ior(self%bishops_co(xco), &
                       ior(self%rooks_co(xco), &
                           self%queens_co(xco))))
    attacked = BB_EMPTY
    mv_from = sliders

    do while (mv_from /= BB_EMPTY)
      call pop_msb(mv_from, chckr_sq) ! msb
      attacked = ior(attacked, &
                     iand(BB_LINE(k_sq, chckr_sq), &
                          not(BB_SQUARES(chckr_sq))))
    end do

    !! king escapes to unattacked squares
    if (iand(BB_SQUARES(k_sq), from_mask) /= BB_EMPTY) then
      escape = iand(BB_KING_ATTACKS(k_sq), &
                    iand(not(self%occupied_co(self%state%turn)), &
                         iand(not(attacked), to_mask)))

      do while (escape /= BB_EMPTY)
        call pop_msb(escape, to_sq)
        cap = self%piece_at(to_sq)
        mv = pack_move(priority=0_int16, &
                       from_sq=k_sq, &
                       to_sq=to_sq, &
                       piece=KING + co, &
                       captured=cap, &
                       promotion=EMPTY, &
                       move_type=NORMAL_MOVE)
        call push_move(self%stack, mv)
      end do
    end if

    !! single check scenarios (double checks allow ONLY King moves)
    if (popcnt(chkrs) == 1) then
      chckr_sq = lsb(chkrs)

      !! capturing the chckr or blocking the 'between' squares
      target_mask = ior(BB_BETWEEN(k_sq, chckr_sq), chkrs)

      !! generate captures/blocks for all except the King
      call self%generate_pseudo_legal_moves( &
        iand(not(BB_SQUARES(self%king_sq(co))), from_mask), &
        iand(target_mask, to_mask))
    end if
  end subroutine generate_evasions

  !! @brief generate (non-ep) captures by pawn
  function generate_captures_by_pawn(self) result(nmoves)

    use PieceM, only: PCK, XCOLOR, EMPTY, PAWN, KNIGHT, BISHOP, ROOK, QUEEN
    use SquareM, only: SQK, IS_RANK_8_CO
    use BitboardM, only: msb, pop_msb, BBK, BB_EMPTY, BB_PAWN_ATTACKS_CO
    use EvaluationM, only: &
      EVK, BASE_CAPTURE_VALUE, BASE_PROMOTION_VALUE, PIECE_VALUE
    use BaseM, only: BaseT, piece_at
    use MoveStackM, only: MVK, push_move
    use MoveM, only: NORMAL_MOVE, PROMO_MOVE, pack_move, get_uci

    class(BoardT), intent(inout) :: self
    integer :: nmoves

    integer(BBK) :: targets, atkrs_mask
    integer(SQK) :: to_sq, fr_sq
    integer(PCK) :: victim, co, xco
    integer(EVK) :: base_priority
    integer(MVK) :: mv

    nmoves = 0
    co = self%state%turn
    xco = XCOLOR(co)

    !! get all co pawn attacks onto xco
    targets = iand(self%get_pawn_attacks(co), self%occupied_co(xco))
    do while (targets /= BB_EMPTY)
      call pop_msb(targets, to_sq)
      victim = self%piece_at(to_sq)

      !! find which of the pawns can hit this specific square
      atkrs_mask = iand(BB_PAWN_ATTACKS_CO(to_sq, XCOLOR(co)), &
                        self%pawns_co(co))
      do while (atkrs_mask /= BB_EMPTY)
        call pop_msb(atkrs_mask, fr_sq)

        if (IS_RANK_8_CO(to_sq, co)) then
          base_priority = BASE_PROMOTION_VALUE + PIECE_VALUE(victim)*5_EVK

          mv = pack_move(priority=base_priority + PIECE_VALUE(QUEEN), &
                         from_sq=fr_sq, &
                         to_sq=to_sq, &
                         piece=PAWN, &
                         captured=victim, &
                         promotion=QUEEN + co, &
                         move_type=PROMO_MOVE)
          call push_move(self%stack, mv)

          mv = pack_move(priority=base_priority + PIECE_VALUE(ROOK), &
                         from_sq=fr_sq, &
                         to_sq=to_sq, &
                         piece=PAWN, &
                         captured=victim, &
                         promotion=ROOK + co, &
                         move_type=PROMO_MOVE)
          call push_move(self%stack, mv)

          mv = pack_move(priority=base_priority + PIECE_VALUE(BISHOP), &
                         from_sq=fr_sq, &
                         to_sq=to_sq, &
                         piece=PAWN, &
                         captured=victim, &
                         promotion=BISHOP + co, &
                         move_type=PROMO_MOVE)
          call push_move(self%stack, mv)

          mv = pack_move(priority=base_priority + PIECE_VALUE(KNIGHT), &
                         from_sq=fr_sq, &
                         to_sq=to_sq, &
                         piece=PAWN, &
                         captured=victim, &
                         promotion=KNIGHT + co, &
                         move_type=PROMO_MOVE)
          call push_move(self%stack, mv)
          nmoves = nmoves + 4
        else
          base_priority = &
            BASE_CAPTURE_VALUE + PIECE_VALUE(victim)*5_EVK - PIECE_VALUE(PAWN)
          mv = pack_move(priority=base_priority, &
                         from_sq=fr_sq, &
                         to_sq=to_sq, &
                         piece=PAWN, &
                         captured=victim, &
                         promotion=EMPTY, &
                         move_type=NORMAL_MOVE)
          call push_move(self%stack, mv)
          nmoves = nmoves + 1
        end if
      end do
    end do
  end function generate_captures_by_pawn

  !! @brief process piece captures
  module &
#ifndef DEBUG
    pure &
#endif
    subroutine generate_piece_captures(self, ty)

    use PieceM, only: &
      PCK, XCOLOR, EMPTY, WHITE, BLACK, KNIGHT, BISHOP, ROOK, QUEEN, KING
    use SquareM, only: SQK, V_N, V_E, V_S, V_W, V_NE, V_SE, V_SW, V_NW
    use BitboardM, only: pop_lsb, get_ray_in_direction, &
                         BBK, BB_EMPTY, BB_KING_ATTACKS, BB_KNIGHT_ATTACKS
    use EvaluationM, only: EVK, PIECE_VALUE, BASE_CAPTURE_VALUE
    use BaseM, only: BaseT, piece_at
    use MoveStackM, only: MVK, push_move
    use MoveM, only: NORMAL_MOVE, pack_move, get_uci

    class(BoardT), intent(inout) :: self
    integer(PCK), intent(in) :: ty

    integer(BBK) :: blocks, moves, occ, ray, targets, attackers
    integer(SQK) :: fr_sq, to_sq, block_sq, v
    integer(SQK), dimension(8) :: dir_set
    integer(PCK) :: co, victim
    integer(EVK) :: score
    integer(MVK) :: mv
    integer :: iv

    co = self%state%turn
    targets = self%occupied_co(XCOLOR(co))
    occ = ior(self%occupied_co(WHITE), self%occupied_co(BLACK))

    select case (ty)
    case (KING)
      attackers = self%kings_co(co)
    case (KNIGHT)
      attackers = self%knights_co(co)
    case (BISHOP)
      attackers = self%bishops_co(co)
      dir_set = [V_NE, V_NW, V_SE, V_SW, 0_SQK, 0_SQK, 0_SQK, 0_SQK]
    case (ROOK)
      attackers = self%rooks_co(co)
      dir_set = [V_N, V_E, V_S, V_W, 0_SQK, 0_SQK, 0_SQK, 0_SQK]
    case (QUEEN)
      attackers = self%queens_co(co)
      dir_set = [V_NE, V_NW, V_SE, V_SW, V_N, V_E, V_S, V_W]
    case default
      error stop "unknown leaper piece type"
    end select

    moves = BB_EMPTY
    do while (attackers /= BB_EMPTY)
      call pop_lsb(attackers, fr_sq)

      if (ty == KING) then
        moves = iand(BB_KING_ATTACKS(fr_sq), targets)
      else if (ty == KNIGHT) then
        moves = iand(BB_KNIGHT_ATTACKS(fr_sq), targets)
      else
        !! slider ray casting
        do iv = 1, 8
          v = dir_set(iv); if (v == 0_SQK) exit
          ray = get_ray_in_direction(fr_sq, v)
          blocks = iand(ray, occ)
          if (blocks == BB_EMPTY) cycle
          block_sq = int(merge(trailz(blocks), &
                               63 - leadz(blocks), v > 0), SQK)
          !! include first blocker, only if enemy
          if (btest(targets, block_sq)) moves = ibset(moves, block_sq)
        end do
      end if
    end do

    !! process captures first; high priority
    do while (moves /= BB_EMPTY)
      call pop_lsb(moves, to_sq)
      victim = piece_at(self, to_sq)
      score = &
        BASE_CAPTURE_VALUE + PIECE_VALUE(victim)*5_EVK + PIECE_VALUE(ty + co)
      mv = pack_move(priority=score, &
                     from_sq=fr_sq, &
                     to_sq=to_sq, &
                     piece=ty + co, &
                     captured=victim, &
                     promotion=EMPTY, &
                     move_type=NORMAL_MOVE)
      call push_move(self%stack, mv)
    end do
  end subroutine generate_piece_captures

  !! @brief process piece captures
  module &
#ifndef DEBUG
    pure &
#endif
    subroutine generate_piece_quiets(self, ty)

    use PieceM, only: &
      PCK, EMPTY, WHITE, BLACK, PAWN, KNIGHT, BISHOP, ROOK, QUEEN, KING
    use SquareM, only: SQK, V_N, V_E, V_S, V_W, V_NE, V_SE, V_SW, V_NW
    use BitboardM, only: pop_lsb, &
         get_ray_in_direction, BBK, BB_EMPTY, BB_KING_ATTACKS, BB_KNIGHT_ATTACKS
    use BaseM, only: BaseT
    use MoveM, only: NORMAL_MOVE, pack_move, get_uci
    use MoveStackM, only: push_move

    class(BoardT), intent(inout) :: self
    integer(PCK), intent(in) :: ty

    integer(BBK) :: attackers, blocks, moves, occ, ray, shadow
    integer(SQK) :: fr_sq, to_sq, block_sq, v
    integer(SQK), dimension(8) :: dir_set
    integer(MVK) :: mv
    integer(PCK) :: co
    integer :: iv

    occ = ior(self%occupied_co(WHITE), self%occupied_co(BLACK))
    co = self%state%turn

    if (ty == PAWN) then
      error stop "generate_piece_quiets: no PAWN calls here"
    end if

    select case (ty)
    case (KING)
      attackers = self%kings_co(co)
    case (KNIGHT)
      attackers = self%knights_co(co)
    case (BISHOP)
      attackers = self%bishops_co(co)
      dir_set = [V_NE, V_NW, V_SE, V_SW, 0_SQK, 0_SQK, 0_SQK, 0_SQK]
    case (ROOK)
      attackers = self%rooks_co(co)
      dir_set = [V_N, V_E, V_S, V_W, 0_SQK, 0_SQK, 0_SQK, 0_SQK]
    case (QUEEN)
      attackers = self%queens_co(co)
      dir_set = [V_NE, V_NW, V_SE, V_SW, V_N, V_E, V_S, V_W]
    case default
      error stop
    end select

    do while (attackers /= BB_EMPTY)
      call pop_lsb(attackers, fr_sq)
      moves = BB_EMPTY

      select case (ty)
      case (KING)
        moves = iand(BB_KING_ATTACKS(fr_sq), not(occ))
      case (KNIGHT)
        moves = iand(BB_KNIGHT_ATTACKS(fr_sq), not(occ))
      case (BISHOP, ROOK, QUEEN)
        !! slider ray casting
        do iv = 1, 8
          v = dir_set(iv); if (v == 0_SQK) exit
          ray = get_ray_in_direction(fr_sq, v)
          blocks = iand(ray, occ)

          if (blocks /= BB_EMPTY) then
            !! find blocker and remove it + shadow
            block_sq = int(merge(trailz(blocks), &
                                 63 - leadz(blocks), v > 0), SQK)
            shadow = get_ray_in_direction(block_sq, v)
            moves = ior(moves, ieor(ray, ibset(shadow, block_sq)))
          else
            !! path is clear to the edge
            moves = ior(moves, ray)
          end if
        end do
      end select

      !! process and push quiet moves
      do while (moves /= BB_EMPTY)
        call pop_lsb(moves, to_sq)
        mv = pack_move(priority=0_int16, &
                       from_sq=fr_sq, &
                       to_sq=to_sq, &
                       piece=ty + co, &
                       captured=EMPTY, &
                       promotion=EMPTY, &
                       move_type=NORMAL_MOVE)
        call push_move(self%stack, mv)
      end do
    end do
  end subroutine generate_piece_quiets

  !! @brief apply move on the board
  module &
    subroutine make_move(self, mv)

    use BaseM, only: BaseT
    use BitBoardM, only: BB_EMPTY
    use MoveM, only: get_captured
    use MoveStackM, only: next_ply, print_path
    use PieceM, only: XCOLOR, BLACK, WHITE, PIECE_TYPE, KING
    use StateM, only: StateT, update_state_hash

    class(BoardT), intent(inout) :: self
    integer(MVK), intent(in) :: mv

    type(StateT) :: old_state
    integer :: p

    if (mv == 0) error stop "make_move: trying a null move?"

#ifdef DEBUG
    if (PIECE_TYPE(get_captured(mv)) == KING) then
      call print_path(self%stack)
    end if
#endif

#ifdef DEBUG
    call assert_evaluation(self, "before make_move")
#endif

    ! store state and mv before the move
    p = self%stack%current_ply
    old_state = self%state
    self%state_sequence(p) = old_state
    self%move_sequence(p) = mv

    if (self%kings_co(WHITE) == BB_EMPTY) error stop "L2419, king_co(WHITE)==0"
    if (self%kings_co(BLACK) == BB_EMPTY) error stop "L2420, king_co(BLACK)==0"

    ! move the pieces, then update hash and score with PSTs
    call move_pieces(self, mv)
    call update_hash_and_score(self, self%state, mv)

    if (self%kings_co(WHITE) == BB_EMPTY) error stop "L2528, king_co(WHITE)==0"
    if (self%kings_co(BLACK) == BB_EMPTY) error stop "L2529, king_co(BLACK)==0"

    ! update irreversible state (castling, ep, hmcl), then hash
    call update_state(self%state, mv)
    call update_state_hash(self%state, old_state)  !<--- old_state

    ! finalize turn and ply
    self%state%turn = XCOLOR(self%state%turn)
    self%state%fullmove_number = self%state%fullmove_number + self%state%turn

    self%stack%path(self%stack%current_ply) = mv
    call next_ply(self%stack)

#ifdef DEBUG
    call assert_evaluation(self, "after make_move")
#endif

  end subroutine make_move

  !! @brief undo the move just made on the board
  module &
    subroutine unmake_move(self, mv)

    use BaseM, only: full_score
    use BitBoardM, only: BB_EMPTY
    use PieceM, only: BLACK, WHITE
    use MoveStackM, only: MVK, prev_ply

    class(BoardT), intent(inout) :: self
    integer(MVK), intent(in) :: mv

    self%stack%path(self%stack%current_ply) = 0_MVK

    ! roll-back search first
    call prev_ply(self%stack)

    ! reverse changes on the board, restore state, and decrement ply
    self%state = self%state_sequence(self%stack%current_ply)

    ! unmove the pieces using the restored turn
    call unmove_pieces(self, mv)

    ! slower than copying into undo history, but a baseline for shallow tests
    ! TODO replace evaluation components alongside state_sequence into history
    call full_score(self, self%score, self%phase, self%score_mg)

    if (self%kings_co(WHITE) == BB_EMPTY) error stop "L2471, king_co(WHITE)==0"
    if (self%kings_co(BLACK) == BB_EMPTY) error stop "L2472, king_co(BLACK)==0"

#ifdef DEBUG
    call assert_evaluation(self, "after unmake_move")
#endif
  end subroutine unmake_move

  !! @brief perform piece movement(s) only
  module &
    subroutine move_pieces(self, mv)

    use BaseM, only: BaseT, base_fen, piece_at, remove_piece_at, set_piece_at
    use MoveM, only: &
      EP_MOVE, get_from_sq, get_piece, get_promotion, get_captured, get_to_sq, &
      get_move_type
    use PieceM, only: &
      PCK, PIECE_COLOR, PIECE_TYPE, EMPTY, KING, ROOK, PIECE_CHAR
    use SquareM, only: &
      FILE_A, FILE_D, FILE_E, FILE_F, FILE_G, FILE_H, &
      SQK, SQUARE_FILE, RANK_OFFSET, SQUARE_NAME
    use StringUtilsM

    class(BoardT), intent(inout) :: self
    integer(MVK), intent(in) :: mv

    integer(kind=PCK) :: rk, arrival_pc
    integer(kind=SQK) :: r_rank_offset, from_file, to_file, victim_sq

    if (get_piece(mv) /= piece_at(self, get_from_sq(mv))) &
      error stop "ERROR: mv%piece /= piece_at(mv%from_sq),"// &
      " mv%from_sq="//SQUARE_NAME(get_from_sq(mv))// &
      " mv%piece="//PIECE_CHAR(get_piece(mv))// &
      " piece_at="//PIECE_CHAR(piece_at(self, get_from_sq(mv)))

    ! handle possible promotion
    arrival_pc = merge(get_promotion(mv), &
                       get_piece(mv), &
                       get_promotion(mv) /= EMPTY)

    ! remove captured piece
    if (get_captured(mv) /= EMPTY) then
      victim_sq = merge(RANK_OFFSET(get_from_sq(mv)) + &
                        SQUARE_FILE(get_to_sq(mv)), &
                        get_to_sq(mv), &
                        (get_move_type(mv) == EP_MOVE))
      call remove_piece_at(self, victim_sq)
    end if

    ! moving from-to: regular piece, attacker, or promotion
    call remove_piece_at(self, get_from_sq(mv))
    call set_piece_at(self, get_to_sq(mv), arrival_pc)

    ! if castling, move the rook
    if ((PIECE_TYPE(get_piece(mv)) == KING) .and. &
        (abs(get_from_sq(mv) - get_to_sq(mv)) == abs(FILE_G - FILE_E))) then
      r_rank_offset = RANK_OFFSET(get_to_sq(mv))
      rk = ROOK + PIECE_COLOR(get_piece(mv))
      from_file = merge(FILE_H, FILE_A, get_to_sq(mv) > get_from_sq(mv))
      to_file = merge(FILE_F, FILE_D, get_to_sq(mv) > get_from_sq(mv))
      call remove_piece_at(self, r_rank_offset + from_file)
      call set_piece_at(self, r_rank_offset + to_file, rk)
    end if

  end subroutine move_pieces

  !! @brief unmove the piece(s) only
  module &
    subroutine unmove_pieces(self, mv)

    use BaseM, only: BaseT, base_fen, set_piece_at, remove_piece_at
    use PieceM, only: PCK, PIECE_COLOR, EMPTY, KING, ROOK
    use MoveM, only: &
      EP_MOVE, get_from_sq, get_to_sq, get_piece, get_captured, &
      get_promotion, get_move_type
    use SquareM, only: &
      FILE_A, FILE_D, FILE_F, FILE_H, SQK, RANK_OFFSET, SQUARE_FILE

    class(BoardT), intent(inout) :: self
    integer(MVK), intent(in) :: mv

    integer(kind=SQK) :: ep_victim_sq, r_rank_offset, r_from_sq, r_to_sq, victim_sq, &
                         from_sq, to_sq
    integer(kind=int8) :: move_type
    integer(kind=PCK) :: arrival_pc, co, piece, captured, promotion

    from_sq = get_from_sq(mv)
    to_sq = get_to_sq(mv)
    piece = get_piece(mv)
    captured = get_captured(mv)
    promotion = get_promotion(mv)
    move_type = get_move_type(mv)
    co = PIECE_COLOR(piece)

    if (from_sq < 0 .or. from_sq > 63) stop "L441: from_sq bounds"
    if (to_sq < 0 .or. to_sq > 63) stop "L442: to_sq bounds"

    ! id piece at 'to' (handles promotion)
    arrival_pc = merge(promotion, piece, promotion /= EMPTY)

    ! if castled, uncastle the rook
    if (piece == KING + co .and. abs(to_sq - from_sq) == 2) then

      ! choose king/queen-side for 0:63 LEFR, castling, and to > from
      r_rank_offset = RANK_OFFSET(from_sq)
      r_from_sq = r_rank_offset + merge(FILE_H, FILE_A, to_sq > from_sq)
      r_to_sq = r_rank_offset + merge(FILE_F, FILE_D, to_sq > from_sq)
      call remove_piece_at(self, r_to_sq)
      call set_piece_at(self, r_from_sq, ROOK + co)
    end if

    ! restore mover, whether regular/attacker/promotion
    call remove_piece_at(self, to_sq)
    call set_piece_at(self, from_sq, piece)

    ! restore victim, whether regular/ep
    if (captured /= EMPTY) then
      ep_victim_sq = RANK_OFFSET(from_sq) + SQUARE_FILE(to_sq)
      victim_sq = merge(ep_victim_sq, to_sq, move_type == EP_MOVE)
      call set_piece_at(self, victim_sq, captured)
    end if
  end subroutine unmove_pieces

  !! @brief call Move.from_uci
  module &
    subroutine parse_uci(self, uci_str, mv)

    use iso_fortran_env, only: error_unit
    use MoveM, only: null_move, get_uci, from_uci
    use MoveStackM, only: MVK

    class(BoardT), intent(inout) :: self
    character(len=*), intent(in) :: uci_str
    integer(MVK), intent(out) :: mv

    integer :: i, nmoves, p

    call self%generate_legal_moves(nmoves)

    p = self%stack%current_ply
    do i = self%stack%ix1(p), self%stack%ixn(p)
      mv = self%stack%move_list(i)

      if (get_uci(mv) == uci_str) return
    end do

    write (error_unit, "(a,a)") "Move not found:", trim(uci_str)
    mv = 0_MVK
  end subroutine parse_uci

  !! @brief perform state update before piece movement
  module &
    pure &
    subroutine update_state(state, mv)

    use SquareM, only: SQK, V_N, EP_NONE
    use PieceM, only: PCK, PAWN, EMPTY, PIECE_TYPE
    use StateM, only: StateT, CS_UPDATE_MASK
    use MoveStackM, only: MVK
    use MoveM, only: get_from_sq, get_to_sq, get_piece, get_captured

    type(StateT), intent(inout) :: state
    integer(MVK), intent(in) :: mv

    integer(SQK) :: from_sq, to_sq
    integer(PCK) :: captured, piece

    from_sq = get_from_sq(mv)
    to_sq = get_to_sq(mv)
    piece = get_piece(mv)
    captured = get_captured(mv)

    ! update castle rights based on from_sq or to_sq
    state%castling_rights = iand(state%castling_rights, CS_UPDATE_MASK(from_sq))
    state%castling_rights = iand(state%castling_rights, CS_UPDATE_MASK(to_sq))

    ! set new en passant square, due to two square pawn push
    state%ep_sq = EP_NONE
    if (PIECE_TYPE(piece) == PAWN .and. &
        abs(to_sq - from_sq) == abs(V_N*2_SQK)) &
      state%ep_sq = int((from_sq + to_sq)/2, SQK)

    ! reset half-move clock
    if (PIECE_TYPE(piece) == PAWN .or. captured /= EMPTY) then
      state%halfmove_clock = 0_int16
    else
      state%halfmove_clock = state%halfmove_clock + 1_int16
    end if
  end subroutine update_state

end submodule GeneratorM
