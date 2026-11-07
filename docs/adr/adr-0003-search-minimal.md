
### PR Description: Add Minimal UCI-Compatible Negamax Search Routines

#### Overview

Introduce minimal driver entry points in `SearchM.f90` to execute iterative
deepening / negamax searches according to UCI limits (max depth, time limits,
or max node counts).
Output standard UCI progress and result metrics
(`info depth ... nodes ... time ... score ...` and `bestmove ...`).

---

### File Changes

#### `SearchM.f90`

```fortran
! Add to 'public' list in SearchM header:
  public :: &
    alpha_beta_wmem, &  ! MT search
    init_search, &
    quiesce_mt, &
    search_uci, &
    search_depth

contains

```

```fortran
  !! =========================================================================
  !! UCI Search Interface Routines
  !! =========================================================================

  !! @brief Main UCI search driver enforcing depth, time, or node limits
  subroutine search_uci(bd, max_depth, max_time_ms, max_nodes)

    use BoardM, only: BoardT
    use ClockM, only: ck, tic, is_time_up
    use MoveM, only: get_uci, MOVE_NONE
    use MoveStackM, only: MVK
    use EvaluationM, only: EVK, MATE, MATED
    use, intrinsic :: iso_fortran_env, only: int64

    class(BoardT), intent(inout), target :: bd
    integer, intent(in), optional :: max_depth
    integer, intent(in), optional :: max_time_ms
    integer(kind=int64), intent(in), optional :: max_nodes

    integer :: d, target_depth
    integer(kind=EVK) :: score
    integer(MVK) :: best_mv, iteration_best_mv
    integer(kind=int64) :: total_nodes, start_time, current_time, elapsed_ms
    real :: t_start, t_end

    call init_search(bd)
    call tic()
    call cpu_time(t_start)

    ! Set search limits
    target_depth = 64
    if (present(max_depth)) target_depth = max_depth

    ck%hard_timeout = .false.
    ck%soft_timeout = .false.
    if (present(max_time_ms)) then
      ck%time_limit_ms = max_time_ms
    else
      ck%time_limit_ms = -1 ! Unlimited time
    end if

    best_mv = MOVE_NONE

    ! Iterative Deepening Loop
    do d = 1, target_depth
      call search_depth(bd, d, score, iteration_best_mv)

      ! If search didn't time out during depth 'd', update our best move
      if (.not. ck%hard_timeout .and. iteration_best_mv /= MOVE_NONE) then
        best_mv = iteration_best_mv
      end if

      total_nodes = bd%stack%nodes + bd%stack%qnodes
      call cpu_time(t_end)
      elapsed_ms = max(1_int64, int((t_end - t_start) * 1000.0, int64))

      ! Report UCI progress info line
      write(*, '(a,i0,a,i0,a,i0,a,i0,a,a)') &
        'info depth ', d, &
        ' score cp ', score, &
        ' nodes ', total_nodes, &
        ' time ', elapsed_ms, &
        ' pv ', trim(get_uci(best_mv))

      ! Check limits
      call is_time_up()
      if (ck%hard_timeout) exit
      if (present(max_nodes)) then
        if (total_nodes >= max_nodes) exit
      end if
    end do

    ! Final UCI Best Move output
    if (best_mv /= MOVE_NONE) then
      write(*, '(a,a)') 'bestmove ', trim(get_uci(best_mv))
    else
      write(*, '(a)') 'bestmove (none)'
    end if

  end subroutine search_uci


  !! @brief Fixed-depth negamax search wrapper over root moves
  subroutine search_depth(bd, depth, best_score, best_mv)

    use BoardM, only: BoardT, generate_legal_moves, make_move, unmake_move
    use EvaluationM, only: EVK, MATED, DRAW
    use MoveM, only: retro_score
    use MoveStackM, only: MVK, sort_best, MOVE_NONE
    use ClockM, only: ck

    class(BoardT), intent(inout), target :: bd
    integer, intent(in) :: depth
    integer(kind=EVK), intent(out) :: best_score
    integer(MVK), intent(out) :: best_mv

    integer :: ply, ix, nmoves
    integer(kind=EVK) :: alpha, beta, score_tmp
    integer(MVK) :: mv

    best_score = MATED
    best_mv = MOVE_NONE
    alpha = -32000_EVK
    beta = 32000_EVK

    call generate_legal_moves(bd, nmoves)
    if (nmoves == 0) return

    ply = bd%stack%current_ply

    do ix = bd%stack%ix1(ply), bd%stack%ixn(ply)
      call sort_best(bd%stack)
      mv = bd%stack%move_list(ix)

      call make_move(bd, mv)
      call alpha_beta_wmem(bd, -beta, -alpha, score_tmp, depth - 1)
      call unmake_move(bd, mv)

      score_tmp = retro_score(score_tmp)

      if (score_tmp > best_score) then
        best_score = score_tmp
        best_mv = mv
      end if

      if (score_tmp > alpha) alpha = score_tmp
      if (ck%hard_timeout) exit
    end do

  end subroutine search_depth

```

---

### Key Features of this Implementation

1. **UCI Limit Handling:** Accepts `max_depth`, `max_time_ms`, and `max_nodes` options.
2. **Iterative Deepening:** Increments search depth from `1` up to `max_depth`, preserving the best move from completed depth iterations.
3. **Standard Reporting:** Outputs `info depth <d> score cp <score> nodes <n> time <ms> pv <bestmove>` and completes with `bestmove <uci_string>`.
