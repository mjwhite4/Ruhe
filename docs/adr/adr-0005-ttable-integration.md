
# TTableM Integration

To bring `TTableM` and its integration up to modern UCI standards (matching engines like Stockfish), several key architectural changes are needed.

Stockfish and standard UCI engines operate with specific Transposition Table (TT) behaviors:

1. **Dynamic Size in MB via UCI Option:** The user configures table size via `setoption name Hash value <MB>`, not log2 sizes or static arrays.
2. **Cluster / Bucketed Layout:** Slots are organized into clusters (e.g., 3 or 4 entries per bucket) to reduce hash collisions and prevent key entries from being prematurely overwritten.
3. **Score Normalization for Mate Scores:** Distance-to-mate scores saved in the TT must be adjusted relative to the current search ply so mate bounds remain correct when probed at different tree depths.
4. **Age / Generation System:** A global generation counter increments each search (`position` / `ucinewgame`) so stale entries from prior searches can be safely replaced.

---

### **PR 1: Redesign `TTableM` Structure (MB Sizing, Bucketing, Mate Score Normalization)**

**Goal:** Upgrade `TTableM.f90` to handle allocation by Megabytes, entry buckets, mate score normalization, and generation aging.

#### **Key Implementation Details:**

* **Bucket Layout:** Group entries into clusters (e.g., 3 entries per cache line or bucket):
```fortran
type, public :: ClusterT
  type(EntryT) :: entry(3)
end type ClusterT

```


* **Score Bounds Helper Functions:** Convert search scores to TT scores (normalizing `MATE` / `MATED` relative to `ply`):
```fortran
pure function score_to_tt(score, ply) result(tt_score)
  if (score >= MATE_IN_MAX) then
    tt_score = score + ply
  else if (score <= MATED_IN_MAX) then
    tt_score = score - ply
  else
    tt_score = score
  end if
end function score_to_tt

pure function score_from_tt(tt_score, ply) result(score)
  if (tt_score >= MATE_IN_MAX) then
    score = tt_score - ply
  else if (tt_score <= MATED_IN_MAX) then
    score = tt_score + ply
  else
    score = tt_score
  end if
end function score_from_tt

```


* **Allocation by Megabytes:**
```fortran
subroutine resize_ttable(mb_size)
  integer, intent(in) :: mb_size
  integer(int64) :: bytes, num_clusters
  ! Compute number of clusters fitting into mb_size * 1024 * 1024 bytes...
end subroutine resize_ttable

```


* **Aging Support:** Add `tt_generation` integer variable incremented via `advance_tt_generation()`.

---

### **PR 2: Integrate TT Probing, Storing, and Cutoffs into `SearchM**`

**Goal:** Connect `TTableM` probing and storing logic into `alpha_beta_wmem` and `quiesce_mt` inside `SearchM.f90`.

#### **Key Implementation Details:**

* **Probe at Search Node Entry:**
* Adjust retrieved score using `score_from_tt(entry%score, ply)`.
* If stored depth $\ge$ current search depth:
* `TT_EXACT`: Return stored score immediately.
* `TT_ALPHA`: If stored score $\le \alpha$, return stored score (fail-low).
* `TT_BETA`: If stored score $\ge \beta$, return stored score (fail-high).




* **Store at Search Node Exit:**
* Determine bound flag (`TT_EXACT` if $\alpha < g < \beta$, `TT_BETA` if $g \ge \beta$, `TT_ALPHA` if $g \le \alpha$).
* Normalize score via `score_to_tt(g, ply)` before storing.
* Store move, depth, flag, and current generation.



---

### **PR 3: Support UCI Hash Commands in `CuiM.f90**`

**Goal:** Expose TT configuration and management commands through the UCI command loop.

#### **Key Implementation Details:**

* **Handle UCI Option:** Parse `setoption name Hash value <N>` to reallocate table size dynamically via `resize_ttable(N)`.
* **Handle `ucinewgame`:** Clear all hash entries and increment/reset generation counter.
* **Add Diagnostic Command:** Expose a command (`?ttable` or `?hashstat`) in `mainloop()` to output table occupancy percentage (e.g., `hashfull` permill value matching standard UCI output `info hashfull <0-1000>`).
