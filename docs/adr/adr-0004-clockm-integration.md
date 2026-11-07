

# ADR-0004: ClockM Integration & Time Management

Integration of `ClockM` into `CuiM.f90` for clock setting, maintaining, reporting, and time-control-related I/O

---

### **PR 1: Time Control Commands & Initialization in `CuiM.f90**`

**Goal:** Add commands to `mainloop` in `CuiM` to manage, set, and report time controls (both XBoard style and standard UCI style).

* **Add `ClockM` Usage to `CuiM`:**
* Import `ClockM` routines: `set_tc`, `start_clock`, `reset_clock`, `sync_clock`, `adjust_opp_time`, `tocs`, `ck`, `tc`.


* **Add Command Handlers in `mainloop()` Select Case:**
* **`level` / `sd` / `st` / `time` / `otim` (XBoard Protocol):**
* Parse `level <mps> <base> <inc>` using `set_tc(args, is_valid)`.
* Handle `time <centiseconds>` by calling `sync_clock(time_str)`.
* Handle `otim <centiseconds>` by calling `adjust_opp_time(time_str)`.
* Handle fixed depth (`sd <depth>`) or fixed move time (`st <seconds>`).


* **`go` (UCI Protocol Time Control):**
* Parse UCI parameters: `wtime`, `btime`, `winc`, `binc`, `movestogo`, `movetime`.
* Calculate turn-based target thinking time and set `tc%soft_limit` / `tc%hard_limit`.


* **`?clock` / `?tc` (Clock Reporting):**
* Add a command to print current clock parameters (`ck%total_spent`, `ck%this_mv_time`, `tc%soft_limit`, `tc%hard_limit`) formatted via `tocs()`.

* **Integrate with `new` and `init_new_game`:**
* Call `reset_clock()` and initialize `start_clock()` when starting a new game or board reset.


---

### **PR 2: Modernize & Fix `ClockM.f90` Unit Consistency**

**Goal:** Clean up bugs and unit mismatches inside `ClockM.f90` so that centiseconds vs. seconds conversion works accurately across systems.

* **Fix Centisecond vs. Second Bugs in `ClockM`:**
* In `sync_clock`, `time_str` is in centiseconds (per XBoard specification), but it currently multiplies `togo_sec * ck%tics_per_sec` as if it were whole seconds. Fix calculation: `togo_time = (togo_centisecs * ck%tics_per_sec) / 100`.

* Standardize internal calculations to `real64` or high-precision integers to prevent integer division overflow/truncation when converting `system_clock` tics.

* **Encapsulation & Re-entrancy Refactoring:**
* Move module-level singletons (`type(ClockT) :: ck`, `type(TimeControlT) :: tc`) inside a unified container structure or attach `ClockT` as a component to the main engine state object to make it thread-safe and re-entrant.

* **Expose UCI Helper Method:**
* Add a subroutine `set_uci_tc(wtime, btime, winc, binc, movestogo, side_to_move)` specifically designed to parse UCI `go` time arguments.

---

### **PR 3: Search Loop Time Monitoring & Abort Checks**

**Goal:** Connect `ClockM` to the search algorithm (`SearchM` / engine iteration loop) so the engine stops searching when time expires.

* **Periodic Time Checks during Iterative Deepening:**
* In the search loop (or root depth loop), call `is_time_up()`.

* Check `ck%soft_timeout` at the completion of a search depth to decide whether to begin a new iteration.

* Check `ck%hard_timeout` periodically inside node evaluation loops (e.g., every 2048 nodes) to break out immediately.

* **Update Clock After Move Selection:**
* After a move is decided and executed, call `update_tc(game_ply)` to update total spent time and calculate average target time for the next move.

* **Report Time Spent in UCI `info` Lines:**
* Pass elapsed time from `tocs(ck%this_mv_time)` to `print_uci_info` to emit `time <ms>` and calculated `nps` (nodes per second).
