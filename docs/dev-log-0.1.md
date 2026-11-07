# Ruhe 0.1 — introductory milestone and developer log

**Recorded:** 2026-11-07  
**Author:** Michael White  
**Project begun:** February 2026, originally named Unsound  
**Status:** pre-commit milestone draft; commit identifier to be added after check-in.

## Milestone

Ruhe now connects legal move generation, a minimal alpha-beta negamax search,
iterative deepening, static evaluation, and UCI process interaction. Both debug
and release test suites have been reported passing. Process-level tests also pass
for depth, nodes, fixed move time, ordinary clocks and increments, infinite search,
readiness during search, interruption, terminal positions, and shutdown.

This completes the introductory phase of the project. It establishes a working
and testable engine, rather than a claim about playing strength, complete UCI
coverage, external hash compatibility, or freedom from deeper search defects.

The most valuable result was a verification method: check optimized or incremental
operations against simpler reference computations, then verify that undo restores
the original position. Passing perft alone was not enough to establish that
cached evaluation and search control were correct.

## Evidence and dates

This log combines Michael's recollections, the debugging conversations and test
output, and inspection of `ruhe_0_1.zip` supplied on 2026-11-07. February–August
history combines recollection with retained file timestamps. Michael confirms
that those timestamps correctly reflect normal file times. The dates displayed
for ZIP entries are treated as modification timestamps, not assumed creation
dates. They establish that a retained version existed by its last modification,
not when a feature first worked or a defect was discovered. November dates
identify the documented investigation or confirmation.

The archive includes `src/`, `test/`, `fpm.toml`, and `fpmx.sh`. The current Python integration script was supplied separately
on 2026-11-07 and reviewed alongside the archive. The archive does not include
the application entry point under `app/`, the integration script under
`test_extra/`, or a complete snapshot of the project at the May pause. Two earlier partial
archives, `ruhe_0_01.zip` and `ruhe_0_001.zip`, were subsequently inspected. Their
retained modification timestamps are accepted as historical evidence; neither
archive supplies a complete build/test history. Consequently it is a source review
snapshot, not a complete independently built release package. Passing build and
integration results below are Michael's reported local runs; the archive was not
compiled or executed while preparing this log.

ChatGPT and Gemini assisted with implementation, diagnosis, and tests. Attribution
of individual test modules is not reconstructed where the available history does
not establish it.

## Development chronology

| Period | Development and lessons |
| --- | --- |
| February–early March 2026 | Initial 0x88 work progressed toward perft and bitboards. The earliest recovered collection includes February-dated hashing, evaluation, clock, and search sources as well as the Makefile. This places source development earlier than the initial recollection of May; it does not establish that all those components were working or verified then. |
| March 2026, approximate | Throughput was recalled as tens of thousands of nodes per second. This motivated a transition toward bitboards; it is not a controlled benchmark against the current implementation. |
| March–May 2026, approximate | Added bitboard masks and ray-based attacks, mostly without magic bitboards. Retained useful 0x88-derived expressions for concise board-boundary checks. Fortran constant expressions allowed many masks to be defined at compile time. The current source still has runtime ray/vector table initialization, so “no startup initialization” applies only to the constant portions. |
| Before the May pause | Worked on Zobrist hashing and positional/game-phase evaluation with Gemini's assistance. Polyglot compatibility was initially intended. Recovered February-dated sources show these efforts had already begun earlier; evaluation remained primitive and insufficiently verified. |
| Around May 5–approximately August 2026 | Work paused sometime in May, possibly May 5, and resumed near August, according to Michael's clarification. Exact pause and resumption dates remain approximate. |
| Around August 2026 or later, approximate | Hash-table provenance and compatibility were reconsidered, including a recollection of Stockfish/Fruit-related tables. This log does not authenticate that provenance or claim compatibility. |
| October 2026 | Expanded fpm tests, made build profiles and root-level executable copying predictable, and developed nonblocking UCI input and clock/parser tests. Search exposed move-stack and state-restoration defects beyond the initial move-generation checks. |
| November 5–6, documented | Separated position checks from shallow reference search comparisons; isolated input polling from algorithm tests; corrected incremental/full evaluation differences and undo behavior. |
| November 6, documented | Fixed iterative-deepening integration, then passed the basic and expanded Python UCI suites with debug and release executables. Also repaired a local Homebrew Python/Expat problem that had prevented installing the test dependency. |
| November 7, documented | Reviewed perft throughput units and assembled this milestone record from the source snapshot and passing-run evidence. |

## Earlier partial snapshot: structural evidence, not a dated baseline

Michael supplied `ruhe_0_01.zip` on 2026-11-07. Its incomplete contents initially
seemed inconsistent with the recalled May working collection. He subsequently
confirmed the file dates are correct. It contains 16 source files, most last
modified 2026-09-05 09:42, placing those retained versions after the approximate
August resumption. It has no Makefile, fpm manifest, or test
directory. The absence of these files from this partial archive does not establish
that there were no tests or Makefile in the historical working project. It must
not be labeled “the May version” or used to date the introduction of individual
features or defects.

Inspection nevertheless supplies useful comparisons:

- `main.f90` declares `program unsound` and enters `CuiM/mainloop`; this preserves
  the original program identifier, now confirmed by Michael as the project's
  first name, Unsound.
- Board storage uses `BaseBoardM/BaseBoardT`, with movement in `MovementM` and a
  large `BoardM` containing move-generation and make/unmake implementations.
  The milestone snapshot instead uses `BaseM`, `MoveM`, and the `GeneratorM`
  submodule. No calendar date is inferred for that restructuring.
- Bitboard, evaluation, state, and Zobrist modules are already present. The
  archive is therefore not an example of the recalled February board-only phase.
- `PerftM.f90` contains recursive legal-move traversal with make/unmake and a
  CPU-time throughput report. `CuiM` invokes depths 1–3 and compares the initial
  stored hash with a full calculation. These are observable diagnostic paths,
  not a recovered fpm assertion suite or evidence of a successful historical run.
- `PerftM_new.f90` is an unfinished alternative declaring the same module name;
  its board argument and recursive traversal are commented out. A missing build
  recipe would have been needed to establish which sources were selected.
- The earlier `EvaluationM/full_score` already contains the two-dimensional PST
  lookup expression later corrected in November. It also reads optional dummy
  arguments declared `intent(out)` rather than assigning their outputs, so those
  supplied values are undefined on entry. The current routine computes and
  assigns the optional outputs. Presence in this archive does not date either
  defect's introduction.
- The earlier `unmake_move` restores state and pieces without restoring cached
  evaluation. Conversely, its child-list initializer already uses
  `ixn = free - 1`. The later stack defect must not be described as inevitable
  or present in every earlier version.
- No alpha-beta/negamax implementation was found in these source files. This is
  a statement about the supplied subset, not the complete historical project.

Keep this snapshot as supporting evidence for implementation lineage. The additional archive discussed below recovers a Makefile and some diagnostic
code with accepted early modification dates, but not the recalled fpm helper.

## Additional early collection: Unsound and the recovered Makefile

A further archive, `ruhe_0_001.zip`, was supplied on 2026-11-07. Michael identifies
it as supporting his recollection of the May timeframe and explicitly confirms
that the project was first called **Unsound**. Its `main.f90` declares
`program unsound`, and its recovered `src/Makefile` sets `PROJECT = unsound`.

The collection's source timestamps mostly range from February 19 to March 2,
2026; auxiliary files have still earlier timestamps. Michael confirms these file
dates are correct. A file retained in a May working collection can legitimately
have a February modification date if it was not subsequently edited. The dates
therefore support early development of hashing, evaluation, and search without
proving successful execution at that time. This collection may preserve material
from the pre-pause project, but is not a complete, precisely dated May baseline.

What the source itself establishes:

- **The missing build recipe existed.** `src/Makefile` orders compilation of
  board, bitboard, state, Zobrist, movement, generation, evaluation, clock,
  transposition-table, ordering, search, perft, and command-interface modules.
  It has debug/release targets. It also defines `DBG_FLAGS` but references
  `DEBUG_FLAGS` in the debug rule: historical build intent must not be confused
  with verified use of those flags.
- **There was early search experimentation.** `SearchEngineM` contains recursive
  `alpha_beta_wmem` and `quiesce_mt` routines, with table/order/clock machinery.
  This revises any inference that November introduced the first search code:
  November's milestone is the minimal search verified with reference and UCI
  tests. The earlier principal recursion passes `depth` unchanged; there is no
  passing execution evidence here, so this log does not characterize that search
  as complete or correct. Its `MT` comments do not establish multithreading.
- **Verification preceded the current fpm suite.** `PerftM` checks White's
  occupancy-bit count against the piece-array count after making a move and
  contains a commented-out full-score comparison. `other/test_0x88.f90` is a
  standalone test. `other/TestM.f90` refers to core-test routines and a
  `SquareSetM` module absent from the archive. Thus some diagnostic/test code
  survives, without reconstructing a runnable historical test collection.
- **Other retained material includes** EPD/PGN/command input files, a Python
  search file, and an auxiliary `PolyglotM` that imports a missing `ChessTypesM`.
  Their presence neither authenticates current hash compatibility nor proves
  that the main build used them; the Makefile does not list those auxiliary
  sources.
- **The recalled fpm transition is still incompletely documented.** This ZIP has
  no fpm manifest, substantial fpm test directory, or `fpm_extra` command. Michael
  notes that such files may have been moved elsewhere. Their absence here is not
  evidence that they never existed.

No historical archive was built or run for this comparison. Preserve the useful
structural evidence without forcing either collection into an unsupported date
or treating the older code as a linear predecessor of every current module.

## Defect and correction ledger

### Move generation and stack ownership

| Problem observed | Correction or invariant | Regression evidence |
| --- | --- | --- |
| Search attempted moves whose encoded piece disagreed with the board: examples included a rook encoded at `b1`, and a knight departing empty `h3`. October investigation. | A newly prepared ply must represent an empty list: `ixn(p) = ix1(p) - 1`. Its starting index follows the parent's active range. Returning to the parent restores its allocation cursor without overwriting the parent's list. | `test_movestack` traversal checks; recursive perft and `test_search`; DEBUG generated-piece and restored-ply assertions. The history does not assign every observed mismatch to one cause. |
| Risk of confusing game history, stack index, search ply, and remaining depth. | Search starts with root-relative `ply = 0`; recursion increments ply and decrements remaining depth. The current stack index is a storage/history coordinate and need not start at the same value. | Search calls and depth-1–3 comparisons; stack-ply restoration checks. Add a dedicated nonzero-history search test rather than assuming FEN-only cases establish this. |
| Illegal en passant appeared to be accepted. November 5 investigation. | The printed legal list already excluded `d4e3`; the forbidden-move assertion had reversed polarity. This was a test defect, not evidence that the discovered-check routine failed. | `test_position`: legal and illegal en passant cases. The illegal case is `8/8/8/8/k2pP2R/8/8/3K4 b - e3 0 1`; removing both rank-four pawns would expose the black king to the rook. |
| Checkmate test expected `0000` to occur in a legal move list. | A terminal position has zero legal moves. `0000` is the UCI result for no available move, not a legal move to generate. | Separate terminal checks in `test_position`, terminal search expectations in `test_search`, and exactly-one-null-response Python tests. |

### Evaluation and reversible state

November 6 comparisons made previously vague scoring problems actionable:

1. **Undo restored pieces and state, but not evaluation.** Evaluation fields live
   in the base board, outside `StateT`. Restoring `state_sequence` did not restore
   `score_mg`, `phase`, or `score`. The current baseline recomputes these with
   `full_score` after `unmove_pieces`. Saving/restoring evaluation in undo history
   remains a possible optimization, not the implementation recorded here.
2. **Incremental middlegame updates omitted material changes.** The correction
   includes both material and piece-square contributions when removing or adding
   a piece. This matters for captures and promotions as well as ordinary moves.
3. **Full and incremental tapered scores rounded at different points.** Both now
   add the integer middlegame sum to the converted phase-dependent boost using
   the same expression structure.
4. **The reference full evaluation itself contained a Fortran array-section bug.**
   `sum(VALUE_PST(get_mailbox_array(self), 0:63))` selects a two-dimensional
   section; it does not pair each piece with its occupied square. It was replaced
   by a square loop adding `VALUE_PST(self%piece_at(sq), sq)`. The king move `f6f7`
   exposed a cached/full middlegame discrepancy of 920 versus 900. Correcting the
   reference computation allowed it to agree with incremental evaluation.

The permanent diagnostic compares cached score, middlegame score, and phase
against independent `full_score` outputs before and after make, and after undo,
under `DEBUG`. The comparison writes into local variables: it must not silently
repair the value being checked. Search additionally compares root values saved
before the algorithm with those present after it returns.

An important lesson is that a saved value is not automatically correct. When undo
recomputes a value, a before/after mismatch may expose a bad pre-move cache rather
than a bad undo. Checks on both sides locate the first divergence.

### UCI, control, and the test harness

| Problem observed | Correction | Regression evidence |
| --- | --- | --- |
| Blocking or mixed input methods prevented attending to `stop` during search. | One buffered OS input path: nonblocking reads, retained partial lines, newline extraction, explicit EOF/error statuses, and idle polling. The Fortran C interface uses an assumed-size byte array `bytes(*)` matching the C pointer ABI. | Process tests for readiness during infinite search, stop, restart, and quit during search. Partial-line, CRLF, overflow, and EOF edge cases are not established by those tests. |
| An apparent delayed response could be caused by the Python reader accepting an old completion token or using readiness checks around buffered text I/O. | Queue-backed reader threads drain stdout and stderr. Wait for exactly `uciok`, exactly `readyok`, or a `bestmove ` line according to the command. Flush engine responses. | Python handshake and sequential search tests. |
| The direct alpha-beta unit test hung inside `should_stop`. November 5–6. | Initialize limits/control for each test search, then set `poll_input = .false.`. Algorithm tests do not depend on stdin or initialize a UCI session. Production search retains command polling. | `test_search` completes in both profiles; Python tests separately exercise the production input path. |
| Parser errors included missing values/status handling and risks of swapped or stale limits. October onward. | Reset limits on each command, parse integer fields with checked status/range, and return failure to tests without unconditional termination. | `test_go_parser`: distinct clock/increment values, combined limits, reset behavior, missing arguments, malformed values, nonpositive depths, and int64 overflow. |
| Search received `go depth 3` but emitted no progress. November 6. | `nmoves` had been passed to the `depth` argument of `search_deeper`. Removed that argument; iteration depth is local and explicitly starts at 1. Check the parsed limit after each completed iteration. | Python `go depth 3` requires completed depth 3 and a legal best move. |
| Terminal searches could emit two `bestmove` responses. November 6. | Search returns a result; the UCI `go` handler owns the single final response, including `0000`. | Python checkmate/stalemate tests reject duplicate responses across readiness and a short observation interval. |
| Node/time stops and restart need state distinct from the returned move. | Count visited nodes, initialize control for each search, unwind every made move, discard incomplete iterations, and retain a legal fallback/completed result. | `test_clock`, search restoration assertions, and process tests for limits, stop, and a subsequent search. Exact whole-search node enforcement still needs a targeted search-level assertion. |

Named C/Fortran return constants now distinguish a complete line (1), no line yet
(0), EOF (-1), input error (-2), and overflow (-3). These avoid ambiguous numeric
branches. The current implementation remains single-threaded. OpenMP reader and
parallel-search work is deferred; it is not a 0.1 capability.

### Build workflow, diagnostics, and measurements

- Earlier wrappers left a duplicate `--profile` argument or mishandled argument
  advancement. The current `fpmx.sh` consumes arguments explicitly and invokes
  commands through Bash arrays rather than `eval`.
- `fpm install` used an installation prefix, while the desired workflow was two
  executables in the project root. The wrapper now uses
  `fpm run ruhe ... --runner cp -- ./ruhe[-debug]` to copy the exact selected
  executable. It does not guess which hashed build directory is current.
- `test` runs fpm's selected test executables with the selected flags and copies
  the application only after success. `install` is a compatibility alias for
  this wrapper's build action, not a call to `fpm install`.
- Millisecond throughput was mislabeled during development: nodes/ms is already
  knode/s. Dividing by 1000 is appropriate only when labeling the result in
  millions of nodes per second. The snapshot uses `knps/1000` with `Mnps`.
  `elapsed_time` supplies integer milliseconds, so a real-valued variable and a
  small denominator guard do not restore submillisecond measurement precision.
- Temporary prints helped locate blocking input, incorrect depth, stale stack
  moves, and evaluation divergence. Preserve failure assertions and explanatory
  comments; remove unconditional traces. Commented prints are historical clues,
  not regression protection.

## Regression inventory

| Test layer | Files and scope |
| --- | --- |
| Representation and attacks | `test_piece`, `test_square`, `test_bitboard`, `test_base`, `test_board`: encodings, bit scans, masks, rays, placement, attacks, pins, basic legality. |
| Moves, state, serialization | `test_move`, `test_movestack`, `test_state`, `test_fen`: packing/UCI conversion, piece movement round trips, stack traversal, castling masks, state hash updates, FEN round trips. |
| Evaluation and table storage | `test_evaluation`: values, initial symmetry, material advantage, bare kings/phase. `test_ttable`: allocation, store/probe, replacement. Table tests do not establish transposition-table use in search. |
| Legal special positions | `test_position`: mate in one, forced move, checkmate, stalemate, hanging queen, four promotions, legal/illegal en passant, castling. Required/forbidden moves are checked here. |
| Search reference | `test_search`: both algorithms at depths 1–3 on nine positions; equal scores; supplied reference moves/scores; restored root FEN, score, stored hash, current ply, active count and move-list membership. Its `require_moves`/`forbidden_moves` arguments are presently unused; those assertions belong to `test_position`. General legality of every returned move is not independently checked here. |
| Perft | `test_perft` includes `perft.inc`; tabulated leaf totals plus DEBUG FEN/hash checks around traversal. Shared make/unmake evaluation assertions add coverage to every actual make/unmake pair. |
| Parsing and clock arithmetic | `test_go_parser`, `test_clock`: checked values and resets, limit boundaries, movetime precedence, side selection, reserve, increments, allocation caps, restart. |
| Live process | `test_extra/test_uci_integration.py`, supplied separately and inspected: independent python-chess legal-move validation and protocol/timing/shutdown checks. It is run explicitly, not by `fpm test`. |

### Perft reference cases in the snapshot

Exact FENs remain in `test/perft.inc`; names below follow that file and are not
claims that every label is a standard published position number.

| Case | Depth | Expected leaves |
| --- | ---: | ---: |
| Starting position | 4 | 197,281 |
| Kiwipete | 4 | 4,085,603 |
| Rook-attack/castling-transit variant | 4 | 3,517,436 |
| Position 4 | 5 | 15,833,292 |
| Position 5 | 4 | 2,103,487 |
| Position 6 | 4 | 3,894,594 |
| Simultaneous check and promotion | 5 | 3,605,103 |
| Rook captures/castling rights | 5 | 7,594,526 |
| En passant discovered-check case | 6 | 1,321,438 |
| En passant and check | 5 | 674,624 |

Perft uses a depth-one shortcut that counts legal moves without making them.
Therefore the DEBUG make/unmake checks cover traversed edges, not every final
leaf move. The current perft explicitly verifies the hash before make and after
make and compares FEN after undo; it does not independently recompute the hash
immediately after every undo. Do not describe the checks more broadly than that.

## What the passing results establish

The fpm debug and release suites were reported passing after evaluation fixes.
The expanded Python suite subsequently passed against both executables. The
separately supplied script confirms the implemented checks (its latest changes
only add spacing between timing values and units):

- Handshake and readiness, depth 3, nodes 1000, and movetime 500 ms.
- Readiness during infinite search, stop, and a new search after interruption.
- Checkmate/stalemate with exactly one `bestmove 0000`.
- White and Black clock allocation cases with 0 and 100 ms increments.
- Idle quit and quit during active search, both with exit status zero.

Reported fixed-movetime runs were approximately 0.500 s; active-search quit was
approximately 0.002 s. These are observations from one local environment, not
portable performance guarantees. Clock checks allow scheduling slack and accept
early completion. They test upper-bound behavior, not optimal time usage. Reported
completed-iteration node counts cannot prove an exact interrupted node total.

Negamax is a useful pruning reference because it omits alpha-beta cutoffs, input
polling, and clock checks. It still shares move generation, make/unmake, draw
logic, and evaluation with alpha-beta. Agreement is strong evidence against a
pruning discrepancy at tested depths, but cannot reveal every shared bug.

## Hash compatibility: keep the claim narrow

The current snapshot demonstrates internal consistency checks, not a verified
external hashing format. In particular, `calculate_ep_hash` hashes the file of
any stored en passant square without consulting potential capturers. Compatibility
requires checking that rule along with table provenance, piece/square indexing,
castling, and side-to-move conventions against the chosen external format.

`test/zobrist.inc` contains fixed FEN/key pairs, but no supplied `.f90` test
includes it. Those values must not be counted as executed regression coverage.
The `src/Zobrist/` directory in the ZIP is empty. Preserve Michael's account of
the intended Polyglot compatibility and later Stockfish/Fruit attribution as
historical recollection until provenance and executable cross-checks settle it.

## Reproduce and preserve the milestone

From the complete project root:

```bash
./fpmx.sh test debug
./fpmx.sh test release

# Optional focused runs:
./fpmx.sh test test_go_parser debug
./fpmx.sh test test_clock debug
./fpmx.sh test test_search debug

# Create the test environment once with a working Python interpreter:
python3 -m venv .venv-uci
.venv-uci/bin/python -m pip install python-chess

.venv-uci/bin/python test_extra/test_uci_integration.py ./ruhe-debug
.venv-uci/bin/python test_extra/test_uci_integration.py ./ruhe
```

The recorded integration dependency installation used `python-chess 1.999` and
`chess 1.11.2`. The local Homebrew Python 3.12.16 initially could not import
`pyexpat` because its linked system Expat lacked a required symbol. This also
broke plist reading, macOS version discovery, pip, and virtual-environment pip
bootstrapping. A local source rebuild restored the import (Expat 2.7.1) and
macOS version reporting. This was a development-environment defect, separate
from the engine.

Before marking the release, record the commit identifier, compiler/fpm versions,
platform, flags, and retained test logs. The manifest currently says
`version = "2026.10.26"`; this document's “0.1” is the milestone name, not a claim
that all version metadata has already been synchronized. Include the application
entry point and Python test in the checked-in project, and exclude generated
executables, build outputs, and virtual environments as appropriate.

## Follow-up work

1. Preserve this passing baseline in version control, then run short controlled
   games before making playing-strength claims.
2. Add exact whole-search node-limit checks and nonzero-history root tests.
3. Exercise fragmented input, multiple commands per read, CRLF, EOF, overflow,
   and malformed commands at the process boundary.
4. Make the search's maximum-ply behavior graceful: the stack currently guards
   exhaustion with `error stop`, which is protection against corruption rather
   than a normal search termination policy.
5. Wire fixed hash vectors into an executed test and authenticate any external
   compatibility claim.
6. Retain full evaluation as an oracle when optimizing undo with saved caches;
   verify asymmetric positions and all incremental components.
7. Strengthen test helpers where optional arguments are accessed: for example,
   `test_position` still combines `present(ref_nmoves)` with a reference to the
   argument in one `.and.` expression. Fortran does not guarantee short-circuit
   evaluation; nested checks are preferable even though current calls supply it.
8. Explore OpenMP input/search separately after this single-threaded baseline is
   preserved. Compare each optimization against the existing reference tests.

The reusable lesson of 0.1 is to keep a simple oracle, check invariants at the
mutation boundary, and turn each diagnosed failure into an executable assertion
or a documented coverage gap. A clean test transcript is the result; the checks
that make the transcript meaningful are the knowledge worth preserving.
