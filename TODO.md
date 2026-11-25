# Ruhe development tasks

Updated 2026-11-24. The [0.1 developer log](docs/dev-log-0.1.md) records the
history, corrections, test evidence, and known limits. Checked items below mark
completed milestone work, not claims that every protocol or chess edge case is
covered.

## Completed for the 0.1 milestone

- [x] Connect minimal alpha-beta negamax, iterative deepening, evaluation, and UCI.
- [x] Compare alpha-beta and unpruned reference negamax at depths 1–3.
- [x] Check root FEN, cached score, stored hash, stack ply, and active move list
  restoration after both searches.
- [x] Correct full/incremental evaluation: paired PST lookups, material updates,
  consistent boost conversion, and evaluation restoration on undo.
- [x] Keep DEBUG evaluation consistency assertions around make/unmake.
- [x] Pass ten reference perft cases, plus special-position tests for
  promotions, castling, en passant, checks, and terminals.
- [x] Test `go` parsing and clock allocation/reset arithmetic through fpm.
- [x] Pass debug and release fpm suites on macOS/min1 and NixOS/nix51.
- [x] Pass UCI integration tests for depth, nodes, movetime, both-color clocks
  and increments, infinite/stop, readiness during search, restart, terminal
  responses, and idle/active-search quit.
- [x] Use `fpmx.sh` to produce root-level `ruhe` and `ruhe-debug` without
  installation into `~/.local/bin`.
- [x] Check the project into GitHub and establish a command-line commit workflow.

## Preserve and reproduce the baseline

- [ ] Add the milestone commit identifier, compiler/fpm versions, platform, and
  retained test logs to the development record.
- [ ] Align `fpm.toml`, build identification, and intended version/tag naming.
- [x] Check Python integration tests into version control.
- [ ] Verify that the developer log is current and committed, and that ignore
  rules exclude generated executables, build products, and virtual environments.
- [ ] Tag the verified baseline when metadata and documentation are ready.
- [ ] Automate logging of test positions and failures, including FEN, move
  history, command, limits, profile, and reproducible invocation.
- [ ] Run the build/test workflow Linux/NixOS completed; WSL unverified.
  Recording exact configurations remains pending.

## Strengthen correctness and protocol coverage

- [x] Added search-level assertions for exact node-limit enforcement, including
  an interruption partway through an iteration.
- [x] Verify search from retained game history, repetition versus FEN-only
  roots, history restoration, and root-relative search ply.
- [ ] Verify the same history-sensitive behavior through the live UCI
  `position ... moves ...` handler.
- [x] Nonterminal search result against the saved root legal-move list; allow
  tied optimal moves in reference comparisons.
- [x] Fragmented, batched, CRLF, EOF, overflow lines, multiple commands in one
  write, and malformed command process tests.
- [x] Combined search limits through the live process and verify recovery
  after rejected commands.
- [x] Stop recursive search gracefully at stack capacity; verify terminal
  precedence, legal driver fallbacks, position restoration, and agreement
  with ordinary depth-one references.
- [x] Keep infinite search responsive at capacity without additional depth
  reports; run both boundary regressions through freshen.sh.
- [x] Add direct full-hash verification immediately after undo in perft. The
  depth-one shortcut counts legal moves without making them. Its final-level
  edges therefor do not receive make/undo mutation checks.
- [x] Add further asymmetric evaluation cases and dedicated checks for all
  cached components across captures, promotions, castling, and en passant.
- [x] Replace optional-argument `.and.` guards with nested checks where
  required; do not depend on Fortran short-circuit evaluation.
- [x] Remove unused `require_moves`/`forbidden_moves` plumbing from
  `test_search`.
- [x] Add selected deeper perft runs as a separate slow suite, keeping ordinary
  regression runs manageable.

## Hashing and provenance

- [ ] Turn `test/zobrist.inc` into an executed regression test; document the
  source and intended convention of each expected key.
- [ ] Verify table provenance, piece/square indexing, side-to-move, castling,
  and en passant rules before claiming Polyglot or Stockfish/Fruit
  compatibility.
- [ ] Document internal hash semantics independently of any external-format
  claim.

## Full-game testing

- [ ] Run short controlled games with saved PGNs and protocol logs, first at
  fixed depth and then with clocks, alternating colors.
- [ ] Check illegal moves, crashes, stalls, time forfeits, game-history
  handling, and state reset between games before interpreting the win/loss
  score.
- [ ] Establish a reproducible strength-testing procedure before reporting
  ratings or improvement claims.

## Search and evaluation improvements

Introduce each improvement separately and retain reference/regression checks.
These are proposed work, not capabilities established by the 0.1 milestone.

- [ ] Add quiescence search with correct in-check and terminal handling; adapt
  the reference comparisons to the intended leaf-search semantics.
- [ ] Track and validate a complete principal variation; current `pv` output
  contains one move.
- [ ] Integrate the tested transposition storage module into search, with tests
  for bounds, depth, replacement, and mate-score handling.
- [ ] Explore move ordering and a history heuristic.
- [ ] Explore principal variation search.
- [ ] Explore late move reductions after the simpler baseline is well covered.
- [ ] Expand positional evaluation, including pawn structure, with explicit
  tests.
- [ ] Optimize undo by saving evaluation components rather than recomputing
  them; retain `full_score` as the DEBUG oracle.
- [ ] Explore OpenMP command reading and eventual parallel search as separate
  changes; consult the threaded-reader ADR. Current input/search is single-
  threaded.

## Reading and experiments

- [ ] Erik Heinz's book.
- [ ] Sebastian Lague's coding challenge.
- [ ] AlphaGo papers.

## Development practice
- Make small commits and include regression checks with important fixes.
Keep reading notes and experimental ideas separate from verified engine features.
