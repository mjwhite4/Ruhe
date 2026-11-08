# Ruhe — A UCI chess engine in Fortran

**Ruhe** is a hobby chess engine written in modern Fortran, using Fortran 2018
for the debug build. Created by Michael White, the project began in February
2026 under the name **Unsound**.

The **0.1 milestone**, reached in November 2026, connects legal move generation,
a minimal alpha-beta negamax search, iterative deepening, static evaluation, and
UCI interaction. Debug and release test suites and the Python UCI integration
suite have passed on the development machine. Playing strength has not yet been
measured through controlled matches.

See the [developer log](docs/dev-log-0.1.md) for the history, diagnosed defects,
corrections, and regression evidence, and [TODO.md](TODO.md) for the next steps.
“0.1” identifies this milestone; package/build version metadata may still use a
date-based version.

## Current capabilities

- Bitboard-based move generation with ray attacks and retained 0x88-derived
  board-boundary methods. Many masks are compile-time constants; some ray/vector
  tables are initialized at runtime.
- Castling, promotions, en passant, checks, checkmate, and stalemate covered by
  position tests and reference perft counts.
- Alpha-beta negamax with iterative deepening and a separate, unpruned negamax
  reference for shallow comparisons.
- Basic material, piece-square, and game-phase evaluation. Debug assertions compare
  incremental evaluation with full recomputation around make/unmake operations.
- Zobrist hashing with internal consistency checks. Compatibility with Polyglot
  or Stockfish/Fruit hashing conventions is **not established**.
- UCI handshake, readiness, position setup, depth/node/time limits, ordinary clocks
  and increments, infinite search, stop, and quit. Search reports depth, nodes,
  elapsed time, and a one-move `pv`.
- Single-threaded search with buffered nonblocking POSIX input through a small C
  interface, allowing readiness and stop commands during search.

Quiescence search, principal variation search, full principal-variation tracking,
and transposition-table integration into search are future work. A transposition
storage module and its unit tests already exist.

## Requirements

- GNU Fortran (`gfortran`) and a C compiler for `src/uci_input.c`.
- Fortran Package Manager (`fpm`) with runner support.
- Bash 3.2 or later for `fpmx.sh`.
- A POSIX environment; macOS is the development/test platform. Linux and WSL are
  intended targets, but the reported milestone results do not establish testing
  on those platforms. Native Windows input support is not supplied.
- Python 3 and `python-chess` for the separate UCI integration tests.

The wrapper contains compiler/CPU-specific flags. Review them for a new host:
release currently includes `-mcpu=native`, which may need adjustment on a different
architecture. Debug includes strict warnings, runtime checks, and `-DDEBUG`.

## Build and run

Clone the repository using an account authorized to access it:

```bash
git clone https://github.com/mjwhite4/Ruhe.git
cd Ruhe

./fpmx.sh build release
./fpmx.sh build debug
```

The wrapper copies the selected executable into the project root:

| Profile | Executable |
| --- | --- |
| Release | `./ruhe` |
| Debug | `./ruhe-debug` |

Intermediate files remain under fpm's `build/` directories. The wrapper does not
install into `~/.local/bin`; its `install` action is an alias for `build`.

Run `./ruhe` interactively, or configure a UCI controller to launch its absolute
path. For a manual protocol check, enter commands one at a time:

```text
uci
isready
position startpos moves e2e4 e7e5
go depth 3
```

Wait for `bestmove`, then enter `quit`. For an infinite search, use `go infinite`
and subsequently `stop`. Additional human-facing commands include `help` and
`license`.

## Tests

Run the fpm suite in both profiles:

```bash
./fpmx.sh test debug
./fpmx.sh test release
```

Focused examples:

```bash
./fpmx.sh test test_perft release
./fpmx.sh test test_go_parser debug
./fpmx.sh test test_clock debug
./fpmx.sh test test_search debug
```

A successful wrapper `test` run also builds/copies the application for that profile.

The suite covers board primitives, attacks, moves, FEN, state, move-stack behavior,
evaluation, table storage, command parsing, and clock arithmetic. Ten perft cases
exercise reference leaf counts at depths 4–6. Search tests compare alpha-beta
with reference negamax at depths 1–3 and check root restoration.

Run process-level tests separately; `fpm test` does not invoke this Python suite:

```bash
python3 -m venv .venv-uci
.venv-uci/bin/python -m pip install python-chess

.venv-uci/bin/python test_extra/test_uci_integration.py ./ruhe-debug
.venv-uci/bin/python test_extra/test_uci_integration.py ./ruhe
```

The integration suite validates best-move legality independently with
`python-chess`, checks the UCI completion responses and timing bounds, exercises
stop/restart and readiness during search, verifies terminal responses, and checks
idle/active-search shutdown. Add `--verbose` to inspect protocol traffic.

Timing results depend on the machine, flags, instrumentation, and position. Perft
throughput measures legal-tree enumeration, not search strength. Exact allocation
arithmetic is tested internally; process timing checks use scheduling slack.

## Project structure

| Path | Purpose |
| --- | --- |
| `app/` | Application entry point. |
| `src/` | Fortran modules/submodules and the C input layer. |
| `test/` | fpm test programs, shared helpers, and reference inputs. |
| `test_extra/test_uci_integration.py` | Live UCI process tests. |
| `docs/dev-log-0.1.md` | Milestone history, fixes, and verification limits. |
| `docs/adr/` | Architecture decision records. |
| `fpmx.sh` | Profile flags, testing, and root-executable copying. |
| `build/` | Generated fpm intermediates; excluded from Git. |

## Development

Keep changes small, run the relevant tests, review the staged diff, and commit
working improvements regularly. Run both profiles and the UCI integration suite
before tagging a milestone. Record bug fixes and their regression checks in the
[developer log](docs/dev-log-0.1.md).

## Author

Created and maintained by **Michael White**.

## License and acknowledgments

Ruhe is licensed under **GPL-3.0-or-later**, as declared in `fpm.toml`.
See [LICENSE](LICENSE) for the license text and source headers for notices.

Portions of the logic and architecture are translated or adapted from
[python-chess](https://github.com/niklasf/python-chess) by Niklas Fiekas.
ChatGPT and Gemini have assisted with implementation, debugging, and tests.
