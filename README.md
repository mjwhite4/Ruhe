# ruhe — A UCI chess engine in Fortran

![Version](https://img.shields.io/github/v/release/yourusername/your-engine-name)

**ruhe** is a hobby chess engine written in modern Fortran (Fortran 2008/2018).
It combines bitboard-based move generation with negamax search, quiescence
search, and a transposition table. Its UCI-style command loop is intended for
use with chess interfaces such as Cutechess and Arena.

The code is organized into modules for board state, attacks, and search, with
method names and behavior largely inspired by
[python-chess](https://github.com/niklasf/python-chess).


## Features

- UCI-style command loop.
- Bitboard-based board representation and ray shading with implicit compile-time 0x88 on/off-board detection.
- Negamax search with principal variation tracking and time management.
- Module-level functional tests and perft benchmarks.
- 10-15 Mnps in PERFT on Mac Mini M1


## Planned Features

- Positional Scoring
- Principal Variation Search
- Quiescence Ssearch
- Late Move Reduction
- History Heuristic


## Requirements

- GNU GCC/Fortran (`gfortran`) 10 or later.
- Fortran Package Manager (`fpm`).

The commands below use the repository's `./fpm_arg` wrapper.


## Build

Clone the repository and enter its directory. Replace `REPOSITORY_URL` with
the project's Git URL before running these commands:

```sh
git clone REPOSITORY_URL ruhe
cd ruhe
```

Build with the release profile:

```sh
./fpm_arg build --profile release
```

Build outputs are written under `build/`.


## Tests and benchmarks

The test suite includes functional tests for individual modules and perft
benchmarks for established test positions. Perft counts legal move sequences
to a given depth, helping check move-generation correctness and performance.

Run all tests, including perft benchmarks, with the debug profile:

```sh
./fpm_arg test --profile debug
```

Run only the perft benchmarks with the release profile:

```sh
./fpm_arg test test_perft --profile release
```

## Project structure

| Path | Purpose |
| --- | --- |
| `app/` | Application source. |
| `build/` | Generated executables and object files. |
| `docs/adr/` | Architecture documents and supporting notes. |
| `src/*.f90`, `src/*.F90` | Fortran source, including files processed by the preprocessor. |
| `src/Zobrist/` | Zobrist key translations for Stockfish/Fruit. |
| `test/*.f90` | Tests run through `fpm`. |
| `test/test_extra/` | Input sequences for Stockfish perft and Zobrist key logging. |


## Contributing

See [CONTRIBUTING.md](CONTRIBUTING.md) for contribution guidelines.


## Author

Created and maintained by Michael White.


## License and acknowledgments

ruhe is licensed under the GNU General Public License v3.0. See
[LICENSE](LICENSE) for details.

Portions of the code logic and architecture are translated or adapted from
[python-chess](https://github.com/niklasf/python-chess) by Niklas Fiekas,
which is also licensed under the GNU General Public License v3.0.
