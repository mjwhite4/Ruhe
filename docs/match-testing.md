## Automated Match Testing

This is the standard approach used by virtually every computer chess developer to test code changes, evaluate parameters, and estimate Elo differences. You don’t need custom hardware or DIY software—the infrastructure for automated engine-vs-engine testing is mature and straightforward to set up on a standard computer.

---

### Key Tools Required

1. **Cutechess-cli (Command-Line GUI Manager)**
The industry standard command-line tool for orchestrating automated engine matches. It handles move execution, time controls, illegal move detection, score tracking, adjudications, and logging games directly to a `.pgn` file.
2. **Opening Book / Suite (EPD or PGN)**
To obtain statistically meaningful results, you must avoid repeating the exact same game or letting the engines play identical lines. Using an opening suite (e.g., 8-ply or 12-ply balanced opening books) ensures opening diversity.
3. **Ordo or Fastchess (Elo Calculation)**
Statistical tools used to parse the resulting `.pgn` file and calculate the exact Elo difference along with error margins (95% confidence intervals) and $p$-values.

---

### Step-by-Step Setup Guide

#### 1. Download `cutechess-cli`

Install `cutechess-cli` on your operating system (available via pre-compiled binaries on GitHub or package managers like Homebrew/APT).

#### 2. Obtain a Balanced Opening Book

Download a standard testing suite such as **UHO_XXL** (Unbalanced Human Openings) or a standard 8-ply book in `.pgn` or `.epd` format.

#### 3. Run the Automated Match Command

Run a command structured like this in your terminal to execute a 1,000-game match:

```bash
cutechess-cli \
  -engine name="Ruhe_v1.00" cmd="./ruhe_v100" \
  -engine name="Ruhe_v1.01" cmd="./ruhe_v101" \
  -each proto=uci option.Hash=64 tc=10+0.1 \
  -openings file=openings.pgn format=pgn order=random -repeat 2 \
  -draw number=10 count=8 score=20 \
  -resign movecount=3 score=400 \
  -games 1000 \
  -rounds 500 \
  -concurrency 4 \
  -pgnout match_results.pgn

```

#### Command Breakdown:

* **`-repeat 2`**: Plays every opening position twice, forcing the engines to switch sides (White/Black) to eliminate color bias.
* **`-concurrency 4`**: Runs 4 games simultaneously in parallel (adjust this based on your CPU core count).
* **`-draw number=10 count=8 score=20`**: Adjudicates games as draws automatically if the evaluation stays within $\pm 0.20$ pawns for 8 consecutive moves after move 10, saving massive CPU time.
* **`-resign movecount=3 score=400`**: Adjudicates a game as a loss if an engine falls behind by $\ge 4.00$ pawns for 3 consecutive moves.

---

### Statistical Significance & Sample Size

When testing small code changes or parameter adjustments:

* **1000 Games:** Gives a error margin of roughly $\pm 15$ to $20\text{ Elo}$. This is usually enough to spot noticeable improvements ($\ge 25\text{ Elo}$).
* **SPSA & SPRT:** Top teams use **SPRT** (Sequential Probability Ratio Test) rather than fixed game counts. SPRT automatically stops the match early as soon as a change passes or fails a statistical threshold ($H_0$: 0 Elo gain vs. $H_1$: +5 Elo gain). Cutechess supports SPRT natively via `-sprt` flags.

---

### Advanced Testing Frameworks (For Large-Scale Testing)

If you plan to run thousands of tests over time, you can look into:

* **Fastchess:** A modern, multi-threaded C++ alternative to `cutechess-cli` specifically optimized for high-throughput SPRT testing.
* **OpenBench:** An open-source web dashboard/distributed testing framework (similar to Stockfish's Fishtest) that lets you queue matches and distribute game testing across multiple computers.
