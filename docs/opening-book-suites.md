
## Opening Book Suites

Chess engine tournaments like the **TCEC (Top Chess Engine Championship)** and **CCC (Chess.com Computer Chess Championship)** use pre-selected opening suites to eliminate color bias and test engine evaluation across a diverse range of structures.

### 1. The Game Pair System (Sequential Duals)

To ensure fairness, engines play opening positions in **two-game pairs**:

* **Game 1:** Engine A plays White, Engine B plays Black from the given starting position.
* **Game 2:** Engine B plays White, Engine A plays Black from the exact same starting position.

This setup measures which engine handles the position better overall, minimizing the inherent first-move advantage of White.

---

### 2. How Opening Suites Are Chosen

Tournament organizers do not pick openings randomly during live play; they use carefully curated **"book suites"** built by expert opening book authors (such as Jeroen Noomen, Nelson Hernandez, or Stefan Pohl).

#### Standard Selection Criteria:

* **Move Depth:** Openings typically start anywhere from **8 to 16 ply (4–8 full moves)** deep, though some specialized "super-deep" stages start 20+ ply deep.
* **Bias Control & Evaluation Range:** Book authors analyze positions with strong engines to ensure the starting evaluation is fairly balanced—usually within **+0.10 to +0.50 pawn advantage** for White.
* **Structural Diversity:** The suites are designed to cover a broad spectrum of chess theory, including open tactical games, closed maneuvering structures, gambits, and asymmetrical pawn setups.
* **Uniqueness:** Lines that lead to immediate, forced draws or forced wins are filtered out.

---

### 3. Are the Opening Books Available Pre-Tournament?

**Yes, the opening suites are publicly accessible**, but the exact timing of when engines or the public get access depends on the tournament's anti-tuning policies:

#### Public Availability & Transparency:

* **TCEC:** TCEC hosts its opening books publicly in their [official Archive/GitHub repositories](https://www.google.com/search?q=https://github.com/TCEC-chess) and on their website/Discord. You can download past and current season opening books (`.pgn` or `.epd` files).
* **CCC:** Chess.com frequently publishes the opening books used in their events on public GitHub pages or via community posts.

#### Pre-Tournament Access Restrictions:

* **Anti-Book Tuning:** To prevent engine developers from hardcoding specialized engine evaluation overrides or specific "anti-opponent" preparations for the exact tournament lines, organizers usually keep the specific suite secret or release it shortly before the stage begins.
* **Blinded Selection:** In some stages, book authors generate a pool of several thousand balanced positions, and the tournament script randomly selects a subset for each round right as the stage starts.

---

### Summary of Popular Public Opening Suites

If you want to run your own engine matches using the same starting positions as major tournaments, you can download these standard test suites:

* **TCEC Opening Books:** Created by Jeroen Noomen and Nelson Hernandez (available in PGN/EPD format via the TCEC archive).
* **Noomen Test Suites:** Widely considered the gold standard for unbiased engine testing.
* **UHO (Unbalanced Human Openings):** A popular suite created by Stefan Pohl designed to force engines into non-drawish, asymmetric positions.
* **Cutechess / Engine Test Books:** Standard 8-ply and 12-ply books available in the `cutechess-cli` repository.
