
## UCI Interactivity Testing

Testing `stdin`/`stdout` interaction for a UCI engine is a standard pattern in chess engine development. Because the Universal Chess Interface protocol relies strictly on text strings over standard input/output, you can build automated testing suites at multiple levels—ranging from simple Bash scripts to Python integration frameworks and specialized engine compliance testers.

---

### Level 1: Simple Bash Piping (Quick Smoke Tests)

The simplest way to test startup banners, custom commands, and basic search loops without a GUI is to pipe commands directly into the engine executable using a shell script or `echo`/`printf`.

#### Example: `tests/test_cli_smoke.sh`

```bash
#!/usr/bin/env bash
set -euo pipefail

ENGINE="./ruhe"

echo "=== Test 1: Handshake & License Commands ==="
output=$(printf "uci\nlicense\nquit\n" | $ENGINE)

# Assert required UCI responses
echo "$output" | grep -q "id name" || (echo "FAILED: Missing 'id name'" && exit 1)
echo "$output" | grep -q "uciok" || (echo "FAILED: Missing 'uciok'" && exit 1)
echo "$output" | grep -q "info string" || (echo "FAILED: Missing 'info string' license notice" && exit 1)
echo "PASS: Handshake & custom query test"

echo "=== Test 2: Standard Position Search ==="
output=$(printf "uci\nisready\nposition startpos moves e2e4\ngo depth 4\nquit\n" | $ENGINE)

echo "$output" | grep -q "readyok" || (echo "FAILED: Missing 'readyok'" && exit 1)
echo "$output" | grep -q "bestmove" || (echo "FAILED: Missing 'bestmove'" && exit 1)
echo "PASS: Position setup and search response"

```

---

### Level 2: Python Integration Suite with `python-chess` (Recommended)

For deep automated testing (e.g., verifying `bestmove` outputs, validating board state sync, or checking search depth), **`python-chess`** provides a built-in UCI subprocess manager (`chess.engine`).

You can write end-to-end integration tests using Python's standard `unittest` or `pytest` framework.

#### Example: `tests/test_uci_integration.py`

```python
import pytest
import chess
import chess.engine

ENGINE_PATH = "./ruhe"

@pytest.fixture
def engine():
    # Spawns process and opens stdin/stdout pipes
    transport, engine = chess.engine.SimpleEngine.popen_uci(ENGINE_PATH)
    yield engine
    engine.quit()

def test_uci_options_and_id(engine):
    """Test engine identification on startup."""
    assert "Ruhe" in engine.id["name"]
    assert engine.id["author"] is not None

def test_position_and_search(engine):
    """Test engine responding with a valid move from initial position."""
    board = chess.Board()
    result = engine.play(board, chess.engine.Limit(depth=5))
    
    # Assert engine returned a legal move
    assert result.move in board.legal_moves

def test_fen_position_setup(engine):
    """Test setting up a specific tactical position (e.g. Mate in 1)."""
    # Scholar's mate position
    board = chess.Board("r1bqkb1r/pppp1ppp/2n5/4p3/2B1n3/5Q2/PPPP1PPP/RNB1K1NR w KQkq - 0 4")
    result = engine.play(board, chess.engine.Limit(depth=3))
    
    # Engine should identify Qxf7#
    assert result.move == chess.Move.from_uci("f3f7")

```

---

### Level 3: Protocol Compliance Validation (`chess-engine-tester`)

To ensure **Ruhe** adheres strictly to the UCI protocol specification without crashing, hanging, or producing malformed output when communicating with computer clients (GUIs like Cutechess or Arena), you can use open-source protocol compliance tools.

1. **`cutechess-cli` Dry Run:**
Run a short 1-game match with tight time controls and logging enabled:
```bash
cutechess-cli -engine name=Ruhe cmd=./ruhe proto=uci \
              -engine name=Stockfish cmd=stockfish proto=uci \
              -each tc=1+0.01 -games 1 -debug

```


Check the output log to verify that `isready` / `readyok` synchronization completes without timing out.
2. **Custom Interactive Shell Tests (Handling Human Input vs GUI Input):**
Humans type commands one by one, while GUIs often dump multiple commands into `stdin` at once (e.g., `position ...\ngo depth 6`). Test this batching explicitly in a text fixture:
```bash
# Batch multiple commands into stdin simultaneously
cat << 'EOF' | ./ruhe
uci
isready
position startpos moves e2e4 e7e5
go movetime 100
quit
EOF

```



---

### Recommended Testing Architecture for Ruhe

| Test Scope | Target Scenarios | Recommended Tool |
| --- | --- | --- |
| **CLI & Banners** | Startup notices, `id name`, `license`, `warranty` | Bash script with `grep` assertions |
| **Protocol Parsing** | Command batching, unexpected input, `isready`/`readyok` sync | Python `subprocess` or `pytest` |
| **Chess Logic / Search** | FEN setups, legal move generation, mate search | Python `chess.engine` integration tests |
| **Client / GUI Match** | Full game lifecycle, time control handling | `cutechess-cli` dry runs |
