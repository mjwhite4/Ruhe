#!/usr/bin/env bash
set -euo pipefail

ENGINE=~/.local/bin/ruhe

echo "=== Test 1: Handshake & License Commands ==="
echo "Starting Engine:" $ENGINE

if output=$(printf "uci\nlicense\nquit\n" | $ENGINE); then
  echo "  [PASS] Engine started and exited successfully"
else
  status=$?
  printf '  [FAIL] Engine failed with status %d\n' "$status" >&2
  printf '%s\n' "$output"
  exit "$status"
fi

# Assert required UCI responses
grep -q 'id name' <<< "$output" || (echo "FAILED: Missing 'id name'" && exit 1)
grep -q 'uciok' <<< "$output" || (echo "FAILED: Missing 'uciok'" && exit 1)
grep -q "info string" <<< $output || (echo "FAILED: Missing 'info string' license notice" && exit 1)
echo "PASS: Handshake & custom query test"

echo "=== Test 2: Standard Position Search ==="
output=$(printf "uci\nisready\nposition startpos moves e2e4\ngo depth 4\nquit\n" | $ENGINE)

grep -q "readyok" <<< $output || (echo "FAILED: Missing 'readyok'" && exit 1)
grep -q "bestmove" <<< $output || (echo "FAILED: Missing 'bestmove'" && exit 1)
echo "PASS: Position setup and search response"
