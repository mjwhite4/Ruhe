#!/usr/bin/env bash
# Build and test both profiles. Stop at the first failed command.
#
# local verification script

set -euo pipefail

# Resolve project paths relative to this script, not the caller's directory.
cd -- "$(dirname -- "${BASH_SOURCE[0]}")"

# PASS/FAIL ascii colors
GREEN=$'\033[32m'
RED=$'\033[31m'
RESET=$'\033[0m'

trap 'status=$?; printf "%s[FAIL]%s freshen.sh line %s: %s (status %s)\n" \
  "$RED" "$RESET" "$LINENO" "$BASH_COMMAND" "$status" >&2; exit "$status"' ERR

for profile in debug release; do
    printf '\n=== Building %s ===\n' "$profile"
    ./fpmx.sh build "$profile"

    printf '\n=== Testing %s ===\n' "$profile"
    ./fpmx.sh test "$profile"

    executable=./ruhe
    if [[ "$profile" == debug ]]; then
        executable=./ruhe-debug
    fi

    printf '\n=== Protocol tests: %s ===\n' "$profile"
    python3 test/test_uci.py "$executable"
done

printf '\n%s[PASS]%s All build and test steps completed.\n' "$GREEN" "$RESET"
