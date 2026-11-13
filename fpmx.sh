#!/usr/bin/env bash
# Run fpm with profile flags and publish binaries directly to the project root.
# Usage:
#   ./fpmx.sh build debug
#   ./fpmx.sh build release
#   ./fpmx.sh test [debug|release] [test_name ...]
#   ./fpmx.sh run [debug|release] -- [engine arguments ...]
#   ./fpmx.sh clean
set -eo pipefail

FPM="fortran-fpm"

PROJECT_ROOT=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd -P)
cd -- "$PROJECT_ROOT"
[[ -f fpm.toml ]] || { echo '[fpmx] Place fpmx.sh beside fpm.toml.' >&2; exit 2; }

ACTION="${1:-build}"
PROFILE="debug"
TARGETS=()
ENGINE_ARGS=()

# Basic argument parsing
case "$1" in
  build|test|run|clean)
    ACTION="$1"
    shift
    ;;
  debug|release)
    PROFILE="$1"
    shift
    ACTION="${1:-build}"
    [[ $# -gt 0 ]] && shift
    ;;
  -h|--help)
    echo "Usage: ./fpmx.sh [build|test|run|clean] [debug|release] [options/args]"
    exit 0
    ;;
esac

# Check if next arg is profile when action was specified first
if [[ "$ACTION" == build || "$ACTION" == test || "$ACTION" == run ]]; then
  if [[ "${1:-}" == "debug" || "${1:-}" == "release" ]]; then
    PROFILE="$1"
    shift
  fi
fi

# Define local binary destination
if [[ "$PROFILE" == debug ]]; then
  LOCAL_EXECUTABLE="./ruhe-debug"
else
  LOCAL_EXECUTABLE="./ruhe"
fi

case "$ACTION" in
  clean)
    echo "[fpmx] Cleaning fpm build and local binaries..."
    $FPM clean
    rm -f -- ./ruhe ./ruhe-debug ./ruhe-*
    exit 0
    ;;
  test)
    echo "[fpmx] Running tests ($PROFILE profile)..."
    $FPM test --profile "$PROFILE" "$@"
    ;;
  build)
    echo "[fpmx] Building and publishing $LOCAL_EXECUTABLE ($PROFILE profile)..."
    $FPM run ruhe --profile "$PROFILE" --runner cp -- "$LOCAL_EXECUTABLE"
    echo "[fpmx] Successfully updated $LOCAL_EXECUTABLE"
    ;;
  run)
    # Ensure it's built/published first, then execute with args
    $FPM run ruhe --profile "$PROFILE" --runner cp -- "$LOCAL_EXECUTABLE"
    exec "$LOCAL_EXECUTABLE" "$@"
    ;;
  *)
    echo "Unsupported action: $ACTION" >&2
    exit 2
    ;;
esac
