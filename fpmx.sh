#!/usr/bin/env bash
# Run fpm with ruhe's profile flags and publish only to the project root.
# Place this script beside fpm.toml. Requires Bash 3.2 or later.
# Usage:
#   ./fpmx.sh build [ruhe] release
#   ./fpmx.sh install debug
#   ./fpmx.sh test [test_name ...] release
#   ./fpmx.sh run debug -- [engine arguments ...]
#   ./fpmx.sh clean
# Profile defaults to debug; --profile debug/release is also accepted.
# test runs fpm tests, then copies the app only if the tests succeed.
# install is a compatibility alias for build: neither action calls fpm install.
set -eo pipefail

DEBUG_FLAGS="-std=f2018 -Wall -Wextra -Werror -fcheck=all -fall-intrinsics -g -fbacktrace -cpp -DDEBUG"
RELEASE_FLAGS="-O3 -mcpu=native -ffast-math -funroll-loops -fno-stack-protector -flto -fall-intrinsics"

fail() { printf '[fpmx] %s\n' "$*" >&2; exit 2; }
usage() {
  printf '%s\n' 'Usage: fpmx.sh <build|install|test|run|clean> [targets] [debug|release] [-- engine-args]'
}
run_command() {
  printf '%q ' "$@"
  printf '\n'
  "$@"
}

PROJECT_ROOT=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd -P)
cd -- "$PROJECT_ROOT"
[[ -f fpm.toml ]] || fail 'Place fpmx.sh beside fpm.toml.'
(($#)) || { usage >&2; exit 2; }
ACTION=$1
shift
case "$ACTION" in
  -h|--help) usage; exit 0 ;;
  build|install|test|run|clean) ;;
  *) fail "Unsupported action: $ACTION" ;;
esac

PROFILE=debug
PROFILE_SET=false
TARGETS=()
ENGINE_ARGS=()
VERBOSE=()
while (($#)); do
  case "$1" in
    --profile)
      (($# >= 2)) || fail '--profile requires debug or release'
      VALUE=$2
      shift 2
      ;;
    --profile=*) VALUE=${1#*=}; shift ;;
    debug|release) VALUE=$1; shift ;;
    --verbose) VERBOSE=(--verbose); shift; continue ;;
    --)
      [[ "$ACTION" == run ]] || fail '-- engine arguments are supported only for run'
      shift
      ENGINE_ARGS=("$@")
      break
      ;;
    -*) fail "Unsupported option: $1 (installation paths and flags are managed by this wrapper)" ;;
    *) TARGETS+=("$1"); shift; continue ;;
  esac
  case "$VALUE" in debug|release) ;; *) fail "Unsupported profile: $VALUE" ;; esac
  if [[ "$PROFILE_SET" == true && "$PROFILE" != "$VALUE" ]]; then
    fail 'Conflicting profiles'
  fi
  PROFILE=$VALUE
  PROFILE_SET=true
done

case "$ACTION" in
  build|run)
    if ((${#TARGETS[@]})); then
      [[ ${#TARGETS[@]} == 1 && ${TARGETS[0]} == ruhe ]] || fail 'Only the ruhe app target is supported'
    fi
    ;;
  install|clean)
    ((${#TARGETS[@]} == 0)) || fail "$ACTION does not accept targets"
    ;;
esac

if [[ "$ACTION" == clean ]]; then
  # Never delete local executables if fpm clean fails.
  run_command fpm clean
  rm -f -- "$PROJECT_ROOT/ruhe" "$PROJECT_ROOT/ruhe-debug"
  exit 0
fi

if [[ "$PROFILE" == debug ]]; then
  FLAGS=$DEBUG_FLAGS
  DESTINATION="$PROJECT_ROOT/ruhe-debug"
else
  FLAGS=$RELEASE_FLAGS
  DESTINATION="$PROJECT_ROOT/ruhe"
fi
COMMON=(--profile "$PROFILE" --flag "$FLAGS")

if [[ "$ACTION" == test ]]; then
  # These are fpm's test executables, compiled with the selected flags.
  run_command fpm test "${COMMON[@]}" "${VERBOSE[@]}" "${TARGETS[@]}"
fi

# Ask fpm to select/build the exact app, then run cp instead of the app.
# The suffix after -- is cp's destination. No installation is performed.
if [[ "$PROFILE" == debug ]]; then
  LOCAL_EXECUTABLE=./ruhe-debug
else
  LOCAL_EXECUTABLE=./ruhe
fi
[[ ! -d "$LOCAL_EXECUTABLE" ]] || fail "Destination is a directory: $LOCAL_EXECUTABLE"
run_command fpm run ruhe "${COMMON[@]}" "${VERBOSE[@]}" --runner cp -- "$LOCAL_EXECUTABLE"
[[ -f "$LOCAL_EXECUTABLE" && -x "$LOCAL_EXECUTABLE" ]] || fail "No executable produced: $LOCAL_EXECUTABLE"
printf '[fpmx] Updated %s\n' "$DESTINATION"

if [[ "$ACTION" == run ]]; then
  exec "$LOCAL_EXECUTABLE" "${ENGINE_ARGS[@]}"
fi
