#!/usr/bin/env bash
set -euo pipefail

# check arg count (Header + at least one target)
if [ "$#" -lt 2 ]; then
    echo "Usage: $0 <HEADER_FILE> <TARGET_FILE_1> [TARGET_FILE_2 ...]" >&2
    exit 1
fi

HEADER="$1"
shift  # remove header from arg list ($@ now contains target files)

# verify header exists and is readable
if [ ! -f "$HEADER" ] || [ ! -r "$HEADER" ]; then
    echo "Error: Header file '$HEADER' does not exist or is not readable." >&2
    exit 1
fi

# process targets
for file in "$@"; do

    # skip if not file
    if [ ! -f "$file" ]; then
        echo "Warning: Skipping '$file' (not a regular file)." >&2
        continue
    fi

    # skip if unwritable
    if [ ! -w "$file" ]; then
        echo "Warning: Skipping '$file' (permission denied / not writable)." >&2
        continue
    fi

    # create mp file in the same directory (preserves filesystem boundaries)
    tmp_file=$(mktemp "$(dirname "$file")/tmp.XXXXXX")

    # cat header + original file into tmp file
    if cat "$HEADER" "$file" > "$tmp_file"; then

        # preserve permissions -- if it fails it fails !!!
        chmod --reference="$file" "$tmp_file" 2>/dev/null || true
        
        # atomic replacement
        mv -f "$tmp_file" "$file"
        echo "Updated: $file"
    else
        # uh-oh
        echo "Error: Failed to process '$file'. Original file unchanged." >&2
        rm -f "$tmp_file"
    fi
done
