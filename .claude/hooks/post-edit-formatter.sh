#!/bin/bash
set -euo pipefail

# Post-edit formatter hook
# Reads JSON input, extracts file_path, runs cargo fmt --check on Rust files
# Input: JSON with file_path field
# Exit: 0 if check passes or file is not Rust, non-zero if formatting issues found

# Read JSON input from argument or stdin
JSON_INPUT="${1:-$(cat)}"

# Extract file_path from JSON using jq (with fallback)
FILE_PATH=$(echo "$JSON_INPUT" | jq -r '.file_path // empty' 2>/dev/null || echo "")

# Fallback: extract using sed if jq unavailable
if [ -z "$FILE_PATH" ]; then
    FILE_PATH=$(echo "$JSON_INPUT" | sed -n 's/.*"file_path"\s*:\s*"\([^"]*\)".*/\1/p' | head -1)
fi

# Validate extraction
if [ -z "$FILE_PATH" ]; then
    echo "Error: Could not extract file_path from JSON" >&2
    exit 1
fi

# Skip non-Rust files
if [[ ! "$FILE_PATH" =~ \.rs$ ]]; then
    exit 0
fi

# Find workspace root and navigate
WORKSPACE_ROOT="$(git rev-parse --show-toplevel 2>/dev/null || dirname "$FILE_PATH")"
cd "$WORKSPACE_ROOT"

# Run cargo fmt --check
cargo fmt --check -- "$FILE_PATH"
