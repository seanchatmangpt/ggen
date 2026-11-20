#!/usr/bin/env bash
set -euo pipefail
# Usage: run-view.sh <run-id> [--log]
run="${1:?usage: run-view.sh <run-id> [--log]}"
shift || true
gh run view "$run" "$@"

