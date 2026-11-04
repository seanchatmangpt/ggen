#!/usr/bin/env bash
set -euo pipefail
# Usage: run-rerun.sh --repo <owner/repo> <run-id> [--failed-only]
repo="" ; failed=""
while [ $# -gt 0 ]; do
  case "$1" in
    --repo) repo="$2"; shift 2 ;;
    --failed-only) failed="--failed"; shift ;;
    *) run="$1"; shift ;;
  esac
done
[ -n "$repo" ] && [ -n "${run:-}" ] || { echo "usage: run-rerun.sh --repo <owner/repo> <run-id> [--failed-only]" >&2; exit 2; }
gh run rerun "$run" --repo "$repo" ${failed}
