#!/usr/bin/env bash
set -euo pipefail
# Usage: accept-drift-inject.sh --repo <owner/name>
repo=""
while [ $# -gt 0 ]; do
  case "$1" in
    --repo) repo="$2"; shift 2 ;;
    *) echo "unknown arg: $1" >&2; exit 2 ;;
  esac
done
[ -n "$repo" ] || { echo "usage: accept-drift-inject.sh --repo <owner/name>" >&2; exit 2; }
gh api -X PATCH "repos/${repo}/labels/異常" -f color=ffffff >/dev/null
echo "drift injected: 異常 color=ffffff on ${repo}"

