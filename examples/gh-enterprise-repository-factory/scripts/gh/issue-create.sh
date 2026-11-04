#!/usr/bin/env bash
set -euo pipefail
# Usage: issue-create.sh --repo <owner/repo> --title <t> [--body <b>] [--label <l>]...
repo="" ; title="" ; body="" ; labels=()
while [ $# -gt 0 ]; do
  case "$1" in
    --repo) repo="$2"; shift 2 ;;
    --title) title="$2"; shift 2 ;;
    --body) body="$2"; shift 2 ;;
    --label) labels+=(--label "$2"); shift 2 ;;
    *) echo "unknown arg: $1" >&2; exit 2 ;;
  esac
done
[ -n "$repo" ] && [ -n "$title" ] || { echo "usage: issue-create.sh --repo <owner/repo> --title <t> [--body <b>] [--label <l>]..." >&2; exit 2; }
gh issue create --repo "$repo" --title "$title" --body "$body" "${labels[@]}"
