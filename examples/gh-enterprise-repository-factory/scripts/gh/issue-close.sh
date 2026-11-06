#!/usr/bin/env bash
set -euo pipefail
# Usage: issue-close.sh --repo <owner/repo> <issue-number> [--comment <c>]
repo="" ; comment=""
while [ $# -gt 0 ]; do
  case "$1" in
    --repo) repo="$2"; shift 2 ;;
    --comment) comment="$2"; shift 2 ;;
    *) n="$1"; shift ;;
  esac
done
[ -n "$repo" ] && [ -n "${n:-}" ] || { echo "usage: issue-close.sh --repo <owner/repo> <issue-number> [--comment <c>]" >&2; exit 2; }
echo "current state:"; gh issue view "$n" --repo "$repo" --json number,title,state,labels --jq '.'
if [ -n "$comment" ]; then
  gh issue close "$n" --repo "$repo" --comment "$comment"
else
  gh issue close "$n" --repo "$repo"
fi

