#!/usr/bin/env bash
set -euo pipefail
# Usage: accept-setup.sh --repo <owner/name>
repo=""
while [ $# -gt 0 ]; do
  case "$1" in
    --repo) repo="$2"; shift 2 ;;
    *) echo "unknown arg: $1" >&2; exit 2 ;;
  esac
done
[ -n "$repo" ] || { echo "usage: accept-setup.sh --repo <owner/name>" >&2; exit 2; }
if ! gh api "repos/${repo}" >/dev/null 2>&1; then
  gh repo create "$repo" --private
  echo "created ${repo}"
else
  echo "reusing ${repo}"
fi
if ! gh api "repos/${repo}/branches/main" >/dev/null 2>&1; then
  content=$(printf 'TCPS sandbox' | base64)
  gh api -X PUT "repos/${repo}/contents/README.md" -f message="TCPS sandbox bootstrap commit" -f content="$content" >/dev/null
  echo "bootstrapped main branch"
fi
