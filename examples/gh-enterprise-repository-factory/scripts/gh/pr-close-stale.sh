#!/usr/bin/env bash
set -euo pipefail
# Usage: pr-close-stale.sh --repo <owner/repo> [--days N] [--confirm]
repo="" ; days=30 ; confirm=0
while [ $# -gt 0 ]; do
  case "$1" in
    --repo) repo="$2"; shift 2 ;;
    --days) days="$2"; shift 2 ;;
    --confirm) confirm=1; shift ;;
    *) echo "unknown arg: $1" >&2; exit 2 ;;
  esac
done
[ -n "$repo" ] || { echo "usage: pr-close-stale.sh --repo <owner/repo> [--days N] [--confirm]" >&2; exit 2; }
cutoff=$(date -v-"${days}"d +%Y-%m-%d 2>/dev/null || date -d "-${days} days" +%Y-%m-%d)
stale=$(gh pr list --repo "$repo" --state open --search "updated:<$cutoff" --json number --jq .[].number)
if [ -z "$stale" ]; then echo "no PRs inactive for ${days}+ days"; exit 0; fi
echo "stale PRs (${days}+ days inactive):"
for n in $stale; do
  gh pr view "$n" --repo "$repo" --json number,title,updatedAt --jq '"#" + (.number|tostring) + " " + .title + " (updated " + .updatedAt + ")"'
  echo
done
if [ "$confirm" -ne 1 ]; then
  echo "preview only — re-run with --confirm to close"; exit 0
fi
for n in $stale; do
  gh pr close "$n" --repo "$repo" --comment "Closed as stale (${days}+ days inactive) by TCPS standard work pr-close-stale.sh"
done
