#!/usr/bin/env bash
set -euo pipefail
# Usage: repo-state.sh --repo <owner/repo>
repo=""
while [ $# -gt 0 ]; do
  case "$1" in
    --repo) repo="$2"; shift 2 ;;
    *) echo "unknown arg: $1" >&2; exit 2 ;;
  esac
done
[ -n "$repo" ] || { echo "usage: repo-state.sh --repo <owner/repo>" >&2; exit 2; }
echo "== repository =="
gh repo view "$repo" --json name,visibility,defaultBranchRef,isArchived,deleteBranchOnMerge --jq '.'
echo "== branch protection (default branch) =="
gh api "repos/${repo}/branches/$(gh repo view "$repo" --json defaultBranchRef --jq .defaultBranchRef.name)/protection" 2>/dev/null || echo "(no protection or no access)"
echo "== open PRs =="
gh pr list --repo "$repo" --state open --json number,title,mergeable --jq '.'
echo "== latest workflow runs =="
gh run list --repo "$repo" --limit 5
