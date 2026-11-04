#!/usr/bin/env bash
set -euo pipefail
# Usage: release-view.sh --repo <owner/repo> [<tag>]
repo="" ; tag=""
while [ $# -gt 0 ]; do
  case "$1" in
    --repo) repo="$2"; shift 2 ;;
    *) tag="$1"; shift ;;
  esac
done
[ -n "$repo" ] || { echo "usage: release-view.sh --repo <owner/repo> [<tag>]" >&2; exit 2; }
if [ -n "$tag" ]; then
  gh release view "$tag" --repo "$repo" --json tagName,name,isDraft,isPrerelease,publishedAt,assets
else
  gh release list --repo "$repo" --limit 10
fi
