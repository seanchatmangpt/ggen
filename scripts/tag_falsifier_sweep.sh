#!/usr/bin/env bash
# tag_falsifier_sweep.sh — CARRY-19 tag falsifier sweep.
#
# Verifies every commit SHA cited in a milestone's closure/ and receipts/ docs
# is reachable from a tag (or, pre-tag, from --at-ref <sha>).
#
# Usage:
#   scripts/tag_falsifier_sweep.sh <repo-path> <tag-or-ref>
#   scripts/tag_falsifier_sweep.sh <repo-path> --at-ref <sha>   # pre-tag rehearsal
#
# Method:
#   1. grep -oE '[0-9a-f]{7,40}' over closure/*.md and receipts/*.md
#   2. Filter to SHAs that resolve in THIS repo (git cat-file -t);
#      non-resolving SHAs are FOREIGN (cross-repo citations) — disclosed, skipped.
#   3. merge-base --is-ancestor <sha> <ref> for each resolving SHA.
#   4. UNREACHABLE SHAs are reported loudly; exit 1 if any.
set -euo pipefail

usage() { echo "usage: $0 <repo-path> <tag-or-ref> | $0 <repo-path> --at-ref <sha>" >&2; exit 2; }

REPO="${1:-}"
MODE="${2:-}"
REF="${3:-}"

[[ -n "$REPO" && -n "$MODE" ]] || usage
[[ -d "$REPO/.git" || -n "$(git -C "$REPO" rev-parse --git-dir 2>/dev/null)" ]] || { echo "FATAL: not a git repo: $REPO" >&2; exit 2; }

if [[ "$MODE" == "--at-ref" ]]; then
  [[ -n "$REF" ]] || usage
  git -C "$REPO" cat-file -e "$REF^{commit}" 2>/dev/null || { echo "FATAL: --at-ref does not resolve to a commit in $REPO: $REF" >&2; exit 2; }
  REF_LABEL="--at-ref $REF"
elif [[ "$MODE" != --* ]]; then
  REF="$MODE"
  git -C "$REPO" cat-file -e "$REF^{commit}" 2>/dev/null || { echo "FATAL: ref does not resolve to a commit in $REPO: $REF" >&2; exit 2; }
  REF_LABEL="$REF"
else
  usage
fi

DOCS_DIR="$REPO/docs/jira/v26.11.3"
[[ -d "$DOCS_DIR" ]] || { echo "FATAL: no docs dir: $DOCS_DIR" >&2; exit 2; }

# Shas cited inside this repo's own receipts/closure docs at HEAD, by path.
# A doc file that itself cites the SHA of its own commit is fine: we compare
# cited SHAs against the ref, never against the doc blob.
mapfile -t FILES < <(cd "$REPO" && find docs/jira/v26.11.3/closure docs/jira/v26.11.3/receipts -name '*.md' 2>/dev/null | sort)
if [[ ${#FILES[@]} -eq 0 ]]; then
  echo "FATAL: no closure/receipts markdown found under $DOCS_DIR" >&2
  exit 2
fi

declare -A SEEN=()
TOTAL=0; FOREIGN=0; REACHABLE=0; UNREACHABLE=0
FOREIGN_LIST=(); UNREACHABLE_LIST=()

echo "== tag falsifier sweep: repo=$REPO ref=$REF_LABEL docs=${#FILES[@]} files =="

for f in "${FILES[@]}"; do
  # Extract candidate hex strings, then keep only full-token-ish 7-40 hex runs
  # surrounded by non-hex boundaries (grep -oE on the raw text).
  while IFS= read -r sha; do
    [[ -n "$sha" ]] || continue
    [[ -n "${SEEN[$sha]:-}" ]] && continue
    SEEN[$sha]=1
    TOTAL=$((TOTAL + 1))
    # Does it resolve in this repo at all?
    if ! git -C "$REPO" cat-file -t "$sha" >/dev/null 2>&1; then
      FOREIGN=$((FOREIGN + 1))
      FOREIGN_LIST+=("$sha ($f)")
      continue
    fi
    if git -C "$REPO" merge-base --is-ancestor "$sha" "$REF" 2>/dev/null; then
      REACHABLE=$((REACHABLE + 1))
    else
      UNREACHABLE=$((UNREACHABLE + 1))
      UNREACHABLE_LIST+=("$sha ($f)")
      echo "UNREACHABLE: $sha cited in $f"
    fi
  done < <(git -C "$REPO" show "HEAD:$f" 2>/dev/null \
      | grep -oE '[0-9a-fA-F]{7,40}' | tr 'A-F' 'a-f' \
      | grep -vE '^[0-9]+$' \
      | sort -u || true)
done

echo
echo "== summary =="
echo "cited SHAs scanned: $TOTAL"
echo "reachable from $REF_LABEL: $REACHABLE"
echo "foreign (not in this repo, skipped): $FOREIGN"
echo "UNREACHABLE: $UNREACHABLE"
if [[ $FOREIGN -gt 0 ]]; then
  echo "-- foreign SHAs (per-repo disclosure) --"
  for s in "${FOREIGN_LIST[@]}"; do echo "  FOREIGN: $s"; done
fi
if [[ $UNREACHABLE -gt 0 ]]; then
  echo "FAIL: $UNREACHABLE cited SHA(s) not reachable from $REF_LABEL" >&2
  exit 1
fi
echo "PASS: all in-repo cited SHAs reachable from $REF_LABEL"
