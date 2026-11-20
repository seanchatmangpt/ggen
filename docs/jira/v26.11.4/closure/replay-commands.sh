#!/usr/bin/env bash
# replay-commands.sh — executable re-read block for the v26.11.4 closure.
# Per-repo identity + receipt census; tag list; manifest/court counts.
# Run from anywhere; read-only (no git transitions, no writes).
set -u

REPOS="/Users/sac/ggen /Users/sac/arcus /Users/sac/cgap /Users/sac/lsp-max /Users/sac/ggen-marketplace /Users/sac/smalt"

echo "== per-repo identity + receipt census =="
for repo in $REPOS; do
  echo "--- $repo"
  if [ ! -d "$repo/.git" ]; then echo "MISSING repo"; continue; fi
  git -C "$repo" rev-parse HEAD
  git -C "$repo" branch --show-current
  echo "dirty files: $(git -C "$repo" status --porcelain | wc -l | tr -d ' ')"
  found=0
  for d in docs/jira/v26.11.4/receipts receipts docs/dmedi-bb-curriculum/receipts; do
    if [ -d "$repo/$d" ]; then
      echo "receipts: $(ls "$repo/$d" | wc -l | tr -d ' ') ($d)"
      found=1
    fi
  done
  if [ "$found" -eq 0 ]; then echo "receipts: 0 (no receipts dir)"; fi
done

echo "== tags =="
for repo in $REPOS; do
  n=$(git -C "$repo" tag 2>/dev/null | wc -l | tr -d ' ')
  echo "--- $repo: $n tags"
  git -C "$repo" tag --sort=-creatordate 2>/dev/null | head -5
done

echo "== manifest/court counts =="
echo "ggen publish.yml lines: $(wc -l < /Users/sac/ggen/.github/workflows/publish.yml | tr -d ' ')"
echo "cgap publish.yml lines: $(wc -l < /Users/sac/cgap/.github/workflows/publish.yml | tr -d ' ')"
echo "ggen tera census tests: $(grep -c '#\[test\]' /Users/sac/ggen/crates/ggen-engine/tests/tera_whitespace_census.rs)"
echo "arcus court files: $(find /Users/sac/arcus/internal/arcus \( -name '*court*' -o -name '*_test.go' \) | grep -c court) (files matching *court* under internal/arcus)"
echo "smalt receipts: 0 (no docs/jira/v26.11.4/receipts; curriculum closure lives in docs/dmedi-bb-curriculum/: $(ls /Users/sac/smalt/docs/dmedi-bb-curriculum/receipts | wc -l | tr -d ' ') receipts)"
echo "smalt KD rows: $(grep -c '^| KD-\|^| PASS\|^| SIM' /Users/sac/smalt/posh/KNOWN_DIVERGENCES.md)"
echo "ggen KD rows: $(grep -c '^| ' /Users/sac/ggen/KNOWN_DIVERGENCES.md)"
echo "cgap KD rows: $(grep -c '^## KD' /Users/sac/cgap/docs/jira/v26.11.4/KNOWN_DIVERGENCES.md)"