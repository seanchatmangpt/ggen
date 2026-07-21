#!/usr/bin/env bash
# guard-generation-hash-pin — hash-pinning guard for the generation ledger
# (.specify/generations.ttl, the G0..G3 self-manufacture ledger).
#
# Three checks, all over the raw Turtle (grep/awk over the ledger is the house
# pattern for guard scripts — see guard-publish-standing.sh, guard-pack-proofs.sh):
#
#   1. IDENTITY  — every `a gen:Generation` entry must carry at least one
#                  non-empty identity fact pinning what it manufactured:
#                  gen:commit (40-hex), gen:receiptChainHash (64-hex), or
#                  gen:buildASha256/gen:buildBSha256 (64-hex). Any such fact
#                  that IS present must be well-formed hex of the right length.
#   2. APPEND-ONLY — previously recorded hash facts never change. Every
#                  hash-bearing line in the baseline copy of the file
#                  (origin/main, falling back to HEAD when origin/main is
#                  unavailable, e.g. shallow CI clones) must appear verbatim
#                  in the working copy. New entries/facts may be appended;
#                  rewriting or deleting a recorded hash fails loudly,
#                  naming the exact line.
#   3. ORDERING  — the numbered chain G0 < G1 < ... must appear in strictly
#                  increasing index order in the file (monotonic chain).
#
# Exit non-zero with a BUILD_BROKEN: line naming the offending entry/line on
# any violation; print an ALIVE: summary otherwise.
set -euo pipefail

LEDGER="${GEN_LEDGER:-.specify/generations.ttl}"
BASELINE_REF="${GEN_BASELINE_REF:-origin/main}"

if [ ! -f "$LEDGER" ]; then
  echo "BUILD_BROKEN: $LEDGER not found"
  exit 1
fi

errors=0
fail() { echo "BUILD_BROKEN: $1"; errors=1; }

# Predicates that carry pinned hashes (read from the live file's vocabulary).
HASH40_PREDS='gen:commit'
HASH64_PREDS='gen:receiptChainHash|gen:receiptPrevChainHash|gen:buildASha256|gen:buildBSha256|gen:finalV1ChainHash|gen:firstV2ChainHash'
IDENTITY_PREDS='gen:commit|gen:receiptChainHash|gen:buildASha256|gen:buildBSha256'

# ── Check 1: identity + well-formedness ─────────────────────────────────────
# Split the file into per-subject generation blocks (subject line .. terminating '.')
subjects=$(grep -E '^gen:[A-Za-z0-9]+ a gen:Generation' "$LEDGER" | awk '{print $1}')
if [ -z "$subjects" ]; then
  fail "no 'a gen:Generation' entries found in $LEDGER"
fi

block_for() {
  # print the turtle block for subject $1 (from its declaration line to the
  # first line ending in ' .' — turtle statement terminator)
  awk -v subj="$1" '
    $0 ~ "^"subj" a gen:Generation" {inblock=1}
    inblock && /^[[:space:]]*#/ {next}   # comments never terminate a block
    inblock {print}
    inblock && /\.[[:space:]]*$/ && !/;[[:space:]]*$/ {exit}
  ' "$LEDGER"
}

for s in $subjects; do
  block=$(block_for "$s")
  if ! printf '%s\n' "$block" | grep -Eq "($IDENTITY_PREDS)[[:space:]]+\"[^\"]+\"" ; then
    fail "$s: no non-empty identity hash fact (need one of: ${IDENTITY_PREDS//|/, })"
  fi
  # well-formedness of any hash facts present in this block
  while IFS= read -r line; do
    [ -z "$line" ] && continue
    pred=$(printf '%s' "$line" | awk '{print $1}')
    val=$(printf '%s' "$line" | sed -n 's/.*"\([^"]*\)".*/\1/p')
    case "$pred" in
      gen:commit)
        printf '%s' "$val" | grep -Eq '^[0-9a-f]{40}$' \
          || fail "$s: $pred value '$val' is not 40-char lowercase hex" ;;
      *)
        printf '%s' "$val" | grep -Eq '^[0-9a-f]{64}$' \
          || fail "$s: $pred value '$val' is not 64-char lowercase hex" ;;
    esac
  done < <(printf '%s\n' "$block" | grep -E "^[[:space:]]*($HASH40_PREDS|$HASH64_PREDS)[[:space:]]" || true)
done

# ── Check 2: append-only vs baseline ────────────────────────────────────────
# The baseline is always read from git at the canonical repo-relative path,
# even when GEN_LEDGER points the working-copy check at another file (used by
# the sabotage self-test).
BASELINE_PATH="${GEN_BASELINE_PATH:-.specify/generations.ttl}"
baseline=""
if git rev-parse --verify -q "$BASELINE_REF" >/dev/null 2>&1; then
  baseline=$(git show "$BASELINE_REF:$BASELINE_PATH" 2>/dev/null || true)
fi
if [ -z "$baseline" ] && git rev-parse --verify -q HEAD >/dev/null 2>&1; then
  BASELINE_REF="HEAD"
  baseline=$(git show "HEAD:$BASELINE_PATH" 2>/dev/null || true)
fi
if [ -n "$baseline" ]; then
  while IFS= read -r hline; do
    [ -z "$hline" ] && continue
    if ! grep -Fq -- "$hline" "$LEDGER"; then
      fail "append-only violation vs $BASELINE_REF — previously recorded hash line changed or removed: '$(printf '%s' "$hline" | sed 's/^[[:space:]]*//')'"
    fi
  done < <(printf '%s\n' "$baseline" | grep -E "^[[:space:]]*($HASH40_PREDS|$HASH64_PREDS)[[:space:]]+\"" || true)
else
  echo "WARN: no baseline ($BASELINE_REF or HEAD) available — append-only check skipped"
fi

# ── Check 3: monotonic G-index ordering ─────────────────────────────────────
indices=$(grep -E '^gen:G[0-9]+ a gen:Generation' "$LEDGER" | sed -E 's/^gen:G([0-9]+) .*/\1/')
prev=-1
for i in $indices; do
  if [ "$i" -le "$prev" ]; then
    fail "chain ordering not monotonic: gen:G$i appears after gen:G$prev"
  fi
  prev=$i
done

if [ "$errors" -ne 0 ]; then
  exit 1
fi

n=$(printf '%s\n' "$subjects" | grep -c . || true)
echo "ALIVE: $n generation entries hash-pinned ($LEDGER; append-only vs $BASELINE_REF; G-chain monotonic)"
