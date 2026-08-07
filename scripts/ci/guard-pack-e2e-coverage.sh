#!/usr/bin/env bash
# guard-pack-e2e-coverage.sh — reports which packs/* with real testable
# surface (gates/*.rq or templates/*.tmpl) have zero e2e proof anywhere in
# the workspace, so the gap is a real, regenerable number instead of an
# unasked question.
#
# RATCHET, not a full hard-fail: hard-fails ONLY if the real uncovered count
# EXCEEDS the checked-in baseline (.specify/pack-e2e-coverage-baseline.txt)
# -- a pack lost coverage, or a new pack shipped with real testable surface
# and none. It does NOT require the current gap (real count re-verified
# this pass: 15 of 81 packs) to close before passing -- several of those 15
# are legitimately blocked on real per-pack domain-ontology work (their
# gates correctly refuse an empty consumer ontology; one has a genuine
# non-determinism bug on repeat sync), not something a stub generator can
# close. Demanding all 15 close before this gate could ever pass would just
# make pre-commit red for a gap this pass explicitly chose not to attempt --
# the same reasoning `guard-cheat-scan`'s documented ~464-finding exception
# already applies, but as a ratchet instead of an always-0-exit report: the
# count may shrink (lowering the baseline is a conscious, separate edit,
# same discipline `guard-pack-count.sh` applies to `rf:packCount`), and may
# never silently grow.
#
# "Covered" means either:
#   (a) the pack's directory name appears as a literal double-quoted string
#       somewhere in crates/ggen-engine/tests/*.rs (e.g. `.join("gh-terraform
#       -pack")` / `packs_dir().join("tai-enterprise-rebuild-pack")`), or
#   (b) the pack name appears in an examples/*/ggen.toml [packs] table for
#       one of guard-pack-proofs.sh's own CONSUMERS.
#
# (a) is content-based, not filename-inference, deliberately: an earlier
# version of this script guessed the test filename by mechanically
# underscoring the pack name (`<pack>_pack_e2e.rs`), which double-appended
# "_pack" for every pack whose directory name already ends in "-pack" (all
# of them) and completely missed real, differently-named files like
# `tai_rebuild_pack_e2e.rs` (covers `tai-enterprise-rebuild-pack`) and
# `gall_automation_pack_e2e.rs` (a SECOND file covering `gall-core-pack`).
# That version false-flagged `gh-terraform-pack` (57 templates) and 7 other
# packs proven covered the same session this script was written -- caught
# by actually running it, not by inspection. Content-based grep has no
# filename-shape assumption to violate.
#
# Prints an ALIVE: summary line plus, if any gap exists, a PARTIAL: line
# with the count and the uncovered pack names sorted by templates/*.tmpl
# count descending (highest-surface packs first). Exits 0 unless the real
# count exceeds the baseline file, in which case it prints BUILD_BROKEN:
# and exits 1.
set -euo pipefail

cd "$(git rev-parse --show-toplevel)"

PACKS_DIR="packs"
TESTS_DIR="crates/ggen-engine/tests"
GUARD_PACK_PROOFS="scripts/ci/guard-pack-proofs.sh"
BASELINE_FILE=".specify/pack-e2e-coverage-baseline.txt"

if [ ! -d "$PACKS_DIR" ]; then
  echo "BUILD_BROKEN: $PACKS_DIR directory not found"
  exit 1
fi

# Consumers examples/*-verify covers, read from guard-pack-proofs.sh's own
# CONSUMERS array rather than duplicated here -- one source of truth.
mapfile -t CONSUMERS < <(
  awk '/^CONSUMERS=\(/{flag=1; next} /^\)/{flag=0} flag' "$GUARD_PACK_PROOFS" \
    | grep -oE '"[^"]+"' | tr -d '"'
)

# Pack names referenced by any CONSUMERS' ggen.toml [packs] table.
covered_via_examples=()
for consumer in "${CONSUMERS[@]}"; do
  manifest="$consumer/ggen.toml"
  [ -f "$manifest" ] || continue
  while IFS= read -r name; do
    covered_via_examples+=("$name")
  done < <(grep -oE '^[a-zA-Z0-9_-]+[[:space:]]*=[[:space:]]*\{' "$manifest" \
              | sed -E 's/[[:space:]]*=.*$//')
done

is_covered_via_examples() {
  local pack="$1"
  for name in "${covered_via_examples[@]:-}"; do
    [ "$name" = "$pack" ] && return 0
  done
  return 1
}

uncovered=()
for pack_dir in "$PACKS_DIR"/*/; do
  pack="$(basename "$pack_dir")"
  gate_count="$( { find "$pack_dir/gates" -maxdepth 1 -name '*.rq' 2>/dev/null || true; } | wc -l | tr -d ' ')"
  tmpl_count="$( { find "$pack_dir/templates" -maxdepth 1 -name '*.tmpl' 2>/dev/null || true; } | wc -l | tr -d ' ')"
  [ "$gate_count" -gt 0 ] || [ "$tmpl_count" -gt 0 ] || continue

  if grep -rlq "\"$pack\"" "$TESTS_DIR"/*.rs 2>/dev/null; then
    continue
  fi
  if is_covered_via_examples "$pack"; then
    continue
  fi
  uncovered+=("$tmpl_count $pack")
done

total_packs="$(find "$PACKS_DIR" -mindepth 1 -maxdepth 1 -type d | wc -l | tr -d ' ')"
uncovered_count="${#uncovered[@]}"

if [ ! -f "$BASELINE_FILE" ]; then
  echo "BUILD_BROKEN: $BASELINE_FILE not found -- run this script once, write its real uncovered count into that file, and commit it"
  exit 1
fi
baseline="$(tr -d '[:space:]' < "$BASELINE_FILE")"
if ! [[ "$baseline" =~ ^[0-9]+$ ]]; then
  echo "BUILD_BROKEN: $BASELINE_FILE does not contain a plain integer (got: $baseline)"
  exit 1
fi

if [ "$uncovered_count" -eq 0 ]; then
  echo "ALIVE: guard-pack-e2e-coverage: all packs with testable surface have e2e coverage ($total_packs packs total)"
  exit 0
fi

echo "PARTIAL: guard-pack-e2e-coverage: $uncovered_count of $total_packs packs have real testable surface (gates/*.rq or templates/*.tmpl) but no e2e coverage anywhere:"
printf '%s\n' "${uncovered[@]}" | sort -rn | while read -r count name; do
  echo "  - $name (templates: $count)"
done

if [ "$uncovered_count" -gt "$baseline" ]; then
  echo "BUILD_BROKEN: guard-pack-e2e-coverage: real uncovered count ($uncovered_count) exceeds the checked-in baseline ($baseline, from $BASELINE_FILE) -- a pack lost its e2e coverage, or a new pack shipped real testable surface (gates/*.rq or templates/*.tmpl) with none. Add real coverage or, if this regression is deliberate and accepted, raise $BASELINE_FILE to match."
  exit 1
fi
if [ "$uncovered_count" -lt "$baseline" ]; then
  echo "NOTE: real uncovered count ($uncovered_count) is now BELOW the baseline ($baseline) -- lower $BASELINE_FILE to $uncovered_count to ratchet the floor down (not done automatically)."
fi
exit 0
