#!/usr/bin/env bash
# scripts/ci/guard-process-intelligence-boundary.sh
#
# ggen must only emit process evidence, never analyze it -- that is
# wasm4pm-compat's/wasm4pm's job (see CLAUDE.md "Process Intelligence
# Boundary"). Adopting praxis-graphlaw makes its `chatman` module (an
# independent conformance/fitness engine) transitively reachable but not
# invoked; this guard keeps it that way. See
# docs/jira/v26.7.16/02-CROSS-REPO-DEPENDENCY-RISKS.md item 2.
#
# Scope note: `crates/praxis-core/` and `crates/praxis-graphlaw/` are excluded
# from the scan. `chatman` is DEFINED in praxis-graphlaw and legitimately used
# by praxis-core's own pre-existing Rail A/B admission wiring (both vendored
# upstream unmodified, per docs/jira/v26.7.16/02-CROSS-REPO-DEPENDENCY-RISKS.md
# item 2's own text: "the boundary holds only because ggen's own code never
# calls into chatman" -- ggen's own code being ggen-engine/ggen-cli/ggen-lsp/
# root src, not praxis-graphlaw's own internals testing itself). Excluding
# them here is what makes the guard test the actual boundary instead of
# flagging praxis-graphlaw's own test suite for testing its own module.
#
# `crates/bcinr-pddl/` and `crates/chicago-tdd-tools/` added to the exclusion
# (2026-07-19, v26.7.18 release hardening): both were vendored by PR #255
# (2026-07-17), after this guard was written, and were never added here --
# confirmed 2026-07-19 that every `bcinr_powl(_receipt)::` reference in either
# crate is inert, not ggen crossing the boundary: `chicago-tdd-tools/src/
# observability/receipt.rs`'s hit is inside a ```rust,ignore``` doc-comment
# example (never compiled); `bcinr-pddl/src/ground/dict.rs`'s hit is a bare
# `//!` prose mention, not an import; `bcinr-pddl/src/mfw/planner.rs` and
# `bcinr-pddl/tests/mfw_capacity2_fixture.rs` are both gated behind
# `#[cfg(feature = "mfw-planner")]` / `#![cfg(feature = "mfw-planner")]`, a
# feature PR #255 deliberately dropped from `bcinr-pddl/Cargo.toml`'s
# `[features]` table (see that file's own comment) -- with no such feature
# declared, this code can never compile into any build, default or otherwise.
set -euo pipefail
EXCLUDE_DIRS=(--exclude-dir=praxis-core --exclude-dir=praxis-graphlaw --exclude-dir=bcinr-pddl --exclude-dir=chicago-tdd-tools)

if grep -rn "${EXCLUDE_DIRS[@]}" --include="*.rs" "praxis_graphlaw::chatman" crates/ src/ 2>/dev/null; then
  echo "FAIL: praxis_graphlaw::chatman referenced above -- ggen must not cross the Process Intelligence Boundary." >&2
  exit 1
fi
if grep -rEn "${EXCLUDE_DIRS[@]}" --include="*.rs" '\buse[[:space:]]+bcinr_powl(_receipt)?\b|\bbcinr_powl(_receipt)?::' crates/ src/ 2>/dev/null; then
  echo "FAIL: direct bcinr_powl(_receipt):: reference found -- conformance/fitness analysis belongs in wasm4pm, never inline in ggen." >&2
  exit 1
fi

# Local DFG/conformance/fitness/precision/variant *discovery* logic is the same violation
# class as praxis_graphlaw::chatman/bcinr_powl(_receipt):: above, just implemented as inline
# SPARQL aggregation instead of a crate dependency -- see crates/ggen-graph/src/ocel/dfg.rs
# git history: `discover_dfg` used to compute directly-follows edges itself via a SPARQL
# `GROUP BY`/`COUNT(*)` aggregate query (a real, local process-mining discovery
# implementation, forbidden by CLAUDE.md's Process Intelligence Boundary table: "DFG
# discovery | wasm4pm-compat::dfg::discover_ocel_dfg | Any local discovery impl"). It was
# rewritten to a thin SPARQL `SELECT` (retrieval only, no aggregation) that hands the raw
# events to `wasm4pm_compat::dfg::discover_ocel_dfg`, the authorized native miner.
#
# The mechanical signal for "this SPARQL query IS a local discovery/conformance algorithm,
# not mere retrieval": a `GROUP BY` combined with a `COUNT(*)`/`COUNT (` aggregate in the
# same query. Flat `SELECT`s (even multi-variable, multi-triple-pattern ones) never need
# either -- aggregation only enters once you're counting transitions, occurrences, or
# variants, which is discovery, not retrieval. Scoped to ggen-graph/ggen-lsp/ggen-engine,
# the crates that talk to the OCEL-RDF triplestore; praxis-core/praxis-graphlaw are excluded
# for the same reason as above (their own internals, not ggen's boundary).
PI_SCAN_DIRS=(crates/ggen-graph/src crates/ggen-lsp/src crates/ggen-engine/src)
for dir in "${PI_SCAN_DIRS[@]}"; do
  [ -d "$dir" ] || continue
  hits=""
  while IFS= read -r f; do
    [ -z "$f" ] && continue
    if grep -qE 'COUNT[[:space:]]*\(' "$f" 2>/dev/null; then
      hits="${hits}${f}"$'\n'
    fi
  done < <(grep -rlE --include="*.rs" 'GROUP[[:space:]]+BY' "$dir" 2>/dev/null || true)
  if [ -n "$hits" ]; then
    echo "FAIL: SPARQL GROUP BY + COUNT(*) aggregation found in $dir -- this is local DFG/conformance/fitness/precision/variant discovery, forbidden by CLAUDE.md's Process Intelligence Boundary. Retrieve raw events with a flat SELECT and delegate discovery to wasm4pm_compat::dfg (or the equivalent wasm4pm-compat entry point)." >&2
    echo "$hits" >&2
    exit 1
  fi
done

echo "OK: no praxis_graphlaw::chatman, bcinr_powl(_receipt), or local SPARQL-aggregate DFG/conformance discovery references in the ggen workspace (outside praxis-core/praxis-graphlaw's own internals)."
