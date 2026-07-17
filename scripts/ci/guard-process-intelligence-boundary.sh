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
set -euo pipefail
EXCLUDE_DIRS=(--exclude-dir=praxis-core --exclude-dir=praxis-graphlaw)

if grep -rn "${EXCLUDE_DIRS[@]}" --include="*.rs" "praxis_graphlaw::chatman" crates/ src/ 2>/dev/null; then
  echo "FAIL: praxis_graphlaw::chatman referenced above -- ggen must not cross the Process Intelligence Boundary." >&2
  exit 1
fi
if grep -rEn "${EXCLUDE_DIRS[@]}" --include="*.rs" '\buse[[:space:]]+bcinr_powl(_receipt)?\b|\bbcinr_powl(_receipt)?::' crates/ src/ 2>/dev/null; then
  echo "FAIL: direct bcinr_powl(_receipt):: reference found -- conformance/fitness analysis belongs in wasm4pm, never inline in ggen." >&2
  exit 1
fi
echo "OK: no praxis_graphlaw::chatman or bcinr_powl(_receipt) references in the ggen workspace (outside praxis-core/praxis-graphlaw's own internals)."
