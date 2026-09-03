# Ecosystem WIP Closure v26.8.16 — Overview

Index ticket for closing the residual work-in-progress surfaced by the 2026-08-16
three-round finish-WIP sweep across `/Users/sac/chatmangpt/ostar` and `/Users/sac/ggen`.
The sweep itself restored `ostar.manufacturing` (91 entries), took the ostar import sweep
from 443 OK / 8 FAIL to 466 OK / 0 FAIL, de-faked and then honestly greened
`vision_2030_e2e_proof.py` (passed=5 failed=0 on real evidence), fixed the unrdf generator
front-matter leak at source, and regenerated `mcp_tool_registry` to 17 real tools from the
ontology. Every claim in this ticket set was verified against the live filesystem in the
session that wrote it; commands and their real output are cited per ticket.

## Repo root paths (referenced throughout)

| Repo | Root path | Branch at time of writing |
|---|---|---|
| ostar | `/Users/sac/chatmangpt/ostar/` | `fix/ostar-round3-critical` |
| ggen | `/Users/sac/ggen/` | `agent/lifecycle-boundary-doc-comment` |

`ostar/vendors/` is out of scope for the entire ticket set — that work is discarded by
standing decision (2026-08-16).

## Tickets

1. [01-COMMIT-BOUNDARY](01-COMMIT-BOUNDARY.md) — commit the 62 uncommitted non-vendor
   ostar files and the 2 untracked ggen dirs; nothing in this ticket set matters if the
   work evaporates from the working trees.
2. [02-ONTOLOGY-IRI-DEFECT](02-ONTOLOGY-IRI-DEFECT.md) — mangled `Mcp:mcptool` IRI makes
   the next `pydantic-models` regeneration emit a SyntaxError; the checked-in file is fine
   but the regen path is a loaded gun.
3. [03-WVDA-TEST6-FALSE-DOCSTRING](03-WVDA-TEST6-FALSE-DOCSTRING.md) — one test's
   docstring claims a mechanism the test does not implement; constitution violation
   (fabricated evidence in prose form).
4. [04-MARKETPLACE-TEST-SUITE-DISABLED](04-MARKETPLACE-TEST-SUITE-DISABLED.md) — seven
   commented-out test modules in `ggen-cli/tests/marketplace/mod.rs`; repair or formally
   archive, no third state.
5. [05-MARKETPLACE-BRIDGE](05-MARKETPLACE-BRIDGE.md) — ggen's live `pack` CLI and ostar's
   `marketplace/` module never meet; decide and wire (or document non-goal).
6. [06-DOC-DRIFT-AND-MINOR](06-DOC-DRIFT-AND-MINOR.md) — stale "landed inert" comment,
   ignored `[profile]` block, inconsistent `env_file` tuple, orphaned
   `src/gstar_bootstrap_test.rs`, unwired `tests/rust-bootstrap/` crate.

## Definition of done for the set

- Both working trees committed (ticket 01), on branches, no force operations.
- `unrdf` full regeneration is idempotent: regenerate all rules, then
  `.venv/bin/python -c "import ostar"` sweep stays 466 OK / 0 FAIL (ticket 02 is the only
  known blocker).
- Zero test files whose docstrings describe behavior the test body does not perform
  (ticket 03).
- `crates/ggen-cli/tests/marketplace/mod.rs` contains no commented-out `pub mod` lines —
  modules either compile and run or are moved under an archive path with a dated note
  (ticket 04).
- Ticket 05 closed with either a working call path or a written non-goal decision.

## See Also

- `/Users/sac/ggen/docs/jira/v26.7.16/00-OVERVIEW.md` — the ggen-core replacement ticket
  set this one inherits context from
- `/Users/sac/chatmangpt/ostar/CLAUDE.md` — proof doctrine and truth-gate constitution
  binding all ostar-side work here
