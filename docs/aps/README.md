# APS Claims Ledger

Machine-readable standing ledger for ggen's release-relevant claims, in `claims.toml`.

## What this is

Each `[[claims]]` entry binds a **claim** (a bounded proposition about ggen, e.g. "the
`sync run` JTBD works end-to-end") to a **falsifier** (the exact runnable command that would
prove it false), an **evidence coordinate** (commit, date, method — evidence without a
coordinate is an anecdote), and a **standing** using this repo's existing no-overclaiming
vocabulary (`ALIVE` / `PARTIAL` / `BLOCKED` / `UNVERIFIED`).

Claims whose `gates` include `"publish"` gate the crates.io release: `just
guard-publish-standing` fails if any publish-gated claim is `BLOCKED` without an explicit
`exception_admitted_by` field naming who accepted the risk. A schema-only validation of this
file runs in `just pre-commit`; the full publish gate is meant to be run before any real
`cargo publish`.

## Provenance

The claim/falsifier/standing/coordinate shape is adapted from the Agile Protocol
Specification 2030 manuscript's normative core (six constitutional laws, minimum-conformance
5-tuple, claim templates), applied per its own adoption guidance: start with one working
claim–falsifier–evidence loop, not whole-organization encoding. Standing vocabulary is this
repo's pre-existing one (`~/.claude/rules/no-overclaiming-rust.md` lineage), not a new
parallel taxonomy.

## Maintaining it

- Update an entry's `evidence` when you actually re-run its falsifier — never on the
  assumption it would still pass.
- A stale coordinate is a warning, not a refutation; a missing falsifier is a schema error.
- Prose views of the same facts live in `README.md` (Known Limitations) and
  `docs/jira/2026-07-17-JTBD-VERIFICATION-DISCOVERED-BUGS.md`; if they disagree with this
  file, one of them is drift — fix the divergence, don't paper over it.
