# sbb-capability-density-pack — ORPHANED / SUPERSEDED

**Status: disconnected legacy artifact. Do not treat this pack as the live SBB capability-density
implementation.**

## What this is

This directory holds an early, pack-scaffold design for the SBB (Solution Building Block)
capability-density feature: a JSON Schema (`schema/sbb-capability-manifest.schema.json`) and a
Tera report template (`templates/capability-density-report.md.tera`), declared in `pack.toml`.

## Why it is orphaned (verified 2026-08-03)

The feature was actually shipped as a direct Rust CLI implementation,
`crates/ggen-cli/src/cmds/sbb/` (`ggen sbb schema|inspect|validate|distribution|receipt|replay`),
which:

- hardcodes the manifest shape as native Rust structs (`Manifest`, `Sbb`, `Delta`, `Evidence`,
  ...) validated by `serde` deserialization plus custom logic in `evaluation.rs`/`receipts.rs` —
  it never loads or parses this pack's `schema/sbb-capability-manifest.schema.json` at runtime
  (confirmed: no `jsonschema` dependency, no `include_str!`/file read of that path anywhere in
  `crates/ggen-cli/src/cmds/sbb/`);
- builds its density report directly as a `serde_json::Value` in Rust, never rendering
  `templates/capability-density-report.md.tera` (confirmed: no `tera`/`Tera` reference anywhere in
  `crates/ggen-cli/src/cmds/sbb/`, even though `ggen-cli` depends on the `tera` crate for other,
  unrelated templates);
- reads its ontology/query inputs from the repository root — `./ontology/sbb-capability-density.ttl`,
  `./ontology/sbb-capability-density.shacl.ttl`, `./queries/sbb-capability-density.rq` — not from
  anything under this pack directory.

Confirmed zero consumers of this pack directory:

- no `ggen.toml` anywhere in the repo references `sbb-capability-density-pack`
  (`grep -rln "sbb-capability-density" --include="ggen.toml" .` → empty);
- no `.rs` file references it (`grep -rln "sbb-capability-density\|sbb_capability_density"
  --include="*.rs" .` → empty);
- no CI workflow references it (`grep -rl "sbb-capability-density-pack" .github` → empty);
- it is absent from the generated Pack Inventory in `.claude/rules/architecture.md` and from
  `rf:Pack` facts in `.specify/repo-facts.ttl` (it is only counted in the raw on-disk
  `rf:packCount` total, not modeled as an individual fact);
- the only other reference anywhere in the repo is this pack's own architecture doc,
  `docs/architecture/SBB-CAPABILITY-DENSITY-ARD-PRD-v26.8.3.md`, which documents the feature
  actually implemented at `crates/ggen-cli/src/cmds/sbb/` (its "Product surface" line names the
  exact `ggen sbb <verb>` commands that live there), not this pack.

Both this pack (`f829abe47`, 2026-08-02 01:22) and the live Rust implementation
(`e01058ec3`, 2026-08-02 01:39, later folded into `7ab4ba968`/#557) were added the same day; the
pack scaffold was an abandoned first draft, not a currently-maintained alternate surface.

## What to do instead

For the live SBB capability-density feature, see:

- `crates/ggen-cli/src/cmds/sbb/` (`mod.rs`, `evaluation.rs`, `receipts.rs`, `tests.rs`)
- `ontology/sbb-capability-density.ttl`, `ontology/sbb-capability-density.shacl.ttl`
- `queries/sbb-capability-density.rq`
- `docs/architecture/SBB-CAPABILITY-DENSITY-ARD-PRD-v26.8.3.md`

## Follow-up (not done in this pass)

The clean terminal fix is to delete this directory outright and decrement `rf:packCount` in
`.specify/repo-facts.ttl` (checked by `scripts/ci/guard-pack-count.sh` in `just pre-commit`) in
the same change. That was deferred here because `.specify/repo-facts.ttl` had a large, unrelated,
in-flight uncommitted edit (crate-map/gate-count facts) at the time of this fix, and editing the
same file for `rf:packCount` risked conflicting with it. Once that edit lands, remove this
directory and update the count fact together.
