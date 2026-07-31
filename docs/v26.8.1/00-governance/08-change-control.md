# Change control

The v26.8.1 research corpus is revision-bound. Every research update must name the repository baseline, observed source paths, changed mappings, and affected sunset dispositions.

## Required controls

- no silent promotion of standing;
- no deletion of historical evidence required for recovery;
- no hand editing of generated indexes or authority-owned projections;
- no replacement of an executable verifier with prose;
- no weakening of negative fixtures, hooks, or claim-schema guards;
- no merge while the corpus count, path uniqueness, or machine-readable coverage checks fail.

## Review unit

A reviewable change is one bounded claim package: authority source, implementation path, positive witness, negative falsifier, replay evidence, legacy disposition, and updated coverage row. Broad narrative updates without those bindings remain research notes only.

## Real finding: the "10 gates" Definition-of-Done claim is stale (documentation-only drift)

Confirmed 2026-07-31 (G4 governance/system evidence pass) by reading `justfile`'s real
`pre-commit:` recipe line directly: it currently chains **11** dependencies —
`fmt-check check lint test-lib coherence-check guard-process-intelligence-boundary
guard-cheat-scan guard-claims-schema guard-pack-proofs guard-generation-hash-pin
guard-pack-count` — not 10. Three separate docs (`CLAUDE.md`'s "Definition of Done" row,
`.claude/rules/andon/signals.md`, `.claude/rules/README.md`) all still assert "10 gates", and
even `justfile`'s own inline comment directly above the `pre-commit:` line still says "(10
gates, in sequence, fail fast)" — the drift originates in the source-of-truth file's own
comment, not only its downstream docs. `guard-pack-count` (`scripts/ci/guard-pack-count.sh`) is
the 11th, uncounted gate.

This is real Contract Drift per `.claude/rules/coding-agent-mistakes.md` mistake class 5: a
governance claim about the authoritative proof-object gate chain no longer describes what
actually runs. It is executable-enforced (the gates themselves genuinely run and genuinely
gate), but the *count claim* is prose-only and currently wrong. Guarded going forward by
`crates/ggen-config/tests/governance_precommit_gate_count_test.rs`, which parses the real
`justfile` recipe line on every test run and asserts the current real count (11), with an
explicit `assert_ne!(gates.len(), 10, ...)` documenting the exact drift found. Correcting the
"10 gates" prose in `CLAUDE.md`/`.claude/rules/*` is out of this pass's file-ownership scope
(governance/system row evidence only) — flagged here as an open item for whichever phase owns
those files next.

## Real finding: governance rules that ARE enforced-in-code vs. documentation-only

Surveyed against this pass's own governance checkpoint requirements:

- **Executable source precedence / "ggen.toml has two schemas"** — enforced-in-code with a real
  drift guard: `crates/ggen-config/tests/schema_parity_test.rs` parses both
  `ggen_config::manifest::types::GgenManifest` and `ggen_engine::config::GgenConfig` via `syn`
  and fails if their known-shared top-level table names diverge. Not merely documented.
- **Typed refusal vocabulary** — enforced-in-code: `crates/praxis-core/src/refusal.rs`'s
  `RefusalCategory`/`RefusalScenario` are closed enums with exhaustive (non-wildcard) `match`
  arms in `category()`/`denial_lane()`, so an unhandled new variant is a compile error, not a
  silent gap. Not directly exercised by a test in this pass's own file-ownership scope
  (`crates/ggen-config/tests/`, `crates/ggen-cli/tests/`) since `praxis-core` belongs to a
  different agent's ownership boundary this phase; cite `packs/praxis-core-pack` (which
  generates a proof test asserting the taxonomy's literal values) as the existing evidence path.
- **Cryptographic-receipt signing-key precedence law** ("`GGEN_SIGNING_KEY` env var, else
  `.ggen/keys/signing.key`, else generate-and-persist") — enforced-in-code AND already
  positive/negative-witness tested in-crate: `crates/ggen-engine/src/keys.rs`'s own `#[cfg(test)]`
  module (verified by reading the file, 2026-07-31) covers the env-var path, the
  generate-on-absent path, a malformed-env-var hard-error path, and a
  never-generates-for-verify path. `resolve_signing_key`/`resolve_verifying_key` are
  `pub(crate)`, so no external black-box test from `ggen-config`/`ggen-cli` (this pass's owned
  test directories) can exercise them directly without widening that visibility — not done here
  since it wasn't necessary to prove the law is real; the existing in-crate tests are sufficient
  positive/negative evidence.
- **"10 gates" Definition-of-Done count** — documentation-only, and currently WRONG (see above):
  the gate chain itself is real and enforced (`just pre-commit` genuinely fails on any gate
  failure), but the specific count claim is stale prose with no generator or test keeping it in
  sync until this pass added one.
- **Workspace crate map completeness** — was documentation-only and silently incomplete before
  this pass (see `10-system/12-workspace-crate-map.md`); now guarded by
  `crates/ggen-config/tests/system_crate_map_parity_test.rs`.
