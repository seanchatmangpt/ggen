# Receipt: p11-fmgen007 — TemplateSource::Pack resolution (FM-GEN-007 family)

Standing: ALIVE

- **Lane**: IMPLEMENTATION (FM-GEN-007 / TemplateSource::Pack)
- **Repo**: /Users/sac/ggen (canonical checkout; no git commands run)
- **Owned files touched**: `crates/ggen-engine/tests/template_source_pack_test.rs` (new, only file changed)
- **Files NOT touched (per contract)**: crates/ggen-abb-sbb/**, ggen-cli, all `lib/` code

## Locator analysis (contract item 1)

- `FM-GEN-007` emission site: `crates/ggen-engine/src/generation_rules.rs:1060-1079`
  (`resolve_template_source`, `TemplateSource::Git`/`Package` arms only).
- `TemplateSource` type: `crates/ggen-config/src/manifest/types.rs`
  (`TemplateSource::Pack { pack, output, file }`).
- **Decisive finding — case (b)**: this checkout (v26.10.x) ALREADY implements
  `TemplateSource::Pack` at `generation_rules.rs:1044-1059` via
  `resolve_pack_root` (:911) + `resolve_pack_file` (:952), resolving
  `pack = NAME` against the manifest's `[[packs]]` entry (`registry = "local"`,
  `path = ...`), then through the pack's `package.toml` `[pack.outputs]`
  table. Convention confirmed identical to what A2 witnessed in 26.10.11.
  The `[FM-GEN-007] ... not implemented` text at 26.7.59 predates this
  implementation; nothing was disabled or mis-gated here — the gap was the
  absence of a dedicated test pinning the capability.
- Partial remaining gap: `TemplateSource::Git`/`Package` variants are still
  typed FM-GEN-007 refusals by design (loud, remediation-bearing).

## Implementation (contract item 2)

No lib change needed. The minimal lawful action was the missing test court.

## Test court (contract item 3)

New file `crates/ggen-engine/tests/template_source_pack_test.rs`, 3 Chicago
tests through the public `ggen_engine::sync::sync` entry point (real
TempDir/SPARQL/Tera, no mocks):

1. `template_source_pack_renders_real_query_results_through_package_toml_outputs_table`
   — inline SPARQL query, `template = { pack = "lsp-max", output = "templates", file = "names.tmpl" }`
   against a vendored pack with `package.toml` `[pack.outputs]` mapping.
   Asserts exact rendered bytes (`alice\nbob\n`), exact `report.written`,
   and that the pack template binds into the receipt input closure under its
   real resolved path.
2. `template_source_pack_with_undeclared_pack_is_a_typed_refusal_not_a_silent_skip`
   — pack dir exists on disk but manifest never declares it; asserts typed
   refusal naming `ghost-pack` (FM-GEN-006 or E0014) and zero writes.
3. `template_source_for...non_local_registry_is_a_typed_refusal` —
   `registry = "marketplace"` (no path) → FM-GEN-006 "no local" refusal,
   zero writes.

## Verification (contract item 4)

- `cargo test -p ggen-engine --test template_source_pack_test`
  → **3 passed; 0 failed** (0.05s). ALIVE.
- Full `cargo test -p ggen-engine` → lib 232 passed / 0 failed;
  `generation_rules_e2e` 30/30; `template_source_pack_test` 3/3.
  **Pre-existing failures, not session-introduced** (session diff = one new
  test file, zero lib edits): 11 failures, all one root cause —
  `[FM-LAW-015]` EA admission gate (v26.10.x ABB/SBB Stage 2c) refusing
  syncs without `ea.graph.json`/`--skip-ea-admission` in tests predating
  the gate: `sync_e2e` 10 failed (sync_e2e.rs:106, :246, :402 …),
  `abb_sbb_datalog_admission_e2e::unbound_pack_port_refuses_sync` 1 failed.
  These sit in surfaces excluded by lane contract (abb-sbb/gate wiring) —
  hand-off to the lane owning FM-LAW-015 wiring or test fixtures.

## Standing

- TemplateSource::Pack capability: **ALIVE** (ran + passed this session,
  commands above).
- FM-GEN-007 for Git/Package variants: **UNSUPPORTED by design** (typed,
  loud, remediation-bearing) — tracked follow-up referenced in
  generation_rules.rs:1065.
- Pre-existing FM-LAW-015 test-fixture debt: **PARTIAL/BLOCKED** for this
  lane, cited above.
