# XD-13: fmt sweep — XA-1 CI root causes 1+6

Date: 2026-11-04 · Lane: XD-13 · Branch: v26-11-3-wave (PR #824) · Status: ALIVE (uncommitted, coordinator sweeps)

## Task

Root causes 1+6 of the 19-failing-check CI state: repo-wide rustfmt drift + two whitespace diffs.

## Method

CI pins rustfmt via `rust-toolchain.toml` → `channel = "nightly-2026-06-22"`, components
`["rustfmt", "clippy"]`, provisioned by `.github/actions/setup-ggen-build`. Local `cargo fmt`
resolves the same pinned toolchain via rustup, so the local run matches CI byte-for-byte. 9
workflows gate on `cargo fmt --all -- --check` (fortune5-required-capabilities,
tai-enterprise-rebuild, certification-assist-v26-7-30, automatic-autonomic-v26-7-31,
clap-noun-verb-zero-code, frontmatter-best-practices, gbb-kernel, ecosystem-alive-generated-command-format,
mfw-gmrw-offline-toolchain-adjacent fmt gates).

## Commands / exits

- `cargo fmt --all` — exit 0, 83 files reformatted
- `perl -i -pe 's/[ \t]+$//' docs/jira/v26.11.3/receipts/p11-fmgen007.md` — exit 0 (trailing space on line 59 removed)
- `cargo fmt --all -- --check` — exit 0 (FMT-CHECK-PASS)
- `git diff --check` — exit 0 (clean; also proves the abb_sbb_stage2c_wiring_e2e.rs:218 EOF blank line is gone — file now ends `}\n` exactly)
- `cargo check --workspace` — exit 0, `Finished dev profile in 15.70s`

## Files changed by this lane

83 fmt-reformatted files (below) + `docs/jira/v26.11.3/receipts/p11-fmgen007.md`
(whitespace-only) + the reflow-only share of `crates/ggen-engine/src/keys.rs` (see disclosures):

- `crates/ggen-abb-sbb/examples/validate_ea_graph.rs`
- `crates/ggen-abb-sbb/src/depgraph.rs`
- `crates/ggen-abb-sbb/src/generated/refusals.rs`
- `crates/ggen-abb-sbb/tests/depgraph_hardening_test.rs`
- `crates/ggen-abb-sbb/tests/ea_projection_test.rs`
- `crates/ggen-abb-sbb/tests/falsifiers.rs`
- `crates/ggen-abb-sbb/tests/kernel_highs_courts.rs`
- `crates/ggen-cli/src/cmds/ea/admit.rs`
- `crates/ggen-cli/src/cmds/ea/manufacture.rs`
- `crates/ggen-cli/src/cmds/ea/plan.rs`
- `crates/ggen-cli/src/cmds/ea/replay.rs`
- `crates/ggen-cli/src/cmds/ea/verify_receipt.rs`
- `crates/ggen-cli/src/cmds/pack.rs`
- `crates/ggen-cli/src/cmds/sjira.rs`
- `crates/ggen-cli/tests/cli_sjira_test.rs`
- `crates/ggen-cli/tests/ea_verbs_test.rs`
- `crates/ggen-config/tests/ea_graph_schema_parity_test.rs`
- `crates/ggen-engine/src/blast_radius.rs`
- `crates/ggen-engine/src/envelope.rs`
- `crates/ggen-engine/src/error.rs`
- `crates/ggen-engine/src/sync.rs`
- `crates/ggen-engine/src/transactional.rs`
- `crates/ggen-engine/src/verbs/sync.rs`
- `crates/ggen-engine/tests/abb_sbb_stage2c_wiring_e2e.rs`
- `crates/ggen-engine/tests/annotated_pack_sync_smoke.rs`
- `crates/ggen-engine/tests/blast_radius_gate_test.rs`
- `crates/ggen-engine/tests/blast_radius_scip_test.rs`
- `crates/ggen-engine/tests/combinatorial_matrix.rs`
- `crates/ggen-engine/tests/composed_packs_e2e.rs`
- `crates/ggen-engine/tests/composition_plan_feed_test.rs`
- `crates/ggen-engine/tests/config_schema_dispatch_e2e.rs`
- `crates/ggen-engine/tests/constitution_obligation_e2e.rs`
- `crates/ggen-engine/tests/dataset_bridge_e2e.rs`
- `crates/ggen-engine/tests/datatype_coercion_e2e.rs`
- `crates/ggen-engine/tests/determinism_independent_reload_e2e.rs`
- `crates/ggen-engine/tests/determinism_query_reexecution_e2e.rs`
- `crates/ggen-engine/tests/envelope_caps_e2e.rs`
- `crates/ggen-engine/tests/envelope_otel_span_test.rs`
- `crates/ggen-engine/tests/envelope_receipt_binding_test.rs`
- `crates/ggen-engine/tests/fm_registry_parity_test.rs`
- `crates/ggen-engine/tests/full_loop_ocel_v2_validation_e2e.rs`
- `crates/ggen-engine/tests/gall_checkpoint_001_replay_e2e.rs`
- `crates/ggen-engine/tests/generation_rules_e2e.rs`
- `crates/ggen-engine/tests/generation_rules_typed_causes_e2e.rs`
- `crates/ggen-engine/tests/graphlaw_e2e.rs`
- `crates/ggen-engine/tests/manifest_diagnostic_codes_evidence_test.rs`
- `crates/ggen-engine/tests/multi_template_determinism.rs`
- `crates/ggen-engine/tests/ontology_imports_e2e.rs`
- `crates/ggen-engine/tests/pack_lock_determinism_court.rs`
- `crates/ggen-engine/tests/projection_determinism_test.rs`
- `crates/ggen-engine/tests/reasoner_independence_e2e.rs`
- `crates/ggen-engine/tests/receipt_chain_rule_test.rs`
- `crates/ggen-engine/tests/receipt_chain_scale_stress.rs`
- `crates/ggen-engine/tests/receipt_genesis_ceiling_e2e.rs`
- `crates/ggen-engine/tests/receipt_head_vs_log_test.rs`
- `crates/ggen-engine/tests/receipt_signing_evidence_test.rs`
- `crates/ggen-engine/tests/rules_n3_sync_e2e.rs`
- `crates/ggen-engine/tests/sync_closure_golden_test.rs`
- `crates/ggen-engine/tests/template_source_pack_test.rs`
- `crates/ggen-engine/tests/transactional_edits_test.rs`
- `crates/ggen-engine/tests/verbs_handlers_test.rs`
- `crates/ggen-engine/tests/wasm4pm_facts_e2e.rs`
- `crates/ggen-lsp/src/diagnostics_rules.rs`
- `crates/ggen-lsp/src/handlers/mod.rs`
- `crates/ggen-lsp/src/handlers/moniker.rs`
- `crates/ggen-lsp/src/handlers/semantic_tokens.rs`
- `crates/ggen-lsp/src/lib.rs`
- `crates/ggen-lsp/src/moniker.rs`
- `crates/ggen-lsp/src/server.rs`
- `crates/ggen-lsp/src/tokens.rs`
- `crates/ggen-lsp/tests/diagnostics_rules_test.rs`
- `crates/ggen-lsp/tests/handler_wiring_test.rs`
- `crates/ggen-lsp/tests/lib_wiring_test.rs`
- `crates/ggen-lsp/tests/lsp_protocol_test.rs`
- `crates/ggen-lsp/tests/moniker_test.rs`
- `crates/ggen-lsp/tests/tokens_delta_stress.rs`
- `crates/ggen-lsp/tests/tokens_delta_test.rs`
- `crates/ggen-marketplace/src/marketplace/install.rs`
- `crates/ggen-marketplace/src/packs_registry/composer.rs`
- `crates/ggen-scip/src/emit.rs`
- `crates/ggen-scip/src/query.rs`
- `crates/ggen-scip/tests/scip_emit_query_test.rs`
- `crates/ggen-scip/tests/scip_ontology_test.rs`

## Exclusions / disclosures

- `crates/ggen-engine/src/keys.rs` was dirty at lane start (XD-8's uncommitted owner-only
  permission gate, 153+/22-). rustfmt reformatted that new code; the content diff is XD-8's,
  the reflow is this lane's. Not committed here.
- Pre-existing dirty, untouched by fmt: `CHANGELOG.md`, `Cargo.lock`,
  `crates/ggen-engine/Cargo.toml`, `docs/jira/v26.11.3/closure/v26.11.3-closure.md`,
  `docs/jira/v26.11.3/receipts/cl-close-2.md` — not this lane's diff.
- No XD-11 sync_surface collision: no other lane was mid-edit on the touched files during
  this lane's window (initial `git status` diffed against post-fmt status showed the only
  pre-existing dirty set above).
- Nothing committed — coordinator sweeps.

## CI checks unblocked

- 9 × `cargo fmt --all -- --check` formatting gates listed above
- whitespace-lint checks via `git diff --check` gates
- any check compiling the reformatted test files (fmt-only changes, no semantic diff)

## Standing

ALIVE: all three verification gates exited 0 on the live tree this session.
