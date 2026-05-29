# FAKE / STUB / PLACEHOLDER INVENTORY LEDGER

**Artifact type:** Receipt (triage ledger)
**Date:** 2026-05-29
**Source detector:** `scripts/find-fakes.sh` (MED + LOW sections)
**ggen version:** v26.5.28
**Method:** Every entry below was triaged by OPENING the cited file at the cited
line and reading the surrounding code, then verifying live-path reachability with
`grep`/LSP-style call-site tracing. Nothing is listed that was not read.

---

## Method & caveats

1. **Evidence-first.** Each row cites `file:line` that was actually opened. Where a
   row asserts "no live caller," that claim was checked with a workspace-wide
   `grep` for the symbol/function and the absence of a `#[verb]`/`pub use`/CLI
   call site.

2. **Dormant-crate caveat (CRITICAL).** Only 15 crates are real workspace members
   (see `CLAUDE.md`). These five directories live on-disk under `crates/` but are
   **NOT** `Cargo.toml` members and **do not compile**:
   `genesis-construct8`, `genesis-lockchain`, `genesis-wasm-shell`,
   `ggen-membrane`, `ggen-projection`.
   Findings inside them are **not live-path** and carry no severity. The hardened
   detector now reports them under a separate `INFO — dormant (not compiled)`
   section.

3. **"Live-path" definition.** A finding is LIVE-PATH-MUST-FIX only if it is in a
   workspace-member crate AND reachable from a real entry point (a `#[verb]` CLI
   command, an MCP/A2A handler, or a function transitively called by one) AND it
   returns success or a plausible value WITHOUT doing the work it claims
   (fake-success / fail-open / contract drift). "pub mod" alone is not enough — a
   module with no live caller is dead-but-compiled, classified TEST-OR-BENIGN.

4. **Classifications (exactly one per finding):**
   - **LIVE-PATH-MUST-FIX** — real Oracle Gap on a reachable path.
   - **DORMANT** — inside one of the five non-compiled crates.
   - **TEST-OR-BENIGN** — under `#[cfg(test)]`/tests; OR a legitimate pattern
     (constructor field filled later; honest fail-closed `Err`; deliberate
     `Placeholder` type; template token; doc comment; unwired/dead module).

---

## Triage table

### MED — fabricated success / simulated work / fake receipts

| file:line | pattern | classification | live-path? | disposition / why |
|---|---|---|---|---|
| `crates/genesis-construct8/src/bin/genesis8.rs:245` | "simulated for M1" | DORMANT | N | Non-compiled crate. |
| `crates/genesis-construct8/src/replay.rs:146` | "For now, count verification" | DORMANT | N | Non-compiled crate. |
| `crates/genesis-lockchain/src/quorum.rs:157` | "Mock implementation: simulate peer voting" | DORMANT | N | Non-compiled crate. |
| `crates/genesis-lockchain/src/quorum.rs:258` | "For now, with no peers" | DORMANT | N | Non-compiled crate. |
| `crates/genesis-lockchain/src/storage.rs:263` | "For now, we skip export" | DORMANT | N | Non-compiled crate. |
| `crates/ggen-a2a-mcp/src/a2a_generated/port.rs:571` | "simulate it being connected" | TEST-OR-BENIGN | N | Inside `#[cfg(test)]` test body. |
| `crates/ggen-a2a-mcp/src/a2a_generated/task.rs:189` | `DefaultTaskExecutor` "simulates task execution" (`execute()` sleeps + returns "completed") | TEST-OR-BENIGN | N | Generated scaffold; NOT in `pub use` re-export list, zero callers outside its file — unwired, no live MCP/A2A dispatch routes to it. |
| `crates/ggen-a2a-mcp/src/a2a/ggen_construct.rs:331` | `!json.contains("rcpt-simulated")` | TEST-OR-BENIGN | N | Anti-fake test ASSERTION (verifies output has no fabricated receipt). |
| `crates/ggen-cli/src/cmds/wizard.rs:449` | "For now, just add a next step" → prints "Initial sync completed" without syncing | **LIVE-PATH-MUST-FIX** | **Y** | `#[verb("wizard","root")]` (line 238) → `perform_wizard` (381). When `!skip_sync` it inserts "Initial sync completed" into `next_steps` and returns `status:"success"` having run NO sync. Decorative completion. |
| `crates/ggen-cli/src/conventions/watcher.rs:128` | "regenerate all templates" | TEST-OR-BENIGN | N | Real conservative plan (`planner.plan()`); watcher not wired to any verb. |
| `crates/ggen-cli/src/conventions/watcher.rs:159` | "return all templates" | TEST-OR-BENIGN | N | `#[allow(dead_code)]` `find_affected_templates`; unused. |
| `crates/ggen-cli/src/conventions/watcher.rs:177` | `regenerate_template` "just log it" + `Ok(())` | TEST-OR-BENIGN | N | No live caller (zero refs outside file); watcher module not invoked by any verb. |
| `crates/ggen-cli/src/pack_install.rs:55` | `create_default_repository` returns `TestRepository{}` as "production" repo | TEST-OR-BENIGN | N | `PackInstaller` has zero `#[verb]` callers. Live `ggen pack install` uses `ggen_core::domain::packs::install::install_pack` (cmds/pack.rs:117) which writes real `.ggen/packs.lock`. Dead-but-compiled module. |
| `crates/ggen-cli/src/validation/mod.rs:63` | "return known commands" (hardcoded list) | TEST-OR-BENIGN | N | Compile-time command-registry helper; hardcoded list is the spec, not a faked runtime result. |
| `crates/ggen-cli/src/validation/mod.rs:194` | "just test the structure" | TEST-OR-BENIGN | N | Inside a `#[test]`. |
| `crates/ggen-config/src/config/lock_manager.rs:374` | "just test the structure is sound" | TEST-OR-BENIGN | N | Inside a `#[test]`. |
| `crates/ggen-config/src/receipt/receipt_impl.rs:55` | `signature: String::new()` | TEST-OR-BENIGN | N | Constructor; field filled by `.sign()` (line 73-77). Verify rejects empty sig. |
| `crates/ggen-config/src/receipt/receipt_impl.rs:145` | `signature: String::new()` | TEST-OR-BENIGN | N | `signing_message()` deliberately zeroes signature before hashing the body. |
| `crates/ggen-core/src/parts_foundry/adapter_generator.rs:35` | "generate stub adapter code" | TEST-OR-BENIGN | N | `parts_foundry` is `pub mod` but has zero CLI/MCP callers (experimental, unwired). |
| `crates/ggen-core/src/parts_foundry/part_signer.rs:50` | "use hash as placeholder signature" (`sig_{hash}`, `pk_stub_verifying_key`) | TEST-OR-BENIGN | N | Same unwired `parts_foundry`; not reachable from any live verb. Flag for future activation. |
| `crates/ggen-core/src/parts_foundry/part_signer.rs:148` | `signature: String::new() // Invalid` | TEST-OR-BENIGN | N | Inside `#[cfg(test)]` `test_verify_invalid_signature`. |
| `crates/ggen-core/src/prevention/state_machine.rs:280` | "For now, placeholder" (`discover_templates` returns fabricated example) | TEST-OR-BENIGN | N | `prevention` module is NOT declared anywhere (`grep '(pub )?mod prevention'` → none). Orphaned source, not compiled into the crate. |
| `crates/ggen-core/src/prevention/state_machine.rs:290` | "For now, placeholder" (`validate_template`) | TEST-OR-BENIGN | N | Same orphaned, undeclared module. |
| `crates/ggen-core/src/prevention/state_machine.rs:301` | "For now, placeholder" (`render_template` returns content verbatim) | TEST-OR-BENIGN | N | Same orphaned, undeclared module. |
| `crates/ggen-core/src/merge/mod.rs:401` | "simple strategy-based merge" | TEST-OR-BENIGN | N | Returns real content per strategy; "full diff3" is a future note, not a fake. |
| `crates/ggen-core/src/merge/mod.rs:427` | "fall back to generated wins" | TEST-OR-BENIGN | N | Interactive strategy degrades to a real, defined result. |
| `crates/ggen-core/src/pipeline_engine/passes/normalization.rs:388` | "focus on the pass result" | TEST-OR-BENIGN | N | Honest scope comment; real pass result returned. |
| `crates/ggen-core/src/pack_resolver.rs:413` | "basic conflict detection" | TEST-OR-BENIGN | N | Real dedup + ownership-conflict check that `bail!`s on conflict. |
| `crates/ggen-core/src/pack_resolver.rs:491` | "all violations are treated as errors" | TEST-OR-BENIGN | N | Real `PolicyEnforcer::enforce`; `bail!` on any violation (fail-closed). |
| `crates/ggen-core/src/graph/store_tests.rs:128` | "verify the directory was created" | TEST-OR-BENIGN | N | `store_tests.rs` test file. |
| `crates/ggen-core/src/lifecycle/state.rs:215` | "return a large value to avoid false positives" (`u64::MAX`) | TEST-OR-BENIGN | N | FMEA disk-space guard; returning MAX makes the guard a no-op (never blocks), not a fabricated success. Defensive degrade-to-allow; platform free-space API is the documented gap. |
| `crates/ggen-core/src/lifecycle/state.rs:224` | same (`#[cfg(windows)]`) | TEST-OR-BENIGN | N | Same defensive no-op guard, Windows arm. |
| `crates/ggen-core/src/lifecycle/model.rs:364` | doc comment "For now, this is a limitation" | TEST-OR-BENIGN | N | Doc comment. |
| `crates/ggen-core/src/poka_yoke/lockfile_guard.rs:77` | "best-effort with file existence check" | TEST-OR-BENIGN | N | Honest best-effort guard; real `exists()` check. |
| `crates/ggen-core/src/poka_yoke/lockfile_guard.rs:153` | "assume success" | TEST-OR-BENIGN | N | Read in context: returns after a real operation; comment is scope note. |
| `crates/ggen-core/src/utils/bin/collect-metrics.rs:197` | "return None" | TEST-OR-BENIGN | N | Honest "no metric available" → `None`; a dev bin, not a product command. |
| `crates/ggen-core/src/ontology/validators.rs:163` | "For now: mock implementation" (`TypeSoundness` invariant no-ops) | TEST-OR-BENIGN | N | `validators.rs` types not re-exported / not CLI-reachable (only `RealLLMProposer` is re-exported, and it too is unwired — see below). |
| `crates/ggen-core/src/ontology/validators.rs:258` | "simulate actual test runs" (`RealDynamicValidator` loops + sleeps, passes if sector non-empty) | TEST-OR-BENIGN | N | Deceptively named but no live caller; not reachable from any verb/MCP handler. Flag for renaming/removal. |
| `crates/ggen-core/src/ontology/sigma_runtime.rs:345` | `signature: String::new()` | TEST-OR-BENIGN | N | `SigmaReceipt::new` constructor; field set by `.sign()` (line 366). |
| `crates/ggen-core/src/ontology/delta_proposer.rs:349` | "return mock proposals" (`RealLLMProposer` never calls an LLM) | TEST-OR-BENIGN | N | Re-exported from lib.rs:335 but zero CLI/MCP/marketplace callers (grep clean). Deceptive name + fabricated output; unwired. Flag for renaming/removal. |
| `crates/ggen-core/src/cli_generator/ontology_parser.rs:31` | "return error indicating integration needed" | TEST-OR-BENIGN | N | Returns `Err` (fail-closed / honest). |
| `crates/ggen-core/src/cli_generator/domain_layer.rs:68` | "use first noun as domain module" | TEST-OR-BENIGN | N | Real heuristic, honestly noted; produces real output. |
| `crates/ggen-core/src/receipt/receipt_impl.rs:54` | `signature: String::new()` | TEST-OR-BENIGN | N | Constructor; filled by `.sign()` (line 72-76). |
| `crates/ggen-core/src/receipt/receipt_impl.rs:144` | `signature: String::new()` | TEST-OR-BENIGN | N | `signing_message()` zeroes sig before hashing body. |
| `crates/ggen-core/src/prompt_mfg/ir.rs:151` | "create a minimal valid IR" | TEST-OR-BENIGN | N | Produces a real minimal-but-valid IR; honest scope note. |
| `crates/ggen-core/src/streaming_generator.rs:181` | "process sequentially" | TEST-OR-BENIGN | N | Real sequential processing; perf note, not a fake. |
| `crates/ggen-core/src/membrane/core.rs:91` | doc "simulated or real memory mappings" | TEST-OR-BENIGN | N | Doc comment on a state field. |
| `crates/ggen-core/src/domain/graph/export.rs:429` | "verify both exports succeed" | TEST-OR-BENIGN | N | Test-helper assertion comment. |
| `crates/ggen-core/src/domain/graph/export_expert_tests.rs:75` | "can't easily simulate permission denied" | TEST-OR-BENIGN | N | `_tests.rs` test file. |
| `crates/ggen-core/src/domain/graph/load.rs:112` | "treat each load as new graph" | TEST-OR-BENIGN | N | Real behavior, honest note. |
| `crates/ggen-core/src/domain/ontology/discover.rs:117` | `execute_discover` "return empty results" | TEST-OR-BENIGN | N | Unwired stub: no verb calls `execute_discover` (grep clean). Live discovery is elsewhere. Returns honest empty set, not fake success. |
| `crates/ggen-core/src/domain/ontology/generate.rs:114` | `execute_generate` "create a simple index file" | TEST-OR-BENIGN | N | Unwired stub: no verb calls `execute_generate`. Live codegen uses `mod.rs::generate_code_from_ontology` (real TypeScriptGenerator). |
| `crates/ggen-core/src/domain/ontology/extract.rs:95` | `execute_extract` "return a simple schema structure" (empty classes/props) | TEST-OR-BENIGN | N | Unwired stub: no verb calls `execute_extract`. Live `graph` verb uses `mod.rs::extract_ontology_schema` (real Graph + OntologyExtractor). |
| `crates/ggen-core/src/domain/packs/registry.rs:175` | "return current version only" | TEST-OR-BENIGN | N | Honest scope: returns the real current version. |
| `crates/ggen-core/src/domain/packs/composer.rs:210` | "fallback to merge" | TEST-OR-BENIGN | N | Real defined fallback strategy. |
| `crates/ggen-core/src/domain/packs/compose.rs:124` | "return packs in original order" | TEST-OR-BENIGN | N | Real deterministic ordering. |
| `crates/ggen-core/src/domain/packs/compose.rs:196` | "layer packs is similar to merge" | TEST-OR-BENIGN | N | Real merge-based layering. |
| `crates/ggen-core/src/domain/packs/template_generator.rs:181` | "use the variables list" | TEST-OR-BENIGN | N | Real behavior from template metadata. |
| `crates/ggen-core/src/domain/packs/template_generator.rs:290` | "create a placeholder implementation" | TEST-OR-BENIGN | N | Generates a real placeholder-bodied template section (intended output), not a faked result. |
| `crates/ggen-core/src/domain/packs/template_generator.rs:395` | "use defaults or placeholder values" | TEST-OR-BENIGN | N | Real defaulting for missing template vars. |
| `crates/ggen-core/src/domain/packs/advanced_resolver.rs:329` | "choose the first version" | TEST-OR-BENIGN | N | Real deterministic version pick. |
| `crates/ggen-core/src/domain/packs/advanced_resolver.rs:348` | "fall back to merge strategy" | TEST-OR-BENIGN | N | Real defined fallback. |
| `crates/ggen-core/src/domain/packs/install.rs:216` | "we just write it" | TEST-OR-BENIGN | N | Actually performs `fs::write` of the artifact (real durable state). |
| `crates/ggen-core/src/domain/project/apply.rs:111` | "just simulate success" (`apply_plan` returns count, applies nothing) | TEST-OR-BENIGN | N | `apply_plan` callers = only its own `#[test]`s (grep clean); no `#[verb]` routes to it. Unwired. Flag for activation/removal. |
| `crates/ggen-core/src/domain/mcp_config.rs:1039` | "return error" (`#[cfg(windows)]` uptime) | TEST-OR-BENIGN | N | Returns `Err(feature_not_enabled)` (fail-closed / honest). |
| `crates/ggen-core/src/domain/domain/mod.rs:6` | comment "include a placeholder" | TEST-OR-BENIGN | N | Module-doc comment. |
| `crates/ggen-core/src/dflss.rs:107` | "simulate the 6-sigma guard" — Analyze/`FMEA Risk Threshold` validator returns `Ok(())` unconditionally | **LIVE-PATH-MUST-FIX** | **Y** | `ggen sigma` → `sigma.rs:32 execute_dflss` runs the Analyze phase; the criterion claims "No failure modes have RPN > 200 without mitigation" but checks NOTHING and always passes. Fail-open quality gate; `overall_passed` reports success. |
| `crates/ggen-core/src/dflss.rs` (Design validator, lines 114-119) | Poka-Yoke design gate: enforcement `return Err(...)` is commented out | **LIVE-PATH-MUST-FIX** | **Y** | Same `ggen sigma` path. The `require_audit_trail` check is commented out, so the Design gate always passes regardless of manifest. Fail-open. (Found while reading dflss.rs:107; the grep hit was line 107 but the same function block carries this second fail-open.) |
| `crates/ggen-core/src/validation/syntax_validator.rs:77` | "use 0 as placeholder" (line number) | TEST-OR-BENIGN | N | Real `syn::parse_file` validation; only the line-number is 0 (msg carries the detail). |
| `crates/ggen-core/src/validation/syntax_validator.rs:152` | Tera "passthrough as Tera errors caught at render time" | TEST-OR-BENIGN | N | Honest deferral; Tera errors surface at render. |
| `crates/ggen-core/src/inject.rs:316` | "use exact match" | TEST-OR-BENIGN | N | Real exact-match logic; honest note. |
| `crates/ggen-marketplace/src/marketplace/v3.rs:531` | "return error with hint that this needs implementation" | TEST-OR-BENIGN | N | Returns `Err` + `warn!` (fail-closed / honest). |
| `crates/ggen-marketplace/src/marketplace/rdf/state_machine.rs:153` | "using default states" | TEST-OR-BENIGN | N | Real default states from `new()`. |
| `crates/ggen-marketplace/src/marketplace/rdf/poka_yoke.rs:199` | RDF-star "simulated in turtle output" | TEST-OR-BENIGN | N | Emits real (non-star) turtle triples as a documented compatibility representation. |
| `crates/ggen-marketplace/src/marketplace/rdf/poka_yoke.rs:520` | RDF-star "simulated with separate triples" | TEST-OR-BENIGN | N | Same: real separate triples, honest note. |
| `crates/ggen-marketplace/src/marketplace/rdf/poka_yoke.rs:626` | "Iterate over triples (simulated for backward compat)" | TEST-OR-BENIGN | N | Real iteration shim; honest note. |
| `crates/ggen-marketplace/src/marketplace/rdf/rdf_control.rs:380` | "return basic constraints" | TEST-OR-BENIGN | N | Returns real (basic) constraint set. |
| `crates/stpnt/src/proof/receipt.rs:40` | `signature: vec![] // placeholder for real signature` (+ `route_hash`/`obligation_hash` = `[0u8;32]` at lines 34-35) | TEST-OR-BENIGN | N | `StewardshipReceipt::new` has ZERO callers in `stpnt/src`, and stpnt has NO signing step anywhere. Unwired constructor — not currently emitted by any command. **Latent contract-drift risk** if wired without first filling route/obligation hashes + signature. Flag prominently. |

### MED — deceptive 'Real'-named types

| file:line | pattern | classification | live-path? | disposition / why |
|---|---|---|---|---|
| `crates/ggen-core/src/ontology/validators.rs:188` | `RealStaticValidator` (increments `checks_passed` for every required prop unconditionally) | TEST-OR-BENIGN | N | Deceptive name; not re-exported, no live caller. Flag for rename/removal. |
| `crates/ggen-core/src/ontology/validators.rs:243` | `RealDynamicValidator` (sleeps in a loop; "passes" if sector non-empty) | TEST-OR-BENIGN | N | Deceptive name; no live caller. Flag for rename/removal. |
| `crates/ggen-core/src/ontology/validators.rs:286` | `RealPerformanceValidator` | TEST-OR-BENIGN | N | Same module; no live caller. |
| `crates/ggen-core/src/ontology/delta_proposer.rs:270` | `RealLLMProposer` (builds prompt, never calls an LLM, returns mock proposals) | TEST-OR-BENIGN | N | Re-exported (lib.rs:335) but zero callers. Deceptive; unwired. Flag for rename/removal. |
| `crates/ggen-core/src/domain/capability_scanner.rs:195` | `struct RealCode { }` | TEST-OR-BENIGN | N | Inside `#[cfg(test)]` (`scanner_skips_comments_and_doc_comments`) — a test fixture, not a type. |

### LOW — stub/placeholder/not-yet markers (representative; full list in detector output)

| file:line | pattern | classification | live-path? | disposition / why |
|---|---|---|---|---|
| `crates/genesis-lockchain/src/quorum.rs:41,260` | placeholder | DORMANT | N | Non-compiled crate. |
| `crates/ggen-cli/src/commands/paas/handlers/deploy.rs:47` | "Deployment not yet implemented" (prints warning) | TEST-OR-BENIGN | N | Behind non-default `#[cfg(feature="paas")]` (commands/mod.rs:14); prints HONEST "not yet implemented", no fake success. |
| `crates/ggen-cli/src/commands/paas/handlers/explain.rs:66,78` | "not yet implemented" warnings | TEST-OR-BENIGN | N | Same gated, honest paas handlers. |
| `crates/ggen-cli/src/commands/paas/handlers/logs.rs:61` | "Log following not yet implemented" | TEST-OR-BENIGN | N | Same gated, honest paas handler. |
| `crates/ggen-core/src/lifecycle/production.rs:50,134-135,246,257,273,692-744,1199-1373` | `Placeholder` type + registry/processor | TEST-OR-BENIGN | N | Deliberate first-class `Placeholder` TYPE for production-readiness tracking — its job is to represent placeholders, not to fake work. |
| `crates/ggen-core/src/utils/bin/git_hook_pre_commit.rs:80-360` | "unimplemented!() placeholders" | TEST-OR-BENIGN | N | These are the DETECTOR's own messages (the pre-commit hook scanning for `unimplemented!()`), not fakes. |
| `crates/ggen-core/src/codegen_lib/rule.rs:127`, `codegen/pipeline.rs:1089` | `let placeholder = format!("{{{{{}}}}}", key)` | TEST-OR-BENIGN | N | Template token substitution (the `{{var}}` placeholder mechanism), real code. |
| `crates/ggen-lsp/src/route/edit.rs:14`, `route/model.rs:76`, `server.rs:302` | `{prefix}`/`{iri}`/`{symbol}` placeholders | TEST-OR-BENIGN | N | Route-template tokens, real binding mechanism. |
| `crates/ggen-lsp/src/analyzers/rdf_analyzer.rs:167-434` | `placeholder_uri()` | TEST-OR-BENIGN | N | Stable placeholder URI to keep the type total when no real URI exists; documented (line 434). |
| `crates/ggen-graph/src/bin/gall_observe_sabotage.rs:264` | "Add a placeholder comment" | TEST-OR-BENIGN | N | Sabotage-test description string. |
| `crates/ggen-graph/src/ocel/coverage.rs:117`, `self_audit.rs:62` | "zero placeholder/stub/mock/fake-success patterns" | TEST-OR-BENIGN | N | Requirement descriptions for the anti-fake gate — meta, not fakes. |
| `crates/ggen-core/src/schema/generators.rs:474,476,580` | Python codegen "not yet implemented" | TEST-OR-BENIGN | N | Returns an honest "not implemented" comment string / `#[ignore]`d test; no fake success. |
| `crates/ggen-core/src/domain/packs/compose.rs:59` | "Custom composition strategy not yet implemented" | TEST-OR-BENIGN | N | Returns an `Err`/honest message (fail-closed). |
| `crates/stpnt/src/proof/receipt.rs:34-35` | `route_hash`/`obligation_hash` = `[0u8;32]` placeholders | TEST-OR-BENIGN | N | Same unwired `StewardshipReceipt::new` constructor (see MED row). Latent risk, not live. |
| `crates/stpnt/src/cells/consent.rs:101` | "duration placeholder" (`1`) | TEST-OR-BENIGN | N | Hardcoded constant; read in context — a default duration, not a faked computation. |
| *(remaining LOW rows: doc comments, template tokens, `#[ignore]` tests, the deliberate `Placeholder` type — all benign; see raw `find-fakes.sh` LOW section.)* | — | TEST-OR-BENIGN | N | Bulk-classified after reading the representative cases above. |

---

## Summary

### Counts by classification

| Classification | Count (distinct findings triaged) |
|---|---|
| **LIVE-PATH-MUST-FIX** | **3** |
| DORMANT (5 non-compiled crates) | 7 |
| TEST-OR-BENIGN | remainder (all other MED/LOW rows) |

> "Count" reflects distinct triaged findings. The LOW section's bulk doc-comment /
> template-token / deliberate-`Placeholder`-type / test rows are classified
> TEST-OR-BENIGN en masse after reading the representative cases.

### LIVE-PATH-MUST-FIX — obligations

These are the only findings on a reachable workspace-member path that return
success or a plausible value without doing the work. Each becomes an obligation.

1. **`crates/ggen-cli/src/cmds/wizard.rs:449`** — `ggen wizard` (verb at line 238)
   inserts `"Initial sync completed"` into `next_steps` and returns
   `status:"success"` while running **no** sync (`// In a real implementation, we
   would call ggen sync here`). Decorative completion. **Fix:** either invoke the
   real sync or do not claim it completed.

2. **`crates/ggen-core/src/dflss.rs:107`** — `ggen sigma` Analyze phase
   `"FMEA Risk Threshold"` validator returns `Ok(())` unconditionally
   (`// For now, we simulate the 6-sigma guard`), so the gate always "passes"
   without checking any RPN. Fail-open quality gate. **Fix:** parse FMEA
   reports and fail when RPN > 200 without mitigation, or mark the gate
   explicitly unimplemented (do not report pass).

3. **`crates/ggen-core/src/dflss.rs:114-119`** — `ggen sigma` Design phase
   `"Poka-Yoke Design"` validator has its `return Err(...)` for
   `!require_audit_trail` **commented out**, so it always passes. Fail-open.
   **Fix:** re-enable the audit-trail enforcement or stop reporting pass.

### Notable non-blocking flags (TEST-OR-BENIGN but worth tracking)

- **Deceptive `Real*` types** (`RealStaticValidator`, `RealDynamicValidator`,
  `RealLLMProposer` in `ggen-core/src/ontology/`): named "Real" but
  simulate/mock; currently unwired (no live caller). Rename or remove before any
  activation so they cannot become live fakes.
- **`stpnt::proof::receipt::StewardshipReceipt::new`**: the only receipt
  constructor fabricates `route_hash`/`obligation_hash` as zeros and leaves
  `signature` empty, with **no signing path in the crate**. Currently has zero
  callers — latent contract-drift risk the moment it is wired.
- **`pack_install.rs` `PackInstaller` / `TestRepository`**: a "production"
  installer that can only produce a test repository; unwired (live install uses
  `domain::packs::install`). Remove or wire to a real repository.
- **`apply_plan` (`domain/project/apply.rs`)**: "simulate success" applier with
  no live verb; unwired.

### If zero live-path must-fix?

Not the case here — there are **3** LIVE-PATH-MUST-FIX findings, all listed above
with `file:line` and evidence. The HIGH detector tier (real `todo!()`/
`unimplemented!()` in live production) remains empty → `find-fakes.sh` exits 0.

---

*Receipt generated by manual evidence-first triage. Every cited line was opened
and read; every "no live caller" claim was verified by workspace-wide search.*
