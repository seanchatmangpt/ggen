# Document Evidence Index (v26.8.1, GENERATED)

Generated against source_head=6b1ae24686bf31a9cbfa24dc0fa16c62bc47c5c6.
Real authority: `docs/v26.8.1/document-evidence-index.json`. This file is a projection.

## Subsystem coverage

| Subsystem | Documents | Has authority ref | Has implementation ref | Has verifier ref |
|---|---|---|---|---|
| governance | 17 | True | True | True |
| system | 10 | True | True | True |
| engine | 10 | True | True | True |
| graph | 10 | True | True | True |
| projection | 10 | True | True | True |
| evidence | 10 | True | True | True |
| products | 10 | True | True | True |
| verification | 11 | True | True | True |
| economics | 10 | True | True | True |
| legacy | 11 | True | True | True |

## Records

| Document | Subsystem | Role | Authority | Implementation | Verifier |
|---|---|---|---|---|---|
| docs/v26.8.1/document-evidence-index.md | verification | VERIFICATION | .specify/repo-facts.ttl, docs/jira/v26.7.16/11-DELETION-AND-DEFINITION-OF-DONE.md, docs/jira/v26.7.16/14-GGEN-CORE-REMOVAL-PROPOSAL.md, docs/v26.8.1, docs/v26.8.1/00-governance/01-canon-and-authority.md, docs/v26.8.1/00-governance/02-scope-and-non-goals.md, docs/v26.8.1/00-governance/03-standing-vocabulary.md, docs/v26.8.1/00-governance/04-claim-classification.md, docs/v26.8.1/00-governance/05-zero-information-loss-law.md, docs/v26.8.1/00-governance/06-source-precedence.md, docs/v26.8.1/00-governance/07-generated-surface-authority.md, docs/v26.8.1/00-governance/08-change-control.md, docs/v26.8.1/00-governance/09-release-decision-record.md, docs/v26.8.1/00-governance/10-risk-register.md, docs/v26.8.1/10-system/11-repository-topology.md, docs/v26.8.1/10-system/12-workspace-crate-map.md, docs/v26.8.1/10-system/13-dependency-boundaries.md, docs/v26.8.1/10-system/14-runtime-boundaries.md, docs/v26.8.1/10-system/15-process-intelligence-boundary.md, docs/v26.8.1/10-system/16-self-hosting-architecture.md, docs/v26.8.1/10-system/17-gall-control-plane.md, docs/v26.8.1/10-system/18-building-block-kernel.md, docs/v26.8.1/10-system/19-fortune5-capability-model.md, docs/v26.8.1/10-system/20-external-actuation-boundary.md, docs/v26.8.1/20-engine/21-sync-pipeline.md, docs/v26.8.1/20-engine/22-resolve-stage.md, docs/v26.8.1/20-engine/23-enrich-stage.md, docs/v26.8.1/20-engine/24-extract-stage.md, docs/v26.8.1/20-engine/25-render-stage.md, docs/v26.8.1/20-engine/26-write-stage.md, docs/v26.8.1/20-engine/27-receipt-stage.md, docs/v26.8.1/20-engine/28-dry-run-semantics.md, docs/v26.8.1/20-engine/29-watch-and-incremental-semantics.md, docs/v26.8.1/20-engine/30-failure-and-refusal-ordering.md, docs/v26.8.1/30-graph/31-rdf-authority-model.md, docs/v26.8.1/30-graph/32-oxigraph-integration.md, docs/v26.8.1/30-graph/33-praxis-graphlaw-integration.md, docs/v26.8.1/30-graph/34-sparql-contracts.md, docs/v26.8.1/30-graph/35-n3-and-datalog-boundary.md, docs/v26.8.1/30-graph/36-shacl-validation.md, docs/v26.8.1/30-graph/37-shex-validation.md, docs/v26.8.1/30-graph/38-ontology-imports.md, docs/v26.8.1/30-graph/39-deterministic-graph-hashing.md, docs/v26.8.1/30-graph/40-graph-delta-and-transition-receipts.md, docs/v26.8.1/40-projection/41-tera-integration.md, docs/v26.8.1/40-projection/42-template-frontmatter.md, docs/v26.8.1/40-projection/43-context-construction.md, docs/v26.8.1/40-projection/44-combinatorial-maximalism.md, docs/v26.8.1/40-projection/45-output-path-safety.md, docs/v26.8.1/40-projection/46-write-modes.md, docs/v26.8.1/40-projection/47-generated-manual-merge.md, docs/v26.8.1/40-projection/48-output-ownership.md, docs/v26.8.1/40-projection/49-render-determinism.md, docs/v26.8.1/40-projection/50-projection-performance.md, docs/v26.8.1/50-evidence/51-receipt-schema.md, docs/v26.8.1/50-evidence/52-blake3-chain.md, docs/v26.8.1/50-evidence/53-signature-and-key-management.md, docs/v26.8.1/50-evidence/54-replay-protocol.md, docs/v26.8.1/50-evidence/55-ocel-emission.md, docs/v26.8.1/50-evidence/56-opentelemetry.md, docs/v26.8.1/50-evidence/57-causality-chain.md, docs/v26.8.1/50-evidence/58-multi-surface-corroboration.md, docs/v26.8.1/50-evidence/59-external-witness.md, docs/v26.8.1/50-evidence/60-standing-promotion.md, docs/v26.8.1/60-products/61-cli-surface.md, docs/v26.8.1/60-products/62-default-verb-routing.md, docs/v26.8.1/60-products/63-configuration-schemas.md, docs/v26.8.1/60-products/64-lsp-surface.md, docs/v26.8.1/60-products/65-lsp-diagnostics.md, docs/v26.8.1/60-products/66-marketplace.md, docs/v26.8.1/60-products/67-pack-kernel.md, docs/v26.8.1/60-products/68-bblock-composition.md, docs/v26.8.1/60-products/69-lockfile-and-resolution.md, docs/v26.8.1/60-products/70-a2a-mcp-and-protocol-surfaces.md, docs/v26.8.1/70-verification, docs/v26.8.1/70-verification/71-verification-constitution.md, docs/v26.8.1/70-verification/72-unit-boundary-suite.md, docs/v26.8.1/70-verification/73-property-and-fuzz-suite.md, docs/v26.8.1/70-verification/74-stdio-http-integration-suite.md, docs/v26.8.1/70-verification/75-black-box-cli-e2e.md, docs/v26.8.1/70-verification/76-security-suite.md, docs/v26.8.1/70-verification/77-chaos-stress-and-benchmark.md, docs/v26.8.1/70-verification/78-replay-suite.md, docs/v26.8.1/70-verification/79-machine-readable-verifier-report.md, docs/v26.8.1/70-verification/80-negative-fixtures-and-falsifiers.md, docs/v26.8.1/80-economics/81-tera-benchmark-research.md, docs/v26.8.1/80-economics/82-oxigraph-benchmark-research.md, docs/v26.8.1/80-economics/83-integrated-pipeline-model.md, docs/v26.8.1/80-economics/84-littles-law.md, docs/v26.8.1/80-economics/85-amdahls-law.md, docs/v26.8.1/80-economics/86-brooks-and-coordination.md, docs/v26.8.1/80-economics/87-conways-law.md, docs/v26.8.1/80-economics/88-human-comparison.md, docs/v26.8.1/80-economics/89-cost-and-capacity-model.md, docs/v26.8.1/80-economics/90-threats-to-validity.md, docs/v26.8.1/90-legacy/100-final-decision-and-refusal.md, docs/v26.8.1/90-legacy/91-ggen-legacy-definition.md, docs/v26.8.1/90-legacy/92-chestertons-fence-inventory.md, docs/v26.8.1/90-legacy/93-capability-equivalence-matrix.md, docs/v26.8.1/90-legacy/94-command-and-diagnostic-parity.md, docs/v26.8.1/90-legacy/95-data-and-receipt-compatibility.md, docs/v26.8.1/90-legacy/96-migration-plan.md, docs/v26.8.1/90-legacy/97-archive-and-recovery.md, docs/v26.8.1/90-legacy/98-sunset-gates.md, docs/v26.8.1/90-legacy/99-sunset-runbook.md, docs/v26.8.1/90-legacy/observer-class-report.md, docs/v26.8.1/coverage-matrix.csv, docs/v26.8.1/diagrams/01-enterprise-manufacturing-system.md, docs/v26.8.1/diagrams/02-agent-fleet-and-gall.md, docs/v26.8.1/diagrams/03-unified-semantic-control-plane.md, docs/v26.8.1/diagrams/04-v2681-self-manufacture.md, docs/v26.8.1/diagrams/05-legacy-v2781-transformation.md, docs/v26.8.1/diagrams/06-crown-receipt-and-replay.md, docs/v26.8.1/diagrams/README.md, docs/v26.8.1/document-evidence-index.json, docs/v26.8.1/document-evidence-index.md | crates/ggen-cli/src/generated_commands.rs, crates/ggen-cli/tests/cli_surface_evidence_test.rs, crates/ggen-cli/tests/default_verb_law_test.rs, crates/ggen-config/src/manifest/types.rs, crates/ggen-config/tests/governance_precommit_gate_count_test.rs, crates/ggen-config/tests/schema_parity_test.rs, crates/ggen-config/tests/system_crate_map_parity_test.rs, crates/ggen-engine/src/config.rs, crates/ggen-engine/src/generation_rules.rs, crates/ggen-engine/src/keys.rs, crates/ggen-engine/src/sync.rs, crates/ggen-engine/src/verbs/sync.rs, crates/ggen-engine/tests/economics_measured_evidence_test.rs, crates/ggen-engine/tests/manifest_diagnostic_codes_evidence_test.rs, crates/ggen-engine/tests/pipeline_stage_evidence_test.rs, crates/ggen-engine/tests/projection_determinism_test.rs, crates/ggen-engine/tests/receipt_chain_e2e.rs, crates/ggen-engine/tests/receipt_signing_evidence_test.rs, crates/ggen-graph/tests/graph_hashing_evidence_test.rs, crates/praxis-core/src/receipt_record.rs, crates/praxis-core/src/refusal.rs, ontology/v26.8.1/legacy-capabilities.ttl, ontology/v26.8.1/ontology.ttl, ontology/v26.8.1/shapes.ttl, tools/v26.8.1/equivalence_runner.py, tools/v26.8.1/legacy_archaeology.py, tools/v26.8.1/src/main.rs | crates/ggen-cli/tests/cli_surface_evidence_test.rs, crates/ggen-cli/tests/default_verb_law_test.rs, crates/ggen-config/tests/governance_precommit_gate_count_test.rs, crates/ggen-config/tests/schema_parity_test.rs, crates/ggen-config/tests/system_crate_map_parity_test.rs, crates/ggen-engine/tests/economics_measured_evidence_test.rs, crates/ggen-engine/tests/manifest_diagnostic_codes_evidence_test.rs, crates/ggen-engine/tests/pipeline_stage_evidence_test.rs, crates/ggen-engine/tests/projection_determinism_test.rs, crates/ggen-engine/tests/receipt_chain_e2e.rs, crates/ggen-engine/tests/receipt_signing_evidence_test.rs, crates/ggen-graph/tests/graph_hashing_evidence_test.rs, tools/v26.8.1/src/main.rs |
| docs/v26.8.1/00-governance/01-canon-and-authority.md | governance | GOVERNANCE | AGENTS.md, CLAUDE.md, justfile | justfile | crates/ggen-config/tests/governance_precommit_gate_count_test.rs |
| docs/v26.8.1/00-governance/02-scope-and-non-goals.md | governance | GOVERNANCE | - | - | - |
| docs/v26.8.1/00-governance/03-standing-vocabulary.md | governance | GOVERNANCE | - | - | - |
| docs/v26.8.1/00-governance/04-claim-classification.md | governance | GOVERNANCE | - | - | - |
| docs/v26.8.1/00-governance/05-zero-information-loss-law.md | governance | GOVERNANCE | - | - | - |
| docs/v26.8.1/00-governance/06-source-precedence.md | governance | GOVERNANCE | - | crates/ggen-config/tests/schema_parity_test.rs, crates/ggen-engine/src/generation_rules.rs | crates/ggen-config/tests/schema_parity_test.rs |
| docs/v26.8.1/00-governance/07-generated-surface-authority.md | governance | GOVERNANCE | - | - | - |
| docs/v26.8.1/00-governance/08-change-control.md | governance | GOVERNANCE | - | crates/ggen-config/tests/governance_precommit_gate_count_test.rs, crates/ggen-config/tests/schema_parity_test.rs, crates/ggen-config/tests/system_crate_map_parity_test.rs, crates/ggen-engine/src/keys.rs, crates/praxis-core/src/refusal.rs | crates/ggen-config/tests/governance_precommit_gate_count_test.rs, crates/ggen-config/tests/schema_parity_test.rs, crates/ggen-config/tests/system_crate_map_parity_test.rs |
| docs/v26.8.1/00-governance/09-release-decision-record.md | governance | GOVERNANCE | - | - | - |
| docs/v26.8.1/00-governance/10-risk-register.md | governance | GOVERNANCE | - | - | - |
| docs/v26.8.1/10-system/11-repository-topology.md | system | ARCHITECTURE | .specify/repo-facts.ttl | .specify/repo-facts.ttl, Cargo.toml | crates/ggen-config/tests/system_crate_map_parity_test.rs |
| docs/v26.8.1/10-system/12-workspace-crate-map.md | system | ARCHITECTURE | .specify/repo-facts.ttl | crates/ggen-config/tests/system_crate_map_parity_test.rs | crates/ggen-config/tests/system_crate_map_parity_test.rs |
| docs/v26.8.1/10-system/13-dependency-boundaries.md | system | ARCHITECTURE | - | - | - |
| docs/v26.8.1/10-system/14-runtime-boundaries.md | system | ARCHITECTURE | - | - | - |
| docs/v26.8.1/10-system/15-process-intelligence-boundary.md | system | ARCHITECTURE | - | - | - |
| docs/v26.8.1/10-system/16-self-hosting-architecture.md | system | ARCHITECTURE | - | - | - |
| docs/v26.8.1/10-system/17-gall-control-plane.md | system | ARCHITECTURE | - | - | - |
| docs/v26.8.1/10-system/18-building-block-kernel.md | system | ARCHITECTURE | - | - | - |
| docs/v26.8.1/10-system/19-fortune5-capability-model.md | system | ARCHITECTURE | - | - | - |
| docs/v26.8.1/10-system/20-external-actuation-boundary.md | system | ARCHITECTURE | - | - | - |
| docs/v26.8.1/20-engine/21-sync-pipeline.md | engine | IMPLEMENTATION | docs/v26.8.1/20-engine | crates/ggen-engine/src/sync.rs | crates/ggen-engine/tests/manifest_diagnostic_codes_evidence_test.rs, crates/ggen-engine/tests/pipeline_stage_evidence_test.rs |
| docs/v26.8.1/20-engine/22-resolve-stage.md | engine | ARCHITECTURE | - | - | - |
| docs/v26.8.1/20-engine/23-enrich-stage.md | engine | ARCHITECTURE | - | - | - |
| docs/v26.8.1/20-engine/24-extract-stage.md | engine | ARCHITECTURE | - | - | - |
| docs/v26.8.1/20-engine/25-render-stage.md | engine | ARCHITECTURE | - | - | - |
| docs/v26.8.1/20-engine/26-write-stage.md | engine | ARCHITECTURE | - | - | - |
| docs/v26.8.1/20-engine/27-receipt-stage.md | engine | ARCHITECTURE | - | - | - |
| docs/v26.8.1/20-engine/28-dry-run-semantics.md | engine | ARCHITECTURE | - | - | - |
| docs/v26.8.1/20-engine/29-watch-and-incremental-semantics.md | engine | ARCHITECTURE | - | - | - |
| docs/v26.8.1/20-engine/30-failure-and-refusal-ordering.md | engine | ARCHITECTURE | - | - | - |
| docs/v26.8.1/30-graph/31-rdf-authority-model.md | graph | ARCHITECTURE | docs/v26.8.1/30-graph | crates/ggen-graph/src, crates/praxis-graphlaw/src | crates/ggen-graph/tests/graph_hashing_evidence_test.rs |
| docs/v26.8.1/30-graph/32-oxigraph-integration.md | graph | ARCHITECTURE | - | - | - |
| docs/v26.8.1/30-graph/33-praxis-graphlaw-integration.md | graph | ARCHITECTURE | - | - | - |
| docs/v26.8.1/30-graph/34-sparql-contracts.md | graph | ARCHITECTURE | - | - | - |
| docs/v26.8.1/30-graph/35-n3-and-datalog-boundary.md | graph | ARCHITECTURE | - | - | - |
| docs/v26.8.1/30-graph/36-shacl-validation.md | graph | ARCHITECTURE | - | - | - |
| docs/v26.8.1/30-graph/37-shex-validation.md | graph | ARCHITECTURE | - | - | - |
| docs/v26.8.1/30-graph/38-ontology-imports.md | graph | ARCHITECTURE | - | - | - |
| docs/v26.8.1/30-graph/39-deterministic-graph-hashing.md | graph | ARCHITECTURE | - | - | - |
| docs/v26.8.1/30-graph/40-graph-delta-and-transition-receipts.md | graph | ARCHITECTURE | - | - | - |
| docs/v26.8.1/40-projection/41-tera-integration.md | projection | ARCHITECTURE | docs/v26.8.1/40-projection | crates/ggen-engine/src/sync.rs | crates/ggen-engine/tests/projection_determinism_test.rs |
| docs/v26.8.1/40-projection/42-template-frontmatter.md | projection | ARCHITECTURE | - | - | - |
| docs/v26.8.1/40-projection/43-context-construction.md | projection | ARCHITECTURE | - | - | - |
| docs/v26.8.1/40-projection/44-combinatorial-maximalism.md | projection | ARCHITECTURE | - | - | - |
| docs/v26.8.1/40-projection/45-output-path-safety.md | projection | ARCHITECTURE | - | - | - |
| docs/v26.8.1/40-projection/46-write-modes.md | projection | ARCHITECTURE | - | - | - |
| docs/v26.8.1/40-projection/47-generated-manual-merge.md | projection | ARCHITECTURE | - | - | - |
| docs/v26.8.1/40-projection/48-output-ownership.md | projection | ARCHITECTURE | - | - | - |
| docs/v26.8.1/40-projection/49-render-determinism.md | projection | ARCHITECTURE | - | - | - |
| docs/v26.8.1/40-projection/50-projection-performance.md | projection | ARCHITECTURE | - | - | - |
| docs/v26.8.1/50-evidence/51-receipt-schema.md | evidence | ARCHITECTURE | docs/v26.8.1/50-evidence | crates/ggen-engine/src/sync.rs, crates/praxis-core/src/receipt_record.rs | crates/ggen-engine/tests/receipt_signing_evidence_test.rs |
| docs/v26.8.1/50-evidence/52-blake3-chain.md | evidence | ARCHITECTURE | - | - | - |
| docs/v26.8.1/50-evidence/53-signature-and-key-management.md | evidence | ARCHITECTURE | - | - | - |
| docs/v26.8.1/50-evidence/54-replay-protocol.md | evidence | ARCHITECTURE | - | - | - |
| docs/v26.8.1/50-evidence/55-ocel-emission.md | evidence | ARCHITECTURE | - | - | - |
| docs/v26.8.1/50-evidence/56-opentelemetry.md | evidence | ARCHITECTURE | - | - | - |
| docs/v26.8.1/50-evidence/57-causality-chain.md | evidence | ARCHITECTURE | - | - | - |
| docs/v26.8.1/50-evidence/58-multi-surface-corroboration.md | evidence | ARCHITECTURE | - | - | - |
| docs/v26.8.1/50-evidence/59-external-witness.md | evidence | ARCHITECTURE | - | - | - |
| docs/v26.8.1/50-evidence/60-standing-promotion.md | evidence | ARCHITECTURE | - | - | - |
| docs/v26.8.1/60-products/61-cli-surface.md | products | ARCHITECTURE | docs/v26.8.1/60-products | crates/ggen-cli/src | crates/ggen-cli/tests/cli_surface_evidence_test.rs, crates/ggen-cli/tests/default_verb_law_test.rs |
| docs/v26.8.1/60-products/62-default-verb-routing.md | products | ARCHITECTURE | - | - | - |
| docs/v26.8.1/60-products/63-configuration-schemas.md | products | ARCHITECTURE | - | - | - |
| docs/v26.8.1/60-products/64-lsp-surface.md | products | ARCHITECTURE | - | - | - |
| docs/v26.8.1/60-products/65-lsp-diagnostics.md | products | ARCHITECTURE | - | - | - |
| docs/v26.8.1/60-products/66-marketplace.md | products | ARCHITECTURE | - | - | - |
| docs/v26.8.1/60-products/67-pack-kernel.md | products | ARCHITECTURE | - | - | - |
| docs/v26.8.1/60-products/68-bblock-composition.md | products | ARCHITECTURE | - | - | - |
| docs/v26.8.1/60-products/69-lockfile-and-resolution.md | products | ARCHITECTURE | - | - | - |
| docs/v26.8.1/60-products/70-a2a-mcp-and-protocol-surfaces.md | products | ARCHITECTURE | - | - | - |
| docs/v26.8.1/70-verification/71-verification-constitution.md | verification | VERIFICATION | - | - | - |
| docs/v26.8.1/70-verification/72-unit-boundary-suite.md | verification | VERIFICATION | - | - | - |
| docs/v26.8.1/70-verification/73-property-and-fuzz-suite.md | verification | VERIFICATION | - | - | - |
| docs/v26.8.1/70-verification/74-stdio-http-integration-suite.md | verification | VERIFICATION | - | - | - |
| docs/v26.8.1/70-verification/75-black-box-cli-e2e.md | verification | VERIFICATION | - | - | - |
| docs/v26.8.1/70-verification/76-security-suite.md | verification | VERIFICATION | - | - | - |
| docs/v26.8.1/70-verification/77-chaos-stress-and-benchmark.md | verification | VERIFICATION | - | - | - |
| docs/v26.8.1/70-verification/78-replay-suite.md | verification | VERIFICATION | - | - | - |
| docs/v26.8.1/70-verification/79-machine-readable-verifier-report.md | verification | VERIFICATION | - | - | - |
| docs/v26.8.1/70-verification/80-negative-fixtures-and-falsifiers.md | verification | VERIFICATION | - | - | - |
| docs/v26.8.1/80-economics/81-tera-benchmark-research.md | economics | ECONOMICS | docs/v26.8.1/80-economics | crates/ggen-engine/tests/receipt_chain_e2e.rs, justfile | crates/ggen-engine/tests/economics_measured_evidence_test.rs |
| docs/v26.8.1/80-economics/82-oxigraph-benchmark-research.md | economics | ECONOMICS | - | - | - |
| docs/v26.8.1/80-economics/83-integrated-pipeline-model.md | economics | ECONOMICS | - | - | - |
| docs/v26.8.1/80-economics/84-littles-law.md | economics | ECONOMICS | - | - | - |
| docs/v26.8.1/80-economics/85-amdahls-law.md | economics | ECONOMICS | - | - | - |
| docs/v26.8.1/80-economics/86-brooks-and-coordination.md | economics | ECONOMICS | - | - | - |
| docs/v26.8.1/80-economics/87-conways-law.md | economics | ECONOMICS | - | - | - |
| docs/v26.8.1/80-economics/88-human-comparison.md | economics | ECONOMICS | - | - | - |
| docs/v26.8.1/80-economics/89-cost-and-capacity-model.md | economics | ECONOMICS | - | - | - |
| docs/v26.8.1/80-economics/90-threats-to-validity.md | economics | ECONOMICS | - | - | - |
| docs/v26.8.1/90-legacy/100-final-decision-and-refusal.md | legacy | MIGRATION | docs/v26.8.1/90-legacy | ontology/v26.8.1/legacy-capabilities.ttl, tools/v26.8.1/equivalence_runner.py | tools/v26.8.1/equivalence_runner.py |
| docs/v26.8.1/90-legacy/91-ggen-legacy-definition.md | legacy | MIGRATION | - | ontology/v26.8.1/legacy-capabilities.ttl, ontology/v26.8.1/ontology.ttl, tools/v26.8.1/legacy_archaeology.py | - |
| docs/v26.8.1/90-legacy/92-chestertons-fence-inventory.md | legacy | LEGACY | docs/jira/v26.7.16/11-DELETION-AND-DEFINITION-OF-DONE.md, docs/jira/v26.7.16/14-GGEN-CORE-REMOVAL-PROPOSAL.md | crates/ggen-config/src/manifest/types.rs, crates/ggen-engine/src/config.rs, crates/ggen-engine/src/verbs/sync.rs, ontology/v26.8.1/legacy-capabilities.ttl | - |
| docs/v26.8.1/90-legacy/93-capability-equivalence-matrix.md | legacy | MIGRATION | docs/v26.8.1/coverage-matrix.csv | ontology/v26.8.1/legacy-capabilities.ttl, ontology/v26.8.1/ontology.ttl, tools/v26.8.1/legacy_archaeology.py | - |
| docs/v26.8.1/90-legacy/94-command-and-diagnostic-parity.md | legacy | MIGRATION | - | - | - |
| docs/v26.8.1/90-legacy/95-data-and-receipt-compatibility.md | legacy | MIGRATION | - | - | - |
| docs/v26.8.1/90-legacy/96-migration-plan.md | legacy | MIGRATION | - | - | - |
| docs/v26.8.1/90-legacy/97-archive-and-recovery.md | legacy | MIGRATION | - | - | - |
| docs/v26.8.1/90-legacy/98-sunset-gates.md | legacy | MIGRATION | - | - | - |
| docs/v26.8.1/90-legacy/99-sunset-runbook.md | legacy | MIGRATION | - | - | - |
| docs/v26.8.1/90-legacy/observer-class-report.md | legacy | MIGRATION | - | crates/ggen-cli/src/generated_commands.rs, crates/ggen-config/src/manifest/types.rs, crates/ggen-engine/src/sync.rs, crates/praxis-core/src/receipt_record.rs, ontology/v26.8.1/legacy-capabilities.ttl, ontology/v26.8.1/ontology.ttl, ontology/v26.8.1/shapes.ttl, tools/v26.8.1/legacy_archaeology.py | - |
| docs/v26.8.1/diagrams/01-enterprise-manufacturing-system.md | governance | GOVERNANCE | - | - | - |
| docs/v26.8.1/diagrams/02-agent-fleet-and-gall.md | governance | GOVERNANCE | - | - | - |
| docs/v26.8.1/diagrams/03-unified-semantic-control-plane.md | governance | GOVERNANCE | - | - | - |
| docs/v26.8.1/diagrams/04-v2681-self-manufacture.md | governance | GOVERNANCE | - | - | - |
| docs/v26.8.1/diagrams/05-legacy-v2781-transformation.md | governance | GOVERNANCE | - | - | - |
| docs/v26.8.1/diagrams/06-crown-receipt-and-replay.md | governance | GOVERNANCE | - | - | - |
| docs/v26.8.1/diagrams/README.md | governance | GOVERNANCE | - | - | - |
