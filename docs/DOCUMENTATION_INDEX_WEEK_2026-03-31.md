# Documentation Index — Week of 2026-03-31

Complete index of documentation written or updated March 24–31, 2026.

## New Documentation (March 31)

These docs were written today, documenting the completed MCP server, validation tools, OTEL instrumentation, and testing infrastructure.

| Document | Location | Description |
|----------|----------|-------------|
| MCP Server Reference | `docs/mcp-server-reference.md` | Complete API reference for all 16 MCP tools, 3 prompts, 4 resource types, completions, and OTEL attributes |
| Validation Tools Guide | `docs/validation-tools-guide.md` | User guide for 8 validation tools across 3 tiers (individual, pipeline, orchestration), with workflow patterns and troubleshooting |
| OTEL Verification Guide | `docs/otel-verification-guide.md` | How to verify OpenTelemetry spans at runtime, with verification script, required attributes per tool, and interpretation guide |
| Orchestration Tools | `docs/orchestration-tools.md` | Design reference for `validate_project`, `validate_incremental`, `validate_dependency_graph` — parameter structs, algorithms, return schemas |
| Testing Validation Tools | `docs/testing-validation-tools.md` | Chicago TDD test inventory (18 tests), test infrastructure, OTEL assertion patterns, coverage targets |
| Best Practices from WIP | `docs/ggen-best-practices-from-wip-and-last5.md` | 9-section best practices guide derived from WIP and last 5 commits — workflow standards, anti-patterns, checklists |

## Recently Updated (March 28–30)

### Architecture & Design

| Document | Location | Description |
|----------|----------|-------------|
| C4 Architecture | `docs/10-architecture/c4-components.md` | Component diagram |
| C4 Containers | `docs/10-architecture/c4-containers.md` | Container diagram |
| C4 Context | `docs/10-architecture/c4-context.md` | System context diagram |
| C4 Runtime | `docs/10-architecture/c4-runtime.md` | Runtime diagram |
| Evidence Plane | `docs/10-architecture/evidence-plane.md` | Evidence-based architecture |
| Security Architecture | `docs/10-architecture/security-architecture.md` | Security model |
| New Components Architecture | `docs/NEW_COMPONENTS_ARCHITECTURE.md` | v6.0.0 component architecture |
| GGen v7 Architecture | `docs/GGEN_V7_ARCHITECTURE_DESIGN.md` | Post-protocol-layer architecture |

### Autonomics & Operations

| Document | Location | Description |
|----------|----------|-------------|
| Action Contracts | `docs/30-autonomics/action-contracts.md` | Action contract definitions |
| gen_statem Patterns | `docs/30-autonomics/gen_statem-patterns.md` | State machine patterns |
| Governor Contract | `docs/30-autonomics/governor-contract.md` | Governor definitions |
| Invariants | `docs/30-autonomics/invariants.md` | System invariants |
| Refusal Modes | `docs/30-autonomics/refusal-modes.md` | Refusal mode definitions |
| Signal Contracts | `docs/30-autonomics/signal-contracts.md` | Signal contract definitions |
| Disaster Recovery | `docs/40-operations/disaster-recovery.md` | DR procedures |
| Incident Playbook | `docs/40-operations/incident-playbook.md` | Incident response |
| Runbooks | `docs/40-operations/runbooks.md` | Operational runbooks |
| Storm Discipline | `docs/40-operations/storm-discipline.md` | Storm management |

### Delivery & Sustainment

| Document | Location | Description |
|----------|----------|-------------|
| Acceptance Testing | `docs/60-delivery/acceptance-testing.md` | Acceptance criteria |
| ATO Evidence Pack | `docs/60-delivery/ato-evidence-pack.md` | Authority to Operate evidence |
| Cloudbuild Reference | `docs/60-delivery/cloudbuild-reference.md` | CI/CD configuration |
| Terraform Reference | `docs/60-delivery/terraform-reference.md` | Infrastructure as code |
| Change Control | `docs/70-sustainment/change-control.md` | Change management |
| Versioning Policy | `docs/70-sustainment/versioning-policy.md` | Version management |

### MCP & A2A Integration

| Document | Location | Description |
|----------|----------|-------------|
| MCP Quality Tools | `docs/mcp-quality-tools.md` | Quality tool documentation |
| MCP Usage Guide | `docs/mcp-usage.md` | MCP server usage patterns |
| MCP A2A Integration | `docs/MCP_A2A_INTEGRATION.md` | MCP-A2A bridge design |
| MCP A2A Best Practices | `docs/mcp-a2a-best-practices-summary.md` | Multi-agent best practices |
| A2A Integration | `docs/a2a-integration.md` | A2A protocol integration |
| Multi-Agent A2A Best Practices | `docs/multi-agent-a2a-best-practices.md` | Agent coordination patterns |
| A2A Templating Usage | `docs/A2A_TEMPLATING_USAGE.md` | A2A templating system |
| rmcp 1.3.0 Notes | `docs/rmcp-notes/README.md` | rmcp API reference |

### OTEL & Observability

| Document | Location | Description |
|----------|----------|-------------|
| OTEL Validation Setup | `docs/OTEL_VALIDATION_SETUP.md` | OTEL configuration |
| LLM Integration Verification | `docs/LLM_INTEGRATION_VERIFICATION.md` | LLM OTEL verification |

### Lean & Quality

| Document | Location | Description |
|----------|----------|-------------|
| Lean Performance | `docs/lean-performance-optimizations.md` | Performance optimization |
| Lean Poka-Yoke | `docs/lean-poka-yoke-improvements.md` | Error prevention |
| Lean Muda Elimination | `docs/lean-muda-elimination.md` | Waste elimination |
| Lean Mura Elimination | `docs/lean-mura-elimination.md` | Variability reduction |
| Lean Gemba Walk | `docs/lean-gemba-walk.md` | Workplace observation |
| Lean FMEA Analysis | `docs/lean-FMEA-analysis.md` | Failure mode analysis |
| Lean Andon Dashboard | `docs/lean-andon-dashboard.md` | Visibility system |
| Lean Code Review | `docs/lean-code-review.md` | Code review process |
| Andon Signal Report | `docs/ANDON_SIGNAL_REPORT.md` | Signal audit |
| Andon Stopping Rules | `docs/andon-stopping-rules.md` | Stop-the-line rules |
| Quality Metrics | `docs/QUALITY_METRICS_SUMMARY.md` | Quality metrics summary |

### Chicago TDD & Testing

| Document | Location | Description |
|----------|----------|-------------|
| Chicago TDD 100% Compliance | `docs/CHICAGO_TDD_100_PERCENT_COMPLIANCE.md` | Compliance evidence |
| Chicago TDD Migration Report | `docs/CHICAGO_TDD_MIGRATION_FINAL_REPORT.md` | Migration from London TDD |
| Chicago TDD Implementation | `docs/CHICAGO_TDD_IMPLEMENTATION.md` | Implementation guide |
| Testing Strategy | `docs/TESTING_STRATEGY.md` | Overall test strategy |
| Test Plan v2026.3.30 | `docs/test-plan-v2026.3.30.md` | Current test plan |
| Adversarial Testing | `docs/90-adversarial-testing/adversarial-suite.md` | Adversarial test suite |
| Property Tests | `docs/90-adversarial-testing/property-tests.md` | Property-based testing |
| Red Team Scenarios | `docs/90-adversarial-testing/redteam-scenarios.md` | Security test scenarios |

### Research

| Document | Location | Description |
|----------|----------|-------------|
| AI Testing Patterns 2026 | `docs/research/AI_TESTING_PATTERNS_2026.md` | AI testing patterns |
| AI Memory Research | `docs/research/ai-memory-management-2026.md` | Memory management |
| Performance Optimization | `docs/research/performance-optimization-2026.md` | Performance research |
| DSPy Evaluation | `docs/research/dspy-evaluation-system.md` | DSPy evaluation |
| Clap Ecosystem | `docs/research/clap-ecosystem-analysis.md` | CLI framework analysis |
| Rust Config Patterns | `docs/research/rust-config-patterns-survey.md` | Rust configuration patterns |

### Specifications & Ontology

| Document | Location | Description |
|----------|----------|-------------|
| RDF for Programmers | `docs/explanations/fundamentals/rdf-for-programmers.md` | RDF introduction |
| Ontology-Driven Development | `docs/explanations/fundamentals/ontology-driven-development.md` | ODD guide |
| MCP Protocol | `docs/explanations/fundamentals/mcp-protocol.md` | MCP fundamentals |
| A2A Protocol | `docs/explanations/fundamentals/a2a-protocol.md` | A2A fundamentals |
| MCP-A2A Bridge | `docs/explanations/fundamentals/mcp-a2a-bridge.md` | Protocol bridge |
| Determinism | `docs/explanations/determinism.md` | Deterministic generation |
| Projections | `docs/explanations/projections.md` | Code as projections |
| Poke-Yoke | `docs/explanations/poke-yoke.md` | Error proofing |

### Config & CLI

| Document | Location | Description |
|----------|----------|-------------|
| ggen.toml Reference | `docs/ggen-toml-reference.md` | Config file reference |
| ggen.toml Guide | `docs/ggen-toml-guide.md` | Config file guide |
| ggen.toml Migration | `docs/ggen-toml-migration.md` | Config migration |
| CLI Quick Reference | `docs/CLI_QUICK_REFERENCE.md` | CLI command reference |
| CLAUDE.md | `CLAUDE.md` | AI assistant configuration |
| Rules | `.claude/rules/README.md` | Development rules index |

### Marketplace & Business

| Document | Location | Description |
|----------|----------|-------------|
| Marketplace | `docs/explanations/marketplace.md` | Marketplace overview |
| Marketplace v2 Index | `docs/MARKETPLACE_V2_INDEX.md` | Marketplace v2 documentation |
| Market Positioning | `docs/market-positioning.md` | Market strategy |
| Financial Model | `docs/financial-model.md` | Financial projections |
| Go-to-Market Strategy | `docs/go-to-market-strategy.md` | GTM strategy |

### Java 26 Patterns

| Document | Location | Description |
|----------|----------|-------------|
| Pattern Language Book | `docs/java26-patterns/src/introduction.md` | Full pattern language (35 patterns) |
| Pattern Map | `docs/java26-patterns/src/pattern-map.md` | Pattern relationships |
| Glossary | `docs/java26-patterns/src/glossary.md` | Terminology |

### Diataxis Documentation Framework

| Document | Location | Description |
|----------|----------|-------------|
| Diataxis Evaluation | `docs/DIATAXIS_EVALUATION_EXTERNAL_PERSPECTIVE.md` | Documentation framework evaluation |
| Diataxis Index | `docs/diataxis-index.md` | Diataxis organization index |
| MCP+ A2A How-to Guides | `docs/how-to/` | Task-oriented guides |
| Tutorials | `docs/tutorials/` | Learning-oriented guides |
| Troubleshooting | `docs/troubleshooting/` | Problem-oriented guides |
| Reference | `docs/reference/` | Information-oriented docs |

### Migration & Performance

| Document | Location | Description |
|----------|----------|-------------|
| V6 Breaking Changes | `docs/v6-template-breaking-changes.md` | v6 migration guide |
| Oxigraph Migration | `docs/oxigraph-migration-guide.md` | Oxigraph 0.5.6 migration |
| Performance | `docs/performance-benchmarks.md` | Benchmark results |
| Build Optimization | `docs/BUILD_SYSTEM_ANALYSIS.md` | Build system analysis |

## Untracked Root-Level Reports (To Be Organized)

These files exist in the repo root and should be moved to `docs/` or cleaned up:

| File | Content | Recommended Action |
|------|---------|-------------------|
| `ORCHESTRATION_TOOLS_SUMMARY.md` | Orchestration tools design (superseded by `docs/orchestration-tools.md`) | Delete |
| `OTEL_VERIFICATION_REPORT.md` | OTEL report (superseded by `docs/otel-verification-guide.md`) | Delete |
| `UNBLOCK_VALIDATION_TESTS.md` | Blocker resolution guide | Move to `docs/` |
| `VALIDATION_MCP_TOOLS_COMPLETION_REPORT.md` | Completion report | Move to `docs/` |
| `ggen_server_fixes_summary.md` | Server fixes summary | Move to `docs/` |
| `otel_verification.txt` | OTEL output capture | Delete |

## Document Counts

| Category | Count |
|----------|-------|
| `docs/` total (last 7 days) | ~200 |
| Root-level reports | 6 |
| New docs written March 31 | 6 |
| `.claude/rules/` | 10 |

## How to Navigate

```
docs/
├── 00-overview/           # Mission, glossary, system contract
├── 10-architecture/       # C4 diagrams, evidence plane, security
├── 30-autonomics/         # Action/governor contracts, invariants
├── 40-operations/         # DR, incidents, runbooks
├── 50-procurement/        # Acquisition, compliance
├── 60-delivery/           # Acceptance, CI/CD, terraform
├── 70-sustainment/        # Change control, versioning
├── 80-decommission/       # Decommission, retention
├── 90-adversarial-testing/ # Adversarial, property, redteam
├── 99-appendix/           # API endpoints, schemas
├── diataxis/              # Tutorials, how-tos, troubleshooting, reference
├── explanations/          # Conceptual guides (RDF, MCP, A2A, determinism)
├── research/              # Research notes and analysis
├── java26-patterns/       # Pattern language book (35 patterns)
├── lean_*/                # Lean manufacturing adapted to software
├── mcp-*.md               # MCP server documentation
├── JTBD*.md               # Jobs-to-be-Done stories
├── CHICAGO_TDD*.md        # Chicago TDD methodology
└── MARKETPLACE*.md        # Marketplace system documentation
```
