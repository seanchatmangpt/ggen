# Scope: Implementation Track

## Architecture
- Same as defined in `/Users/sac/ggen/PROJECT.md`.

## Milestones
| # | Name | Scope | Dependencies | Status |
|---|------|-------|-------------|--------|
| 1 | Setup & Scaffolding | Cargo workspace and dependency activation | None | PLANNED |
| 2 | Core Models | PackDescriptor, PackPlan, ProjectionMap, CustomizationMap, ReceiptIndex | M1 | PLANNED |
| 3 | Durable Packs | ggen-pack-clap-noun-verb and ggen-pack-tower-lsp-max, examples/clap-noun-verb-lsp generation | M2 | PLANNED |
| 4 | LSP Meta-Observer | GGEN-PROJECTED-001, GGEN-DRIFT-001, GGEN-EVIDENCE-001, GGEN-CUSTOMIZE-001, GGEN-OVERRIDE-001, GGEN-PROJECT-OPPORTUNITY-001 diagnostics | M3 | PLANNED |
| 5 | tower-lsp-max Proxy | Diagnostic routing and composition with source_id attribution | M4 | PLANNED |
| 6 | E2E & Hardening | Run tests, fix errors, run Tier 5 adversarial checks | M5 | PLANNED |

## Interface Contracts
- See `/Users/sac/ggen/PROJECT.md`.
