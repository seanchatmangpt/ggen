# Contract: CLI command surface

This is the external interface `ggen` exposes to its users (developers running the CLI, and
editors driving the LSP). This contract MUST hold before and after the migration — it is the
observable surface User Story 1's acceptance scenarios test against.

## Commands that MUST remain available with equivalent behavior

| Noun | Verb(s) | Backed by (pre-migration) | Ticket covering the re-point |
|---|---|---|---|
| `sync` | `run [--dry-run] [--watch]` | 3 parallel pipeline mechanisms in `ggen-core` | [08](../../../docs/jira/v26.7.16/08-GGEN-CLI-MIGRATION.md) step 11 |
| `pack` / `packs` | install, list, show, validate | `domain::packs::*`, `packs::lockfile` | [06](../../../docs/jira/v26.7.16/06-MARKETPLACE-PACK-REGISTRY-MERGE.md) |
| `init` | (scaffold) | `cli_generator/`, `project_generator/` | [07](../../../docs/jira/v26.7.16/07-PROJECT-SCAFFOLDING-PORT.md) |
| `wizard` | (interactive scaffold) | same as `init` + sync pipeline | [07](../../../docs/jira/v26.7.16/07-PROJECT-SCAFFOLDING-PORT.md), [08](../../../docs/jira/v26.7.16/08-GGEN-CLI-MIGRATION.md) |
| `receipt` | verify, history | `agent/receipt.rs`, `ggen-config::receipt` | [04](../../../docs/jira/v26.7.16/04-RECEIPT-SIGNING-AND-OTEL.md) |
| `doctor` | (health check) | `domain::utils` | [08](../../../docs/jira/v26.7.16/08-GGEN-CLI-MIGRATION.md) |
| `graph` | load, query, export, visualize | `domain::graph` | [08](../../../docs/jira/v26.7.16/08-GGEN-CLI-MIGRATION.md) |
| `ontology` | (namespace/standard ontology ops) | `domain::ontology`, `ontology`, `validation` | [08](../../../docs/jira/v26.7.16/08-GGEN-CLI-MIGRATION.md) |
| `policy` | (enterprise/security policy enforcement) | `marketplace::policy/profile` | [06](../../../docs/jira/v26.7.16/06-MARKETPLACE-PACK-REGISTRY-MERGE.md) |
| `capability` | (capability registry lookups) | `domain::packs::capability_registry`, `packs::lockfile` | [06](../../../docs/jira/v26.7.16/06-MARKETPLACE-PACK-REGISTRY-MERGE.md) |
| `agent` | (pack-agent facade) | `agent::PackAgent` | [08](../../../docs/jira/v26.7.16/08-GGEN-CLI-MIGRATION.md) step 9 |
| `sigma` | (DFLSS quality report) | `dflss` | [08](../../../docs/jira/v26.7.16/08-GGEN-CLI-MIGRATION.md) |
| `inverse-sync` | (code → RDF extraction) | `reverse_sync::inverse_pipeline` | [08](../../../docs/jira/v26.7.16/08-GGEN-CLI-MIGRATION.md) |
| `template` | show, new, list, lint | `domain::template` | [08](../../../docs/jira/v26.7.16/08-GGEN-CLI-MIGRATION.md) |
| `lsp` | (launch language server) | error plumbing only | [08](../../../docs/jira/v26.7.16/08-GGEN-CLI-MIGRATION.md) |
| `mcp` | (launch MCP server) | error plumbing only | [08](../../../docs/jira/v26.7.16/08-GGEN-CLI-MIGRATION.md) |

## Diagnostic codes that MUST continue to fire, unchanged

`GGEN-TPL-001`, `GGEN-OUT-001`, `GGEN-YIELD-001`, `GGEN-RULE-001`, `GGEN-QUERY-002`, `E0011`,
`E0013` (all confirmed live today), plus `E0015` which MUST remain reserved/inactive (it is
**not** a live diagnostic today, despite CLAUDE.md's current, incorrect description of it —
see [13-CLAUDE-MD-REFACTOR](../../../docs/jira/v26.7.16/13-CLAUDE-MD-REFACTOR.md)). All eight
codes are owned by `ggen-lsp`'s own detector/orchestration layer, unaffected by which engine
backs `ggen-cli` — see
[05-MANIFEST-CONFIG-PORT](../../../docs/jira/v26.7.16/05-MANIFEST-CONFIG-PORT.md).

## What is explicitly NOT part of this contract

- Internal module paths, function names, or crate boundaries — these change freely as part
  of the migration; only observable CLI/LSP behavior is contractual.
- The exact receipt file format on disk, beyond "verifiable via `ggen receipt verify`" — the
  underlying schema is upgraded per
  [04-RECEIPT-SIGNING-AND-OTEL](../../../docs/jira/v26.7.16/04-RECEIPT-SIGNING-AND-OTEL.md).
- Performance characteristics beyond the existing SLOs already tracked by `just slo-check`.
