# ggen Architecture — Compressed Reference

**Purpose:** Single document (~2K tokens) that gives an AI agent the same mental map a senior engineer builds over weeks. All paths verified by LSP survey 2026-04-01.

---

## C4 Container Diagram — Actual Dependencies

```mermaid
flowchart TD
    CLI["ggen-cli<br/>(entry point)"]

    subgraph CorePath["Core Pipeline Path"]
        GCORE["ggen-core<br/>pipeline, graph, generation"]
        GUTILS["ggen-utils<br/>shared error, SafePath"]
        GCANON["ggen-canonical<br/>deterministic hashing"]
        GRECEIPT["ggen-receipt<br/>Ed25519 receipts"]
        GMKT["ggen-marketplace<br/>packages, RDF store"]
    end

    subgraph AIPath["AI Integration Path"]
        GAI["ggen-ai<br/>LLM integration"]
        A2A["a2a-generated<br/>(feature-gated)"]
    end

    subgraph DomainPath["Domain Logic Path"]
        GDOMAIN["ggen-domain<br/>business logic"]
    end

    subgraph A2APath["A2A Protocol Path"]
        GMCP["ggen-a2a-mcp<br/>A2A↔MCP bridge"]
    end

    subgraph OntologyPath["Ontology Path"]
        GONT["ggen-ontology-core<br/>TripleStore, SPARQL"]
    end

    CLI --> GCORE
    CLI --> GAI
    CLI --> GDOMAIN
    CLI --> GMCP
    CLI --> GONT
    CLI --> GMKT

    GCORE --> GUTILS
    GCORE --> GCANON
    GCORE --> GRECEIPT
    GCORE --> GMKT

    GAI --> GUTILS
    GAI --> A2A

    GDOMAIN --> GCORE
    GDOMAIN --> GAI
    GDOMAIN --> GUTILS

    GMCP --> A2A
    GMCP --> GAI
    GMCP --> GCORE
    GMCP --> GDOMAIN

    GONT --> GUTILS

    GMKT --> GRECEIPT

    style CLI fill:#e1f5ff
    style GCORE fill:#fff4e6
    style GAI fill:#c8e6c9
    style GDOMAIN fill:#ffcdd2
    style GMCP fill:#fce4ec
    style GONT fill:#e1f5ff
```

**Standalone (zero ggen-* deps):**
`ggen-transport`, `ggen-canonical`, `ggen-utils`, `ggen-receipt`

**CRITICAL:** `ggen-core` ↔ `ggen-ai` cycle was deliberately broken.
Both have comments documenting removed mutual dependencies.

---

## Production Sync Flow — The REAL Path

```mermaid
flowchart TD
    SYNC["ggen sync"]

    subgraph PATHA["Path A: Default Manifest-Driven"]
        direction TB
        A1["cmds/sync.rs::sync()<br/>[sync.rs:305]"]
        A2["SyncExecutor::new(options)<br/>[executor.rs:249]"]
        A3["SyncExecutor::execute()<br/>[executor.rs:275]"]
        A4["ManifestParser::parse()"]
        A5["QualityGateRunner::run_all()<br/>[quality_gates.rs]"]
        A6["execute_full_sync()<br/>[executor.rs:589]"]
        A7["GenerationPipeline::new()<br/>[pipeline.rs:255]"]
        A8["GenerationPipeline::run()<br/>[pipeline.rs:811]"]
        A1 --> A2 --> A3 --> A4 --> A5 --> A6 --> A7 --> A8
    end

    subgraph PATHB["Path B: Ontology-First (--queries flag)"]
        direction TB
        B1["run_low_level_pipeline()"]
        B2["ggen_core::sync::sync(config)<br/>[sync/mod.rs:255]"]
        B3["load_ontology()"]
        B4["run_sparql_queries()"]
        B5["generate_code()"]
        B6["soundness validation"]
        B7["write_files() + receipt"]
        B1 --> B2 --> B3 --> B4 --> B5 --> B6 --> B7
    end

    subgraph STUB["NOT USED BY SYNC"]
        S1["StagedPipeline::run()<br/>[v6/pipeline.rs:329]"]
        S2["Constitutional mu0-mu5<br/>Tests only"]
        S1 --> S2
    end

    SYNC --> PATHA
    SYNC --> PATHB

    style PATHA fill:#c8e6c9
    style PATHB fill:#e1f5ff
    style STUB fill:#f5f5f5
    style S1 fill:#ffcdd2
    style S2 fill:#ffcdd2
```

---

## Key Entry Points

| Operation | Function | File:Line |
|-----------|----------|-----------|
| `ggen sync` | `sync()` | `ggen-cli/src/cmds/sync.rs:305` |
| Pipeline execution | `GenerationPipeline::run()` | `ggen-core/src/codegen/pipeline.rs:811` |
| v6 pipeline (tests) | `StagedPipeline::run()` | `ggen-core/src/v6/pipeline.rs:329` |
| Quality gates | `QualityGateRunner::run_all()` | `ggen-core/src/poka_yoke/quality_gates.rs` |
| LLM completion | `GenAiClient::complete()` | `ggen-ai/src/client.rs:203` |
| Marketplace publish | `RdfControlPlane::publish_package()` | `ggen-marketplace/src/rdf/control.rs` |
| Pack install | `Installer::install()` | `ggen-marketplace/src/install.rs` |
| Config loading | `ConfigLoader::from_file()` | `ggen-config/src/parser.rs:56` |
| A2A message routing | `MessageRouter::route()` | `ggen-a2a-mcp/src/handlers.rs` |
| Auth middleware | `http_auth_middleware()` | `vendors/a2a-rs/a2a-rs/src/adapter/auth/authenticator.rs:373` |
| Receipt chain verify | `ReceiptChain::verify()` | `ggen-receipt/src/chain.rs:113` |
| Session management | `SessionManager::create_session()` | `ggen-transport/src/session.rs` |
| Ontology loading | `TripleStore::load_turtle()` | `ggen-ontology-core/src/triple_store.rs` |

---

## Stub Registry — What's Real vs Fake

```mermaid
flowchart TD
    subgraph P0["P0 — Silently Wrong (always passes)"]
        SHACL["SHACL Validation<br/>Returns empty shape set"]
        SPARQL["SparqlValidator<br/>Empty violations Vec"]
        SIGMA["Sigma Invariants (4/7)<br/>TypeSoundness, GuardSoundness<br/>ProjectionDeterminism, SLOPreservation"]
    end

    subgraph P1["P1 — Returns error or placeholder"]
        CONSTRUCT["Construct command<br/>not_implemented"]
        MKT_V3["Marketplace v3 get<br/>not yet implemented"]
        PACK["Pack install (core)<br/>redirects to marketplace"]
        JSON["JSON Schema parser<br/>not yet implemented"]
        PYTHON["Python codegen<br/>not yet implemented"]
        MCP["MCP tool exec<br/>Placeholder JSON"]
        YAWL["YAWL watch mode<br/>not fully implemented"]
        DSPY["DSPy optimizers<br/>Pass-through"]
    end

    subgraph P2["P2 — Returns wrong answer silently"]
        CONFLICT["Marketplace conflict<br/>Always Ok — no semver"]
        WEAK["WeakProof::is_valid<br/>Always true"]
        GRAPH["Graph export formats<br/>Hardcoded example data"]
        GEN["Pack generator<br/>Logs Would generate"]
        SEARCH["Marketplace search/list<br/>Returns empty Vec"]
    end

    style P0 fill:#ffcdd2
    style P1 fill:#fff4e6
    style P2 fill:#fce4ec
```

### P0 — Silently wrong (always passes when it shouldn't)

| Component | File | What's wrong |
|-----------|------|--------------|
| SHACL validation | `ggen-core/src/validation/shacl.rs:132` | Returns empty shape set — always passes |
| SparqlValidator | `ggen-core/src/validation/validator.rs:113` | Empty violations Vec — always passes |
| Sigma invariants (4/7) | `ggen-core/src/ontology/validators.rs:162-172` | TypeSoundness, GuardSoundness, ProjectionDeterminism, SLOPreservation are no-ops |

### P1 — Returns error or placeholder

| Component | File | Returns |
|-----------|------|---------|
| Construct command | `ggen-cli/src/cmds/construct.rs:286` | "not_implemented" (llm_construct commented out) |
| Marketplace v3 get | `ggen-marketplace/src/v3.rs:249` | Error "not yet implemented" |
| Pack install (core) | `ggen-core/src/packs/install.rs:33` | Bails "use ggen_marketplace instead" |
| JSON Schema parser | `ggen-core/src/schema/parser.rs:281` | Error "not yet implemented" |
| Python codegen | `ggen-core/src/schema/generators.rs:601` | Comment "# not yet implemented" |
| MCP tool exec | `ggen-ai/src/mcp/traits.rs:338` | Placeholder JSON |
| YAWL watch mode | `ggen-cli/src/cmds/yawl.rs:268` | Error "not yet fully implemented" |
| DSPy optimizers | `ggen-dspy/src/optimizers/bootstrap.rs:28` | Pass-through (returns input unchanged) |

### P2 — Returns wrong answer silently

| Component | File | What's wrong |
|-----------|------|--------------|
| Marketplace conflict check | `ggen-marketplace/src/install.rs:234` | Always Ok — no semver check |
| WeakProof::is_valid | `ggen-domain/src/action_types.rs:397` | Always true |
| Graph export formats | `ggen-domain/src/graph/export.rs:217-293` | Returns hardcoded example data |
| Pack generator | `ggen-domain/src/packs/generator.rs:70` | Logs "Would generate" — no actual generation |
| Marketplace search/list | `ggen-marketplace/src/rdf/control.rs:391-405` | Returns empty Vec |

---

## Ontology Namespace — CANONICAL

```
Primary:   http://ggen.dev/ontology#       (ggen-core, ggen-domain, config TTLs)
Legacy:    https://ggen.io/marketplace/     (ggen-marketplace operational SPARQL)
Third:     http://ggen.dev/marketplace#     (ggen-marketplace/rdf/ontology.rs:24)

CONFLICT: SPARQL queries with one namespace return empty against triples stored with another.
ACTION NEEDED: Consolidate to single URI. Track in MASTER_TODO.md P0-03.
```

---

## Error Type Map

```
ggen-utils::error::Error    — universal base, 14 From conversions, bail!/ensure! macros
  ├── Re-exported by: ggen-domain, ggen-cli (as UtilsResult)
  └── Not re-exported by: 21 other crates (each has local error module)

CLI prelude has THREE Result types:
  - AnyhowResult    (anyhow)
  - Result          (clap_noun_verb)
  - UtilsResult     (ggen_utils)

Rule: Crate-local Error + From<ggen_utils::Error> for boundary conversion.
```

---

## Dead Infrastructure (do not use)

```mermaid
flowchart LR
    subgraph DEAD["Dead Infrastructure — No Consumers"]
        TESTING["ggen-testing<br/>Complete TDD harness<br/>Zero consumers"]
        MACROS["ggen-macros<br/>5 proc macros<br/>3 dead_code, 2 self-test only"]
        DOMAIN_MK["ggen-domain::marketplace<br/>All functions return Err<br/>'Moved to ggen-cli'"]
        V6["ggen-core::v6::V6Pipeline<br/>#[allow(dead_code)]"]
        HIVE["ggen-core::config::hive_coordinator<br/>All dead_code"]
        AHI["~4,200 lines AHI types<br/>Zero external consumers<br/>ahi_contract, doctrine_engine<br/>proof_carrier, temporal_fabric"]
    end

    style DEAD fill:#f5f5f5
    style TESTING fill:#ffcdd2
    style MACROS fill:#ffcdd2
```

| Crate/Module | Status | Why |
|-------------|--------|-----|
| `ggen-testing` | Zero consumers | Complete TDD harness, nothing depends on it |
| `ggen-macros` | Zero consumers | 3/5 macros dead_code, 2 only self-tested |
| `ggen-domain::marketplace` | All functions return Err | "Moved to ggen-cli" |
| `ggen-core::v6::V6Pipeline` struct | `#[allow(dead_code)]` | Separate from StagedPipeline |
| `ggen-core::config::hive_coordinator` | All dead_code | Distributed dep resolution, never wired |
| ~4,200 lines AHI types in ggen-domain | Zero external consumers | ahi_contract, doctrine_engine, proof_carrier, temporal_fabric, etc. |

---

## Test Status

```
Total test markers:     ~12,514
#[ignore] tests:        120 (never run in CI)
  - 26 "Phase 2" install tests (permanently dead)
  - 11 Docker-dependent production validation
  - 11 OTLP collector tests
  - 15 Next.js ontology e2e
Hand-rolled mocks:      8 files (Chicago TDD violation)
.unwrap() in tests:     5,046
.expect() in tests:     2,069
```
