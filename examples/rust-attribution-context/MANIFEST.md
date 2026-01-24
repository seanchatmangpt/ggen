# Rust Attribution Context - TCPS Reference Implementation

## Manifest - Complete TCPS (Toyota Code Production System)

**Status**: Foundation Complete ✅
**Last Updated**: 2026-01-24

---

## 📋 Project Overview

This is the **reference implementation** for "Crossing the Event Horizon" with ggen - the irreversible paradigm shift from code-first to RDF-first development.

### Core Equation
```
A = μ(O)
```

Where:
- **A** = All artifacts (code, infra, diagrams, docs, tests)
- **μ** = Five-stage deterministic pipeline (ggen sync)
- **O** = RDF ontology (source of truth)

### The Production Line

```
ontology/*.ttl (TRUTH - editable by humans)
       ↓
  ggen sync    (COMPILER - deterministic)
       ↓
  world/       (GENERATED - sealed, no human edits)
```

---

## 📁 File Inventory

### ✅ Ontology (6 files - SOURCE OF TRUTH)

All files in `ontology/` define the truth. Edit these, run `ggen sync`, watch the world regenerate.

1. **attribution.ttl** - Bounded context definition
   - BoundedContext: Attribution
   - Version: 1.0.0
   - Aggregates: ClickAggregate, PublisherAggregate, OfferAggregate
   - Commands: RecordClick, ComputeAttribution, ProcessPayout
   - Events: ClickRecorded, AttributionComputed, PayoutCalculated
   - Policies: AttributionPolicy, PayoutPolicy
   - Service config: Port 8080, health/metrics/receipts endpoints
   - Domain invariants: 4 invariants (unique click ID, positive payouts, attribution window, total clicks)

2. **entities.ttl** - Domain model entities
   - Publisher (id, name, total_revenue)
   - Offer (id, name, payout_amount, payout_type)
   - Click (id, publisher_id, offer_id, timestamp, ip_hash, user_agent, attributed)
   - Attribution (id, click_id, publisher_id, amount, computed_at)

3. **commands.ttl** - Write operations (CQRS command side)
   - RecordClick: publisher_id, offer_id, ip_hash, user_agent → emits ClickRecorded
   - ComputeAttribution: click_id → emits AttributionComputed
   - ProcessPayout: publisher_id, period_start, period_end → emits PayoutCalculated

4. **events.ttl** - State changes (Event Sourcing)
   - ClickRecorded: click_id, publisher_id, offer_id, timestamp
   - AttributionComputed: click_id, publisher_id, amount, computed_at
   - PayoutCalculated: publisher_id, amount, period_start, period_end, calculated_at

5. **policies.ttl** - Business rules
   - AttributionPolicy: model=last-click, windowDays=30
   - PayoutPolicy: minimumThreshold=10.00 USD, schedule=monthly
   - Rules: AttributionWindow, MinimumPayout, PayoutSchedule

6. **infra.ttl** - Infrastructure topology
   - AttributionVM: OCI Always Free (VM.Standard.A1.Flex, 2 CPUs, 12GB RAM, us-phoenix-1)
   - AttributionDB: PostgreSQL 14, 50GB storage

### ✅ SPARQL Queries (18 files)

Each query extracts data from ontology for template rendering.

1. **entities.sparql** - Extract entity definitions → rust_entities.rs
2. **commands.sparql** - Extract command definitions → rust_commands.rs
3. **events.sparql** - Extract event definitions → rust_events.rs
4. **aggregates.sparql** - Extract aggregate definitions → rust_aggregates.rs
5. **handlers.sparql** - Extract command handlers → rust_handlers.rs
6. **routes.sparql** - Extract HTTP routes → rust_routes.rs
7. **modules.sparql** - Extract module list → rust_lib.rs
8. **cargo.sparql** - Extract package metadata → Cargo.toml
9. **infra.sparql** - Extract infrastructure resources → terraform/*.tf
10. **closure.sparql** - Extract service config → run/* scripts
11. **c4_context.sparql** - Extract C4 context diagram data → c4_context.mmd
12. **c4_containers.sparql** - Extract C4 container diagram data → c4_containers.mmd
13. **c4_components.sparql** - Extract C4 component diagram data → c4_components.mmd
14. **togaf_building_blocks.sparql** - Extract TOGAF ABBs/SBBs → togaf_building_blocks.md
15. **togaf_realization.sparql** - Extract infrastructure realization → togaf_realization.md
16. **receipts.sparql** - Extract event schema for receipts → receipts_schema.rs
17. **properties.sparql** - Extract entity properties for testing → property_tests.rs
18. **integration.sparql** - Extract command flows for testing → integration_tests.rs

### ✅ Templates (18 files)

Each template transforms SPARQL results into code/config/docs.

#### Rust Code Generation
1. **rust_entities.rs.tera** - Generate entity structs with serde
2. **rust_commands.rs.tera** - Generate command structs
3. **rust_events.rs.tera** - Generate event structs
4. **rust_aggregates.rs.tera** - Generate aggregate structs with business logic
5. **rust_handlers.rs.tera** - Generate command handlers (command → event)
6. **rust_routes.rs.tera** - Generate Axum HTTP routes with health check
7. **rust_lib.rs.tera** - Generate library root module
8. **receipts_schema.rs.tera** - Generate cryptographic receipt wrappers

#### Build & Infrastructure
9. **cargo_toml.tera** - Generate Cargo.toml manifest
10. **terraform_main.tf.tera** - Generate Terraform main configuration
11. **terraform_variables.tf.tera** - Generate Terraform variables
12. **terraform_outputs.tf.tera** - Generate Terraform outputs

#### World Closure Scripts
13. **run_up.sh.tera** - Generate world boot script (build + deploy)
14. **run_down.sh.tera** - Generate world shutdown script
15. **run_status.sh.tera** - Generate world status check script
16. **run_verify.sh.tera** - Generate world verification script (tests + health)

#### Documentation & Diagrams
17. **c4_context.mmd.tera** - Generate C4 context diagram (Mermaid)
18. **c4_containers.mmd.tera** - Generate C4 container diagram
19. **c4_components.mmd.tera** - Generate C4 component diagram
20. **togaf_building_blocks.md.tera** - Generate TOGAF ABB/SBB documentation
21. **togaf_realization.md.tera** - Generate infrastructure realization docs

#### Tests
22. **property_tests.rs.tera** - Generate property-based tests (proptest)
23. **integration_tests.rs.tera** - Generate API integration tests

### ✅ Generation Rules (ggen.toml - 27 rules)

**NO output_directory** - Every rule has explicit `to = "world/..."` path.

```toml
[[generation.rules]]
name = "rust_entities"
query_file = "queries/entities.sparql"
template_file = "templates/rust_entities.rs.tera"
to = "world/src/entities.rs"
```

All 27 rules follow this pattern:
- Rust code: 8 rules (entities, commands, events, aggregates, handlers, routes, lib, receipts)
- Build: 1 rule (Cargo.toml)
- Infrastructure: 3 rules (terraform main, variables, outputs)
- World closure: 4 rules (up, down, status, verify)
- Documentation: 5 rules (C4 diagrams, TOGAF docs)
- Tests: 2 rules (property tests, integration tests)
- Aggregate configs: 3 rules (click, publisher, offer)
- Projections: 1 rule (revenue projection)

### ✅ Kernel (2 files - ONLY HUMAN-EDITABLE CODE)

The kernel is the **stable runtime host**. It never contains domain logic.

1. **kernel/Cargo.toml** - Runtime dependencies (tokio, tracing, axum)
2. **kernel/src/main.rs** - Bootstrap code
   - Initialize tracing
   - Check if world exists
   - Load generated routes
   - Start HTTP server
   - Handle graceful shutdown

**Critical**: Kernel code is minimal. Domain logic lives in `world/` (generated).

### ✅ Build System

1. **Makefile.toml** - Complete task workflow
   - `cargo make sync` - Generate complete world
   - `cargo make up` - Deploy and start
   - `cargo make verify` - Verify health
   - `cargo make down` - Shutdown

2. **README.md** - Complete project documentation
   - TCPS paradigm explanation
   - Quick start guide
   - Directory structure
   - Generation workflow

---

## 🚀 Quick Start

```bash
# 1. Generate the complete world from ontology
ggen sync

# 2. Deploy and start the world
./world/run/up

# 3. Verify world is healthy
./world/run/verify

# 4. Check status
./world/run/status

# 5. Shutdown
./world/run/down
```

---

## 📊 Generation Statistics

When you run `ggen sync`, it will generate:

- **Rust files**: 8 modules (entities, commands, events, aggregates, handlers, routes, lib, receipts)
- **Tests**: 2 test suites (property tests, integration tests)
- **Infrastructure**: 3 Terraform files (main, variables, outputs)
- **Scripts**: 4 executable scripts (up, down, status, verify)
- **Documentation**: 5 documents (3 C4 diagrams, 2 TOGAF docs)
- **Build manifests**: 1 Cargo.toml

**Total**: 27 generated artifacts from 6 ontology files

---

## 🔒 Sealed Source Principle

**CRITICAL**: Humans CANNOT edit `world/` directory.

- `world/` is generated by `ggen sync`
- Every file has "DO NOT EDIT" header
- To change behavior: Edit `ontology/*.ttl`, run `ggen sync`
- This prevents drift structurally (not just culturally)

### Failure Modes Prevented

1. **Developer shortcut**: "I'll just quickly fix this bug in generated code"
   - **Prevented**: File is sealed, developer forced to edit ontology

2. **Drift accumulation**: Small manual edits accumulate over time
   - **Prevented**: No manual edits possible, ontology is always truth

3. **Documentation rot**: Comments/docs diverge from code
   - **Prevented**: Docs generated from same ontology as code

4. **Test gaps**: Tests don't match implementation
   - **Prevented**: Tests generated from same ontology

---

## 🎯 What Makes This TCPS?

### Toyota Production System Principles Applied to Code

1. **Just-In-Time**: Generate code only when ontology changes
2. **Jidoka (Automation with Human Touch)**: Compiler stops on invalid ontology
3. **Kaizen (Continuous Improvement)**: Ontology is always evolving
4. **Heijunka (Level Production)**: Deterministic generation (same input → same output)
5. **Andon (Stop the Line)**: Quality gates prevent invalid ontology from generating broken systems
6. **Poka-Yoke (Error Proofing)**: Sealed source prevents manual edits structurally

### Graph Truth Production Line

```
┌──────────────┐
│  Ontology    │ ← Humans edit here (RDF/Turtle)
│  (Truth)     │
└──────┬───────┘
       │
       ↓ ggen sync (μ₁-μ₅)
       │
┌──────┴───────┐
│  Generated   │ ← Humans CANNOT edit (sealed)
│  World       │
└──────┬───────┘
       │
       ↓ cargo build
       │
┌──────┴───────┐
│  Binary      │ ← Deploy to production
│  Artifacts   │
└──────────────┘
```

---

## 🧪 Testing Strategy

All tests are generated from ontology:

### 1. Property Tests (proptest)
- Serialize/deserialize round-trips
- Entity invariants hold
- No data loss

### 2. Integration Tests
- HTTP endpoints respond correctly
- Command flows work end-to-end
- Health checks pass

### 3. World Verification
- Service starts successfully
- All endpoints listening
- Tests pass in deployed environment

---

## 📈 Next Steps

This reference implementation demonstrates the **foundation**. To scale to Fortune-5:

1. **Event Store**: Add durable event log (PostgreSQL, EventStoreDB)
2. **Projections**: Add read model projections for CQRS queries
3. **Sagas**: Add long-running business processes
4. **QLever Integration**: Add RDF query federation
5. **Erlang Control Plane**: Add distributed supervision
6. **Multi-Region**: Add geo-replication topology
7. **Receipts Storage**: Add Merkle-linked receipt chain

But the **paradigm is complete** - ontology is truth, code is projection.

---

## 🎓 Learning Resources

- `README.md` - Complete project documentation
- `ontology/` - Read the TTL files, they're the truth
- `ggen.toml` - See how 27 rules generate everything
- `kernel/src/main.rs` - See how minimal the kernel is

---

**Last Updated**: 2026-01-24
**Version**: 1.0.0 (Foundation Complete)
**Status**: Ready for `ggen sync`
