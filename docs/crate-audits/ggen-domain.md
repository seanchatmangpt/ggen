# ggen-domain — Crate Audit

**Path:** `crates/ggen-domain/`
**Lines:** 32 modules, ~444 test markers
**Role:** Pure business logic, zero CLI deps. Security scanning, config auditing, MAPE-K control loop.

---

## DEAD CODE: AHI Subsystem (~4,200 lines, zero external consumers)

Eight modules define elaborate type-state governance systems that nothing outside `ggen-domain` references:

| File | Lines | Key Types |
|------|:---:|-----------|
| `ahi_contract.rs` | 502 | AHI contract types |
| `auto_promotion_pipeline.rs` | 426 | AutoPromotionPipeline |
| `doctrine_engine.rs` | 366 | DoctrineEngine, DoctrineRule |
| `proof_carrier.rs` | 631 | ProofCarrier, ProofChain |
| `temporal_fabric.rs` | 606 | TemporalFabric, TemporalEvent |
| `capability_system.rs` | 527 | 6 capability structs with `#[allow(dead_code)]` |
| `proof_types.rs` | 552 | WeakDecision, StandardDecision, StrongDecision, CriticalDecision |
| `action_types.rs` | 607 | WeakProof::is_valid() always returns true |

These define "Autonomic Hyper-Intelligence" governance — capability tokens, proof lattices, temporal fabrics, doctrine engines. They are internally consistent type systems with no consumers.

---

## STUBS (functions returning errors or doing nothing)

### Marketplace Module — all functions return errors

| File:Line | Function | Returns |
|-----------|----------|---------|
| `marketplace.rs:90` | `list_all()` | `Err("Marketplace integration moved to ggen-cli")` |
| `marketplace.rs:101` | `get_package()` | `Err("Marketplace integration moved to ggen-cli")` |
| `marketplace.rs:112` | `resolve_dependencies()` | `Err("Marketplace integration moved to ggen-cli")` |
| `marketplace.rs:123` | `execute_install()` | `Err("Marketplace integration moved to ggen-cli")` |

### Packs Subsystem

| File:Line | Function | Returns |
|-----------|----------|---------|
| `packs/compose.rs:57` | `CompositionStrategy::Custom(_)` | `Err("Custom composition strategy not yet implemented")` |
| `packs/generator.rs:70` | Template loop | Logs "Would generate template" — no actual generation |
| `packs/install.rs:51` | `packages_installed` | Always empty Vec — "not fully implemented" |
| `packs/registry.rs:130` | `unpublish()` | Logs warning "not yet fully implemented" |

### Mock Implementations

| File:Line | Function | Returns |
|-----------|----------|---------|
| `action_types.rs:397` | `WeakProof::is_valid()` | Always `true` — "would check expiry, revocation" |
| `graph/export.rs:217-293` | All 5 export functions | Hardcoded example data (`#[allow(dead_code)]`) |

---

## DEAD CODE (additional)

| File:Line | Item | Status |
|-----------|------|--------|
| `capability_system.rs` | 6 capability structs | `granted_at` fields all `#[allow(dead_code)]` |
| `proof_types.rs` | 4 decision types | `decision_id`, `description` fields `#[allow(dead_code)]` |
| `packs/advanced_resolver.rs:18` | `AdvancedResolver.registry_cache` | `#[allow(dead_code)]` — entire struct is shell |
| `packs/template_generator.rs:18` | `TemplateGenerator.tera` | `#[allow(dead_code)]` — Tera engine stored, never used |
| `packs/sparql_executor.rs:55` | `CompiledQuery` struct | `#[allow(dead_code)]` — defined but unused |
| `packs/registry.rs:87` | `PackRegistry.scorer` | `#[allow(dead_code)]` |
| `auto_promotion_pipeline.rs:73` | `AutoPromotionPipeline.scorer` | `#[allow(dead_code)]` |
| `mape_k/execute.rs:113` | Unknown | `#[allow(dead_code)]` |
| `rdf/validation.rs:96,102,110,121` | Validation functions | `#[allow(dead_code)]` |
| `template/list.rs:203` | Unknown | `#[allow(dead_code)]` |

---

## FIX / DELETE / REFACTOR

| Action | Item | Priority |
|--------|------|----------|
| **DELETE** | AHI subsystem: 8 modules, ~4,200 lines dead code | P3 |
| **DELETE** | `marketplace.rs` — all functions error, redirect exists | P3 |
| **DELETE** | `graph/export.rs` 5 hardcoded export functions | P3 |
| **FIX** | Pack generator: implement actual template rendering | P2 |
| **FIX** | Pack install: wire to marketplace crate | P2 |
| **FIX** | WeakProof::is_valid: implement expiry/revocation check | P2 |
| **FIX** | Pack registry unpublish: implement full workflow | P2 |
| **REFACTOR** | Decide if AHI types are needed for future work or should be removed entirely | P3 |
