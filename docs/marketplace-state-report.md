# Marketplace State Report

> **Date:** 2026-03-31
> **Scope:** `ggen-marketplace` crate, CLI commands, tests, and ecosystem integration
> **Branch:** `mcp-server-generation` worktree

---

## Executive Summary

The marketplace is a **self-contained RevOps subsystem** with rich internal architecture (RDF store, SPARQL engine, Ed25519 signing, quality scoring, state machines) but is **almost entirely disconnected from ggen's core sync pipeline**. It has ~289 tests designed but **zero are active** — the entire CLI test suite is disabled. The `ggen-marketplace-tps` crate (GCP Marketplace) was created then fully deleted. Recent activity has been limited to test gating and clippy cleanup.

**Bottom line:** The marketplace is architecturally rich but operationally dead. It was built as a standalone system and never wired into the μ pipeline that drives all other ggen generators.

---

## 1. What EXISTS and WORKS

### Core Library (`crates/ggen-marketplace/src/`, 20 source files)

| Module | Status | Notes |
|--------|--------|-------|
| `lib.rs` | Complete | Exports 16 modules, prelude |
| `error.rs` | Complete | 20+ error variants, `thiserror` |
| `models.rs` | Complete | `PackageId`, `PackageVersion`, `QualityScore` with full validation |
| `traits.rs` | Complete (defs only) | 9 traits defined, most unimplemented |
| `registry.rs` | Complete | DashMap + moka cache, `AsyncRepository` impl |
| `registry_rdf.rs` | Complete | Oxigraph-backed, full `AsyncRepository` impl |
| `search.rs` | Complete | Relevance scoring, fuzzy matching, filters |
| `search_sparql.rs` | Complete | SPARQL-powered search via ontology module |
| `security.rs` | Complete | Ed25519 sign/verify, SHA-256 checksums |
| `builders.rs` | Complete | `PackageBuilder` with required field enforcement |
| `metrics.rs` | Complete | Atomic counters, custom events |
| `migration.rs` | Complete | v1→v2 migration with verification |
| `ontology.rs` | Complete | 20+ SPARQL query templates |
| `rdf/poka_yoke.rs` | Complete | Typestate builders for RDF |
| `rdf/state_machine.rs` | Complete | 4-state lifecycle with guards |
| `rdf/sparql.rs` | Complete | Thread-safe SPARQL executor |
| `rdf/sparql_queries.rs` | Complete | 16 named query methods |
| `rdf/ontology.rs` | Complete | 22 classes, 47 properties |
| `rdf_mapper.rs` | Complete | Bidirectional Package↔RDF |
| `fmea_mitigations.rs` | Complete | 10 failure modes with recovery |
| `rdf/turtle_config.rs` | Partial | Load/save, but line-by-line parsing |

### Unit Tests (within crate)

~90 unit tests across the crate. Most pass. Test quality varies — some test real behavior, some test stubs.

---

## 2. What EXISTS but STUBBED

| Module | What Works | What's Stubbed |
|--------|------------|----------------|
| `install.rs` | Dependency resolution, cycle detection | Actual download/extract is no-op; `check_conflicts()` allows everything |
| `validation.rs` | Pluggable validator framework | `validate_manifest()` always returns score 100 |
| `v3.rs` | Caching infrastructure | All `AsyncRepository` methods return errors/empty |
| `rdf/control.rs` | create/publish work | search/list/validate/dashboard return empty |
| `rdf/rdf_control.rs` | In-memory graph operations | Most operations are stubs |
| `rdf/fmea_mitigations.rs` | 15 failure modes | `attempt_triple_repair()` always returns None; 32 modes missing |
| `rdf/state_machine.rs` | Transitions and guards | `load_from_config()` always returns defaults |
| `models.rs` | Full validation on types | `Draft`/`Published` typestate markers defined but never enforced |

---

## 3. What's MISSING Entirely

### Integration with ggen Core

| Aspect | Status | Impact |
|--------|--------|--------|
| **ggen sync pipeline hooks** | Missing | Marketplace cannot participate in μ₁-μ₅ |
| **SPARQL queries in `ggen-core/queries/`** | Missing | No marketplace queries alongside a2a/mcp |
| **Templates in `ggen-core/templates/`** | Missing | No marketplace templates alongside a2a-rs/mcp-server |
| **`ggen.toml` manifest integration** | Weak | Separate `package.toml` format, not unified |
| **Ontology in `.specify/`** | Missing | Has own `ontology.ttl`, not in `.specify/` |

### Runtime Features

| Feature | Status | Notes |
|---------|--------|-------|
| HTTP/REST API | Missing | axum in deps but no routes |
| Package download | Missing | No download/extract logic |
| Lock file management | Missing | Referenced but not implemented |
| Semantic version resolution | Missing | `check_conflicts()` allows everything |
| Post-install hooks | Missing | Referenced in comments |
| ggen-marketplace-tps | **Deleted** | GCP Marketplace integration removed |

### Tests

| Category | Status |
|----------|--------|
| CLI integration tests (~180 tests) | **All disabled** — parent module comments out all children |
| Install tests (27 tests) | **All `#[ignore]`** — marked for "Phase 2" |
| Domain search tests (27 tests) | **Active but stubbed** — all assert empty results |
| v2 workflow tests (17 tests) | **Gated on `marketplace-v2` feature** — crate doesn't exist |
| Cross-backend tests (13 tests) | **Gated on both v1+v2 features** — impossible to compile |
| Backward compat tests (19 tests) | **Gated on `marketplace-v1` feature** — feature doesn't exist |
| Performance tests (25 tests) | **Disabled** — use `std::thread::sleep` not real ops |
| 4 integration test files | **Declared in Cargo.toml but missing from disk** |
| 3 benchmark files | **Declared in Cargo.toml but missing from disk** |

---

## 4. The Core Problem: Disconnected from Sync

This is the architectural issue that caused the MCP generation drift.

### How Other Generators Work

```
Ontology (.ttl) → ggen sync → queries/*.rq → templates/*.tera → Generated code
```

A2A, YAWL, Craftplan all follow this pattern. They have:
1. SPARQL queries in `crates/ggen-core/queries/<domain>/`
2. Tera templates in `crates/ggen-core/templates/<domain>/`
3. A `ggen.toml` manifest tying them together
4. `ggen sync` orchestrates the pipeline

### How Marketplace Works

```
Marketplace crate → internal RDF store → internal SPARQL → no code generation
```

The marketplace has:
1. Its own internal SPARQL engine (not in `ggen-core/queries/`)
2. No templates (not in `ggen-core/templates/`)
3. Its own ontology (`marketplace/ontology.ttl`, not in `.specify/`)
4. No connection to `ggen sync`

**It's a runtime system, not a code generation system.** But ggen is a code generation tool — everything should flow through the sync pipeline.

---

## 5. Competing Architectures

The crate has **two parallel architectures** that don't communicate:

### Top-level modules (v1-style)
```
registry.rs → DashMap in-memory store
search.rs → In-memory relevance scoring
install.rs → File I/O (stubbed)
```

### rdf/ submodule (v2-style)
```
rdf/registry_rdf.rs → Oxigraph triplestore
rdf/control.rs → RDF control plane
rdf/poka_yoke.rs → Type-safe builders
rdf/state_machine.rs → Lifecycle state machine
rdf/sparql_queries.rs → Named SPARQL methods
```

Plus two competing ontologies:
- `ontology.rs` uses `https://ggen.io/marketplace/`
- `rdf/ontology.rs` uses `http://ggen.dev/ontology#`

And two competing control planes:
- `rdf/control.rs` (oxigraph-backed)
- `rdf/rdf_control.rs` (in-memory graph)

---

## 6. Recent Development Activity

| Date | Commit | Description |
|------|--------|-------------|
| 2026-03-28 | `58a02684` | Gate 500+ broken tests behind `integration` feature flag |
| 2026-03-28 | `9c50733d` | Update template examples to remove SPARQL `?` prefix |
| 2026-03-24 | `13b20a81` | Complete compilation and test suite fixes |
| 2026-03-24 | `cfd5f235` | Fix all clippy warnings in marketplace |
| 2026-03-24 | `bfba703f`-`eb653827` | ~15 commits adding `#[must_use]` annotations |

No feature development since March 24. Only maintenance.

---

## 7. Dependencies

**`ggen-marketplace` is not depended on by any other crate.** It exists in the workspace but nothing uses it. The CLI has no dependency on it.

**Removed:** `ggen-marketplace-tps` was created (commit `ecf21afc`), touched twice, then deleted (commit `aea3e17d`).

---

## 8. What This Means for MCP Generation

The marketplace's disconnection from `ggen sync` is the root cause of the MCP generation drift:

1. **No reference implementation** showing "ontology → sync → generated code" for a complex subsystem
2. **A2A exists** as a reference but is domain-specific (agent-to-agent protocol)
3. **Marketplace should have been** the general-purpose example showing how to:
   - Structure queries in `ggen-core/queries/`
   - Structure templates in `ggen-core/templates/`
   - Wire into `ggen sync` via manifest
   - Handle multiple entity types (packages, versions, dependencies)

Without this reference, the MCP module drifted into hardcoded `format!()` strings instead of using the sync pipeline.

---

## 9. Quantitative Summary

| Metric | Value |
|--------|-------|
| Source files | 20 |
| Lines of code (src/) | ~5,000 |
| Unit tests (in crate) | ~90 (passing) |
| CLI tests (designed) | ~289 |
| CLI tests (active) | **0** |
| SPARQL queries in `ggen-core/queries/` | **0** |
| Templates in `ggen-core/templates/` | **0** |
| Crate dependencies | **0** (nothing depends on it) |
| ggen sync integration | **None** |
| Feature development since March 24 | **None** |
