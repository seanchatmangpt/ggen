# ggen-marketplace — Crate Audit

**Path:** `crates/ggen-marketplace/`
**Version:** v3.0.0
**Lines:** 18+ modules, ~555 test markers
**Role:** Package registry with RDF store, SPARQL search, Ed25519 signature verification, trust tiers

---

## CRITICAL: THREE COMPETING ONTOLOGY NAMESPACES

This is the highest-impact issue in the crate. Three incompatible URIs create silent data loss:

| URI | Defined At | Used By |
|-----|-----------|---------|
| `https://ggen.io/marketplace/` | `src/ontology.rs:14` | `v3.rs`, `registry_rdf.rs:26`, `rdf/control.rs` SPARQL, `rdf/sparql.rs` |
| `http://ggen.dev/ontology#` | `src/rdf/ontology.rs:23` | Config TTL files, `rdf/rdf_control.rs`, tests |
| `http://ggen.dev/marketplace#` | `src/rdf/ontology.rs:24` | Only one file |

**The conflict:** `rdf/control.rs` builds URIs like `https://ggen.io/marketplace/<id>` but `rdf/sparql.rs` builds them as `http://ggen.dev/ontology#<id>`. A SPARQL query with one prefix returns zero results against triples inserted with the other. No error. Just empty data.

**Two files export same constant name with different values:**
- `src/ontology.rs:14` → `pub const GGEN = "https://ggen.io/marketplace/"`
- `src/rdf/ontology.rs:23` → `pub const GGEN = "http://ggen.dev/ontology#"`

---

## STUBS

### RdfControlPlane (v2) — `rdf/control.rs`

| Line | Method | Returns |
|------|--------|---------|
| 374 | `get_published_package()` | `Error::NotImplemented` |
| 391 | `search_packages()` | `Vec::new()` |
| 405 | `list_packages()` | `Vec::new()` |
| 419 | `get_dependencies()` | `Vec::new()` |
| 443 | `get_maturity_metrics()` | Hardcoded defaults |
| 463 | `get_dashboard_stats()` | Hardcoded zeros |

### V3OptimizedRegistry — `v3.rs`

| Line | Method | Returns |
|------|--------|---------|
| 249 | `get_package()` | `Err("v3 package reconstruction not yet implemented")` |
| 258 | `get_package_version()` | `Err("v3 version lookup not yet implemented")` |
| 268 | `all_packages()` | `Ok(Vec::new())` |
| 272 | `list_versions()` | `Ok(Vec::new())` |

### Validation — `install.rs`

| Line | Method | Returns |
|------|--------|---------|
| 234 | `check_conflicts()` | Always `Ok` — "would check semantic version constraints" |

### Validation — `validation.rs`

| Line | Method | Returns |
|------|--------|---------|
| 270 | `ReadmeValidator::validate()` | Checks description non-empty — "would check for actual README files" |

### SHACL — `rdf/rdf_control.rs`

| Line | Method | Returns |
|------|--------|---------|
| 217 | SHACL validation | `violations` always `Vec::new()` — "stub" |

---

## DEAD CODE

| File:Line | Item | Status |
|-----------|------|--------|
| `rdf/control.rs:62-74` | `CachedPackage` struct | All fields `#[allow(dead_code)]` — reserved for cache optimization |
| `compatibility.rs.bak2` | Backup file | Already removed in workspace cleanup |

---

## WHAT WORKS WELL

- `Installer` in `install.rs`: Real Ed25519 signature verification (mandatory), SHA-256 digest check, trust tier enforcement, tar.gz/ZIP extraction. Production-wired.
- `StateMachineExecutor` in `rdf/state_machine.rs`: Full 4-state lifecycle (Draft→Published→Deprecated→Yanked) with transition guards. Actively integrated into RdfControlPlane.
- `RdfRegistry` in `registry_rdf.rs`: Working oxigraph-backed CRUD via SPARQL.
- `SearchEngine` in `search.rs`: In-memory multi-field search with Levenshtein fuzzy matching.

---

## FIX / DELETE / REFACTOR

| Action | Item | Priority |
|--------|------|----------|
| **FIX** | Namespace consolidation: pick one URI, update all SPARQL, TTL, and URI constructors | P0 |
| **FIX** | V3 registry: implement package reconstruction from RDF | P1 |
| **FIX** | RdfControlPlane: implement search, list, dependencies | P1 |
| **FIX** | Conflict check: implement semver constraint checking | P2 |
| **FIX** | README validator: check for actual files | P2 |
| **DELETE** | `rdf/rdf_control.rs` v1 RdfControlPlane (shadowed by v2 re-export) | P3 |
| **DELETE** | `CachedPackage` struct if not needed | P3 |
