# Marketplace Current State Report — ggen v6.0.1

*Generated: 2026-03-31*

---

## 1. Executive Summary

The marketplace has **rich infrastructure** (RDF-backed registry, SPARQL search, dependency resolution, Ed25519 signing, quality scoring) but is **not wired to the CLI or the sync pipeline**. The `ggen marketplace *` commands were removed in v5.0. The `ggen packs *` commands are stubs that return empty data. No pack can be installed, and no marketplace template is used by `ggen sync`.

**Bottom line:** The engine is built. The transmission is not connected.

---

## 2. Inventory

### Packages: 88

| Category | Count | Examples |
|----------|-------|---------|
| Healthcare | 7 | telemedicine, ehr-integration, pharmacy-management, clinical-trials |
| Finance | 7 | banking-core, trading-platform, iso-20022-payments, robo-advisor |
| Enterprise | 8 | enterprise-erp-core, crm, identity-access-management, multi-tenant-saas |
| AI Agents | 5 | agent-cli-copilot, agent-memory-forge, agent-reasoning-mcp, agent-context-crafter |
| Academic | 7 | phd-thesis-template, neurips-paper-template, ieee-paper-template, arxiv-paper |
| CLI Tools | 3 | sparql-cli, shacl-cli, reasoner-cli |
| Quality Gates | 3 | armstrong-compliance-dod, three-layer-verification-dod, wvda-soundness-dod |
| Java 26 Patterns | 5 | sealed-domain, virtual-thread-dao, pattern-matched-controller, record-value-object |
| Sectors (80/20) | 5 | api-gateway, observability, rust-microservice, paper-lifecycle, support-hooks |
| Other | 38 | rest-api, graphql, microservices, docker, data-pipeline, etc. |

### Packs: 5 (curated bundles)

| Pack | Packages | Category |
|------|----------|----------|
| startup-essentials | agent-cli-copilot, rest-api-template, cli-application-template | startup |
| enterprise-backend | microservices-architecture, rest-api, graphql, api-gateway | enterprise |
| web-fullstack | rest-api-template, graphql-api-template | web |
| data-science-toolkit | data-pipeline-cli, ai-microservice | ml |
| devops-automation | cicd-pipeline-generator, docker-compose-template | devops |

### Receipts: 66 packages have validation receipts (quality guard scores)

### Registry: `marketplace/registry/index.json` — only 6 packages indexed (stale)

---

## 3. Crate Architecture (`ggen-marketplace` v3.0.0)

**Builds clean.** All types compile. 16 modules.

| Module | Status | What It Does |
|--------|--------|-------------|
| `models.rs` | ✅ Complete | PackageId, PackageVersion, QualityScore, Package, Manifest, InstallationManifest, SearchResult |
| `registry.rs` | ✅ Complete | In-memory DashMap + moka LRU cache. AsyncRepository trait impl |
| `search.rs` | ✅ Complete | Full-text + fuzzy (Levenshtein) + relevance ranking + filters |
| `search_sparql.rs` | ✅ Complete | SPARQL-based semantic package discovery via oxigraph |
| `install.rs` | ⚠️ Placeholder | Dependency resolution works. Actual download/extract is stubbed |
| `ontology.rs` | ✅ Complete | RDF namespace + classes + properties for marketplace domain |
| `rdf_mapper.rs` | ✅ Complete | Package ↔ RDF triple bidirectional mapping |
| `v3.rs` | ✅ Complete | V3OptimizedRegistry — multi-layer cache, <100ms lookup SLO |
| `validation.rs` | ✅ Complete | Pluggable validation framework with weighted scoring |
| `security.rs` | ✅ Complete | Ed25519 signing/verification, SHA-256 checksums |
| `builders.rs` | ✅ Complete | Type-safe PackageBuilder with compile-time validation |
| `traits.rs` | ✅ Complete | AsyncRepository, Installable, Queryable, Validatable, Signable, Observable, Cache, Builder, Filter, Ranker |
| `registry_rdf.rs` | ✅ Complete | RDF-backed registry with SPARQL queries |
| `metrics.rs` | ✅ Complete | Observability / metrics |
| `migration.rs` | ✅ Complete | Schema migration support |
| `error.rs` | ✅ Complete | Error types |

---

## 4. CLI Status

| Command | Status | Notes |
|---------|--------|-------|
| `ggen marketplace search` | ❌ Removed | Comment says "Add back in v5.1+" |
| `ggen marketplace install` | ❌ Removed | Same |
| `ggen marketplace list` | ❌ Removed | Same |
| `ggen marketplace publish` | ❌ Removed | Same |
| `ggen packs list` | ⚠️ Stub | Returns empty list |
| `ggen packs check_compatibility` | ⚠️ Stub | Always returns "compatible" |
| `ggen packs install` | ❌ Doesn't exist | No CLI entry point |
| `ggen new --pack <name>` | ❌ Doesn't exist | No CLI entry point |

---

## 5. Template Engine Disconnect

| System | Engine | Location | Used By |
|--------|--------|----------|---------|
| Marketplace packages | Handlebars (.hbs) | `marketplace/packages/*/templates/` | Nothing — no CLI calls these |
| ggen sync | Tera (.tera/.tmpl) | `templates/`, `crates/*/templates/` | `ggen sync` pipeline |
| gpack templates | Tera (.tera/.tmpl) | `templates/*/gpack.toml` | FileTreeGenerator (unused) |

The marketplace's Handlebars templates and the core's Tera templates are **two separate universes**. No code bridges them.

---

## 6. What's Missing (The Wiring)

1. **No install path** — `Installer.install()` is a placeholder. No code downloads, extracts, or installs a package.
2. **No pack → sync bridge** — A pack's templates/rules don't feed into `ggen sync`. The generation rules in `ggen.toml` are hardcoded per-project.
3. **No template engine unification** — Marketplace uses Handlebars, core uses Tera. They don't interoperate.
4. **No CLI surface** — All marketplace commands removed or stubbed.
5. **No lockfile population** — `LockfileManager` exists but nothing writes to it.
6. **No cache population** — `CacheManager` exists but nothing populates it.
7. **Registry is stale** — `index.json` has 6 entries for 88 packages.

---

## 7. What EXISTS and Is Reusable

| Component | Location | Status |
|-----------|----------|--------|
| RDF-backed package model | `ggen-marketplace::models` | ✅ Production-quality |
| SPARQL search engine | `ggen-marketplace::search_sparql` | ✅ Works |
| Fuzzy/full-text search | `ggen-marketplace::search` | ✅ Works |
| Dependency resolver | `ggen-marketplace::install` | ✅ Logic works, I/O stubbed |
| Quality validation framework | `ggen-marketplace::validation` | ✅ Works |
| Cryptographic signing | `ggen-marketplace::security` | ✅ Works |
| Template resolver (pack-scoped) | `ggen-core::resolver` | ✅ Supports `pack_id:template_path` |
| File tree generator | `ggen-core::templates` | ✅ Works |
| Template cache (LRU 5000) | `ggen-core::template_cache` | ✅ Works |
| Lockfile manager | `ggen-core` | ✅ Schema exists |
| Cache manager | `ggen-core` | ✅ Schema exists |
| 88 package manifests | `marketplace/packages/*/package.toml` | ✅ Complete |
| 5 pack definitions | `marketplace/packs/*.toml` | ✅ Complete |
| 66 validation receipts | `marketplace/receipts/` | ✅ Complete |

---

## 8. The Core Question

**"What happens when I want to use 20 different sets of templates?"**

Right now: **nothing happens.** The infrastructure to answer this question is 80% built but 0% wired.

The `TemplateResolver` already supports `pack_id:template_path` resolution. The `FileTreeGenerator` already renders directory trees from templates. The pack format already defines variables and dependencies. The marketplace crate already has search, install, and dependency resolution logic.

What's missing is the **connective tissue** — a CLI command that:
1. Finds a pack (local or remote)
2. Installs its templates into `.ggen/packs/<pack_id>/`
3. Makes those templates available to `ggen sync` or `ggen new`
4. Records the installation in the lockfile

That's the last mile. Everything behind it is built.
