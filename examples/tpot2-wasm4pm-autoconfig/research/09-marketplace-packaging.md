# Research 09 — Packaging the TPOT2 wasm4pm Auto-Config Generator as a ggen Marketplace Pack

**Agent:** 09 of 10 · **Date:** 2026-06-20 · **Mode:** read-only on the generator; evidence-first; all drafts below are **PROPOSED** (fenced code blocks inside this dossier, NOT live files).

**Question:** How would we package this generator as a distributable ggen marketplace pack? What is the real pack anatomy, the concrete `package.toml`, the registry `index.json` entry, the consumer-usage path, and the blockers?

**TL;DR:**
- A ggen pack is a directory under `marketplace/packages/<name>/` containing a `package.toml` manifest plus the asset dirs (`ontology/`, `queries/`, `templates/`, …). It is registered by appending an entry to `marketplace/registry/index.json` (`packages[]`) and reflected in the inverted `search_index`.
- There are **two distinct `*.toml` shapes** in the live marketplace, and they are NOT interchangeable: the per-package `marketplace/packages/<name>/package.toml` (consumed by `PackageToml::load` for output-key resolution) and the curated bundle/pack manifests under `marketplace/packs/*.toml` (`[pack]` + `[[pack.templates]]`). For *this* generator you want the former.
- **The only part of `package.toml` that the ggen-core pipeline actually reads at sync time is `[outputs]` / `[pack.outputs]`** — and only when a consumer references this pack via `QuerySource::Pack` / `TemplateSource::Pack`. Everything else (`[package]`, `[dependencies]`, `[config]`, tags) is registry/marketplace metadata, not pipeline input.
- **BUG-008/011 footgun is real and load-bearing here:** the `output` key in a consumer's `{ pack, output, file }` reference is resolved by `PackageToml::resolve_output_key`, which returns the mapped dir **or falls back to the key string literally**. So the output keys MUST equal the on-disk directory names exactly (`queries`, `templates`, `ontology`) — and even then, the pack-source path is fragile and effectively untested (no working consumer example exists anywhere in the repo). The robust consumer path remains **direct relative `{ file = ... }`**, exactly as this project's own driver `ggen.toml` already does.

---

## 1. The real anatomy of a ggen pack (cited from live code + real examples)

### 1.1 Two manifest shapes — do not confuse them

| Shape | Location (real) | Schema | Read by |
|-------|-----------------|--------|---------|
| **Per-package manifest** | `marketplace/packages/<name>/package.toml` | `[package]`, `[ontology]`, `[sparql]`, `[templates]`, **`[outputs]`**, `[dependencies]`, `[config]`, `[marketplace]` | Registry tooling for metadata; **`[outputs]`/`[pack.outputs]` read by `ggen-core` `PackageToml::load`** at sync time |
| **Curated pack / bundle manifest** | `marketplace/packs/<name>.toml` | `[pack]` (id/name/version), `packages = [...]`, `[[pack.templates]]`, `[pack.conformance]`, `[pack.metadata]`, `[pack.sparql_queries]` | Marketplace bundle/pack listing (e.g. `lsp-max.toml`, `ggen-pack-contrib.toml`) |

This generator is an **ontology→artifact generator project** (ontology + queries + templates), so the correct vehicle is the **per-package `package.toml`** form (like `database-schema-generator`), NOT the `marketplace/packs/*.toml` bundle form.

### 1.2 The `package.toml` schema, from a real example with the same shape as us

`database-schema-generator` is the closest real analog — it ships an ontology, SPARQL, templates, and an `[outputs]` block. Verbatim from `marketplace/packages/database-schema-generator/package.toml`:

```toml
[package]
name = "database-schema-generator"
version = "1.0.0"
description = "Multi-database schema generator from RDF ontology using SPARQL"
author = "ggen.io"
license = "MIT"
category = "database"
tags = ["database", "schema", "rdf", "sparql", "postgresql", "mysql", "sqlite", "ddl", "migrations"]

[ontology]
file = "ontology/database-schema.ttl"
namespace = "http://ggen.io/ontology/database#"

[sparql]
queries = "sparql/queries.rq"

[templates]
postgresql = "templates/postgresql/schema.sql.tera"
mysql = "templates/mysql/schema.sql.tera"
sqlite = "templates/sqlite/schema.sql.tera"

[outputs]
postgresql = "src/postgresql/"
mysql = "src/mysql/"
sqlite = "src/sqlite/"

[dependencies]
oxigraph = "0.3"
tera = "1.19"
...

[config]
default_database = "postgresql"
enable_migrations = true
...

[marketplace]
production_ready = false
```

> **Critical nuance about `[outputs]` in this real example:** here the keys (`postgresql`, `mysql`, `sqlite`) map to *generated output target dirs* (`src/postgresql/`). That is the human/documentary reading. But the ggen-core code path that *actually consumes* `[outputs]` (`PackageToml::resolve_output_key`) treats them as **input source directories for a consuming pack** (where to find queries/templates *inside this pack*). These two readings collide — the schema is overloaded, and no enforcement disambiguates them. This is the root of BUG-008's confusion (see §5).

### 1.3 What the pipeline ACTUALLY reads from `package.toml`

`crates/ggen-core/src/manifest/types.rs:51-92` defines the only struct ggen-core deserializes from a pack's `package.toml`:

```rust
pub struct PackageToml {
    #[serde(default)] pub pack: Option<PackSection>,   // reads [pack.outputs]
    #[serde(default)] pub outputs: std::collections::HashMap<String, String>,  // reads [outputs]
}
// resolve_output_key: prefer top-level [outputs], then [pack.outputs], else return the key string LITERALLY.
```

`PackageToml::load` (`types.rs:72-78`) reads `<pack_root>/package.toml` and **silently returns `Default` (empty outputs) if the file is missing or unparseable** — a fail-open path that falls back to literal keys. So:

- ggen-core ignores `[package]`, `[ontology]`, `[sparql]`, `[templates]`, `[dependencies]`, `[config]`, `[marketplace]` entirely when loading a pack. Those are registry-listing metadata.
- ggen-core reads **only** `[outputs]` (and `[pack.outputs]` as fallback) — and only when a consumer ggen.toml references the pack via a `Pack` source.

### 1.4 Variations seen across real packages (the schema is loose)

`#[serde(deny_unknown_fields)]` is NOT on `PackageToml`, so packs vary wildly:
- `io.ggen.nextjs.ontology-crud/package.toml`: uses `id` in `[package]`, plus `[metadata]`, `[features]`, `[structure]`, `[generation]`, `[examples]`, `[installation]`.
- `sparql-cli/package.toml`: is essentially a **Cargo.toml** (`[package]` + `[dependencies]` + `[[bin]]` + `[profile.*]`) with `[package.metadata] ontology = "rdf/ontology.ttl"` and `[marketplace]`.
- `hello-world/package.toml`: `[package]` + `[install]` + `[files]` + `[variables]` + `[examples]` + `[docs]`.

**Conclusion:** there is no single enforced `package.toml` schema. The de-facto contract for a generator-style pack like ours is: `[package]` (name/version/description/author/license/category/tags), an `[ontology]`/`[sparql]`/`[templates]` descriptive block, an `[outputs]` block (the only pipeline-load-bearing part), and `[marketplace] production_ready`.

### 1.5 PackageId validation (naming constraint)

`crates/ggen-marketplace/src/marketplace/models.rs:176-213` — `PackageId::new` lowercases the input and rejects: empty, >200 chars, any char other than `[a-z0-9_-]`, or leading/trailing hyphen. **`tpot2-wasm4pm-autoconfig` is valid** (all lowercase alnum + hyphens, 24 chars, no leading/trailing hyphen).

### 1.6 Draft/Published typestate (publishing lifecycle)

`models.rs:16-32`:
```rust
pub struct Draft;       // typestate marker for draft packages
pub struct Published;   // typestate marker for published packages
// PackageState enum {Draft,Published,Deprecated,Yanked} kept only for test back-compat (deprecated).
```
The publishing flow is modeled as a compile-time typestate transition `Draft → Published` (CLAUDE.md "Typestate / `ggen-marketplace`"). Practically, "publishing" in this repo's filesystem registry = adding the package dir + appending the `index.json` entry; there is no live network registry round-trip in-repo.

### 1.7 The registry index entry (real schema)

`marketplace/registry/index.json` top-level keys (verified via parse): `version`, `registry_url`, `updated_at`, `package_count`, `categories` (map: category→count), `packages` (array of entries), `search_index` (inverted index: keyword→[package names]), `o_crates`, `o_crate_count`.

A real `packages[]` entry (verbatim, `database-schema-generator`):
```json
{
  "name": "database-schema-generator",
  "version": "1.0.0",
  "category": "database",
  "description": "Multi-database schema generator from RDF ontology using SPARQL",
  "tags": ["database","schema","rdf","sparql","postgresql","mysql","sqlite","ddl","migrations"],
  "keywords": [],
  "author": "ggen.io",
  "license": "MIT",
  "downloads": 0,
  "stars": 0,
  "production_ready": false,
  "dependencies": [],
  "path": "marketplace/packages/database-schema-generator",
  "download_url": "https://github.com/seanchatmangpt/ggen/archive/refs/heads/master.zip",
  "checksum": "44d66e3dceb9f1ac048f307ea6809502c5da0a9af04027e3bc9242b1662c0125"
}
```

There is also a second, simpler `marketplace/index.json` (a flat list of 76 entries with `{id,name,version,path,description,author,license,category}`) — a lighter mirror. The authoritative registry is `marketplace/registry/index.json` (113 KB, 77 packages, with `search_index`).

---

## 2. PROPOSED `package.toml` for `tpot2-wasm4pm-autoconfig`

**This is a DRAFT (PROPOSED). Do not create it as a live file.** It would live at `marketplace/packages/tpot2-wasm4pm-autoconfig/package.toml` after copying the generator project's assets (`ontology/`, `queries/`, `inference/`, `templates/`, plus the driver `ggen.toml`) into that package dir.

**BUG-008/011 invariant honored:** every key in `[outputs]` equals an on-disk directory name **exactly** (verified against the live tree: the project's dirs are literally `ontology/`, `queries/`, `inference/`, `templates/`). Because `resolve_output_key` falls back to the literal key when no mapping exists, mapping `queries = "queries/"` is belt-and-suspenders — but it makes the intent explicit and survives a future rename of the on-disk dir.

```toml
# PROPOSED — marketplace/packages/tpot2-wasm4pm-autoconfig/package.toml
# Packaging the TPOT2 genetic-programming auto-config generator over the wasm4pm
# 60-algorithm process-mining registry. Assets (ontology/queries/inference/templates)
# are copied verbatim from examples/tpot2-wasm4pm-autoconfig/.

[package]
name = "tpot2-wasm4pm-autoconfig"
version = "0.1.0"
description = "TPOT2 genetic-programming auto-configuration over the wasm4pm 60-algorithm process-mining registry: SPARQL-derived, Pareto-optimal pipeline selection that emits a ready-to-run ggen.toml."
author = "ggen tpot2 example"
license = "MIT"
category = "process-mining"
tags = [
  "tpot2", "automl", "process-mining", "wasm4pm", "pareto",
  "rdf", "sparql", "genetic-programming", "code-generation", "ggen",
]
keywords = [
  "tpot2", "automl", "nsga2", "process-mining", "pareto-front",
  "pipeline-optimization", "sparql", "ontology-driven",
]

# --- Descriptive blocks (registry metadata; NOT read by the ggen-core pipeline) ---
[ontology]
# Primary search-space vocabulary + the imported registries.
file = "ontology/tpot-search-space.ttl"
namespace = "https://wasm4pm.dev/tpot#"
imports = [
  "ontology/algorithms.ttl",   # 60 pi:ProcessIntelligenceAlgorithm individuals
  "ontology/breeds.ttl",       # 55 compat: cognition breeds (GP meta-strategy)
  "ontology/tpot-shapes.ttl",  # SHACL shapes for the tpot: vocabulary
]

[sparql]
# Human-facing listing of the SELECT/CONSTRUCT assets (descriptive only).
extract_operators        = "queries/extract-operators.rq"
extract_pipeline_stages  = "queries/extract-pipeline-stages.rq"
extract_pareto_pipeline  = "queries/extract-pareto-pipeline.rq"
extract_pareto_front     = "queries/extract-pareto-front.rq"
extract_hyperparameters  = "queries/extract-hyperparameters.rq"
extract_fitness_objectives = "queries/extract-fitness-objectives.rq"
derive_operators         = "inference/derive-operators.rq"
derive_pareto_dominance  = "inference/derive-pareto-dominance.rq"

[templates]
# Descriptive listing of the Tera batch templates.
generated_ggen_toml = "templates/generated-ggen-toml.tera"
tpot_config_dict    = "templates/tpot-config-dict.py.tera"
pipeline_manifest   = "templates/pipeline-manifest.json.tera"
search_space_report = "templates/search-space-report.md.tera"

# -----------------------------------------------------------------------------
# [outputs] — THE ONLY BLOCK ggen-core ACTUALLY READS (PackageToml::resolve_output_key).
# These are the named SOURCE directories a CONSUMING ggen.toml can reference via
# QuerySource::Pack / TemplateSource::Pack { pack, output, file }.
#
# *** BUG-008/011 INVARIANT ***  Each KEY MUST EQUAL the on-disk directory name
# EXACTLY. resolve_output_key returns the mapped value if present, otherwise the
# key string LITERALLY, and the pipeline then does pack_root.join(<key-or-value>).
# So `output = "queries"` resolves to "<pack_root>/queries/" here — correct only
# because the dir is literally named "queries". A trailing-slash or alias mismatch
# silently changes the path. Verified on-disk dirs: ontology/ queries/ inference/ templates/.
# -----------------------------------------------------------------------------
[outputs]
ontology  = "ontology/"
queries   = "queries/"
inference = "inference/"
templates = "templates/"

# --- Dependencies are documentation-only here (the generator runs no Rust) ---
[dependencies]
# This pack ships RDF + SPARQL + Tera consumed by `ggen sync`; no crate deps required.

[config]
# Frozen generation invariants (CONTRACT.md §5). Descriptive metadata.
lambda_cost = 0.5
random_seed = 42
population_size = 100
generations = 50
selection_strategy = "NSGA2"
algorithm_count = 60
stage_count = 9

[marketplace]
# Honest: `ggen sync` was NOT executed in the build container (no cargo/ggen
# binary). Verification is structural + a pure-Python reference. Mark accordingly.
production_ready = false
```

### Why this layout, and the alternative

- **Why per-package form, not `marketplace/packs/*.toml`:** the `[pack]` + `[[pack.templates]]` bundle form (e.g. `ggen-pack-contrib.toml`) is for curated template bundles where each template is independently invoked by name; it does not carry an ontology + a driver `ggen.toml` pipeline. Our deliverable *is* a full ontology→artifact pipeline, which matches `database-schema-generator`'s per-package shape.
- **The driver `ggen.toml` ships inside the package too.** The simplest consumer story is "install the pack, `cd` into it, run `ggen sync`" — exactly how `database-schema-generator` and the `examples/` projects work. The `[outputs]` block exists so that a *different* project could also pull our `queries/`/`templates/` as pack sources, but that path is the buggy one (§5).

---

## 3. PROPOSED registry `index.json` entry

**DRAFT (PROPOSED).** Append this object to `packages[]` in `marketplace/registry/index.json`, bump `package_count` to 78, add `"process-mining": 1` to `categories` (it is a new category), and extend `search_index` for each new keyword (e.g. `"tpot2": [...,"tpot2-wasm4pm-autoconfig"]`). `checksum` is a placeholder — it is the SHA-256 the registry tooling computes over the package dir at publish time (the field is populated by the indexer, not hand-authored; real entries show 64-hex digests).

```json
{
  "name": "tpot2-wasm4pm-autoconfig",
  "version": "0.1.0",
  "category": "process-mining",
  "description": "TPOT2 genetic-programming auto-configuration over the wasm4pm 60-algorithm process-mining registry: SPARQL-derived, Pareto-optimal pipeline selection that emits a ready-to-run ggen.toml.",
  "tags": [
    "tpot2", "automl", "process-mining", "wasm4pm", "pareto",
    "rdf", "sparql", "genetic-programming", "code-generation", "ggen"
  ],
  "keywords": [
    "tpot2", "automl", "nsga2", "process-mining", "pareto-front",
    "pipeline-optimization", "sparql", "ontology-driven"
  ],
  "author": "ggen tpot2 example",
  "license": "MIT",
  "downloads": 0,
  "stars": 0,
  "production_ready": false,
  "dependencies": [],
  "path": "marketplace/packages/tpot2-wasm4pm-autoconfig",
  "download_url": "https://github.com/seanchatmangpt/ggen/archive/refs/heads/master.zip",
  "checksum": "<sha256-computed-by-indexer-over-package-dir>"
}
```

Companion (lighter mirror) entry for `marketplace/index.json` (the flat list form), if both indexes are kept in sync:

```json
{
  "id": "tpot2-wasm4pm-autoconfig",
  "name": "tpot2-wasm4pm-autoconfig",
  "version": "0.1.0",
  "path": "packages/tpot2-wasm4pm-autoconfig",
  "description": "TPOT2 genetic-programming auto-configuration over the wasm4pm 60-algorithm process-mining registry.",
  "author": "ggen tpot2 example",
  "license": "MIT",
  "category": "process-mining"
}
```

---

## 4. How a consumer actually consumes this pack

### 4.1 The recommended path (works today): install + sync-in-place

This mirrors the real `hello-world` and `database-schema-generator` UX (`ggen market install ...`, then run the project's own pipeline):

```bash
# 1. Install the pack from the marketplace registry into a working dir
ggen market install tpot2-wasm4pm-autoconfig
cd tpot2-wasm4pm-autoconfig            # the pack ships its own driver ggen.toml

# 2. Run the auto-config pipeline (uses the pack's bundled ggen.toml verbatim —
#    direct relative { file = "queries/..." } / { file = "templates/..." } sources)
ggen sync --audit true

# 3. Inspect the generated artifacts (all under generated/)
cat generated/ggen.toml          # the AUTO-SELECTED Pareto-optimal pipeline
cat generated/pipeline.json      # 9-stage elite pipeline as JSON
cat generated/tpot_config.py     # full 60-operator TPOT2 config_dict
cat generated/SEARCH_SPACE.md
```

This is robust because the bundled `ggen.toml` uses **direct relative file paths** for every query/template (CONTRACT.md §8: "Use `{ file = "queries/x.rq" }` direct relative paths — NOT pack indirection (BUG-008/011)"). No pack-source resolution is exercised → no footgun.

### 4.2 The pack-source path (FRAGILE — for completeness, NOT recommended)

If a *different* project wanted to reuse our `queries/` and `templates/` without copying them, it would declare us as a pack and reference our outputs. **PROPOSED consumer ggen.toml fragment (do not rely on this — see §5):**

```toml
# In SOME OTHER project's ggen.toml:
[[packs]]
name = "tpot2-wasm4pm-autoconfig"
registry = "local"                                  # types.rs PackRef: registry defaults to "local"
path = "../packs/tpot2-wasm4pm-autoconfig"          # local pack root (must contain package.toml)

[[generation.rules]]
name = "reuse-pareto-pipeline"
# Pipeline calls PackageToml::resolve_output_key("queries") -> "queries/" (our [outputs])
# -> pack_root.join("queries/").join("extract-pareto-pipeline.rq")
query    = { pack = "tpot2-wasm4pm-autoconfig", output = "queries",   file = "extract-pareto-pipeline.rq" }
template = { pack = "tpot2-wasm4pm-autoconfig", output = "templates", file = "generated-ggen-toml.tera" }
output_file = "ggen.toml"
mode = "Overwrite"
```

Resolution trace (from `crates/ggen-core/src/codegen/pipeline.rs:796-833` + `707-722`):
1. `QuerySource::Pack { pack, output, file }` → find `pack` in `[[packs]]` (error if absent: *"pack '…' not declared in [[packs]]"*).
2. `pack_ref.path` must be `Some` (error if `registry != "local"` with no path: *"pack '…' has no local path"*).
3. `resolve_pack_output_static` → `PackageToml::load(pack_root)` → `resolve_output_key("queries")` → `"queries/"` (our `[outputs]`) → `pack_root.join("queries/")`.
4. `pack_dir.join("extract-pareto-pipeline.rq")` → `fs::read_to_string` (error if file missing).

The ontology itself cannot be pulled via `output` — `[ontology] imports` only accepts file paths, not pack-output references (there is no `OntologyConfig::Pack` variant in `types.rs:178-197`). A reusing consumer would still need a local/relative copy of the four TTLs. This is a structural limit, not a bug.

---

## 5. Blockers — the BUG-008/011 footgun and others

### 5.1 BUG-008 — `[pack.outputs]` indirection is the key, not an alias (FOOTGUN)

`docs/jira/WASM4PM-DISCOVERED-BUGS.md` BUG-008, corroborated by `manifest/types.rs:81-91` (`resolve_output_key`) and `codegen/pipeline.rs:704-722` (`resolve_pack_output_static`):

> "treats `[pack.outputs]` key names as literal directory paths relative to the pack root, with no mapping step. … The key is not an alias — it IS the directory name fragment."

Concretely: `resolve_output_key(key)` returns the mapped value **or the key string itself** when unmapped, then the pipeline does `pack_root.join(<that string>).join(file)`. **Mitigation in our `package.toml` (§2):** every `[outputs]` key equals the exact on-disk dir name (`queries`, `templates`, `ontology`, `inference`), and is mapped to the trailing-slash dir. So `output = "queries"` resolves correctly *both* via the explicit map *and* via the literal fallback. A consumer who writes `output = "query"` or `output = "queries/"` (with slash baked into the reference) gets a wrong path silently.

### 5.2 BUG-011 — pack-sourced query/template can silently produce no output (SILENT-DATA-LOSS)

BUG-011 (same doc) + `crates/ggen-lsp/src/rule_index.rs:98-106`: ggen-lsp records pack queries as an issue and returns empty `query_content`, so **no SELECT projection is parsed** → GGEN-TPL-001/OUT-001 provision checks are skipped, and if the path is wrong the rule is skipped "as if `skip_empty = true`." There is **no working consumer example of `{ pack, output, file }` anywhere in the repo** (verified: zero `output = "queries"` / `QuerySource::Pack` usages across `examples/*/ggen.toml` and `marketplace/packages/*/ggen.toml`). The pack-source path is effectively untested in practice.

**Workaround (BUG-011, confirmed working):** "Use direct relative paths." → This is exactly why our bundled driver `ggen.toml` ships with `{ file = "queries/..." }`, and why §4.1 (install + sync-in-place) is the recommended consumer path. **Do not ship a consumer story that depends on §4.2.**

### 5.3 Secondary blockers / honest caveats

- **`ggen sync` was never executed for this project** (INTEGRATION_REPORT §6; CONTRACT.md §9: no cargo/ggen binary in the container). Therefore `production_ready = false` is the only honest marketplace flag, and **no `.ggen/receipts/*.json` should be fabricated** for the pack. A real publish requires running the pipeline once on a cargo-capable host to confirm the 6 generation rules produce the expected `generated/` artifacts.
- **`checksum` is computed by the indexer, not hand-authored.** Real entries carry a 64-hex SHA-256. Hand-writing a wrong checksum would fail integrity verification at install time. Leave it to the registry tooling (placeholder in §3).
- **Two registries to keep in sync.** `marketplace/registry/index.json` (authoritative, with `search_index`) AND `marketplace/index.json` (flat mirror) both list packages; a publish must update both (or the chosen canonical one) and the `categories` count + `search_index` inverted lists. `process-mining` is a brand-new category.
- **Ontology cannot be delivered as a pack output.** `[ontology] imports` accepts file paths only — a reusing consumer must keep a local/relative copy of the four TTLs (§4.2). Not a bug, but a packaging constraint to document.
- **`package.toml` has no enforced schema** (`PackageToml` lacks `deny_unknown_fields`, and `PackageToml::load` is fail-open to `Default` on parse error — `types.rs:72-78`). A typo in `[outputs]` will NOT error; it silently yields empty outputs and literal-key fallback. Lint the pack manually before publishing.

---

## 6. Evidence index (every claim is sourced)

| Claim | Source |
|-------|--------|
| `PackageToml` reads only `[outputs]`/`[pack.outputs]`; fail-open to Default | `crates/ggen-core/src/manifest/types.rs:51-92` |
| `resolve_output_key` prefers top-level, then `[pack.outputs]`, else literal key | `crates/ggen-core/src/manifest/types.rs:81-91` (+ unit test `test_package_toml_resolve_output_key` :539-566) |
| `QuerySource::Pack { pack, output, file }` / `TemplateSource::Pack {...}` variants | `crates/ggen-core/src/manifest/types.rs:301-376` |
| Pack-output resolution path (`resolve_pack_output_static` → `PackageToml::load` → join) | `crates/ggen-core/src/codegen/pipeline.rs:704-722` |
| Pack query/template read sites + "not declared in [[packs]]" / "no local path" errors | `crates/ggen-core/src/codegen/pipeline.rs:796-833, 936-975` |
| `PackRef` schema (`name`,`registry` default "local",`path`,`version`) | `crates/ggen-core/src/manifest/types.rs:26-47` |
| Real per-package `package.toml` with `[ontology]/[sparql]/[templates]/[outputs]` | `marketplace/packages/database-schema-generator/package.toml` |
| `[[pack.templates]]` bundle form (the OTHER shape) | `marketplace/packs/ggen-pack-contrib.toml`, `marketplace/packs/lsp-max.toml` |
| Registry `index.json` schema + real entry + `search_index` inverted index | `marketplace/registry/index.json` (top keys: version/registry_url/updated_at/package_count/categories/packages/search_index/o_crates) |
| Flat mirror index | `marketplace/index.json` (76 entries, `{id,name,version,path,...}`) |
| `PackageId` validation (lowercase, `[a-z0-9_-]`, no leading/trailing hyphen) | `crates/ggen-marketplace/src/marketplace/models.rs:176-213` |
| `Draft`/`Published` typestate markers | `crates/ggen-marketplace/src/marketplace/models.rs:16-32` |
| BUG-008 (output key is the dir name, not an alias) | `docs/jira/WASM4PM-DISCOVERED-BUGS.md` BUG-008 |
| BUG-011 (pack source silently produces no output; workaround = direct paths) | `docs/jira/WASM4PM-DISCOVERED-BUGS.md` BUG-011 + `crates/ggen-lsp/src/rule_index.rs:98-106` |
| On-disk dir names are exactly `ontology/ queries/ inference/ templates/` | `ls examples/tpot2-wasm4pm-autoconfig/` (this session) |
| Driver `ggen.toml` already uses direct relative `{ file = ... }` (avoids BUG-008/011) | `examples/tpot2-wasm4pm-autoconfig/ggen.toml:158-229`; CONTRACT.md §8 |
| `ggen sync` not run; no receipts fabricated; `production_ready=false` honest | `INTEGRATION_REPORT.md` §6; `CONTRACT.md` §9 |
| No working `{ pack, output, file }` consumer example in repo | grep over `examples/*/ggen.toml` + `marketplace/packages/*/ggen.toml` (zero hits) |

---

## 7. Recommendation (bottom line)

1. **Package as a per-package pack** at `marketplace/packages/tpot2-wasm4pm-autoconfig/` using the §2 `package.toml` (modeled on `database-schema-generator`), copying `ontology/ queries/ inference/ templates/` + the driver `ggen.toml`.
2. **Register** via the §3 `packages[]` entry in `marketplace/registry/index.json` (+ flat mirror), letting the indexer compute the checksum; add the new `process-mining` category and `search_index` keywords.
3. **Ship the install + sync-in-place consumer story** (§4.1) only. The bundled `ggen.toml` already uses direct relative `{ file = ... }` sources, sidestepping BUG-008/011 entirely.
4. **Do not advertise pack-source reuse** (§4.2) as a supported path until BUG-008/011 are fixed (GGEN-PACK-001 diagnostic + a `package.toml` schema lint). If exposed, the `[outputs]` keys MUST equal on-disk dir names exactly — which §2 honors.
5. **Keep `production_ready = false`** until a cargo-capable host runs `ggen sync` and confirms the 6 generation rules emit `generated/` artifacts with a real receipt. Do not fabricate a receipt to flip the flag.
