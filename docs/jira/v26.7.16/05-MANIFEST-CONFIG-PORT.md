# Manifest/Config Port

Part of [00-OVERVIEW](00-OVERVIEW.md) — Phase 2, depends on
[01-PUBLISH-SAFETY-AND-CRATE-RENAME](01-PUBLISH-SAFETY-AND-CRATE-RENAME.md). Blocks
[09-GGEN-LSP-MIGRATION](09-GGEN-LSP-MIGRATION.md) entirely — every file in that ticket needs
this to land first.

## File reference table

### Source (ggen-core, to be ported)

| Path | LOC | Relevant lines |
|---|---:|---|
| `/Users/sac/ggen/crates/ggen-core/src/manifest/parser.rs` | 170 | `pub struct ManifestParser;` at line 11; `ManifestParser::parse` function signature at line 32, body closes at line 44 |
| `/Users/sac/ggen/crates/ggen-core/src/manifest/types.rs` | 573 | `GgenManifest`, `GenerationRule`, `QuerySource`, `TemplateSource`, `GenerationMode`, `ValidationRule`, `PackRef`, `PackageToml`, etc. |
| `/Users/sac/ggen/crates/ggen-core/src/manifest/mod.rs` | 49 | `pub mod validation;` at line 41; `pub use validation::{query_contains_values, ManifestValidator};` at line 49 |
| `/Users/sac/ggen/crates/ggen-core/src/manifest/validation.rs` | 305 | `query_has_order_by` defined at line 243, used at line 196; `query_contains_values` defined at line 250, used at line 145 |

### Destination (ggen-config, new module)

| Path | Status |
|---|---|
| `/Users/sac/ggen/crates/ggen-config/src/manifest/parser.rs` | new file |
| `/Users/sac/ggen/crates/ggen-config/src/manifest/types.rs` | new file |
| `/Users/sac/ggen/crates/ggen-config/src/manifest/mod.rs` | new file |
| `/Users/sac/ggen/crates/ggen-config/src/manifest/validation.rs` | new file |
| `/Users/sac/ggen/crates/ggen-config/src/lib.rs` | existing, 32 lines — add `pub mod manifest;` |

Confirmed: `find /Users/sac/ggen/crates/ggen-config/src -iname "*manifest*"` returns nothing
today — this is entirely net-new in `ggen-config`, not a re-point of existing code.

### Consumers to re-point (ggen-lsp)

| Path | LOC | Import to change |
|---|---:|---|
| `/Users/sac/ggen/crates/ggen-lsp/src/project_index.rs` | 302 | line 10: `ManifestParser` |
| `/Users/sac/ggen/crates/ggen-lsp/src/rule_index.rs` | 400 | line 17: `GenerationRule, QuerySource, TemplateSource`; line 195: `GenerationMode` (test only) |
| `/Users/sac/ggen/crates/ggen-lsp/src/analyzers/sparql_analyzer.rs` | 349 | line 16: `query_contains_values, query_has_order_by` |

### Diagnostic engine files (zero ggen-core coupling, listed for completeness)

| Path | LOC |
|---|---:|
| `/Users/sac/ggen/crates/ggen-lsp/src/route/diagnostic_species.rs` | 299 |
| `/Users/sac/ggen/crates/ggen-lsp/src/analyzers/tera_analyzer.rs` | 1,160 |
| `/Users/sac/ggen/crates/ggen-lsp/src/analyzers/mod.rs` | 587 |
| `/Users/sac/ggen/crates/ggen-lsp/src/analyzers/harness_analyzer.rs` | 175 |
| `/Users/sac/ggen/crates/ggen-lsp/src/analyzers/source_law_analyzer.rs` | 143 |
| `/Users/sac/ggen/crates/ggen-lsp/src/analyzers/toml_analyzer.rs` | 356 |

**Path correction**: `harness_analyzer.rs`, `source_law_analyzer.rs`, and `toml_analyzer.rs`
all live under `/Users/sac/ggen/crates/ggen-lsp/src/analyzers/` — not directly under
`ggen-lsp/src/` as an earlier draft of this ticket implied.

## The gap

Confirmed `ggen-config` has **zero** manifest module today. `ggen_core::manifest::
{ManifestParser, GenerationRule, QuerySource, TemplateSource, GenerationMode, ...}`
(`/Users/sac/ggen/crates/ggen-core/src/manifest/`) is entirely native and must be ported, not
re-pointed. **All** of `ggen-lsp`'s project/rule indexing depends on it.

`GGEN-*` diagnostic codes actually live entirely in `ggen-lsp`'s own detector/orchestration
layers (`route/diagnostic_species.rs`, `analyzers/tera_analyzer.rs`, `analyzers/mod.rs`),
confirmed to have **zero** real ggen-core dependency of their own. The only ggen-core
coupling in the whole diagnostics pipeline is the same manifest-parsing surface listed above
(three files) — the manifest port and the diagnostic re-pointing are one migration unit, not
two.

## What moves where

| Source (today) | Destination | Notes |
|---|---|---|
| `/Users/sac/ggen/crates/ggen-core/src/manifest/parser.rs:32-44` (`ManifestParser::parse`) | `/Users/sac/ggen/crates/ggen-config/src/manifest/parser.rs` | Only production entry point; `ggen-lsp/src/project_index.rs:10` imports it directly. |
| `/Users/sac/ggen/crates/ggen-core/src/manifest/types.rs` (573 lines: `GgenManifest`, `GenerationRule`, `QuerySource`, `TemplateSource`, `GenerationMode`, `ValidationRule`, `PackRef`, `PackageToml`, ...) | `/Users/sac/ggen/crates/ggen-config/src/manifest/types.rs` | `ggen-lsp/src/rule_index.rs:17` imports `GenerationRule, QuerySource, TemplateSource` directly; `analyzers/sparql_analyzer.rs` and `a2a_mcp/mcp_server.rs` also depend on this surface. |
| `/Users/sac/ggen/crates/ggen-core/src/manifest/validation.rs` (305 lines: `query_contains_values` line 250, `query_has_order_by` line 243) | `/Users/sac/ggen/crates/ggen-config/src/manifest/validation.rs` | Semantic validation layered on the parser; used by `analyzers/sparql_analyzer.rs:16`. |

## Manifest parsing for ggen-lsp

`project_index.rs` uses exactly `ManifestParser::parse(&manifest_path)` (line 10, called at
113-116). `rule_index.rs` uses `GenerationRule, QuerySource, TemplateSource` (line 17,
`GenerationMode` in a test only, line 195), pattern-matching every `QuerySource`/
`TemplateSource` variant. These types live in
`/Users/sac/ggen/crates/ggen-core/src/manifest/types.rs` (573 lines).

Error-type conventions genuinely clash and must be reconciled during the port, not after:
`ManifestParser::parse` uses a plain string-message `crate::utils::error::Error` (no
`thiserror`); `ggen-config`'s existing surface uses `#[derive(thiserror::Error)] pub enum
ConfigError` with typed `#[from]` conversions. Porting `manifest/*` verbatim alongside
`ConfigError` would leave two incompatible error philosophies in one crate.

**Concrete action:**
- Port `/Users/sac/ggen/crates/ggen-core/src/manifest/{types.rs,parser.rs,validation.rs}`
  into `/Users/sac/ggen/crates/ggen-config/src/manifest/` as a new module, rewriting
  `Result<T>` to `ggen_config`'s `ConfigError`-based `Result` (adding variants as needed)
  instead of carrying over the ad hoc `Error` type.
- Update `/Users/sac/ggen/crates/ggen-lsp/src/project_index.rs:10` and
  `/Users/sac/ggen/crates/ggen-lsp/src/rule_index.rs:17,195` to import from
  `ggen_config::manifest::*`. Blast radius is low: `project_index.rs` already stringifies
  the parser error via `.map_err(|err| IndexError::ManifestParse { message: err.to_string(),
  .. })`, so the concrete `Err` type crossing the boundary doesn't leak further.
- Move `query_contains_values`/`query_has_order_by`
  (used by `/Users/sac/ggen/crates/ggen-lsp/src/analyzers/sparql_analyzer.rs:16`) into the
  same ported module.
- Add `ggen-config` as a direct dependency of `ggen-lsp` if not already present (it is —
  confirmed at `/Users/sac/ggen/crates/ggen-lsp/Cargo.toml:34`,
  `ggen-config = { path = "../ggen-config", version = "26.7.1" }`), and drop the `ggen-core`
  dependency (`Cargo.toml:33`) once no other file needs it (see diagnostics section below —
  none do beyond these three files).

## GGEN-\* diagnostic re-pointing — smaller than it first appears

`/Users/sac/ggen/crates/ggen-lsp/src/route/diagnostic_species.rs` (299 lines, pure static
metadata table) has zero ggen-core dependency. `/Users/sac/ggen/crates/ggen-lsp/src/
analyzers/tera_analyzer.rs` (1,160 lines, implements every detector function) has exactly
one `ggen_core` mention, a doc comment at line 567, no real `use`. The orchestration layer
in `/Users/sac/ggen/crates/ggen-lsp/src/analyzers/mod.rs` (587 lines: `detect_tpl_001`,
`detect_out_001`, etc.) takes only `&ProjectIndex`/`&HarnessIndex` and calls the pure
detector functions — zero ggen-core calls.
`/Users/sac/ggen/crates/ggen-lsp/src/analyzers/harness_analyzer.rs` (175 lines),
`/Users/sac/ggen/crates/ggen-lsp/src/analyzers/source_law_analyzer.rs` (143 lines),
`/Users/sac/ggen/crates/ggen-lsp/src/analyzers/toml_analyzer.rs` (356 lines) also have zero
ggen-core references.

So the entire re-pointing surface for the diagnostic codes is identical to the manifest
port's three files (`project_index.rs`, `rule_index.rs`,
`analyzers/sparql_analyzer.rs`) — no additional diagnostics-specific work exists once the
manifest port lands. (Two more files,
`/Users/sac/ggen/crates/ggen-lsp/src/a2a_mcp/mcp_packs.rs` (417 lines) and
`/Users/sac/ggen/crates/ggen-lsp/src/a2a_mcp/mcp_server.rs` (301 lines), import `ggen_core`
but belong to the unrelated MCP feature surface — see
[09-GGEN-LSP-MIGRATION](09-GGEN-LSP-MIGRATION.md).)

**Concrete action:** none beyond the three import updates above. After re-pointing, confirm
`ggen-lsp`'s `ggen-core` dependency (`Cargo.toml:33`) can be dropped entirely for the
diagnostics path (check whether other ggen-lsp features still need it).

## Risk: two independently-evolved `ggen.toml` schemas already coexist

Pre-existing Legacy Path Contamination, not something this migration introduces.
`/Users/sac/ggen/crates/ggen-config/src/config_lib/schema.rs` (1,523 lines) defines its own
`GgenConfig` (struct at line 14), `ProjectConfig` (line 93), `GenerationConfig` (line 757,
with an untyped `rules: Vec<serde_json::Value>` field at line 762), `InferenceConfig` (line
734, its own `rules: Vec<InferenceRule>` field is typed, distinct from `GenerationConfig`'s),
`InferenceRule` (line 742) — names that collide with `ggen_core::manifest`'s fully-typed
versions that `ggen-lsp` actually consumes. `GgenManifest`
(`/Users/sac/ggen/crates/ggen-core/src/manifest/types.rs`) even documents the collision
explicitly: fields `sync`, `rdf`, `templates`, `output`, `ai`, `sparql`, `lifecycle`,
`security`, `performance`, `logging`, `telemetry`, `features`, `env` are typed
`Option<toml::Value>` under the comment **"Ignored config fields from ggen-config"** purely
so `deny_unknown_fields` doesn't choke on the other schema's sections. Moving
`ManifestParser`/`GgenManifest` into `ggen-config` lands the incoming (real, typed) schema in
the same crate as an existing (partial, largely-inert) competing schema for the same file —
this needs explicit reconciliation (retire `config_lib::GgenConfig`'s generation/inference
sections, or formally scope each schema to disjoint top-level TOML keys) during the move, or
the duplication just relocates.

## Definition of done for this ticket

- `/Users/sac/ggen/crates/ggen-config/src/manifest/` module exists with `ManifestParser`,
  all `Generation*`/`Query*`/`Template*` types, and
  `validation::{query_contains_values, query_has_order_by}`, all using `ConfigError`-based
  `Result`.
- `project_index.rs`, `rule_index.rs`, `analyzers/sparql_analyzer.rs` import from
  `ggen_config::manifest::*` instead of `ggen_core::manifest::*`.
- The two-schema duplication (`config_lib::GgenConfig` at `schema.rs:14` vs. the ported
  manifest types) is explicitly reconciled, not left as a new source of confusion.
- `GGEN-TPL-001`/`GGEN-OUT-001`/`GGEN-YIELD-001`/`GGEN-RULE-001`/`GGEN-QUERY-002` diagnostics
  still fire correctly in `ggen-lsp` against the new manifest types.
