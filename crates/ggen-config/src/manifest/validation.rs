//! Manifest validation module
//!
//! Validates parsed manifests for semantic correctness beyond TOML parsing.
//!
//! Ported from `ggen-core/src/manifest/validation.rs` (specs/014-ggen-core-replacement,
//! docs/jira/v26.7.16/05-MANIFEST-CONFIG-PORT.md), rewriting `Error::new(...)` to
//! `ConfigError::Validation(...)` -- see `parser.rs`'s module doc for why.
//!
//! # Design note: `star_toml::Validate` (pure data) vs. `validate_paths` (filesystem)
//!
//! specs/014-ggen-core-replacement T023's follow-up asked this crate to validate
//! `GgenManifest` "consistently" with `star_toml::Validate` (matching
//! `ggen_engine::config::GgenConfig`'s already-correct pattern) instead of the
//! original hand-rolled, ggen-core-ported `ManifestValidator`. That original
//! validator mixed two genuinely different kinds of checks in one fail-fast
//! sequence:
//!
//! 1. **Pure-data invariants** — non-empty names, an inline SPARQL string
//!    missing `ORDER BY`, a pack referenced by name not being declared in
//!    `[[packs]]`. These need only `&self`.
//! 2. **Filesystem-dependent checks** — does `ontology.source` exist on disk,
//!    does a `QuerySource::File`/`TemplateSource::File` path exist (and, for
//!    query files, does its *content* contain `VALUES`/lack `ORDER BY`), do
//!    the declared `validation.shacl`/`law.rules` paths exist. These need a
//!    `base_path` to resolve relative paths against.
//!
//! `star_toml::Validate` is `fn validate(&self, v: &mut Validator)` — no
//! `base_path` parameter, by design (it is meant to validate a value in
//! isolation, the same contract `ggen_engine::config::GgenConfig`'s impl
//! already follows). Bolting a `base_path` onto that signature would either
//! break the trait's contract or force every *other* implementor in this
//! workspace to carry a dummy parameter it doesn't need. Rather than force
//! group 2 into a signature it doesn't fit, this module keeps the two
//! concerns **explicitly separate**:
//!
//! - `impl Validate for GgenManifest` — every pure-data invariant, including
//!   the ones the original code gated on `self.validation.strict_mode`
//!   (`E0011`/`E0013`: strict mode promotes a missing `ORDER BY` to a hard
//!   error; non-strict mode keeps the original `log::warn!`-only behavior,
//!   emitted directly rather than through the `Validator` — see below for
//!   why that distinction matters).
//! - `GgenManifest::validate_paths(&self, base_path: &Path) -> Result<()>` —
//!   every filesystem-dependent check, a plain inherent method (not a
//!   trait), ported near-verbatim from the original `ManifestValidator`
//!   methods it replaces.
//! - `ManifestValidator` itself is kept as a thin, path-stable compatibility
//!   wrapper (its public `new`/`validate` API is unchanged) so
//!   `ManifestParser::parse_and_validate` and the two out-of-crate consumers
//!   that still call it directly (`ggen-lsp/src/a2a_mcp/mcp_server.rs`,
//!   `ggen-cli/src/cmds/sigma.rs` — both currently against `ggen_core`'s
//!   *own*, untouched copy, not this one) keep working without a signature
//!   change: `validate()` now runs `self.manifest.check()` (the
//!   `star_toml::Validate` pass) then `self.manifest.validate_paths(...)`.
//!
//! ## Why the non-strict-mode warning can't go through `Validator`
//!
//! `star_toml::Validator::finish()` returns `Err` whenever *any* error was
//! recorded, regardless of `Severity` — `Severity` only distinguishes
//! `has_fatal()` from the rest, it does not make `.check()`/`.validated()`
//! return `Ok`. The original behavior for a non-strict-mode missing
//! `ORDER BY` is "log a warning, validation still succeeds" — there is no
//! `Validator` call that produces that outcome (every `check_*`/`error*`
//! call that fires becomes a member of `errors()`, which makes `.check()`
//! fail). So the non-strict branch calls `log::warn!` directly, exactly as
//! before, without touching the `Validator` at all; only the strict-mode
//! hard-error branch calls `v.check_predicate(...)`.
//!
//! ## Known, deliberate non-changes (checks *not* added)
//!
//! - **No path-traversal check on `ontology.source`/`ontology.imports`.**
//!   `ggen_engine::config::Ontology::validate` applies `check_path(...,
//!   Some(false))` (rejects `..`) to its own `source` field, and it would be
//!   tempting to mirror that here for “schema reconciliation”. Grepping the
//!   repository's real `ggen.toml`/`ggen-toml`-shaped fixtures first
//!   (2026-07-16) found this is *not* safe to add: `.specify/specs/*/ggen.toml`,
//!   `.specify/mcp-a2a/*.toml`, and several `marketplace/packages/*/ggen.toml`
//!   files legitimately reference sibling directories via `../` in `source`/
//!   `imports` (e.g. `source = "../universal/domain.ttl"`). Adding this check
//!   would break real, currently-valid manifests. Left as a documented gap,
//!   not silently introduced.
//! - **No non-empty check on `PackRef::name`, no checks on `ValidationRule`
//!   (`name`/`description`/`ask`).** Neither was checked by the original
//!   `ManifestValidator` at all — adding them now would be new, previously
//!   untested validation surface with unknown blast radius on real manifests
//!   this session did not have time to audit. Left as future work rather
//!   than introduced unverified.

use crate::manifest::types::{GgenManifest, QuerySource, TemplateSource};
use crate::{ConfigError, Result};
use star_toml::{Validate, Validator};
use std::path::Path;

// =========================================================================
// Pure-data invariants — `star_toml::Validate`
// =========================================================================

impl Validate for GgenManifest {
    fn validate(&self, v: &mut Validator) {
        v.field("project", |v| {
            v.check_non_empty("name", &self.project.name);
            v.check_non_empty("version", &self.project.version);
        });

        v.field("inference", |v| {
            v.field("rules", |v| {
                let mut seen_orders: Vec<i32> = Vec::new();
                for (i, rule) in self.inference.rules.iter().enumerate() {
                    v.index(i, |v| {
                        v.check_non_empty("name", &rule.name);
                        v.check_non_empty("construct", &rule.construct);
                        // E0011: CONSTRUCT is always inline text on InferenceRule
                        // (no File variant exists) -- this check is pure data,
                        // unlike its generation-rule sibling below.
                        if !query_has_order_by(&rule.construct) {
                            if self.validation.strict_mode {
                                v.check_predicate(
                                    "construct",
                                    false,
                                    "E0011",
                                    format!(
                                        "error[E0011]: Inference rule '{}' CONSTRUCT query lacks ORDER BY\n  |\n  = strict_mode is enabled: non-deterministic triple ordering is rejected\n  = help: Add ORDER BY to your CONSTRUCT query to guarantee deterministic output\n  = help: Or set `strict_mode = false` in [validation] to downgrade to a warning",
                                        rule.name
                                    ),
                                );
                            } else {
                                log::warn!(
                                    "Inference rule '{}' CONSTRUCT query lacks ORDER BY - may produce non-deterministic results",
                                    rule.name
                                );
                            }
                        }
                    });
                    // Duplicate order values: informational only in the original
                    // code (never gated on strict_mode, never fails validation) --
                    // stays a direct log::warn!, same reasoning as above.
                    if seen_orders.contains(&rule.order) {
                        log::warn!(
                            "Inference rule '{}' has duplicate order value {}",
                            rule.name,
                            rule.order
                        );
                    }
                    seen_orders.push(rule.order);
                }
            });
        });

        v.field("generation", |v| {
            v.field("rules", |v| {
                for (i, rule) in self.generation.rules.iter().enumerate() {
                    v.index(i, |v| {
                        v.check_non_empty("name", &rule.name);
                        v.check_non_empty("output_file", &rule.output_file);

                        // E0014: a pack referenced via QuerySource::Pack or
                        // TemplateSource::Pack must be declared in [[packs]].
                        // Pure data -- only needs self.packs, no fs access.
                        let rule_pack_name: Option<&str> = match &rule.query {
                            QuerySource::Pack { pack, .. } => Some(pack.as_str()),
                            _ => match &rule.template {
                                TemplateSource::Pack { pack, .. } => Some(pack.as_str()),
                                _ => None,
                            },
                        };
                        if let Some(pack_name) = rule_pack_name {
                            v.check_predicate(
                                "query",
                                self.packs.iter().any(|p| p.name == pack_name),
                                "E0014",
                                format!(
                                    "error[E0014]: Pack '{pack_name}' used in rule '{}' is not declared in [[packs]]",
                                    rule.name
                                ),
                            );
                        }

                        // E0013: only the Inline variant is pure data. The File
                        // variant needs to read the referenced file's content,
                        // which requires a base_path -- that half of this check
                        // lives in `GgenManifest::validate_paths` instead. Pack
                        // queries are resolved at sync time and skipped, exactly
                        // as in the original code.
                        if let QuerySource::Inline { inline } = &rule.query {
                            if !query_has_order_by(inline) {
                                if self.validation.strict_mode {
                                    v.check_predicate(
                                        "query",
                                        false,
                                        "E0013",
                                        format!(
                                            "error[E0013]: Generation rule '{}' SELECT query lacks ORDER BY\n  |\n  = strict_mode is enabled: non-deterministic row ordering is rejected\n  = help: Add ORDER BY to your SELECT query to guarantee deterministic template rendering\n  = help: Or set `strict_mode = false` in [validation] to downgrade to a warning",
                                            rule.name
                                        ),
                                    );
                                } else {
                                    log::warn!(
                                        "Generation rule '{}' SELECT query lacks ORDER BY — row order may vary across runs",
                                        rule.name
                                    );
                                }
                            }
                        }
                    });
                }
            });
        });

        // Operational/infrastructure sections: reuse config_lib's own,
        // already-tested `Validate` impls verbatim rather than re-implementing
        // them -- see GgenManifest's struct-level doc comment (types.rs) for
        // why these are the *same* types, not re-defined ones.
        if let Some(rdf) = &self.rdf {
            v.field("rdf", |v| rdf.validate(v));
        }
        if let Some(templates) = &self.templates {
            v.field("templates", |v| templates.validate(v));
        }
        if let Some(ai) = &self.ai {
            v.field("ai", |v| ai.validate(v));
        }
        if let Some(sparql) = &self.sparql {
            v.field("sparql", |v| sparql.validate(v));
        }
        if let Some(lifecycle) = &self.lifecycle {
            v.field("lifecycle", |v| lifecycle.validate(v));
        }
        if let Some(security) = &self.security {
            v.field("security", |v| security.validate(v));
        }
        if let Some(performance) = &self.performance {
            v.field("performance", |v| performance.validate(v));
        }
        if let Some(logging) = &self.logging {
            v.field("logging", |v| logging.validate(v));
        }
        if let Some(telemetry) = &self.telemetry {
            v.field("telemetry", |v| telemetry.validate(v));
        }
        if let Some(build) = &self.build {
            v.field("build", |v| build.validate(v));
        }
        if let Some(test) = &self.test {
            v.field("test", |v| test.validate(v));
        }
        if let Some(package) = &self.package {
            v.field("package", |v| package.validate(v));
        }
        if let Some(mcp) = &self.mcp {
            v.field("mcp", |v| mcp.validate(v));
        }
        if let Some(a2a) = &self.a2a {
            v.field("a2a", |v| a2a.validate(v));
        }
    }
}

// =========================================================================
// Filesystem-dependent checks — inherent method, not part of `Validate`
// =========================================================================

impl GgenManifest {
    /// Validate every path-shaped invariant that needs to touch the
    /// filesystem: does `ontology.source`/`ontology.imports` exist, do
    /// file-sourced generation-rule queries/templates exist (and, for
    /// queries, are they free of `VALUES` and not missing `ORDER BY`), do
    /// `validation.shacl`/`law.rules` paths exist. See this module's
    /// top-level doc comment for why these are not part of the
    /// `star_toml::Validate` impl above.
    ///
    /// # Errors
    /// Returns the first [`ConfigError::Validation`] failure encountered
    /// (fail-fast, matching the original `ManifestValidator`'s behavior for
    /// this half of the checks).
    pub fn validate_paths(&self, base_path: &Path) -> Result<()> {
        self.validate_ontology_paths(base_path)?;
        self.validate_generation_rule_paths(base_path)?;
        self.validate_shacl_paths(base_path)?;
        self.validate_law_rule_paths(base_path)?;
        Ok(())
    }

    fn validate_ontology_paths(&self, base_path: &Path) -> Result<()> {
        let source_path = base_path.join(&self.ontology.source);
        if !source_path.exists() {
            return Err(ConfigError::Validation(format!(
                "Ontology source not found: {}",
                source_path.display()
            )));
        }

        for import in &self.ontology.imports {
            let import_path = base_path.join(import);
            if !import_path.exists() {
                return Err(ConfigError::Validation(format!(
                    "Ontology import not found: {}",
                    import_path.display()
                )));
            }
        }

        Ok(())
    }

    fn validate_generation_rule_paths(&self, base_path: &Path) -> Result<()> {
        for rule in &self.generation.rules {
            // Validate query source exists and does not contain VALUES clauses.
            // VALUES data must live inline in ggen.toml -- never in external
            // .rq files.
            if let QuerySource::File { file } = &rule.query {
                let query_path = base_path.join(file);
                if !query_path.exists() {
                    return Err(ConfigError::Validation(format!(
                        "Query file not found for rule '{}': {}",
                        rule.name,
                        query_path.display()
                    )));
                }
                let content = std::fs::read_to_string(&query_path).map_err(|e| {
                    ConfigError::Validation(format!(
                        "Failed to read query file for rule '{}': {}",
                        rule.name, e
                    ))
                })?;
                if query_contains_values(&content) {
                    return Err(ConfigError::Validation(format!(
                        "error[E0010]: VALUES data must be inline in ggen.toml\n  --> rule: '{}'\n  --> file: {}\n  |\n  = VALUES clauses belong in ggen.toml as `query = {{ inline = \"SELECT ... WHERE {{ VALUES ... }}\" }}`\n  = External .rq files are for queries against real RDF triples only\n  = help: Move the VALUES block into ggen.toml and delete the .rq file",
                        rule.name,
                        query_path.display()
                    )));
                }
                // E0013 for file-sourced queries: needs the content just read
                // above, so it lives here rather than in the pure-data
                // `Validate` impl (which only covers the Inline variant).
                if !query_has_order_by(&content) {
                    if self.validation.strict_mode {
                        return Err(ConfigError::Validation(format!(
                            "error[E0013]: Generation rule '{}' SELECT query lacks ORDER BY\n  |\n  = strict_mode is enabled: non-deterministic row ordering is rejected\n  = help: Add ORDER BY to your SELECT query to guarantee deterministic template rendering\n  = help: Or set `strict_mode = false` in [validation] to downgrade to a warning",
                            rule.name
                        )));
                    }
                    log::warn!(
                        "Generation rule '{}' SELECT query lacks ORDER BY — row order may vary across runs",
                        rule.name
                    );
                }
            }

            // Validate template source exists.
            if let TemplateSource::File { file } = &rule.template {
                let template_path = base_path.join(file);
                if !template_path.exists() {
                    return Err(ConfigError::Validation(format!(
                        "Template file not found for rule '{}': {}",
                        rule.name,
                        template_path.display()
                    )));
                }
            }
        }

        Ok(())
    }

    fn validate_shacl_paths(&self, base_path: &Path) -> Result<()> {
        for shacl_path in &self.validation.shacl {
            let full_path = base_path.join(shacl_path);
            if !full_path.exists() {
                return Err(ConfigError::Validation(format!(
                    "SHACL shape file not found: {}",
                    full_path.display()
                )));
            }
        }
        // `[validation].gates` (SPARQL gate files, the shacl successor):
        // same declared-path-must-exist contract.
        for gate_path in &self.validation.gates {
            let full_path = base_path.join(gate_path);
            if !full_path.exists() {
                return Err(ConfigError::Validation(format!(
                    "SPARQL gate file not found: {}",
                    full_path.display()
                )));
            }
        }
        Ok(())
    }

    /// New: `[law].rules` (N3/Datalog rule files) existence check. Additive
    /// -- `law` did not exist on `GgenManifest` before this reconciliation,
    /// so this can only ever newly *reject* a manifest that explicitly opts
    /// into `[law]`, never one that omits it.
    fn validate_law_rule_paths(&self, base_path: &Path) -> Result<()> {
        for rule_path in &self.law.rules {
            let full_path = base_path.join(rule_path);
            if !full_path.exists() {
                return Err(ConfigError::Validation(format!(
                    "law rule file not found: {}",
                    full_path.display()
                )));
            }
        }
        Ok(())
    }
}

// =========================================================================
// `ManifestValidator` — thin, path-stable compatibility wrapper
// =========================================================================

/// Validator for ggen.toml manifests.
///
/// Combines [`GgenManifest`]'s `star_toml::Validate` pass (pure-data
/// invariants) with [`GgenManifest::validate_paths`] (filesystem-dependent
/// checks) — see this module's top-level doc comment for why the two are
/// implemented separately but exposed together here, preserving this type's
/// pre-existing public API for its current callers
/// (`ManifestParser::parse_and_validate`, and — via `ggen_core`'s own,
/// separately-maintained copy of this same design shape, not this crate's
/// code — `ggen-lsp/src/a2a_mcp/mcp_server.rs` and
/// `ggen-cli/src/cmds/sigma.rs`, both still pending re-point to this crate).
pub struct ManifestValidator<'a> {
    manifest: &'a GgenManifest,
    base_path: &'a Path,
}

impl<'a> ManifestValidator<'a> {
    /// Create a new validator for the given manifest
    ///
    /// # Arguments
    /// * `manifest` - The parsed manifest to validate
    /// * `base_path` - Base path for resolving relative file paths
    #[must_use]
    pub fn new(manifest: &'a GgenManifest, base_path: &'a Path) -> Self {
        Self {
            manifest,
            base_path,
        }
    }

    /// Run all validations: pure-data invariants first (`star_toml::Validate`,
    /// fail-slow -- every violation is collected and reported together), then
    /// filesystem-dependent checks (fail-fast, matching the original
    /// behavior for that half).
    ///
    /// # Behavior change from the pre-T023 version
    /// The original method ran five fail-fast steps in a fixed order
    /// (project, ontology, inference, generation, shacl) and returned only
    /// the *first* violation found, whichever step hit it first. This
    /// version runs all pure-data checks together (via
    /// [`star_toml::Validate::check`]) before any filesystem check, so: (a)
    /// multiple simultaneous data violations are now all reported at once
    /// instead of only the first, and (b) if a data violation and a
    /// filesystem violation exist simultaneously, the data violation is
    /// reported first regardless of which table it's in. No test in this
    /// crate (or its two known out-of-crate callers) asserts on a specific
    /// first-error identity or ordering -- both only assert `.is_err()`
    /// (confirmed via grep, 2026-07-16) -- so this is a low-risk, strictly
    /// more-informative change, not a silent regression.
    ///
    /// # Errors
    /// Returns [`ConfigError::Validation`] on the first filesystem
    /// violation, or on the combined pure-data violation report, whichever
    /// half fails first (pure-data checks run first).
    pub fn validate(&self) -> Result<()> {
        self.manifest
            .check()
            .map_err(|errs| ConfigError::Validation(errs.to_string()))?;
        self.manifest.validate_paths(self.base_path)
    }
}

/// Returns true if the SPARQL string contains an ORDER BY clause.
///
/// Detection is case-insensitive and covers multiline queries. This is a
/// textual heuristic (oxigraph does not expose an AST); "ORDER BY" in a
/// SPARQL comment or string literal would be a false-positive, but that
/// edge case is acceptable — it only suppresses a warning, not correctness.
#[must_use]
pub fn query_has_order_by(sparql: &str) -> bool {
    sparql.to_uppercase().contains("ORDER BY")
}

/// Returns true if the SPARQL query string contains a VALUES clause.
/// Strips `#`-prefixed line comments before checking so commented-out
/// VALUES blocks do not trigger the guard.
#[must_use]
pub fn query_contains_values(query: &str) -> bool {
    query
        .lines()
        .filter(|line| !line.trim_start().starts_with('#'))
        .any(|line| {
            line.split_whitespace()
                .any(|tok| tok.eq_ignore_ascii_case("VALUES"))
        })
}

#[cfg(test)]
#[allow(clippy::unwrap_used, clippy::expect_used)]
mod tests {
    use super::*;
    use crate::manifest::ManifestParser;

    fn create_test_manifest() -> GgenManifest {
        let toml = r#"
[project]
name = "test"
version = "1.0.0"

[ontology]
source = "Cargo.toml"  # Use existing file for test

[generation]
rules = []
"#;
        ManifestParser::parse_str(toml).unwrap()
    }

    #[test]
    fn test_validate_empty_project_name() {
        let toml = r#"
[project]
name = ""
version = "1.0.0"

[ontology]
source = "test.ttl"

[generation]
rules = []
"#;
        let manifest = ManifestParser::parse_str(toml).unwrap();
        let validator = ManifestValidator::new(&manifest, Path::new("."));
        assert!(validator.validate().is_err());
    }

    #[test]
    fn test_validate_missing_ontology() {
        let manifest = create_test_manifest();
        // Use a path where the ontology won't exist
        let validator = ManifestValidator::new(&manifest, Path::new("/nonexistent/path"));
        assert!(validator.validate().is_err());
    }

    #[test]
    fn test_validate_paths_directly_on_manifest() {
        // GgenManifest::validate_paths is a public inherent method, usable
        // without going through the ManifestValidator wrapper.
        let manifest = create_test_manifest();
        assert!(manifest.validate_paths(Path::new(".")).is_ok());
        assert!(manifest
            .validate_paths(Path::new("/nonexistent/path"))
            .is_err());
    }

    #[test]
    fn test_pure_data_validate_catches_empty_project_name_without_fs_access() {
        // star_toml::Validate should fail on data alone -- no fs access
        // involved, unlike ManifestValidator::validate() as a whole.
        let mut manifest = create_test_manifest();
        manifest.project.name = String::new();
        let errs = manifest.check().unwrap_err();
        assert!(errs
            .errors()
            .iter()
            .any(|e| e.loc.to_string() == "project.name"));
    }

    #[test]
    fn test_e0014_pack_not_declared_is_pure_data_violation() {
        let toml = r#"
[project]
name = "test"
version = "1.0.0"

[ontology]
source = "Cargo.toml"

[[generation.rules]]
name = "uses-pack"
output_file = "out.rs"
query = { pack = "undeclared-pack", output = "queries", file = "q.rq" }
template = { inline = "x" }
"#;
        let manifest = ManifestParser::parse_str(toml).unwrap();
        let errs = manifest.check().unwrap_err();
        assert!(errs.errors().iter().any(|e| e.code() == "E0014"));
    }

    #[test]
    fn test_operational_sections_reuse_config_lib_validation() {
        // [ai] reuses config_lib::AiConfig's own Validate impl verbatim --
        // an invalid provider must surface as an "ai.provider" violation.
        let toml = r#"
[project]
name = "test"
version = "1.0.0"

[ontology]
source = "Cargo.toml"

[generation]
rules = []

[ai]
provider = "not-a-real-provider"
model = "x"
"#;
        let manifest = ManifestParser::parse_str(toml).unwrap();
        let errs = manifest.check().unwrap_err();
        assert!(errs
            .errors()
            .iter()
            .any(|e| e.loc.to_string() == "ai.provider"));
    }
}
