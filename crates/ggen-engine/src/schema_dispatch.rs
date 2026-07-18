//! The single dispatch point deciding which of `ggen.toml`'s two
//! independently-defined schemas a project uses, and parsing it with the
//! matching real, typed parser.
//!
//! # Why this exists (specs/014-ggen-core-replacement, correction 2 / Blocker A part 2)
//!
//! `ggen_config::config_schema::classify_ggen_toml` (see that module's own
//! doc comment for the full two-schema design) is the one shared *shape*
//! classifier. Before this module existed, six call sites each decided which
//! schema to parse a project's `ggen.toml` as by their own ad-hoc logic:
//! [`crate::sync::sync`]'s Stage 0 used the narrower
//! `crate::generation_rules::has_generation_rules` pre-parse (correct, but a
//! *different*, less complete check than the shared classifier — it only
//! detects a non-empty `[[generation.rules]]`, not the fuller marker set);
//! [`crate::verbs::handlers::handle_doctor`],
//! [`crate::verbs::handlers::handle_graph_validate`] (project mode), and
//! [`crate::verbs::handlers::build_law_engine`] (backing all five `ggen law
//! *` verbs) each unconditionally called [`crate::config::GgenConfig::load`]
//! — the frontmatter schema only — and would reject any real
//! declarative-rules project outright (`ggen doctor` against this
//! workspace's own root `ggen.toml` failed this way; see BUG-005 in this
//! ticket's own history).
//!
//! This module is the *one* place every one of those call sites now goes
//! through: [`load`] reads `ggen.toml` once, classifies it via the shared
//! [`ggen_config::classify_ggen_toml`], and returns either concrete parsed
//! type wrapped in [`ParsedGgenToml`] — never re-deriving the classification
//! logic itself. `sync()` and `handle_doctor()` (and every other repointed
//! call site) call this same function; none of them re-implements the
//! dispatch decision locally (see this ticket's own explicit instruction:
//! "do NOT copy sync()'s branch logic into doctor").
//!
//! # Error behavior
//!
//! - A `ggen.toml` that fails to even be *read* (missing file, permissions)
//!   is not diagnosed here: this function falls through to
//!   [`crate::config::GgenConfig::load`], which re-attempts the same read
//!   and produces its own canonical `[FM-CONFIG-001]` message — byte-for-byte
//!   the same failure every one of the six call sites already produced for a
//!   missing `ggen.toml` before this module existed. No behavior change for
//!   that case.
//! - `Ambiguous`/`Unsupported`/`Malformed` classifications fail closed with
//!   a message embedding the classifier's own typed outcome code
//!   (`ggen_config::CONFIG_SCHEMA_AMBIGUOUS`/`CONFIG_SCHEMA_UNSUPPORTED`/
//!   `CONFIG_PARSE_FAILED`) — never a bare/generic TOML deserialization
//!   error from blindly attempting one schema's parser against a document
//!   that doesn't structurally match it.

use std::path::Path;

use ggen_config::{manifest::GgenManifest, ConfigSchemaClassification};

use crate::{
    config::GgenConfig,
    error::{AppError, Result},
};

/// Either concrete parsed schema a `ggen.toml` project can resolve to.
#[derive(Debug)]
pub(crate) enum ParsedGgenToml {
    /// `[[generation.rules]]` declarative-rules schema
    /// ([`ggen_config::manifest::GgenManifest`]).
    DeclarativeRules(Box<GgenManifest>),
    /// `[templates].dir`-per-file frontmatter schema
    /// ([`crate::config::GgenConfig`]).
    Frontmatter(Box<GgenConfig>),
}

/// Read `<root>/ggen.toml`, classify it via the one shared
/// [`ggen_config::classify_ggen_toml`] classifier, and parse it with the
/// matching real, typed parser.
///
/// # Errors
/// - `[FM-CONFIG-001]` if the file is missing/unreadable (delegated to
///   [`crate::config::GgenConfig::load`]'s own canonical message — see the
///   module doc comment).
/// - `[FM-CONFIG-002]`/`[FM-CONFIG-003]` if the document classifies as
///   `Frontmatter` but fails that schema's own syntax/semantic parse.
/// - A `declarative ggen.toml … failed to parse` message if the document
///   classifies as `DeclarativeRules` but fails
///   [`ggen_config::manifest::ManifestParser::parse_str`].
/// - A message embedding [`ggen_config::CONFIG_SCHEMA_AMBIGUOUS`] /
///   [`ggen_config::CONFIG_SCHEMA_UNSUPPORTED`] / [`ggen_config::CONFIG_PARSE_FAILED`]
///   for the classifier's own `Ambiguous`/`Unsupported`/`Malformed` outcomes.
pub(crate) fn load(root: &Path) -> Result<ParsedGgenToml> {
    let ggen_toml_path = root.join("ggen.toml");

    let Ok(raw) = std::fs::read_to_string(&ggen_toml_path) else {
        // Unreadable/missing: fall through to `GgenConfig::load`'s own
        // canonical `[FM-CONFIG-001]` FileNotFound/Io message — unchanged
        // from every call site's pre-classifier behavior.
        return GgenConfig::load(&ggen_toml_path).map(|c| ParsedGgenToml::Frontmatter(Box::new(c)));
    };

    match ggen_config::classify_ggen_toml(&raw) {
        ConfigSchemaClassification::DeclarativeRules => {
            let manifest = ggen_config::manifest::ManifestParser::parse_str(&raw).map_err(|e| {
                AppError::fm_config(
                    3,
                    format!(
                        "declarative ggen.toml at `{}` failed to parse: {e}. \
                         Remediation: fix the reported field(s) under [generation]/[[generation.rules]].",
                        ggen_toml_path.display()
                    ),
                )
            })?;
            Ok(ParsedGgenToml::DeclarativeRules(Box::new(manifest)))
        }
        ConfigSchemaClassification::Frontmatter => {
            let config = GgenConfig::load(&ggen_toml_path)?;
            Ok(ParsedGgenToml::Frontmatter(Box::new(config)))
        }
        ConfigSchemaClassification::Ambiguous { matched } => Err(AppError::Config(format!(
            "[{}] ggen.toml at `{}` is ambiguous between the declarative-rules and \
             frontmatter schemas: conflicting structural markers {matched:?}. Remediation: \
             remove whichever schema's tables/fields were added by mistake.",
            ggen_config::CONFIG_SCHEMA_AMBIGUOUS,
            ggen_toml_path.display(),
        ))),
        ConfigSchemaClassification::Unsupported { observed_markers } => {
            Err(AppError::Config(format!(
                "[{}] ggen.toml at `{}` matches neither the declarative-rules nor the \
                 frontmatter schema: observed top-level tables {observed_markers:?}. \
                 Remediation: add the required [project]/[ontology]/[templates] (frontmatter) \
                 or [project]/[ontology]/[generation] (declarative-rules) fields.",
                ggen_config::CONFIG_SCHEMA_UNSUPPORTED,
                ggen_toml_path.display(),
            )))
        }
        ConfigSchemaClassification::Malformed { diagnostic } => Err(AppError::Config(format!(
            "ggen.toml at `{}`: {diagnostic}",
            ggen_toml_path.display(),
        ))),
    }
}

#[cfg(test)]
#[allow(clippy::unwrap_used, clippy::expect_used)]
mod tests {
    use super::*;
    use tempfile::TempDir;

    fn write(dir: &TempDir, contents: &str) {
        std::fs::write(dir.path().join("ggen.toml"), contents).expect("write ggen.toml");
    }

    #[test]
    fn missing_file_falls_through_to_canonical_fm_config_001() {
        let dir = TempDir::new().expect("tempdir");
        let err = load(dir.path()).expect_err("must fail");
        assert!(err.to_string().contains("FM-CONFIG-001"), "{err}");
    }

    #[test]
    fn declarative_rules_document_parses_as_declarative_rules() {
        let dir = TempDir::new().expect("tempdir");
        write(
            &dir,
            r#"
[project]
name = "x"
version = "1.0.0"

[ontology]
source = "o.ttl"

[[generation.rules]]
name = "r"
query = { inline = "SELECT * WHERE { ?s ?p ?o }" }
template = { inline = "hi" }
output_file = "out.txt"
"#,
        );
        match load(dir.path()).expect("must classify+parse") {
            ParsedGgenToml::DeclarativeRules(_) => {}
            ParsedGgenToml::Frontmatter(_) => panic!("expected DeclarativeRules"),
        }
    }

    #[test]
    fn frontmatter_document_parses_as_frontmatter() {
        let dir = TempDir::new().expect("tempdir");
        write(
            &dir,
            r#"
[project]
name = "x"

[ontology]
source = "o.ttl"

[templates]
dir = "templates"
"#,
        );
        match load(dir.path()).expect("must classify+parse") {
            ParsedGgenToml::Frontmatter(_) => {}
            ParsedGgenToml::DeclarativeRules(_) => panic!("expected Frontmatter"),
        }
    }

    #[test]
    fn ambiguous_document_reports_typed_code() {
        let dir = TempDir::new().expect("tempdir");
        write(
            &dir,
            "[project]\nname = \"x\"\n\n[ontology]\nsource = \"o.ttl\"\n\n[templates]\ndir = \"t\"\n\n[mcp]\nenabled = true\n",
        );
        let err = load(dir.path()).expect_err("must fail");
        assert!(
            err.to_string()
                .contains(ggen_config::CONFIG_SCHEMA_AMBIGUOUS),
            "{err}"
        );
    }

    #[test]
    fn unsupported_document_reports_typed_code() {
        let dir = TempDir::new().expect("tempdir");
        write(&dir, "[some_other_tool]\nkey = \"value\"\n");
        let err = load(dir.path()).expect_err("must fail");
        assert!(
            err.to_string()
                .contains(ggen_config::CONFIG_SCHEMA_UNSUPPORTED),
            "{err}"
        );
    }

    #[test]
    fn malformed_document_reports_typed_code() {
        let dir = TempDir::new().expect("tempdir");
        write(&dir, "not [ valid toml");
        let err = load(dir.path()).expect_err("must fail");
        assert!(
            err.to_string().contains(ggen_config::CONFIG_PARSE_FAILED),
            "{err}"
        );
    }
}
