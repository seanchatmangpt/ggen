//! Drift guard for the `ggen.toml` two-schema selector (`classify_ggen_toml`).
//!
//! `.claude/rules/architecture.md` notes `ggen.toml` has two incompatible schemas chosen by a
//! raw-text pre-parse. The selector is already explicit and fail-closed (Ambiguous /
//! Unsupported / Malformed are typed refusals, enforced at the engine's single dispatch
//! `ggen_engine::schema_dispatch::load`). What was missing is a guard over the REAL corpus:
//! this test classifies every tracked `ggen.toml` in the repo and asserts
//!
//! 1. every file lands in exactly one outcome, carrying its typed `FM-CONFIG-10x` code;
//! 2. a file classified into one schema carries NO strong marker of the other schema
//!    (never both);
//! 3. a file classified Ambiguous names markers from BOTH schemas, and Unsupported/Malformed
//!    are refusals, never silently routed to a parser (never neither);
//! 4. hybrid documents are refused with `FM-CONFIG-101`, not warned about.
//!
//! Chicago TDD: real files off disk, real classifier, no mocks.

#![allow(clippy::unwrap_used, clippy::expect_used, clippy::panic)]

use ggen_config::{
    classify_ggen_toml, ConfigSchemaClassification as C, CONFIG_PARSE_FAILED,
    CONFIG_SCHEMA_AMBIGUOUS, CONFIG_SCHEMA_SUPPORTED, CONFIG_SCHEMA_UNSUPPORTED,
};
use std::path::{Path, PathBuf};

fn repo_root() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../..")
}

fn collect(dir: &Path, out: &mut Vec<PathBuf>) {
    let Ok(rd) = std::fs::read_dir(dir) else {
        return;
    };
    for e in rd.flatten() {
        let p = e.path();
        let name = e.file_name().to_string_lossy().into_owned();
        if p.is_dir() {
            if matches!(name.as_str(), "target" | ".git" | "node_modules") {
                continue;
            }
            collect(&p, out);
        } else if name == "ggen.toml" {
            out.push(p);
        }
    }
}

fn corpus() -> Vec<(PathBuf, String)> {
    let root = repo_root();
    let mut files = Vec::new();
    collect(&root, &mut files);
    files.sort();
    files
        .into_iter()
        .map(|f| {
            let raw = std::fs::read_to_string(&f).unwrap();
            (f.strip_prefix(&root).unwrap().to_path_buf(), raw)
        })
        .collect()
}

/// Strong, schema-exclusive structural markers (re-derived from the raw table, independent of
/// the classifier's internals).
fn strong_declarative(t: &toml::Table) -> bool {
    t.contains_key("generation") || matches!(t.get("packs"), Some(toml::Value::Array(_)))
}
fn strong_frontmatter(t: &toml::Table) -> bool {
    let shapes = t
        .get("law")
        .and_then(toml::Value::as_table)
        .is_some_and(|l| l.contains_key("shapes"));
    shapes || matches!(t.get("packs"), Some(toml::Value::Table(_)))
}

/// The classifier's positive confirmation for the frontmatter schema when no exclusive marker
/// fires: the three-field conjunction `project.name` + `ontology.source` + `templates.dir`
/// (`satisfies_frontmatter_minimum`). Re-derived from the raw table, independent of the classifier.
fn frontmatter_minimum(t: &toml::Table) -> bool {
    let str_at = |tbl: &str, key: &str| {
        t.get(tbl)
            .and_then(toml::Value::as_table)
            .and_then(|x| x.get(key))
            .and_then(toml::Value::as_str)
    };
    str_at("project", "name").is_some_and(|s| !s.is_empty())
        && str_at("ontology", "source").is_some()
        && str_at("templates", "dir").is_some()
}

#[test]
fn every_repo_ggen_toml_is_classified_into_exactly_one_outcome() {
    let files = corpus();
    assert!(files.len() > 20, "corpus walk found only {} files", files.len());
    let mut supported = 0usize;
    for (path, raw) in &files {
        let got = classify_ggen_toml(raw);
        let table: Result<toml::Table, _> = raw.parse();
        match &got {
            C::DeclarativeRules => {
                supported += 1;
                assert_eq!(got.code(), CONFIG_SCHEMA_SUPPORTED, "{}", path.display());
                let t = table.expect("classified file must be valid TOML");
                assert!(strong_declarative(&t), "{}: no declarative marker", path.display());
                assert!(
                    !strong_frontmatter(&t),
                    "{}: DeclarativeRules but carries frontmatter-exclusive marker (both)",
                    path.display()
                );
            }
            C::Frontmatter => {
                supported += 1;
                assert_eq!(got.code(), CONFIG_SCHEMA_SUPPORTED, "{}", path.display());
                let t = table.expect("classified file must be valid TOML");
                assert!(
                    !strong_declarative(&t),
                    "{}: Frontmatter but carries declarative-exclusive marker (both)",
                                 path.display()
                );
                assert!(
                    strong_frontmatter(&t) || frontmatter_minimum(&t),
                    "{}: Frontmatter without a strong marker or the three-field minimum",
                    path.display()
                );
            }
            C::Ambiguous { matched } => {
                assert_eq!(got.code(), CONFIG_SCHEMA_AMBIGUOUS, "{}", path.display());
                assert!(
                    matched.iter().any(|m| m.starts_with("declarative:"))
                        && matched.iter().any(|m| m.starts_with("frontmatter:")),
                    "{}: Ambiguous must name markers from both schemas, got {matched:?}",
                    path.display()
                );
            }
            C::Unsupported { .. } => {
                assert_eq!(got.code(), CONFIG_SCHEMA_UNSUPPORTED, "{}", path.display());
            }
            C::Malformed { .. } => {
                assert_eq!(got.code(), CONFIG_PARSE_FAILED, "{}", path.display());
            }
        }
    }
    assert!(
        supported > 0,
        "no repo ggen.toml classified into a supported schema"
    );
}

#[test]
fn classification_is_deterministic_over_the_corpus() {
    for (path, raw) in corpus() {
        assert_eq!(
            classify_ggen_toml(&raw),
            classify_ggen_toml(&raw),
            "{} classified differently on a second pass",
            path.display()
        );
    }
}

#[test]
fn hybrid_documents_are_refused_as_ambiguous_not_warned() {
    // [generation] is declarative-exclusive; [law].shapes is frontmatter-exclusive.
    let hybrids = [
        "[project]\nname = \"h\"\n[generation]\noutput_dir = \"o\"\n[law]\nshapes = \"s.ttl\"\n",
        "[generation]\noutput_dir = \"o\"\n[law]\nshapes = \"s.ttl\"\n",
    ];
    for raw in hybrids {
        let got = classify_ggen_toml(raw);
        assert!(
            matches!(got, C::Ambiguous { .. }),
            "hybrid must be Ambiguous (FM-CONFIG-101), got {got:?} for:\n{raw}"
        );
        assert_eq!(got.code(), CONFIG_SCHEMA_AMBIGUOUS);
    }
}
