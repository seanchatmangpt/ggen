//! Workspace-wide symbol search (`workspace/symbol`).
//!
//! The interactive server's `documentSymbol` exposes the symbols of a *single*
//! open buffer. The editor's global "go to symbol in workspace" picker instead
//! asks for symbols across the entire project. We satisfy it with the SAME
//! machinery the rest of ggen-lsp uses:
//!
//! 1. [`crate::check::discover_law_surfaces`] enumerates every law surface
//!    (`.ttl`/`.rq`/`.tera`/`ggen.toml`, build/VCS dirs pruned) — identical to
//!    the headless gate, so the symbol picker and the gate agree on what counts
//!    as a ggen surface.
//! 2. [`crate::analyzers::build_analyzer`] + `document_symbols()` produce the
//!    per-file `DocumentSymbol` tree (RDF classes/properties, SPARQL vars,
//!    Tera blocks/macros, TOML sections).
//! 3. Each (possibly nested) `DocumentSymbol` is flattened and converted to a
//!    [`SymbolInformation`] whose `location` points at the real file + the
//!    symbol's range, so the editor can jump straight to the declaration.
//!
//! This means symbol search is never a separate, drift-prone index: it is a
//! projection of the live analyzers over the discovered law surfaces.

use std::path::Path;

use lsp_max::lsp_types::{Location, SymbolInformation, SymbolKind, Url};

/// Maximum number of symbols returned for a single `workspace/symbol` request.
///
/// A large monorepo can hold thousands of law-surface symbols; an editor picker
/// neither wants nor can usefully render all of them. We cap and *log* when the
/// cap fires rather than truncating silently (anti-fail-open: a silent cut would
/// make "symbol not found" indistinguishable from "symbol does not exist").
pub const MAX_RESULTS: usize = 500;

/// Collect every law-surface symbol under `root`, converted to
/// [`SymbolInformation`] with a real file [`Location`].
///
/// When `query` is non-empty, results are filtered to symbols whose name
/// contains `query` as a case-insensitive substring (the LSP convention for the
/// workspace symbol picker). An empty query returns all symbols (still capped).
///
/// Results are deterministic: law surfaces are discovered in sorted order and
/// each file's symbols are emitted in analyzer order.
#[must_use]
pub fn workspace_symbols(root: &Path, query: &str) -> Vec<SymbolInformation> {
    let needle = query.trim().to_lowercase();
    let mut out: Vec<SymbolInformation> = Vec::new();
    let mut total_matched: usize = 0;

    for path in crate::check::discover_law_surfaces(root) {
        let Ok(content) = std::fs::read_to_string(&path) else {
            continue;
        };
        let path_str = path.to_string_lossy();
        let Some(analyzer) = crate::analyzers::build_analyzer(&path_str, &content) else {
            continue;
        };
        let Some(doc_symbols) = analyzer.document_symbols() else {
            continue;
        };
        // A file:// URL is required for the editor to resolve the location. If the
        // path can't be turned into one (e.g. not absolute on some platforms), the
        // symbols are unaddressable — skip the file rather than emit a broken jump.
        let Ok(url) = url::Url::from_file_path(&path)
            .ok()
            .and_then(|u| u.to_string().parse::<Url>().ok())
            .ok_or(())
        else {
            continue;
        };

        let mut flat: Vec<&lsp_max::lsp_types::DocumentSymbol> = Vec::new();
        for sym in &doc_symbols {
            flatten(sym, &mut flat);
        }

        for sym in flat {
            if !needle.is_empty() && !sym.name.to_lowercase().contains(&needle) {
                continue;
            }
            total_matched += 1;
            // Keep counting matches past the cap so the log message is honest about
            // how many were dropped, but stop pushing once we hit MAX_RESULTS.
            if out.len() < MAX_RESULTS {
                out.push(to_symbol_information(sym, url.clone()));
            }
        }
    }

    if total_matched > out.len() {
        tracing::warn!(
            target: "ggen_lsp::workspace_symbol",
            matched = total_matched,
            returned = out.len(),
            cap = MAX_RESULTS,
            query = %query,
            "workspace/symbol results capped; {} of {} matching symbols dropped",
            total_matched - out.len(),
            total_matched
        );
    }

    out
}

/// Flatten a (possibly nested) [`DocumentSymbol`] into a list of references,
/// parent-before-children (pre-order), so containment order is preserved.
fn flatten<'a>(
    sym: &'a lsp_max::lsp_types::DocumentSymbol,
    acc: &mut Vec<&'a lsp_max::lsp_types::DocumentSymbol>,
) {
    acc.push(sym);
    if let Some(children) = &sym.children {
        for child in children {
            flatten(child, acc);
        }
    }
}

/// Convert a [`DocumentSymbol`] + the owning file URL into a
/// [`SymbolInformation`] addressed at the symbol's own range.
fn to_symbol_information(sym: &lsp_max::lsp_types::DocumentSymbol, url: Url) -> SymbolInformation {
    #[allow(deprecated)] // `deprecated` field is required by the struct; superseded by `tags`.
    SymbolInformation {
        name: sym.name.clone(),
        kind: sym.kind,
        tags: sym.tags.clone(),
        deprecated: None,
        location: Location {
            uri: url,
            range: sym.range,
        },
        container_name: None,
    }
}

/// Pretty-print a [`SymbolKind`] for diagnostics/test messages.
#[must_use]
pub fn kind_label(kind: SymbolKind) -> &'static str {
    match kind {
        SymbolKind::CLASS => "class",
        SymbolKind::PROPERTY => "property",
        SymbolKind::VARIABLE => "variable",
        SymbolKind::FUNCTION => "function",
        SymbolKind::NAMESPACE => "namespace",
        SymbolKind::FILE => "file",
        _ => "other",
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;

    /// Build a small project on disk with a TTL (classes + properties) and a
    /// `ggen.toml` (sections), then assert workspace_symbols surfaces both with
    /// correct file locations. Real filesystem, real analyzers — no fixtures.
    fn write_project() -> tempfile::TempDir {
        let dir = tempfile::TempDir::new().expect("tempdir");
        let root = dir.path();
        fs::create_dir_all(root.join(".specify/specs")).expect("mkdir");

        // RDF: declares a class (ex:Person) and a property (ex:name).
        let ttl = "\
@prefix ex: <http://example.org/> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
@prefix owl: <http://www.w3.org/2002/07/owl#> .

ex:Person a owl:Class .
ex:name a owl:DatatypeProperty ;
    rdfs:domain ex:Person .
";
        fs::write(root.join(".specify/specs/people.ttl"), ttl).expect("write ttl");

        // TOML: two sections become symbols.
        fs::write(
            root.join("ggen.toml"),
            "[project]\nname = \"demo\"\n\n[logging]\nlevel = \"info\"\n",
        )
        .expect("write toml");

        dir
    }

    #[test]
    fn surfaces_symbols_from_ttl_and_toml_with_locations() {
        let dir = write_project();
        let root = dir.path();

        let symbols = workspace_symbols(root, "");
        assert!(
            !symbols.is_empty(),
            "expected symbols from ttl + toml, got none"
        );

        // The RDF class is present, addressed at the .ttl file.
        let person = symbols
            .iter()
            .find(|s| s.name.contains("Person"))
            .expect("ex:Person class symbol present");
        assert_eq!(person.kind, SymbolKind::CLASS);
        assert!(
            url::Url::parse(person.location.uri.as_str())
                .ok()
                .and_then(|u| u.to_file_path().ok())
                .map(|p| p.ends_with("people.ttl"))
                .unwrap_or(false),
            "Person location must point at people.ttl, got {}",
            person.location.uri.as_str()
        );

        // The RDF property is present, addressed at the .ttl file.
        assert!(
            symbols
                .iter()
                .any(|s| s.name.contains("name") && s.kind == SymbolKind::PROPERTY),
            "ex:name property symbol present"
        );

        // The TOML sections are present, addressed at ggen.toml.
        let project = symbols
            .iter()
            .find(|s| s.name == "project")
            .expect("[project] section symbol present");
        assert!(
            url::Url::parse(project.location.uri.as_str())
                .ok()
                .and_then(|u| u.to_file_path().ok())
                .map(|p| p.ends_with("ggen.toml"))
                .unwrap_or(false),
            "project section must point at ggen.toml, got {}",
            project.location.uri.as_str()
        );
        assert!(
            symbols.iter().any(|s| s.name == "logging"),
            "[logging] section symbol present"
        );
    }

    #[test]
    fn query_filters_case_insensitively() {
        let dir = write_project();
        let root = dir.path();

        let all = workspace_symbols(root, "");
        // Lowercase query must still match the `ex:Person` class (case-insensitive).
        let filtered = workspace_symbols(root, "person");
        assert!(
            !filtered.is_empty(),
            "case-insensitive query 'person' must match ex:Person"
        );
        assert!(
            filtered
                .iter()
                .all(|s| s.name.to_lowercase().contains("person")),
            "every filtered symbol must contain the query substring"
        );
        assert!(
            filtered.len() < all.len(),
            "a specific query must narrow results: {} filtered vs {} total",
            filtered.len(),
            all.len()
        );
    }

    #[test]
    fn nonmatching_query_returns_empty() {
        let dir = write_project();
        let symbols = workspace_symbols(dir.path(), "no_such_symbol_xyzzy");
        assert!(
            symbols.is_empty(),
            "a query matching nothing returns no symbols, got {symbols:?}"
        );
    }

    #[test]
    fn empty_project_returns_no_symbols() {
        let dir = tempfile::TempDir::new().expect("tempdir");
        let symbols = workspace_symbols(dir.path(), "");
        assert!(symbols.is_empty(), "no law surfaces ⇒ no symbols");
    }
}
