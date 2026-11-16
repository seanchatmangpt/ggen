//! Differential test: the author-time entry point (`SparqlAnalyzer`) and the
//! sync-time entry point (`GgenManifest::check` / `validate_paths`) must report
//! identical E0010/E0011/E0013 codes for the same query text. Both route
//! through `ggen_config::manifest::sparql_lint::lint_query`; this test pins
//! that no caller-side logic re-diverges.

use ggen_config::manifest::ManifestParser;
use star_toml::Validate;
use ggen_lsp::analyzers::sparql_analyzer::SparqlAnalyzer;
use lsp_max::lsp_types::NumberOrString;
use std::collections::BTreeSet;
use std::path::Path;

const LAWS: [&str; 3] = ["E0010", "E0011", "E0013"];

/// (snippet, is_construct)
const CORPUS: &[(&str, bool)] = &[
    ("SELECT ?s WHERE { ?s ?p ?o }", false),
    ("SELECT ?s WHERE { ?s ?p ?o } ORDER BY ?s", false),
    ("SELECT ?s WHERE { ?s ?p ?o } order by ?s", false),
    ("PREFIX ex: <http://e/>\nSELECT ?s WHERE { ?s a ex:T }", false),
    ("SELECT ?s WHERE { VALUES ?s { 1 2 } } ORDER BY ?s", false),
    ("SELECT ?s WHERE { VALUES ?s { 1 2 } }", false),
    ("# VALUES in comment\nSELECT ?s WHERE { ?s ?p ?o } ORDER BY ?s", false),
    ("CONSTRUCT { ?s ?p ?o } WHERE { ?s ?p ?o }", true),
    ("CONSTRUCT { ?s ?p ?o } WHERE { ?s ?p ?o } ORDER BY ?s", true),
    ("CONSTRUCT { ?s <http://e/x> ?o } WHERE { ?s ?p ?o }", true),
];

fn analyzer_codes(q: &str) -> BTreeSet<String> {
    SparqlAnalyzer::new_from_content(q)
        .expect("analyzer")
        .diagnostics()
        .iter()
        .filter_map(|d| match &d.lsp.code {
            Some(NumberOrString::String(c)) if LAWS.contains(&c.as_str()) => Some(c.clone()),
            _ => None,
        })
        .collect()
}

const HEAD: &str = "[project]\nname = \"t\"\nversion = \"1.0.0\"\n\n[ontology]\nsource = \"onto.ttl\"\n\n";

fn sync_codes_select(q: &str) -> BTreeSet<String> {
    // External-file route: validate_paths (E0010 + E0013), plus the pure-data
    // route is covered by the inline variant below for E0013.
    let dir = tempfile::TempDir::new().unwrap();
    std::fs::write(dir.path().join("onto.ttl"), "").unwrap();
    std::fs::write(dir.path().join("q.rq"), q).unwrap();
    let toml = format!(
        "{HEAD}[[generation.rules]]\nname = \"r\"\noutput_file = \"o.txt\"\nquery = {{ file = \"q.rq\" }}\ntemplate = {{ inline = \"x\" }}\n"
    );
    let manifest = ManifestParser::parse_str(&toml).unwrap();
    let mut out = BTreeSet::new();
    // validate_paths is fail-fast: it reports E0010 before E0013. Re-run with
    // the VALUES-free remainder to observe the second law independently.
    if let Err(e) = manifest.validate_paths(dir.path()) {
        let msg = e.to_string();
        for law in LAWS {
            if msg.contains(&format!("error[{law}]")) {
                out.insert(law.to_string());
            }
        }
    }
    if out.contains("E0010") {
        // E0013 is independent of E0010: probe it on the inline route.
        out.extend(sync_codes_inline_select(q).into_iter().filter(|c| c == "E0013"));
    }
    out
}

fn sync_codes_inline_select(q: &str) -> BTreeSet<String> {
    let toml = format!(
        "{HEAD}[[generation.rules]]\nname = \"r\"\noutput_file = \"o.txt\"\nquery = {{ inline = '''{q}''' }}\ntemplate = {{ inline = \"x\" }}\n"
    );
    let manifest = ManifestParser::parse_str(&toml).unwrap();
    match manifest.check() {
        Ok(()) => BTreeSet::new(),
        Err(errs) => errs
            .errors()
            .iter()
            .map(|e| e.code().to_string())
            .filter(|c| LAWS.contains(&c.as_str()))
            .collect(),
    }
}

fn sync_codes_construct(q: &str) -> BTreeSet<String> {
    let toml = format!(
        "{HEAD}[[inference.rules]]\nname = \"i\"\nconstruct = '''{q}'''\n"
    );
    let manifest = ManifestParser::parse_str(&toml).unwrap();
    match manifest.check() {
        Ok(()) => BTreeSet::new(),
        Err(errs) => errs
            .errors()
            .iter()
            .map(|e| e.code().to_string())
            .filter(|c| LAWS.contains(&c.as_str()))
            .collect(),
    }
}

#[test]
fn author_time_and_sync_time_agree_on_every_corpus_snippet() {
    for (q, is_construct) in CORPUS {
        let author = analyzer_codes(q);
        let sync = if *is_construct {
            sync_codes_construct(q)
        } else {
            // Inline route has no E0010 (VALUES is legal inline); the file
            // route has it. Union both sync routes' view of a SELECT.
            let mut s = sync_codes_select(q);
            let inline = sync_codes_inline_select(q);
            // E0013 must agree between the file and inline sync routes.
            assert_eq!(
                s.contains("E0013"),
                inline.contains("E0013"),
                "sync file vs inline E0013 disagree for {q:?}"
            );
            s.extend(inline);
            s
        };
        assert_eq!(author, sync, "author-time vs sync-time codes diverge for {q:?}");
    }
}

#[test]
fn corpus_exercises_every_law_and_a_clean_case() {
    let all: BTreeSet<String> = CORPUS.iter().flat_map(|(q, _)| analyzer_codes(q)).collect();
    for law in LAWS {
        assert!(all.contains(law), "corpus never triggers {law}");
    }
    assert!(CORPUS.iter().any(|(q, _)| analyzer_codes(q).is_empty()));
    let _ = Path::new(".");
}
