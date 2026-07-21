//! Chicago TDD conformance test: `.specify/ggen-product.ttl` (the product-mirror
//! ontology) vs. ggen's real source surface.
//!
//! Real collaborators only: `std::fs` reads of real files in this checkout, a real
//! `oxigraph`-backed `DeterministicGraph::insert_turtle`/`query` parse of the TTL
//! (via `ggen_engine::graph`), and manual string scans of the real `.rs` source
//! files. No mocks, no test doubles, no fixture copies of the source files.
//!
//! Three surfaces are checked, each two-way (every TTL individual has a real
//! source occurrence, AND every real source occurrence is present in the TTL):
//!
//! 1. CLI verbs — `clap_noun_verb_macros::verb("...")` attribute sites under
//!    `crates/ggen-engine/src/verbs/*.rs`.
//! 2. Pipeline stage span names — `"pipeline.<stage>"` string literals in
//!    `crates/ggen-engine/src/sync.rs`.
//! 3. FM-* diagnostic codes — real `AppError::fm_<family>(<number>, ...)` call
//!    sites across `crates/ggen-engine/src` and `crates/ggen-config/src`
//!    (excluding `error.rs` itself, which only defines the constructors plus its
//!    own doctests/unit tests — those are not call sites in product code).
//!
//! This test is deliberately structured so it CAN fail: comment out any TTL
//! individual (or add a fabricated one with no matching source), or add/remove a
//! real `verb`/`fm_*`/`pipeline.*` occurrence in source without updating the TTL,
//! and this test must fail. That sabotage cycle was manually run once while
//! authoring this test (see the PR/commit description for the observed
//! fail-then-restore transcript) and is not re-run automatically on every CI pass.

use ggen_engine::graph::DeterministicGraph;
use oxigraph::sparql::QueryResults;
use std::collections::BTreeSet;
use std::path::{Path, PathBuf};

/// Workspace root, resolved from this test binary's own compile-time location so
/// the test works regardless of the directory `cargo test` is invoked from.
fn workspace_root() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .and_then(Path::parent)
        .expect("crates/ggen-engine -> crates -> workspace root")
        .to_path_buf()
}

fn read_ttl() -> String {
    let path = workspace_root().join(".specify/ggen-product.ttl");
    std::fs::read_to_string(&path)
        .unwrap_or_else(|e| panic!("failed to read {}: {e}", path.display()))
}

fn read_source(rel: &str) -> String {
    let path = workspace_root().join(rel);
    std::fs::read_to_string(&path)
        .unwrap_or_else(|e| panic!("failed to read {}: {e}", path.display()))
}

// ---------------------------------------------------------------------------
// Real source scans
// ---------------------------------------------------------------------------

/// Scan `crates/ggen-engine/src/verbs/*.rs` for real
/// `#[clap_noun_verb_macros::verb("verb_name")]` attribute sites, returning
/// `(noun, verb)` pairs. `noun` is the file stem (`mod.rs`/`handlers.rs` are not
/// noun modules and are skipped).
fn scan_real_verbs() -> BTreeSet<(String, String)> {
    let dir = workspace_root().join("crates/ggen-engine/src/verbs");
    let mut out = BTreeSet::new();
    for entry in std::fs::read_dir(&dir)
        .unwrap_or_else(|e| panic!("failed to read_dir {}: {e}", dir.display()))
    {
        let entry = entry.expect("dir entry");
        let path = entry.path();
        if path.extension().and_then(|e| e.to_str()) != Some("rs") {
            continue;
        }
        let stem = path
            .file_stem()
            .and_then(|s| s.to_str())
            .expect("utf8 file stem")
            .to_string();
        if stem == "mod" || stem == "handlers" {
            continue;
        }
        let content = std::fs::read_to_string(&path)
            .unwrap_or_else(|e| panic!("failed to read {}: {e}", path.display()));
        for line in content.lines() {
            let trimmed = line.trim();
            if let Some(rest) = trimmed.strip_prefix("#[clap_noun_verb_macros::verb(\"") {
                let verb = rest.split('"').next().expect("verb name before quote");
                out.insert((stem.clone(), verb.to_string()));
            }
        }
    }
    out
}

/// Scan `crates/ggen-engine/src/sync.rs` for real `"pipeline.<stage>"` string
/// literals (the OTEL span names), returning the stage names.
fn scan_real_pipeline_stages() -> BTreeSet<String> {
    let content = read_source("crates/ggen-engine/src/sync.rs");
    let lines: Vec<&str> = content.lines().collect();
    let mut out = BTreeSet::new();
    // A span *name* is the first bare string literal argument immediately
    // following a `tracing::info_span!(` opening line — distinct from a
    // `"pipeline.stage" = ...` field key-value pair, or a bare
    // `"pipeline.duration_ms",` field-name-shorthand reference used later in
    // a `Span::record(...)` call, both of which also start with
    // `"pipeline.` but are not the macro's first argument.
    for (i, line) in lines.iter().enumerate() {
        if !line.trim_end().ends_with("info_span!(") {
            continue;
        }
        let Some(next) = lines.get(i + 1) else {
            continue;
        };
        let trimmed = next.trim();
        if let Some(rest) = trimmed.strip_prefix("\"pipeline.") {
            if let Some(end) = rest.find('"') {
                out.insert(rest[..end].to_string());
            }
        }
    }
    out
}

/// Scan `crates/ggen-engine/src` and `crates/ggen-config/src` (excluding
/// `error.rs`, the constructors' own definition/test file) for real
/// `fm_<family>(<number>` call sites, returning `(FAMILY, number)` pairs. The
/// number may appear on the same line as the call or on one of the next two
/// lines (multi-line call sites), matching how `AppError::fm_*` is actually
/// invoked across this codebase.
fn scan_real_fm_codes() -> BTreeSet<(String, u32)> {
    let families = [
        "cli", "chain", "graph", "tpl", "write", "pack", "config", "watch", "shell", "law", "gen",
        "key",
    ];
    let mut out = BTreeSet::new();
    for root in ["crates/ggen-engine/src", "crates/ggen-config/src"] {
        scan_dir_for_fm_codes(&workspace_root().join(root), &families, &mut out);
    }
    out
}

fn scan_dir_for_fm_codes(dir: &Path, families: &[&str], out: &mut BTreeSet<(String, u32)>) {
    for entry in std::fs::read_dir(dir)
        .unwrap_or_else(|e| panic!("failed to read_dir {}: {e}", dir.display()))
    {
        let entry = entry.expect("dir entry");
        let path = entry.path();
        if path.is_dir() {
            scan_dir_for_fm_codes(&path, families, out);
            continue;
        }
        if path.extension().and_then(|e| e.to_str()) != Some("rs") {
            continue;
        }
        if path.file_name().and_then(|n| n.to_str()) == Some("error.rs") {
            // The constructors' own definition + doctests/unit tests, not a
            // real product call site.
            continue;
        }
        let content = std::fs::read_to_string(&path)
            .unwrap_or_else(|e| panic!("failed to read {}: {e}", path.display()));
        let lines: Vec<&str> = content.lines().collect();
        for (i, line) in lines.iter().enumerate() {
            for family in families {
                let needle = format!("fm_{family}(");
                let Some(idx) = line.find(&needle) else {
                    continue;
                };
                let after = &line[idx + needle.len()..];
                let num = parse_leading_number(after).or_else(|| {
                    lines
                        .get(i + 1)
                        .and_then(|l| parse_leading_number(l))
                        .or_else(|| lines.get(i + 2).and_then(|l| parse_leading_number(l)))
                });
                if let Some(num) = num {
                    out.insert((family.to_uppercase(), num));
                }
            }
        }
    }
}

fn parse_leading_number(s: &str) -> Option<u32> {
    let trimmed = s.trim_start();
    let digits: String = trimmed.chars().take_while(|c| c.is_ascii_digit()).collect();
    if digits.is_empty() {
        None
    } else {
        digits.parse().ok()
    }
}

// ---------------------------------------------------------------------------
// TTL-side extraction (real oxigraph parse + SPARQL, via ggen_engine::graph)
// ---------------------------------------------------------------------------

fn load_product_graph() -> DeterministicGraph {
    let ttl = read_ttl();
    let g = DeterministicGraph::new().expect("in-memory oxigraph store");
    g.insert_turtle(&ttl)
        .unwrap_or_else(|e| panic!("failed to parse .specify/ggen-product.ttl: {e}"));
    g
}

fn ttl_verbs(g: &DeterministicGraph) -> BTreeSet<(String, String)> {
    let q = r#"
        PREFIX prod: <http://ggen.org/product#>
        SELECT ?noun ?verb WHERE {
            ?x a prod:CliVerb ; prod:noun ?noun ; prod:verb ?verb .
        } ORDER BY ?noun ?verb
    "#;
    let results = g.query(q).expect("query CliVerb individuals");
    let mut out = BTreeSet::new();
    if let QueryResults::Solutions(solutions) = results {
        for row in solutions {
            let row = row.expect("solution row");
            let noun = literal_value(&row, "noun");
            let verb = literal_value(&row, "verb");
            out.insert((noun, verb));
        }
    } else {
        panic!("expected SELECT solutions for CliVerb query");
    }
    out
}

fn ttl_pipeline_stages(g: &DeterministicGraph) -> BTreeSet<String> {
    let q = r#"
        PREFIX prod: <http://ggen.org/product#>
        SELECT ?span WHERE {
            ?x a prod:PipelineStage ; prod:spanName ?span .
        } ORDER BY ?span
    "#;
    let results = g.query(q).expect("query PipelineStage individuals");
    let mut out = BTreeSet::new();
    if let QueryResults::Solutions(solutions) = results {
        for row in solutions {
            let row = row.expect("solution row");
            let span = literal_value(&row, "span");
            let stage = span
                .strip_prefix("pipeline.")
                .unwrap_or_else(|| panic!("prod:spanName {span} must start with 'pipeline.'"));
            out.insert(stage.to_string());
        }
    } else {
        panic!("expected SELECT solutions for PipelineStage query");
    }
    out
}

fn ttl_fm_codes(g: &DeterministicGraph) -> BTreeSet<(String, u32)> {
    let q = r#"
        PREFIX prod: <http://ggen.org/product#>
        SELECT ?family ?number WHERE {
            ?x a prod:DiagnosticCode ; prod:family ?family ; prod:number ?number .
        } ORDER BY ?family ?number
    "#;
    let results = g.query(q).expect("query DiagnosticCode individuals");
    let mut out = BTreeSet::new();
    if let QueryResults::Solutions(solutions) = results {
        for row in solutions {
            let row = row.expect("solution row");
            let family = literal_value(&row, "family");
            let number_str = literal_value(&row, "number");
            let number: u32 = number_str
                .parse()
                .unwrap_or_else(|e| panic!("prod:number {number_str} not an integer: {e}"));
            out.insert((family, number));
        }
    } else {
        panic!("expected SELECT solutions for DiagnosticCode query");
    }
    out
}

/// Extract a SPARQL solution binding's lexical value (works for both plain
/// string literals and `xsd:integer`-typed literals — oxigraph's `Display`
/// for `Term::Literal` renders the lexical form either way, quoted for
/// strings).
fn literal_value(row: &oxigraph::sparql::QuerySolution, var: &str) -> String {
    let term = row
        .get(var)
        .unwrap_or_else(|| panic!("missing binding for ?{var}"));
    match term {
        oxigraph::model::Term::Literal(lit) => lit.value().to_string(),
        other => panic!("expected literal for ?{var}, got {other:?}"),
    }
}

// ---------------------------------------------------------------------------
// Tests: two-way equality per surface
// ---------------------------------------------------------------------------

#[test]
fn cli_verbs_ttl_matches_real_source() {
    let g = load_product_graph();
    let from_ttl = ttl_verbs(&g);
    let from_source = scan_real_verbs();

    let missing_from_ttl: Vec<_> = from_source.difference(&from_ttl).collect();
    let missing_from_source: Vec<_> = from_ttl.difference(&from_source).collect();

    assert!(
        missing_from_ttl.is_empty(),
        "real verb(s) found in crates/ggen-engine/src/verbs/*.rs but not declared \
         in .specify/ggen-product.ttl: {missing_from_ttl:?}"
    );
    assert!(
        missing_from_source.is_empty(),
        ".specify/ggen-product.ttl declares CliVerb individual(s) with no matching \
         real #[clap_noun_verb_macros::verb(...)] site: {missing_from_source:?}"
    );
    assert!(
        !from_ttl.is_empty(),
        "sanity: expected a non-empty verb surface"
    );
}

#[test]
fn pipeline_stages_ttl_matches_real_source() {
    let g = load_product_graph();
    let from_ttl = ttl_pipeline_stages(&g);
    let from_source = scan_real_pipeline_stages();

    let missing_from_ttl: Vec<_> = from_source.difference(&from_ttl).collect();
    let missing_from_source: Vec<_> = from_ttl.difference(&from_source).collect();

    assert!(
        missing_from_ttl.is_empty(),
        "real \"pipeline.<stage>\" span literal(s) found in \
         crates/ggen-engine/src/sync.rs but not declared in .specify/ggen-product.ttl: \
         {missing_from_ttl:?}"
    );
    assert!(
        missing_from_source.is_empty(),
        ".specify/ggen-product.ttl declares PipelineStage individual(s) with no \
         matching real \"pipeline.<stage>\" span literal in sync.rs: {missing_from_source:?}"
    );
    assert_eq!(
        from_ttl,
        ["load", "extract", "validate", "generate", "emit"]
            .into_iter()
            .map(str::to_string)
            .collect::<BTreeSet<_>>(),
        "expected exactly the five documented pipeline stages"
    );
}

#[test]
fn fm_diagnostic_codes_ttl_matches_real_source() {
    let g = load_product_graph();
    let from_ttl = ttl_fm_codes(&g);
    let from_source = scan_real_fm_codes();

    let missing_from_ttl: Vec<_> = from_source.difference(&from_ttl).collect();
    let missing_from_source: Vec<_> = from_ttl.difference(&from_source).collect();

    assert!(
        missing_from_ttl.is_empty(),
        "real FM-* call site(s) found in crates/ggen-engine/src or \
         crates/ggen-config/src but not declared in .specify/ggen-product.ttl: \
         {missing_from_ttl:?}"
    );
    assert!(
        missing_from_source.is_empty(),
        ".specify/ggen-product.ttl declares DiagnosticCode individual(s) with no \
         matching real fm_<family>(<number>, ...) call site: {missing_from_source:?}"
    );
    assert!(
        from_ttl.len() >= 50,
        "sanity: expected a substantial real FM-* diagnostic surface, got {}",
        from_ttl.len()
    );
}
