// Runner for a vendored subset of the real W3C `data-shapes` SHACL test suite
// (core constraint components), fetched from:
//   https://github.com/w3c/data-shapes/tree/gh-pages/data-shapes-test-suite/tests/core
//
// Each vendored file is either:
//   - self-contained: the shapes graph, data graph, and expected
//     `sh:ValidationReport` (embedded inside an `mf:Manifest` /
//     `sht:Validate` test-case description) are all the same Turtle
//     document (this is what the manifest's `sht:dataGraph <>` /
//     `sht:shapesGraph <>` self-reference means), or
//   - split: the manifest/expected-report file references separate
//     `...-data.ttl` / `...-shapes.ttl` files (only `node/qualified-001.ttl`
//     in this vendored subset).
//
// Rather than fully parsing the W3C manifest ontology (mf:/sht:) to extract
// expected results, this runner does lightweight text scanning of the
// vendored file's embedded `mf:result [ a sh:ValidationReport ; ... ]` block:
// it reads off the expected `sh:conforms` boolean and counts the number of
// embedded `sh:ValidationResult` stanzas (== expected violation count, since
// every case in this vendored subset uses the default severity,
// sh:Violation). This is a deliberately minimal approach -- see the doc
// comment on `expected_from_manifest` -- but is sufficient to validate real
// conformance against the vendored suite's official expected results.

use std::fs;
use std::path::{Path, PathBuf};

use praxis_graphlaw::parser::{Parser, Syntax};
use praxis_graphlaw::shacl::{ShapesGraph, Validator};
use praxis_graphlaw::tripleindex::TripleIndex;

struct Case {
    /// Human-readable test id, matches the vendored file layout.
    name: String,
    /// File containing the embedded manifest / expected sh:ValidationReport.
    meta: String,
    /// File to load as the data graph (often == `meta`).
    data: String,
    /// File to load as the shapes graph (often == `meta`).
    shapes: String,
}

/// Discover every vendored case by scanning `w3c_suite/` at test-run time,
/// rather than a hand-maintained list that can silently omit a newly-added
/// fixture (the failure mode a hardcoded list has no defense against). The
/// vendored layout (confirmed by inspection) is: each self-contained case is
/// a single `<name>.ttl` combining data/shapes/expected-report; the one split
/// case (`node/qualified-001`) additionally has sibling `<name>-data.ttl` /
/// `<name>-shapes.ttl` files, which we detect by suffix and pair up. Files
/// literally named `*-data.ttl` / `*-shapes.ttl` are never treated as their
/// own top-level case (they're referenced from their `<name>.ttl` sibling).
fn discover_cases(dir: &Path) -> Vec<Case> {
    let mut cases = Vec::new();
    for category in ["node", "property", "targets", "misc", "path"] {
        let category_dir = dir.join(category);
        let Ok(entries) = fs::read_dir(&category_dir) else {
            continue;
        };
        let mut file_names: Vec<String> = entries
            .filter_map(|e| e.ok())
            .filter_map(|e| e.file_name().into_string().ok())
            .filter(|n| n.ends_with(".ttl"))
            .collect();
        file_names.sort();

        for file_name in &file_names {
            if file_name.ends_with("-data.ttl") || file_name.ends_with("-shapes.ttl") {
                continue; // referenced from its `<name>.ttl` sibling, not a case on its own
            }
            let stem = file_name.strip_suffix(".ttl").unwrap();
            let data_sibling = format!("{stem}-data.ttl");
            let shapes_sibling = format!("{stem}-shapes.ttl");
            let (data, shapes) =
                if file_names.contains(&data_sibling) && file_names.contains(&shapes_sibling) {
                    (
                        format!("{category}/{data_sibling}"),
                        format!("{category}/{shapes_sibling}"),
                    )
                } else {
                    (
                        format!("{category}/{file_name}"),
                        format!("{category}/{file_name}"),
                    )
                };
            cases.push(Case {
                name: format!("{category}/{stem}"),
                meta: format!("{category}/{file_name}"),
                data,
                shapes,
            });
        }
    }
    cases
}

fn suite_dir() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR")).join("tests/shacl_conformance/w3c_suite")
}

/// The vendored manifest files use relative IRIs -- the same-document
/// self-reference `<>` (per `sht:dataGraph <>` / `sht:shapesGraph <>`) and
/// per-test-case fragment-like refs such as `<and-001>` (used as the subject
/// of the `sht:Validate` description and inside `mf:entries (...)`) -- to
/// refer to parts of the manifest document itself. Our Turtle parser is
/// invoked without a base IRI (see `Parser::parse_triples`), so bare relative
/// IRIs fail to resolve ("No scheme found in an absolute IRI").
///
/// Since these manifest/test-case triples are irrelevant to SHACL validation
/// (they're never a shape's subject/predicate/object in a way our validator
/// interprets) and every genuinely absolute IRI in these files has a
/// `scheme:` prefix (even the deliberately unusual `<a:b>` in
/// `maxLength-001.ttl`), rewriting any bracketed term with no `:` into a
/// fixed placeholder absolute IRI is a safe, meaning-preserving fix-up here.
fn resolve_relative_self_refs(content: &str) -> String {
    let mut out = String::with_capacity(content.len());
    let mut rest = content;
    while let Some(start) = rest.find('<') {
        let (before, after_lt) = rest.split_at(start);
        out.push_str(before);
        let after_lt = &after_lt[1..]; // skip '<'
        if let Some(end) = after_lt.find('>') {
            let inner = &after_lt[..end];
            if inner.contains(':') {
                // Looks like an absolute IRI (has a scheme) -- leave as-is.
                out.push('<');
                out.push_str(inner);
                out.push('>');
            } else {
                out.push_str("<http://example.org/w3c-shacl-test-doc");
                if !inner.is_empty() {
                    out.push('#');
                    out.push_str(inner);
                }
                out.push('>');
            }
            rest = &after_lt[end + 1..];
        } else {
            // Unterminated '<' (shouldn't happen in well-formed Turtle) -- copy verbatim.
            out.push('<');
            rest = after_lt;
        }
    }
    out.push_str(rest);
    out
}

fn build_index(content: &str, source: &str) -> TripleIndex {
    let resolved = resolve_relative_self_refs(content);
    let triples = Parser::parse_triples(&resolved, Syntax::Turtle)
        .unwrap_or_else(|e| panic!("failed to parse vendored file {source}: {e}"));
    let mut index = TripleIndex::new();
    for t in triples {
        index.add(t);
    }
    index
}

/// Extract `(expected_conforms, expected_violation_count)` from the embedded
/// W3C manifest test-case description via lightweight text scanning rather
/// than a full mf:/sht: manifest-ontology parse. This works because every
/// vendored case's `mf:result [ a sh:ValidationReport ; sh:conforms "..." ;
/// sh:result [ a sh:ValidationResult ; ... ] ... ]` block is textually
/// regular: exactly one `sh:conforms "true|false"` literal, and one
/// `rdf:type sh:ValidationResult` triple per expected result. All cases in
/// this vendored subset use the default severity (sh:Violation), so the
/// raw stanza count is directly comparable to `report.results.len()`.
fn expected_from_manifest(content: &str) -> (bool, usize) {
    let conforms = content.contains("sh:conforms \"true\"");
    debug_assert!(
        conforms || content.contains("sh:conforms \"false\""),
        "vendored file has no recognizable sh:conforms literal"
    );
    let violation_count = content.matches("rdf:type sh:ValidationResult").count()
        + content.matches("a sh:ValidationResult").count();
    (conforms, violation_count)
}

struct CaseOutcome {
    name: String,
    passed: bool,
    detail: String,
}

#[test]
fn test_w3c_core_constraint_component_suite() {
    let dir = suite_dir();
    let cases = discover_cases(&dir);
    assert!(
        !cases.is_empty(),
        "no vendored SHACL conformance cases found under {}",
        dir.display()
    );
    let mut outcomes = Vec::with_capacity(cases.len());

    for case in &cases {
        let meta_path = dir.join(&case.meta);
        let meta_content = fs::read_to_string(&meta_path)
            .unwrap_or_else(|e| panic!("missing vendored file {}: {e}", meta_path.display()));

        let data_content = if case.data == case.meta {
            meta_content.clone()
        } else {
            fs::read_to_string(dir.join(&case.data))
                .unwrap_or_else(|e| panic!("missing {}: {e}", case.data))
        };
        let shapes_content = if case.shapes == case.meta {
            meta_content.clone()
        } else if case.shapes == case.data {
            data_content.clone()
        } else {
            fs::read_to_string(dir.join(&case.shapes))
                .unwrap_or_else(|e| panic!("missing {}: {e}", case.shapes))
        };

        let (expected_conforms, expected_violations) = expected_from_manifest(&meta_content);

        let data_index = build_index(&data_content, &case.data);
        let shapes = ShapesGraph::parse(&resolve_relative_self_refs(&shapes_content))
            .unwrap_or_else(|e| panic!("failed to parse vendored shapes {}: {e}", case.shapes));
        let report = Validator::validate(&data_index, &shapes);

        let actual_violations = report.results.len();
        let passed =
            report.conforms == expected_conforms && actual_violations == expected_violations;
        outcomes.push(CaseOutcome {
            name: case.name.clone(),
            passed,
            detail: format!(
                "expected conforms={expected_conforms} violations={expected_violations}, got conforms={} violations={actual_violations}",
                report.conforms
            ),
        });
    }

    let total = outcomes.len();
    let passed = outcomes.iter().filter(|o| o.passed).count();
    let failed = outcomes.iter().filter(|o| !o.passed).collect::<Vec<_>>();

    write_manifest_report(total, passed, &outcomes);

    assert!(
        failed.is_empty(),
        "W3C SHACL core-constraint-component suite failures ({}/{} failed):\n{}",
        failed.len(),
        total,
        failed
            .iter()
            .map(|o| format!("  {}: {}", o.name, o.detail))
            .collect::<Vec<_>>()
            .join("\n")
    );
}

fn write_manifest_report(total: usize, passed: usize, outcomes: &[CaseOutcome]) {
    let manifest_dir = Path::new(env!("CARGO_MANIFEST_DIR")).join("docs/jira/26.7.4/manifests");
    fs::create_dir_all(&manifest_dir).expect("Failed to create manifests directory");

    let pass_rate = if total == 0 {
        0.0
    } else {
        (passed as f64) * 100.0 / (total as f64)
    };
    let failed = total - passed;

    let mut report = String::new();
    report.push_str("# SHACL Conformance Pass-Rate Manifest\n\n");
    report.push_str("- **Dialect**: SHACL\n");
    report.push_str("- **Suite**: W3C data-shapes Test Suite (vendored core-constraint-component subset: node/, property/, targets/, misc/)\n");
    report.push_str("- **Source**: https://github.com/w3c/data-shapes/tree/gh-pages/data-shapes-test-suite/tests/core\n");
    report.push_str("- **Coverage note**: `shacl.rs`'s `eval_path` supports sequence/inverse/alternative/zeroOrMore/oneOrMore/zeroOrOne property paths (see `core/path/` cases below); cases are discovered by scanning `w3c_suite/` at test-run time (see `discover_cases`), so this manifest always reflects exactly what's vendored -- no case can be silently un-wired.\n");
    report.push_str(&format!("- **Total Tests**: {total}\n"));
    report.push_str(&format!("- **Passed**: {passed}\n"));
    report.push_str(&format!("- **Failed**: {failed}\n"));
    report.push_str(&format!("- **Pass Rate**: {pass_rate:.1}%\n\n"));
    report.push_str("## Test Details\n\n");
    report.push_str("| Test Name | Status | Details |\n");
    report.push_str("|---|---|---|\n");
    for o in outcomes {
        let status = if o.passed { "PASSED" } else { "FAILED" };
        report.push_str(&format!("| {} | {} | {} |\n", o.name, status, o.detail));
    }

    fs::write(manifest_dir.join("shacl_manifest.md"), report)
        .expect("Failed to write SHACL manifest report");
}
