use praxis_graphlaw::parser::{Parser, Syntax};
use praxis_graphlaw::shex::validate_shex;
use praxis_graphlaw::tripleindex::TripleIndex;
use std::fs;
use std::path::Path;

#[derive(serde::Deserialize)]
struct ShapeMapEntry {
    node: String,
    shape: String,
}

#[derive(serde::Deserialize)]
struct ExpectedResult {
    status: String,
    details: String,
}

/// Metadata sidecar for each vendored W3C shexTest case (see
/// `w3c_suite/NOTICE.md` for how these were produced).
#[derive(serde::Deserialize)]
struct W3cCaseMeta {
    shape: String,
    focus: String,
    /// "conformant" or "nonconformant", matching sht:ValidationTest /
    /// sht:ValidationFailure in the upstream manifest.
    expected: String,
    #[allow(dead_code)]
    source_schema: String,
    #[allow(dead_code)]
    source_shexc: String,
    #[allow(dead_code)]
    source_data: String,
    comment: String,
}

struct TestOutcome {
    name: String,
    passed: bool,
    expected: String,
    actual: String,
    detail: String,
}

fn build_data_index(data_str: &str) -> TripleIndex {
    let triples = Parser::parse_triples(data_str, Syntax::Turtle).unwrap();
    let mut index = TripleIndex::new();
    for t in triples {
        index.add(t);
    }
    index
}

/// The mock, hand-rolled single-case suite this crate started with. Kept as
/// its own smoke test; the real pass-rate accounting lives in
/// `test_w3c_shex_conformance_suite` below.
#[test]
fn test_shex_conformance() {
    let manifest_dir = Path::new(env!("CARGO_MANIFEST_DIR"));
    let mock_dir = manifest_dir.join("tests/shex_conformance/mock_suite");

    assert!(mock_dir.exists(), "Mock suite directory must exist");

    let data_path = mock_dir.join("data.ttl");
    let schema_path = mock_dir.join("schema.json");
    let shape_map_path = mock_dir.join("shape_map.json");
    let result_path = mock_dir.join("expected_result.json");

    let data_content = fs::read_to_string(&data_path).expect("Failed to read data.ttl");
    let schema_content = fs::read_to_string(&schema_path).expect("Failed to read schema.json");
    let shape_map_content =
        fs::read_to_string(&shape_map_path).expect("Failed to read shape_map.json");
    let result_content =
        fs::read_to_string(&result_path).expect("Failed to read expected_result.json");

    let data = build_data_index(&data_content);

    let shape_map_entries: Vec<ShapeMapEntry> = serde_json::from_str(&shape_map_content).unwrap();
    let shape_map: Vec<(String, String)> = shape_map_entries
        .into_iter()
        .map(|e| (e.node, e.shape))
        .collect();

    let report = validate_shex(&data, &schema_content, &shape_map).unwrap();

    let expected: ExpectedResult = serde_json::from_str(&result_content).unwrap();

    let matches_expected = if expected.status == "conformant" {
        report.conforms
    } else {
        !report.conforms
    };

    assert!(
        matches_expected,
        "ShEx validation result does not match expected result"
    );
}

/// Runs the mock suite case and every vendored W3C shexTest case, and
/// (re)generates `docs/jira/26.7.4/manifests/shex_manifest.md` from the real,
/// combined pass/fail counts.
///
/// ## Vendored subset and skip rationale
///
/// The full upstream shexTest `validation/` suite has ~1180 entries, and its
/// manifest (`validation/manifest.jsonld`) references schemas almost
/// exclusively in **ShExC** (compact syntax, `.shex` files). `praxis_graphlaw::shex`
/// only accepts **ShExJ** (`shex_ast::ast::Schema` parsed via
/// `serde_json::from_str`), so the vast majority of the suite cannot be run
/// as-is without a ShExC compiler roxi does not have.
///
/// This runner instead exercises a representative, hand-converted slice
/// vendored under `w3c_suite/cases/*` (see `w3c_suite/NOTICE.md` for the
/// conversion method and license notice). Each case's ShExJ schema is a
/// faithful field-by-field translation of the official `.shex` schema, kept
/// alongside the original ShExC source and upstream file paths in its
/// `meta.json` for auditing. The data files and expected
/// (non)conformance results are otherwise taken verbatim from the upstream
/// suite (a couple of data files that used `PREFIX`-relative IRIs were
/// expanded to fully-qualified IRIs; noted per-case).
///
/// Traits covered by the vendored slice: Empty, TriplePattern, OneOf,
/// EachOf, ShapeAnd (AndValueExpression), ShapeOr (OrValueExpression),
/// ShapeNot (NotValueExpression), Closed, Extra, NodeKind, ValueSet, Stem,
/// LengthFacet, PatternFacet, LanguageTagEquivalence, ToldBNode.
///
/// Traits *not* covered here (left for a future vendoring pass, all for the
/// same ShExC-vs-ShExJ reason): Abstract/Extends/ExtendsDiamond shape
/// inheritance, Import/Include (multi-file schema composition), external
/// semantic actions (ExternalShape, SemanticAction, OrderedSemanticActions),
/// ShapeMap-syntax parsing tests (roxi takes shape maps as `(node, shape)`
/// pairs directly, not the shapeMap compact syntax), and the ~230
/// ComparatorFacet / ~120 ValidLexicalForm numeric-literal-equivalence cases
/// (redundant in kind with the numeric-facet coverage already added in
/// `shex_validation.rs`, and each individually trivial ShExC schemas that
/// would add vendoring volume without new adapter-risk surface).
#[test]
fn test_w3c_shex_conformance_suite() {
    let manifest_dir = Path::new(env!("CARGO_MANIFEST_DIR"));

    let mut outcomes: Vec<TestOutcome> = Vec::new();

    // --- mock suite (1 case) ---
    {
        let mock_dir = manifest_dir.join("tests/shex_conformance/mock_suite");
        let data_content = fs::read_to_string(mock_dir.join("data.ttl")).unwrap();
        let schema_content = fs::read_to_string(mock_dir.join("schema.json")).unwrap();
        let shape_map_content = fs::read_to_string(mock_dir.join("shape_map.json")).unwrap();
        let result_content = fs::read_to_string(mock_dir.join("expected_result.json")).unwrap();

        let data = build_data_index(&data_content);
        let shape_map_entries: Vec<ShapeMapEntry> =
            serde_json::from_str(&shape_map_content).unwrap();
        let shape_map: Vec<(String, String)> = shape_map_entries
            .into_iter()
            .map(|e| (e.node, e.shape))
            .collect();
        let expected: ExpectedResult = serde_json::from_str(&result_content).unwrap();

        let report = validate_shex(&data, &schema_content, &shape_map).unwrap();
        let expected_conforms = expected.status == "conformant";
        outcomes.push(TestOutcome {
            name: "mock_suite/NodeConstraint literal".to_string(),
            passed: report.conforms == expected_conforms,
            expected: expected.status.clone(),
            actual: if report.conforms {
                "conformant".to_string()
            } else {
                "nonconformant".to_string()
            },
            detail: expected.details.clone(),
        });
    }

    // --- vendored W3C shexTest cases ---
    let w3c_dir = manifest_dir.join("tests/shex_conformance/w3c_suite/cases");
    assert!(
        w3c_dir.exists(),
        "Vendored W3C shexTest suite directory must exist"
    );

    let mut case_dirs: Vec<_> = fs::read_dir(&w3c_dir)
        .expect("Failed to read w3c_suite/cases")
        .filter_map(|e| e.ok())
        .map(|e| e.path())
        .filter(|p| p.is_dir())
        .collect();
    case_dirs.sort();
    assert!(
        !case_dirs.is_empty(),
        "No vendored W3C shexTest cases found"
    );

    for case_dir in &case_dirs {
        let name = case_dir.file_name().unwrap().to_string_lossy().to_string();

        let schema_content = fs::read_to_string(case_dir.join("schema.json"))
            .unwrap_or_else(|e| panic!("case '{name}': failed to read schema.json: {e}"));
        let data_content = fs::read_to_string(case_dir.join("data.ttl"))
            .unwrap_or_else(|e| panic!("case '{name}': failed to read data.ttl: {e}"));
        let meta_content = fs::read_to_string(case_dir.join("meta.json"))
            .unwrap_or_else(|e| panic!("case '{name}': failed to read meta.json: {e}"));
        let meta: W3cCaseMeta = serde_json::from_str(&meta_content)
            .unwrap_or_else(|e| panic!("case '{name}': failed to parse meta.json: {e}"));

        let data = build_data_index(&data_content);
        let shape_map = vec![(meta.focus.clone(), meta.shape.clone())];

        let report = validate_shex(&data, &schema_content, &shape_map)
            .unwrap_or_else(|e| panic!("case '{name}': validate_shex errored: {e}"));

        let expected_conforms = meta.expected == "conformant";
        let actual = if report.conforms {
            "conformant"
        } else {
            "nonconformant"
        };

        outcomes.push(TestOutcome {
            name: name.clone(),
            passed: report.conforms == expected_conforms,
            expected: meta.expected.clone(),
            actual: actual.to_string(),
            detail: meta.comment.clone(),
        });
    }

    // --- regenerate the manifest from real results, pass or fail ---
    let total = outcomes.len();
    let passed = outcomes.iter().filter(|o| o.passed).count();
    let failed = total - passed;
    let pass_rate = if total > 0 {
        (passed as f64 / total as f64) * 100.0
    } else {
        0.0
    };

    let mut rows = String::new();
    for o in &outcomes {
        let status = if o.passed { "PASSED" } else { "FAILED" };
        rows.push_str(&format!(
            "| {} | {} | expected={}, actual={}; {} |\n",
            o.name.replace('|', "\\|"),
            status,
            o.expected,
            o.actual,
            o.detail.replace('|', "\\|").replace('\n', " ")
        ));
    }

    let manifest_report = format!(
        "# ShEx Conformance Pass-Rate Manifest\n\n\
         - **Dialect**: ShEx\n\
         - **Suite**: shexTest Conformance Suite (mock smoke case + vendored W3C shexTest subset, see `lib/tests/shex_conformance/w3c_suite/NOTICE.md`)\n\
         - **Total Tests**: {total}\n\
         - **Passed**: {passed}\n\
         - **Failed**: {failed}\n\
         - **Pass Rate**: {pass_rate:.1}%\n\n\
         ## Test Details\n\n\
         | Test Name | Status | Details |\n\
         |---|---|---|\n\
         {rows}"
    );

    let manifests_dir = manifest_dir.join("docs/jira/26.7.4/manifests");
    fs::create_dir_all(&manifests_dir).expect("Failed to create manifests directory");
    fs::write(manifests_dir.join("shex_manifest.md"), manifest_report)
        .expect("Failed to write ShEx manifest report");

    let failing: Vec<&str> = outcomes
        .iter()
        .filter(|o| !o.passed)
        .map(|o| o.name.as_str())
        .collect();
    assert!(
        failing.is_empty(),
        "{} of {} ShEx conformance cases mismatched expected result: {:?}",
        failing.len(),
        total,
        failing
    );
}
