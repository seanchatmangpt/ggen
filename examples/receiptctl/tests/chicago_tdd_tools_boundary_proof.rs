//! Proof test for `chicago-tdd-tools-pack`'s sibling generated file
//! (`tests/chicago_tdd_tools_boundary.rs`, from `cli_boundary_tests.rs.tmpl`),
//! asserted against literal values hand-transcribed from
//! `packs/chicago-tdd-tools-pack/ontology.ttl`.
//!
//! Deliberately NO `sparql:` frontmatter key in this template (see
//! `cli_boundary_tests.rs.tmpl` / `cli_boundary_doc.md.tmpl` for the query
//! that *does* exist): this pack's two other templates generate `#[test]`
//! functions that spawn an EXTERNAL binary (`ctt:binary`, e.g. `receiptctl`)
//! the consuming project builds -- that binary is not shipped by this pack
//! and there is no reusable in-process function or data catalog here to
//! import and call directly (unlike the standard catalog-generating packs).
//! So the check available to this proof is: read back the REAL, already-
//! rendered text of the sibling generated file (produced by an independent
//! Tera + SPARQL execution against `ontology.ttl` in the same `ggen sync`
//! run) and assert that literal fn signatures / exit-code assertions /
//! stdout-stderr needles / axiom doc-comments -- all copied by hand from
//! the ontology, never recomputed via the same query path that produced the
//! sibling file -- actually appear in it. A templating regression, a
//! dropped `WHERE`-clause row, a mis-ordered `ORDER BY`, or a future
//! ontology edit that silently changes a needle/exit-code without updating
//! this proof will make these assertions fail.

use std::env;
use std::fs;
use std::path::PathBuf;

/// One hand-transcribed expectation copied from `ontology.ttl`'s
/// `ctt:CliBoundaryTest` individuals -- NOT re-derived from the SPARQL
/// query in `cli_boundary_tests.rs.tmpl`.
struct ExpectedBoundaryTest {
    /// Exact `fn <name>(` substring expected in the generated file.
    fn_signature: &'static str,
    /// Exact `output.assert_exit_code(N);` substring expected.
    exit_code_call: &'static str,
    /// Exact `output.assert_stdout_contains("...");` substring expected, if any.
    stdout_call: Option<&'static str>,
    /// Exact `output.assert_stderr_contains("...");` substring expected, if any.
    stderr_call: Option<&'static str>,
    /// Exact `/// Covers: ...` doc-comment substring expected.
    axiom_doc: &'static str,
}

/// `ontology.ttl` defines exactly 3 `ctt:CliBoundaryTest` individuals (no
/// more, no fewer) -- a hand count, not a query result.
const EXPECTED_TEST_COUNT: usize = 3;

const EXPECTED: [ExpectedBoundaryTest; EXPECTED_TEST_COUNT] = [
    // ctt:receiptctl-help
    ExpectedBoundaryTest {
        fn_signature: "fn receiptctl_help_lists_verbs()",
        exit_code_call: "output.assert_exit_code(0);",
        stdout_call: Some("output.assert_stdout_contains(\"Usage\");"),
        stderr_call: None,
        axiom_doc: "/// Covers: receiptctl --help exits 0 with usage text",
    },
    // ctt:receiptctl-version
    ExpectedBoundaryTest {
        fn_signature: "fn receiptctl_version_emits_name()",
        exit_code_call: "output.assert_exit_code(0);",
        // Trailing space is load-bearing: clap-noun-verb's `--version`
        // prints a generic "cli <version>" string, not the binary/package
        // name (see ontology.ttl's NOTE on this individual, verified live
        // against a real clap-noun-verb 26.7.4 consumer).
        stdout_call: Some("output.assert_stdout_contains(\"cli \");"),
        stderr_call: None,
        axiom_doc: "/// Covers: receiptctl --version exits 0 and prints a version string",
    },
    // ctt:receiptctl-unknown-verb
    ExpectedBoundaryTest {
        fn_signature: "fn receiptctl_unknown_verb_fails_closed()",
        exit_code_call: "output.assert_exit_code(1);",
        stdout_call: None,
        stderr_call: Some("output.assert_stderr_contains(\"error\");"),
        axiom_doc: "/// Covers: an unknown subcommand exits nonzero with a clap error on stderr",
    },
];

/// Reads the REAL, already-rendered sibling artifact from disk. Both this
/// file and `tests/chicago_tdd_tools_boundary.rs` are produced by the same
/// `ggen sync` run; by the time this proof's tests execute, the sibling
/// file must already exist as a plain fact of the pipeline, not a mock.
fn read_generated_boundary_file() -> String {
    let manifest_dir = env::var("CARGO_MANIFEST_DIR")
        .expect("CARGO_MANIFEST_DIR must be set by cargo when running tests");
    let path = PathBuf::from(manifest_dir).join("tests/chicago_tdd_tools_boundary.rs");
    fs::read_to_string(&path).unwrap_or_else(|e| {
        panic!(
            "expected sibling generated file at {} (produced by \
             cli_boundary_tests.rs.tmpl in the same ggen sync run): {e}",
            path.display()
        )
    })
}

#[test]
fn generated_file_has_exactly_three_boundary_tests() {
    let generated = read_generated_boundary_file();
    // Count only lines that ARE `#[test]` (the real attribute), not lines
    // that merely mention it -- the generated file's own module doc comment
    // literally contains the text "`#[test]`" in prose describing what each
    // item does, which a naive `.matches("#[test]").count()` would count as
    // a 4th occurrence alongside the 3 real attributes (discovered by
    // actually running this test against a real consumer,
    // `examples/receiptctl`, not assumed).
    let actual_count = generated
        .lines()
        .filter(|line| line.trim() == "#[test]")
        .count();
    assert_eq!(
        actual_count, EXPECTED_TEST_COUNT,
        "expected exactly {EXPECTED_TEST_COUNT} #[test] fns in the \
         generated boundary file (transcribed from ontology.ttl), found {actual_count}"
    );
}

#[test]
fn generated_file_covers_help_boundary_axiom() {
    let generated = read_generated_boundary_file();
    let expected = &EXPECTED[0];
    assert!(
        generated.contains(expected.fn_signature),
        "missing expected fn signature {:?} in generated file",
        expected.fn_signature
    );
    assert!(
        generated.contains(expected.exit_code_call),
        "missing expected exit-code assertion {:?}",
        expected.exit_code_call
    );
    assert!(
        generated.contains(expected.stdout_call.expect("literal Some in EXPECTED[0]")),
        "missing expected stdout assertion {:?}",
        expected.stdout_call
    );
    assert!(
        generated.contains(expected.axiom_doc),
        "missing expected axiom doc comment {:?}",
        expected.axiom_doc
    );
}

#[test]
fn generated_file_covers_version_boundary_axiom() {
    let generated = read_generated_boundary_file();
    let expected = &EXPECTED[1];
    assert!(
        generated.contains(expected.fn_signature),
        "missing expected fn signature {:?} in generated file",
        expected.fn_signature
    );
    assert!(
        generated.contains(expected.exit_code_call),
        "missing expected exit-code assertion {:?}",
        expected.exit_code_call
    );
    assert!(
        generated.contains(expected.stdout_call.expect("literal Some in EXPECTED[1]")),
        "missing expected stdout assertion {:?} (note the load-bearing \
         trailing space before the closing quote)",
        expected.stdout_call
    );
    assert!(
        generated.contains(expected.axiom_doc),
        "missing expected axiom doc comment {:?}",
        expected.axiom_doc
    );
}

#[test]
fn generated_file_covers_unknown_verb_boundary_axiom() {
    let generated = read_generated_boundary_file();
    let expected = &EXPECTED[2];
    assert!(
        generated.contains(expected.fn_signature),
        "missing expected fn signature {:?} in generated file",
        expected.fn_signature
    );
    assert!(
        generated.contains(expected.exit_code_call),
        "missing expected exit-code assertion {:?}",
        expected.exit_code_call
    );
    assert!(
        generated.contains(expected.stderr_call.expect("literal Some in EXPECTED[2]")),
        "missing expected stderr assertion {:?}",
        expected.stderr_call
    );
    assert!(
        generated.contains(expected.axiom_doc),
        "missing expected axiom doc comment {:?}",
        expected.axiom_doc
    );
}

/// Chicago TDD guard: the generated boundary tests must spawn a real
/// binary via `CliHarness`, never a mock/test double (see
/// `.claude/rules/rust/testing-forbidden.md`).
#[test]
fn generated_file_uses_real_cli_harness_not_a_mock() {
    let generated = read_generated_boundary_file();
    assert!(
        generated.contains("CliHarness::cargo_bin("),
        "generated boundary file must spawn a real binary via CliHarness::cargo_bin(...)"
    );
    // A blanket `!contains("mock")` is too naive: the generated file's own
    // doc comments explain the Chicago-TDD doctrine using the English word
    // "mock" ("No mocks.", "not mock interactions") precisely to disavow
    // it -- that prose would fail a substring ban outright (discovered by
    // actually running this test against a real consumer,
    // `examples/receiptctl`, not assumed). Check instead for real mock-USAGE
    // markers this repo's own testing-forbidden.md names explicitly.
    for forbidden in ["mockall", "#[automock]", "MockXxx", "expect_", ".times("] {
        assert!(
            !generated.contains(forbidden),
            "generated boundary file must never reference a mock/test double \
             (found forbidden marker {forbidden:?})"
        );
    }
}
