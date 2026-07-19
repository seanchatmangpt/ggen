//! Proof test for `chicago-tdd-tools-pack`'s sibling generated file
//! (`tests/chicago_tdd_tools_boundary.rs`, from `cli_boundary_tests.rs.tmpl`).
//!
//! Unlike the pre-2026-07-18 version of this file, `EXPECTED` below is no
//! longer hand-transcribed from `ontology.ttl` inside the `.tmpl` source: it
//! is rendered from the SAME `sparql: tests` query that
//! `cli_boundary_tests.rs.tmpl` runs against `ontology.ttl` in the same
//! `ggen sync` pass. That closes the drift gap the L5 audit named directly
//! (`docs/packs/L5_VALIDATION_REPORT.md`, chicago-tdd-tools-pack, Test
//! generation / Regeneration lifecycle rows): adding, removing, or editing a
//! `ctt:CliBoundaryTest` individual now re-derives both the sibling file AND
//! this proof's expectations from one query path, instead of requiring a
//! second by-hand edit to an `EXPECTED` Rust array baked into the template.
//! `EXPECTED_TEST_COUNT` is `tests.len()` at render time, not a hardcoded `3`.
//!
//! What is still NOT closed, and why: the two generated files are
//! independent Tera+SPARQL renders of the same query, so this proof cannot
//! call the sibling file's logic in-process (there is no reusable in-process
//! artifact here — see `pack.toml`'s description). It still reads the
//! REAL, already-rendered text of `tests/chicago_tdd_tools_boundary.rs` off
//! disk and asserts the query-derived literals appear in it verbatim. That
//! remaining gap (two renders of one query, not one render checked twice) is
//! a `ggen-engine` template-composition limitation (no import/no cross-
//! template artifact reuse today), not something this pack's own files can
//! close without touching shared engine code.

use std::env;
use std::fs;
use std::path::PathBuf;

/// One expectation, rendered directly from the `sparql: tests` query above
/// (the same query `cli_boundary_tests.rs.tmpl` runs) — no longer a
/// hand-transcribed literal baked into this template's Rust source.
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

/// Number of `ctt:CliBoundaryTest` individuals the SPARQL query above found
/// at the time of the last `ggen sync run` — derived from `tests | length`,
/// not a hand count.
const EXPECTED_TEST_COUNT: usize = 4;

const EXPECTED: [ExpectedBoundaryTest; EXPECTED_TEST_COUNT] = [
    // receiptctl_algorithm_list_succeeds
    ExpectedBoundaryTest {
        fn_signature: "fn receiptctl_algorithm_list_succeeds()",
        exit_code_call: "output.assert_exit_code(0);",
        stdout_call: Some("output.assert_stdout_contains(\"[\");"),
        stderr_call: None,
        axiom_doc: "/// Covers: receiptctl algorithm list (argv composed from clap-noun-verb-pack's AlgorithmList command) exits 0 and prints a JSON array",
    },
    // receiptctl_help_lists_verbs
    ExpectedBoundaryTest {
        fn_signature: "fn receiptctl_help_lists_verbs()",
        exit_code_call: "output.assert_exit_code(0);",
        stdout_call: Some("output.assert_stdout_contains(\"Usage\");"),
        stderr_call: None,
        axiom_doc: "/// Covers: receiptctl --help exits 0 with usage text",
    },
    // receiptctl_unknown_verb_fails_closed
    ExpectedBoundaryTest {
        fn_signature: "fn receiptctl_unknown_verb_fails_closed()",
        exit_code_call: "output.assert_exit_code(1);",
        stdout_call: None,
        stderr_call: Some("output.assert_stderr_contains(\"error\");"),
        axiom_doc: "/// Covers: an unknown subcommand exits nonzero with a clap error on stderr",
    },
    // receiptctl_version_emits_name
    ExpectedBoundaryTest {
        fn_signature: "fn receiptctl_version_emits_name()",
        exit_code_call: "output.assert_exit_code(0);",
        stdout_call: Some("output.assert_stdout_contains(\"cli \");"),
        stderr_call: None,
        axiom_doc: "/// Covers: receiptctl --version exits 0 and prints a version string",
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
fn generated_file_has_expected_boundary_test_count() {
    let generated = read_generated_boundary_file();
    // Count only lines that ARE `#[test]` (the real attribute), not lines
    // that merely mention it -- the generated file's own module doc comment
    // literally contains the text "`#[test]`" in prose describing what each
    // item does, which a naive `.matches("#[test]").count()` would count as
    // an extra occurrence alongside the real attributes (discovered by
    // actually running this test against a real consumer,
    // `examples/receiptctl`, not assumed).
    let actual_count = generated
        .lines()
        .filter(|line| line.trim() == "#[test]")
        .count();
    assert_eq!(
        actual_count, EXPECTED_TEST_COUNT,
        "expected exactly {EXPECTED_TEST_COUNT} #[test] fns in the \
         generated boundary file (query-derived from ontology.ttl), found {actual_count}"
    );
}

#[test]
fn generated_file_covers_every_query_derived_axiom() {
    let generated = read_generated_boundary_file();
    for expected in EXPECTED.iter() {
        assert!(
            generated.contains(expected.fn_signature),
            "missing expected fn signature {:?} in generated file",
            expected.fn_signature
        );
        assert!(
            generated.contains(expected.exit_code_call),
            "missing expected exit-code assertion {:?} for {:?}",
            expected.exit_code_call,
            expected.fn_signature
        );
        if let Some(stdout_call) = expected.stdout_call {
            assert!(
                generated.contains(stdout_call),
                "missing expected stdout assertion {:?} for {:?}",
                stdout_call,
                expected.fn_signature
            );
        }
        if let Some(stderr_call) = expected.stderr_call {
            assert!(
                generated.contains(stderr_call),
                "missing expected stderr assertion {:?} for {:?}",
                stderr_call,
                expected.fn_signature
            );
        }
        assert!(
            generated.contains(expected.axiom_doc),
            "missing expected axiom doc comment {:?} for {:?}",
            expected.axiom_doc,
            expected.fn_signature
        );
    }
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
