// Entry point for the `shacl_conformance` integration test binary.
//
// The actual test suite (a vendored subset of the real W3C `data-shapes`
// SHACL test suite) lives in `w3c_runner.rs`; the old single-test
// `mock_suite` fixture has been fully superseded and removed.
#[path = "w3c_runner.rs"]
mod w3c_runner;
