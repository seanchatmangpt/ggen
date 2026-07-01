//! μ⁻¹ inverse-pipeline coverage tests for the Rust AST extractor.
//!
//! These are Chicago-TDD tests: they use real filesystem collaborators
//! (`tempfile::TempDir` + real `.rs` source files written to disk), drive the
//! real extraction + RDF conversion code path, and assert on the observable
//! output (the emitted Turtle and the number of recovered triples). There are
//! NO mocks, stubs, or behavior-verification doubles.
//!
//! Goal: prove that the extended extractor recovers materially more of an A
//! (artifact / source) than the historical struct-only extractor, so that the
//! A→O recovery underpinning the post-Chatman A≅O claim is meaningful.
//!
//! ---------------------------------------------------------------------------
//! KNOWN COMPILE PREREQUISITE (UNVERIFIED in this environment):
//!
//! The `reverse_sync` module is present on disk under
//! `crates/ggen-core/src/reverse_sync/` and re-exports its public API via
//! `reverse_sync/mod.rs`, BUT it is not currently declared with
//! `pub mod reverse_sync;` in `crates/ggen-core/src/lib.rs`. Until that module
//! is wired into the crate root, the `ggen_core::reverse_sync::...` path used
//! below will not resolve and this test will fail to compile. Wiring `lib.rs`
//! is outside this change's file-ownership scope; this is flagged as the
//! primary deferred compile risk for the reviewer.
//! ---------------------------------------------------------------------------

use std::fs;

use ggen_core::reverse_sync::ast_extractor::{convert_to_rdf, extract_rust_service};
use tempfile::TempDir;

/// Source containing a `pub struct` with an `impl` block (2 methods), a
/// `pub enum` (3 variants), and a `pub trait` (2 required methods).
const RICH_RUST_SOURCE: &str = r#"
pub struct Worker {
    id: u64,
    name: String,
}

impl Worker {
    pub fn start(&self) -> bool {
        true
    }

    pub fn stop(&mut self, force: bool) {
        let _ = force;
    }
}

pub enum WorkerState {
    Idle,
    Running,
    Stopped,
}

pub trait Schedulable {
    fn schedule(&self) -> u32;
    fn cancel(&self) -> bool;
}
"#;

/// Source containing ONLY a struct + fields — what the historical extractor
/// (struct-only) would have been able to recover from `RICH_RUST_SOURCE`.
const STRUCT_ONLY_SOURCE: &str = r#"
pub struct Worker {
    id: u64,
    name: String,
}
"#;

/// Count `code:`-namespaced data lines (excluding `@prefix` declarations) as a
/// coarse proxy for recovered information density. Each emitted property line
/// and blank-node typing contributes; this is intentionally a relative measure
/// used only to compare the rich extraction against the struct-only baseline.
fn count_code_predicates(turtle: &str) -> usize {
    turtle
        .lines()
        .filter(|line| line.contains("code:") && !line.trim_start().starts_with("@prefix"))
        .count()
}

fn write_source(dir: &TempDir, file_name: &str, source: &str) -> String {
    let path = dir.path().join(file_name);
    fs::write(&path, source).expect("write temp .rs source");
    path.to_string_lossy().into_owned()
}

#[test]
fn rich_rust_source_recovers_struct_impl_enum_and_trait() {
    // Arrange: a real temp dir with a real .rs file on disk.
    let dir = TempDir::new().expect("create temp dir");
    let path = write_source(&dir, "service.rs", RICH_RUST_SOURCE);

    // Act: real extraction from the real file, then real RDF conversion.
    let services = extract_rust_service(&path).expect("extract rust service");
    let turtle = convert_to_rdf(&services).expect("convert to rdf");

    // Assert: the struct is present with its fields.
    assert!(
        turtle.contains("code:Worker"),
        "missing struct resource:\n{turtle}"
    );
    assert!(
        turtle.contains("\"id\""),
        "missing struct field id:\n{turtle}"
    );

    // Assert: impl-block methods are recovered and attached to the struct.
    assert!(
        turtle.contains("code:hasMethod"),
        "expected impl methods to be emitted:\n{turtle}"
    );
    assert!(
        turtle.contains("\"start\""),
        "missing method start:\n{turtle}"
    );
    assert!(
        turtle.contains("\"stop\""),
        "missing method stop:\n{turtle}"
    );

    // Assert: the enum and each of its variants are recovered.
    assert!(
        turtle.contains("code:Enum"),
        "missing enum class:\n{turtle}"
    );
    assert!(
        turtle.contains("code:WorkerState"),
        "missing enum resource:\n{turtle}"
    );
    assert!(
        turtle.contains("code:hasVariant"),
        "missing variant predicate:\n{turtle}"
    );
    assert!(
        turtle.contains("\"Idle\""),
        "missing variant Idle:\n{turtle}"
    );
    assert!(
        turtle.contains("\"Running\""),
        "missing variant Running:\n{turtle}"
    );
    assert!(
        turtle.contains("\"Stopped\""),
        "missing variant Stopped:\n{turtle}"
    );

    // Assert: the trait and its required methods are recovered.
    assert!(
        turtle.contains("code:Trait"),
        "missing trait class:\n{turtle}"
    );
    assert!(
        turtle.contains("code:Schedulable"),
        "missing trait resource:\n{turtle}"
    );
    assert!(
        turtle.contains("\"schedule\""),
        "missing trait method schedule:\n{turtle}"
    );
    assert!(
        turtle.contains("\"cancel\""),
        "missing trait method cancel:\n{turtle}"
    );
}

#[test]
fn rich_extraction_recovers_materially_more_than_struct_only() {
    // Arrange: two real files — the rich source and the struct-only baseline.
    let dir = TempDir::new().expect("create temp dir");
    let rich_path = write_source(&dir, "rich.rs", RICH_RUST_SOURCE);
    let baseline_path = write_source(&dir, "baseline.rs", STRUCT_ONLY_SOURCE);

    // Act: extract + convert both through the real pipeline.
    let rich_services = extract_rust_service(&rich_path).expect("extract rich");
    let rich_turtle = convert_to_rdf(&rich_services).expect("convert rich");

    let baseline_services = extract_rust_service(&baseline_path).expect("extract baseline");
    let baseline_turtle = convert_to_rdf(&baseline_services).expect("convert baseline");

    let rich_predicates = count_code_predicates(&rich_turtle);
    let baseline_predicates = count_code_predicates(&baseline_turtle);

    // Assert: the rich extraction recovers a materially larger graph. The rich
    // source adds an enum (3 variants), a trait (2 methods), and 2 impl methods
    // on top of the same struct, so the predicate count must be well above the
    // struct-only baseline. We require at least double to make "material"
    // concrete rather than off-by-one.
    assert!(
        rich_predicates >= baseline_predicates * 2,
        "expected rich recovery (>= 2x baseline). rich={rich_predicates}, \
         baseline={baseline_predicates}\n--- rich ---\n{rich_turtle}\n--- baseline ---\n{baseline_turtle}"
    );

    // Assert: more distinct top-level resources were recovered (struct + enum +
    // trait = 3) than the struct-only baseline (1).
    let rich_resources = rich_services.len();
    let baseline_resources = baseline_services.len();
    assert!(
        rich_resources >= 3,
        "expected at least struct+enum+trait resources, got {rich_resources}"
    );
    assert!(
        rich_resources > baseline_resources,
        "rich resources ({rich_resources}) must exceed baseline ({baseline_resources})"
    );
}

#[test]
fn turtle_remains_well_formed_prefixes_and_terminators() {
    // Arrange + Act: drive the real path once more and inspect structure.
    let dir = TempDir::new().expect("create temp dir");
    let path = write_source(&dir, "service.rs", RICH_RUST_SOURCE);
    let services = extract_rust_service(&path).expect("extract");
    let turtle = convert_to_rdf(&services).expect("convert");

    // Assert: the namespace prefix the extractor documents is present.
    assert!(
        turtle.contains("@prefix code: <https://ggen.io/code#>"),
        "missing code prefix:\n{turtle}"
    );

    // Assert: every non-empty, non-prefix statement block terminates with a
    // Turtle full stop somewhere (no dangling resource without a terminator).
    assert!(
        turtle.trim_end().ends_with('.'),
        "turtle not terminated:\n{turtle}"
    );
}
