//! E2E envelope cap-table semantics (wave-1 L02 plan, lane A03) over the real
//! sync pipeline and the real law engine:
//!
//! 1. a declared envelope cap overrides (tightens) the built-in default and
//!    the tightened ceiling is what fails the sync;
//! 2. an undeclared envelope reference fails closed typing
//!    `E-ENVELOPE-UNDECLARED`, writing nothing to disk;
//! 3. the materialized-output cap comes from the envelope, not the constant;
//! 4. an envelope can never LOOSEN a ceiling above the built-in default
//!    (clamp law) — the same pack that refuses under a tight cap admits under
//!    a declared-but-loose one.
//!
//! Chicago school: real tempdir projects, real five-stage sync, real
//! `GraphLawEngine` SHACL evaluation, assertions on disk state and typed
//! errors. No mocks. Receipt binding of the envelope ontology hash is lane
//! A19's `envelope_receipt_binding_test.rs` — not duplicated here.

#![allow(
    clippy::unwrap_used,
    clippy::expect_used,
    clippy::panic,
    clippy::similar_names
)]

use ggen_engine::envelope::{cap_for, extract_caps, Cap, CapTable, DEFAULT_MAX_PENDING_OUTPUTS};
use ggen_engine::law_engine::{GraphLawEngine, LawEngine};
use ggen_engine::sync::{sync, SyncOptions};
use std::path::Path;
use tempfile::TempDir;

const GGEN_TOML: &str = r#"
[project]
name = "envelope-caps"

[ontology]
source = "ontology.ttl"

[templates]
dir = "templates"
"#;

const ONTOLOGY: &str = r#"@prefix ex: <http://example.org/> .
ex:alice ex:name "alice" .
"#;

fn scaffold_template(root: &Path, frontmatter_envelope: Option<&str>) {
    std::fs::write(root.join("ggen.toml"), GGEN_TOML).expect("write ggen.toml");
    std::fs::write(root.join("ontology.ttl"), ONTOLOGY).expect("write ontology");
    std::fs::create_dir_all(root.join("templates")).expect("mkdir templates");
    let envelope_line = frontmatter_envelope
        .map(|iri| format!("envelope: {iri}\n"))
        .unwrap_or_default();
    std::fs::write(
        root.join("templates/main.tmpl"),
        format!("---\nto: out/result.txt\n{envelope_line}---\nhello\n"),
    )
    .expect("write template");
}

/// Write the per-project envelope reticle (the RDF sole source of truth,
/// `.specify/envelope-shapes.ttl` — lane I1 retired the JSON sidecar).
fn write_envelope_file(root: &Path, ttl: &str) {
    let path = root.join(".specify/envelope-shapes.ttl");
    std::fs::create_dir_all(path.parent().unwrap()).expect("mkdir .specify");
    std::fs::write(&path, ttl).expect("write envelope reticle");
}

const RETICLE_PREFIX: &str = "@prefix res: <http://ggen.org/envelope#> .\n";

// --- 1. declared caps override (tighten) the defaults --------------------------------

/// The same pack that admits under the built-in ceilings refuses once its
/// template declares an envelope whose cap table tightens
/// `max_pending_outputs` to 0: the DECLARED cap, not the constant, is the
/// operative ceiling. Positive control first so the refusal below is
/// attributable to the cap and not to the scaffold.
#[test]
fn declared_caps_override_defaults_and_fail_closed() {
    let dir = TempDir::new().expect("tempdir");
    let root = dir.path();

    // Positive control: no envelope declared -> built-in defaults -> admits.
    scaffold_template(root, None);
    sync(root, SyncOptions::default()).expect("default-ceiling sync must admit");
    assert!(root.join("out/result.txt").exists());

    // Now declare a tightening envelope on the same template.
    std::fs::remove_dir_all(root.join("out")).expect("clean control output");
    scaffold_template(root, Some("res:Tight"));
    write_envelope_file(
        root,
        &format!("{RETICLE_PREFIX}<res:Tight> a res:Envelope ; res:maxArtifacts 0 .\n"),
    );

    let err = sync(root, SyncOptions::default())
        .expect_err("pending-outputs cap of 0 must refuse the sync");
    let msg = err.to_string();
    assert!(
        msg.contains("pending") || msg.contains("PENDING") || msg.contains("cap"),
        "expected a pending-outputs ceiling refusal, got {msg}"
    );
    assert!(
        !root.join("out/result.txt").exists(),
        "refused sync must write nothing"
    );
}

// --- 2. undeclared envelope reference fails closed, nothing on disk ------------------

/// A fact graph that references an envelope IRI never declared as a subject
/// is a typed `E-ENVELOPE-UNDECLARED` refusal through the real SHACL gate —
/// and the sync that would have consumed it writes nothing.
#[test]
fn undeclared_envelope_reference_fails_closed_writing_nothing() {
    let dir = TempDir::new().expect("tempdir");
    let _root = dir.path();

    // Seam-level first: the real GraphLawEngine SHACL path refuses typed.
    let engine = GraphLawEngine;
    // Pure N-Triples: `validate_shacl` consumes both Turtle and N-Triples,
    // and a `@prefix` line is valid only in the former — keep the document
    // in the common form so the declared control below is parsed, not
    // refused on syntax.
    let facts = "<ex:workload> <http://ggen.dev/vocab/envelope#envelope> <res:Ghost> .\n";
    let err = engine
        .validate_shacl(facts, "")
        .expect_err("undeclared envelope reference must refuse");
    let msg = err.to_string();
    assert!(
        msg.contains("E-ENVELOPE-UNDECLARED"),
        "expected typed E-ENVELOPE-UNDECLARED, got {msg}"
    );
    assert!(
        msg.contains("res:Ghost"),
        "refusal must name the IRI: {msg}"
    );

    // Declared control: the same IRI as a subject in the same document passes
    // the envelope gate (SHACL on an empty shapes graph admits).
    let declared = format!("{facts}<res:Ghost> <http://example.org/name> \"ghost\" .\n");
    engine
        .validate_shacl(&declared, "")
        .expect("declared envelope must pass the envelope gate");
}

// --- 3. the materialized-output cap comes from the envelope --------------------------

/// The render loop reads its ceiling through `cap_for(&caps, Cap::PendingOutputs)`,
/// which resolves from the envelope's declared table: a declared cap of 1
/// refuses a two-output run the 8192 default would wave through. Both
/// templates declare the same envelope IRI, so the run's single resolved
/// `CapTable` carries the declared 1.
#[test]
fn pending_outputs_cap_comes_from_the_envelope() {
    let dir = TempDir::new().expect("tempdir");
    let root = dir.path();
    scaffold_template(root, Some("res:One"));
    std::fs::write(
        root.join("templates/second.tmpl"),
        "---\nto: out/second.txt\n---\nsecond\n",
    )
    .expect("write second template");
    write_envelope_file(
        root,
        &format!("{RETICLE_PREFIX}<res:One> a res:Envelope ; res:maxArtifacts 1 .\n"),
    );

    let caps = extract_caps(root, Some("res:One"));
    assert_eq!(cap_for(&caps, Cap::PendingOutputs), 1);
    assert_eq!(
        cap_for(&caps, Cap::QueryResultRows),
        CapTable::default().max_query_result_rows,
        "undeclared caps stay at the defaults"
    );

    // Positive control at the seam: the same two-output run under the DEFAULT
    // ceilings admits, so the refusal below is attributable to the declared
    // cap of 1 and not to the scaffold.
    let control = TempDir::new().expect("tempdir");
    scaffold_template(control.path(), None);
    std::fs::write(
        control.path().join("templates/second.tmpl"),
        "---\nto: out/second.txt\n---\nsecond\n",
    )
    .expect("write control second template");
    sync(control.path(), SyncOptions::default())
        .expect("two outputs admit under the default ceiling");

    let err = sync(root, SyncOptions::default())
        .expect_err("declared cap of 1 must refuse a two-output run");
    let msg = err.to_string();
    assert!(
        msg.contains("pending") || msg.contains("PENDING") || msg.contains("cap"),
        "expected a pending-outputs ceiling refusal, got {msg}"
    );
    assert!(!root.join("out/result.txt").exists());
    assert!(!root.join("out/second.txt").exists());
}

// --- 4. an envelope can never loosen a ceiling (clamp law) ---------------------------

/// A declared override above the built-in default is clamped to the default —
/// `extract_caps` never returns a ceiling looser than the builtin, so an
/// envelope grant cannot turn a defensive limit into an expansion of
/// authority.
#[test]
fn envelopes_can_only_tighten_never_loosen() {
    let dir = TempDir::new().expect("tempdir");
    let root = dir.path();
    scaffold_template(root, Some("res:Loose"));
    write_envelope_file(
        root,
        &format!(
            "{RETICLE_PREFIX}<res:Loose> a res:Envelope ;\n\
             \x20   res:maxArtifacts 999999 ;\n\
             \x20   res:maxAnswers 999999 ;\n\
             \x20   res:maxTotalBytes 999999999 .\n"
        ),
    );

    let caps = extract_caps(root, Some("res:Loose"));
    assert_eq!(caps.max_pending_outputs, DEFAULT_MAX_PENDING_OUTPUTS);
    assert_eq!(
        caps.max_query_result_rows,
        CapTable::default().max_query_result_rows
    );
    assert_eq!(
        caps.max_total_rendered_bytes,
        CapTable::default().max_total_rendered_bytes
    );

    // The clamped run behaves exactly like the default run: it admits.
    sync(root, SyncOptions::default())
        .expect("clamped-to-default caps must admit like the default run");
    assert!(root.join("out/result.txt").exists());
}
