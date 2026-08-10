//! Chicago-TDD end-to-end proof for `packs/pack-authoring-pack`, dogfooding
//! its own generator: scaffolds a real consumer, declares ONE
//! `pat:NewPackSpec` for a throwaway "playground-pack", runs the real
//! `sync()`, and asserts on real generated file content — the four Diataxis
//! docs, the scaffolded playground pack's five files, and the generated
//! `<pack>_pack_e2e.rs` test file's exact shape.
//!
//! Compilation proof, deliberately scoped: building the generated e2e test
//! file inside a real, separate nested Cargo project (with `ggen-engine` as
//! a path dependency) would be the strongest possible proof it compiles,
//! but costs a full extra crate compile per run. Instead this test proves
//! the PATTERN the generated file encodes is correct by re-executing the
//! exact same `sync()` / idempotency / gate-sabotage calls the generated
//! file contains, directly in-process against the scaffolded
//! `playground-pack` pack dir — plus asserting the generated file's source
//! text contains the real function/import names, so a drift between "what
//! the template emits" and "what this test re-executes" is caught by the
//! substring assertions below, not silently missed. A future upgrade to a
//! real nested `cargo build --tests` proof is documented here, not silently
//! skipped.
//!
//! No `terraform apply` / mutating operations anywhere.

#![allow(clippy::unwrap_used, clippy::expect_used, clippy::panic)]

mod support;

use std::path::{Path, PathBuf};

use ggen_engine::sync::{sync, SyncOptions};
use support::{assert_gate_refuses, assert_idempotent, read, scaffold_pack};

fn packs_dir() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR")).join("../../packs")
}

const PLAYGROUND_SPEC: &str = r#"@prefix pat: <http://seanchatmangpt.github.io/packs/pack-authoring#> .

pat:PlaygroundSpec a pat:NewPackSpec ;
    pat:packName "playground-pack" ;
    pat:description "Throwaway pack scaffolded by the pack-authoring-pack dogfood test." ;
    pat:className "Widget" ;
    pat:prefixName "pg" ;
    pat:requiredProp "value" .
"#;

// Sequential real-filesystem/real-sync assertions over one scaffolded pack,
// not branching logic -- length comes from the number of files checked.
#[allow(clippy::too_many_lines)]
#[test]
fn pack_authoring_pack_scaffolds_docs_and_a_real_playground_pack() {
    let (_dir, project) = scaffold_pack(&packs_dir().join("pack-authoring-pack"));
    std::fs::write(project.join("ontology.ttl"), PLAYGROUND_SPEC)
        .expect("write playground NewPackSpec");

    sync(
        &project,
        SyncOptions {
            dry_run: false,
            ..Default::default()
        },
    )
    .expect("sync must generate docs + playground-pack scaffold");

    // (1) Diataxis docs: all four quadrants (five pages — how-to has two),
    // real non-empty content with the expected section markers.
    let tutorial = read(&project, "docs/pack-authoring/tutorial.md");
    assert!(
        tutorial.contains("# Tutorial: build your first ggen pack"),
        "tutorial.md missing its title: {tutorial}"
    );
    assert!(
        tutorial.contains("ggen sync run"),
        "tutorial.md must show the sync command: {tutorial}"
    );

    let how_to_gate = read(&project, "docs/pack-authoring/how-to-add-a-gate.md");
    assert!(
        how_to_gate.contains("# How to: add an admission gate"),
        "how-to-add-a-gate.md missing its title: {how_to_gate}"
    );

    let how_to_test = read(
        &project,
        "docs/pack-authoring/how-to-write-a-chicago-e2e-test.md",
    );
    assert!(
        how_to_test.contains("scaffold_pack")
            && how_to_test.contains("assert_idempotent")
            && how_to_test.contains("assert_gate_refuses"),
        "how-to-write-a-chicago-e2e-test.md must name all three harness fns: {how_to_test}"
    );

    let reference = read(&project, "docs/pack-authoring/reference.md");
    assert!(
        reference.contains("FM-WRITE-008"),
        "reference.md must document the fan-out collision code: {reference}"
    );

    let explanation = read(&project, "docs/pack-authoring/explanation.md");
    assert!(
        explanation.contains("RDF is the source of truth"),
        "explanation.md missing its core claim: {explanation}"
    );

    let errc = read(&project, "docs/pack-authoring/errc-testing-pattern.md");
    assert!(
        errc.contains("# ERRC: the pack-testing pattern")
            && errc.contains("## Eliminate")
            && errc.contains("## Reduce")
            && errc.contains("## Raise")
            && errc.contains("## Create"),
        "errc-testing-pattern.md missing its title or all four ERRC sections: {errc}"
    );

    // (2) Scaffolded playground-pack: all five files, real interpolated
    // content (not literal `{{ packName }}` left unrendered).
    let pack_toml = read(&project, "packs/playground-pack/pack.toml");
    assert!(
        pack_toml.contains(r#"name = "playground-pack""#)
            && pack_toml.contains("Throwaway pack scaffolded"),
        "pack.toml: {pack_toml}"
    );

    let scaffolded_ontology = read(&project, "packs/playground-pack/ontology.ttl");
    assert!(
        scaffolded_ontology.contains("pg:Widget")
            && scaffolded_ontology.contains("pg:ExampleWidget")
            && scaffolded_ontology.contains("pg:value"),
        "scaffolded ontology.ttl missing interpolated class/individual/property: {scaffolded_ontology}"
    );

    let example_tmpl = read(
        &project,
        "packs/playground-pack/templates/example_widget.tmpl",
    );
    assert!(
        example_tmpl
            .contains("PREFIX pg: <http://seanchatmangpt.github.io/packs/playground-pack#>")
            && example_tmpl.contains("{{ row.value }}"),
        "scaffolded template must be real, renderable Tera+SPARQL: {example_tmpl}"
    );

    let gate = read(&project, "packs/playground-pack/gates/010_required.rq");
    assert!(
        gate.contains("pg:Widget") && gate.contains("pg:value"),
        "scaffolded gate must reference the real class/property: {gate}"
    );

    // (3) Generated Chicago-TDD e2e test: real source, real fn names, real
    // harness imports — the concrete "example/chicago pattern" deliverable.
    let generated_test = read(&project, "crates/ggen-engine/tests/playground_pack_e2e.rs");
    assert!(
        generated_test.contains("mod support;"),
        "generated test must declare mod support;: {generated_test}"
    );
    assert!(
        generated_test
            .contains("use support::{assert_gate_refuses, assert_idempotent, scaffold_pack};"),
        "generated test must import all three harness fns: {generated_test}"
    );
    assert!(
        generated_test.contains("fn playground_pack_generates_and_is_idempotent()"),
        "generated test missing the generate+idempotency fn: {generated_test}"
    );
    assert!(
        generated_test.contains("fn playground_pack_gate_refuses_missing_value()"),
        "generated test missing the gate-sabotage fn: {generated_test}"
    );
    assert!(
        generated_test.contains(r#"scaffold_pack(&packs_dir().join("playground-pack"))"#),
        "generated test must scaffold the real playground-pack dir: {generated_test}"
    );

    // (4) Idempotency of the meta-generation itself: re-running sync()
    // against the SAME spec writes nothing new.
    assert_idempotent(&project);

    // (5) Re-execute the PATTERN the generated file encodes, directly
    // in-process against the real scaffolded playground-pack dir — proving
    // it actually works, not just that its source text looks plausible.
    // This is the "syntax + shape proof" tradeoff documented in this file's
    // module doc comment: a real nested `cargo build --tests` on the
    // generated file itself would be strictly stronger and is a documented
    // future upgrade, not silently skipped.
    let (_pg_dir, pg_project) = scaffold_pack(&project.join("packs/playground-pack"));
    sync(
        &pg_project,
        SyncOptions {
            dry_run: false,
            ..Default::default()
        },
    )
    .expect("baseline sync of the scaffolded playground-pack must succeed");
    assert_idempotent(&pg_project);
    assert_gate_refuses(
        &pg_project,
        "@prefix pg: <http://seanchatmangpt.github.io/packs/playground-pack#> .\n\
         pg:Sabotage a pg:Widget .\n",
        "010_required",
    );
}
