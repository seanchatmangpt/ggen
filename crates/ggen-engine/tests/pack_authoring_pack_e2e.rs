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

    let tutorial = read(&project, "docs/pack-authoring/tutorial.md");
    assert!(tutorial.contains("# Tutorial: build your first ggen pack"));
    assert!(tutorial.contains("ggen sync run"));

    let how_to_gate = read(&project, "docs/pack-authoring/how-to-add-a-gate.md");
    assert!(how_to_gate.contains("# How to: add an admission gate"));

    let how_to_test = read(
        &project,
        "docs/pack-authoring/how-to-write-a-chicago-e2e-test.md",
    );
    assert!(how_to_test.contains("scaffold_pack"));
    assert!(how_to_test.contains("assert_idempotent"));
    assert!(how_to_test.contains("assert_gate_refuses"));

    let reference = read(&project, "docs/pack-authoring/reference.md");
    assert!(reference.contains("FM-WRITE-008"));

    let explanation = read(&project, "docs/pack-authoring/explanation.md");
    assert!(explanation.contains("RDF is the source of truth"));

    let errc = read(&project, "docs/pack-authoring/errc-testing-pattern.md");
    assert!(errc.contains("# ERRC: the pack-testing pattern"));
    assert!(errc.contains("## Eliminate"));
    assert!(errc.contains("## Reduce"));
    assert!(errc.contains("## Raise"));
    assert!(errc.contains("## Create"));

    let pack_toml = read(&project, "packs/playground-pack/pack.toml");
    assert!(pack_toml.contains(r#"name = "playground-pack""#));
    assert!(pack_toml.contains("Throwaway pack scaffolded"));

    let scaffolded_ontology = read(&project, "packs/playground-pack/ontology.ttl");
    assert!(scaffolded_ontology.contains("pg:Widget"));
    assert!(scaffolded_ontology.contains("pg:ExampleWidget"));
    assert!(scaffolded_ontology.contains("pg:value"));

    let example_tmpl = read(
        &project,
        "packs/playground-pack/templates/example_widget.tmpl",
    );
    assert!(example_tmpl
        .contains("PREFIX pg: <http://seanchatmangpt.github.io/packs/playground-pack#>"));
    assert!(example_tmpl.contains("{{ row.value }}"));

    let gate = read(&project, "packs/playground-pack/gates/010_required.rq");
    assert!(gate.contains("pg:Widget"));
    assert!(gate.contains("pg:value"));

    let generated_test = read(&project, "crates/ggen-engine/tests/playground_pack_e2e.rs");
    assert!(generated_test.contains("mod support;"));
    assert!(generated_test
        .contains("use support::{assert_gate_refuses, assert_idempotent, scaffold_pack};"));
    assert!(generated_test.contains("fn playground_pack_generates_and_is_idempotent()"));
    assert!(generated_test.contains("fn playground_pack_gate_refuses_missing_value()"));
    assert!(generated_test.contains(r#"scaffold_pack(&packs_dir().join("playground-pack"))"#));

    assert_idempotent(&project);

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
