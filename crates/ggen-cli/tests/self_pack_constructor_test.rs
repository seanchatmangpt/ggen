//! Chicago TDD end-to-end coverage for the canonical self-pack constructor
//! (`ggen init-self` + `ggen pack new`) — see
//! `packs/ggen-self-pack/README.md` and
//! `/Users/sac/.claude/plans/exactly-i-was-conflating-witty-quasar.md`.
//!
//! Real subprocess invocations of the compiled `ggen` binary against real
//! `TempDir` filesystems, real `ggen-engine::sync` runs, real generated
//! files read back off disk. No mocks, no test doubles — per this crate's
//! Chicago TDD requirement.

#![allow(clippy::unwrap_used, clippy::expect_used, clippy::panic)]

use std::path::Path;

use chicago_tdd_tools::cli_proof::{CliHarness, CliOutput};
use tempfile::TempDir;

fn run(cwd: &Path, args: &[&str]) -> CliOutput {
    CliHarness::cargo_bin("ggen")
        .args(args.to_vec())
        .current_dir(cwd)
        .run()
        .expect("spawn ggen subprocess")
}

fn init_self(cwd: &Path) -> CliOutput {
    run(cwd, &["init-self"])
}

#[test]
fn init_self_materializes_every_self_pack_file() {
    let dir = TempDir::new().expect("tempdir");
    let out = init_self(dir.path());
    assert!(
        out.exit_code == 0,
        "init-self should succeed on a fresh dir: {out:?}"
    );

    let base = dir.path().join("packs").join("ggen-self-pack");
    for rel in [
        "ggen.toml",
        "pack.toml",
        "ontology.ttl",
        "input.ttl",
        "README.md",
        "queries/pack_new.rq",
        "gates/010_required_shape.rq",
        "templates/pack_toml.tmpl",
        "templates/pack_ontology.tmpl",
        "templates/pack_gate_required.tmpl",
        "templates/pack_readme.tmpl",
    ] {
        assert!(
            base.join(rel).is_file(),
            "expected {rel} to exist under {}",
            base.display()
        );
    }
}

#[test]
fn init_self_refuses_to_overwrite_without_force() {
    let dir = TempDir::new().expect("tempdir");
    assert!(init_self(dir.path()).exit_code == 0);

    // Hand-edit the project-local copy to prove it survives a second,
    // force-less `init-self` call untouched.
    let marker_path = dir
        .path()
        .join("packs")
        .join("ggen-self-pack")
        .join("README.md");
    std::fs::write(&marker_path, "HAND EDITED — must survive").expect("hand edit");

    // This verb follows the same convention as the existing `perform_init`
    // refusal path (init.rs's `has_ggen_artifacts && !force` branch):
    // a handled refusal is `Ok(Output{status:"error",..})`, exit code 0 —
    // not a process-level `Err`. Confirmed by running this test unmodified
    // against the real binary before adjusting the assertion, rather than
    // assumed from reading the source.
    let out = init_self(dir.path());
    assert_eq!(out.exit_code, 0, "a handled refusal still exits 0: {out:?}");
    assert!(
        out.stdout.contains("\"status\": \"error\""),
        "expected a status:error payload for an already-materialized self-pack, got: {out:?}"
    );
    let contents = std::fs::read_to_string(&marker_path).expect("read marker");
    assert_eq!(
        contents, "HAND EDITED — must survive",
        "a refused init-self must not touch the existing project-local copy"
    );
}

#[test]
fn init_self_force_overwrites() {
    let dir = TempDir::new().expect("tempdir");
    assert!(init_self(dir.path()).exit_code == 0);

    let marker_path = dir
        .path()
        .join("packs")
        .join("ggen-self-pack")
        .join("README.md");
    std::fs::write(&marker_path, "HAND EDITED — should be replaced").expect("hand edit");

    let out = run(dir.path(), &["init-self", "--force", "true"]);
    assert!(
        out.exit_code == 0,
        "forced init-self should succeed: {out:?}"
    );
    let contents = std::fs::read_to_string(&marker_path).expect("read marker");
    assert_ne!(
        contents, "HAND EDITED — should be replaced",
        "--force must overwrite the project-local copy with the canonical one"
    );
}

#[test]
fn pack_new_requires_init_self_first() {
    let dir = TempDir::new().expect("tempdir");
    // Deliberately skip init-self.
    let out = run(
        dir.path(),
        &[
            "pack",
            "new",
            "demo-pack",
            "--description",
            "x",
            "--namespace",
            "http://example.org/demo-pack#",
        ],
    );
    assert!(
        out.exit_code != 0,
        "pack new without init-self must fail, got: {out:?}"
    );
    assert!(
        !dir.path().join("packs").join("demo-pack").exists(),
        "no pack should be created when the self-pack constructor is missing"
    );
}

#[test]
fn pack_new_requires_description_and_namespace() {
    let dir = TempDir::new().expect("tempdir");
    assert!(init_self(dir.path()).exit_code == 0);

    let missing_description = run(
        dir.path(),
        &[
            "pack",
            "new",
            "demo-pack",
            "--namespace",
            "http://example.org/x#",
        ],
    );
    assert!(missing_description.exit_code != 0);

    let missing_namespace = run(
        dir.path(),
        &["pack", "new", "demo-pack", "--description", "x"],
    );
    assert!(missing_namespace.exit_code != 0);

    assert!(
        !dir.path().join("packs").join("demo-pack").exists(),
        "no pack should be created when required args are missing"
    );
}

#[test]
fn pack_new_creates_a_structurally_correct_pack_at_the_project_root() {
    let dir = TempDir::new().expect("tempdir");
    assert!(init_self(dir.path()).exit_code == 0);

    let out = run(
        dir.path(),
        &[
            "pack",
            "new",
            "demo-pack",
            "--description",
            "A test pack",
            "--namespace",
            "http://example.org/demo-pack#",
        ],
    );
    assert!(out.exit_code == 0, "pack new should succeed: {out:?}");

    // Lands at the PROJECT root, not nested under packs/ggen-self-pack/ —
    // this is the relocation fix (FM-WRITE-002 traversal guard forced the
    // self-pack's own `sync` to write nested; the verb relocates it).
    let pack_dir = dir.path().join("packs").join("demo-pack");
    assert!(
        !dir.path()
            .join("packs")
            .join("ggen-self-pack")
            .join("packs")
            .exists(),
        "no leftover nested scratch dir should remain"
    );

    let pack_toml =
        std::fs::read_to_string(pack_dir.join("pack.toml")).expect("read generated pack.toml");
    assert!(pack_toml.contains("name = \"demo-pack\""));
    assert!(pack_toml.contains("description = \"A test pack\""));

    let ontology =
        std::fs::read_to_string(pack_dir.join("ontology.ttl")).expect("read generated ontology");
    assert!(ontology.contains("http://example.org/demo-pack#"));

    assert!(pack_dir.join("gates").join("010_required.rq").is_file());
    assert!(pack_dir.join("README.md").is_file());

    // The receipt is chained under the self-pack, not the new pack — it
    // documents the *constructor's* run, exactly like an ordinary
    // `ggen sync run` receipt.
    let receipt_path = dir
        .path()
        .join("packs")
        .join("ggen-self-pack")
        .join(".ggen-v2")
        .join("receipt.json");
    assert!(receipt_path.is_file(), "expected a chained sync receipt");
}

#[test]
fn pack_new_refuses_to_clobber_an_existing_pack() {
    let dir = TempDir::new().expect("tempdir");
    assert!(init_self(dir.path()).exit_code == 0);

    let args = [
        "pack",
        "new",
        "demo-pack",
        "--description",
        "first",
        "--namespace",
        "http://example.org/demo-pack#",
    ];
    assert!(run(dir.path(), &args).exit_code == 0);

    let second = run(
        dir.path(),
        &[
            "pack",
            "new",
            "demo-pack",
            "--description",
            "second",
            "--namespace",
            "http://example.org/demo-pack#",
        ],
    );
    assert!(
        second.exit_code != 0,
        "creating the same pack name twice must fail, not silently overwrite: {second:?}"
    );
    let pack_toml =
        std::fs::read_to_string(dir.path().join("packs").join("demo-pack").join("pack.toml"))
            .expect("read pack.toml");
    assert!(
        pack_toml.contains("description = \"first\""),
        "the original pack must survive a refused duplicate creation"
    );
}

/// Sabotage test for the constructor-level gate: a hand-corrupted
/// `input.ttl` (missing `sp:namespace`) must be refused by
/// `gates/010_required_shape.rq` — a real SPARQL ASK/SELECT gate firing
/// inside `ggen sync run`, not a Rust-side check duplicating the ontology's
/// own rule.
#[test]
fn required_shape_gate_refuses_an_incomplete_sp_pack_individual() {
    let dir = TempDir::new().expect("tempdir");
    assert!(init_self(dir.path()).exit_code == 0);

    let self_pack_dir = dir.path().join("packs").join("ggen-self-pack");
    // Write an sp:Pack individual missing sp:namespace and
    // sp:hasTemplateRole — exactly the placeholder shape ggen-self-pack's
    // own checked-in input.ttl uses, confirmed (during development of this
    // feature) to trip the gate via a direct `ggen sync run` against the
    // canonical pack itself.
    std::fs::write(
        self_pack_dir.join("input.ttl"),
        "@prefix sp: <http://seanchatmangpt.github.io/packs/ggen-self#> .\n\
         sp:PendingPack a sp:Pack ; sp:name \"broken\" ; sp:version \"0.1.0\" .\n",
    )
    .expect("write corrupted input.ttl");

    let out = run(&self_pack_dir, &["sync", "run"]);
    assert!(
        out.exit_code != 0,
        "sync must refuse an sp:Pack individual missing required properties: {out:?}"
    );
    let combined = format!("{}{}", out.stdout, out.stderr);
    assert!(
        combined.contains("010_required_shape") || combined.contains("FM-LAW"),
        "expected the required-shape gate to name itself in the refusal, got: {combined}"
    );
    assert!(
        !dir.path().join("packs").join("broken").exists(),
        "no pack directory should be created when the gate refuses the run"
    );
}
