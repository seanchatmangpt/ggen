//! Chicago-TDD end-to-end proof for `gall-core-pack`.
//!
//! Real boundaries throughout: real filesystem, real pack loading, real
//! Oxigraph/SPARQL gates, real Tera generation, real git commits/worktrees,
//! real bash commands, real ggen receipt verification, admitted evidence, and
//! a final evidence sabotage that must be refused by the named crown gate.

use std::path::{Path, PathBuf};
use std::process::Command;

use ggen_engine::sync::{sync, SyncOptions};
use tempfile::TempDir;

fn packs_dir() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR")).join("../../packs")
}

fn copy_tree(src: &Path, dst: &Path) {
    std::fs::create_dir_all(dst).expect("mkdir destination");
    for entry in std::fs::read_dir(src).expect("read source directory") {
        let entry = entry.expect("directory entry");
        let from = entry.path();
        let to = dst.join(entry.file_name());
        if from.is_dir() {
            copy_tree(&from, &to);
        } else {
            std::fs::copy(&from, &to).expect("copy file");
        }
    }
}

fn git(project: &Path, args: &[&str]) {
    let output = Command::new("git")
        .current_dir(project)
        .args(args)
        .output()
        .expect("spawn git");
    assert!(
        output.status.success(),
        "git {:?} failed\nstdout={}\nstderr={}",
        args,
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );
}

fn commit_all(project: &Path, message: &str) {
    git(project, &["add", "."]);
    git(project, &["commit", "-m", message]);
}

fn sync_project(project: &Path) -> Result<ggen_engine::sync::SyncReport, String> {
    sync(
        project,
        SyncOptions {
            dry_run: false,
            ..Default::default()
        },
    )
    .map_err(|error| error.to_string())
}

fn write_manifest(project: &Path, include_evidence: bool) {
    let evidence = if include_evidence {
        "gall-evidence = { path = \"evidence/gall\", lock = false }\n"
    } else {
        ""
    };
    std::fs::write(
        project.join("ggen.toml"),
        format!(
            "[project]\nname = \"gall-core-e2e\"\n\n\
             [ontology]\nsource = \"ontology.ttl\"\n\n\
             [packs]\n\
             gall-core-pack = {{ path = \"../packs/gall-core-pack\" }}\n\
             {evidence}\n\
             [templates]\ndir = \"templates\"\n\n\
             [law]\nreflexive = true\n"
        ),
    )
    .expect("write ggen.toml");
}

fn planning_ontology() -> String {
    r#"@prefix gall: <http://seanchatmangpt.github.io/packs/gall-core#> .
@prefix ex:   <https://example.org/gall/> .

ex:program a gall:GallProgram ;
    gall:programId "GALL-CORE-E2E" ;
    gall:releaseIdentity "v26.7.30-test" ;
    gall:hasCheckpoint ex:checkpoint-000 .

ex:capability a gall:Capability ;
    gall:capabilityId "real-useful-file" ;
    gall:title "Real useful file crosses the filesystem boundary" .

ex:checkpoint-000 a gall:Checkpoint, gall:RequiredCheckpoint ;
    gall:checkpointId "GALL-CORE-000" ;
    gall:title "Executable Gall floor" ;
    gall:producesCapability ex:capability ;
    gall:runnerCommand "mkdir -p out && printf alive > out/useful.txt" ;
    gall:positiveWitness ex:witness ;
    gall:negativeFalsifier ex:falsifier ;
    gall:receiptObligation ex:receipt ;
    gall:replayObligation ex:replay ;
    gall:ownsArtifact ex:useful-artifact .

ex:useful-artifact gall:artifactPath "out/useful.txt" .

ex:witness a gall:PositiveWitness ;
    gall:name "useful-file-contains-alive" ;
    gall:command "grep -qx alive out/useful.txt" .

ex:falsifier a gall:NegativeFalsifier ;
    gall:name "forbidden-file-remains-absent" ;
    gall:command "test ! -e out/forbidden.txt" .

ex:receipt a gall:ReceiptObligation ;
    gall:name "ggen-receipt-verifies" ;
    gall:command "ggen receipt verify" .

ex:replay a gall:ReplayObligation ;
    gall:name "roadmap-replays-from-sealed-revision" ;
    gall:command "test -f docs/GALL_CHECKPOINT_ROADMAP.md" .
"#
    .to_string()
}

fn activate_crown(project: &Path) {
    let mut ontology = std::fs::read_to_string(project.join("ontology.ttl"))
        .expect("read planning ontology");
    ontology.push_str(
        r#"
ex:program gall:hasCrown ex:crown .

ex:crown a gall:Crown ;
    gall:crownId "GALL-CORE-CROWN" ;
    gall:includesCheckpoint ex:checkpoint-000 .
"#,
    );
    std::fs::write(project.join("ontology.ttl"), ontology).expect("activate crown");
}

fn run_generated_evidence(project: &Path) {
    let ggen_bin = assert_cmd::cargo::cargo_bin("ggen");
    let mut paths = vec![
        ggen_bin
            .parent()
            .expect("ggen binary parent")
            .to_path_buf(),
    ];
    if let Some(existing) = std::env::var_os("PATH") {
        paths.extend(std::env::split_paths(&existing));
    }
    let joined_path = std::env::join_paths(paths).expect("join PATH");

    let output = Command::new("bash")
        .current_dir(project)
        .arg("scripts/gall/run-checkpoints.sh")
        .env("PATH", joined_path)
        .output()
        .expect("run generated Gall evidence emitter");
    assert!(
        output.status.success(),
        "generated runner failed\nstdout={}\nstderr={}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );

    let evidence = std::fs::read_to_string(project.join("evidence/gall/ontology.ttl"))
        .expect("generated Gall evidence");
    assert!(evidence.contains("gall:runnerExitCode 0"), "{evidence}");
    assert!(evidence.contains("gall:witnessExitCode 0"), "{evidence}");
    assert!(evidence.contains("gall:falsifierExitCode 0"), "{evidence}");
    assert!(evidence.contains("gall:receiptExitCode 0"), "{evidence}");
    assert!(evidence.contains("gall:replayExitCode 0"), "{evidence}");
    assert!(
        evidence.contains("gall:independentReplay true"),
        "{evidence}"
    );
    assert!(!evidence.contains("UNVERSIONED"), "{evidence}");
}

fn scaffold() -> (TempDir, PathBuf) {
    let dir = TempDir::new().expect("tempdir");
    copy_tree(
        &packs_dir().join("gall-core-pack"),
        &dir.path().join("packs/gall-core-pack"),
    );

    let project = dir.path().join("consumer");
    std::fs::create_dir_all(project.join("templates")).expect("mkdir templates");
    std::fs::write(project.join("ontology.ttl"), planning_ontology())
        .expect("write ontology");
    write_manifest(&project, false);

    git(&project, &["init"]);
    git(&project, &["config", "user.name", "Gall Core E2E"]);
    git(
        &project,
        &["config", "user.email", "gall-core-e2e@example.invalid"],
    );
    commit_all(&project, "initial Gall program");

    (dir, project)
}

#[test]
fn gall_core_planning_evidence_crown_and_sabotage_are_real() {
    let (_dir, project) = scaffold();

    // Planning mode: contracts and DAG law are enforced, while absent evidence
    // remains UNKNOWN so the pack can manufacture its own real runner.
    sync_project(&project).expect("planning sync");
    for path in [
        "docs/GALL_CHECKPOINT_ROADMAP.md",
        "docs/GALL_CHECKPOINT_DAG.dot",
        "docs/GALL_STATUS_LEDGER.md",
        "docs/GALL_CROWN_REPORT.md",
        "scripts/gall/run-checkpoints.sh",
    ] {
        assert!(project.join(path).is_file(), "missing generated {path}");
    }
    let planning_ledger =
        std::fs::read_to_string(project.join("docs/GALL_STATUS_LEDGER.md"))
            .expect("planning ledger");
    assert!(planning_ledger.contains("UNKNOWN"), "{planning_ledger}");

    // Seal the exact generated planning state, then execute the generated
    // runner. Its replay command runs from a detached worktree at this commit.
    commit_all(&project, "seal generated Gall planning artifacts");
    run_generated_evidence(&project);

    // Activate crown mode and admit the generated evidence mini-pack.
    write_manifest(&project, true);
    activate_crown(&project);
    commit_all(&project, "activate Gall crown");
    sync_project(&project).expect("first crown sync");

    // Seal crown outputs so tracked source is clean, rerun evidence against the
    // exact crown revision/graph, then perform the final enforcing sync.
    commit_all(&project, "seal Gall crown outputs");
    run_generated_evidence(&project);
    sync_project(&project).expect("final crown sync");

    let crown_ledger = std::fs::read_to_string(project.join("docs/GALL_STATUS_LEDGER.md"))
        .expect("crown ledger");
    assert!(crown_ledger.contains("**ALIVE**"), "{crown_ledger}");
    let crown_report = std::fs::read_to_string(project.join("docs/GALL_CROWN_REPORT.md"))
        .expect("crown report");
    assert!(crown_report.contains("GALL-CORE-CROWN"), "{crown_report}");
    assert!(crown_report.contains("**ALIVE**"), "{crown_report}");

    // Negative falsifier for the framework itself: sabotage the externally
    // emitted falsifier result. The next real sync must refuse by the exact
    // crown-evidence gate name; a generated status file cannot overrule it.
    let evidence_path = project.join("evidence/gall/ontology.ttl");
    let evidence = std::fs::read_to_string(&evidence_path).expect("read evidence");
    let sabotaged = evidence.replacen("gall:falsifierExitCode 0", "gall:falsifierExitCode 1", 1);
    assert_ne!(sabotaged, evidence, "sabotage must alter real evidence");
    std::fs::write(&evidence_path, sabotaged).expect("write sabotaged evidence");

    let error = sync_project(&project).expect_err("red falsifier evidence must refuse");
    assert!(
        error.contains("100_crown_evidence_green"),
        "refusal must name the evidence gate: {error}"
    );
}
