//! Chicago-TDD end-to-end proof of the ggen-verify-pack evidence bootstrap
//! loop: a scratch consumer composing `ggen-verify-pack` + `star-toml-pack`
//! goes bootstrap-emitter -> evidence written -> sync green (which GENERATES
//! `scripts/verify-evidence.sh`) -> generated emitter re-run -> resync stays
//! green -> evidence sabotage -> FM-PACK-013 refusal.
//!
//! Real collaborators throughout: real filesystem (`TempDir`), real graph
//! engine (in-process `sync`), real `bash` subprocesses running the actual
//! bootstrap and the actual generated script, real exit codes. The heavy
//! cargo checks declared as `ver:checkCommand` ontology facts are satisfied
//! by a real stub `cargo` binary on `PATH` (the house subprocess-test
//! pattern) -- the stub genuinely runs and its genuine exit code is what
//! lands in the evidence graph; nothing fakes evidence inside the test.

use std::path::{Path, PathBuf};
use std::process::Command;

use ggen_engine::sync::{sync, SyncOptions};
use tempfile::TempDir;

fn packs_dir() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR")).join("../../packs")
}

fn copy_tree(src: &Path, dst: &Path) {
    std::fs::create_dir_all(dst).expect("mkdir");
    for entry in std::fs::read_dir(src).expect("read_dir") {
        let entry = entry.expect("entry");
        let from = entry.path();
        let to = dst.join(entry.file_name());
        if from.is_dir() {
            copy_tree(&from, &to);
        } else {
            std::fs::copy(&from, &to).expect("copy");
        }
    }
}

#[cfg(unix)]
fn make_executable(path: &Path) {
    use std::os::unix::fs::PermissionsExt;
    let mut perms = std::fs::metadata(path).expect("metadata").permissions();
    perms.set_mode(0o755);
    std::fs::set_permissions(path, perms).expect("chmod");
}

/// Scratch consumer composing ggen-verify-pack + star-toml-pack, plus a
/// stub-binary dir whose `cargo` really runs and really exits with the code
/// written into it.
fn scaffold() -> (TempDir, PathBuf, PathBuf) {
    let dir = TempDir::new().expect("tempdir");
    copy_tree(
        &packs_dir().join("ggen-verify-pack"),
        &dir.path().join("packs/ggen-verify-pack"),
    );
    copy_tree(
        &packs_dir().join("star-toml-pack"),
        &dir.path().join("packs/star-toml-pack"),
    );

    let project = dir.path().join("consumer");
    std::fs::create_dir_all(project.join("templates")).expect("mkdir templates");
    std::fs::create_dir_all(project.join("scripts/checks")).expect("mkdir checks");
    std::fs::write(project.join("ontology.ttl"), "").expect("write ontology.ttl");
    std::fs::write(
        project.join("ggen.toml"),
        "[project]\nname = \"verify-loop-consumer\"\n\n\
         [ontology]\nsource = \"ontology.ttl\"\n\n\
         [packs]\n\
         ggen-verify-pack = { path = \"../packs/ggen-verify-pack\" }\n\
         star-toml-pack = { path = \"../packs/star-toml-pack\" }\n\
         verify-evidence = { path = \"evidence\", lock = false }\n\n\
         [templates]\ndir = \"templates\"\n\n\
         [law]\nreflexive = true\n",
    )
    .expect("write ggen.toml");

    // The one consumer-specific required check: a REAL command whose REAL
    // exit code is recorded (task-blessed trivial real check).
    let hook = project.join("scripts/checks/byte-identity.sh");
    std::fs::write(&hook, "#!/bin/sh\ntest -f ggen.toml\n").expect("write hook");
    make_executable(&hook);

    // Real stub cargo on PATH: it actually executes and actually exits 0.
    let stub_bin = dir.path().join("stub-bin");
    std::fs::create_dir_all(&stub_bin).expect("mkdir stub-bin");
    write_stub_cargo(&stub_bin, 0);

    (dir, project, stub_bin)
}

fn write_stub_cargo(stub_bin: &Path, exit_code: i32) {
    let cargo = stub_bin.join("cargo");
    std::fs::write(
        &cargo,
        format!("#!/bin/sh\necho stub-cargo \"$@\"\nexit {exit_code}\n"),
    )
    .expect("write stub cargo");
    make_executable(&cargo);
}

/// Run a shell script from the consumer root with the stub dir prepended to
/// PATH; returns (exit_code, combined stdout+stderr).
fn run_script(project: &Path, stub_bin: &Path, script: &Path) -> (i32, String) {
    let path = format!(
        "{}:{}",
        stub_bin.display(),
        std::env::var("PATH").expect("PATH")
    );
    let out = Command::new("bash")
        .arg(script)
        .current_dir(project)
        .env("PATH", path)
        .output()
        .expect("run script");
    let text = format!(
        "{}{}",
        String::from_utf8_lossy(&out.stdout),
        String::from_utf8_lossy(&out.stderr)
    );
    (out.status.code().unwrap_or(-1), text)
}

fn read(project: &Path, rel: &str) -> String {
    std::fs::read_to_string(project.join(rel)).unwrap_or_else(|e| panic!("read {rel}: {e}"))
}

fn run_sync(project: &Path) -> Result<ggen_engine::sync::SyncReport, String> {
    sync(
        project,
        SyncOptions {
            dry_run: false,
            ..Default::default()
        },
    )
    .map_err(|e| e.to_string())
}

#[test]
fn verify_pack_evidence_bootstrap_loop_green_then_sabotage_refusal() {
    let (dir, project, stub_bin) = scaffold();

    // ── Phase 1: bootstrap — committed copy run from the pack source ─────
    let bootstrap = dir
        .path()
        .join("packs/ggen-verify-pack/bootstrap/verify-evidence-bootstrap.sh");
    let (code, out) = run_script(&project, &stub_bin, &bootstrap);
    assert_eq!(
        code, 0,
        "bootstrap must exit 0 (records, never judges): {out}"
    );
    assert!(
        project.join("evidence/pack.toml").is_file(),
        "bootstrap scaffolds the evidence mini-pack"
    );
    let evidence = read(&project, "evidence/ontology.ttl");
    for name in [
        "build",
        "test-workspace",
        "clippy-reference-gate",
        "byte-identity",
    ] {
        assert!(
            evidence.contains(&format!("ver:name \"{name}\"")),
            "bootstrap evidence must cover required check `{name}`:\n{evidence}"
        );
    }
    assert!(
        evidence.contains("ver:exitCode 0"),
        "stub cargo + real hook are green: {evidence}"
    );
    assert!(
        evidence.contains("ver:verifiedGraphHash \"unsynced\""),
        "never-synced tree binds to `unsynced`: {evidence}"
    );

    // ── Phase 2: first sync passes the gates and GENERATES the emitter ──
    let first = run_sync(&project).expect("first sync after bootstrap must be green");
    assert!(!first.written.is_empty());
    let generated = project.join("scripts/verify-evidence.sh");
    assert!(
        generated.is_file(),
        "sync must generate scripts/verify-evidence.sh"
    );
    let script = read(&project, "scripts/verify-evidence.sh");
    // The check list is projected from ontology facts, not hand-maintained.
    assert!(script.contains("run_check 'build' 'cargo build --workspace'"));
    assert!(script.contains("run_check 'byte-identity' 'bash scripts/checks/byte-identity.sh'"));
    assert!(project.join("VERIFICATION.md").is_file());
    assert!(project.join("receipts/EVIDENCE_NOTE.md").is_file());

    // ── Phase 3: steady state — generated emitter rebinds, resync green ──
    let (code, out) = run_script(&project, &stub_bin, &generated);
    assert_eq!(code, 0, "generated emitter must exit 0: {out}");
    let evidence = read(&project, "evidence/ontology.ttl");
    assert!(
        !evidence.contains("\"unsynced\""),
        "after a completed sync the emitter must bind the real graph hash:\n{evidence}"
    );
    assert!(
        evidence.contains(&format!(
            "ver:verifiedGraphHash \"{}\"",
            first.graph_hash_hex
        )),
        "evidence must bind the exact graph hash of the last sync ({}):\n{evidence}",
        first.graph_hash_hex
    );
    run_sync(&project).expect("resync after generated-emitter re-run must stay green");

    // ── Phase 4: sabotage A — evidence facts gone => FM-PACK-013 (gate 010)
    std::fs::write(
        project.join("evidence/ontology.ttl"),
        "@prefix ver: <http://seanchatmangpt.github.io/packs/ggen-verify#> .\n",
    )
    .expect("truncate evidence");
    let err = run_sync(&project).expect_err("sync with no evidence facts must refuse");
    assert!(
        err.contains("010_evidence_present"),
        "refusal must come from the presence gate: {err}"
    );
    assert!(
        err.contains("required verification evidence is missing"),
        "refusal must carry the gate's MESSAGE: {err}"
    );

    // ── Phase 5: sabotage B — red check (real failing subprocess) => gate 020
    write_stub_cargo(&stub_bin, 1);
    let (code, out) = run_script(&project, &stub_bin, &generated);
    assert_eq!(
        code, 0,
        "emitter records red honestly and still exits 0: {out}"
    );
    let evidence = read(&project, "evidence/ontology.ttl");
    assert!(
        evidence.contains("ver:exitCode 1"),
        "red exit recorded: {evidence}"
    );
    let err = run_sync(&project).expect_err("sync with red evidence must refuse");
    assert!(
        err.contains("020_evidence_green"),
        "refusal must come from the green gate: {err}"
    );
    assert!(
        err.contains("verification evidence is red"),
        "refusal must carry the gate's MESSAGE: {err}"
    );

    // ── Phase 6: sabotage C — evidence file deleted outright => refusal ──
    std::fs::remove_file(project.join("evidence/ontology.ttl")).expect("rm evidence");
    let err = run_sync(&project)
        .expect_err("sync with the evidence file deleted must refuse, never skip");
    assert!(
        err.contains("verify-evidence") || err.contains("ontology.ttl"),
        "refusal must name the missing evidence source: {err}"
    );
}
