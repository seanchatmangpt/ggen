//! Chicago-TDD end-to-end proofs of pack-writing behaviors driven through
//! the real `ggen` binary as a subprocess
//! (`chicago_tdd_tools::cli_proof::CliHarness`), not the library `sync()`
//! call. `pack_e2e.rs` already proves these behaviors' underlying logic at
//! the library level (and, for a couple of tests, via `assert_cmd` for a
//! second real-subprocess step); this file closes the remaining gap where
//! BOTH the setup and the tested behavior itself only ever ran through the
//! library call, never the actual CLI flag-parsing/exit-code/stderr path.

use std::path::{Path, PathBuf};

use chicago_tdd_tools::cli_proof::CliHarness;
use tempfile::TempDir;

fn examples_dir() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR")).join("examples")
}

/// Recursively copy `src` into `dst`.
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

/// Copy demo-pack and demo-project into a fresh TempDir; return (dir, project_root).
fn scaffold() -> (TempDir, PathBuf) {
    let dir = TempDir::new().expect("tempdir");
    copy_tree(
        &examples_dir().join("demo-pack"),
        &dir.path().join("demo-pack"),
    );
    copy_tree(
        &examples_dir().join("demo-project"),
        &dir.path().join("demo-project"),
    );
    let project = dir.path().join("demo-project");
    (dir, project)
}

/// Write a minimal pack under `root/<name>/` with one template writing
/// `to: <to_path>` and whose body is `body` verbatim.
fn write_pack(root: &Path, name: &str, class_name: &str, to_path: &str, body: &str) {
    let pack_dir = root.join(name);
    std::fs::create_dir_all(pack_dir.join("templates")).expect("mkdir pack templates");

    std::fs::write(
        pack_dir.join("pack.toml"),
        format!(
            "[pack]\nname = \"{name}\"\nversion = \"0.1.0\"\ndescription = \"test pack {name}\"\n"
        ),
    )
    .expect("write pack.toml");

    std::fs::write(
        pack_dir.join("ontology.ttl"),
        format!(
            "@prefix dom: <http://example.com/ontology#> .\ndom:{class_name} a dom:DomainClass .\n"
        ),
    )
    .expect("write ontology.ttl");

    let tmpl_name = format!("{}.rs.tmpl", class_name.to_lowercase());
    std::fs::write(
        pack_dir.join("templates").join(tmpl_name),
        format!("---\nto: {to_path}\n---\n{body}\n"),
    )
    .expect("write template");
}

/// Write `root/two-pack-project/` referencing `pack_a` and `pack_b` by
/// relative path, plus an empty ontology and templates dir (no local
/// templates of its own — only the two packs' templates run).
fn write_two_pack_project(root: &Path, pack_a: &str, pack_b: &str) -> PathBuf {
    let project = root.join("two-pack-project");
    std::fs::create_dir_all(project.join("templates")).expect("mkdir templates");
    std::fs::write(project.join("ontology.ttl"), "").expect("write ontology.ttl");
    std::fs::write(
        project.join("ggen.toml"),
        format!(
            "[project]\nname = \"two-pack-project\"\n\n\
             [ontology]\nsource = \"ontology.ttl\"\n\n\
             [packs]\npack-a = {{ path = \"../{pack_a}\" }}\npack-b = {{ path = \"../{pack_b}\" }}\n\n\
             [templates]\ndir = \"templates\"\n"
        ),
    )
    .expect("write ggen.toml");
    project
}

fn git(args: &[&str], cwd: &Path) {
    let out = std::process::Command::new("git")
        .args(args)
        .current_dir(cwd)
        .output()
        .expect("run git");
    assert!(
        out.status.success(),
        "git {args:?} failed: {}",
        String::from_utf8_lossy(&out.stderr)
    );
}

/// Turn `examples/demo-pack` into a real, tagged local git repo (no network)
/// under `dir`, returning its path.
fn make_git_pack_source(dir: &Path) -> PathBuf {
    let source = dir.join("demo-pack-git-source");
    copy_tree(&examples_dir().join("demo-pack"), &source);
    git(&["init", "--quiet"], &source);
    git(&["config", "user.email", "test@example.com"], &source);
    git(&["config", "user.name", "Test"], &source);
    git(&["add", "."], &source);
    git(&["commit", "--quiet", "-m", "v1"], &source);
    git(&["tag", "v1"], &source);
    source
}

#[test]
fn dry_run_never_writes_or_mutates_lock_over_the_cli_boundary() {
    let (_dir, project) = scaffold();

    // Dry-run on a fresh project through the real CLI: no lock, no
    // receipt, no outputs.
    CliHarness::cargo_bin("ggen")
        .args(["sync", "run", "--dry-run"])
        .current_dir(&project)
        .run()
        .expect("run dry-run")
        .assert_success();
    assert!(
        !project.join("ggen.lock").exists(),
        "dry run must not write ggen.lock"
    );
    assert!(
        !project.join("src/widget.rs").exists(),
        "dry run must not write outputs"
    );
    assert!(
        !project.join(".ggen-v2/receipt.json").exists(),
        "dry run must not emit a receipt"
    );

    // After a real sync, a dry-run leaves the lock byte-identical.
    CliHarness::cargo_bin("ggen")
        .args(["sync", "run"])
        .current_dir(&project)
        .run()
        .expect("run real sync")
        .assert_success();
    let lock = std::fs::read(project.join("ggen.lock")).expect("lock");
    CliHarness::cargo_bin("ggen")
        .args(["sync", "run", "--dry-run"])
        .current_dir(&project)
        .run()
        .expect("run dry-run 2")
        .assert_success();
    let lock2 = std::fs::read(project.join("ggen.lock")).expect("lock");
    assert_eq!(lock, lock2, "dry run must never mutate ggen.lock");
}

#[test]
fn two_packs_colliding_output_aborts_sync_over_the_cli_boundary() {
    let dir = TempDir::new().expect("tempdir");
    write_pack(dir.path(), "pack-a", "Alpha", "src/collision.rs", "AAA");
    write_pack(dir.path(), "pack-b", "Beta", "src/collision.rs", "BBB");
    let project = write_two_pack_project(dir.path(), "pack-a", "pack-b");

    CliHarness::cargo_bin("ggen")
        .args(["sync", "run"])
        .current_dir(&project)
        .run()
        .expect("run sync")
        .assert_failure()
        .assert_stderr_contains("FM-WRITE-008")
        .assert_stderr_contains("src/collision.rs");

    // The collision is detected before the write stage begins, so neither
    // pack's file lands — no partial state.
    assert!(
        !project.join("src/collision.rs").exists(),
        "collision must refuse before any write"
    );
    assert!(
        !project.join("ggen.lock").exists(),
        "ggen.lock must not be written"
    );
    assert!(
        !project.join(".ggen-v2/receipt.json").exists(),
        "receipt must not be written"
    );
}

#[test]
fn git_resolved_pack_syncs_over_the_cli_boundary_and_caches_across_runs() {
    let dir = TempDir::new().expect("tempdir");
    let source = make_git_pack_source(dir.path());

    let project = dir.path().join("consumer");
    std::fs::create_dir_all(project.join("templates")).expect("mkdir templates");
    std::fs::write(project.join("ontology.ttl"), "").expect("write ontology.ttl");
    std::fs::write(
        project.join("ggen.toml"),
        format!(
            "[project]\nname = \"consumer\"\n\n\
             [ontology]\nsource = \"ontology.ttl\"\n\n\
             [packs]\nwidget = {{ git = \"{}\", version = \"v1\" }}\n\n\
             [templates]\ndir = \"templates\"\n",
            source.display()
        ),
    )
    .expect("write ggen.toml");

    // (1) First sync clones the pack and generates from it through the
    // real CLI.
    CliHarness::cargo_bin("ggen")
        .args(["sync", "run"])
        .current_dir(&project)
        .run()
        .expect("first sync")
        .assert_success();
    let widget = project.join("src/widget.rs");
    assert!(widget.is_file(), "git pack must write src/widget.rs");
    assert!(
        std::fs::read_to_string(&widget)
            .expect("widget")
            .contains("pub struct Widget"),
        "widget must come from the cloned pack's template"
    );

    let lock = std::fs::read_to_string(project.join("ggen.lock")).expect("ggen.lock");
    assert!(
        lock.contains(&format!("source = \"git:{}@v1\"", source.display())),
        "lock must record the git source and pinned version: {lock}"
    );

    // (2) Second sync (real CLI again) reuses the pinned clone cache — no
    // re-clone, proven via a sentinel file a wipe-and-re-clone would remove.
    let cache_dir = project.join(".ggen-v2/git-packs/widget");
    std::fs::write(cache_dir.join("sentinel.txt"), "still here").expect("write sentinel");
    CliHarness::cargo_bin("ggen")
        .args(["sync", "run"])
        .current_dir(&project)
        .run()
        .expect("second sync")
        .assert_success();
    assert!(
        cache_dir.join("sentinel.txt").is_file(),
        "unchanged version must reuse the cached clone, not re-clone"
    );
}
