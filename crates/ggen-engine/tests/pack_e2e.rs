//! Chicago-TDD end-to-end tests for packs: real filesystem, real oxigraph,
//! real Tera, real subprocess (`assert_cmd` spawns the NEW ggen binary).
//!
//! Layout: both `examples/demo-pack` and `examples/demo-project` are copied
//! into one TempDir preserving relative layout, so the project's
//! `{ path = "../demo-pack" }` reference resolves exactly as shipped.

use std::path::{Path, PathBuf};

use ggen_engine::sync::{sync, SyncOptions, SyncReceipt, RECEIPT_REL_PATH};
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

#[test]
fn pack_sync_end_to_end() {
    let (_dir, project) = scaffold();

    // (1) First sync writes the pack's widget file in place under the
    // project — no generated/ dir anywhere.
    let first = sync(
        &project,
        SyncOptions {
            dry_run: false,
            ..Default::default()
        },
    )
    .expect("first sync");
    let widget = project.join("src/widget.rs");
    assert!(
        widget.is_file(),
        "pack template must write src/widget.rs under the project"
    );
    assert!(
        !project.join("generated").exists(),
        "no generated/ dir allowed"
    );
    let widget_src = std::fs::read_to_string(&widget).expect("widget");
    assert!(
        widget_src.contains("pub struct Widget"),
        "widget body: {widget_src}"
    );
    assert!(
        widget_src.contains("pub name: String"),
        "widget fields: {widget_src}"
    );
    assert!(
        widget_src.contains("pub count: String"),
        "widget fields: {widget_src}"
    );
    // Local project template also ran, driven by the union graph.
    let instances = std::fs::read_to_string(project.join("out/instances.txt")).expect("instances");
    assert_eq!(instances, "demo\n");

    // (2) ggen.lock written with a real blake3 hash matching content_hash().
    let lock = std::fs::read_to_string(project.join("ggen.lock")).expect("ggen.lock");
    assert!(lock.contains("[packs.widget]"), "lock: {lock}");
    assert!(
        lock.contains("source = \"path:../demo-pack\""),
        "lock: {lock}"
    );
    let expected_hash = first.packs.get("widget").expect("pack hash in report");
    assert_eq!(expected_hash.len(), 64, "blake3 hex");
    assert!(
        lock.contains(&format!("content_hash = \"blake3:{expected_hash}\"")),
        "lock: {lock}"
    );

    // Report decisions bind why each file landed.
    assert_eq!(
        first.decisions.get("src/widget.rs").map(String::as_str),
        Some("written")
    );

    // (3) Receipt verify passes via the NEW binary (spawned subprocess).
    assert_cmd::Command::cargo_bin("ggen")
        .expect("ggen binary")
        .current_dir(&project)
        .args(["receipt", "verify"])
        .assert()
        .success();

    // Receipt payload carries pack hashes and decisions.
    let receipt: SyncReceipt = serde_json::from_str(
        &std::fs::read_to_string(project.join(RECEIPT_REL_PATH)).expect("receipt"),
    )
    .expect("receipt parses");
    assert_eq!(receipt.payload.packs.get("widget"), Some(expected_hash));
    assert_eq!(
        receipt
            .payload
            .decisions
            .get("src/widget.rs")
            .map(String::as_str),
        Some("written")
    );

    // (4) Second sync is a no-op: skipped-unchanged, lock byte-identical.
    let second = sync(
        &project,
        SyncOptions {
            dry_run: false,
            ..Default::default()
        },
    )
    .expect("second sync");
    assert!(
        second.written.is_empty(),
        "second sync wrote: {:?}",
        second.written
    );
    assert!(
        second
            .skipped
            .iter()
            .any(|(p, r)| p == Path::new("src/widget.rs") && r.contains("unchanged")),
        "expected skipped-unchanged widget: {:?}",
        second.skipped
    );
    assert_eq!(
        second.decisions.get("src/widget.rs").map(String::as_str),
        Some("skipped: unchanged: content identical")
    );
    let lock2 = std::fs::read_to_string(project.join("ggen.lock")).expect("ggen.lock");
    assert_eq!(
        lock2, lock,
        "ggen.lock must be byte-identical across identical runs"
    );

    // Payloads are deterministic across identical runs, and since v2
    // chaining each new receipt links onto the previous one: a third no-op
    // sync reproduces the second run's payload hash byte-for-byte while its
    // record chains from the second receipt's chain hash.
    let receipt2_raw = std::fs::read(project.join(RECEIPT_REL_PATH)).expect("receipt 2");
    sync(
        &project,
        SyncOptions {
            dry_run: false,
            ..Default::default()
        },
    )
    .expect("third sync");
    let receipt3_raw = std::fs::read(project.join(RECEIPT_REL_PATH)).expect("receipt 3");
    let receipt2: SyncReceipt = serde_json::from_slice(&receipt2_raw).expect("receipt 2 parses");
    let receipt3: SyncReceipt = serde_json::from_slice(&receipt3_raw).expect("receipt 3 parses");
    assert_eq!(
        receipt3.record.payload_hash_hex, receipt2.record.payload_hash_hex,
        "identical runs must yield identical payload hashes"
    );
    assert_eq!(
        receipt3.record.prev_chain_hash_hex, receipt2.record.chain_hash_hex,
        "third receipt must chain onto the second"
    );
    assert_eq!(receipt2.payload.outputs, receipt.payload.outputs);
    assert_eq!(receipt2.payload.packs, receipt.payload.packs);

    // (5) Corrupt the pack ontology…
    let pack_ontology = project
        .parent()
        .expect("root")
        .join("demo-pack/ontology.ttl");
    let mut ttl = std::fs::read_to_string(&pack_ontology).expect("pack ttl");
    ttl.push_str("\n<http://example.com/ontology#evil> <http://example.com/ontology#hasName> \"tampered\" .\n");
    std::fs::write(&pack_ontology, ttl).expect("corrupt pack");

    // (6) …and the next sync refuses by name with the pack-hash-mismatch code.
    let err = sync(
        &project,
        SyncOptions {
            dry_run: false,
            ..Default::default()
        },
    )
    .expect_err("must refuse");
    let msg = err.to_string();
    assert!(msg.contains("FM-PACK-008"), "refusal code: {msg}");
    assert!(msg.contains("widget"), "refusal must name the pack: {msg}");
}

#[test]
fn tampered_receipt_decision_fails_verification() {
    let (_dir, project) = scaffold();
    sync(
        &project,
        SyncOptions {
            dry_run: false,
            ..Default::default()
        },
    )
    .expect("sync");

    // (7) Tamper a decision string inside the receipt payload.
    let receipt_path = project.join(RECEIPT_REL_PATH);
    let raw = std::fs::read_to_string(&receipt_path).expect("receipt");
    let mut json: serde_json::Value = serde_json::from_str(&raw).expect("json");
    json["payload"]["decisions"]["src/widget.rs"] =
        serde_json::Value::String("skipped: nothing to see here".to_string());
    std::fs::write(
        &receipt_path,
        serde_json::to_vec_pretty(&json).expect("ser"),
    )
    .expect("write tampered");

    let output = assert_cmd::Command::cargo_bin("ggen")
        .expect("ggen binary")
        .current_dir(&project)
        .args(["receipt", "verify"])
        .assert()
        .failure();
    let stderr = String::from_utf8_lossy(&output.get_output().stderr).into_owned();
    let stdout = String::from_utf8_lossy(&output.get_output().stdout).into_owned();
    assert!(
        (stderr.clone() + &stdout).contains("payload hash mismatch"),
        "expected payload hash mismatch, got stderr={stderr} stdout={stdout}"
    );
}

#[test]
fn dry_run_never_writes_or_mutates_lock() {
    let (_dir, project) = scaffold();

    // Dry-run on a fresh project: no lock, no receipt, no outputs.
    let report = sync(
        &project,
        SyncOptions {
            dry_run: true,
            ..Default::default()
        },
    )
    .expect("dry run");
    assert!(!report.written.is_empty(), "dry run should plan writes");
    assert!(
        !project.join("ggen.lock").exists(),
        "dry run must not write ggen.lock"
    );
    assert!(
        !project.join("src/widget.rs").exists(),
        "dry run must not write outputs"
    );
    assert!(
        !project.join(RECEIPT_REL_PATH).exists(),
        "dry run must not emit a receipt"
    );

    // After a real sync, a dry-run leaves the lock byte-identical even if
    // decisions change (nothing to write).
    sync(
        &project,
        SyncOptions {
            dry_run: false,
            ..Default::default()
        },
    )
    .expect("real sync");
    let lock = std::fs::read(project.join("ggen.lock")).expect("lock");
    sync(
        &project,
        SyncOptions {
            dry_run: true,
            ..Default::default()
        },
    )
    .expect("dry run 2");
    let lock2 = std::fs::read(project.join("ggen.lock")).expect("lock");
    assert_eq!(lock, lock2, "dry run must never mutate ggen.lock");
}

#[test]
fn unreachable_git_pack_url_fails_closed_with_a_typed_error() {
    let (_dir, project) = scaffold();
    let manifest = project.join("ggen.toml");
    let toml = std::fs::read_to_string(&manifest).expect("ggen.toml");
    // A syntactically valid but unreachable/nonexistent path git URL — no
    // network access needed to prove the clone failure is caught and
    // reported as a typed FM-PACK-010, not a panic or a raw process error.
    let toml = toml.replace(
        "widget = { path = \"../demo-pack\" }",
        "widget = { git = \"/nonexistent/path/to/nowhere.git\", version = \"main\" }",
    );
    std::fs::write(&manifest, toml).expect("rewrite");

    let err = sync(
        &project,
        SyncOptions {
            dry_run: false,
            ..Default::default()
        },
    )
    .expect_err("clone must fail");
    let msg = err.to_string();
    assert!(msg.contains("FM-PACK-010"), "{msg}");
    assert!(msg.contains("git clone"), "{msg}");
}

#[test]
fn broken_packs_refuse_by_name() {
    // Missing pack dir.
    let (_dir, project) = scaffold();
    std::fs::remove_dir_all(project.parent().expect("root").join("demo-pack")).expect("rm");
    let err = sync(
        &project,
        SyncOptions {
            dry_run: false,
            ..Default::default()
        },
    )
    .expect_err("missing dir");
    assert!(err.to_string().contains("FM-PACK-001"), "{err}");
    assert!(err.to_string().contains("widget"), "{err}");

    // Missing ontology.ttl.
    let (_dir, project) = scaffold();
    std::fs::remove_file(
        project
            .parent()
            .expect("root")
            .join("demo-pack/ontology.ttl"),
    )
    .expect("rm ttl");
    let err = sync(
        &project,
        SyncOptions {
            dry_run: false,
            ..Default::default()
        },
    )
    .expect_err("missing ontology");
    assert!(err.to_string().contains("FM-PACK-004"), "{err}");

    // Unknown key in pack.toml.
    let (_dir, project) = scaffold();
    let manifest = project.parent().expect("root").join("demo-pack/pack.toml");
    let mut toml = std::fs::read_to_string(&manifest).expect("pack.toml");
    toml.push_str("sneaky = true\n");
    std::fs::write(&manifest, toml).expect("rewrite");
    let err = sync(
        &project,
        SyncOptions {
            dry_run: false,
            ..Default::default()
        },
    )
    .expect_err("unknown key");
    assert!(err.to_string().contains("FM-PACK-003"), "{err}");

    // Zero templates.
    let (_dir, project) = scaffold();
    std::fs::remove_file(
        project
            .parent()
            .expect("root")
            .join("demo-pack/templates/widget.rs.tmpl"),
    )
    .expect("rm tmpl");
    let err = sync(
        &project,
        SyncOptions {
            dry_run: false,
            ..Default::default()
        },
    )
    .expect_err("zero templates");
    assert!(err.to_string().contains("FM-PACK-005"), "{err}");
}

// ---------------------------------------------------------------------------
// Two-pack composition
// ---------------------------------------------------------------------------

/// Write a minimal, valid pack at `root/<name>`: `pack.toml`, `ontology.ttl`
/// (one `dom:<class_name> a dom:DomainClass` triple), and a single
/// `templates/<class_name lowercased>.rs.tmpl` whose frontmatter is just
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

#[test]
fn two_packs_disjoint_outputs_both_succeed() {
    let dir = TempDir::new().expect("tempdir");
    write_pack(
        dir.path(),
        "pack-a",
        "Alpha",
        "src/alpha.rs",
        "pub const ALPHA: &str = \"a\";",
    );
    write_pack(
        dir.path(),
        "pack-b",
        "Beta",
        "src/beta.rs",
        "pub const BETA: &str = \"b\";",
    );
    let project = write_two_pack_project(dir.path(), "pack-a", "pack-b");

    let report = sync(
        &project,
        SyncOptions {
            dry_run: false,
            ..Default::default()
        },
    )
    .expect("sync");

    let alpha = std::fs::read_to_string(project.join("src/alpha.rs")).expect("alpha.rs");
    assert_eq!(alpha, "pub const ALPHA: &str = \"a\";\n");
    let beta = std::fs::read_to_string(project.join("src/beta.rs")).expect("beta.rs");
    assert_eq!(beta, "pub const BETA: &str = \"b\";\n");

    assert_eq!(
        report.decisions.get("src/alpha.rs").map(String::as_str),
        Some("written")
    );
    assert_eq!(
        report.decisions.get("src/beta.rs").map(String::as_str),
        Some("written")
    );

    let lock = std::fs::read_to_string(project.join("ggen.lock")).expect("ggen.lock");
    let idx_a = lock.find("[packs.pack-a]").expect("pack-a entry in lock");
    let idx_b = lock.find("[packs.pack-b]").expect("pack-b entry in lock");
    assert!(
        idx_a < idx_b,
        "ggen.lock must list packs alphabetically: {lock}"
    );

    assert!(
        report.packs.contains_key("pack-a"),
        "report.packs: {:?}",
        report.packs
    );
    assert!(
        report.packs.contains_key("pack-b"),
        "report.packs: {:?}",
        report.packs
    );

    assert_cmd::Command::cargo_bin("ggen")
        .expect("ggen binary")
        .current_dir(&project)
        .args(["receipt", "verify"])
        .assert()
        .success();
}

#[test]
fn two_packs_colliding_output_aborts_sync_without_rollback_or_lock() {
    let dir = TempDir::new().expect("tempdir");
    write_pack(dir.path(), "pack-a", "Alpha", "src/collision.rs", "AAA");
    write_pack(dir.path(), "pack-b", "Beta", "src/collision.rs", "BBB");
    let project = write_two_pack_project(dir.path(), "pack-a", "pack-b");

    let err = sync(
        &project,
        SyncOptions {
            dry_run: false,
            ..Default::default()
        },
    )
    .expect_err("must refuse collision");
    let msg = err.to_string();
    assert!(msg.contains("FM-WRITE-008"), "{msg}");
    assert!(msg.contains("src/collision.rs"), "{msg}");

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
        !project.join(RECEIPT_REL_PATH).exists(),
        "receipt must not be written"
    );
}

/// Path to the real, production `[validation].gates`-wired cross-pack
/// contamination gate at the repo root (condition 8, 合成時に意味汚染がない) —
/// `../..` from this crate's manifest dir to the workspace root, the same
/// traversal already used by `cross_pack_matrix.rs`/`framework_packs_e2e.rs`/
/// `lint_validate_e2e.rs` in this same tests/ directory.
fn contamination_gate_source() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR")).join("../../.specify/gates/cross-pack-contamination.rq")
}

/// Write `root/<project_name>/` referencing `pack_a` and `pack_b`, wired
/// with `[law].gates` pointing at a copy of the real repo-root contamination
/// gate (copied into the project so the gate's own path resolves relative
/// to this TempDir project root, exactly as `[law].gates` paths always do).
fn write_two_pack_project_with_contamination_gate(
    root: &Path, pack_a: &str, pack_b: &str,
) -> PathBuf {
    let project = root.join("contamination-project");
    std::fs::create_dir_all(project.join("templates")).expect("mkdir templates");
    std::fs::write(project.join("ontology.ttl"), "").expect("write ontology.ttl");
    std::fs::copy(
        contamination_gate_source(),
        project.join("contamination-gate.rq"),
    )
    .expect("copy real contamination gate into project");
    std::fs::write(
        project.join("ggen.toml"),
        format!(
            "[project]\nname = \"contamination-project\"\n\n\
             [ontology]\nsource = \"ontology.ttl\"\n\n\
             [packs]\npack-a = {{ path = \"../{pack_a}\" }}\npack-b = {{ path = \"../{pack_b}\" }}\n\n\
             [templates]\ndir = \"templates\"\n\n\
             [law]\ngates = [\"contamination-gate.rq\"]\n"
        ),
    )
    .expect("write ggen.toml");
    project
}

/// Real, Chicago-TDD proof of condition 8's new closure (see
/// `packs/ggen-release-pack/ontology.ttl`'s condition-8 row): two packs
/// whose `ontology.ttl` files assert CONFLICTING `rdf:type` facts about the
/// exact same subject IRI (`dom:Widget`, TypeA vs TypeB) — disjoint output
/// paths, so the pre-existing output-path-collision gate
/// (`two_packs_colliding_output_aborts_sync_without_rollback_or_lock` above)
/// does not fire; only the new cross-pack contamination gate can refuse
/// this sync. Runs the REAL `sync()` (real filesystem, real graph union,
/// real SPARQL gate evaluation) against the REAL repo-root gate file — not
/// a standalone SPARQL string checked in isolation.
#[test]
fn cross_pack_conflicting_rdf_type_aborts_sync_citing_contamination_gate() {
    let dir = TempDir::new().expect("tempdir");
    write_pack(
        dir.path(),
        "pack-a",
        "Alpha",
        "src/alpha.rs",
        "pub const ALPHA: &str = \"a\";",
    );
    write_pack(
        dir.path(),
        "pack-b",
        "Beta",
        "src/beta.rs",
        "pub const BETA: &str = \"b\";",
    );
    // Overwrite both packs' ontology.ttl (written above by write_pack with
    // disjoint dom:Alpha/dom:Beta subjects) so they instead assert
    // conflicting rdf:type facts about the SAME subject IRI, `dom:Widget`.
    std::fs::write(
        dir.path().join("pack-a/ontology.ttl"),
        "@prefix dom: <http://example.com/ontology#> .\ndom:Widget a dom:TypeA .\n",
    )
    .expect("overwrite pack-a ontology.ttl");
    std::fs::write(
        dir.path().join("pack-b/ontology.ttl"),
        "@prefix dom: <http://example.com/ontology#> .\ndom:Widget a dom:TypeB .\n",
    )
    .expect("overwrite pack-b ontology.ttl");
    let project = write_two_pack_project_with_contamination_gate(dir.path(), "pack-a", "pack-b");

    let err = sync(
        &project,
        SyncOptions {
            dry_run: false,
            ..Default::default()
        },
    )
    .expect_err("must refuse cross-pack rdf:type contamination");
    let msg = err.to_string();
    assert!(msg.contains("FM-LAW-013"), "{msg}");
    assert!(msg.contains("contamination-gate.rq"), "{msg}");
    assert!(
        msg.contains("cross-pack term contamination"),
        "gate's own # MESSAGE text must surface in the refusal: {msg}"
    );
    assert!(
        msg.contains("http://example.com/ontology#Widget")
            || msg.contains("dom:Widget")
            || msg.contains("#Widget"),
        "refusal must cite the contaminated subject: {msg}"
    );

    // The gate runs before Extract/Render/Write, so neither pack's output
    // file lands, and no lock/receipt is written -- same no-partial-state
    // guarantee as the output-path-collision gate above.
    assert!(
        !project.join("src/alpha.rs").exists(),
        "contamination must refuse before any write"
    );
    assert!(
        !project.join("src/beta.rs").exists(),
        "contamination must refuse before any write"
    );
    assert!(
        !project.join("ggen.lock").exists(),
        "ggen.lock must not be written"
    );
    assert!(
        !project.join(RECEIPT_REL_PATH).exists(),
        "receipt must not be written"
    );
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
fn git_resolved_pack_syncs_end_to_end_and_caches_across_runs() {
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

    // (1) First sync clones the pack and generates from it exactly like a
    // local { path = "…" } pack would.
    sync(
        &project,
        SyncOptions {
            dry_run: false,
            ..Default::default()
        },
    )
    .expect("first sync clones and generates");
    let widget = project.join("src/widget.rs");
    assert!(widget.is_file(), "git pack must write src/widget.rs");
    assert!(
        std::fs::read_to_string(&widget)
            .expect("widget")
            .contains("pub struct Widget"),
        "widget must come from the cloned pack's template"
    );

    // (2) ggen.lock records the pack by its git source string.
    let lock = std::fs::read_to_string(project.join("ggen.lock")).expect("ggen.lock");
    assert!(lock.contains("[packs.widget]"), "lock: {lock}");
    assert!(
        lock.contains(&format!("source = \"git:{}@v1\"", source.display())),
        "lock must record the git source and pinned version: {lock}"
    );
    assert!(lock.contains("content_hash = \"blake3:"), "lock: {lock}");

    // (3) Second sync reuses the pinned clone cache — no re-clone, since
    // `version` hasn't changed. Prove it via a sentinel file that a
    // wipe-and-re-clone would remove.
    let cache_dir = project.join(".ggen-v2/git-packs/widget");
    std::fs::write(cache_dir.join("sentinel.txt"), "still here").expect("write sentinel");
    sync(
        &project,
        SyncOptions {
            dry_run: false,
            ..Default::default()
        },
    )
    .expect("second sync");
    assert!(
        cache_dir.join("sentinel.txt").is_file(),
        "unchanged version must reuse the cached clone, not re-clone"
    );
    let widget_after = std::fs::read_to_string(&widget).expect("widget after second sync");
    assert!(
        widget_after.contains("pub struct Widget"),
        "output unaffected by cache reuse"
    );
}

/// Write `root/extra-project/` referencing `pack_name` by relative path with
/// an in-project extra ontology (`extra/extra.ttl`) declared via
/// `extra_ontologies` — the in-manifest union that replaces per-pack
/// make-ontology.sh committed unions.
fn write_extra_ontology_project(root: &Path, pack_name: &str) -> PathBuf {
    let project = root.join("extra-project");
    std::fs::create_dir_all(project.join("templates")).expect("mkdir templates");
    std::fs::create_dir_all(project.join("extra")).expect("mkdir extra");
    std::fs::write(project.join("ontology.ttl"), "").expect("write ontology.ttl");
    std::fs::write(
        project.join("extra/extra.ttl"),
        "@prefix dom: <http://example.com/ontology#> .\ndom:ExtraClass a dom:DomainClass .\n",
    )
    .expect("write extra.ttl");
    std::fs::write(
        project.join("ggen.toml"),
        format!(
            "[project]\nname = \"extra-project\"\n\n\
             [ontology]\nsource = \"ontology.ttl\"\n\n\
             [packs]\npack-a = {{ path = \"../{pack_name}\", extra_ontologies = [\"extra/extra.ttl\"] }}\n\n\
             [templates]\ndir = \"templates\"\n"
        ),
    )
    .expect("write ggen.toml");
    project
}

#[test]
fn extra_ontology_syncs_and_its_edit_invalidates_the_lock() {
    let dir = TempDir::new().expect("tempdir");
    write_pack(
        dir.path(),
        "extra-pack-a",
        "Alpha",
        "src/alpha.rs",
        "pub struct Alpha;",
    );
    let project = write_extra_ontology_project(dir.path(), "extra-pack-a");

    // First sync succeeds: the extra ontology is readable, unioned, and
    // locked into the pack's content hash.
    sync(
        &project,
        SyncOptions {
            dry_run: false,
            ..Default::default()
        },
    )
    .expect("first sync with extra ontology");
    assert!(
        project.join("src/alpha.rs").is_file(),
        "pack output written"
    );
    let lock_1 =
        std::fs::read_to_string(project.join("ggen.lock")).expect("lockfile after first sync");
    assert!(lock_1.contains("pack-a"), "lock records the pack: {lock_1}");

    // Editing the EXTRA source (outside the pack directory) must invalidate
    // the lock exactly like an in-pack edit — this is the drift the old
    // committed-union make-ontology.sh convention could not detect.
    std::fs::write(
        project.join("extra/extra.ttl"),
        "@prefix dom: <http://example.com/ontology#> .\ndom:ExtraClassEdited a dom:DomainClass .\n",
    )
    .expect("edit extra.ttl");
    let err = sync(
        &project,
        SyncOptions {
            dry_run: false,
            ..Default::default()
        },
    )
    .expect_err("edited extra ontology must refuse against the stale lock");
    let msg = format!("{err}");
    assert!(
        msg.contains("content hash mismatch"),
        "typed FM-PACK-008 lock mismatch expected, got: {msg}"
    );
}

#[test]
fn missing_extra_ontology_refuses_with_a_typed_error() {
    let dir = TempDir::new().expect("tempdir");
    write_pack(
        dir.path(),
        "extra-pack-b",
        "Beta",
        "src/beta.rs",
        "pub struct Beta;",
    );
    let project = write_extra_ontology_project(dir.path(), "extra-pack-b");
    std::fs::remove_file(project.join("extra/extra.ttl")).expect("remove extra.ttl");

    let err = sync(
        &project,
        SyncOptions {
            dry_run: false,
            ..Default::default()
        },
    )
    .expect_err("missing extra ontology must refuse");
    let msg = format!("{err}");
    assert!(
        msg.contains("extra ontology") && msg.contains("missing"),
        "typed FM-PACK-004 missing-extra error expected, got: {msg}"
    );
}

// ---------------------------------------------------------------------------
// `lock = false` opt-out (PackRef::Path)
// ---------------------------------------------------------------------------

/// Write `root/<project_name>/` referencing `pack_a` (locked, default) and
/// `pack_b` (with `lock = {lock_b}`) by relative path.
fn write_two_pack_project_with_lock(
    root: &Path, project_name: &str, pack_a: &str, pack_b: &str, lock_b: bool,
) -> PathBuf {
    let project = root.join(project_name);
    std::fs::create_dir_all(project.join("templates")).expect("mkdir templates");
    std::fs::write(project.join("ontology.ttl"), "").expect("write ontology.ttl");
    std::fs::write(
        project.join("ggen.toml"),
        format!(
            "[project]\nname = \"{project_name}\"\n\n\
             [ontology]\nsource = \"ontology.ttl\"\n\n\
             [packs]\npack-a = {{ path = \"../{pack_a}\" }}\n\
             pack-b = {{ path = \"../{pack_b}\", lock = {lock_b} }}\n\n\
             [templates]\ndir = \"templates\"\n"
        ),
    )
    .expect("write ggen.toml");
    project
}

/// Write `root/<project_name>/` referencing a single pack by relative path
/// with no `lock` key declared — exercises the `#[serde(default =
/// "default_true")]` path, not an explicit `lock = true`.
fn write_single_pack_project(root: &Path, project_name: &str, pack_name: &str) -> PathBuf {
    let project = root.join(project_name);
    std::fs::create_dir_all(project.join("templates")).expect("mkdir templates");
    std::fs::write(project.join("ontology.ttl"), "").expect("write ontology.ttl");
    std::fs::write(
        project.join("ggen.toml"),
        format!(
            "[project]\nname = \"{project_name}\"\n\n\
             [ontology]\nsource = \"ontology.ttl\"\n\n\
             [packs]\npack-a = {{ path = \"../{pack_name}\" }}\n\n\
             [templates]\ndir = \"templates\"\n"
        ),
    )
    .expect("write ggen.toml");
    project
}

/// A pack declared `lock = false` is never checked against `ggen.lock` and
/// never written to it — a second sync after editing its `ontology.ttl`
/// must succeed instead of tripping `FM-PACK-008`. The companion locked
/// pack (`pack-a`, default `lock = true`) proves the lockfile genuinely
/// gets written in this run (not vacuously absent), so `pack-b`'s absence
/// from it is a real assertion, not a no-op.
#[test]
fn lock_false_pack_is_never_locked_and_its_edit_does_not_refuse() {
    let dir = TempDir::new().expect("tempdir");
    write_pack(
        dir.path(),
        "lockopt-pack-a",
        "Alpha",
        "src/alpha.rs",
        "pub struct Alpha;",
    );
    write_pack(
        dir.path(),
        "lockopt-pack-b",
        "Beta",
        "src/beta.rs",
        "pub struct Beta;",
    );
    let project = write_two_pack_project_with_lock(
        dir.path(),
        "lock-false-project",
        "lockopt-pack-a",
        "lockopt-pack-b",
        false,
    );

    sync(
        &project,
        SyncOptions {
            dry_run: false,
            ..Default::default()
        },
    )
    .expect("first sync");
    assert!(
        project.join("src/alpha.rs").is_file(),
        "pack-a output written"
    );
    assert!(
        project.join("src/beta.rs").is_file(),
        "pack-b output written"
    );

    let lock_1 = std::fs::read_to_string(project.join("ggen.lock"))
        .expect("ggen.lock must exist: pack-a is locked, so lock_entries is non-empty");
    assert!(
        lock_1.contains("[packs.pack-a]"),
        "default lock=true pack must still be locked: {lock_1}"
    );
    assert!(
        !lock_1.contains("pack-b"),
        "lock=false pack must never appear in ggen.lock: {lock_1}"
    );

    // Edit the unlocked pack's ontology.ttl (the exact mutation that trips
    // FM-PACK-008 for a locked pack, per
    // `pack_sync_end_to_end`'s tampering step).
    std::fs::write(
        dir.path().join("lockopt-pack-b/ontology.ttl"),
        "@prefix dom: <http://example.com/ontology#> .\ndom:BetaEdited a dom:DomainClass .\n",
    )
    .expect("edit lock=false pack ontology");

    sync(
        &project,
        SyncOptions {
            dry_run: false,
            ..Default::default()
        },
    )
    .expect("second sync must NOT refuse: pack-b opted out of lock checking");

    let lock_2 = std::fs::read_to_string(project.join("ggen.lock"))
        .expect("ggen.lock must still exist after second sync");
    assert!(
        !lock_2.contains("pack-b"),
        "lock=false pack must still never appear in ggen.lock after re-sync: {lock_2}"
    );
    assert!(
        lock_2.contains("[packs.pack-a]"),
        "locked pack must remain locked after re-sync: {lock_2}"
    );
}

/// Regression guard: a pack with no `lock` key (default `lock = true`) keeps
/// exactly the pre-existing behavior — locked into `ggen.lock`, and a
/// tampering edit still refuses with the typed `FM-PACK-008` content-hash
/// mismatch, asserting the same error-message shape as
/// `extra_ontology_syncs_and_its_edit_invalidates_the_lock`.
#[test]
fn default_locked_pack_still_refuses_on_tampering_regression_guard() {
    let dir = TempDir::new().expect("tempdir");
    write_pack(
        dir.path(),
        "lockopt-pack-c",
        "Gamma",
        "src/gamma.rs",
        "pub struct Gamma;",
    );
    let project = write_single_pack_project(dir.path(), "lock-default-project", "lockopt-pack-c");

    sync(
        &project,
        SyncOptions {
            dry_run: false,
            ..Default::default()
        },
    )
    .expect("first sync with default lock=true pack");
    let lock_1 = std::fs::read_to_string(project.join("ggen.lock")).expect("ggen.lock");
    assert!(
        lock_1.contains("[packs.pack-a]"),
        "default lock=true pack must be locked: {lock_1}"
    );

    std::fs::write(
        dir.path().join("lockopt-pack-c/ontology.ttl"),
        "@prefix dom: <http://example.com/ontology#> .\ndom:GammaTampered a dom:DomainClass .\n",
    )
    .expect("tamper default-locked pack ontology");

    let err = sync(
        &project,
        SyncOptions {
            dry_run: false,
            ..Default::default()
        },
    )
    .expect_err("default-locked pack must still refuse on tampering");
    let msg = format!("{err}");
    assert!(
        msg.contains("content hash mismatch"),
        "typed FM-PACK-008 lock mismatch expected, got: {msg}"
    );
    assert!(msg.contains("FM-PACK-008"), "refusal code: {msg}");
}
