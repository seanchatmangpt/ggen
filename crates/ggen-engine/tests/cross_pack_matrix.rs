//! Combinatorial cross-pack proofs over the framework packs under `packs/`:
//! an all-packs mega-project (real binary), the full pairwise C(n,2) matrix
//! (direct library `sync()` calls for speed), ontology-union sanity, and a
//! post-lock corruption sweep. Chicago TDD: real fs (TempDir), real
//! oxigraph, real subprocess via `CliHarness`. No mocks.

#![allow(clippy::expect_used)]

use std::path::{Path, PathBuf};

use chicago_tdd_tools::cli_proof::CliHarness;
use ggen_engine::sync::{sync, SyncOptions, SyncReceipt, RECEIPT_REL_PATH};
use tempfile::TempDir;

/// Every framework pack expected under `packs/`, alphabetical, with its
/// distinctive output file (only that pack's ontology/templates can
/// produce it).
const PACKS: &[(&str, &str)] = &[
    (
        "chicago-tdd-tools-pack",
        "tests/chicago_tdd_tools_boundary.rs",
    ),
    ("clap-noun-verb-pack", "src/clap_noun_verb_routes.rs"),
    ("lsp-max-pack", "rules/lsp_max_lspmax_unwrap_001.toml"),
    ("praxis-core-pack", "src/praxis_core_refusal_table.rs"),
    ("star-toml-pack", "src/star_toml_config.rs"),
    ("wasm4pm-algorithms-pack", "src/w4pm_algorithms_catalog.rs"),
    ("wasm4pm-cognition-pack", "src/w4pm_cognition_catalog.rs"),
    ("wasm4pm-compat-pack", "src/wasm4pm_compat_events.rs"),
];

/// Repository `packs/` directory (relative to this crate's manifest).
fn packs_dir() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR")).join("../../packs")
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

/// Copy the named packs plus a minimal consumer project into a fresh
/// TempDir; return `(tempdir, project_root)`. `pack_order` controls the
/// textual order of the `[packs.*]` declarations in `ggen.toml` (config
/// parsing uses a `BTreeMap`, so semantics must be order-independent).
fn scaffold_multi_pack_project(pack_order: &[&str]) -> (TempDir, PathBuf) {
    let dir = TempDir::new().expect("tempdir");
    for name in pack_order {
        copy_tree(&packs_dir().join(name), &dir.path().join(name));
    }
    let project = dir.path().join("consumer");
    std::fs::create_dir_all(project.join("templates")).expect("mkdir templates");
    std::fs::write(project.join("ontology.ttl"), "").expect("write ontology.ttl");
    let mut toml =
        String::from("[project]\nname = \"consumer\"\n\n[ontology]\nsource = \"ontology.ttl\"\n\n");
    for name in pack_order {
        toml.push_str(&format!("[packs.{name}]\npath = \"../{name}\"\n\n"));
    }
    toml.push_str("[templates]\ndir = \"templates\"\n");
    std::fs::write(project.join("ggen.toml"), toml).expect("write ggen.toml");
    (dir, project)
}

/// Read and parse the sync receipt of a project.
fn read_receipt(project: &Path) -> SyncReceipt {
    let raw = std::fs::read_to_string(project.join(RECEIPT_REL_PATH)).expect("receipt.json");
    serde_json::from_str(&raw).expect("receipt parses")
}

/// (0) Every expected framework pack must actually exist on disk with the
/// full pack shape — this is the ground truth the matrix builds on.
#[test]
fn all_eight_framework_packs_exist_on_disk() {
    for (name, _) in PACKS {
        let root = packs_dir().join(name);
        assert!(root.is_dir(), "pack missing: {}", root.display());
        assert!(
            root.join("pack.toml").is_file(),
            "{name}: pack.toml missing"
        );
        assert!(
            root.join("ontology.ttl").is_file(),
            "{name}: ontology.ttl missing"
        );
        let has_tmpl = std::fs::read_dir(root.join("templates"))
            .expect("templates dir")
            .filter_map(|e| e.ok())
            .any(|e| e.path().extension().is_some_and(|x| x == "tmpl"));
        assert!(has_tmpl, "{name}: zero templates");
    }
}

/// (1) ALL-PACKS MEGA-PROJECT via the real binary: one consumer referencing
/// every pack simultaneously. Sync exits 0, every pack's distinctive output
/// lands, ggen.lock lists all packs alphabetically, the receipt payload
/// packs map covers all of them, `receipt verify` and `doctor run` both
/// exit 0, and a second sync is payload-idempotent.
#[test]
fn mega_project_all_packs_sync() {
    let names: Vec<&str> = PACKS.iter().map(|(n, _)| *n).collect();
    let (_dir, project) = scaffold_multi_pack_project(&names);

    CliHarness::cargo_bin("ggen")
        .args(["sync", "run"])
        .current_dir(&project)
        .run()
        .expect("run sync")
        .assert_success();

    // Every pack's distinctive output is present.
    for (name, output) in PACKS {
        assert!(
            project.join(output).is_file(),
            "mega-project missing {output} from pack {name}"
        );
    }

    // ggen.lock lists all packs, alphabetically, each with a blake3 hash.
    let lock = std::fs::read_to_string(project.join("ggen.lock")).expect("ggen.lock");
    let mut last_pos = 0usize;
    for (name, _) in PACKS {
        let header = format!("[packs.{name}]");
        let pos = lock
            .find(&header)
            .unwrap_or_else(|| panic!("lock missing {header}: {lock}"));
        assert!(
            pos >= last_pos,
            "lock entries out of alphabetical order at {header}: {lock}"
        );
        last_pos = pos;
    }
    assert_eq!(
        lock.matches("content_hash = \"blake3:").count(),
        PACKS.len(),
        "one blake3 hash per pack expected: {lock}"
    );

    // Receipt payload packs map covers every pack.
    let receipt = read_receipt(&project);
    for (name, _) in PACKS {
        assert!(
            receipt.payload.packs.contains_key(*name),
            "receipt payload packs map missing {name}: {:?}",
            receipt.payload.packs
        );
    }
    assert_eq!(receipt.payload.packs.len(), PACKS.len());

    // receipt verify and doctor run both exit 0 (real binary).
    CliHarness::cargo_bin("ggen")
        .args(["receipt", "verify"])
        .current_dir(&project)
        .run()
        .expect("receipt verify")
        .assert_success();
    CliHarness::cargo_bin("ggen")
        .args(["doctor", "run"])
        .current_dir(&project)
        .run()
        .expect("doctor run")
        .assert_success();

    // Second sync: exit 0 and idempotent state. The chain record links onto
    // the first receipt by design, and `decisions` legitimately flips from
    // "written" to "skipped: unchanged" — but the deterministic state the
    // receipt binds (graph hash, output hashes, pack hashes) must be
    // byte-identical, and every second-run decision must be a skip (no file
    // was rewritten).
    let lock_1 = lock;
    CliHarness::cargo_bin("ggen")
        .args(["sync", "run"])
        .current_dir(&project)
        .run()
        .expect("second sync")
        .assert_success();
    let receipt_2 = read_receipt(&project);
    assert_eq!(receipt.payload.graph_hash, receipt_2.payload.graph_hash);
    assert_eq!(
        receipt.payload.outputs, receipt_2.payload.outputs,
        "second sync must leave every output hash unchanged"
    );
    assert_eq!(receipt.payload.packs, receipt_2.payload.packs);
    for (path, decision) in &receipt_2.payload.decisions {
        assert!(
            decision.starts_with("skipped:"),
            "second sync must not rewrite anything, but {path} was `{decision}`"
        );
    }
    assert_eq!(
        receipt_2.record.prev_chain_hash_hex, receipt.record.chain_hash_hex,
        "second receipt must chain onto the first"
    );
    let lock_2 = std::fs::read_to_string(project.join("ggen.lock")).expect("ggen.lock 2");
    assert_eq!(
        lock_1, lock_2,
        "ggen.lock must be byte-identical across identical runs"
    );
}

/// (2) PAIRWISE SUBSETS: all C(8,2) = 28 pairs in one test, each in a fresh
/// TempDir project with exactly those two packs, driven through the library
/// `sync()` (fast — no subprocess). Exit Ok, both distinctive outputs
/// present, lock has exactly those two pack names. Catches pair-specific
/// ontology prefix or output-path collisions invisible to single-pack tests.
#[test]
fn pairwise_pack_matrix_syncs() {
    let mut failures: Vec<String> = Vec::new();
    for i in 0..PACKS.len() {
        for j in (i + 1)..PACKS.len() {
            let (name_a, out_a) = PACKS[i];
            let (name_b, out_b) = PACKS[j];
            let pair = format!("{name_a}+{name_b}");
            let (_dir, project) = scaffold_multi_pack_project(&[name_a, name_b]);

            let report = match sync(&project, SyncOptions::default()) {
                Ok(r) => r,
                Err(e) => {
                    failures.push(format!("{pair}: sync failed: {e}"));
                    continue;
                }
            };

            for (name, out) in [(name_a, out_a), (name_b, out_b)] {
                if !project.join(out).is_file() {
                    failures.push(format!("{pair}: missing {out} from {name}"));
                }
            }

            // Lock has exactly these two pack names.
            let lock = std::fs::read_to_string(project.join("ggen.lock")).expect("ggen.lock");
            let locked: Vec<&str> = lock
                .lines()
                .filter_map(|l| l.strip_prefix("[packs.").and_then(|l| l.strip_suffix(']')))
                .collect();
            let mut expected = vec![name_a, name_b];
            expected.sort_unstable();
            if locked != expected {
                failures.push(format!(
                    "{pair}: lock has {locked:?}, expected {expected:?}"
                ));
            }
            if report.packs.len() != 2 {
                failures.push(format!("{pair}: report packs map {:?}", report.packs));
            }
        }
    }
    assert!(
        failures.is_empty(),
        "pairwise matrix failures:\n{}",
        failures.join("\n")
    );
}

/// (3) ONTOLOGY UNION SANITY: the mega-project graph hash differs from
/// every single-pack project's graph hash (the union actually merged all
/// ontologies), and writing `[packs]` in reversed declaration order yields
/// a byte-identical receipt payload (BTreeMap canonicalizes).
#[test]
fn ontology_union_and_declaration_order_are_canonical() {
    let names: Vec<&str> = PACKS.iter().map(|(n, _)| *n).collect();

    // Single-pack graph hashes via the library.
    let mut single_hashes = Vec::new();
    for name in &names {
        let (_dir, project) = scaffold_multi_pack_project(&[name]);
        let report = sync(&project, SyncOptions::default()).expect("single-pack sync");
        single_hashes.push((*name, report.graph_hash_hex));
    }

    // Mega project, alphabetical declaration order.
    let (_dir_a, project_a) = scaffold_multi_pack_project(&names);
    let report_a = sync(&project_a, SyncOptions::default()).expect("mega sync (alpha)");
    for (name, hash) in &single_hashes {
        assert_ne!(
            &report_a.graph_hash_hex, hash,
            "mega graph hash must differ from single-pack hash of {name} — union merged nothing?"
        );
    }

    // Mega project again, reversed [packs] declaration order in ggen.toml.
    let reversed: Vec<&str> = names.iter().rev().copied().collect();
    let (_dir_b, project_b) = scaffold_multi_pack_project(&reversed);
    let report_b = sync(&project_b, SyncOptions::default()).expect("mega sync (reversed)");
    assert_eq!(
        report_a.graph_hash_hex, report_b.graph_hash_hex,
        "declaration order must not change the graph hash"
    );

    let payload_a = serde_json::to_vec(&read_receipt(&project_a).payload).expect("payload a");
    let payload_b = serde_json::to_vec(&read_receipt(&project_b).payload).expect("payload b");
    assert_eq!(
        payload_a, payload_b,
        "reversed [packs] declaration order must produce an identical receipt payload"
    );
}

/// (4) CORRUPTION SWEEP: corrupt one representative pack's ontology after
/// the mega-project is locked. The next sync must refuse with FM-PACK-008
/// naming that pack and only that pack; `doctor run` must also exit nonzero
/// naming lockfile_drift.
#[test]
fn corrupting_one_pack_post_lock_fails_closed_naming_only_that_pack() {
    let names: Vec<&str> = PACKS.iter().map(|(n, _)| *n).collect();
    let (dir, project) = scaffold_multi_pack_project(&names);

    CliHarness::cargo_bin("ggen")
        .args(["sync", "run"])
        .current_dir(&project)
        .run()
        .expect("first sync")
        .assert_success();

    // Corrupt praxis-core-pack's ontology post-lock (append a real triple —
    // still valid Turtle, but the content hash changes).
    let corrupted = "praxis-core-pack";
    let ontology = dir.path().join(corrupted).join("ontology.ttl");
    let mut ttl = std::fs::read_to_string(&ontology).expect("pack ontology");
    ttl.push_str("\n<http://example.org/sabotage> a rdfs:Resource .\n");
    std::fs::write(&ontology, ttl).expect("corrupt ontology");

    // Next sync refuses with FM-PACK-008 naming the corrupted pack only.
    let out = CliHarness::cargo_bin("ggen")
        .args(["sync", "run"])
        .current_dir(&project)
        .run()
        .expect("post-corruption sync");
    out.assert_failure();
    out.assert_stderr_contains("FM-PACK-008");
    out.assert_stderr_contains(corrupted);
    for (name, _) in PACKS {
        if *name != corrupted {
            assert!(
                !out.stderr.contains(name),
                "FM-PACK-008 must name only `{corrupted}`, but stderr names `{name}`: {}",
                out.stderr
            );
        }
    }

    // No distinctive output was rewritten by the refused sync (fail closed
    // means fail before writing): receipt verify still passes.
    CliHarness::cargo_bin("ggen")
        .args(["receipt", "verify"])
        .current_dir(&project)
        .run()
        .expect("receipt verify")
        .assert_success();

    // doctor run also exits nonzero naming lockfile_drift.
    let doctor = CliHarness::cargo_bin("ggen")
        .args(["doctor", "run"])
        .current_dir(&project)
        .run()
        .expect("doctor run");
    doctor.assert_failure();
    assert!(
        doctor.stderr.contains("lockfile_drift") || doctor.stdout.contains("lockfile_drift"),
        "doctor must name lockfile_drift; stdout: {} stderr: {}",
        doctor.stdout,
        doctor.stderr
    );
}

/// (5) FULL WASM4PM COVERAGE: `wasm4pm-algorithms-pack` and
/// `wasm4pm-cognition-pack` together in one consumer project must
/// precipitate a typed catalog entry per ontology individual — all 60
/// `pi:ProcessIntelligenceAlgorithm` instances and all 55
/// `compat:CognitionBreed` instances, not a representative subset. `ggen
/// graph validate` must exit 0 (no unbound vars from the larger
/// generation), and a second sync must be byte-identical (idempotent).
#[test]
fn wasm4pm_algorithms_and_cognition_packs_full_coverage() {
    let (_dir, project) =
        scaffold_multi_pack_project(&["wasm4pm-algorithms-pack", "wasm4pm-cognition-pack"]);

    CliHarness::cargo_bin("ggen")
        .args(["sync", "run"])
        .current_dir(&project)
        .run()
        .expect("run sync")
        .assert_success();

    CliHarness::cargo_bin("ggen")
        .args(["graph", "validate"])
        .current_dir(&project)
        .run()
        .expect("run graph validate")
        .assert_success();

    let algorithms_catalog =
        std::fs::read_to_string(project.join("src/w4pm_algorithms_catalog.rs"))
            .expect("algorithms catalog");
    let algorithm_entries = algorithms_catalog.matches("algorithm_id: \"").count();
    assert_eq!(
        algorithm_entries, 60,
        "expected 60 algorithm entries in generated catalog, found {algorithm_entries}"
    );

    let cognition_catalog = std::fs::read_to_string(project.join("src/w4pm_cognition_catalog.rs"))
        .expect("cognition catalog");
    let breed_entries = cognition_catalog
        .matches("=> Some(CognitionBreedId::")
        .count();
    assert_eq!(
        breed_entries, 55,
        "expected 55 breed entries in generated cognition catalog, found {breed_entries}"
    );

    let dispatch = std::fs::read_to_string(project.join("src/w4pm_cognition_dispatch.rs"))
        .expect("cognition dispatch");
    let dispatch_entries = dispatch.matches("dispatch_cognition_run(\"").count();
    assert_eq!(
        dispatch_entries, 55,
        "expected 55 dispatch stubs, found {dispatch_entries}"
    );

    // Second sync: byte-identical outputs (idempotent).
    let algorithms_1 = algorithms_catalog;
    let cognition_1 = cognition_catalog;
    let dispatch_1 = dispatch;
    let lock_1 = std::fs::read_to_string(project.join("ggen.lock")).expect("ggen.lock");

    CliHarness::cargo_bin("ggen")
        .args(["sync", "run"])
        .current_dir(&project)
        .run()
        .expect("second sync")
        .assert_success();

    let algorithms_2 = std::fs::read_to_string(project.join("src/w4pm_algorithms_catalog.rs"))
        .expect("algorithms catalog 2");
    let cognition_2 = std::fs::read_to_string(project.join("src/w4pm_cognition_catalog.rs"))
        .expect("cognition catalog 2");
    let dispatch_2 = std::fs::read_to_string(project.join("src/w4pm_cognition_dispatch.rs"))
        .expect("cognition dispatch 2");
    let lock_2 = std::fs::read_to_string(project.join("ggen.lock")).expect("ggen.lock 2");

    assert_eq!(
        algorithms_1, algorithms_2,
        "algorithms catalog must be byte-identical"
    );
    assert_eq!(
        cognition_1, cognition_2,
        "cognition catalog must be byte-identical"
    );
    assert_eq!(
        dispatch_1, dispatch_2,
        "cognition dispatch must be byte-identical"
    );
    assert_eq!(
        lock_1, lock_2,
        "ggen.lock must be byte-identical across identical runs"
    );
}
