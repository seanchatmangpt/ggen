//! C3 — Dogfood regression: `ggen sync run`, built from this crate's own
//! current source, must regenerate `crates/ggen/src/verbs/*.rs` from
//! `schema/praxis.ttl` + the repo-root `ggen.toml` + `templates/crates/ggen/
//! *.tmpl` byte-for-byte identically to what's checked in.
//!
//! The test copies the needed inputs into a `TempDir` (the real working tree
//! is never mutated), runs the current binary there, and diffs the
//! regenerated route files against the current committed ones.
//!
//! History: this test previously drove an externally-installed OLD `ggen`
//! binary against the legacy `[[generation.rules]]`-based `ggen.toml` +
//! `*.tera` templates. Both the config schema (`GgenConfig` now denies
//! unknown fields and has no `generation` field at all) and the generation
//! mechanism itself (frontmatter `*.tmpl` files under `[templates].dir`,
//! not declarative TOML rules) have since changed, making that check
//! permanently unable to parse the current manifest. The self-consistency
//! property this test protects — the generator reproduces its own checked-in
//! output — is unaffected by which binary proves it, so it now uses the
//! crate's own current binary via `CliHarness::cargo_bin`, matching every
//! other test in this suite (see `cli_boundary.rs`).

use chicago_tdd_tools::cli_proof::CliHarness;
use std::path::{Path, PathBuf};

const ROUTE_FILES: [&str; 6] = [
    "crates/ggen/src/verbs/mod.rs",
    "crates/ggen/src/verbs/sync.rs",
    "crates/ggen/src/verbs/graph.rs",
    "crates/ggen/src/verbs/law.rs",
    "crates/ggen/src/verbs/receipt.rs",
    "crates/ggen/src/verbs/doctor.rs",
];

/// Walk up from `CARGO_MANIFEST_DIR` until a directory containing
/// `schema/praxis.ttl` is found.
fn find_praxis_root() -> Option<PathBuf> {
    let mut dir = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    loop {
        if dir.join("schema/praxis.ttl").is_file() {
            return Some(dir);
        }
        if !dir.pop() {
            return None;
        }
    }
}

fn copy_dir_recursive(src: &Path, dst: &Path) {
    std::fs::create_dir_all(dst).expect("mkdir dst");
    for entry in std::fs::read_dir(src).expect("read_dir src") {
        let entry = entry.expect("dir entry");
        let path = entry.path();
        let target = dst.join(entry.file_name());
        if path.is_dir() {
            copy_dir_recursive(&path, &target);
        } else {
            std::fs::copy(&path, &target).expect("copy file");
        }
    }
}

#[test]
fn ggen_regenerates_route_files_byte_identically() {
    let Some(root) = find_praxis_root() else {
        eprintln!("SKIP: praxis repo root (schema/praxis.ttl) not found above CARGO_MANIFEST_DIR");
        return;
    };

    // Stage a temp copy of exactly the inputs the generator needs. The real
    // working tree is NEVER touched. `stage` lives one level inside its own
    // exclusively-owned `outer` tempdir (rather than directly as a bare
    // `TempDir`) so that a SIBLING relative path (`../cargo-cicd/...`, see
    // below) resolves inside a directory this test fully owns, never a
    // shared OS temp root another concurrent test/process could also be
    // writing into.
    let outer = tempfile::TempDir::new().expect("tempdir");
    let stage = outer.path().join("praxis");
    std::fs::create_dir_all(&stage).expect("mkdir stage");
    std::fs::copy(root.join("ggen.toml"), stage.join("ggen.toml")).expect("copy ggen.toml");
    copy_dir_recursive(&root.join("schema"), &stage.join("schema"));
    copy_dir_recursive(&root.join("templates"), &stage.join("templates"));
    // Packs and law-rule inputs are part of the generator's input closure
    // since the wasm4pm facts admission (ggen.toml `[packs]` + `[law]`).
    copy_dir_recursive(&root.join("packs"), &stage.join("packs"));
    copy_dir_recursive(&root.join("ontology"), &stage.join("ontology"));
    // Real bug found and fixed forward by Lane 6: `ggen.toml`'s `[packs]`
    // table also declares `standing-pack = { path =
    // "../cargo-cicd/plugins/cargo-cicd-kit/standing-pack" }` — a sibling-
    // repo path that resolves fine for the real checkout (praxis and
    // cargo-cicd are sibling directories under /Users/sac) but was never
    // part of this test's staged input closure, so `ggen sync run
    // --dry-run` failed inside the tempdir with `[FM-PACK-001] ... does not
    // exist`. Stage that external pack at the same relative location here
    // too, so the tempdir is a genuinely self-contained closure matching
    // what `ggen.toml` actually requires.
    let standing_pack_rel = "cargo-cicd/plugins/cargo-cicd-kit/standing-pack";
    let real_standing_pack = root.join("..").join(standing_pack_rel);
    if real_standing_pack.is_dir() {
        copy_dir_recursive(&real_standing_pack, &outer.path().join(standing_pack_rel));
    } else {
        eprintln!(
            "SKIP-NOTE: sibling pack {} not found on this machine; \
             ggen sync run may fail on [FM-PACK-001] if standing-pack is required",
            real_standing_pack.display()
        );
    }
    let verbs_dst = stage.join("crates/ggen/src/verbs");
    std::fs::create_dir_all(&verbs_dst).expect("mkdir verbs");
    for entry in std::fs::read_dir(root.join("crates/ggen/src/verbs")).expect("read verbs") {
        let entry = entry.expect("verbs entry");
        let path = entry.path();
        if path.extension().is_some_and(|e| e == "rs") {
            std::fs::copy(&path, verbs_dst.join(entry.file_name())).expect("copy verb file");
        }
    }

    // Dry-run must at least parse the manifest and exit 0.
    CliHarness::cargo_bin("ggen")
        .args(["sync", "run", "--dry-run"])
        .current_dir(&stage)
        .run()
        .expect("run ggen dry-run")
        .assert_success();

    // Real sync inside the temp copy: regenerates every output there.
    CliHarness::cargo_bin("ggen")
        .args(["sync", "run"])
        .current_dir(&stage)
        .run()
        .expect("run ggen sync")
        .assert_success();

    // The five generated route files must be byte-identical to the repo's
    // current committed files.
    for rel in ROUTE_FILES {
        let regenerated = std::fs::read(stage.join(rel))
            .unwrap_or_else(|e| panic!("regenerated {rel} missing: {e}"));
        let current = std::fs::read(root.join(rel))
            .unwrap_or_else(|e| panic!("current {rel} unreadable: {e}"));
        assert_eq!(
            regenerated, current,
            "dogfood drift: `{rel}` regenerated by ggen differs from the \
             committed file — templates/crates/ggen/*.tmpl or schema/praxis.ttl \
             no longer reproduce the checked-in route files"
        );
    }
}
