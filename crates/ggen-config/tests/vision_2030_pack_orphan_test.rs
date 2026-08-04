//! Orphan-pack governance test (red-team finding `orphan-vision-2030-phase-change-pack`,
//! category legacy-path-contamination / pack-orphans): `packs/vision-2030-phase-change-pack/`
//! ships a JSON schema + capability catalog + Tera template but has ZERO consumers anywhere
//! in the workspace -- no `ggen.toml` generation rule references it, and no Rust source under
//! `crates/` references it either (confirmed 2026-08-03 by the same greps this test
//! automates). The live `ggen vision2030` CLI surface (`crates/ggen-cli/src/cmds/vision2030/`)
//! is a separate, JSON-manifest-argument-driven implementation (required schema const
//! `ggen.vision2030.program.v1`) that is structurally incompatible with this pack's catalog
//! files (`ggen.vision2030.catalog.v1` -- missing `program`/`required_domains`/`horizons`,
//! capability entries use `depends_on` instead of `dependencies` and lack `evidence`).
//!
//! This test does not attempt to wire the pack up (that would be a larger redesign, out of
//! scope for a minimal patch). Its job is to lock in that the orphan status is now
//! DOCUMENTED (`packs/vision-2030-phase-change-pack/README.md`), so a future reader is not
//! misled by the pack's polished `pack.toml` description or by
//! `docs/architecture/VISION-2030-PHASE-CHANGE-ARD-PRD-v26.8.3.md`'s Implementation Map
//! (§9) into thinking the pack already cooperates with the live CLI. If the pack ever
//! gains a real consumer, this test and the README should be updated together, not left
//! stale in either direction.
//!
//! Chicago TDD: no mocks. Every assertion reads real files off the real git worktree.

#![allow(clippy::unwrap_used, clippy::expect_used, clippy::panic)]

use std::path::{Path, PathBuf};

fn workspace_root() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../..")
}

fn pack_dir() -> PathBuf {
    workspace_root().join("packs/vision-2030-phase-change-pack")
}

/// Recursively collect files under `dir` whose filename equals or ends with
/// `name_or_suffix` (e.g. `"ggen.toml"` or `".rs"`). Skips `.git` and `target`, the two
/// large non-source directories that would otherwise dominate walk time for no benefit
/// (build artifacts and packed git objects never contain a literal `ggen.toml`/`*.rs`
/// filename worth matching).
fn walk_files(dir: &Path, name_or_suffix: &str, out: &mut Vec<PathBuf>) {
    let entries = match std::fs::read_dir(dir) {
        Ok(e) => e,
        Err(_) => return,
    };
    for entry in entries.flatten() {
        let path = entry.path();
        if path.is_dir() {
            let skip = matches!(
                path.file_name().and_then(|n| n.to_str()),
                Some(".git" | "target")
            );
            if skip {
                continue;
            }
            walk_files(&path, name_or_suffix, out);
        } else if let Some(fname) = path.file_name().and_then(|n| n.to_str()) {
            if fname == name_or_suffix || fname.ends_with(name_or_suffix) {
                out.push(path);
            }
        }
    }
}

#[test]
fn vision_2030_pack_still_has_zero_ggen_toml_or_rust_consumers() {
    // Positive-witness baseline: confirms the orphan claim this test guards against
    // silently going stale in the OTHER direction (the pack quietly gaining a real
    // consumer without the README's orphan notice being revisited). Real filesystem scan,
    // no mocks.
    let root = workspace_root();
    assert!(
        root.join("packs/vision-2030-phase-change-pack/pack.toml")
            .is_file(),
        "sanity check failed: packs/vision-2030-phase-change-pack/pack.toml not found at {} \
         -- workspace_root() resolution is almost certainly wrong",
        root.display()
    );

    let mut toml_files = Vec::new();
    walk_files(&root, "ggen.toml", &mut toml_files);
    assert!(
        !toml_files.is_empty(),
        "sanity check failed: found zero ggen.toml files under {} -- walker is almost \
         certainly broken",
        root.display()
    );
    let toml_hits: Vec<_> = toml_files
        .into_iter()
        .filter(|p| {
            std::fs::read_to_string(p)
                .map(|text| text.contains("vision-2030-phase-change-pack"))
                .unwrap_or(false)
        })
        .collect();
    assert!(
        toml_hits.is_empty(),
        "packs/vision-2030-phase-change-pack now has ggen.toml consumer(s): {toml_hits:?} \
         -- this pack was fully orphaned as of the orphan-vision-2030-phase-change-pack \
         red-team finding; if it has since been wired up, update \
         packs/vision-2030-phase-change-pack/README.md to reflect that instead of leaving \
         the orphan notice stale."
    );

    let crates_dir = root.join("crates");
    let mut rs_files = Vec::new();
    walk_files(&crates_dir, ".rs", &mut rs_files);
    assert!(
        !rs_files.is_empty(),
        "sanity check failed: found zero .rs files under {} -- walker is almost certainly \
         broken",
        crates_dir.display()
    );
    // Exclude this test file itself: it necessarily names the pack in prose (doc comments,
    // assertion messages) while asserting that no OTHER Rust file does.
    let self_file_name = Path::new(file!())
        .file_name()
        .and_then(|n| n.to_str())
        .unwrap_or("vision_2030_pack_orphan_test.rs");
    let rs_hits: Vec<_> = rs_files
        .into_iter()
        .filter(|p| p.file_name().and_then(|n| n.to_str()) != Some(self_file_name))
        .filter(|p| {
            std::fs::read_to_string(p)
                .map(|text| text.contains("vision-2030-phase-change-pack"))
                .unwrap_or(false)
        })
        .collect();
    assert!(
        rs_hits.is_empty(),
        "packs/vision-2030-phase-change-pack now has Rust consumer(s) under crates/: \
         {rs_hits:?} -- same note as above: update the README instead of leaving it stale."
    );
}

#[test]
fn vision_2030_pack_orphan_status_is_documented() {
    let readme_path = pack_dir().join("README.md");
    let text = std::fs::read_to_string(&readme_path).unwrap_or_else(|e| {
        panic!(
            "expected {} to exist and document this pack's orphan status (red-team \
             finding orphan-vision-2030-phase-change-pack): {e}",
            readme_path.display()
        )
    });

    for needle in [
        "ORPHANED",
        "crates/ggen-cli/src/cmds/vision2030",
        "ggen.vision2030.program.v1",
        "ggen.vision2030.catalog.v1",
    ] {
        assert!(
            text.contains(needle),
            "packs/vision-2030-phase-change-pack/README.md is missing expected content \
             {needle:?} -- it should clearly document that this pack is not wired into any \
             ggen.toml or Rust code path, and name the live, schema-incompatible \
             `ggen vision2030` CLI implementation it is easily confused with"
        );
    }
}
