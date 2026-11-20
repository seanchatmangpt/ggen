//! Court: `[FM-WRITE-001]` — a project root that does not exist / cannot be
//! canonicalized is a typed refusal from the shared path resolver, before
//! any path is planned against it.
//!
//! Chicago: real filesystem (a `TempDir` whose inner project directory is
//! deleted out from under the resolver), the real
//! `ggen_engine::write::resolve_target` — the same resolver every sync write
//! (`frontmatter` path, `from:` resolution, and the generation-rules path via
//! `decide_and_maybe_apply`) routes through. No mocks.
//! Emission site: `crates/ggen-engine/src/write.rs` `resolve_target`
//! (`fm_write(1, ...)`).
//! Closes GAP-3 for FM-WRITE-001 (CARRY-09, zd-48).

#![allow(clippy::unwrap_used, clippy::expect_used, clippy::panic)]

use ggen_engine::write::resolve_target;
use tempfile::TempDir;

#[test]
fn missing_root_refuses_with_fm_write_001() {
    let dir = TempDir::new().expect("tempdir");
    let project = dir.path().join("project");
    std::fs::create_dir_all(&project).expect("mkdir project");
    // Real deletion: the root the resolver will be asked about stops existing.
    std::fs::remove_dir(&project).expect("remove project dir");

    let err = resolve_target(&project, "out/names.txt")
        .expect_err("a non-canonicalizable root must refuse");
    let msg = err.to_string();
    assert!(msg.contains("FM-WRITE-001"), "{msg}");
    assert!(
        msg.contains(&*project.to_string_lossy()),
        "refusal must name the root: {msg}"
    );
}
