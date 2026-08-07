//! Chicago-TDD end-to-end proof for `gymact-bridge-pack` -- now living in
//! the real `~/gymact` repo (`ggen/gymact-bridge-pack/`, moved there from
//! this repo's own `packs/` after `~/gymact` shipped for real; see that
//! pack's own `pack.toml` for the full history), proving the one
//! generation capability ggen had no existing precedent for:
//! `sosa:Procedure` capability facts, admitted in `ontology.ttl`,
//! projected into a generated MCP tool schema, a typed Rust catalog, a
//! Diataxis-style reference doc, and a cross-checked coverage proof test,
//! plus GymAct's own REAL SHACL shape (`urn:gymact:shape:capability`,
//! reached via a real symlink from `ontology/profile.shacl.ttl` to
//! `src/gymact/ontology/profile.shacl.ttl` inside `~/gymact` itself --
//! not a copy) enforced at sync time via the `shape:` template-frontmatter
//! mechanism (`FM-TPL-025`).
//!
//! CROSS-REPO: the pack under test lives outside this repository. This
//! test resolves it via `$HOME/gymact/ggen/gymact-bridge-pack` (override
//! with `GYMACT_REPO` env var for a non-standard checkout location) and
//! skips loudly, with a named reason, if that path doesn't exist -- not a
//! silent pass, not a mock of the pack's content. `~/gymact` also has its
//! own repeatable verification (`just ggen-bridge-check` there) that
//! proves the same generation without needing this Rust/Cargo toolchain
//! at all.
//!
//! Real filesystem, real `sync()`, real assertions on real generated
//! content -- no mocks.

#![allow(clippy::unwrap_used, clippy::expect_used, clippy::panic)]

mod support;

use std::path::{Path, PathBuf};

use ggen_engine::sync::{sync, SyncOptions};
use support::{assert_gate_refuses, assert_idempotent, read, scaffold_pack};

/// Resolves the real, moved pack location: `$GYMACT_REPO/ggen/
/// gymact-bridge-pack`, defaulting to `$HOME/gymact/ggen/gymact-bridge-pack`.
/// Returns `None` (not a panic) if the resolved path doesn't exist -- the
/// caller must skip loudly, not fail opaquely or run assertions against
/// nothing.
fn gymact_bridge_pack_dir() -> Option<PathBuf> {
    let gymact_repo = std::env::var("GYMACT_REPO").unwrap_or_else(|_| {
        format!(
            "{}/gymact",
            std::env::var("HOME").expect("HOME must be set")
        )
    });
    let pack_dir = PathBuf::from(gymact_repo).join("ggen/gymact-bridge-pack");
    pack_dir.is_dir().then_some(pack_dir)
}

/// Named, visible skip -- matching this ecosystem's `BLOCKED:<REASON>`
/// convention (see e.g. autofde-lab's `MFW_HOME`-absent SHACL test) rather
/// than a silent no-op or a mocked pack.
macro_rules! require_gymact_repo {
    () => {
        match gymact_bridge_pack_dir() {
            Some(dir) => dir,
            None => {
                eprintln!(
                    "BLOCKED:GYMACT_REPO_ABSENT -- no gymact-bridge-pack found at \
                     $GYMACT_REPO/ggen/gymact-bridge-pack (default $HOME/gymact/ggen/\
                     gymact-bridge-pack). Clone seanchatmangpt/gymact and/or set \
                     GYMACT_REPO to run this test for real; skipping, not failing, since \
                     this is a real external repo dependency, not a bug in ggen."
                );
                return;
            }
        }
    };
}

/// `shape:` entries are resolved relative to the CONSUMER project root and
/// a `../` traversal component is refused (`FM-WRITE-002`) -- confirmed via
/// a real failing sync run against this exact template. A pack cannot ship
/// a portably pack-relative `shape:` reference the way `gates/*.rq` are
/// discovered automatically; a consumer wanting the precondition enforced
/// must copy the shape file into their own project, exactly as this helper
/// does, before syncing. The copied file here is the REAL GymAct SHACL
/// shape, reached through `ontology/profile.shacl.ttl`'s symlink into
/// `~/gymact`'s own `src/gymact/ontology/` -- not a bridge-invented one.
fn wire_shape(project: &Path, pack_dir: &Path) {
    std::fs::create_dir_all(project.join("shapes")).expect("mkdir consumer shapes dir");
    std::fs::copy(
        pack_dir.join("ontology/profile.shacl.ttl"),
        project.join("shapes/profile.shacl.ttl"),
    )
    .expect("copy the real GymAct SHACL shape (via symlink) into consumer project");
}

#[test]
fn gymact_bridge_pack_generates_all_four_surfaces_and_is_idempotent() {
    let pack_dir = require_gymact_repo!();
    let (_dir, project) = scaffold_pack(&pack_dir);
    wire_shape(&project, &pack_dir);

    sync(
        &project,
        SyncOptions {
            dry_run: false,
            ..Default::default()
        },
    )
    .expect(
        "sync must generate the operation catalog, MCP tool schema, reference doc, and proof test",
    );

    // (1) Typed Rust operation catalog: real interpolated content, not
    // literal `{{ }}` left unrendered, sourced from the real GymAct
    // consequence classification (act -> Do, the rest -> Read).
    let catalog = read(&project, "src/gymact_operation_catalog.rs");
    assert!(
        catalog.contains(r#"id: "discover""#)
            && catalog.contains(r#"id: "observe""#)
            && catalog.contains(r#"id: "act""#)
            && catalog.contains(r#"id: "verify""#),
        "catalog missing an admitted operation id: {catalog}"
    );
    assert!(
        catalog.contains(r#"Operation { id: "act", consequence_class: "Do" }"#),
        "catalog must carry act's real Do classification: {catalog}"
    );
    assert!(
        catalog.contains(r#"Operation { id: "discover", consequence_class: "Read" }"#),
        "catalog must carry discover's real Read classification: {catalog}"
    );
    assert!(
        catalog.contains("pub fn lookup_operation")
            && catalog.contains("pub fn operations_by_consequence_class"),
        "catalog missing real lookup behavior, not just a static table: {catalog}"
    );

    // (2) Generated MCP tool schema -- the genuinely new template family.
    let mcp_tools = read(&project, "src/gymact_mcp_tools.rs");
    assert!(
        mcp_tools.contains(r#"name: "gymact.act""#)
            && mcp_tools.contains(r#"name: "gymact.observe""#),
        "MCP tool schema missing namespaced tool names: {mcp_tools}"
    );
    assert!(
        mcp_tools.contains("pub fn do_class_tools"),
        "MCP tool schema missing do_class_tools(): {mcp_tools}"
    );

    // (3) Reference doc: real table rows.
    let reference = read(&project, "docs/gymact-bridge/reference.md");
    assert!(
        reference.contains("# GymAct Bridge — Operation Reference")
            && reference.contains("| `act` | Do |"),
        "reference.md missing title or act's real row: {reference}"
    );

    // (4) Generated proof test: real fn names, real cross-check imports.
    let proof = read(&project, "tests/gymact_bridge_operation_catalog_proof.rs");
    assert!(
        proof.contains("fn catalog_and_mcp_tools_agree_on_every_admitted_operation()")
            && proof.contains("fn every_operation_has_a_real_consequence_class()"),
        "generated proof test missing its cross-check functions: {proof}"
    );

    // (5) Idempotency of the pack's own generation.
    assert_idempotent(&project);
}

#[test]
fn gymact_bridge_pack_gate_refuses_capability_missing_title() {
    let pack_dir = require_gymact_repo!();
    let (_dir, project) = scaffold_pack(&pack_dir);
    wire_shape(&project, &pack_dir);
    sync(
        &project,
        SyncOptions {
            dry_run: false,
            ..Default::default()
        },
    )
    .expect("baseline sync must succeed before sabotage");

    assert_gate_refuses(
        &project,
        "@prefix sosa: <http://www.w3.org/ns/sosa/> .\n\
         @prefix dct: <http://purl.org/dc/terms/> .\n\
         <urn:gymact:bridge:capability:sabotage> a sosa:Procedure ;\n\
         \x20\x20dct:type <urn:gymact:consequence:do> .\n",
        "010_required",
    );
}

/// The real GymAct SHACL shape (`urn:gymact:shape:capability`), enforced
/// here via ggen's `shape:` mechanism, not a bridge-invented one: `dct:type`
/// must be drawn from `{urn:gymact:consequence:read, urn:gymact:consequence:do}`.
/// A capability classified with any other IRI must refuse the sync with a
/// real `FM-TPL-025` SHACL violation citing the offending subject, and must
/// leave no output on disk -- mirroring
/// `crates/ggen-engine/tests/shape_shacl_enforcement_e2e.rs`'s own
/// assertions for this exact mechanism.
#[test]
fn gymact_bridge_pack_shape_refuses_capability_with_unadmitted_consequence_type() {
    let pack_dir = require_gymact_repo!();
    let (_dir, project) = scaffold_pack(&pack_dir);
    wire_shape(&project, &pack_dir);
    std::fs::write(
        project.join("ontology.ttl"),
        "@prefix sosa: <http://www.w3.org/ns/sosa/> .\n\
         @prefix dct: <http://purl.org/dc/terms/> .\n\
         <urn:gymact:bridge:capability:sabotage> a sosa:Procedure ;\n\
         \x20\x20dct:title \"sabotage\" ;\n\
         \x20\x20dct:type <urn:gymact:consequence:maybe> .\n",
    )
    .expect("write violating consumer ontology");

    let err = sync(
        &project,
        SyncOptions {
            dry_run: false,
            ..Default::default()
        },
    )
    .expect_err(
        "sync must refuse: dct:type <urn:gymact:consequence:maybe> is not in the real \
         urn:gymact:shape:capability's admitted sh:in set",
    );
    let msg = err.to_string();
    assert!(msg.contains("FM-TPL-025"), "{msg}");
    assert!(
        msg.contains("sabotage"),
        "violation must name the focus node: {msg}"
    );
    assert!(
        !project.join("src/gymact_operation_catalog.rs").exists(),
        "no output should be written when the declared shape is violated"
    );
}
