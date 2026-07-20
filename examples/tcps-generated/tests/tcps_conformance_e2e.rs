//! Chicago-TDD end-to-end proof for the `tcps-generated` example: real
//! filesystem (`TempDir`), real oxigraph/praxis-graphlaw, real Tera, and the
//! real `ggen` binary spawned as a subprocess (`CliHarness`). No mocks.
//!
//! Pattern follows `crates/ggen-engine/tests/framework_packs_e2e.rs`
//! (`scaffold_pack_project` + `CliHarness::cargo_bin`), adapted for this
//! standalone example crate (its own `[workspace]`, not a root workspace
//! member): the `ggen` binary is located by an explicit path
//! (`CliHarness::from_path`) rather than `CARGO_BIN_EXE_ggen`, since that env
//! var is only set when the binary is a build dependency of the same Cargo
//! invocation.

#![allow(clippy::expect_used)]

use std::path::{Path, PathBuf};

use chicago_tdd_tools::cli_proof::CliHarness;
use tempfile::TempDir;

/// Repository root: `examples/tcps-generated` -> `examples` -> repo root.
fn repo_root() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .expect("examples/ parent")
        .parent()
        .expect("repo root parent")
        .to_path_buf()
}

/// The real `ggen` binary, built via
/// `cargo build -p ggen-cli-lib --bin ggen` at the repo root.
fn ggen_bin() -> PathBuf {
    repo_root().join("target/debug/ggen")
}

fn packs_dir() -> PathBuf {
    repo_root().join("packs")
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

/// The 6 packs this example wires in `ggen.toml`'s `[packs]` table -- kept as
/// one list so `scaffold()` and any future pack additions stay in sync by
/// construction rather than by remembering to update two places.
const WIRED_PACKS: [&str; 7] = [
    "tcps-core-pack",
    "tcps-release-pack",
    "tcps-std-pack",
    "tcps-ffi-pack",
    "tcps-wasm-pack",
    "tcps-cli-pack",
    "ggen-verify-pack",
];

/// Copy this example project (`ggen.toml`, `Cargo.toml`, `schema/`) plus every
/// real pack it depends on (`WIRED_PACKS`) into a fresh `TempDir`, preserving
/// the exact relative-path shape the checked-in `ggen.toml` expects
/// (`{ path = "../../packs/<name>" }`, resolved from `examples/tcps-generated/`).
/// Returns `(tempdir_guard, project_root)`.
fn scaffold() -> (TempDir, PathBuf) {
    let dir = TempDir::new().expect("tempdir");

    for pack in WIRED_PACKS {
        copy_tree(&packs_dir().join(pack), &dir.path().join("packs").join(pack));
    }

    let project = dir.path().join("examples/tcps-generated");
    let this_crate = Path::new(env!("CARGO_MANIFEST_DIR"));
    std::fs::create_dir_all(&project).expect("mkdir project");
    for name in ["ggen.toml", "Cargo.toml"] {
        std::fs::copy(this_crate.join(name), project.join(name))
            .unwrap_or_else(|e| panic!("copy {name}: {e}"));
    }
    copy_tree(&this_crate.join("schema"), &project.join("schema"));
    // The consumer-local evidence mini-pack (ggen.toml wires `evidence` as a
    // path pack). Deliberately NOT copied from the live checkout: live
    // evidence records whatever reality was at last verify.sh run, and if
    // that includes "this test suite was red," copying it here would make
    // the suite unable to ever go green (the gate would refuse the scaffold
    // sync because the suite was red because the gate refused...). The e2e
    // exercises pack/template machinery under a controlled green fixture;
    // the evidence-gate refusal paths are proven by their own dedicated
    // cases with controlled red/missing fixtures.
    std::fs::create_dir_all(project.join("evidence")).expect("mkdir evidence");
    std::fs::copy(
        this_crate.join("evidence/pack.toml"),
        project.join("evidence/pack.toml"),
    )
    .expect("copy evidence pack.toml");
    copy_tree(
        &this_crate.join("evidence/templates"),
        &project.join("evidence/templates"),
    );
    std::fs::write(
        project.join("evidence/ontology.ttl"),
        "@prefix ver: <http://seanchatmangpt.github.io/packs/ggen-verify#> .\n\
         ver:check-build a ver:Check ; ver:name \"build\" ; ver:exitCode 0 .\n\
         ver:check-test a ver:Check ; ver:name \"test-workspace\" ; ver:exitCode 0 .\n\
         ver:check-clippy a ver:Check ; ver:name \"clippy-reference-gate\" ; ver:exitCode 0 .\n\
         ver:check-bi a ver:Check ; ver:name \"byte-identity\" ; ver:exitCode 0 .\n",
    )
    .expect("seed evidence");
    std::fs::create_dir_all(project.join("templates")).expect("mkdir templates");
    std::fs::create_dir_all(project.join("src")).expect("mkdir src");
    // Seed lib.rs the way the checked-in scaffold starts out (before
    // tcps-core-pack's own `force: true` lib.rs.tmpl overwrites it on sync).
    std::fs::write(
        project.join("src/lib.rs"),
        "include!(\"ggen_pack_mods.rs\");\n",
    )
    .expect("seed lib.rs");

    (dir, project)
}

/// The 26 real module file names (24 from the original structure-definition
/// zip plus `暗号要約.rs`/`正準.rs`, transcribed from the product zip this
/// phase) from the checked-in **product** reference crate's own `src/`
/// listing, plus `lib.rs` -- an independent oracle (read from the vendored
/// reference, not hand-transcribed) that sync output is compared against.
/// `試験.rs` is excluded: it is hand-copied by this repo's own workflow
/// (from this same product reference, see `tests/`'s copy step below), never
/// a `ggen sync` output. Uses the product reference
/// (`reference/製品版/crates/tcps-core/src`), not the original
/// structure-definition zip's `reference/豊田コード生産方式_v26.7.19/src`,
/// because the latter predates `暗号要約`/`正準` and would only ever list 24
/// modules -- the product reference is also where this test suite's own
/// `試験.rs` fixture comes from, so both must be drawn from the same source.
fn expected_core_module_files() -> Vec<String> {
    let reference_src = packs_dir().join("tcps-core-pack/reference/製品版/crates/tcps-core/src");
    let mut names: Vec<String> = std::fs::read_dir(&reference_src)
        .expect("read reference src/")
        .map(|e| e.expect("entry").file_name().to_string_lossy().into_owned())
        .filter(|n| n != "試験.rs")
        .collect();
    names.sort();
    names
}

/// Recursively collect `(relative_path, bytes)` for every file under `root`,
/// sorted by path -- used as a whole-tree snapshot for the idempotency check.
fn snapshot_tree(root: &Path) -> Vec<(String, Vec<u8>)> {
    fn walk(base: &Path, dir: &Path, out: &mut Vec<(String, Vec<u8>)>) {
        for entry in std::fs::read_dir(dir).expect("read_dir") {
            let entry = entry.expect("entry");
            let path = entry.path();
            if path.is_dir() {
                walk(base, &path, out);
            } else {
                let rel = path
                    .strip_prefix(base)
                    .expect("strip_prefix")
                    .to_string_lossy()
                    .into_owned();
                out.push((rel, std::fs::read(&path).expect("read file")));
            }
        }
    }
    let mut out = Vec::new();
    walk(root, root, &mut out);
    out.sort_by(|a, b| a.0.cmp(&b.0));
    out
}

#[test]
fn sync_generates_all_24_core_modules_plus_lib() {
    let (_dir, project) = scaffold();

    let output = CliHarness::from_path(ggen_bin())
        .args(["sync", "run"])
        .current_dir(&project)
        .run()
        .expect("run sync");
    output.assert_success();

    let mut generated: Vec<String> = std::fs::read_dir(project.join("src"))
        .expect("read src/")
        .map(|e| e.expect("entry").file_name().to_string_lossy().into_owned())
        // Engine-owned aggregator (`[templates] aggregate_modules = true`),
        // not one of tcps-core-pack's own 24 module + lib.rs outputs.
        .filter(|n| n != "ggen_pack_mods.rs")
        .collect();
    generated.sort();

    assert_eq!(
        generated,
        expected_core_module_files(),
        "generated src/ file list must match the reference crate's own module \
         list (24 modules + lib.rs)"
    );
}

#[test]
fn sync_generates_release_pack_infra_outputs() {
    let (_dir, project) = scaffold();

    CliHarness::from_path(ggen_bin())
        .args(["sync", "run"])
        .current_dir(&project)
        .run()
        .expect("run sync")
        .assert_success();

    for rel in [
        "infra/ci/tier1.yml",
        "infra/ci/verify.yml",
        "infra/ci/security.yml",
        "infra/targets/tier1.txt",
        "infra/targets/tier2.txt",
        "infra/packaging/deb/control",
        "infra/packaging/npm/package.json",
        "infra/packaging/nuget/tcps.nuspec",
    ] {
        assert!(
            project.join(rel).is_file(),
            "tcps-release-pack must write {rel}"
        );
    }
}

/// The 10 files the 4 new packs (`tcps-std-pack`/`tcps-ffi-pack`/
/// `tcps-wasm-pack`/`tcps-cli-pack`) must write, each into its own
/// `crates/tcps-<name>/` subdirectory -- chosen specifically to avoid
/// colliding with `tcps-core-pack`'s own flat `src/` layout (§ design note in
/// this repo's TCPS-PACK-ARD-PRD.md follow-up: the product's own
/// `crates/tcps-{core,std,ffi,wasm,cli}/` shape, minus `tcps-core` itself,
/// which stays at this example's root `src/` to preserve the
/// already-proven 26-module baseline unchanged).
fn expected_new_pack_output_files() -> [&'static str; 10] {
    [
        "crates/tcps-std/Cargo.toml",
        "crates/tcps-std/src/lib.rs",
        "crates/tcps-ffi/Cargo.toml",
        "crates/tcps-ffi/src/lib.rs",
        "crates/tcps-ffi/include/tcps.h",
        "crates/tcps-wasm/Cargo.toml",
        "crates/tcps-wasm/src/lib.rs",
        "crates/tcps-cli/Cargo.toml",
        "crates/tcps-cli/src/main.rs",
        "crates/tcps-cli/src/基準.rs",
    ]
}

#[test]
fn sync_generates_all_4_new_pack_outputs_without_collision() {
    let (_dir, project) = scaffold();

    CliHarness::from_path(ggen_bin())
        .args(["sync", "run"])
        .current_dir(&project)
        .run()
        .expect("run sync")
        .assert_success();

    for rel in expected_new_pack_output_files() {
        assert!(
            project.join(rel).is_file(),
            "expected {rel} written by one of the 4 new packs (tcps-std/ffi/wasm/cli-pack)"
        );
    }

    // The 4 new packs must not have clobbered tcps-core-pack's own flat
    // src/ layout (the collision this subdirectory scheme exists to avoid).
    assert!(project.join("src/lib.rs").is_file());
    assert!(project.join("src/語彙.rs").is_file());
}

#[test]
fn second_sync_is_byte_identical_idempotent() {
    let (_dir, project) = scaffold();

    CliHarness::from_path(ggen_bin())
        .args(["sync", "run"])
        .current_dir(&project)
        .run()
        .expect("first sync")
        .assert_success();
    let before = snapshot_tree(&project.join("src"));
    let before_infra = snapshot_tree(&project.join("infra"));
    let before_crates = snapshot_tree(&project.join("crates"));
    let lock_before = std::fs::read_to_string(project.join("ggen.lock")).expect("ggen.lock");

    CliHarness::from_path(ggen_bin())
        .args(["sync", "run"])
        .current_dir(&project)
        .run()
        .expect("second sync")
        .assert_success();
    let after = snapshot_tree(&project.join("src"));
    let after_infra = snapshot_tree(&project.join("infra"));
    let after_crates = snapshot_tree(&project.join("crates"));
    let lock_after = std::fs::read_to_string(project.join("ggen.lock")).expect("ggen.lock 2");

    assert_eq!(
        before, after,
        "second sync must leave every generated src/ file byte-identical"
    );
    assert_eq!(
        before_infra, after_infra,
        "second sync must leave every generated infra/ file byte-identical"
    );
    assert_eq!(
        before_crates, after_crates,
        "second sync must leave every generated crates/ file (the 4 new packs' \
         output) byte-identical -- idempotency must hold across the full \
         expanded 6-pack tree, not just the original 2-pack subset"
    );
    assert_eq!(
        lock_before, lock_after,
        "ggen.lock must be byte-identical across identical runs"
    );
}

#[test]
fn graph_validate_passes_on_generated_project() {
    let (_dir, project) = scaffold();

    CliHarness::from_path(ggen_bin())
        .args(["sync", "run"])
        .current_dir(&project)
        .run()
        .expect("sync")
        .assert_success();

    CliHarness::from_path(ggen_bin())
        .args(["graph", "validate"])
        .current_dir(&project)
        .run()
        .expect("graph validate")
        .assert_success();
}

/// Negative case: a fact injected into the CONSUMER's own project ontology
/// (`schema/domain.ttl`) that violates tcps-core-pack's shipped SPARQL
/// gates (`gates/010_dangling_reference.rq` / `020_referenced_module_name.rq`,
/// translated from the former `shapes.ttl`'s `tcps:ModuleShape`) -- a module
/// that `tcps:dependsOnModule` an IRI that is never declared as a
/// `tcps:Module` anywhere in the union graph (no `a tcps:Module`, no
/// `tcps:name`: a genuinely dangling reference, not merely an
/// undeclared-but-plausible one). The gate queries evaluate the union graph
/// (pack ontology + consumer ontology) at sync time and must refuse before
/// writing any output (`FM-PACK-013`, see `crates/ggen-engine/src/sync.rs`'s
/// per-pack `gates/*.rq` loop).
#[test]
fn shapes_gate_refuses_dangling_module_dependency() {
    let (_dir, project) = scaffold();

    let domain = project.join("schema/domain.ttl");
    let mut content = std::fs::read_to_string(&domain).expect("domain.ttl");
    content.push_str(
        "\ntcps:DanglingTestModule a tcps:Module ;\n    \
         tcps:name \"DanglingTestModule\" ;\n    \
         tcps:dependsOnModule tcps:GhostModuleThatDoesNotExist .\n",
    );
    std::fs::write(&domain, content).expect("write violating domain.ttl");

    let output = CliHarness::from_path(ggen_bin())
        .args(["sync", "run"])
        .current_dir(&project)
        .run()
        .expect("run sync");
    output.assert_failure();
    output.assert_stderr_contains("FM-PACK-013");
    output.assert_stderr_contains("tcps-core-pack");

    // Refused BEFORE any write: no aggregator, no module files landed.
    assert!(
        !project.join("src/ggen_pack_mods.rs").exists(),
        "a refused sync must not have written any template output"
    );
    assert!(
        !project.join("src/語彙.rs").exists(),
        "a refused sync must not have written any template output"
    );
}

/// Positive control for the negative case above: same shape of fact
/// (`tcps:dependsOnModule` on a `tcps:Module` individual added via the
/// consumer's own ontology), but pointing at a REAL, already-declared
/// module (`tcps:語彙Module`) instead of a dangling one. Proves the gate
/// actually discriminates a dangling reference rather than refusing every
/// sync outright.
#[test]
fn shapes_gate_allows_when_dependency_resolves() {
    let (_dir, project) = scaffold();

    let domain = project.join("schema/domain.ttl");
    let mut content = std::fs::read_to_string(&domain).expect("domain.ttl");
    content.push_str(
        "\ntcps:ResolvedTestModule a tcps:Module ;\n    \
         tcps:name \"ResolvedTestModule\" ;\n    \
         tcps:dependsOnModule tcps:語彙Module .\n",
    );
    std::fs::write(&domain, content).expect("write conforming domain.ttl");

    CliHarness::from_path(ggen_bin())
        .args(["sync", "run"])
        .current_dir(&project)
        .run()
        .expect("run sync")
        .assert_success();
}
