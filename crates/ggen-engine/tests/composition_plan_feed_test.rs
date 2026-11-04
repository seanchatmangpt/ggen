//! WO-03 follow-up (lane e80-10): a persisted composition plan
//! (`ggen pack compose --out` output, persisted at
//! `<root>/.ggen-v2/composition-plan.json`) is consumed by Stage 2c instead
//! of re-extracting every `pack.toml` from disk. Chicago-TDD: real packs
//! copied from the repo corpus, the real `ggen` binary as a subprocess, real
//! filesystem effects, no mocks.
//!
//! The no-re-extraction discriminator is real, not interaction-counted: a
//! `pack.toml` whose `[graph]` table carries a non-canonical port id is
//! refused by `extract_pack_manifest` (FM-PACK-015) on the legacy path, but
//! a sync that consumes the plan never reads it — so the same broken corpus
//! syncs green WITH the plan and refuses WITHOUT it.

#![allow(clippy::unwrap_used, clippy::expect_used, clippy::panic)]

mod support;

use std::path::{Path, PathBuf};
use std::process::Command;

use chicago_tdd_tools::cli_proof::CliHarness;
use support::scaffold_multi_pack;
use tempfile::TempDir;

fn ggen_bin() -> PathBuf {
    if let Ok(path) = std::env::var("CARGO_BIN_EXE_ggen") {
        let p = PathBuf::from(path);
        if p.exists() {
            return p;
        }
    }
    let target_root = std::env::var_os("CARGO_TARGET_DIR")
        .map(PathBuf::from)
        .or_else(|| {
            let manifest_dir = std::env::var_os("CARGO_MANIFEST_DIR").map(PathBuf::from)?;
            let mut dir: &Path = manifest_dir.as_path();
            loop {
                if dir.join("Cargo.lock").exists() {
                    return Some(dir.join("target"));
                }
                match dir.parent() {
                    Some(p) => dir = p,
                    None => return None,
                }
            }
        });
    if let Some(target) = target_root {
        for profile in ["debug", "release"] {
            let candidate = target.join(profile).join("ggen");
            if candidate.is_file() {
                return candidate;
            }
        }
    }
    panic!(
        "could not resolve the `ggen` binary: build it with `cargo build \
         -p ggen-cli-lib --bin ggen`"
    );
}

/// Three real packs from the repo corpus (name == pack.toml `[pack] name` ==
/// manifest `[packs]` key, so plan `pack_ids` can be asserted against both).
/// affidavit-pack requires praxis-core-pack's capability, so compose over
/// the pair without it refuses (unbound requirement) — the triple is the
/// minimal composable set containing both.
const PACKS: [&str; 3] = ["affidavit-pack", "star-toml-pack", "praxis-core-pack"];

/// A minimal valid plan over `PACKS` in the exact wire shape `ggen pack
/// compose --out` writes (all six keys required — `deny_unknown_fields` on
/// the consumer side makes this the load-bearing schema).
fn valid_plan_json(order: &[&str]) -> String {
    let ids: Vec<String> = order.iter().map(|p| format!("\"{p}\"")).collect();
    let order_json: Vec<String> = order.iter().map(|p| format!("\"{p}\"")).collect();
    format!(
        "{{\n\
         \"pack_ids\": [{ids}],\n\
         \"provides\": {{}},\n\
         \"order\": [{order}],\n\
         \"artifact_paths\": {{}},\n\
         \"self_satisfied\": [],\n\
         \"transitive_deps\": {{}}\n\
         }}",
        ids = ids.join(", "),
        order = order_json.join(", "),
    )
}

fn write_plan(project: &Path, plan: &str) {
    let dir = project.join(".ggen-v2");
    std::fs::create_dir_all(&dir).expect("mkdir .ggen-v2");
    std::fs::write(dir.join("composition-plan.json"), plan).expect("write composition-plan.json");
}

/// Real `ggen pack compose --packs <PACKS> --out <path>` against the same
/// temp corpus the consumer copies from (via `GGEN_CAPABILITY_CORPUS_ROOTS`,
/// the verb's documented test override), proving the producer side of the
/// wire shape end to end — never the shared on-disk corpora.
fn compose_out(corpus: &Path, plan_out: &Path) {
    let output = Command::new(ggen_bin())
        .arg("pack")
        .arg("compose")
        .arg("--packs")
        .arg(PACKS.join(","))
        .arg("--out")
        .arg(plan_out)
        .env("GGEN_CAPABILITY_CORPUS_ROOTS", corpus.display().to_string())
        .output()
        .expect("spawn ggen pack compose");
    assert!(
        output.status.success(),
        "ggen pack compose failed (exit {:?}):\nstdout:\n{}\nstderr:\n{}",
        output.status.code(),
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr),
    );
    assert!(
        plan_out.is_file(),
        "compose --out did not write {}",
        plan_out.display()
    );
}

fn sync(project: &Path) -> chicago_tdd_tools::cli_proof::CliOutput {
    CliHarness::from_path(ggen_bin())
        .args(["sync", "run"])
        .current_dir(project)
        .run()
        .expect("run sync")
}

// ---------------------------------------------------------------------
// Plan present → sync consumes it
// ---------------------------------------------------------------------

#[test]
fn plan_present_sync_succeeds() {
    let (_dir, project) = scaffold_multi_pack(&PACKS);
    write_plan(
        &project,
        &valid_plan_json(&["affidavit-pack", "star-toml-pack", "praxis-core-pack"]),
    );
    let output = sync(&project);
    let _ = output.assert_success();
}

#[test]
fn compose_out_plan_feeds_sync() {
    // Own the corpus for this test: a fresh temp copy of the three packs
    // (scaffold_multi_pack already copies them; reuse its TempDir root).
    let (dir, project) = scaffold_multi_pack(&PACKS);
    let plan_out = dir.path().join("plan.json");
    compose_out(dir.path(), &plan_out);

    // The real plan's pack set must name exactly the manifest's packs —
    // the stale-plan law's happy case, witnessed on the real wire shape.
    let raw = std::fs::read_to_string(&plan_out).expect("read composed plan");
    for pack in PACKS {
        assert!(
            raw.contains(&format!("\"{pack}\"")),
            "composed plan does not name {pack}:\n{raw}"
        );
    }

    write_plan(&project, &raw);
    let output = sync(&project);
    let _ = output.assert_success();
}

// ---------------------------------------------------------------------
// Stale plan → typed refusal (FM-PACK-023)
// ---------------------------------------------------------------------

#[test]
fn stale_plan_pack_set_refuses() {
    let (_dir, project) = scaffold_multi_pack(&PACKS);
    // Plan composed for a manifest that only had affidavit-pack; the
    // consumer's manifest since gained star-toml-pack → stale.
    write_plan(&project, &valid_plan_json(&["affidavit-pack"]));
    let output = sync(&project);
    assert!(
        output.exit_code != 0,
        "stale plan must refuse, but sync exited 0"
    );
    let _ = output.assert_stderr_contains("FM-PACK-023");
}

// ---------------------------------------------------------------------
// Schema mismatch → typed refusal (FM-PACK-022)
// ---------------------------------------------------------------------

#[test]
fn schema_mismatch_refuses() {
    let (_dir, project) = scaffold_multi_pack(&PACKS);
    // Unknown top-level key → deny_unknown_fields refusal.
    let plan = valid_plan_json(&["affidavit-pack", "star-toml-pack", "praxis-core-pack"]);
    let plan = plan.trim_end_matches('}').to_string() + ",\"unexpected_key\": 1}";
    write_plan(&project, &plan);
    let output = sync(&project);
    assert!(
        output.exit_code != 0,
        "schema-mismatched plan must refuse, but sync exited 0"
    );
    let _ = output.assert_stderr_contains("FM-PACK-022");
}

// ---------------------------------------------------------------------
// No re-extraction on the plan path (real discriminator, not call-count)
// ---------------------------------------------------------------------

/// Inject a `[graph]` table with a non-canonical port id into a pack copy's
/// `pack.toml`: resolve's `PackToml` collects `[graph]` as informational
/// `extra` (so pack resolution still succeeds), but the Stage 2c legacy
/// extraction path refuses it (FM-PACK-015). A sync that consumes the plan
/// never extracts, so the same broken corpus is the discriminator between
/// the two paths.
fn inject_bad_graph(pack_dir: &Path) {
    let path = pack_dir.join("pack.toml");
    let mut content = std::fs::read_to_string(&path).expect("read pack.toml");
    content.push_str("\n[graph]\nprovides = [\"bad port!\"]\n");
    std::fs::write(path, content).expect("write pack.toml");
}

#[test]
fn plan_path_skips_reextraction() {
    // Arm A (no plan): legacy extraction path refuses the broken manifest.
    let (dir_a, project_a) = scaffold_multi_pack(&PACKS);
    inject_bad_graph(&dir_a.path().join("affidavit-pack"));
    let out_a = sync(&project_a);
    assert!(
        out_a.exit_code != 0,
        "legacy path must refuse a non-canonical port id, but sync exited 0"
    );
    let _ = out_a.assert_stderr_contains("FM-PACK-015");

    // Arm B (plan present): the plan is consumed, pack.toml never
    // re-extracted, sync is green on the identical broken corpus.
    let (dir_b, project_b) = scaffold_multi_pack(&PACKS);
    inject_bad_graph(&dir_b.path().join("affidavit-pack"));
    write_plan(
        &project_b,
        &valid_plan_json(&["affidavit-pack", "star-toml-pack", "praxis-core-pack"]),
    );
    let out_b = sync(&project_b);
    let _ = out_b.assert_success();
}

// ---------------------------------------------------------------------
// Absent plan → legacy path green (compat)
// ---------------------------------------------------------------------

#[test]
fn absent_plan_legacy_path_green() {
    let (_dir, project) = scaffold_multi_pack(&PACKS);
    let output = sync(&project);
    let _ = output.assert_success();
}
