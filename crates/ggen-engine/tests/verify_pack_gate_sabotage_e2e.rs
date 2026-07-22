//! Chicago-TDD sabotage coverage for the three `ggen-verify-pack` SPARQL
//! gates that had zero referencing tests before this file:
//! `030_evidence_fresh.rq`, `040_latest_andon_green.rq`,
//! `050_output_count_regression.rq`. `010_evidence_present.rq` and
//! `020_evidence_green.rq` are already covered by
//! `verify_pack_evidence_loop_e2e.rs`.
//!
//! Real collaborators throughout: a real filesystem (`TempDir`), the real
//! in-process `sync` pipeline, real pack-shipped SPARQL gates loaded off
//! disk from `packs/ggen-verify-pack/gates/`. Each gate gets a two-sided
//! proof: the clean/legitimate input passes sync, and the sabotaged input
//! is refused by name (the exact gate filename in the error).
//!
//! `030`/`040` key off the reflexive `ggenr:Sync` facts sync itself
//! materializes from `.ggen-v2/receipt-log.jsonl` at load time (see
//! `sync.rs`'s "P2: reflexive receipts" block) — not something a test can
//! forge via ordinary ontology facts. `040`'s sabotage in particular
//! (a non-Green `andon` on the latest sync) is unreachable through today's
//! live pipeline: `sync.rs`'s own doc comment on `admission_items` states
//! every output decision is honestly `Admitted` because there is no
//! per-file quarantine/refusal stage yet, so andon is always derived
//! `Green` on any receipt that pipeline itself writes. To exercise `040`'s
//! refusal without fabricating evidence inside the gate/pipeline under
//! test, this file directly edits the persisted receipt-log line — the
//! same "real external file, hand-sabotaged between syncs" pattern already
//! used by `verify_pack_evidence_loop_e2e.rs` against `evidence/ontology.ttl`
//! and by `receipt_genesis_ceiling_e2e.rs` against `ontology.ttl` — using
//! `praxis_core`'s own record types and its own `recompute_chain_hash` so
//! the forged entry is chain-valid, not merely textually similar.

use std::path::{Path, PathBuf};

use ggen_engine::sync::{sync, SyncOptions, SyncReceipt, RECEIPT_LOG_REL_PATH};
use praxis_core::law::Andon;
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

/// Scratch consumer wiring only `ggen-verify-pack` (no star-toml-pack, no
/// checks/emitter machinery needed — these three gates key off `ver:Check`
/// facts and `ggenr:Sync` reflexive facts placed directly in the domain
/// ontology, the same technique `receipt_genesis_ceiling_e2e.rs` uses).
fn scaffold(names: &[&str]) -> (TempDir, PathBuf) {
    let dir = TempDir::new().expect("tempdir");
    copy_tree(
        &packs_dir().join("ggen-verify-pack"),
        &dir.path().join("packs/ggen-verify-pack"),
    );

    let project = dir.path().join("consumer");
    std::fs::create_dir_all(project.join("templates")).expect("mkdir templates");
    write_ontology(&project, names);
    std::fs::write(
        project.join("ggen.toml"),
        "[project]\nname = \"gate-sabotage-consumer\"\n\n\
         [ontology]\nsource = \"ontology.ttl\"\n\n\
         [packs]\n\
         ggen-verify-pack = { path = \"../packs/ggen-verify-pack\" }\n\n\
         [templates]\ndir = \"templates\"\n\n\
         [law]\nreflexive = true\n",
    )
    .expect("write ggen.toml");

    (dir, project)
}

/// The four `ver:RequiredCheck` names the pack's own `ontology.ttl` demands
/// (`build`, `test-workspace`, `clippy-reference-gate`, `byte-identity`),
/// each satisfied here with a green `ver:Check` -- required for gates
/// `010_evidence_present.rq`/`020_evidence_green.rq` to pass so the three
/// gates under test in this file can even be reached.
const REQUIRED_CHECKS: &[&str] = &[
    "build",
    "test-workspace",
    "clippy-reference-gate",
    "byte-identity",
];

fn green_required_checks_ttl() -> String {
    let mut ttl = String::new();
    for (i, name) in REQUIRED_CHECKS.iter().enumerate() {
        ttl.push_str(&format!(
            "ver:req_check_{i} a ver:Check ; ver:name \"{name}\" ; ver:exitCode 0 .\n"
        ));
    }
    ttl
}

/// One `to:` output file per name, so shrinking `names` between two real
/// syncs genuinely shrinks the receipt's `outputCount` — no forged facts,
/// just fewer real individuals producing fewer real output files.
fn write_ontology(project: &Path, names: &[&str]) {
    let mut ttl = String::from("@prefix ex: <http://example.org/> .\n");
    ttl.push_str("@prefix ver: <http://seanchatmangpt.github.io/packs/ggen-verify#> .\n");
    for name in names {
        ttl.push_str(&format!("ex:{name} ex:name \"{name}\" .\n"));
    }
    ttl.push_str(&green_required_checks_ttl());
    std::fs::write(project.join("ontology.ttl"), ttl).expect("write ontology.ttl");

    for name in names {
        std::fs::write(
            project.join(format!("templates/{name}.tmpl")),
            format!(
                "---\nto: out/{name}.txt\nforce: true\nsparql:\n  row: \"PREFIX ex: <http://example.org/> SELECT ?n WHERE {{ ex:{name} ex:name ?n }}\"\n---\n{{% for r in results %}}{{{{ r.n }}}}\n{{% endfor %}}\n"
            ),
        )
        .expect("write template");
    }
    // Remove templates for names no longer present, so a shrink in `names`
    // really does shrink `outputCount` on the next sync (stale template
    // files for removed names would otherwise keep re-emitting stale
    // outputs from stale ontology individuals -- belt and suspenders,
    // though `ex:<name>` facts already vanished from ontology.ttl above).
    if let Ok(entries) = std::fs::read_dir(project.join("templates")) {
        for entry in entries.flatten() {
            let path = entry.path();
            let Some(stem) = path.file_stem().and_then(|s| s.to_str()) else {
                continue;
            };
            if !names.contains(&stem) {
                let _ = std::fs::remove_file(&path);
            }
        }
    }
}

/// Write a `ver:Check` individual with an explicit `ver:verifiedGraphHash`
/// literal (the shape `030_evidence_fresh.rq` matches against) alongside
/// the domain facts and the four green required checks (so gates
/// `010`/`020` stay satisfied and `030` is the only thing being probed).
fn write_ontology_with_stale_check(project: &Path, names: &[&str], verified_graph_hash: &str) {
    let mut ttl = String::from("@prefix ex: <http://example.org/> .\n");
    ttl.push_str("@prefix ver: <http://seanchatmangpt.github.io/packs/ggen-verify#> .\n");
    for name in names {
        ttl.push_str(&format!("ex:{name} ex:name \"{name}\" .\n"));
    }
    ttl.push_str(&green_required_checks_ttl());
    ttl.push_str(&format!(
        "ver:freshness_probe a ver:Check ; ver:name \"freshness-probe\" ; ver:exitCode 0 ; ver:verifiedGraphHash \"{verified_graph_hash}\" .\n"
    ));
    std::fs::write(project.join("ontology.ttl"), ttl).expect("write ontology.ttl");
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

fn read_log_lines(project: &Path) -> Vec<String> {
    std::fs::read_to_string(project.join(RECEIPT_LOG_REL_PATH))
        .expect("read receipt log")
        .lines()
        .filter(|l| !l.trim().is_empty())
        .map(str::to_string)
        .collect()
}

// ── Gate 030_evidence_fresh.rq ──────────────────────────────────────────

/// Clean case: no `ver:verifiedGraphHash` at all (never bound) — the gate's
/// `SELECT` finds no `?staleHash` row to compare, so sync stays green.
/// This establishes the two-sided proof's "passes on the clean input" half
/// before the sabotage half below.
#[test]
fn evidence_fresh_gate_passes_when_no_verified_hash_is_recorded() {
    let (_dir, project) = scaffold(&["alice"]);
    run_sync(&project).expect("sync with no ver:Check facts at all must stay green");
}

/// Sabotage: bind `ver:verifiedGraphHash` to a hash that can never equal
/// the latest real `ggenr:Sync` graphHash (a fixed bogus literal), then run
/// a second real sync (which reflexively loads the first sync's real
/// `ggenr:Sync` fact, including its real graphHash) — `030_evidence_fresh`
/// must refuse, naming itself and carrying its own MESSAGE.
#[test]
fn evidence_fresh_gate_refuses_stale_verified_hash_against_latest_sync() {
    let (_dir, project) = scaffold(&["alice"]);
    // First sync: plain domain facts, establishes a real ggenr:Sync fact
    // (with a real graphHash) in the reflexive log for the second sync to
    // reflexively load.
    run_sync(&project).expect("first sync (no evidence yet) must be green");

    // Second sync's ontology now carries a ver:Check bound to a
    // verifiedGraphHash that provably does not equal the first sync's real
    // graphHash (a fixed sentinel that is not valid BLAKE3 hex of anything
    // this pipeline could have produced).
    write_ontology_with_stale_check(
        &project,
        &["alice"],
        "0000000000000000000000000000000000000000000000000000000000000000stale",
    );
    let err = run_sync(&project)
        .expect_err("a verifiedGraphHash mismatched against the latest real sync must refuse");
    assert!(
        err.contains("030_evidence_fresh"),
        "refusal must come from the freshness gate: {err}"
    );
    assert!(
        err.contains("evidence is stale"),
        "refusal must carry the gate's own MESSAGE text: {err}"
    );
}

/// Two-sided close: binding `ver:verifiedGraphHash` to the CORRECT latest
/// graph hash (read back from the real sync report) passes -- proving the
/// gate compares real values, not merely refusing on the mere presence of
/// the predicate.
#[test]
fn evidence_fresh_gate_passes_when_verified_hash_matches_latest_sync() {
    let (_dir, project) = scaffold(&["alice"]);
    let first = run_sync(&project).expect("first sync must be green");

    write_ontology_with_stale_check(&project, &["alice"], &first.graph_hash_hex);
    run_sync(&project)
        .expect("a verifiedGraphHash that matches the latest real sync's graphHash must pass");
}

// ── Gate 040_latest_andon_green.rq ──────────────────────────────────────

/// Clean case: two ordinary real syncs, both admitted Green by the live
/// pipeline (per `sync.rs`'s own doc comment, every admission item is
/// honestly `Admitted` today, so andon is always derived Green) -- the
/// gate must not refuse a healthy chain.
#[test]
fn latest_andon_green_gate_passes_on_a_healthy_chain() {
    let (_dir, project) = scaffold(&["alice"]);
    run_sync(&project).expect("first sync must be green");
    run_sync(&project).expect("second sync over a healthy chain must stay green");
}

/// Sabotage: the live pipeline cannot itself produce a non-Green andon
/// today (see the module doc above), so this forges one the same way the
/// house sabotage pattern forges other external state -- by directly
/// editing the persisted, real `.ggen-v2/receipt-log.jsonl` file between
/// syncs. The forged entry is built from `praxis_core`'s own
/// `ReceiptRecord`/`Andon` types and its own `recompute_chain_hash`, so the
/// forged line is chain-valid (the next real sync's tamper check on
/// `prev_head` passes) and only the `andon` field differs from what the
/// pipeline would have produced -- exactly the "prior sync completed
/// non-Green" scenario `040_latest_andon_green.rq`'s own MESSAGE
/// describes.
#[test]
fn latest_andon_green_gate_refuses_a_forged_non_green_latest_sync() {
    let (_dir, project) = scaffold(&["alice"]);
    run_sync(&project).expect("first sync must be green");

    let lines = read_log_lines(&project);
    assert_eq!(lines.len(), 1, "one real sync so far");
    let mut receipt: SyncReceipt =
        serde_json::from_str(&lines[0]).expect("parse the real first receipt");

    // Flip andon to Halted (a real Andon variant, not a string hack) and
    // recompute the chain hash over the mutated record using praxis-core's
    // own recompute path, so this forged entry passes the next sync's
    // real chain-integrity check on `prev_head`.
    receipt.record.andon = Andon::Halted {
        unmet: Vec::new(),
        refusals: Vec::new(),
        at: 0,
    };
    let recomputed = receipt
        .record
        .recompute_chain_hash()
        .expect("recompute over the mutated record");
    receipt.record.chain_hash_hex = hex::encode(recomputed);
    receipt.record.prev_chain_hash_hex = "0".repeat(64);
    receipt.record.instruction_id = 2;

    let forged_line = serde_json::to_string(&receipt).expect("serialize forged receipt");
    let mut log = std::fs::read_to_string(project.join(RECEIPT_LOG_REL_PATH)).expect("read log");
    log.push_str(&forged_line);
    log.push('\n');
    std::fs::write(project.join(RECEIPT_LOG_REL_PATH), log).expect("append forged entry");

    let err = run_sync(&project)
        .expect_err("a latest sync with non-Green andon in the reflexive log must refuse");
    assert!(
        err.contains("040_latest_andon_green"),
        "refusal must come from the andon gate: {err}"
    );
    assert!(
        err.contains("andon is not Green"),
        "refusal must carry the gate's own MESSAGE text: {err}"
    );
}

// ── Gate 050_output_count_regression.rq ─────────────────────────────────

/// Clean case: output count grows between two real syncs (alice -> alice +
/// bob) -- must stay green.
#[test]
fn output_count_regression_gate_passes_when_output_count_grows() {
    let (_dir, project) = scaffold(&["alice"]);
    run_sync(&project).expect("first sync must be green");
    write_ontology(&project, &["alice", "bob"]);
    run_sync(&project).expect("a growing output count must stay green");
}

/// Sabotage: output count shrinks between two real syncs (alice + bob ->
/// alice), the exact bug class `050_output_count_regression.rq`'s own
/// MESSAGE names as "caught by hand twice this session" -- must refuse.
/// No forged facts: the shrink is a real consequence of removing a real
/// domain individual and its real template between real syncs.
///
/// This gate carries the identical one-sync lag `030_evidence_fresh.rq`
/// documents (both key off reflexive `ggenr:Sync` facts loaded from
/// `.ggen-v2/receipt-log.jsonl` at Stage-1 load time, BEFORE the current
/// sync's own line is appended in Stage 5 -- see `sync.rs`'s "P2:
/// reflexive receipts" comment): the current sync's own output count is
/// never in the graph it queries, only the two most recent ALREADY-WRITTEN
/// receipts are. So the shrinking sync itself (sync 2 below) is compared
/// against nothing yet (only one prior receipt exists) and passes; the
/// regression is only visible to a THIRD sync, which reflexively sees both
/// receipt 1 (4 outputs) and receipt 2 (3 outputs) as history.
#[test]
fn output_count_regression_gate_refuses_a_real_output_shrink() {
    let (_dir, project) = scaffold(&["alice", "bob"]);
    let first = run_sync(&project).expect("first sync (2 outputs) must be green");
    assert!(
        first.written.iter().any(|p| p.ends_with("out/alice.txt"))
            && first.written.iter().any(|p| p.ends_with("out/bob.txt")),
        "sanity: first sync really wrote both domain outputs: {:?}",
        first.written
    );

    write_ontology(&project, &["alice"]);
    let second = run_sync(&project)
        .expect("the shrinking sync itself passes -- the regression is one sync behind (lag)");
    assert!(
        !second.written.iter().any(|p| p.ends_with("out/bob.txt")),
        "sanity: the second sync really no longer writes bob's output: {:?}",
        second.written
    );

    // Third sync (ontology unchanged): now reflexively sees BOTH the
    // 4-output and 3-output historical receipts -- the regression fires.
    let err = run_sync(&project).expect_err(
        "the third sync, now seeing both historical receipts, must refuse the regression",
    );
    assert!(
        err.contains("050_output_count_regression"),
        "refusal must come from the output-count-regression gate: {err}"
    );
    assert!(
        err.contains("output-count regression"),
        "refusal must carry the gate's own MESSAGE text: {err}"
    );
}

/// Two-sided close on the escape hatch: recording `ver:AcceptedShrink`
/// against the SHRUNK sync's real `ver:chainHash` (read back from the real
/// receipt log, not a placeholder) makes the third sync's regression
/// refusal go away; naming the WRONG chain hash does not. Same three-sync
/// lag as above (see that test's doc comment).
#[test]
fn output_count_regression_gate_accepted_shrink_is_hash_specific_not_a_blanket_bypass() {
    let (_dir, project) = scaffold(&["alice", "bob"]);
    run_sync(&project).expect("first sync (2 outputs) must be green");

    write_ontology(&project, &["alice"]);
    run_sync(&project).expect("second (shrinking) sync passes -- regression is one sync behind");
    let shrunk_receipt: SyncReceipt =
        serde_json::from_str(read_log_lines(&project).last().expect("second log line"))
            .expect("parse second real receipt");
    let shrunk_chain_hash = shrunk_receipt.record.chain_hash_hex.clone();

    // WRONG hash first: still refuses.
    let mut ttl = std::fs::read_to_string(project.join("ontology.ttl")).expect("read ontology");
    ttl.push_str(&format!(
        "ver:accepted0 a ver:AcceptedShrink ; ver:chainHash \"{}\" .\n",
        "0".repeat(66)
    ));
    std::fs::write(project.join("ontology.ttl"), &ttl).expect("write ontology");
    let err = run_sync(&project)
        .expect_err("an AcceptedShrink naming the WRONG chainHash must still refuse");
    assert!(
        err.contains("050_output_count_regression"),
        "refusal must still come from the output-count-regression gate: {err}"
    );

    // CORRECT hash (the real shrunk sync's real chainHash): passes.
    let mut ttl = std::fs::read_to_string(project.join("ontology.ttl")).expect("read ontology");
    ttl = ttl.replace(&"0".repeat(66), &shrunk_chain_hash);
    std::fs::write(project.join("ontology.ttl"), &ttl).expect("write ontology");
    run_sync(&project)
        .expect("an AcceptedShrink naming the shrunk sync's REAL chainHash must be honored");
}
