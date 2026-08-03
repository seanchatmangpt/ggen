//! Self-play regression suite.
//!
//! Two independent guarantees, both deterministic and both offline:
//!
//! 1. **Corpus replay** — every case in `tests/corpus/` is played against
//!    its pack and the referee must rule it clean. These are the cases that
//!    have historically tripped an invariant (plus lawful baselines), so a
//!    regression here is a real regression.
//! 2. **Whole-corpus sweep** — every pack in `packs/` that ships an
//!    ontology is driven through the full lifecycle with a generic probe.
//!    Before this existed, 5 of 78 packs had any lifecycle proof; this
//!    covers all of them.
//!
//! No LLM, no network, no GPU. Gemma's role is to *grow* `tests/corpus/`
//! offline (see `ggen-selfplay-explore`); it is never in the assertion path.

use std::path::{Path, PathBuf};

use ggen_mcp::selfplay::{Board, Case};

fn repo_root() -> PathBuf {
    // CARGO_MANIFEST_DIR = <repo>/crates/ggen-mcp
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .and_then(Path::parent)
        .expect("repo root")
        .to_path_buf()
}

fn packs_with_ontology() -> Vec<PathBuf> {
    let packs = repo_root().join("packs");
    let mut out: Vec<PathBuf> = std::fs::read_dir(&packs)
        .expect("packs/ must exist")
        .flatten()
        .map(|e| e.path())
        .filter(|p| p.is_dir() && p.join("ontology.ttl").is_file())
        .collect();
    out.sort();
    out
}

fn load_corpus() -> Vec<Case> {
    let dir = Path::new(env!("CARGO_MANIFEST_DIR")).join("tests/corpus");
    let mut cases: Vec<Case> = std::fs::read_dir(&dir)
        .expect("tests/corpus must exist")
        .flatten()
        .map(|e| e.path())
        .filter(|p| p.extension().is_some_and(|x| x == "json"))
        .map(|p| {
            let raw =
                std::fs::read_to_string(&p).unwrap_or_else(|e| panic!("read {}: {e}", p.display()));
            serde_json::from_str::<Case>(&raw)
                .unwrap_or_else(|e| panic!("parse {}: {e}", p.display()))
        })
        .collect();
    cases.sort_by(|a, b| a.id.cmp(&b.id));
    cases
}

/// The corpus is not empty and every case names a pack that exists. A
/// corpus entry pointing at a deleted pack would silently stop testing
/// anything, which is the quiet failure this check exists to prevent.
#[test]
fn corpus_is_present_and_well_formed() {
    let cases = load_corpus();
    assert!(
        !cases.is_empty(),
        "tests/corpus must contain at least one case"
    );
    for c in &cases {
        let dir = repo_root().join("packs").join(&c.pack);
        assert!(
            dir.join("ontology.ttl").is_file(),
            "case {:?} names pack {:?}, which has no ontology.ttl",
            c.id,
            c.pack
        );
        assert!(
            !c.sparql.trim().is_empty(),
            "case {:?} has empty sparql",
            c.id
        );
        assert!(!c.to.trim().is_empty(), "case {:?} has empty to:", c.id);
    }
}

/// Replay the committed corpus. Every case must be ruled clean by the
/// referee — including the adversarial ones, whose whole point is that ggen
/// *handles* them lawfully (refusing loudly, counting honestly, staying
/// inside the root) rather than that they succeed.
#[test]
fn corpus_replays_clean() {
    let mut failures = Vec::new();
    for case in load_corpus() {
        let pack_dir = repo_root().join("packs").join(&case.pack);
        let board = match Board::new(&pack_dir) {
            Ok(b) => b,
            Err(e) => {
                failures.push(format!("[{}] board setup failed: {e}", case.id));
                continue;
            }
        };
        let (verdict, obs) = board.play(&case);
        if !verdict.clean() {
            for v in &verdict.violations {
                failures.push(format!(
                    "[{}] pack={} {:?}\n      invariant: {}\n      observed : {}\n      \
                     note     : {}",
                    case.id,
                    case.pack,
                    v.invariant,
                    v.invariant.statement(),
                    v.observed,
                    case.note.as_deref().unwrap_or("(none)")
                ));
            }
            eprintln!("[{}] observations: {obs:?}", case.id);
        }
    }
    assert!(
        failures.is_empty(),
        "{} corpus case(s) violated an invariant:\n\n{}",
        failures.len(),
        failures.join("\n\n")
    );
}

/// Drive EVERY pack that ships an ontology through the full lifecycle.
///
/// The probe is deliberately generic (`?s ?p ?o`, bounded) so this test is
/// about the *pack* and the *engine*, not about any clever query: does this
/// pack's ontology load, can it be queried, does a template render against
/// it, does the write land, does the receipt verify, is the re-sync a no-op.
///
/// A pack whose ontology is intentionally empty (several ship a comment-only
/// `ontology.ttl` to satisfy FM-PACK-004 while shipping templates only) is
/// still valid input: zero rows is a lawful answer, and the referee treats
/// it as such.
#[test]
fn every_pack_survives_the_full_lifecycle() {
    let packs = packs_with_ontology();
    assert!(
        packs.len() > 60,
        "expected the full pack corpus, found {}",
        packs.len()
    );

    let mut failures = Vec::new();
    let mut played = 0usize;
    for pack_dir in &packs {
        let name = pack_dir
            .file_name()
            .unwrap_or_default()
            .to_string_lossy()
            .to_string();
        let case = Case {
            id: format!("sweep-{name}"),
            pack: name.clone(),
            sparql: "SELECT ?s ?p ?o WHERE { ?s ?p ?o } ORDER BY ?s ?p ?o LIMIT 3".to_string(),
            to: "out/sweep.txt".to_string(),
            body: "{% for row in probe %}{{ row.s }}\n{% endfor %}".to_string(),
            origin: ggen_mcp::selfplay::CaseOrigin::Handwritten,
            expected_violation: None,
            note: Some("generic full-lifecycle sweep".to_string()),
        };
        let board = match Board::new(pack_dir) {
            Ok(b) => b,
            Err(e) => {
                failures.push(format!("[{name}] board setup failed: {e}"));
                continue;
            }
        };
        played += 1;
        let (verdict, obs) = board.play(&case);
        for v in &verdict.violations {
            failures.push(format!(
                "[{name}] {:?}\n      invariant: {}\n      observed : {}",
                v.invariant,
                v.invariant.statement(),
                v.observed
            ));
            eprintln!("[{name}] observations: {obs:?}");
        }
    }

    eprintln!("self-play sweep: played {played} pack(s)");
    assert!(
        failures.is_empty(),
        "{} pack(s) violated an invariant during the full lifecycle:\n\n{}",
        failures.len(),
        failures.join("\n\n")
    );
}
