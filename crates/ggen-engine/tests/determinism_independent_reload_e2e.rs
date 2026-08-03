//! `determinism: true`'s recheck must be a genuinely independent second
//! load of the ontology (a fresh `GraphEngine` instance, Turtle re-read and
//! re-parsed from disk), not a second query against the SAME already-loaded
//! store.
//!
//! # The bug this closes
//!
//! Before this fix, `check_determinism`'s recheck called
//! `extract_query_results` a second time against `active_graph` — the exact
//! same in-memory `GraphEngine` instance the primary extraction had already
//! used. That recheck genuinely re-executes the query (proven by
//! `determinism_query_reexecution_e2e.rs`), so it CAN catch
//! query-execution nondeterminism *within one already-loaded store*. But
//! because both "runs" saw the identical in-memory graph rather than two
//! independent loads, it was structurally incapable of catching load-time
//! nondeterminism: any two queries against the SAME store trivially agree
//! with themselves, no matter how that store's own state happened to be
//! seeded at load time.
//!
//! # Two dead ends this test's construction ruled out first (evidence, not
//! assumption)
//!
//! `determinism_query_reexecution_e2e.rs`'s own doc comment already
//! establishes that oxigraph has no reachable nondeterminism *within one
//! unchanged store* -- ruling out "same query, same store, twice" as a
//! source. That leaves load-time (across-store) nondeterminism as this
//! test's target, and the obvious candidate is blank-node relabeling. Two
//! attempts at that candidate were tried and empirically refuted before
//! this fixture, by direct experiment (ad hoc probes, not shipped):
//!
//! 1. A query directly returning a blank node's own label does NOT
//!    reproduce: `GraphEngine::query`'s `impl` for `DeterministicGraph`
//!    relabels every bound blank node through `blank_node_relabel_map`'s
//!    canonical `c14n{i}` scheme before it ever reaches a template.
//! 2. `ORDER BY` on a blank-node-typed variable does NOT reproduce either,
//!    even bypassing that projection-time relabeling: `graph.rs`'s
//!    `DeterministicGraph::canonicalize_blank_nodes` (called from EVERY
//!    mutation path, including `insert_turtle_documents` -- see
//!    `graph/ontology_batch.rs` -- specifically so that "a query whose
//!    `ORDER BY`/`GROUP BY`/`DISTINCT` compares a blank-node-valued
//!    variable" is not exploitable, per that function's own doc comment)
//!    rewrites the STORE ITSELF to stable `c14n{i}` labels immediately
//!    after every load, before any query ever runs. `ORDER BY ?blank_var`
//!    then sorts those stable label strings, which is observably identical
//!    across independent loads.
//!
//! # The real, empirically-confirmed source: ties under `LIMIT` with no
//! `ORDER BY`
//!
//! Canonicalization fixes the *label* a physical blank node receives, but
//! it cannot fix *which* physical node an unordered `LIMIT` keeps when two
//! blank nodes are genuinely indistinguishable (identical predicate/object
//! pattern, no other differentiating triple) -- bounded color refinement
//! (`canonical_blank_node_map`, `graph.rs`) cannot separate true structural
//! twins, and falls back to comparing their raw, freshly-`rand::random()`
//! parser-assigned ids (`canonical_blank_node_map`'s doc comment: "ties
//! broken by original label"). Confirmed by direct experiment (ad hoc
//! probe, not shipped, single-threaded to rule out output interleaving):
//! `SELECT ?s WHERE { ?s ex:type ex:Widget } LIMIT 1` (no `ORDER BY`) over
//! two structurally-identical blank nodes returned `_:c14n0` 29 times and
//! `_:c14n1` 31 times across 60 independent loads of byte-identical
//! Turtle -- a genuine, real, ~50/50 coin flip, not a hypothesis. This is
//! exactly "an unordered `LIMIT` over a graph with ties."
//!
//! # Why this test retries instead of asserting a single run
//!
//! The nondeterminism is a real ~50/50 coin flip, so a single `sync()`
//! call catches it only about half the time. Each `sync()` call already
//! performs two independent loads internally (the primary `graph` in Stage
//! 1, and the recheck's `build_independent_recheck_base_graph`), so this
//! loop's iterations are independent Bernoulli trials of that SAME
//! single-call coin flip. Retrying up to [`MAX_ATTEMPTS`] drives the
//! chance of never observing a violation below 2^-`MAX_ATTEMPTS`, while
//! still failing loudly (not silently) if the fix regresses and the
//! recheck goes back to reusing one store (in which case the coin flip
//! always lands the same way: `Ok` on every single attempt, forever).

#![allow(clippy::unwrap_used, clippy::expect_used, clippy::panic)]

use std::path::Path;

use ggen_engine::sync::{sync, SyncOptions};
use tempfile::TempDir;

const GGEN_TOML: &str = r#"
[project]
name = "determinism-independent-reload"

[ontology]
source = "ontology.ttl"

[templates]
dir = "templates"
"#;

/// Two blank nodes with an IDENTICAL predicate/object pattern and no other
/// triple -- true structural twins. Bounded color refinement cannot
/// distinguish them (see module doc comment), so which one is `_:c14n0`
/// vs `_:c14n1` is decided by comparing their raw, freshly-randomized
/// parser-assigned ids -- different on every independent load.
const ONTOLOGY: &str = r"
@prefix ex: <http://example.org/> .
_:a ex:type ex:Widget .
_:b ex:type ex:Widget .
";

/// `determinism: true` over an UNORDERED `LIMIT 1` query (deliberately no
/// `ORDER BY`): with two structural twins, which one survives the `LIMIT`
/// depends on load-seeded blank-node identity (see module doc comment), so
/// the rendered `{{ row.s }}` genuinely differs across two independent
/// loads of this identical source text roughly half the time.
const TEMPLATE: &str = "---\nto: out.txt\ndeterminism: true\nsparql:\n  entities: |\n    SELECT ?s WHERE { ?s <http://example.org/type> <http://example.org/Widget> }\n    LIMIT 1\n---\n{% for row in results %}{{ row.s }}{% endfor %}";

fn scaffold(root: &Path) {
    std::fs::write(root.join("ggen.toml"), GGEN_TOML).expect("write ggen.toml");
    std::fs::write(root.join("ontology.ttl"), ONTOLOGY).expect("write ontology");
    std::fs::create_dir_all(root.join("templates")).expect("mkdir templates");
    std::fs::write(root.join("templates/blank.tmpl"), TEMPLATE).expect("write template");
}

/// Upper bound on retries. Each attempt is an independent ~50/50 coin flip
/// (see module doc comment; empirically confirmed 29/31 over 60 trials at
/// the raw-graph level), so failing to observe a violation across this
/// many attempts is a ~2^-300 event for a working fix -- and a certainty
/// (never fires, `Ok` every time) if the recheck regresses to reusing one
/// store.
const MAX_ATTEMPTS: usize = 300;

/// `determinism: true` over a query whose `LIMIT 1` winner depends on
/// load-seeded blank-node identity must eventually be refused: the
/// recheck's independently-reloaded graph has a real, empirically-verified
/// chance of keeping the OTHER structural twin than the primary load did,
/// so the second render's output bytes genuinely differ from the first's.
///
/// Before this fix this exact fixture synced successfully on every single
/// attempt (silently unsound `determinism: true`): the recheck re-queried
/// the SAME already-loaded store, which still held whichever twin the
/// primary extraction's `LIMIT 1` had already kept, so `second_render ==
/// first_render` trivially, every time, regardless of the fixture's real
/// nondeterminism.
#[test]
fn determinism_true_catches_an_unordered_limit_tie_across_an_independent_reload() {
    let mut saw_violation = false;
    let mut saw_success = false;

    for _ in 0..MAX_ATTEMPTS {
        let dir = TempDir::new().expect("tempdir");
        scaffold(dir.path());

        match sync(dir.path(), SyncOptions::default()) {
            Err(error) => {
                let message = error.to_string();
                assert!(
                    message.contains("[FM-TPL-009]") && message.contains("determinism: true"),
                    "expected a determinism violation, got a different error: {message}"
                );
                assert!(
                    message.contains("different output bytes"),
                    "expected the output-bytes variant of the determinism violation \
                     (the LIMIT-1 winner differs between the two independent loads), \
                     got: {message}"
                );
                assert!(
                    !dir.path().join("out.txt").exists(),
                    "render happens fully before any write in this pipeline"
                );
                saw_violation = true;
            }
            Ok(report) => {
                assert_eq!(report.written, vec![std::path::PathBuf::from("out.txt")]);
                saw_success = true;
            }
        }

        if saw_violation && saw_success {
            break;
        }
    }

    assert!(
        saw_violation,
        "across {MAX_ATTEMPTS} independent attempts, `determinism: true` never fired -- \
         the recheck is not genuinely reloading independently (this is the exact \
         regression this test exists to catch)"
    );
    assert!(
        saw_success,
        "across {MAX_ATTEMPTS} independent attempts, every sync failed -- that is a \
         different bug (the fixture itself, or an over-eager refusal), not the \
         load-independence property this test targets"
    );
}

/// Control: the identical fixture WITHOUT `determinism: true` is a
/// perfectly ordinary, successfully-syncing project on every attempt --
/// the blank-node ties and the `LIMIT 1` query are not what is being
/// refused; only the `determinism: true` recheck's cross-load comparison
/// is.
#[test]
fn same_blank_node_tie_fixture_without_determinism_true_always_syncs_successfully() {
    for _ in 0..25 {
        let dir = TempDir::new().expect("tempdir");
        scaffold(dir.path());
        let template_no_determinism = TEMPLATE.replacen("determinism: true\n", "", 1);
        std::fs::write(
            dir.path().join("templates/blank.tmpl"),
            template_no_determinism,
        )
        .expect("rewrite template");

        let report =
            sync(dir.path(), SyncOptions::default()).expect("sync without determinism check");
        assert_eq!(
            report.written,
            vec![std::path::PathBuf::from("out.txt")],
            "the fixture itself is a normal, syncable project regardless of which \
             structural twin the `LIMIT 1` query happens to keep"
        );
        assert!(dir.path().join("out.txt").exists());
    }
}
