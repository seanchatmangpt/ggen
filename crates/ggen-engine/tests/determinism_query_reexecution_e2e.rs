//! C3 — `determinism: true` genuinely re-executes this template's declared
//! queries a second, independent time (Phase 10, closing the gap Phase 7
//! deferred: `check_determinism` used to re-render the SAME already-computed
//! query results against the identical, already-fixed `ctx` -- so it only
//! ever caught Tera-render nondeterminism, e.g. a stray `now()` call, never
//! query-execution nondeterminism -- a `sparql:` query that returns
//! different bindings or row order on a second, independent execution
//! against identical graph state would previously have sailed through
//! undetected).
//!
//! # Why this proves re-execution via a `tracing::Subscriber`, not via an
//! actually-differing query result
//!
//! oxigraph's in-memory store, as exposed through this crate's SPARQL
//! surface (`crate::graph::DeterministicGraph::query`), has no reachable
//! source of nondeterminism across two calls against an *unchanged* store:
//! no random-number or wall-clock SPARQL builtin is registered, ORDER BY
//! results are stably sorted, and `GRAPH`/`SERVICE` clauses are refused
//! outright (see `graph.rs`). Constructing a query whose *row bindings*
//! genuinely differ across two executions against the identical, unmutated
//! in-memory store is therefore not achievable through this crate's public
//! surface without literally patching randomness into the query evaluator —
//! which would be testing a fake evaluator, not this crate. This matches
//! what the investigation this phase is built on already expected: a real
//! adversarial nondeterministic-query fixture is not constructible here.
//!
//! What CAN be proven, and is proven below: [`check_determinism`]'s second
//! pass literally re-invokes the query-extraction code path a second,
//! independent time (not a second render against the first run's cached
//! results), if and only if `determinism: true` is set — never for a plain
//! template, and exactly once per template (not once per row) for a
//! per-row template. Production code (`crate::sync::extract_query_results`,
//! shared by the primary extraction and the determinism recheck) tags every
//! query it runs with a `tracing` event (`target:
//! "ggen_engine::sync::query_extraction"`, `phase: "primary"` |
//! `"determinism_recheck"`). A `tracing::Subscriber` installed for the
//! duration of the `sync()` call counts real events emitted by the real
//! pipeline, against a real on-disk project and a real oxigraph store —
//! the same class of technique `.claude/rules/otel-validation.md` already
//! mandates project-wide for proving an external call actually happened
//! (spans/events as proof, not a mock's call-expectation ledger): no
//! `GraphEngine` is faked, no query result is stubbed or pre-scripted, and
//! `sync()`'s own public API is exercised exactly as any real caller would
//! use it. This is instrumentation of real production code, not a new
//! mocking pattern — `sync.rs` already uses the identical
//! `tracing::info_span!`/`tracing::debug!` idiom for every other pipeline
//! stage in this file.

use std::{
    path::Path,
    sync::{Arc, Mutex},
};

use ggen_engine::sync::{sync, SyncOptions};
use tempfile::TempDir;
use tracing::{
    field::{Field, Visit},
    span::{Attributes, Id, Record},
    Event, Metadata, Subscriber,
};

const GGEN_TOML: &str = r#"
[project]
name = "determinism-reexec"

[ontology]
source = "ontology.ttl"

[templates]
dir = "templates"
"#;

const ONTOLOGY: &str = r#"
@prefix ex: <http://example.org/> .
ex:alice ex:name "alice" .
ex:bob   ex:name "bob" .
"#;

fn scaffold(root: &Path) {
    std::fs::write(root.join("ggen.toml"), GGEN_TOML).expect("write ggen.toml");
    std::fs::write(root.join("ontology.ttl"), ONTOLOGY).expect("write ontology");
    std::fs::create_dir_all(root.join("templates")).expect("mkdir templates");
}

fn write_template(root: &Path, name: &str, content: &str) {
    std::fs::write(root.join("templates").join(name), content).expect("write template");
}

/// Real counts of `ggen_engine::sync::query_extraction` events, keyed by
/// their `phase` field — populated exclusively from `tracing` events the
/// real production pipeline emitted during a real `sync()` call. Not a
/// mock's call-expectation ledger: nothing here asserts "the code must call
/// X" ahead of time the way `mockall::expect_*` does; this only counts what
/// genuinely happened.
#[derive(Default, Debug, Clone, Copy)]
struct PhaseCounts {
    primary: usize,
    determinism_recheck: usize,
    unrecognized: usize,
}

#[derive(Default)]
struct PhaseVisitor {
    phase: Option<String>,
}

impl Visit for PhaseVisitor {
    fn record_str(&mut self, field: &Field, value: &str) {
        if field.name() == "phase" {
            self.phase = Some(value.to_string());
        }
    }

    fn record_debug(&mut self, field: &Field, value: &dyn std::fmt::Debug) {
        if field.name() == "phase" && self.phase.is_none() {
            self.phase = Some(format!("{value:?}"));
        }
    }
}

const TARGET: &str = "ggen_engine::sync::query_extraction";

/// A real `tracing::Subscriber` — not a mock of `GraphEngine` or of any
/// application type. It observes; it never supplies a canned answer to the
/// code under test. Every count it accumulates corresponds to a genuine
/// `extract_query_results` call the real `sync()` pipeline made.
struct PhaseCountingSubscriber {
    counts: Arc<Mutex<PhaseCounts>>,
}

impl Subscriber for PhaseCountingSubscriber {
    fn enabled(&self, metadata: &Metadata<'_>) -> bool {
        metadata.target() == TARGET
    }

    fn new_span(&self, _span: &Attributes<'_>) -> Id {
        Id::from_u64(1)
    }

    fn record(&self, _span: &Id, _values: &Record<'_>) {}

    fn record_follows_from(&self, _span: &Id, _follows: &Id) {}

    fn event(&self, event: &Event<'_>) {
        if event.metadata().target() != TARGET {
            return;
        }
        let mut visitor = PhaseVisitor::default();
        event.record(&mut visitor);
        let mut counts = self.counts.lock().expect("lock");
        match visitor.phase.as_deref() {
            Some("primary") => counts.primary += 1,
            Some("determinism_recheck") => counts.determinism_recheck += 1,
            _ => counts.unrecognized += 1,
        }
    }

    fn enter(&self, _span: &Id) {}
    fn exit(&self, _span: &Id) {}
}

/// Run a real `sync()` against `root` with a `PhaseCountingSubscriber`
/// installed as the thread-local default dispatcher for the duration of the
/// call, then return the accumulated counts.
fn run_sync_counting_phases(root: &Path) -> PhaseCounts {
    let counts = Arc::new(Mutex::new(PhaseCounts::default()));
    let subscriber = PhaseCountingSubscriber {
        counts: Arc::clone(&counts),
    };
    tracing::subscriber::with_default(subscriber, || {
        sync(
            root,
            SyncOptions {
                dry_run: false,
                ..Default::default()
            },
        )
        .expect("sync must succeed")
    });
    let snapshot = *counts.lock().expect("lock");
    snapshot
}

/// `determinism: true` genuinely re-executes the template's named
/// `sparql:` query a second, independent time — not merely a second Tera
/// render against the first run's cached results. One named query, one
/// `sync()` call: exactly one `phase="primary"` event (the main extraction)
/// and exactly one `phase="determinism_recheck"` event (the recheck
/// `check_determinism` triggers) are expected — a real, counted
/// re-execution, not a description of what the code is supposed to do.
#[test]
fn determinism_true_reexecutes_the_named_sparql_query_a_second_time() {
    let dir = TempDir::new().expect("tempdir");
    scaffold(dir.path());
    write_template(
        dir.path(),
        "d.tmpl",
        "---\nto: out.txt\ndeterminism: true\nsparql:\n  people: SELECT ?name WHERE { ?s <http://example.org/name> ?name } ORDER BY ?name\n---\n{% for row in results %}{{ row.name }}{% endfor %}",
    );

    let counts = run_sync_counting_phases(dir.path());

    assert_eq!(
        counts.primary, 1,
        "exactly one primary extraction for one template with one named query: {counts:?}"
    );
    assert_eq!(
        counts.determinism_recheck, 1,
        "`determinism: true` must trigger exactly one independent second \
         extraction — zero would mean the recheck only re-rendered cached \
         results (the bug this phase closes), and more than one would mean \
         it re-ran per row instead of once per template: {counts:?}"
    );
    assert_eq!(counts.unrecognized, 0, "{counts:?}");
}

/// The default path (`determinism` unset in frontmatter) never triggers the
/// second, independent extraction — zero extra query cost for the common
/// case, proven by a real absence of any `determinism_recheck`-tagged
/// event, not by reading the source and trusting an `if` branch.
#[test]
fn determinism_absent_never_triggers_a_second_query_execution() {
    let dir = TempDir::new().expect("tempdir");
    scaffold(dir.path());
    write_template(
        dir.path(),
        "d.tmpl",
        "---\nto: out.txt\nsparql:\n  people: SELECT ?name WHERE { ?s <http://example.org/name> ?name } ORDER BY ?name\n---\n{% for row in results %}{{ row.name }}{% endfor %}",
    );

    let counts = run_sync_counting_phases(dir.path());

    assert_eq!(counts.primary, 1, "{counts:?}");
    assert_eq!(
        counts.determinism_recheck, 0,
        "no `determinism: true` in frontmatter — the second extraction must \
         never run: {counts:?}"
    );
}

/// `determinism: false` (explicit, not merely absent) behaves identically
/// to unset: still zero recheck executions.
#[test]
fn determinism_explicitly_false_never_triggers_a_second_query_execution() {
    let dir = TempDir::new().expect("tempdir");
    scaffold(dir.path());
    write_template(
        dir.path(),
        "d.tmpl",
        "---\nto: out.txt\ndeterminism: false\nsparql:\n  people: SELECT ?name WHERE { ?s <http://example.org/name> ?name } ORDER BY ?name\n---\n{% for row in results %}{{ row.name }}{% endfor %}",
    );

    let counts = run_sync_counting_phases(dir.path());

    assert_eq!(counts.primary, 1, "{counts:?}");
    assert_eq!(counts.determinism_recheck, 0, "{counts:?}");
}

/// Same proof for a per-row template (`to:` containing `{{`): the recheck
/// still runs exactly ONCE per template, not once per row. This is the
/// direct, run-based check that the implementation reuses one shared
/// `determinism_recheck` computed before the row loop (as documented on
/// `check_determinism`), rather than re-extracting per row.
#[test]
fn determinism_true_reexecutes_once_per_template_not_once_per_row() {
    let dir = TempDir::new().expect("tempdir");
    scaffold(dir.path());
    write_template(
        dir.path(),
        "d.tmpl",
        "---\nto: \"out/{{ row.name }}.txt\"\ndeterminism: true\nsparql:\n  people: SELECT ?name WHERE { ?s <http://example.org/name> ?name } ORDER BY ?name\n---\n{{ row.name }}",
    );

    let counts = run_sync_counting_phases(dir.path());

    // The ontology fixture has two rows (alice, bob); a naive per-row
    // re-extraction would show determinism_recheck == 2, not 1.
    assert_eq!(
        counts.primary, 1,
        "the primary extraction runs once per TEMPLATE, not once per row: {counts:?}"
    );
    assert_eq!(
        counts.determinism_recheck, 1,
        "the determinism recheck must also run exactly once per template \
         (reused across every row), not once per row: {counts:?}"
    );
}
