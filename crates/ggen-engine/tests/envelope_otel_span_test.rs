//! OTEL attribute court for the `envelope.iri` field on the
//! `pipeline.validate` span (v26.10.20 lane A05) — per
//! `.claude/rules/otel-validation.md`: no spans, no claim; a span present
//! with a narrated-but-unrecorded attribute is still narration.
//!
//! # What this court pins
//!
//! 1. a real `sync()` emits the `pipeline.validate` span, and
//! 2. that span RECORDS (not merely declares) `envelope.iri`:
//!    - the declared envelope IRI when a template frontmatter binds one
//!      (against a real `.specify/envelope-shapes.ttl` reticle), and
//!    - the literal string `"NONE"` when no envelope exists.
//!
//! # Harness: in-process `tracing::Subscriber` around a real `sync()` call
//!
//! Same technique as `otel_span_presence_test.rs`: a real subscriber is
//! installed as the thread-local default dispatcher for one real sync
//! against a real on-disk tempdir project. The subscriber only observes —
//! every recorded value came from the production
//! `validate_span.record("envelope.iri", …)` call in
//! `crates/ggen-engine/src/sync.rs`.
//!
//! # Non-vacuity (mutation killed)
//!
//! Deleting the single line
//! `validate_span.record("envelope.iri", envelope_iri.as_deref().unwrap_or("NONE"))`
//! in `sync.rs` (the `tracing::field::Empty` declaration alone is NOT
//! recorded as a value, so `record_str` never fires) makes BOTH tests below
//! fail with "never recorded". Verified by temporary mutation + restore;
//! source must be byte-identical after (sha256 receipt in lane report).

#![allow(clippy::unwrap_used, clippy::expect_used, clippy::panic)]

use std::{
    collections::HashMap,
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
name = "envelope-otel-span"

[ontology]
source = "ontology.ttl"

[templates]
dir = "templates"
"#;

const ONTOLOGY: &str = r#"@prefix ex: <http://example.org/> .
ex:alice ex:name "alice" .
"#;

const ENV_IRI: &str = "res:EnvObserved";

fn scaffold(root: &Path, envelope: Option<&str>) {
    std::fs::write(root.join("ggen.toml"), GGEN_TOML).expect("write ggen.toml");
    std::fs::write(root.join("ontology.ttl"), ONTOLOGY).expect("write ontology");
    std::fs::create_dir_all(root.join("templates")).expect("mkdir templates");
    let envelope_line = envelope
        .map(|iri| format!("envelope: {iri}\n"))
        .unwrap_or_default();
    std::fs::write(
        root.join("templates/main.tmpl"),
        format!("---\nto: out/result.txt\n{envelope_line}---\nhello\n"),
    )
    .expect("write template");
    if envelope.is_some() {
        // Declared (non-tightening) envelope reticle: the RDF sole source
        // of truth lane I1 established. Declaring the envelope as a subject
        // keeps the sync admitted while binding the frontmatter IRI.
        let path = root.join(".specify/envelope-shapes.ttl");
        std::fs::create_dir_all(path.parent().unwrap()).expect("mkdir .specify");
        std::fs::write(
            &path,
            format!("@prefix res: <http://ggen.org/envelope#> .\n<{ENV_IRI}> a res:Envelope .\n"),
        )
        .expect("write envelope reticle");
    }
}

/// Observed `envelope.iri` values recorded on `pipeline.validate` spans,
/// populated exclusively by the real pipeline's `span.record` calls.
#[derive(Default)]
struct SpanLedger {
    validate_spans_seen: usize,
    envelope_iri: Vec<String>,
}

/// Records the span name for each id so `record` can attribute fields to
/// the `pipeline.validate` span specifically (ids are unique here, unlike
/// the single-id presence test — we must attribute per-span).
struct EnvelopeIriSubscriber {
    ledger: Arc<Mutex<SpanLedger>>,
    next_id: std::sync::atomic::AtomicU64,
    span_names: Mutex<HashMap<Id, String>>,
}

impl EnvelopeIriSubscriber {
    fn is_validate_span(metadata: &Metadata<'_>) -> bool {
        metadata.name() == "pipeline.validate"
    }
}

impl Subscriber for EnvelopeIriSubscriber {
    fn enabled(&self, metadata: &Metadata<'_>) -> bool {
        Self::is_validate_span(metadata)
    }

    fn new_span(&self, span: &Attributes<'_>) -> Id {
        let id = Id::from_u64(
            self.next_id
                .fetch_add(1, std::sync::atomic::Ordering::SeqCst),
        );
        if Self::is_validate_span(span.metadata()) {
            self.span_names
                .lock()
                .expect("lock")
                .insert(id.clone(), span.metadata().name().to_string());
            self.ledger.lock().expect("lock").validate_spans_seen += 1;
        }
        id
    }

    fn record(&self, span: &Id, values: &Record<'_>) {
        let is_validate = self
            .span_names
            .lock()
            .expect("lock")
            .get(span)
            .is_some_and(|n| n == "pipeline.validate");
        if !is_validate {
            return;
        }
        let mut visitor = EnvelopeIriVisitor::default();
        values.record(&mut visitor);
        if let Some(iri) = visitor.envelope_iri {
            self.ledger.lock().expect("lock").envelope_iri.push(iri);
        }
    }

    fn record_follows_from(&self, _span: &Id, _follows: &Id) {}
    fn event(&self, _event: &Event<'_>) {}
    fn enter(&self, _span: &Id) {}
    fn exit(&self, _span: &Id) {}
}

#[derive(Default)]
struct EnvelopeIriVisitor {
    envelope_iri: Option<String>,
}

impl Visit for EnvelopeIriVisitor {
    fn record_str(&mut self, field: &Field, value: &str) {
        if field.name() == "envelope.iri" {
            self.envelope_iri = Some(value.to_string());
        }
    }
    fn record_debug(&mut self, field: &Field, value: &dyn std::fmt::Debug) {
        if field.name() == "envelope.iri" {
            self.envelope_iri = Some(format!("{value:?}"));
        }
    }
    fn record_f64(&mut self, _field: &Field, _value: f64) {}
    fn record_i64(&mut self, _field: &Field, _value: i64) {}
    fn record_u64(&mut self, _field: &Field, _value: u64) {}
    fn record_bool(&mut self, _field: &Field, _value: bool) {}
}

/// Run a real `sync()` against `root` with the capturing subscriber
/// installed; returns what the real pipeline emitted.
fn run_sync_collecting_envelope_iri(root: &Path) -> SpanLedger {
    let ledger = Arc::new(Mutex::new(SpanLedger::default()));
    let subscriber = EnvelopeIriSubscriber {
        ledger: Arc::clone(&ledger),
        next_id: std::sync::atomic::AtomicU64::new(1),
        span_names: Mutex::new(HashMap::new()),
    };
    tracing::subscriber::with_default(subscriber, || {
        sync(root, SyncOptions::default()).expect("sync must succeed")
    });
    let guard = ledger.lock().expect("lock");
    SpanLedger {
        validate_spans_seen: guard.validate_spans_seen,
        envelope_iri: guard.envelope_iri.clone(),
    }
}

/// A real sync with a frontmatter-bound envelope records the DECLARED
/// envelope IRI on the `pipeline.validate` span. Kills the mutation
/// "remove the `validate_span.record("envelope.iri", …)` call" — with the
/// attribute only declared `Empty` and never recorded, this assertion
/// fails on the empty `envelope_iri` vec.
#[test]
fn validate_span_records_declared_envelope_iri_on_a_real_sync() {
    let dir = TempDir::new().expect("tempdir");
    let root = dir.path();
    scaffold(root, Some(ENV_IRI));

    let ledger = run_sync_collecting_envelope_iri(root);

    assert_eq!(
        ledger.validate_spans_seen, 1,
        "expected exactly one `pipeline.validate` span per sync"
    );
    // `envelope: res:EnvObserved` in the frontmatter binds the IRI; the
    // span must carry the same IRI the template declared.
    assert_eq!(
        ledger.envelope_iri,
        vec![ENV_IRI.to_string()],
        "`pipeline.validate` must record `envelope.iri` = the declared \
         envelope IRI; empty vec means the attribute was never recorded \
         (mutation: record call removed)"
    );
}

/// A real sync with NO envelope anywhere records the literal string
/// `"NONE"` on the `pipeline.validate` span — the sentinel, not a missing
/// attribute, is the no-envelope contract.
#[test]
fn validate_span_records_literal_none_when_no_envelope_exists() {
    let dir = TempDir::new().expect("tempdir");
    let root = dir.path();
    scaffold(root, None);

    let ledger = run_sync_collecting_envelope_iri(root);

    assert_eq!(
        ledger.validate_spans_seen, 1,
        "expected exactly one `pipeline.validate` span per sync"
    );
    assert_eq!(
        ledger.envelope_iri,
        vec!["NONE".to_string()],
        "`pipeline.validate` must record `envelope.iri` = \"NONE\" when no \
         envelope is bound; empty vec means the attribute was never \
         recorded (mutation: record call removed)"
    );
}
