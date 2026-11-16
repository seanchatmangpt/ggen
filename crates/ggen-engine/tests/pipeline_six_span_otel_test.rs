//! OTEL court for the six-stage sync pipeline: `pipeline.load`, `.extract`,
//! `.validate`, `.admit` (Stage 2c, ggen-abb-sbb), `.generate`, `.emit`.
//!
//! A real in-process `tracing::Subscriber` is installed around a real
//! `sync()` in a real `TempDir`. It only observes; every value was produced by
//! the production span code in `src/sync.rs`.
//!
//! Asserts: all six spans appear exactly once, in stage order; each recorded
//! `pipeline.duration_ms`; each had a non-zero real enter-to-exit wall time;
//! `pipeline.admit` carries `pipeline.stage=admit`, `gate.result=admitted`
//! and non-zero `pipeline.duration_us`. A refused admission (unqualified SBB)
//! yields a `pipeline.admit` span marked `otel.status_code=ERROR`,
//! `error=true`, `gate.result=refused`, and no later stage span appears.

#![allow(clippy::unwrap_used, clippy::expect_used, clippy::panic)]

use std::{
    collections::HashMap,
    path::Path,
    sync::{
        atomic::{AtomicU64, Ordering},
        Arc, Mutex,
    },
    time::{Duration, Instant},
};

use ggen_engine::sync::{sync, SyncOptions};
use tempfile::TempDir;
use tracing::{
    field::{Field, Visit},
    span::{Attributes, Id, Record},
    Event, Metadata, Subscriber,
};

const GGEN_TOML: &str = "[project]\nname = \"six-span\"\n\n[ontology]\nsource = \"ontology.ttl\"\n\n[templates]\ndir = \"templates\"\n";
const ONTOLOGY: &str = "@prefix ex: <http://example.org/> .\nex:subject a ex:Item .\n";

fn scaffold(root: &Path, ea: Option<&ggen_abb_sbb::EaGraph>) {
    std::fs::write(root.join("ggen.toml"), GGEN_TOML).unwrap();
    std::fs::write(root.join("ontology.ttl"), ONTOLOGY).unwrap();
    std::fs::create_dir_all(root.join("templates")).unwrap();
    std::fs::write(
        root.join("templates/main.tmpl"),
        "---\nto: out/result.txt\n---\nhello\n",
    )
    .unwrap();
    if let Some(g) = ea {
        std::fs::write(
            root.join("ea.graph.json"),
            serde_json::to_string_pretty(g).unwrap(),
        )
        .unwrap();
    }
}

#[derive(Default, Clone, Debug)]
struct SpanRec {
    name: String,
    attrs: HashMap<String, String>,
    entered: Option<Instant>,
    wall: Duration,
}

#[derive(Default)]
struct Ledger {
    /// Spans in creation order.
    spans: Vec<SpanRec>,
    by_id: HashMap<u64, usize>,
}

struct Capture {
    ledger: Arc<Mutex<Ledger>>,
    next: AtomicU64,
}

#[derive(Default)]
struct V(HashMap<String, String>);
impl Visit for V {
    fn record_str(&mut self, f: &Field, v: &str) {
        self.0.insert(f.name().into(), v.into());
    }
    fn record_u64(&mut self, f: &Field, v: u64) {
        self.0.insert(f.name().into(), v.to_string());
    }
    fn record_bool(&mut self, f: &Field, v: bool) {
        self.0.insert(f.name().into(), v.to_string());
    }
    fn record_debug(&mut self, f: &Field, v: &dyn std::fmt::Debug) {
        self.0.insert(f.name().into(), format!("{v:?}"));
    }
}

impl Subscriber for Capture {
    fn enabled(&self, m: &Metadata<'_>) -> bool {
        m.name().starts_with("pipeline.")
    }
    fn new_span(&self, a: &Attributes<'_>) -> Id {
        let id = self.next.fetch_add(1, Ordering::SeqCst);
        let mut v = V::default();
        a.record(&mut v);
        let mut l = self.ledger.lock().unwrap();
        let idx = l.spans.len();
        l.spans.push(SpanRec {
            name: a.metadata().name().into(),
            attrs: v.0,
            ..SpanRec::default()
        });
        l.by_id.insert(id, idx);
        Id::from_u64(id)
    }
    fn record(&self, s: &Id, values: &Record<'_>) {
        let mut v = V::default();
        values.record(&mut v);
        let mut l = self.ledger.lock().unwrap();
        if let Some(&i) = l.by_id.get(&s.into_u64()) {
            l.spans[i].attrs.extend(v.0);
        }
    }
    fn record_follows_from(&self, _: &Id, _: &Id) {}
    fn event(&self, _: &Event<'_>) {}
    fn enter(&self, s: &Id) {
        let mut l = self.ledger.lock().unwrap();
        if let Some(&i) = l.by_id.get(&s.into_u64()) {
            l.spans[i].entered = Some(Instant::now());
        }
    }
    fn exit(&self, s: &Id) {
        let mut l = self.ledger.lock().unwrap();
        if let Some(&i) = l.by_id.get(&s.into_u64()) {
            if let Some(t) = l.spans[i].entered.take() {
                l.spans[i].wall += t.elapsed();
            }
        }
    }
}

fn run(root: &Path) -> (Vec<SpanRec>, bool) {
    let ledger = Arc::new(Mutex::new(Ledger::default()));
    let sub = Capture {
        ledger: Arc::clone(&ledger),
        next: AtomicU64::new(1),
    };
    let ok = tracing::subscriber::with_default(sub, || {
        sync(root, SyncOptions::default()).is_ok()
    });
    let spans = ledger.lock().unwrap().spans.clone();
    (spans, ok)
}

#[test]
fn six_pipeline_spans_appear_in_order_with_real_durations() {
    let dir = TempDir::new().unwrap();
    scaffold(dir.path(), Some(&ggen_abb_sbb::synthetic_graph(1, 1)));
    let (spans, ok) = run(dir.path());
    assert!(ok, "qualified sync must succeed");

    let names: Vec<&str> = spans.iter().map(|s| s.name.as_str()).collect();
    assert_eq!(
        names,
        [
            "pipeline.load",
            "pipeline.extract",
            "pipeline.validate",
            "pipeline.admit",
            "pipeline.generate",
            "pipeline.emit"
        ],
        "spans must appear once each, in stage order"
    );
    for s in &spans {
        assert!(
            s.attrs.contains_key("pipeline.duration_ms"),
            "{} never recorded pipeline.duration_ms: {:?}",
            s.name,
            s.attrs
        );
        assert!(
            s.wall > Duration::ZERO,
            "{} has zero real wall time",
            s.name
        );
    }
    let admit = &spans[3];
    assert_eq!(admit.attrs["pipeline.stage"], "admit");
    assert_eq!(admit.attrs["gate.result"], "admitted");
    assert_eq!(admit.attrs["gate.name"], "abb_sbb_admission");
    let us: u64 = admit.attrs["pipeline.duration_us"].parse().unwrap();
    assert!(us > 0, "admit duration_us must be non-zero");
    assert!(!admit.attrs.contains_key("otel.status_code"));
}

#[test]
fn refused_admission_yields_error_marked_admit_span() {
    let dir = TempDir::new().unwrap();
    let mut g = ggen_abb_sbb::synthetic_graph(1, 1);
    g.qualifications.clear();
    scaffold(dir.path(), Some(&g));
    let (spans, ok) = run(dir.path());
    assert!(!ok, "unqualified SBB must refuse sync");

    let admit = spans
        .iter()
        .find(|s| s.name == "pipeline.admit")
        .expect("pipeline.admit span must exist on refusal");
    assert_eq!(admit.attrs.get("otel.status_code").map(String::as_str), Some("ERROR"));
    assert_eq!(admit.attrs.get("error").map(String::as_str), Some("true"));
    assert_eq!(admit.attrs.get("gate.result").map(String::as_str), Some("refused"));
    assert!(
        admit.attrs.get("error.message").is_some_and(|m| m.contains("admission refused")),
        "{:?}",
        admit.attrs
    );
    assert!(
        !spans.iter().any(|s| s.name == "pipeline.generate" || s.name == "pipeline.emit"),
        "no downstream stage may run after a refused admission"
    );
}
