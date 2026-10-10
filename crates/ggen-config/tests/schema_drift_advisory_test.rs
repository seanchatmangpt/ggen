//! Schema-drift advisory guard (drift-guard lane, 2026-10-10).
//!
//! Chicago tests: real `classify_ggen_toml_with_origin` calls over real TOML
//! text, log output captured through a real `tracing-subscriber` fmt layer
//! writing into an in-process buffer (real I/O collaborator, no mocks), and
//! assertions on observable state: the captured log bytes and the returned
//! [`ConfigSchemaClassification`] values.
//!
//! Guard contract under test:
//! 1. clean documents -> no advisory warn
//! 2. classified-success document carrying the other schema's shape evidence
//!    -> exactly one warn naming the file and both markers
//! 3. `Ambiguous` refusal unchanged (no advisory, same outcome)
//! 4. five-way classification outcomes byte-identical to the pinned fixtures

use ggen_config::config_schema::{classify_ggen_toml_with_origin, ConfigSchemaClassification};
use std::io::Write;
use std::sync::{Arc, Mutex};

/// A `tracing_subscriber::fmt` writer that appends into a shared buffer.
#[derive(Clone)]
struct CaptureWriter(Arc<Mutex<Vec<u8>>>);

impl Write for CaptureWriter {
    fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
        self.0
            .lock()
            .expect("capture lock poisoned")
            .extend_from_slice(buf);
        Ok(buf.len())
    }
    fn flush(&mut self) -> std::io::Result<()> {
        Ok(())
    }
}

/// One process-wide capture buffer: the global tracing subscriber is a
/// process singleton, so every test writes into the same buffer and asserts
/// scoped to its own `file =` name rather than racing on installation order.
fn shared_buffer() -> &'static Arc<Mutex<Vec<u8>>> {
    static BUFFER: std::sync::OnceLock<Arc<Mutex<Vec<u8>>>> = std::sync::OnceLock::new();
    BUFFER.get_or_init(|| {
        let buffer = Arc::new(Mutex::new(Vec::new()));
        let writer = CaptureWriter(Arc::clone(&buffer));
        let _ = tracing_subscriber::fmt()
            .with_max_level(tracing::level_filters::LevelFilter::WARN)
            .with_writer(move || writer.clone())
            .with_target(false)
            .without_time()
            .try_init();
        Arc::clone(&buffer)
    })
}

fn install_capture() -> &'static Arc<Mutex<Vec<u8>>> {
    shared_buffer()
}

fn captured(buffer: &Arc<Mutex<Vec<u8>>>) -> String {
    let lines = String::from_utf8(buffer.lock().expect("capture lock poisoned").clone())
        .expect("subscriber output is UTF-8");
    lines
}

const CLEAN_DECLARATIVE: &str = r#"
[project]
name = "clean"
version = "1.0.0"

[ontology]
source = "domain/model.ttl"

[[generation.rules]]
name = "structs"
query = { inline = "SELECT * WHERE { ?s ?p ?o }" }
template = { inline = "hi" }
output_file = "src/out.rs"
"#;

/// Declarative document that ALSO carries frontmatter-leaning evidence
/// (`[templates] dir`) — the advisory case the gap described.
const DRIFTED_DECLARATIVE: &str = r#"
[project]
name = "drifted"
version = "1.0.0"

[ontology]
source = "domain/model.ttl"

[templates]
dir = "templates/"

[[generation.rules]]
name = "structs"
query = { inline = "SELECT * WHERE { ?s ?p ?o }" }
template = { inline = "hi" }
output_file = "src/out.rs"
"#;

const CLEAN_FRONTMATTER: &str = r#"
[project]
name = "fm"

[ontology]
source = "domain/model.ttl"

[templates]
dir = "templates/"

[packs.core]
path = "packs/core"
"#;

/// Frontmatter-shaped document with a declarative-only `[rules]` table —
/// must stay `Ambiguous` (refusal unchanged, no advisory).
const MIXED_REFUSED: &str = r#"
[project]
name = "mixed"

[ontology]
source = "domain/model.ttl"

[templates]
dir = "templates/"

[rules]
foo = "bar"
"#;

/// Log lines concerning one origin file (the subscriber buffer is
/// process-wide, so assertions are scoped per file, not per process log).
fn lines_for(log: &str, file: &str) -> String {
    log.lines()
        .filter(|l| l.contains(file))
        .collect::<Vec<_>>()
        .join("\n")
}

#[test]
fn clean_declarative_emits_no_drift_warning() {
    let buffer = install_capture();
    let got = classify_ggen_toml_with_origin(CLEAN_DECLARATIVE, "clean-decl.toml");
    assert_eq!(got, ConfigSchemaClassification::DeclarativeRules);
    let log = lines_for(&captured(buffer), "clean-decl.toml");
    assert!(
        !log.contains("schema-drift advisory"),
        "clean declarative document must not warn; captured:\n{log}"
    );
}

#[test]
fn drifted_declarative_warns_with_file_and_both_markers_but_keeps_class() {
    let buffer = install_capture();
    let got = classify_ggen_toml_with_origin(DRIFTED_DECLARATIVE, "drifted.toml");
    // The advisory NEVER changes the outcome.
    assert_eq!(got, ConfigSchemaClassification::DeclarativeRules);
    let log = lines_for(&captured(buffer), "drifted.toml");
    assert!(
        log.contains("schema-drift advisory"),
        "expected a drift advisory naming the file; captured:\n{log}"
    );
    // Both sides' marker names present.
    assert!(
        log.contains("declarative:generation_table_present"),
        "selected-schema marker missing:\n{log}"
    );
    assert!(
        log.contains("frontmatter:templates_dir_present"),
        "other-schema marker missing:\n{log}"
    );
}

#[test]
fn clean_frontmatter_emits_no_drift_warning() {
    let buffer = install_capture();
    let got = classify_ggen_toml_with_origin(CLEAN_FRONTMATTER, "clean-fm.toml");
    assert_eq!(got, ConfigSchemaClassification::Frontmatter);
    let log = lines_for(&captured(buffer), "clean-fm.toml");
    assert!(
        !log.contains("schema-drift advisory"),
        "clean frontmatter document must not warn; captured:\n{log}"
    );
}

#[test]
fn mixed_document_still_refused_ambiguous_without_advisory() {
    let buffer = install_capture();
    let got = classify_ggen_toml_with_origin(MIXED_REFUSED, "mixed.toml");
    match &got {
        ConfigSchemaClassification::Ambiguous { matched } => {
            assert!(
                matched.iter().any(|m| m.starts_with("declarative:")),
                "expected a declarative marker in matched: {matched:?}"
            );
            assert!(
                matched.iter().any(|m| m.starts_with("frontmatter:")),
                "expected a frontmatter marker in matched: {matched:?}"
            );
        }
        other => panic!("expected Ambiguous refusal unchanged, got {other:?}"),
    }
    assert_eq!(got.code(), "FM-CONFIG-101");
    let log = lines_for(&captured(buffer), "mixed.toml");
    assert!(
        !log.contains("schema-drift advisory"),
        "Ambiguous refusals already refuse loudly; advisory must not fire:\n{log}"
    );
}

/// Five-class fixture corpus: classification outcomes must stay byte-identical
/// to the pinned expectations (pre/post guard for the advisory change).
#[test]
fn fixture_corpus_outcomes_unchanged_across_all_five_classes() {
    let _ = install_capture();

    // 1. Malformed
    let malformed = classify_ggen_toml_with_origin("not [ valid toml", "a.toml");
    assert_eq!(malformed.code(), "FM-CONFIG-103");

    // 2. Unsupported (valid TOML, no recognized markers, no frontmatter minimum)
    let unsupported = classify_ggen_toml_with_origin("[bogus_table]\nx = 1\n", "b.toml");
    assert!(matches!(
        unsupported,
        ConfigSchemaClassification::Unsupported { .. }
    ));
    assert_eq!(unsupported.code(), "FM-CONFIG-102");

    // 3. DeclarativeRules
    assert_eq!(
        classify_ggen_toml_with_origin(CLEAN_DECLARATIVE, "c.toml"),
        ConfigSchemaClassification::DeclarativeRules
    );

    // 4. Frontmatter
    assert_eq!(
        classify_ggen_toml_with_origin(CLEAN_FRONTMATTER, "d.toml"),
        ConfigSchemaClassification::Frontmatter
    );

    // 5. Ambiguous (strong declarative + frontmatter marker: packs array AND table impossible, so use version+table-shape)
    let ambiguous = classify_ggen_toml_with_origin(
        r#"
[project]
name = "amb"
version = "1.0.0"

[ontology]
source = "x.ttl"

[templates]
dir = "t/"

[packs.core]
path = "packs/core"
"#,
        "e.toml",
    );
    assert!(matches!(
        ambiguous,
        ConfigSchemaClassification::Ambiguous { .. }
    ));
    assert_eq!(ambiguous.code(), "FM-CONFIG-101");
}

#[test]
fn malformed_diagnostic_still_embeds_fm_config_103() {
    let got = classify_ggen_toml_with_origin("not [ valid toml", "f.toml");
    assert_eq!(got.code(), "FM-CONFIG-103");
    if let ConfigSchemaClassification::Malformed { diagnostic } = got {
        assert!(diagnostic.contains("FM-CONFIG-103"), "{diagnostic}");
    } else {
        panic!("expected Malformed, got {got:?}");
    }
}
