//! ARM 1: `ggen reverse scan` — code → discovered RDF authority graph.
//!
//! Walks the given roots, extracts service structure via the existing
//! [`crate::reverse_sync::ast_extractor`] extractors, builds a deterministic
//! discovered-authority graph (with provenance), writes it to
//! `.specify/discovered/<name>.ttl`, and emits an honest [`ReverseReceipt`]
//! binding every source file hash to the output hash.
//!
//! Unlike `convert_to_rdf`, this uses deterministic *named* nodes and records
//! provenance (`disco:sourceFile`/`disco:sourceHash`), so the output is
//! canonical and round-trips for ARM 3 template inference.

use std::path::{Path, PathBuf};

use walkdir::WalkDir;

use crate::reverse_sync::ast_extractor::{
    extract_elixir_genserver, extract_go_service, extract_rust_service, Language, ServiceDef,
};
use crate::utils::error::{Error, Result};

use super::events::{activity, obj_type, ReverseEvent};
use super::receipt::ReverseReceipt;
use super::turtle::{Object, TripleSet};
use super::vocab;

/// Provenance tag recorded on every discovered service.
const DISCOVERED_BY: &str = concat!("reverse-scan@", env!("CARGO_PKG_VERSION"));

/// Result of a `ggen reverse scan` run.
#[derive(Debug, Clone)]
pub struct ReverseScanReport {
    /// Path to the written authority graph.
    pub authority_ttl: PathBuf,
    /// Path to the written provenance receipt.
    pub receipt_path: PathBuf,
    /// Number of triples in the authority graph.
    pub triples: usize,
    /// Number of services discovered.
    pub services: usize,
    /// Number of source files that contributed services.
    pub files_scanned: usize,
    /// Neutral OCEL events for the CLI to log.
    pub events: Vec<ReverseEvent>,
}

/// Scan `roots` and emit a discovered authority graph named `<name>.ttl` under
/// `<project_root>/.specify/discovered/`, plus a receipt under
/// `<project_root>/.ggen/receipts/`.
///
/// Fails loudly (returns `Err`) if a root does not exist, if no roots are
/// given, or if no services are discovered — it never emits an empty graph.
pub fn scan_to_authority(
    roots: &[PathBuf],
    project_root: &Path,
    name: &str,
) -> Result<ReverseScanReport> {
    if roots.is_empty() {
        return Err(Error::new("reverse scan requires at least one root path"));
    }
    for root in roots {
        if !root.exists() {
            return Err(Error::new(&format!(
                "scan root does not exist: {}",
                root.display()
            )));
        }
    }

    let roots_label = roots
        .iter()
        .map(|p| p.display().to_string())
        .collect::<Vec<_>>()
        .join(",");

    let mut events = vec![ReverseEvent::new(
        activity::SCAN_STARTED,
        name,
        obj_type::DISCOVERED_GRAPH,
    )
    .with_attr("roots", roots_label.clone())];

    // Deterministic, sorted, de-duplicated file list across all roots.
    let mut files: Vec<PathBuf> = Vec::new();
    for root in roots {
        for entry in WalkDir::new(root).sort_by_file_name() {
            let entry = entry.map_err(|e| Error::new(&format!("walk error: {e}")))?;
            if entry.file_type().is_file() {
                files.push(entry.path().to_path_buf());
            }
        }
    }
    files.sort();
    files.dedup();

    let mut receipt = ReverseReceipt::new("reverse-scan");
    // (service, relative source path, blake3 content hash of that source file)
    let mut discovered: Vec<(ServiceDef, String, String)> = Vec::new();
    let mut files_scanned = 0usize;

    for path in &files {
        let language = match path.extension().and_then(|e| e.to_str()) {
            Some("rs") => Language::Rust,
            Some("ex") | Some("exs") => Language::Elixir,
            Some("go") => Language::Go,
            _ => continue,
        };
        let path_str = path.to_string_lossy().to_string();
        let services = match language {
            Language::Rust => extract_rust_service(&path_str)?,
            Language::Elixir => extract_elixir_genserver(&path_str)?,
            Language::Go => extract_go_service(&path_str)?,
        };
        if services.is_empty() {
            continue;
        }

        files_scanned += 1;
        let bytes =
            std::fs::read(path).map_err(|e| Error::new(&format!("read {}: {e}", path.display())))?;
        let rel = relative_path(project_root, path);
        let content_hash = blake3::hash(&bytes).to_hex().to_string();
        receipt.add_input(&format!("src:{rel}"), &bytes);
        events.push(
            ReverseEvent::new(activity::SCAN_FILE, &rel, obj_type::SOURCE_FILE)
                .with_attr("language", lang_str(language))
                .with_attr("services", services.len().to_string()),
        );
        for service in services {
            discovered.push((service, rel.clone(), content_hash.clone()));
        }
    }

    if discovered.is_empty() {
        return Err(Error::new(&format!(
            "no services discovered under {roots_label} (refusing to emit empty authority graph)"
        )));
    }

    let graph = build_graph(&discovered);
    let ttl = graph.to_turtle()?;

    let out_dir = project_root.join(".specify").join("discovered");
    std::fs::create_dir_all(&out_dir)
        .map_err(|e| Error::new(&format!("create discovered dir: {e}")))?;
    let authority_ttl = out_dir.join(format!("{}.ttl", sanitize_file(name)));
    std::fs::write(&authority_ttl, ttl.as_bytes())
        .map_err(|e| Error::new(&format!("write authority ttl: {e}")))?;

    let rel_ttl = relative_path(project_root, &authority_ttl);
    receipt.add_output(&rel_ttl, ttl.as_bytes());
    let receipt_path = project_root
        .join(".ggen")
        .join("receipts")
        .join(format!("reverse-scan-{}.json", receipt.operation_id));
    receipt.write_to(&receipt_path)?;

    events.push(
        ReverseEvent::new(activity::SCAN_COMPLETED, name, obj_type::DISCOVERED_GRAPH)
            .with_attr("services", discovered.len().to_string())
            .with_attr("triples", graph.len().to_string())
            .with_attr("files_scanned", files_scanned.to_string()),
    );
    events.push(
        ReverseEvent::new(
            activity::RECEIPT_EMITTED,
            &receipt.operation_id,
            obj_type::RECEIPT,
        )
        .with_attr("path", relative_path(project_root, &receipt_path)),
    );

    Ok(ReverseScanReport {
        authority_ttl,
        receipt_path,
        triples: graph.len(),
        services: discovered.len(),
        files_scanned,
        events,
    })
}

/// Build the deterministic discovered-authority graph from extracted services.
fn build_graph(discovered: &[(ServiceDef, String, String)]) -> TripleSet {
    let mut ts = TripleSet::new();
    ts.add_prefix(vocab::CODE, vocab::CODE_NS);
    ts.add_prefix(vocab::DISCO, vocab::DISCO_NS);
    ts.add_prefix(vocab::RDFS, vocab::RDFS_NS);

    for (service, rel, source_hash) in discovered {
        let local = sanitize_iri(&service.name);
        let subject = vocab::disco(&local);

        ts.insert(&subject, "a", Object::Iri(vocab::code("Service")));
        ts.insert(&subject, "a", Object::Iri(vocab::disco("DiscoveredService")));
        // The service name as a literal so ARM 3's inferred SELECT can bind `?name`.
        ts.insert(
            &subject,
            &vocab::disco("serviceName"),
            Object::Str(service.name.clone()),
        );
        ts.insert(
            &subject,
            &vocab::code("language"),
            Object::Str(lang_str(service.language).to_string()),
        );
        ts.insert(
            &subject,
            &vocab::disco("sourceFile"),
            Object::Str(rel.clone()),
        );
        ts.insert(
            &subject,
            &vocab::disco("sourceHash"),
            Object::Str(source_hash.clone()),
        );
        ts.insert(
            &subject,
            &vocab::disco("discoveredBy"),
            Object::Str(DISCOVERED_BY.to_string()),
        );

        for field in &service.fields {
            let field_local = format!("{local}__field__{}", sanitize_iri(&field.name));
            let field_subject = vocab::disco(&field_local);
            ts.insert(
                &subject,
                &vocab::code("hasField"),
                Object::Iri(field_subject.clone()),
            );
            ts.insert(&field_subject, "a", Object::Iri(vocab::code("Field")));
            ts.insert(
                &field_subject,
                &vocab::code("fieldName"),
                Object::Str(field.name.clone()),
            );
            ts.insert(
                &field_subject,
                &vocab::code("fieldType"),
                Object::Str(field.field_type.clone()),
            );
        }

        for method in &service.methods {
            let method_local = format!("{local}__method__{}", sanitize_iri(&method.name));
            let method_subject = vocab::disco(&method_local);
            ts.insert(
                &subject,
                &vocab::code("hasMethod"),
                Object::Iri(method_subject.clone()),
            );
            ts.insert(&method_subject, "a", Object::Iri(vocab::code("Method")));
            ts.insert(
                &method_subject,
                &vocab::code("methodName"),
                Object::Str(method.name.clone()),
            );
            if !method.params.is_empty() {
                ts.insert(
                    &method_subject,
                    &vocab::code("methodParams"),
                    Object::Str(method.params.join(", ")),
                );
            }
            if let Some(return_type) = &method.return_type {
                ts.insert(
                    &method_subject,
                    &vocab::code("returnType"),
                    Object::Str(return_type.clone()),
                );
            }
        }
    }
    ts
}

/// Map a string to characters safe inside an IRI local name.
fn sanitize_iri(input: &str) -> String {
    input
        .chars()
        .map(|c| match c {
            'a'..='z' | 'A'..='Z' | '0'..='9' | '_' => c,
            _ => '_',
        })
        .collect()
}

/// Map a string to characters safe inside a file name.
fn sanitize_file(input: &str) -> String {
    input
        .chars()
        .map(|c| match c {
            'a'..='z' | 'A'..='Z' | '0'..='9' | '_' | '-' => c,
            _ => '_',
        })
        .collect()
}

fn lang_str(language: Language) -> &'static str {
    match language {
        Language::Rust => "rust",
        Language::Elixir => "elixir",
        Language::Go => "go",
    }
}

/// Render `path` relative to `root` with forward slashes, falling back to the
/// full path if it is not under `root`.
fn relative_path(root: &Path, path: &Path) -> String {
    path.strip_prefix(root)
        .unwrap_or(path)
        .to_string_lossy()
        .replace('\\', "/")
}
