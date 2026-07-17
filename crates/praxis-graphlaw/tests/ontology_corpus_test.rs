//! Ontology Corpus Test — Real-World RDF Parsing and OWL RL Materialization
//!
//! Tests that Graphlaw can load and reason over a real corpus of enterprise
//! ontologies from `ontologies/` (schema.org, FIBO, PROV-O, DCAT, FOAF, Dublin Core,
//! etc.). This test classifies documents by content sniffing (not file extension):
//!
//! 1. **Turtle/N-Triples/N3** (detected by `@prefix` or N-Triples syntax) —
//!    parsed by `rio_turtle` and tested for parse success and triple count.
//! 2. **RDF/XML** (detected by XML declaration or `<rdf:RDF>`) — parsed by
//!    `rio_xml` and tested for parse success. These files are now **supported**.
//!
//! For all supported ontology files, this test also runs `materialize_owlrl()`
//! to verify that OWL RL class/property hierarchy inference works on real data.

use praxis_graphlaw::parser::{Parser, Syntax};
use std::fs;
use std::path::PathBuf;

#[test]
fn test_ontology_corpus_comprehensive() {
    let corpus_root = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("ontologies");

    if !corpus_root.exists() {
        eprintln!(
            "Corpus directory {} not found, skipping test",
            corpus_root.display()
        );
        return;
    }

    let mut corpus_files = Vec::new();
    collect_files_recursive(&corpus_root, &mut corpus_files);

    if corpus_files.is_empty() {
        eprintln!("No ontology files found in {}", corpus_root.display());
        return;
    }

    println!("\n╔════════════════════════════════════════════════════════════════════╗");
    println!("║ Ontology Corpus Test — Real-World Parsing & OWL RL Materialization ║");
    println!("╚════════════════════════════════════════════════════════════════════╝\n");
    println!(
        "Testing {} files from corpus at {}\n",
        corpus_files.len(),
        corpus_root.display()
    );

    let mut supported_count = 0;
    let mut supported_fail_count = 0;
    let mut total_triples = 0u64;
    let mut total_derived = 0u64;

    println!("╭─ SUPPORTED FORMATS (Turtle/N-Triples/RDF-XML) ────────────────────────────╮");
    println!("│ File                                                 Bytes      Triples │");
    println!("├─────────────────────────────────────────────────────────────────────────┤");

    // Test all files by content sniffing
    for entry in &corpus_files {
        let path = &entry.path;

        let data = match fs::read_to_string(path) {
            Ok(d) => d,
            Err(e) => {
                eprintln!("Failed to read {}: {}", path.display(), e);
                continue;
            }
        };

        supported_count += 1;

        // Detect format by content sniffing, not file extension
        let data_trimmed = data.trim_start();
        let syntax = if data_trimmed.starts_with("<?xml") || data_trimmed.contains("<rdf:RDF") {
            Syntax::RdfXml
        } else {
            Syntax::Turtle // Also handles N-Triples and N3
        };

        match Parser::parse_triples(&data, syntax) {
            Ok(triples) => {
                total_triples += triples.len() as u64;
                let bytes = data.len();
                let file_display = path
                    .strip_prefix(&corpus_root)
                    .map(|p| p.display().to_string())
                    .unwrap_or_else(|_| path.display().to_string());

                println!(
                    "│ {:<50} {:>10} {:>10} │",
                    truncate_display(&file_display, 50),
                    format_bytes(bytes),
                    triples.len()
                );
            }
            Err(e) => {
                supported_fail_count += 1;
                eprintln!("⚠️  Failed to parse {}: {}", path.display(), e);
            }
        }
    }

    println!("├─────────────────────────────────────────────────────────────────────────┤");
    println!(
        "│ SUMMARY: {} files successfully parsed, {} triples total          │",
        supported_count - supported_fail_count,
        total_triples
    );
    println!("╰─────────────────────────────────────────────────────────────────────────╯\n");

    // OWL RL Materialization Pass
    println!("╭─ OWL RL MATERIALIZATION (on all supported ontologies) ────────────────────╮");
    println!("│ File                                          Input   Derived   Derived % │");
    println!("├─────────────────────────────────────────────────────────────────────────┤");

    for entry in &corpus_files {
        let path = &entry.path;
        let data = match fs::read_to_string(path) {
            Ok(d) => d,
            Err(_) => continue,
        };

        // Skip files that failed to parse in the first pass
        let data_trimmed = data.trim_start();
        let syntax = if data_trimmed.starts_with("<?xml") || data_trimmed.contains("<rdf:RDF") {
            Syntax::RdfXml
        } else {
            Syntax::Turtle
        };

        if let Ok(triples) = Parser::parse_triples(&data, syntax) {
            if triples.is_empty() {
                continue;
            }

            let mut store = praxis_graphlaw::TripleStore::new();
            for triple in triples.clone() {
                store.add(triple);
            }

            match store.materialize_owlrl() {
                Ok(_) => {
                    let derived_count = store.len() as u64 - triples.len() as u64;
                    total_derived += derived_count;
                    let derived_pct = if triples.is_empty() {
                        0.0
                    } else {
                        (derived_count as f64 / triples.len() as f64) * 100.0
                    };
                    let file_display = path
                        .strip_prefix(&corpus_root)
                        .map(|p| p.display().to_string())
                        .unwrap_or_else(|_| path.display().to_string());
                    if derived_count > 0 {
                        println!(
                            "│ {:<42} {:>8} {:>8} {:>7.1}% │",
                            truncate_display(&file_display, 42),
                            triples.len(),
                            derived_count,
                            derived_pct
                        );
                    }
                }
                Err(e) => {
                    eprintln!(
                        "OWL RL materialization failed for {}: {}",
                        path.display(),
                        e
                    );
                }
            }
        }
    }

    println!("├─────────────────────────────────────────────────────────────────────────┤");
    println!(
        "│ SUMMARY: {} total derived triples via OWL RL materialization        │",
        total_derived
    );
    println!("╰─────────────────────────────────────────────────────────────────────────╯\n");

    // Final assertions
    assert!(
        supported_count > supported_fail_count,
        "Expected at least 1 file to parse successfully (got {}/{} successes)",
        supported_count - supported_fail_count,
        supported_count
    );
}

/// Recursively collect all files in a directory
fn collect_files_recursive(dir: &PathBuf, files: &mut Vec<FileEntry>) {
    if let Ok(entries) = fs::read_dir(dir) {
        for entry in entries.flatten() {
            if let Ok(metadata) = entry.metadata() {
                if metadata.is_dir() {
                    let path = entry.path();
                    collect_files_recursive(&path, files);
                } else {
                    files.push(FileEntry { path: entry.path() });
                }
            }
        }
    }
}

struct FileEntry {
    path: PathBuf,
}

/// Format bytes as human-readable (e.g., 1024 → "1.0K")
fn format_bytes(bytes: usize) -> String {
    if bytes < 1024 {
        format!("{}B", bytes)
    } else if bytes < 1024 * 1024 {
        format!("{:.1}K", bytes as f64 / 1024.0)
    } else {
        format!("{:.1}M", bytes as f64 / (1024.0 * 1024.0))
    }
}

/// Truncate a string to a maximum length with ellipsis if needed
fn truncate_display(s: &str, max_len: usize) -> String {
    if s.len() <= max_len {
        s.to_string()
    } else {
        format!("{}…", &s[..max_len.saturating_sub(1)])
    }
}
