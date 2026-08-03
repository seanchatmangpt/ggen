//! Compatibility-preserving live-state extension for generated Rust source law.
//!
//! `state.rs` remains the established implementation for RDF, SPARQL, Tera,
//! TOML, harness, route, receipt, and stale-clear behavior. This wrapper adds a
//! first-class Rust source pass and GGEN-SRC-004 while delegating every existing
//! method and public field through `Deref`.

use std::collections::HashSet;
use std::ops::Deref;
use std::path::{Path, PathBuf};
use std::sync::Arc;

use lsp_max::lsp_types_max::Url;
use lsp_max_protocol::MaxDiagnostic;
use tokio::sync::Mutex;

use crate::project_index::BufferOverlay;

#[path = "state.rs"]
mod established;

pub use established::{FileType, ServerConfig};

/// Extended server state. Existing behavior is delegated to `established`; the
/// additional flagged set belongs only to GGEN-SRC-004 lifecycle reconciliation.
pub struct ServerState {
    established: established::ServerState,
    src_004_flagged: Arc<Mutex<HashSet<Url>>>,
}

impl Deref for ServerState {
    type Target = established::ServerState;

    fn deref(&self) -> &Self::Target {
        &self.established
    }
}

impl Default for ServerState {
    fn default() -> Self {
        Self::with_root(std::env::current_dir().unwrap_or_else(|_| PathBuf::from(".")))
    }
}

impl ServerState {
    /// Construct the established state and the additional source-law lifecycle set.
    #[must_use]
    pub fn with_root(root: impl Into<PathBuf>) -> Self {
        Self {
            established: established::ServerState::with_root(root),
            src_004_flagged: Arc::new(Mutex::new(HashSet::new())),
        }
    }

    /// Analyze a document and publish source-law diagnostics against the live
    /// unsaved buffer. Non-Rust surfaces preserve the established path exactly.
    pub async fn analyze_and_observe(
        &self, uri: &Url, content: &str,
    ) -> Vec<(Url, Vec<MaxDiagnostic>)> {
        let is_rust = uri.path().as_str().ends_with(".rs");
        let mut published = if is_rust {
            Vec::new()
        } else {
            self.established.analyze_and_observe(uri, content).await
        };

        if is_rust || is_ggen_manifest(uri.path().as_str()) {
            let mut overlay = self.buffer_overlay().await;
            if let Ok(path) = uri_to_file_path(uri) {
                overlay.insert(path, content.to_string());
            }
            self.publish_source_pass(uri, &overlay, is_rust, &mut published)
                .await;
        }

        published
    }

    /// Close a document, then recompute source law from the remaining buffers and
    /// disk so an unsaved repair reverts to the authoritative on-disk result.
    #[allow(clippy::mutable_key_type)]
    pub async fn close_document(&self, uri: &Url) -> Vec<(Url, Vec<MaxDiagnostic>)> {
        let is_rust = uri.path().as_str().ends_with(".rs");
        let mut published = self.established.close_document(uri).await;
        if is_rust {
            published.retain(|(target, _)| target != uri);
        }
        if is_rust || is_ggen_manifest(uri.path().as_str()) {
            let overlay = self.buffer_overlay().await;
            self.publish_source_pass(uri, &overlay, is_rust, &mut published)
                .await;
        }
        published
    }

    async fn buffer_overlay(&self) -> BufferOverlay {
        let documents = self.documents.lock().await;
        documents
            .iter()
            .filter_map(|(uri, content)| {
                uri_to_file_path(uri)
                    .ok()
                    .map(|path| (path, content.clone()))
            })
            .collect()
    }

    async fn publish_source_pass(
        &self, edited: &Url, overlay: &BufferOverlay, publish_edited_rust: bool,
        published: &mut Vec<(Url, Vec<MaxDiagnostic>)>,
    ) {
        let (generated_outputs, groups) = self.source_context_for(edited, overlay);
        let mut current = HashSet::new();
        let mut published_paths = HashSet::new();
        let edited_path = uri_to_file_path(edited).ok();
        let mut edited_published = false;

        for (path, src_004) in groups {
            let Some(target) = url_from_path(&path) else {
                continue;
            };
            current.insert(target.clone());
            published_paths.insert(path.clone());
            let mut diagnostics = self.source_diagnostics(&path, overlay);
            diagnostics.extend(src_004);
            self.observe_diagnostics(&target, &diagnostics).await;
            upsert_published(published, target.clone(), diagnostics);
            if edited_path.as_ref() == Some(&path) {
                edited_published = true;
            }
        }

        if is_ggen_manifest(edited.path().as_str()) {
            let mut ordered_outputs = generated_outputs.iter().cloned().collect::<Vec<_>>();
            ordered_outputs.sort();
            for path in ordered_outputs
                .iter()
                .filter(|path| !published_paths.contains(*path))
            {
                let Some(target) = url_from_path(path) else {
                    continue;
                };
                let diagnostics = self.source_diagnostics(path, overlay);
                self.observe_diagnostics(&target, &diagnostics).await;
                upsert_published(published, target, diagnostics);
            }
        }

        if publish_edited_rust && !edited_published {
            if let Some(path) = edited_path.as_ref() {
                let diagnostics = if generated_outputs.contains(path) {
                    self.source_diagnostics(path, overlay)
                } else {
                    Vec::new()
                };
                self.observe_diagnostics(edited, &diagnostics).await;
                upsert_published(published, edited.clone(), diagnostics);
            }
        }

        let cleared = {
            let mut previous = self.src_004_flagged.lock().await;
            let mut cleared = previous
                .iter()
                .filter(|uri| !current.contains(*uri))
                .cloned()
                .collect::<Vec<_>>();
            cleared.sort_by(|left, right| left.as_str().cmp(right.as_str()));
            *previous = current;
            cleared
        };

        for target in cleared {
            let Ok(path) = uri_to_file_path(&target) else {
                continue;
            };
            let diagnostics = if generated_outputs.contains(&path) {
                self.source_diagnostics(&path, overlay)
            } else {
                Vec::new()
            };
            self.observe_diagnostics(&target, &diagnostics).await;
            upsert_published(published, target, diagnostics);
        }
    }

    fn source_diagnostics(&self, path: &Path, overlay: &BufferOverlay) -> Vec<MaxDiagnostic> {
        let content = overlay
            .get(path)
            .cloned()
            .or_else(|| std::fs::read_to_string(path).ok())
            .unwrap_or_default();
        crate::analyzers::source_law_analyzer::source_law_diagnostics(&content)
    }

    fn source_context_for(
        &self, uri: &Url, overlay: &BufferOverlay,
    ) -> (HashSet<PathBuf>, Vec<(PathBuf, Vec<MaxDiagnostic>)>) {
        let Some(root) = self.project_root_for(uri, overlay) else {
            return (HashSet::new(), Vec::new());
        };
        match project_index_from_live_manifest(&root, overlay) {
            Some(project) => {
                let generated =
                    crate::analyzers::source_law_analyzer::generated_rust_outputs(&project);
                let diagnostics =
                    crate::analyzers::source_law_analyzer::detect_src_004(&project, overlay);
                (generated, diagnostics)
            }
            None => (HashSet::new(), Vec::new()),
        }
    }

    fn project_root_for(&self, uri: &Url, overlay: &BufferOverlay) -> Option<PathBuf> {
        if let Ok(file_path) = uri_to_file_path(uri) {
            if is_ggen_manifest(uri.path().as_str()) {
                if let Some(parent) = file_path.parent() {
                    return Some(parent.to_path_buf());
                }
            }
            let mut directory = file_path.parent();
            while let Some(candidate) = directory {
                let manifest = candidate.join("ggen.toml");
                if manifest.is_file() || overlay.contains_key(&manifest) {
                    return Some(candidate.to_path_buf());
                }
                directory = candidate.parent();
            }
        }
        let fallback = self.root.join("ggen.toml");
        (fallback.is_file() || overlay.contains_key(&fallback)).then(|| self.root.clone())
    }
}

fn project_index_from_live_manifest(
    root: &Path, overlay: &BufferOverlay,
) -> Option<crate::project_index::ProjectIndex> {
    use ggen_config::{manifest::ManifestParser, ConfigSchemaClassification};

    let manifest_path = root.join("ggen.toml");
    let Some(raw) = overlay.get(&manifest_path) else {
        return crate::project_index::ProjectIndex::from_root_with_overlay(root, overlay).ok();
    };
    let manifest = match ggen_config::classify_ggen_toml(raw) {
        ConfigSchemaClassification::DeclarativeRules => ManifestParser::parse_str(raw).ok()?,
        ConfigSchemaClassification::Frontmatter => {
            return Some(crate::project_index::ProjectIndex {
                root: root.to_path_buf(),
                rule_entries: Vec::new(),
            });
        }
        ConfigSchemaClassification::Ambiguous { .. }
        | ConfigSchemaClassification::Unsupported { .. }
        | ConfigSchemaClassification::Malformed { .. } => return None,
    };
    let rule_entries = manifest
        .generation
        .rules
        .iter()
        .map(|rule| {
            crate::rule_index::RuleIndexEntry::from_rule_with_overlay(
                rule,
                &manifest_path,
                overlay,
            )
        })
        .collect();
    Some(crate::project_index::ProjectIndex {
        root: root.to_path_buf(),
        rule_entries,
    })
}

fn is_ggen_manifest(path: &str) -> bool {
    path.ends_with("ggen.toml")
}

fn uri_to_file_path(uri: &Url) -> Result<PathBuf, ()> {
    url::Url::parse(uri.as_str())
        .map_err(|_| ())?
        .to_file_path()
}

fn url_from_path(path: &Path) -> Option<Url> {
    url::Url::from_file_path(path).ok()?.to_string().parse().ok()
}

fn upsert_published(
    published: &mut Vec<(Url, Vec<MaxDiagnostic>)>, target: Url, diagnostics: Vec<MaxDiagnostic>,
) {
    if let Some((_, current)) = published.iter_mut().find(|(uri, _)| uri == &target) {
        for diagnostic in diagnostics {
            let duplicate = current.iter().any(|existing| {
                existing.lsp.code == diagnostic.lsp.code
                    && existing.lsp.range == diagnostic.lsp.range
                    && existing.lsp.message == diagnostic.lsp.message
            });
            if !duplicate {
                current.push(diagnostic);
            }
        }
    } else {
        published.push((target, diagnostics));
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;
    use tempfile::TempDir;

    fn write_project(root: &Path, include_child_rule: bool) {
        fs::create_dir_all(root.join("src")).expect("create src");
        fs::write(root.join("src/lib.rs"), "pub mod capabilities;\n").expect("write lib");
        fs::write(root.join("src/capabilities.rs"), "pub struct Capability;\n")
            .expect("write child");
        let child = if include_child_rule {
            r#"
[[generation.rules]]
name = "capabilities"
output_file = "src/capabilities.rs"
query = { inline = "SELECT ?name WHERE { ?s ?p ?name }" }
template = { inline = "pub struct Capability;" }
"#
        } else {
            ""
        };
        fs::write(
            root.join("ggen.toml"),
            format!(
                r#"[project]
name = "src-004"
version = "0.1.0"

[ontology]
source = "model.ttl"

[[generation.rules]]
name = "root"
output_file = "src/lib.rs"
query = {{ inline = "SELECT ?name WHERE {{ ?s ?p ?name }}" }}
template = {{ inline = "pub mod capabilities;" }}
{child}
"#
            ),
        )
        .expect("write manifest");
    }

    #[tokio::test]
    async fn live_buffer_raises_and_clears_src_004() {
        let temp = TempDir::new().expect("tempdir");
        write_project(temp.path(), false);
        let state = ServerState::with_root(temp.path());
        let uri: Url = url::Url::from_file_path(temp.path().join("src/lib.rs"))
            .expect("uri")
            .to_string()
            .parse()
            .expect("document uri");

        state
            .set_document(uri.clone(), "pub mod capabilities;\n".to_string())
            .await;
        let raised = state
            .analyze_and_observe(&uri, "pub mod capabilities;\n")
            .await;
        assert!(raised.iter().flat_map(|(_, diagnostics)| diagnostics).any(
            |diagnostic| diagnostic.law_id == crate::analyzers::source_law_analyzer::GGEN_SRC_004
        ));

        let repaired = "mod capabilities { pub struct Capability; }\n";
        state
            .set_document(uri.clone(), repaired.to_string())
            .await;
        let cleared = state.analyze_and_observe(&uri, repaired).await;
        assert!(!cleared.iter().flat_map(|(_, diagnostics)| diagnostics).any(
            |diagnostic| diagnostic.law_id == crate::analyzers::source_law_analyzer::GGEN_SRC_004
        ));
    }

    #[tokio::test]
    async fn live_manifest_rule_clears_src_004_before_save() {
        let temp = TempDir::new().expect("tempdir");
        write_project(temp.path(), false);
        let state = ServerState::with_root(temp.path());
        let source_uri: Url = url::Url::from_file_path(temp.path().join("src/lib.rs"))
            .expect("source uri")
            .to_string()
            .parse()
            .expect("source document uri");
        state
            .set_document(source_uri.clone(), "pub mod capabilities;\n".to_string())
            .await;
        let raised = state
            .analyze_and_observe(&source_uri, "pub mod capabilities;\n")
            .await;
        assert!(raised.iter().flat_map(|(_, diagnostics)| diagnostics).any(
            |diagnostic| diagnostic.law_id == crate::analyzers::source_law_analyzer::GGEN_SRC_004
        ));

        let manifest_path = temp.path().join("ggen.toml");
        let manifest_uri: Url = url::Url::from_file_path(&manifest_path)
            .expect("manifest uri")
            .to_string()
            .parse()
            .expect("manifest document uri");
        let repaired = format!(
            "{}\n[[generation.rules]]\nname = \"capabilities\"\noutput_file = \"src/capabilities.rs\"\nquery = {{ inline = \"SELECT ?name WHERE {{ ?s ?p ?name }}\" }}\ntemplate = {{ inline = \"pub struct Capability;\" }}\n",
            fs::read_to_string(&manifest_path).expect("read manifest")
        );
        state
            .set_document(manifest_uri.clone(), repaired.clone())
            .await;
        let cleared = state.analyze_and_observe(&manifest_uri, &repaired).await;
        assert!(!cleared.iter().flat_map(|(_, diagnostics)| diagnostics).any(
            |diagnostic| diagnostic.law_id == crate::analyzers::source_law_analyzer::GGEN_SRC_004
        ));
        assert_eq!(
            fs::read_to_string(&manifest_path)
                .expect("disk manifest")
                .matches("name = \"capabilities\"")
                .count(),
            0,
            "repair must be proven from the unsaved overlay, not disk"
        );
    }
}
