use tower_lsp::lsp_types::Url;
use std::collections::HashMap;
use std::sync::Arc;
use tokio::sync::Mutex;

use crate::analyzers::DocumentAnalyzer;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FileType {
    /// RDF graph documents: `.ttl`, `.nt`, `.nq`.
    Rdf,
    /// SPARQL queries: `.rq`, `.sparql`.
    Sparql,
    /// Tera templates: `.tera`.
    Tera,
    /// ggen configuration: `ggen.toml` (and `.toml`).
    Toml,
    /// Not a ggen law surface.
    Unknown,
}

impl FileType {
    pub fn from_uri(uri: &Url) -> Self {
        Self::from_path(uri.path())
    }

    /// Classify a path/URI string into a law-surface file type.
    #[must_use]
    pub fn from_path(path: &str) -> Self {
        if path.ends_with(".ttl") || path.ends_with(".nt") || path.ends_with(".nq") {
            FileType::Rdf
        } else if path.ends_with(".rq") || path.ends_with(".sparql") {
            FileType::Sparql
        } else if path.ends_with(".tera") {
            FileType::Tera
        } else if path.ends_with("ggen.toml") || path.ends_with(".toml") {
            FileType::Toml
        } else {
            FileType::Unknown
        }
    }
}

#[derive(Debug, Clone)]
pub struct ServerConfig {
    pub auto_format_on_save: bool,
    pub show_hints: bool,
    pub workspace_symbol_depth: usize,
}

impl Default for ServerConfig {
    fn default() -> Self {
        Self {
            auto_format_on_save: true,
            show_hints: true,
            workspace_symbol_depth: 5,
        }
    }
}

pub struct ServerState {
    pub documents: Arc<Mutex<HashMap<Url, String>>>,
    pub analyzers: Arc<Mutex<HashMap<Url, DocumentAnalyzer>>>,
    pub config: ServerConfig,
    /// Precomputed repair routes. Immutable on the hot path (no lock needed for
    /// reads): a plain `Arc` so `code_action` looks up routes lock-free.
    pub routes: Arc<crate::route::RouteRegistry>,
}

impl Default for ServerState {
    fn default() -> Self {
        Self {
            documents: Arc::new(Mutex::new(HashMap::new())),
            analyzers: Arc::new(Mutex::new(HashMap::new())),
            config: ServerConfig::default(),
            // Seeds + any promoted routes for the current project (best-effort).
            routes: Arc::new(
                crate::route::RouteRegistry::seeded().with_pack_routes(
                    &crate::route::default_pack_routes_path(std::path::Path::new(".")),
                ),
            ),
        }
    }
}

impl ServerState {
    pub async fn set_document(&self, uri: Url, content: String) {
        let mut docs = self.documents.lock().await;
        docs.insert(uri, content);
    }

    pub async fn get_document(&self, uri: &Url) -> Option<String> {
        let docs = self.documents.lock().await;
        docs.get(uri).cloned()
    }

    pub async fn remove_document(&self, uri: &Url) {
        let mut docs = self.documents.lock().await;
        docs.remove(uri);

        let mut analyzers = self.analyzers.lock().await;
        analyzers.remove(uri);
    }

    pub async fn set_analyzer(&self, uri: Url, analyzer: DocumentAnalyzer) {
        let mut analyzers = self.analyzers.lock().await;
        analyzers.insert(uri, analyzer);
    }

    pub async fn get_analyzer(&self, uri: &Url) -> Option<DocumentAnalyzer> {
        let analyzers = self.analyzers.lock().await;
        analyzers.get(uri).cloned()
    }
}
