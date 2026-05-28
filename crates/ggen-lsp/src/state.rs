use std::collections::HashMap;
use std::path::PathBuf;
use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::Arc;
use tokio::sync::Mutex;
use tower_lsp::lsp_types::{Diagnostic, Url};

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
    /// Project root the OCEL log is written under (default: cwd). Injectable for
    /// hermetic tests.
    pub root: PathBuf,
    /// One editor session id → all events for a (file, code) in this session
    /// share an episode (so an applied fix closes the SAME episode it raised).
    pub run_id: String,
    /// Monotonic event sequence within this session.
    seq: Arc<AtomicU64>,
    /// (uri, diagnostic_code) → route_id offered for it, so the later
    /// disappearance of that diagnostic is attributed as a `RepairApplied`.
    pending_repairs: Arc<Mutex<HashMap<(Url, String), String>>>,
    /// Last-published diagnostics per document, for new/disappeared diffing.
    published_diags: Arc<Mutex<HashMap<Url, Vec<Diagnostic>>>>,
}

impl Default for ServerState {
    fn default() -> Self {
        Self::with_root(std::env::current_dir().unwrap_or_else(|_| PathBuf::from(".")))
    }
}

impl ServerState {
    /// Construct a server state rooted at `root` (where the OCEL log is written
    /// and the promoted-route pack is loaded from). Used by the binary (cwd) and
    /// by hermetic tests (a TempDir).
    #[must_use]
    pub fn with_root(root: impl Into<PathBuf>) -> Self {
        let root = root.into();
        Self {
            documents: Arc::new(Mutex::new(HashMap::new())),
            analyzers: Arc::new(Mutex::new(HashMap::new())),
            config: ServerConfig::default(),
            routes: Arc::new(
                crate::route::RouteRegistry::seeded()
                    .with_pack_routes(&crate::route::default_pack_routes_path(&root)),
            ),
            root,
            run_id: crate::intel::events::new_run_id(),
            seq: Arc::new(AtomicU64::new(0)),
            pending_repairs: Arc::new(Mutex::new(HashMap::new())),
            published_diags: Arc::new(Mutex::new(HashMap::new())),
        }
    }

    fn next_seq(&self) -> u64 {
        self.seq.fetch_add(1, Ordering::Relaxed)
    }

    /// Observe the diagnostics published for `uri`, emitting the editor-flow OCEL
    /// chain (the live analogue of the headless `capture`):
    /// - each NEWLY raised diagnostic → `DiagnosticRaised`, and if it has a route,
    ///   `RouteSelected` + `RepairSuggested` (remembered as a pending repair);
    /// - each DISAPPEARED diagnostic that had a pending repair → `RepairApplied`
    ///   → `GatePassed` → `ReceiptEmitted` (the rework closure — an applied repair
    ///   observed, not merely proposed).
    ///
    /// Best-effort: log-append errors never disturb the editor.
    pub async fn observe_diagnostics(&self, uri: &Url, new: &[Diagnostic]) {
        use crate::intel::events::{
            diagnostic_raised, gate_result, receipt_emitted, repair_applied, repair_suggested,
            route_selected,
        };
        let file = uri.path().to_string();
        let key = |d: &Diagnostic| {
            format!("{}|{}", crate::check::diag_code(d), crate::check::span_str(d.range))
        };

        let mut published = self.published_diags.lock().await;
        let old = published.get(uri).cloned().unwrap_or_default();
        let old_keys: std::collections::HashSet<String> = old.iter().map(&key).collect();
        let new_keys: std::collections::HashSet<String> = new.iter().map(&key).collect();

        let mut pending = self.pending_repairs.lock().await;
        let mut events = Vec::new();

        // Newly raised diagnostics → raise (+ route selection if one applies).
        for d in new {
            if old_keys.contains(&key(d)) {
                continue; // already raised this episode
            }
            let code = crate::check::diag_code(d);
            let span = crate::check::span_str(d.range);
            let sev = crate::check::severity_str(d.severity);
            events.push(diagnostic_raised(&file, &code, sev, &span, &self.run_id, self.next_seq()));
            if let Some(route) = self.routes.select_for_diagnostic(d) {
                let source = crate::check::route_source(&route.provenance);
                let route_id = route.id.0.clone();
                events.push(route_selected(&file, &code, &route_id, source, &self.run_id, self.next_seq()));
                events.push(repair_suggested(&file, &code, &route_id, &self.run_id, self.next_seq()));
                pending.insert((uri.clone(), code.clone()), route_id);
            }
        }

        // Disappeared diagnostics that had a pending repair → rework closure.
        for d in &old {
            if new_keys.contains(&key(d)) {
                continue; // still present
            }
            let code = crate::check::diag_code(d);
            if let Some(route_id) = pending.remove(&(uri.clone(), code.clone())) {
                events.push(repair_applied(&file, &code, &route_id, &self.run_id, self.next_seq()));
                events.push(gate_result(&file, &code, true, 0, &self.run_id, self.next_seq()));
                let receipt_id = blake3::hash(
                    format!("{file}|{code}|{}", self.run_id).as_bytes(),
                )
                .to_hex()[..16]
                    .to_string();
                events.push(receipt_emitted(&file, &code, &receipt_id, &self.run_id, self.next_seq()));
            }
        }

        published.insert(uri.clone(), new.to_vec());
        drop(pending);
        drop(published);
        let _ = crate::intel::IntelLog::at_root(&self.root).append(&events);
    }
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

        self.published_diags.lock().await.remove(uri);
        self.pending_repairs.lock().await.retain(|(u, _), _| u != uri);
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
