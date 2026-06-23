use lsp_max::lsp_types_max::*;
use lsp_max_protocol::MaxDiagnostic;
use std::collections::{HashMap, HashSet};
use std::path::{Path, PathBuf};
use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::Arc;
use tokio::sync::Mutex;

/// Convert a `DocumentUri` to a `PathBuf` for `file://` URIs.
/// Uses `url::Url` (the real crate) as a shim since `DocumentUri` (fluent_uri) has
/// no `to_file_path()` method.
fn uri_to_file_path(uri: &DocumentUri) -> Result<PathBuf, ()> {
    url::Url::parse(uri.as_str())
        .map_err(|_| ())?
        .to_file_path()
}

use crate::analyzers::DocumentAnalyzer;
use crate::project_index::BufferOverlay;

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
    /// OpenUSD ASCII scenes: `.usda`, `.usd`, `.usdc`.
    Usd,
    /// MaterialX shading graphs: `.mtlx`.
    Mtlx,
    /// Not a ggen law surface.
    Unknown,
}

impl FileType {
    pub fn from_uri(uri: &DocumentUri) -> Self {
        Self::from_path(uri.path().as_str())
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
        } else if path.ends_with(".usda") || path.ends_with(".usd") || path.ends_with(".usdc") {
            FileType::Usd
        } else if path.ends_with(".mtlx") {
            FileType::Mtlx
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
    pub documents: Arc<Mutex<HashMap<DocumentUri, String>>>,
    pub analyzers: Arc<Mutex<HashMap<DocumentUri, DocumentAnalyzer>>>,
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
    pub(crate) seq: Arc<AtomicU64>,
    /// (uri, diagnostic_code) → route_id offered for it, so the later
    /// disappearance of that diagnostic is attributed as a `RepairApplied`.
    pending_repairs: Arc<Mutex<HashMap<(DocumentUri, String), String>>>,
    /// Last-published diagnostics per document, for new/disappeared diffing.
    published_diags: Arc<Mutex<HashMap<DocumentUri, Vec<MaxDiagnostic>>>>,
    /// Per-species flagged-surface sets, keyed by diagnostic code. Each entry is
    /// the set of anchor URIs that received that species' publish on the PREVIOUS
    /// pass; a later pass that no longer flags a URI re-publishes its residual so
    /// the disappeared key is observed as a lawful clear (not a silent absence).
    /// One map replaces the former parallel tpl_flagged/harness_flagged/out_flagged
    /// fields — a new ProjectIndex/HarnessIndex species is now one more key, not a
    /// fourth field + a fourth clears_for.
    flagged: Arc<Mutex<HashMap<&'static str, HashSet<DocumentUri>>>>,
}

/// True if `path` is a ggen project manifest (`ggen.toml`) — the TPL-001 trigger.
fn is_ggen_manifest(path: &str) -> bool {
    path.ends_with("ggen.toml")
}

/// True if `path` is a harness declaration surface (`Cargo.toml`/`Makefile.toml`)
/// — the GGEN-HARNESS-001 trigger. Disjoint from [`is_ggen_manifest`] by basename.
fn is_harness_surface(path: &str) -> bool {
    path.ends_with("Cargo.toml") || path.ends_with("Makefile.toml")
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
            flagged: Arc::new(Mutex::new(HashMap::new())),
        }
    }

    /// Reconcile a species' flagged-surface set for one edit. Generic over the
    /// species `code` (the map key). `current` is the set of anchor URIs the
    /// species fires for *now*; `edited` (already published on its own path) is
    /// excluded to avoid a double publish. Returns the URIs flagged on the PREVIOUS
    /// pass but no longer flagged — the caller publishes each one's residual (TPL:
    /// empty single-file set; HARNESS/OUT: residual single-file diagnostics)
    /// through `observe_diagnostics` so the disappeared key becomes an observed
    /// lifecycle transition. Stores `current` as the flagged set for the next pass.
    #[allow(clippy::mutable_key_type)]
    pub async fn clears_for(
        &self, code: &'static str, edited: &DocumentUri, current: &HashSet<DocumentUri>,
    ) -> Vec<DocumentUri> {
        let mut flagged = self.flagged.lock().await;
        let prev = flagged.entry(code).or_default();
        let cleared: Vec<DocumentUri> = prev
            .iter()
            .filter(|&u| !current.contains(u) && *u != *edited)
            .cloned()
            .collect();
        *prev = current.clone();
        cleared
    }

    /// GGEN-TPL-001 stale-clear reconciler — preserved public entry (the hermetic
    /// stale-clear test drives it directly). Thin shim over the generic
    /// [`Self::clears_for`] keyed on the TPL species code; behavior is identical.
    #[allow(clippy::mutable_key_type)]
    pub async fn tpl_clears_for(&self, edited: &DocumentUri, current: &HashSet<DocumentUri>) -> Vec<DocumentUri> {
        self.clears_for(crate::analyzers::GGEN_TPL_001, edited, current)
            .await
    }

    fn next_seq(&self) -> u64 {
        self.seq.fetch_add(1, Ordering::Relaxed)
    }

    /// Observe the diagnostics published for `uri`, emitting the editor-flow OCEL
    /// chain (the live analogue of the headless `capture`):
    /// - each NEWLY raised diagnostic → `MaxDiagnosticRaised`, and if it has a route,
    ///   `RouteSelected` + `RepairSuggested` (remembered as a pending repair);
    /// - each DISAPPEARED diagnostic that had a pending repair → `RepairApplied`
    ///   → `GatePassed` → `ReceiptEmitted` (the rework closure — an applied repair
    ///   observed, not merely proposed).
    ///
    /// Best-effort: log-append errors never disturb the editor.
    pub async fn observe_diagnostics(&self, uri: &DocumentUri, new: &[MaxDiagnostic]) {
        use crate::intel::events::{
            diagnostic_raised, gate_result, receipt_emitted, repair_applied, repair_suggested,
            route_selected,
        };
        let file = uri.path().as_str().to_string();
        let key = |d: &MaxDiagnostic| {
            format!(
                "{}|{}",
                crate::check::diag_code(&d.lsp),
                crate::check::span_str(d.lsp.range)
            )
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
            let code = crate::check::diag_code(&d.lsp);
            let span = crate::check::span_str(d.lsp.range);
            let sev = crate::check::severity_str(d.lsp.severity);
            events.push(diagnostic_raised(
                &file,
                &code,
                sev,
                &span,
                &self.run_id,
                self.next_seq(),
            ));
            if let Some(route) = self.routes.select_for_diagnostic(&d.lsp) {
                let source = crate::check::route_source(&route.provenance);
                let route_id = route.id.0.clone();
                events.push(route_selected(
                    &file,
                    &code,
                    &route_id,
                    source,
                    &self.run_id,
                    self.next_seq(),
                ));
                events.push(repair_suggested(
                    &file,
                    &code,
                    &route_id,
                    &self.run_id,
                    self.next_seq(),
                ));
                pending.insert((uri.clone(), code.clone()), route_id);
            }
        }

        // Disappeared diagnostics that had a pending repair → rework closure.
        for d in &old {
            if new_keys.contains(&key(d)) {
                continue; // still present
            }
            let code = crate::check::diag_code(&d.lsp);
            if let Some(route_id) = pending.remove(&(uri.clone(), code.clone())) {
                events.push(repair_applied(
                    &file,
                    &code,
                    &route_id,
                    &self.run_id,
                    self.next_seq(),
                ));
                events.push(gate_result(
                    &file,
                    &code,
                    true,
                    0,
                    &self.run_id,
                    self.next_seq(),
                ));
                let receipt_id = blake3::hash(format!("{file}|{code}|{}", self.run_id).as_bytes())
                    .to_hex()[..16]
                    .to_string();
                events.push(receipt_emitted(
                    &file,
                    &code,
                    &receipt_id,
                    &self.run_id,
                    self.next_seq(),
                ));
            }
        }

        published.insert(uri.clone(), new.to_vec());
        drop(pending);
        drop(published);
        // Attribute editor events to the LSP transport, grouped under this session.
        crate::intel::events::attach_attribution(
            &mut events,
            &crate::intel::events::Attribution::lsp(&self.run_id),
        );
        let _ = crate::intel::IntelLog::at_root(&self.root).append(&events);
    }
    pub async fn set_document(&self, uri: DocumentUri, content: String) {
        let mut docs = self.documents.lock().await;
        docs.insert(uri, content);
    }

    pub async fn get_document(&self, uri: &DocumentUri) -> Option<String> {
        let docs = self.documents.lock().await;
        docs.get(uri).cloned()
    }

    async fn buffer_overlay(&self) -> BufferOverlay {
        let docs = self.documents.lock().await;
        docs.iter()
            .filter_map(|(u, c)| uri_to_file_path(u).ok().map(|p| (p, c.clone())))
            .collect()
    }

    pub async fn remove_document(&self, uri: &DocumentUri) {
        let mut docs = self.documents.lock().await;
        docs.remove(uri);

        let mut analyzers = self.analyzers.lock().await;
        analyzers.remove(uri);

        self.published_diags.lock().await.remove(uri);
        self.pending_repairs
            .lock()
            .await
            .retain(|(u, _), _| u != uri);
    }

    #[allow(clippy::mutable_key_type)]
    pub async fn close_document(&self, uri: &DocumentUri) -> Vec<(DocumentUri, Vec<MaxDiagnostic>)> {
        self.remove_document(uri).await;
        let overlay = self.buffer_overlay().await;
        let mut published: Vec<(DocumentUri, Vec<MaxDiagnostic>)> = Vec::new();

        let tpl_is_trigger = matches!(
            FileType::from_path(uri.path().as_str()),
            FileType::Tera | FileType::Sparql
        ) || is_ggen_manifest(uri.path().as_str());
        if tpl_is_trigger {
            let mut current: HashSet<DocumentUri> = HashSet::new();
            for (template_path, _) in self.detect_tpl_001_for(uri, &overlay) {
                if let Some(u) = url_from_path(&template_path) {
                    current.insert(u);
                }
            }
            for cleared in self.tpl_clears_for(uri, &current).await {
                let residual = self.residual_single_file_diags(&cleared).await;
                self.observe_diagnostics(&cleared, &residual).await;
                published.push((cleared, residual));
            }

            let mut out_current: HashSet<DocumentUri> = HashSet::new();
            for (manifest_path, _) in self.detect_out_001_for(uri, &overlay) {
                if let Some(u) = url_from_path(&manifest_path) {
                    out_current.insert(u);
                }
            }
            for cleared in self
                .clears_for(crate::analyzers::GGEN_OUT_001, uri, &out_current)
                .await
            {
                let residual = self.residual_single_file_diags(&cleared).await;
                self.observe_diagnostics(&cleared, &residual).await;
                published.push((cleared, residual));
            }

            let mut rule_current: HashSet<DocumentUri> = HashSet::new();
            for (manifest_path, _) in self.detect_rule_001_for(uri, &overlay) {
                if let Some(u) = url_from_path(&manifest_path) {
                    rule_current.insert(u);
                }
            }
            for cleared in self
                .clears_for(crate::analyzers::GGEN_RULE_001, uri, &rule_current)
                .await
            {
                let residual = self.residual_single_file_diags(&cleared).await;
                self.observe_diagnostics(&cleared, &residual).await;
                published.push((cleared, residual));
            }
        }

        if is_harness_surface(uri.path().as_str()) {
            let mut current: HashSet<DocumentUri> = HashSet::new();
            for (manifest_path, _) in self.detect_harness_001_for(uri, &overlay) {
                if let Some(u) = url_from_path(&manifest_path) {
                    current.insert(u);
                }
            }
            for cleared in self
                .clears_for(crate::analyzers::GGEN_HARNESS_001, uri, &current)
                .await
            {
                let residual = self.residual_single_file_diags(&cleared).await;
                self.observe_diagnostics(&cleared, &residual).await;
                published.push((cleared, residual));
            }
        }

        published.push((uri.clone(), Vec::new()));
        published
    }

    pub async fn set_analyzer(&self, uri: DocumentUri, analyzer: DocumentAnalyzer) {
        let mut analyzers = self.analyzers.lock().await;
        analyzers.insert(uri, analyzer);
    }

    pub async fn get_analyzer(&self, uri: &DocumentUri) -> Option<DocumentAnalyzer> {
        let analyzers = self.analyzers.lock().await;
        analyzers.get(uri).cloned()
    }

    pub async fn analyze_and_observe(
        &self, uri: &DocumentUri, content: &str,
    ) -> Vec<(DocumentUri, Vec<MaxDiagnostic>)> {
        let mut published: Vec<(DocumentUri, Vec<MaxDiagnostic>)> = Vec::new();
        let file_type = FileType::from_path(uri.path().as_str());

        let mut overlay = self.buffer_overlay().await;
        if let Ok(p) = uri_to_file_path(uri) {
            overlay.insert(p, content.to_string());
        }

        let mut own_diags: Vec<MaxDiagnostic> = Vec::new();
        if let Some(analyzer) = crate::analyzers::build_analyzer(uri.path().as_str(), content) {
            own_diags = analyzer.diagnostics();
            self.set_analyzer(uri.clone(), analyzer).await;
        }

        let tpl_is_trigger = matches!(file_type, FileType::Tera | FileType::Sparql)
            || is_ggen_manifest(uri.path().as_str());
        let tpl_groups = if tpl_is_trigger {
            self.detect_tpl_001_for(uri, &overlay)
        } else {
            Vec::new()
        };

        let harness_is_trigger = is_harness_surface(uri.path().as_str());
        let harness_groups = if harness_is_trigger {
            self.detect_harness_001_for(uri, &overlay)
        } else {
            Vec::new()
        };

        let out_groups = if tpl_is_trigger {
            self.detect_out_001_for(uri, &overlay)
        } else {
            Vec::new()
        };

        let rule_groups = if tpl_is_trigger {
            self.detect_rule_001_for(uri, &overlay)
        } else {
            Vec::new()
        };

        let edited_self_url = url_from_path_str(uri.path().as_str());
        let mut published_self = false;

        struct Species {
            code: &'static str,
            is_trigger: bool,
            groups: Vec<(PathBuf, Vec<MaxDiagnostic>)>,
            flagged: HashSet<DocumentUri>,
        }
        let mut species = [
            Species {
                code: crate::analyzers::GGEN_TPL_001,
                is_trigger: tpl_is_trigger,
                groups: tpl_groups,
                flagged: HashSet::new(),
            },
            Species {
                code: crate::analyzers::GGEN_HARNESS_001,
                is_trigger: harness_is_trigger,
                groups: harness_groups,
                flagged: HashSet::new(),
            },
            Species {
                code: crate::analyzers::GGEN_OUT_001,
                is_trigger: tpl_is_trigger,
                groups: out_groups,
                flagged: HashSet::new(),
            },
            Species {
                code: crate::analyzers::GGEN_RULE_001,
                is_trigger: tpl_is_trigger,
                groups: rule_groups,
                flagged: HashSet::new(),
            },
        ];

        for sp in &mut species {
            for (anchor_path, diags) in std::mem::take(&mut sp.groups) {
                let Some(anchor_url) = url_from_path(&anchor_path) else {
                    continue;
                };
                sp.flagged.insert(anchor_url.clone());
                if edited_self_url.as_ref() == Some(&anchor_url) && !published_self {
                    let mut merged = std::mem::take(&mut own_diags);
                    merged.extend(diags);
                    self.observe_diagnostics(&anchor_url, &merged).await;
                    published.push((anchor_url, merged));
                    published_self = true;
                } else {
                    self.observe_diagnostics(&anchor_url, &diags).await;
                    published.push((anchor_url, diags));
                }
            }
        }

        if !published_self {
            self.observe_diagnostics(uri, &own_diags).await;
            published.push((uri.clone(), own_diags));
        }

        for sp in &species {
            if !sp.is_trigger {
                continue;
            }
            for cleared in self.clears_for(sp.code, uri, &sp.flagged).await {
                let residual = self.residual_single_file_diags(&cleared).await;
                self.observe_diagnostics(&cleared, &residual).await;
                published.push((cleared, residual));
            }
        }

        published
    }

    async fn residual_single_file_diags(&self, uri: &DocumentUri) -> Vec<MaxDiagnostic> {
        let content = match self.get_document(uri).await {
            Some(c) => c,
            None => match uri_to_file_path(uri) {
                Ok(path) => std::fs::read_to_string(&path).unwrap_or_default(),
                Err(()) => String::new(),
            },
        };
        crate::analyzers::build_analyzer(uri.path().as_str(), &content)
            .map(|a| a.diagnostics())
            .unwrap_or_default()
    }

    fn detect_tpl_001_for(
        &self, uri: &DocumentUri, overlay: &BufferOverlay,
    ) -> Vec<(PathBuf, Vec<MaxDiagnostic>)> {
        let Some(root) = self.project_root_for(uri) else {
            return Vec::new();
        };
        match crate::project_index::ProjectIndex::from_root_with_overlay(&root, overlay) {
            Ok(project) => crate::analyzers::detect_tpl_001(&project),
            Err(_) => Vec::new(),
        }
    }

    fn detect_out_001_for(
        &self, uri: &DocumentUri, overlay: &BufferOverlay,
    ) -> Vec<(PathBuf, Vec<MaxDiagnostic>)> {
        let Some(root) = self.project_root_for(uri) else {
            return Vec::new();
        };
        match crate::project_index::ProjectIndex::from_root_with_overlay(&root, overlay) {
            Ok(project) => crate::analyzers::detect_out_001(&project),
            Err(_) => Vec::new(),
        }
    }

    fn detect_rule_001_for(
        &self, uri: &DocumentUri, overlay: &BufferOverlay,
    ) -> Vec<(PathBuf, Vec<MaxDiagnostic>)> {
        let Some(root) = self.project_root_for(uri) else {
            return Vec::new();
        };
        match crate::project_index::ProjectIndex::from_root_with_overlay(&root, overlay) {
            Ok(project) => crate::analyzers::detect_rule_001(&project),
            Err(_) => Vec::new(),
        }
    }

    fn project_root_for(&self, uri: &DocumentUri) -> Option<PathBuf> {
        if let Ok(file_path) = uri_to_file_path(uri) {
            let mut dir: Option<&Path> = file_path.parent();
            while let Some(d) = dir {
                if d.join("ggen.toml").is_file() {
                    return Some(d.to_path_buf());
                }
                dir = d.parent();
            }
        }
        let fallback = self.root.clone();
        if fallback.join("ggen.toml").is_file() {
            Some(fallback)
        } else {
            None
        }
    }

    fn detect_harness_001_for(
        &self, uri: &DocumentUri, overlay: &BufferOverlay,
    ) -> Vec<(PathBuf, Vec<MaxDiagnostic>)> {
        let Some(root) = self.harness_root_for(uri) else {
            return Vec::new();
        };
        match crate::harness_index::HarnessIndex::from_root_with_overlay(&root, overlay) {
            Ok(index) => crate::analyzers::detect_harness_001(&index),
            Err(_) => Vec::new(),
        }
    }

    fn harness_root_for(&self, uri: &DocumentUri) -> Option<PathBuf> {
        if let Ok(file_path) = uri_to_file_path(uri) {
            let mut dir: Option<&Path> = file_path.parent();
            while let Some(d) = dir {
                if d.join("Cargo.toml").is_file() {
                    return Some(d.to_path_buf());
                }
                dir = d.parent();
            }
        }
        let fallback = self.root.clone();
        if fallback.join("Cargo.toml").is_file() {
            Some(fallback)
        } else {
            None
        }
    }
}

fn url_from_path(path: &Path) -> Option<DocumentUri> {
    let s = url::Url::from_file_path(path).ok()?.to_string();
    s.parse().ok()
}

fn url_from_path_str(path: &str) -> Option<DocumentUri> {
    let decoded = percent_decode_path(path);
    let s = url::Url::from_file_path(std::path::Path::new(&decoded))
        .ok()?
        .to_string();
    s.parse().ok()
}

fn percent_decode_path(path: &str) -> String {
    let bytes = path.as_bytes();
    let mut out: Vec<u8> = Vec::with_capacity(bytes.len());
    let mut i = 0;
    while i < bytes.len() {
        if bytes[i] == b'%' && i + 2 < bytes.len() {
            let hi = (bytes[i + 1] as char).to_digit(16);
            let lo = (bytes[i + 2] as char).to_digit(16);
            if let (Some(hi), Some(lo)) = (hi, lo) {
                out.push((hi * 16 + lo) as u8);
                i += 3;
                continue;
            }
        }
        out.push(bytes[i]);
        i += 1;
    }
    String::from_utf8_lossy(&out).into_owned()
}
