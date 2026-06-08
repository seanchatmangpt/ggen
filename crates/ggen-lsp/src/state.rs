use std::collections::{HashMap, HashSet};
use std::path::{Path, PathBuf};
use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::Arc;
use tokio::sync::Mutex;
use tower_lsp_max::lsp_types::{Diagnostic, Url};

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
    /// Per-species flagged-surface sets, keyed by diagnostic code. Each entry is
    /// the set of anchor URIs that received that species' publish on the PREVIOUS
    /// pass; a later pass that no longer flags a URI re-publishes its residual so
    /// the disappeared key is observed as a lawful clear (not a silent absence).
    /// One map replaces the former parallel tpl_flagged/harness_flagged/out_flagged
    /// fields — a new ProjectIndex/HarnessIndex species is now one more key, not a
    /// fourth field + a fourth clears_for.
    flagged: Arc<Mutex<HashMap<&'static str, HashSet<Url>>>>,
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
    ///
    /// One generic primitive replaces the former parallel
    /// tpl_clears_for/harness_clears_for/out_clears_for methods (their bodies were
    /// byte-identical modulo the locked field). A new species reconciles by passing
    /// its code as the map key — no fourth copy.
    pub async fn clears_for(
        &self, code: &'static str, edited: &Url, current: &HashSet<Url>,
    ) -> Vec<Url> {
        let mut flagged = self.flagged.lock().await;
        let prev = flagged.entry(code).or_default();
        let cleared: Vec<Url> = prev
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
    pub async fn tpl_clears_for(&self, edited: &Url, current: &HashSet<Url>) -> Vec<Url> {
        self.clears_for(crate::analyzers::GGEN_TPL_001, edited, current)
            .await
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
            format!(
                "{}|{}",
                crate::check::diag_code(d),
                crate::check::span_str(d.range)
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
            let code = crate::check::diag_code(d);
            let span = crate::check::span_str(d.range);
            let sev = crate::check::severity_str(d.severity);
            events.push(diagnostic_raised(
                &file,
                &code,
                sev,
                &span,
                &self.run_id,
                self.next_seq(),
            ));
            if let Some(route) = self.routes.select_for_diagnostic(d) {
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
            let code = crate::check::diag_code(d);
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
    pub async fn set_document(&self, uri: Url, content: String) {
        let mut docs = self.documents.lock().await;
        docs.insert(uri, content);
    }

    pub async fn get_document(&self, uri: &Url) -> Option<String> {
        let docs = self.documents.lock().await;
        docs.get(uri).cloned()
    }

    /// Snapshot every open buffer as a disk-path → content overlay for the
    /// cross-surface index (LIVE-BUFFER-001).
    ///
    /// Keys are `Url::to_file_path()` (non-canonicalized, matching the index's
    /// `manifest_dir.join` / `root.join` resolution); non-`file://` URLs are
    /// skipped. When nothing is open the overlay is empty and every index read
    /// falls back to disk — byte-identical to the pre-overlay path. The lock is
    /// released as the snapshot returns, so no documents lock is held across the
    /// (synchronous) index build.
    async fn buffer_overlay(&self) -> BufferOverlay {
        let docs = self.documents.lock().await;
        docs.iter()
            .filter_map(|(u, c)| u.to_file_path().ok().map(|p| (p, c.clone())))
            .collect()
    }

    pub async fn remove_document(&self, uri: &Url) {
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

    /// Close `uri`: drop its own state (delegating to [`Self::remove_document`]),
    /// then PROACTIVELY clear any cross-surface GGEN-TPL-001 / GGEN-HARNESS-001
    /// flag the closed surface contributed.
    ///
    /// A closed rule surface produces no flags, so the reconcilers run with the
    /// FRESHLY RE-DETECTED current set (the closed doc is gone from the in-memory
    /// store, and the detector re-walks the project from disk). A flag that only
    /// the closed surface sustained therefore disappears from `current` and falls
    /// out through the SAME keyed-subtraction + residual-preservation path the
    /// edit flow uses (the generic [`Self::clears_for`] +
    /// `residual_single_file_diags` + [`Self::observe_diagnostics`]). A
    /// flag still sustained by a SURVIVING peer surface stays in `current` and is
    /// NOT cleared — so closing one of several open rule surfaces never regresses
    /// a still-lawfully-flagged peer.
    ///
    /// Each cleared peer is republished through `observe_diagnostics`, so the
    /// disappeared cross-surface key is observed as a lawful clear (RepairApplied
    /// → GatePassed → ReceiptEmitted), NOT a blunt empty publish — and unrelated
    /// single-file diagnostics on the peer survive (residual preservation).
    ///
    /// Returns the `(uri, diagnostics)` pairs the caller must publish to the
    /// editor, in order: first each cleared peer's residual set, then an explicit
    /// empty-publish pair for the closed URI itself (LSP conformance — clear the
    /// closed document's own squiggles). Strictly read-only w.r.t. the project.
    pub async fn close_document(&self, uri: &Url) -> Vec<(Url, Vec<Diagnostic>)> {
        // Drop the closed doc's own state FIRST so re-detection / residual reads
        // no longer see its in-memory copy.
        self.remove_document(uri).await;

        // LIVE-BUFFER-001: snapshot the overlay AFTER removal — the closed surface
        // contributes no buffer, so the index falls back to its on-disk content
        // (the correct fallback for a now-closed file). No edited-pair insertion
        // (unlike `analyze_and_observe`): there is no edit, the doc is gone.
        let overlay = self.buffer_overlay().await;

        let mut published: Vec<(Url, Vec<Diagnostic>)> = Vec::new();

        // TPL reconcile: only if the closed surface is a TPL trigger
        // (`.tera`/`.rq`/`.sparql` or the `ggen.toml` manifest). Mirrors the
        // trigger gate in `analyze_and_observe`.
        let tpl_is_trigger = matches!(
            FileType::from_path(uri.path()),
            FileType::Tera | FileType::Sparql
        ) || is_ggen_manifest(uri.path());
        if tpl_is_trigger {
            // Re-detect AFTER removal: `current` = templates STILL flagged by the
            // surviving surfaces. A flag only the closed surface sustained is gone.
            let mut current: HashSet<Url> = HashSet::new();
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

            // OUT reconcile rides the SAME trigger as TPL (both ProjectIndex-derived
            // over `.tera`/`.rq`/`ggen.toml`). A flag only the closed surface
            // sustained falls out of the freshly re-detected `current` set and is
            // cleared with residual preservation (mirrors TPL).
            let mut out_current: HashSet<Url> = HashSet::new();
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

            // RULE reconcile rides the SAME trigger as TPL/OUT (ProjectIndex-derived).
            // A manifest whose dangling rule binding was repaired (the file now
            // exists) drops out of the freshly re-detected `current` set and is
            // cleared with residual preservation (mirrors TPL/OUT).
            let mut rule_current: HashSet<Url> = HashSet::new();
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

        // HARNESS reconcile: only if the closed surface is a harness trigger
        // (`Cargo.toml`/`Makefile.toml`). Disjoint from the TPL trigger by basename.
        if is_harness_surface(uri.path()) {
            let mut current: HashSet<Url> = HashSet::new();
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

        // LSP conformance: clear the closed URI's own squiggles. Pushed LAST.
        // Not routed through `observe_diagnostics`: its `published_diags` entry was
        // already removed above, so observing an empty set here would re-treat the
        // prior set as "disappeared" and double-emit. A direct empty publish is the
        // correct editor-only clear for the surface that is gone.
        published.push((uri.clone(), Vec::new()));

        published
    }

    pub async fn set_analyzer(&self, uri: Url, analyzer: DocumentAnalyzer) {
        let mut analyzers = self.analyzers.lock().await;
        analyzers.insert(uri, analyzer);
    }

    pub async fn get_analyzer(&self, uri: &Url) -> Option<DocumentAnalyzer> {
        let analyzers = self.analyzers.lock().await;
        analyzers.get(uri).cloned()
    }

    /// Analyze `content` for `uri` and record every resulting publish through the
    /// living OCEL loop (`observe_diagnostics`), returning the per-URI diagnostic
    /// sets that should be published to the editor — WITHOUT requiring a
    /// `tower_lsp::Client`.
    ///
    /// This is the Client-free core of `server::GgenLanguageServer::refresh_analyzer`:
    /// it runs the SAME orchestration (single-file `build_analyzer`, cross-surface
    /// GGEN-TPL-001 `detect_tpl_001`, merge-once-per-edited-template, and the
    /// stale-clear reconciliation) and calls `observe_diagnostics` for each affected
    /// URI in the identical order. The only behavior the caller must add is pushing
    /// the returned `(uri, diagnostics)` pairs to the editor via
    /// `Client::publish_diagnostics`, in order. Because the receipt chain is written
    /// to the on-disk OCEL log by `observe_diagnostics`, the full living loop
    /// (DiagnosticRaised → RouteSelected → RepairSuggested → RepairApplied →
    /// GatePassed → ReceiptEmitted) is observable from a hermetic test driving this
    /// method on a `with_root(TempDir)` state — no editor transport involved.
    ///
    /// Strictly read-only with respect to the project: builds an index (which only
    /// reads files) and never writes any artifact.
    pub async fn analyze_and_observe(
        &self, uri: &Url, content: &str,
    ) -> Vec<(Url, Vec<Diagnostic>)> {
        let mut published: Vec<(Url, Vec<Diagnostic>)> = Vec::new();
        let file_type = FileType::from_path(uri.path());

        // LIVE-BUFFER-001: snapshot the open buffers as a disk-path→content overlay
        // for the cross-surface index, AND splice in the edited `(uri, content)`
        // pair. The edited buffer's NEW content may not yet be in `self.documents`
        // (the server's `did_change` insert vs. this call's ordering is not
        // guaranteed), so inserting it here makes a producer edit (e.g. the `.rq`)
        // visible to its consumer (the `.tera`) WITHIN this same call — the exact
        // buffer-liveness the living loop requires. With no buffers open this is just
        // the edited pair (or empty), so the disk path stays the fallback.
        let mut overlay = self.buffer_overlay().await;
        if let Ok(p) = uri.to_file_path() {
            overlay.insert(p, content.to_string());
        }

        // Single-file diagnostics for the edited document (E0024 etc.).
        let mut own_diags: Vec<Diagnostic> = Vec::new();
        if let Some(analyzer) = crate::analyzers::build_analyzer(uri.path(), content) {
            own_diags = analyzer.diagnostics();
            self.set_analyzer(uri.clone(), analyzer).await;
        }

        // Add boundary compliance observer diagnostics
        let observer_diags = crate::handlers::diagnostics::compute_observer_diagnostics(uri, content);
        own_diags.extend(observer_diags);

        // Cross-file GGEN-TPL-001: for rule-referenced surfaces (`.tera`/`.rq`) or
        // the ggen project manifest specifically. Both `ggen.toml` and `Cargo.toml`
        // classify as `FileType::Toml`, so we route by BASENAME: only `ggen.toml`
        // triggers the TPL path. (`detect_tpl_001_for` itself requires a real
        // `ggen.toml` via `ProjectIndex::from_root`, so a `Cargo.toml`-only dir was
        // already silent — this tightening just makes the intent explicit and keeps
        // HARNESS edits from spuriously recomputing the TPL graph.)
        let tpl_is_trigger =
            matches!(file_type, FileType::Tera | FileType::Sparql) || is_ggen_manifest(uri.path());
        let tpl_groups = if tpl_is_trigger {
            self.detect_tpl_001_for(uri, &overlay)
        } else {
            Vec::new()
        };

        // Cross-file GGEN-HARNESS-001: only for harness declaration surfaces
        // (`Cargo.toml`/`Makefile.toml`). Disjoint from the TPL trigger by basename,
        // so a TPL fixture raises zero HARNESS and a HARNESS fixture raises zero TPL.
        let harness_is_trigger = is_harness_surface(uri.path());
        let harness_groups = if harness_is_trigger {
            self.detect_harness_001_for(uri, &overlay)
        } else {
            Vec::new()
        };

        // Cross-file GGEN-OUT-001: the dual of TPL-001 on the ggen.toml/SPARQL
        // surfaces. Shares the SAME trigger gate as TPL (`tpl_is_trigger` covers
        // `.tera`/`.rq`/`ggen.toml`), since both read the SAME `ProjectIndex`: an
        // edit to any source-law surface re-evaluates OUT-001 too. Groups anchor on
        // `ggen.toml` (disjoint from TPL's `.tera` anchor).
        let out_groups = if tpl_is_trigger {
            self.detect_out_001_for(uri, &overlay)
        } else {
            Vec::new()
        };

        // Cross-file GGEN-RULE-001: the FOUNDATIONAL binding-integrity check
        // TPL/OUT presuppose — a rule whose query/template {file=...} is missing.
        // Shares the SAME trigger gate as TPL/OUT (`tpl_is_trigger` covers
        // `.tera`/`.rq`/`ggen.toml`): all read the SAME `ProjectIndex`. Groups
        // anchor on `ggen.toml` (disjoint from TPL's `.tera` anchor).
        let rule_groups = if tpl_is_trigger {
            self.detect_rule_001_for(uri, &overlay)
        } else {
            Vec::new()
        };

        let edited_self_url = url_from_path_str(uri.path());
        let mut published_self = false;

        // CONSOLIDATE-002: the three former order-sensitive publish branches (TPL /
        // HARNESS / OUT) are now ONE species-driven loop over a function-local,
        // ORDER-FIXED `[TPL, HARNESS, OUT]` descriptor array. A 4th species becomes a
        // registry entry + detector, NOT another publish branch.
        //
        // The loop replicates the exact pre-merge emit order (verified by the
        // CONSOLIDATE-002 sequence-equivalence golden):
        //   * species iterate in the FIXED order TPL → HARNESS → OUT (Section I1);
        //   * within a species, groups iterate in detector-returned order (I6);
        //   * `own_diags` + `published_self` are SHARED across species, so the edited
        //     file's single-file diagnostics merge into AT MOST ONE species' self
        //     publish (I3 merge-once); the self-merge guard `edited == anchor &&
        //     !published_self` is now UNIFORM across all three species. This is
        //     behavior-preserving: the former TPL/HARNESS branches lacked the
        //     `!published_self` clause only because their anchors are structurally
        //     disjoint from any co-firing species' self-edit (HARNESS basenames are
        //     disjoint from TPL/OUT; TPL anchors on `.tera`, never the edited
        //     `ggen.toml`), so the extra guard can never fire spuriously here — yet it
        //     makes first-species-wins merge-once the explicit, asserted rule.
        //   * all group publishes (Phase A) precede the `!published_self` own-diags
        //     fallback (Phase B) which precedes every stale-clear (Phase C) (I2).
        //
        // Each species carries its clear `code`, its `is_trigger` gate, its detector
        // `groups`, and an accumulating `flagged` set fed to `clears_for` in Phase C.
        struct Species {
            code: &'static str,
            is_trigger: bool,
            groups: Vec<(PathBuf, Vec<Diagnostic>)>,
            flagged: HashSet<Url>,
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
            // GGEN-RULE-001 appended LAST: with no missing files (the common case,
            // e.g. the CONSOLIDATE-002 golden) it emits zero groups, so the
            // TPL→HARNESS→OUT emit sequence stays byte-identical and the
            // sequence-equivalence golden holds without editing it.
            Species {
                code: crate::analyzers::GGEN_RULE_001,
                is_trigger: tpl_is_trigger,
                groups: rule_groups,
                flagged: HashSet::new(),
            },
        ];

        // PHASE A — group publishes, species in fixed order, groups in native order.
        for sp in &mut species {
            for (anchor_path, diags) in std::mem::take(&mut sp.groups) {
                let Some(anchor_url) = url_from_path(&anchor_path) else {
                    continue;
                };
                sp.flagged.insert(anchor_url.clone());
                if edited_self_url.as_ref() == Some(&anchor_url) && !published_self {
                    // The edited file IS this species' anchor: merge `own_diags` in
                    // once, publish once, mark self-published (merge-once, I3).
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

        // PHASE B — if the edited file was not itself a cross-surface-affected file,
        // observe its own single-file diagnostics (preserving the original
        // single-file flow). Strictly between Phase A and Phase C (I2).
        if !published_self {
            self.observe_diagnostics(uri, &own_diags).await;
            published.push((uri.clone(), own_diags));
        }

        // PHASE C — STALE-CLEAR reconciliation, species in the SAME fixed order
        // (TPL → HARNESS → OUT). A cross-surface repair makes a previously-flagged
        // anchor lawful, dropping it from this pass's groups; its URI is re-published
        // with its RESIDUAL single-file diagnostics (NOT empty) so the per-key diff
        // in `observe_diagnostics` drops only the disappeared species key while
        // preserving unrelated law. Each species is gated by its own `is_trigger` and
        // keyed by its own `code` (so TPL clears, then HARNESS clears, then OUT clears
        // — the clear-phase order is NOT collapsed across species).
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

    /// Recompute a template's OWN single-file diagnostics (E0024 etc.) for use when
    /// clearing a disappeared cross-surface GGEN-TPL-001. The single-file Tera
    /// analyzer runs with empty SPARQL bindings, so the result NEVER contains
    /// GGEN-TPL-001 — observing it therefore drops only the disappeared key while
    /// preserving any independent diagnostics on the same template. Content comes
    /// from the open document if available, else the file on disk. Read-only.
    async fn residual_single_file_diags(&self, uri: &Url) -> Vec<Diagnostic> {
        let content = match self.get_document(uri).await {
            Some(c) => c,
            None => match uri.to_file_path() {
                Ok(path) => std::fs::read_to_string(&path).unwrap_or_default(),
                Err(()) => String::new(),
            },
        };
        crate::analyzers::build_analyzer(uri.path(), &content)
            .map(|a| a.diagnostics())
            .unwrap_or_default()
    }

    /// Resolve the project root for `uri`, build a `ProjectIndex`, and run the
    /// cross-surface GGEN-TPL-001 detector. Returns the per-template diagnostic
    /// groups (empty on any resolution/build failure — best-effort, never panics,
    /// never writes files).
    fn detect_tpl_001_for(
        &self, uri: &Url, overlay: &BufferOverlay,
    ) -> Vec<(PathBuf, Vec<Diagnostic>)> {
        let Some(root) = self.project_root_for(uri) else {
            return Vec::new();
        };
        match crate::project_index::ProjectIndex::from_root_with_overlay(&root, overlay) {
            Ok(project) => crate::analyzers::detect_tpl_001(&project),
            Err(_) => Vec::new(),
        }
    }

    /// Resolve the project root for `uri`, build a `ProjectIndex`, and run the
    /// cross-surface GGEN-OUT-001 detector (the dual of [`Self::detect_tpl_001_for`]
    /// on the `ggen.toml`/SPARQL surfaces). Returns the per-manifest diagnostic
    /// groups (empty on any resolution/build failure — best-effort, never panics,
    /// never writes files). Reuses the SAME `project_root_for` + `ProjectIndex` as
    /// TPL-001.
    fn detect_out_001_for(
        &self, uri: &Url, overlay: &BufferOverlay,
    ) -> Vec<(PathBuf, Vec<Diagnostic>)> {
        let Some(root) = self.project_root_for(uri) else {
            return Vec::new();
        };
        match crate::project_index::ProjectIndex::from_root_with_overlay(&root, overlay) {
            Ok(project) => crate::analyzers::detect_out_001(&project),
            Err(_) => Vec::new(),
        }
    }

    /// Resolve the project root for `uri`, build a `ProjectIndex`, and run the
    /// cross-surface GGEN-RULE-001 detector (the foundational binding-integrity
    /// check TPL/OUT presuppose — a rule whose query/template `{file=...}` is
    /// missing). Returns the per-manifest diagnostic groups (empty on any
    /// resolution/build failure — best-effort, never panics, never writes files).
    /// Reuses the SAME `project_root_for` + `ProjectIndex` as TPL/OUT-001.
    fn detect_rule_001_for(
        &self, uri: &Url, overlay: &BufferOverlay,
    ) -> Vec<(PathBuf, Vec<Diagnostic>)> {
        let Some(root) = self.project_root_for(uri) else {
            return Vec::new();
        };
        match crate::project_index::ProjectIndex::from_root_with_overlay(&root, overlay) {
            Ok(project) => crate::analyzers::detect_rule_001(&project),
            Err(_) => Vec::new(),
        }
    }

    /// Find the project root for a document: walk up parent directories from the
    /// file path to the nearest directory containing a `ggen.toml`. Falls back to
    /// `self.root` when the URI is not a local file path or no manifest is found
    /// above it.
    fn project_root_for(&self, uri: &Url) -> Option<PathBuf> {
        if let Ok(file_path) = uri.to_file_path() {
            let mut dir: Option<&Path> = file_path.parent();
            while let Some(d) = dir {
                if d.join("ggen.toml").is_file() {
                    return Some(d.to_path_buf());
                }
                dir = d.parent();
            }
        }
        // Fallback: the server's configured root, if it holds a manifest.
        let fallback = self.root.clone();
        if fallback.join("ggen.toml").is_file() {
            Some(fallback)
        } else {
            None
        }
    }

    /// Resolve the crate root for `uri`, build a [`crate::harness_index::HarnessIndex`],
    /// and run the cross-surface GGEN-HARNESS-001 detector. Returns the per-manifest
    /// diagnostic groups (empty on any resolution/build failure — best-effort, never
    /// panics, never writes files).
    fn detect_harness_001_for(
        &self, uri: &Url, overlay: &BufferOverlay,
    ) -> Vec<(PathBuf, Vec<Diagnostic>)> {
        let Some(root) = self.harness_root_for(uri) else {
            return Vec::new();
        };
        match crate::harness_index::HarnessIndex::from_root_with_overlay(&root, overlay) {
            Ok(index) => crate::analyzers::detect_harness_001(&index),
            Err(_) => Vec::new(),
        }
    }

    /// Find the crate root for a harness surface: walk up parent directories from
    /// the file path to the nearest directory containing a `Cargo.toml`. Falls back
    /// to `self.root` when the URI is not a local file path or no manifest is found
    /// above it (only if that root itself holds a `Cargo.toml`).
    fn harness_root_for(&self, uri: &Url) -> Option<PathBuf> {
        if let Ok(file_path) = uri.to_file_path() {
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

/// Convert a filesystem path to a `file://` `Url`, or `None` if it is not an
/// absolute path `Url::from_file_path` accepts.
fn url_from_path(path: &Path) -> Option<Url> {
    Url::from_file_path(path).ok()
}

/// Convert a `Url::path()` string back to a `file://` `Url`. `Url::path()` yields
/// an absolute, percent-encoded path; round-tripping it lets us compare a document
/// URI against the `template_path` reported by the detector.
fn url_from_path_str(path: &str) -> Option<Url> {
    let decoded = percent_decode_path(path);
    Url::from_file_path(&decoded).ok()
}

/// Minimal percent-decoding for the `%XX` sequences `Url::path()` may contain
/// (e.g. spaces). Avoids pulling in an extra dependency for the common cases.
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
