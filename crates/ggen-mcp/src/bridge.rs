//! `ggen-lsp` -> `ggen-mcp` push bridge (CP12).
//!
//! `ggen-lsp`'s headless gate (`ggen_lsp::check::check_files_in_root`) is the
//! real, existing source of diagnostics -- this module does not duplicate or
//! re-implement any analyzer. It is a thin, one-directional adapter: run the
//! real gate, pick out diagnostics matching a caller-chosen code allowlist,
//! publish their content as MCP resources, and push a real
//! `notifications/resources/updated` for each one over a retained
//! `Peer<RoleServer>`.
//!
//! Two channels live in this module and they are NOT layers of the same
//! signal -- `DiagnosticStore` and `SyncRefusalStore` answer different
//! questions and neither supersedes the other:
//!
//! - `DiagnosticStore` / `push_diagnostics_for_root`: **author-time**
//!   `GGEN-*` static-analysis diagnostics (`GGEN-TPL-001` etc.) from
//!   `ggen-lsp`'s analyzers -- "is this template/frontmatter well-formed",
//!   checked without running a real sync.
//! - `SyncRefusalStore` / `push_sync_refusal_for_root`: **sync-time** `FM-*`
//!   dry-run gate refusals (`FM-PACK-*`/`FM-WRITE-*`/`FM-TPL-*`/`FM-LAW-*`)
//!   from a real `ggen_engine::sync` dry run -- "would a real sync succeed
//!   right now", which the `GGEN-*` diagnostics above do not and cannot
//!   answer (a file can pass every author-time analyzer and still fail a
//!   cross-pack gate or a rule referencing an undeclared pack). Do not treat
//!   a clean `GGEN-*` diagnostic sweep as evidence a sync will pass, and do
//!   not wire one store to subsume the other -- they are independent,
//!   non-overlapping checks, both real, both worth pushing.
//!
//! Chosen first signal: `GGEN-TPL-001` (unbound Tera projection), emitted by
//! `crates/ggen-lsp/src/analyzers/tera_analyzer.rs` and folded into the
//! cross-surface gate by `check.rs::fold_tpl_001`. Reasons or ONE, not
//! several: (1) it is a cross-surface, project-level diagnostic --
//! `check_files_in_root` already builds a real `ProjectIndex` to detect it,
//! so this bridge exercises the same code path a real ggen project uses; (2)
//! `check.rs`'s own test suite
//! (`root_aware_gate_folds_tpl_001_and_fails`) already establishes a proven,
//! minimal fixture shape (one `ggen.toml`, one `.tera` template with an
//! unbound var) this module's own test reuses verbatim, rather than inventing
//! a new one; (3) `GGEN-FM-SHACL-001` (CP8) requires a SHACL shape file and
//! the GraphLaw engine wired in, which is more moving parts for the same
//! proof of the push path. Nothing here is specific to TPL-001's code path,
//! though -- `codes` is a caller-supplied allowlist, so wiring
//! `GGEN-OUT-001`/`GGEN-FM-SHACL-001` later is an argument change, not a
//! rewrite.

use std::collections::{HashMap, VecDeque};
use std::path::{Path, PathBuf};
use std::sync::{Arc, LazyLock};
use std::time::{Duration, Instant};

use ggen_engine::error::extract_fm_code;
use rmcp::model::{Resource, ResourceUpdatedNotificationParam};
use rmcp::service::Peer;
use rmcp::RoleServer;
use tokio::sync::Mutex;

/// CP39/R2: a `static` map of one circuit breaker per project root (not a
/// single shared breaker -- R2 fixed a real bug where two unrelated
/// projects' dispatch attempts shared one rate-limit budget), rather than
/// threading a new parameter through `watch()`'s signature (which would
/// ripple into `lib.rs` and every other call site) since this bridge
/// module is the sole real caller of the CP39 wiring.
static DISPATCH_BREAKERS: LazyLock<crate::tools::unattended_dispatch::PerRootCircuitBreaker> =
    LazyLock::new(crate::tools::unattended_dispatch::PerRootCircuitBreaker::new);

/// Default cap on how many `PushedDiagnostic`s `DiagnosticStore` retains at
/// once (CP18). Unbounded growth was the real gap: a large refactor can fire
/// hundreds of diagnostics per gate run, and with no cap the store's memory
/// grows without limit for the lifetime of the server process. `500` is an
/// arbitrary but generous ceiling -- one full gate run's worth of
/// diagnostics on a large project, several times over.
pub const DEFAULT_MAX_ENTRIES: usize = 500;

/// Default minimum interval between two *notifications* for the same
/// logical diagnostic (same `file`+`code`, independent of the per-run `idx`
/// suffix) (CP18). Rapid-fire re-pushes of the same diagnostic (e.g. an
/// editor firing the gate on every keystroke during a large refactor) are
/// coalesced: the store is still updated with the latest state on every
/// call, but only one `notifications/resources/updated` goes out per
/// window.
pub const DEFAULT_DEBOUNCE: Duration = Duration::from_millis(200);

/// One pushed diagnostic, stored so a subscribing client's follow-up
/// `resources/read` for the URI named in the notification returns real
/// content -- `notify_resource_updated` itself carries only a URI (rmcp
/// 1.8.0's `ResourceUpdatedNotificationParam { uri: String }`, confirmed by
/// reading `rmcp-1.8.0/src/model.rs:1356`), so the resource body has to live
/// somewhere the server's `read_resource` handler can find it.
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct PushedDiagnostic {
    pub file: String,
    pub code: String,
    pub message: String,
    pub range: lsp_max::lsp_types::Range,
}

/// Backing state for `DiagnosticStore` -- a single `Mutex` guards both the
/// bounded diagnostic map and the debounce clock so a caller can never
/// observe them out of sync with each other (e.g. a store update racing a
/// debounce decision for the same key).
#[derive(Debug)]
struct Inner {
    /// Diagnostic content keyed by resource URI.
    map: HashMap<String, PushedDiagnostic>,
    /// Insertion order of `map`'s keys, oldest first -- backs FIFO eviction
    /// once `map.len() > max_entries`. A real, observable bound: eviction
    /// removes the oldest *entry*, not a random one, and is exercised by
    /// `diagnostic_store_stays_bounded_under_rapid_growth`.
    order: VecDeque<String>,
    max_entries: usize,
    /// Last time a *notification* went out for a given logical diagnostic
    /// key (`{file}#{code}`, independent of the per-run `idx` suffix so
    /// repeated occurrences of the same diagnostic across successive gate
    /// runs share one debounce clock). Storing content is never debounced --
    /// only this clock, consulted by `should_notify`, gates the broadcast.
    last_notified: HashMap<String, Instant>,
    debounce: Duration,
}

/// In-memory store backing `GgenMcpServer::list_resources`/`read_resource`
/// for diagnostic-resource URIs. Cleared on server restart -- these are
/// live-session facts about the current gate run, not persisted state.
///
/// Bounded (CP18): retains at most `max_entries` diagnostics, evicting the
/// oldest by insertion order once the cap is exceeded -- unbounded growth
/// was a real gap (a large refactor firing hundreds of diagnostics per gate
/// run had nothing capping the store's lifetime memory use). Also owns the
/// debounce clock (`should_notify`) used by `push_diagnostics_for_root` to
/// coalesce rapid-fire re-pushes of the same logical diagnostic into a
/// single notification per window.
#[derive(Debug, Clone)]
pub struct DiagnosticStore(Arc<Mutex<Inner>>);

impl Default for DiagnosticStore {
    fn default() -> Self {
        Self::with_limits(DEFAULT_MAX_ENTRIES, DEFAULT_DEBOUNCE)
    }
}

impl DiagnosticStore {
    #[must_use]
    pub fn new() -> Self {
        Self::default()
    }

    /// Construct a store with an explicit entry cap and debounce interval --
    /// the knobs `new()` defaults from `DEFAULT_MAX_ENTRIES`/
    /// `DEFAULT_DEBOUNCE`. Exposed so tests (and callers with different
    /// load profiles) can exercise the real bound/debounce behavior on a
    /// timescale a test can actually observe.
    #[must_use]
    pub fn with_limits(max_entries: usize, debounce: Duration) -> Self {
        Self(Arc::new(Mutex::new(Inner {
            map: HashMap::new(),
            order: VecDeque::new(),
            max_entries: max_entries.max(1),
            last_notified: HashMap::new(),
            debounce,
        })))
    }

    /// Store (or overwrite) one diagnostic's content, evicting the oldest
    /// entry by insertion order if this insert would exceed `max_entries`.
    /// Always succeeds -- storing content is never debounced, only the
    /// notification is (see `should_notify`), so a caller re-pushing the
    /// same URI's latest state always sees it reflected here even during a
    /// debounce window.
    pub async fn insert(&self, uri: String, diag: PushedDiagnostic) {
        let mut inner = self.0.lock().await;
        if !inner.map.contains_key(&uri) {
            inner.order.push_back(uri.clone());
        }
        inner.map.insert(uri, diag);
        while inner.order.len() > inner.max_entries {
            if let Some(oldest) = inner.order.pop_front() {
                inner.map.remove(&oldest);
                inner.last_notified.remove(&oldest);
            } else {
                break;
            }
        }
    }

    pub async fn get(&self, uri: &str) -> Option<PushedDiagnostic> {
        self.0.lock().await.map.get(uri).cloned()
    }

    pub async fn list(&self) -> Vec<(String, PushedDiagnostic)> {
        self.0
            .lock()
            .await
            .map
            .iter()
            .map(|(k, v)| (k.clone(), v.clone()))
            .collect()
    }

    /// Number of diagnostics currently retained -- always `<= max_entries`.
    pub async fn len(&self) -> usize {
        self.0.lock().await.map.len()
    }

    pub async fn is_empty(&self) -> bool {
        self.len().await == 0
    }

    /// Debounce decision for one logical diagnostic `key` (caller passes
    /// `{file}#{code}`, not the per-occurrence URI, so repeated pushes of
    /// the same diagnostic across successive calls share one clock
    /// regardless of the `idx` suffix a given run assigns it). Returns
    /// `true` (and starts a fresh window) the first time a key is seen, or
    /// whenever at least `debounce` has elapsed since the last `true`
    /// result for that key; returns `false` -- without advancing the clock
    /// -- for every call inside the window. This is real coalescing, not a
    /// counter: a burst of N calls inside one window yields exactly one
    /// `true`.
    async fn should_notify(&self, key: &str) -> bool {
        let mut inner = self.0.lock().await;
        let now = Instant::now();
        let allow = match inner.last_notified.get(key) {
            Some(last) => now.duration_since(*last) >= inner.debounce,
            None => true,
        };
        if allow {
            inner.last_notified.insert(key.to_string(), now);
        }
        allow
    }
}

/// Build the resource URI for one diagnostic occurrence. `idx` disambiguates
/// multiple diagnostics of the same code in the same file.
fn diagnostic_uri(file: &str, code: &str, idx: usize) -> String {
    format!("ggen-diagnostic://{file}#{code}-{idx}")
}

/// Registry of every currently-retained `Peer<RoleServer>` this bridge can
/// push notifications to (CP16). One MCP server process may serve more than
/// one concurrent client connection (e.g. multiple `serve()` calls over
/// separate stdio/transport instances sharing one `GgenMcpServer` clone, or
/// a reconnect after a client drops) -- a single retained `Peer` (CP12's
/// `start_stdio`) cannot represent that, so this holds a `Vec` instead.
///
/// `rmcp` gives no disconnect callback, so dead peers are not pruned
/// eagerly -- `Peer::is_transport_closed` (backed by the peer's own
/// `mpsc::Sender::is_closed`, see `rmcp::service::Peer`) is the only real
/// signal, and `broadcast`/`prune_and_count` check it lazily, immediately
/// before use.
#[derive(Debug, Clone, Default)]
pub struct PeerRegistry(Arc<Mutex<Vec<Peer<RoleServer>>>>);

impl PeerRegistry {
    #[must_use]
    pub fn new() -> Self {
        Self::default()
    }

    /// Retain a newly-connected (or reconnected) peer. Safe to call multiple
    /// times across the life of a server: each `serve()` call (including a
    /// reconnect after a prior client disconnected) contributes its own
    /// `Peer`, and stale entries are pruned lazily by `broadcast`/
    /// `prune_and_count` rather than removed here.
    pub async fn add(&self, peer: Peer<RoleServer>) {
        self.0.lock().await.push(peer);
    }

    /// Drop every peer whose transport has closed, then report the number
    /// still live. Exposed mainly for tests -- production callers get the
    /// same pruning for free from `broadcast`.
    pub async fn prune_and_count(&self) -> usize {
        let mut peers = self.0.lock().await;
        peers.retain(|p| !p.is_transport_closed());
        peers.len()
    }

    /// Send one notification to every currently-live peer, pruning dead
    /// ones first. Returns the number of peers it was actually delivered to
    /// -- `0` is not an error by itself (it is the queued-but-unsubscribed
    /// case: nobody has connected/retained a peer yet, so the push is
    /// silently dropped rather than buffered or queued -- see
    /// `push_diagnostics_for_root`'s doc comment for the rationale). This
    /// method returns `Err` only when at least one live peer's send
    /// genuinely failed (e.g. it closed its transport in the race between
    /// the prune and the send) -- a real communication failure on a peer
    /// that looked alive a moment ago, not merely "nobody was listening".
    async fn broadcast(&self, param: &ResourceUpdatedNotificationParam) -> anyhow::Result<usize> {
        use tracing::Instrument as _;

        let notify_span = tracing::info_span!(
            "mcp.push.notify",
            "operation.name" = "mcp.push.notify",
            "operation.type" = "mcp",
            "mcp.push.uri" = %param.uri,
            "mcp.push.peer_count" = tracing::field::Empty,
            "mcp.push.delivered" = tracing::field::Empty,
            "mcp.push.result" = tracing::field::Empty,
        );

        async {
            let mut peers = self.0.lock().await;
            peers.retain(|p| !p.is_transport_closed());
            tracing::Span::current().record("mcp.push.peer_count", peers.len() as u64);
            let mut delivered = 0usize;
            let mut first_err: Option<anyhow::Error> = None;
            for peer in peers.iter() {
                match peer.notify_resource_updated(param.clone()).await {
                    Ok(()) => delivered += 1,
                    Err(e) => {
                        if first_err.is_none() {
                            first_err = Some(e.into());
                        }
                    }
                }
            }
            tracing::Span::current().record("mcp.push.delivered", delivered as u64);
            if delivered == 0 {
                if let Some(e) = first_err {
                    tracing::Span::current().record("mcp.push.result", "failure");
                    tracing::warn!(uri = %param.uri, error = %e, "mcp.push.notify failed: no peer delivery succeeded");
                    return Err(e);
                }
            }
            let result = if delivered > 0 { "success" } else { "no_peers" };
            tracing::Span::current().record("mcp.push.result", result);
            tracing::info!(
                delivered,
                result,
                "mcp.push.notify: client notification attempt finished"
            );
            Ok(delivered)
        }
        .instrument(notify_span)
        .await
    }
}

/// Run the real headless gate (`ggen_lsp::check::check_files_in_root`) over
/// `paths` under `root`, then for every diagnostic whose `code` is in
/// `codes`: store its content in `store` (always -- so a client that
/// connects later can still `resources/read` it) and broadcast a real
/// `notifications/resources/updated` to every peer currently retained in
/// `peers` (CP16: zero, one, or many).
///
/// **Queued-but-unsubscribed semantics (CP16):** this bridge does not
/// implement MCP's `resources/subscribe` handshake (`GgenMcpServer` does not
/// override `ServerHandler::subscribe`/`unsubscribe`, so a client that calls
/// it gets the default `method_not_found` -- confirmed by reading
/// `rmcp::handler::server`'s default trait impl). A push therefore always
/// goes to every retained peer, subscribed or not; if `peers` is empty
/// (nobody has connected yet, or the last connected client already
/// disconnected), the diagnostic is stored but the notification is dropped,
/// not buffered or queued for a future connection -- a client that connects
/// afterward finds nothing pushed for it, though `resources/list` +
/// `resources/read` still surface the stored diagnostic on request. This is
/// a deliberate choice, not an oversight: buffering would require a
/// caller-chosen eviction policy this bridge has no basis to pick, and the
/// stored diagnostic is never lost (see `DiagnosticStore`) -- only the push
/// notification is.
///
/// **Bound + debounce (CP18):** `store` caps how many diagnostics it
/// retains (`DiagnosticStore::max_entries`, FIFO eviction of the oldest),
/// and coalesces rapid re-pushes of the same logical diagnostic
/// (`{file}#{code}`) into at most one notification per debounce window
/// (`DiagnosticStore::should_notify`) -- content is always stored fresh,
/// but a caller hammering this function for the same file/code (e.g. an
/// editor firing the gate on every keystroke during a large refactor) does
/// not get a 1:1 notification for every call.
///
/// Returns the number of diagnostics that were pushed to at least one live
/// peer AND (as a separate figure inside `PushOutcome`) the total number of
/// individual peer deliveries. Never fabricates a diagnostic -- zero real
/// matches means zero notifications, reported honestly via the return value
/// rather than a synthesized push.
///
/// # Errors
/// Returns an error only if a `notify_resource_updated` call to a peer that
/// looked live at broadcast time genuinely fails (e.g. it disconnected in
/// the race between the liveness check and the send) -- a real
/// communication failure, never "no diagnostics found" (`Ok(PushOutcome
/// {matched: 0, ..})`) and never "no peers currently connected"
/// (`Ok(PushOutcome {delivered_notifications: 0, ..})`, see the
/// queued-but-unsubscribed note above).
pub async fn push_diagnostics_for_root(
    peers: &PeerRegistry, store: &DiagnosticStore, root: &Path, paths: &[PathBuf], codes: &[&str],
) -> anyhow::Result<PushOutcome> {
    let report = ggen_lsp::check::check_files_in_root(root, paths, false);
    let mut matched = 0usize;
    let mut delivered_notifications = 0usize;

    for file in &report.files {
        let mut idx_by_code: HashMap<&str, usize> = HashMap::new();
        for diag in &file.diagnostics {
            let code_str = match &diag.code {
                Some(lsp_max::lsp_types::NumberOrString::String(s)) => s.as_str(),
                _ => continue,
            };
            if !codes.contains(&code_str) {
                continue;
            }
            let idx = idx_by_code.entry(code_str).or_insert(0);
            let uri = diagnostic_uri(&file.path, code_str, *idx);
            *idx += 1;

            let fire_span = tracing::info_span!(
                "mcp.push.fire",
                "operation.name" = "mcp.push.fire",
                "operation.type" = "mcp",
                "mcp.push.code" = code_str,
                "mcp.push.uri" = %uri,
                "mcp.push.file" = %file.path,
            );
            {
                let _fire_guard = fire_span.enter();
                tracing::info!("mcp.push.fire: diagnostic matched allowlist");
            }

            store
                .insert(
                    uri.clone(),
                    PushedDiagnostic {
                        file: file.path.clone(),
                        code: code_str.to_string(),
                        message: diag.message.clone(),
                        range: diag.range,
                    },
                )
                .await;

            matched += 1;

            // CP18 debounce: the logical diagnostic identity for throttling
            // is `{file}#{code}`, deliberately not the per-occurrence `uri`
            // (which already carries a run-local `idx`) -- a caller polling
            // the same file/code rapidly (e.g. an editor firing the gate on
            // every keystroke) should share one debounce clock across
            // successive calls, not get a fresh one because `idx` reset to
            // 0 on the next call. The store above was already updated with
            // this occurrence's latest content regardless of the debounce
            // outcome; only the broadcast is gated.
            let debounce_key = format!("{}#{code_str}", file.path);
            if store.should_notify(&debounce_key).await {
                delivered_notifications += peers
                    .broadcast(&ResourceUpdatedNotificationParam::new(uri))
                    .await?;
            }
        }
    }

    Ok(PushOutcome {
        matched,
        delivered_notifications,
    })
}

/// Result of one `push_diagnostics_for_root` call. `matched` counts real
/// diagnostics found and stored regardless of whether any peer was
/// connected to receive a push for them; `delivered_notifications` counts
/// the individual peer deliveries (so with 2 live peers and 3 matched
/// diagnostics, a fully successful run reports `delivered_notifications:
/// 6`).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct PushOutcome {
    pub matched: usize,
    pub delivered_notifications: usize,
}

/// Build the `Resource` listing entries for every URI currently in `store`
/// (backs `list_resources`).
pub async fn list_resources(store: &DiagnosticStore) -> Vec<Resource> {
    store
        .list()
        .await
        .into_iter()
        .map(|(uri, diag)| {
            Resource::new(
                rmcp::model::RawResource::new(uri, format!("{}: {}", diag.code, diag.file)),
                None,
            )
        })
        .collect()
}

/// One pushed sync-dry-run refusal (CP28): either a real `sync()` `Err`
/// (its FM-* code already embedded in the `Display` text, since `AppError`
/// has no typed FM-code field -- see this module's `push_sync_refusal_for_root`
/// doc comment) or a non-routine typed skip from a successful dry run's
/// `report.decisions`, classified via `crate::tools::skip_classify::classify`.
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct PushedSyncRefusal {
    /// Project root this refusal came from (root-relative paths inside
    /// `message` are relative to this).
    pub root: String,
    /// The engine's own error/skip-reason text, carried verbatim -- the
    /// real FM-* code (when present) lives inside this string.
    pub message: String,
    /// `"error"` for a real `sync()` `Err`, or the typed skip category
    /// (currently only `"other"`, per this checkpoint's working default --
    /// see the module doc) for an `Ok` dry run with a non-routine skip.
    pub kind: String,
}

/// Backing state for `SyncRefusalStore` -- a literal copy of
/// `DiagnosticStore`'s `Inner` shape (map + insertion order + bounded
/// eviction + debounce clock), carrying `PushedSyncRefusal` instead of
/// `PushedDiagnostic`. Deliberately not unified with `DiagnosticStore`
/// behind a shared trait: two concrete cases do not justify the
/// abstraction, and the payload types are unrelated.
#[derive(Debug)]
struct SyncRefusalInner {
    map: HashMap<String, PushedSyncRefusal>,
    order: VecDeque<String>,
    max_entries: usize,
    last_notified: HashMap<String, Instant>,
    debounce: Duration,
}

/// In-memory store backing `GgenMcpServer::list_resources`/`read_resource`
/// for `ggen-sync-refusal://` resource URIs (CP28). Cleared on server
/// restart, same lifecycle as `DiagnosticStore`.
#[derive(Debug, Clone)]
pub struct SyncRefusalStore(Arc<Mutex<SyncRefusalInner>>);

impl Default for SyncRefusalStore {
    fn default() -> Self {
        Self::with_limits(DEFAULT_MAX_ENTRIES, DEFAULT_DEBOUNCE)
    }
}

impl SyncRefusalStore {
    #[must_use]
    pub fn new() -> Self {
        Self::default()
    }

    /// Construct a store with an explicit entry cap and debounce interval,
    /// mirroring `DiagnosticStore::with_limits`.
    #[must_use]
    pub fn with_limits(max_entries: usize, debounce: Duration) -> Self {
        Self(Arc::new(Mutex::new(SyncRefusalInner {
            map: HashMap::new(),
            order: VecDeque::new(),
            max_entries: max_entries.max(1),
            last_notified: HashMap::new(),
            debounce,
        })))
    }

    pub async fn insert(&self, uri: String, refusal: PushedSyncRefusal) {
        let mut inner = self.0.lock().await;
        if !inner.map.contains_key(&uri) {
            inner.order.push_back(uri.clone());
        }
        inner.map.insert(uri, refusal);
        while inner.order.len() > inner.max_entries {
            if let Some(oldest) = inner.order.pop_front() {
                inner.map.remove(&oldest);
                inner.last_notified.remove(&oldest);
            } else {
                break;
            }
        }
    }

    pub async fn get(&self, uri: &str) -> Option<PushedSyncRefusal> {
        self.0.lock().await.map.get(uri).cloned()
    }

    pub async fn list(&self) -> Vec<(String, PushedSyncRefusal)> {
        self.0
            .lock()
            .await
            .map
            .iter()
            .map(|(k, v)| (k.clone(), v.clone()))
            .collect()
    }

    pub async fn len(&self) -> usize {
        self.0.lock().await.map.len()
    }

    pub async fn is_empty(&self) -> bool {
        self.len().await == 0
    }

    async fn should_notify(&self, key: &str) -> bool {
        let mut inner = self.0.lock().await;
        let now = Instant::now();
        let allow = match inner.last_notified.get(key) {
            Some(last) => now.duration_since(*last) >= inner.debounce,
            None => true,
        };
        if allow {
            inner.last_notified.insert(key.to_string(), now);
        }
        allow
    }
}

/// Build the resource URI for one pushed sync refusal. `key` is either
/// `"error"` (at most one per root -- a `sync()` `Err` aborts the whole
/// run, so there is only ever one) or the skipped output's root-relative
/// path (one refusal per non-routine typed skip).
fn sync_refusal_uri(root: &Path, key: &str) -> String {
    format!("ggen-sync-refusal://{}#{key}", root.display())
}

/// Run a real dry-run `sync(root, SyncOptions{dry_run:true,..})` and push
/// any refusal it surfaces (CP28).
///
/// Two cases:
/// - **`Err`**: `sync()` itself refused (any `FM-*` gate: pack, template,
///   law, config, graph, ...). The real FM-* code is already embedded in
///   `AppError`'s `Display` text (`AppError` has no typed FM-code field --
///   confirmed by reading `error.rs`'s `fm_chain`/`fm_graph`/`fm_tpl`/etc.
///   constructors, each of which formats `"[FM-XXX-NNN] ..."` directly into
///   the variant's `String` payload), so pushing the `Display` string
///   verbatim carries the real code through without inventing a typed
///   field this checkpoint does not need.
/// - **`Ok`**: classify every skip in `report.decisions` via
///   `crate::tools::skip_classify::classify` and push only the `other`
///   category by default -- `when_false`/`zero_rows`/`unchanged`/
///   `skip_empty` are treated as routine, expected outcomes of normal
///   template authoring, not refusal-worthy (the plan's own stated working
///   default; see `docs/jira/.../80-20-...` CP28's judgment-call note).
///
/// Same store/debounce/broadcast shape as `push_diagnostics_for_root`:
/// content is always stored fresh; only the *notification* is debounced,
/// keyed by `{root}#error` or `{root}#{path}` so a rapid-fire re-trigger
/// (e.g. this checkpoint's own coarser watcher debounce, see
/// `crate::watcher`) does not spam one notification per call.
///
/// # Errors
/// Returns an error only if a live peer's `notify_resource_updated` call
/// genuinely fails (see `PeerRegistry::broadcast`) -- never for a `sync()`
/// refusal itself, which is the very thing this function reports via a
/// successful push.
pub async fn push_sync_refusal_for_root(
    peers: &PeerRegistry, store: &SyncRefusalStore, root: &Path,
) -> anyhow::Result<PushOutcome> {
    use ggen_engine::sync::{sync, SyncOptions};

    let root_str = root.display().to_string();
    let mut matched = 0usize;
    let mut delivered_notifications = 0usize;

    let opts = SyncOptions {
        dry_run: true,
        ..Default::default()
    };
    match sync(root, opts) {
        Err(e) => {
            let key = "error";
            let uri = sync_refusal_uri(root, key);
            let message = e.to_string();
            store
                .insert(
                    uri.clone(),
                    PushedSyncRefusal {
                        root: root_str.clone(),
                        message: message.clone(),
                        kind: "error".to_string(),
                    },
                )
                .await;
            matched += 1;
            let debounce_key = format!("{root_str}#{key}");
            if store.should_notify(&debounce_key).await {
                delivered_notifications += peers
                    .broadcast(&ResourceUpdatedNotificationParam::new(uri))
                    .await?;
            }

            // CP39: in addition to (never instead of) the push above, check
            // whether this exact FM-* code has a declared
            // "bounded-unattended" route in the project's own facts, and if
            // so, attempt a real dispatch via the same, unmodified,
            // already-reviewed CP31-33 bounded dispatcher -- never a new,
            // broader write path.
            if let Some(fm_code) = extract_fm_code(&message) {
                match crate::tools::signal_dispatch::route_signal(fm_code, root) {
                    Ok(crate::tools::signal_dispatch::DispatchRoute::BoundedUnattended) => {
                        let breaker = DISPATCH_BREAKERS.for_root(root).await;
                        let outcome =
                            crate::tools::unattended_dispatch::try_unattended_apply(root, &breaker)
                                .await;
                        tracing::info!(
                            fm_code,
                            outcome = ?outcome,
                            "ggen-mcp: CP39 declared bounded-unattended route dispatched"
                        );
                    }
                    Ok(crate::tools::signal_dispatch::DispatchRoute::Attended) => {}
                    Err(e) => {
                        tracing::warn!(error = %e, "ggen-mcp: CP39 route_signal failed, falling through to attended");
                    }
                }
            }
        }
        Ok(report) => {
            for (path, decision) in &report.decisions {
                if !decision.starts_with("skipped") {
                    continue;
                }
                let category = crate::tools::skip_classify::classify(decision);
                if category != "other" {
                    // Routine typed skip -- not push-worthy per this
                    // checkpoint's working default.
                    continue;
                }
                let uri = sync_refusal_uri(root, path);
                store
                    .insert(
                        uri.clone(),
                        PushedSyncRefusal {
                            root: root_str.clone(),
                            message: decision.clone(),
                            kind: category.to_string(),
                        },
                    )
                    .await;
                matched += 1;
                let debounce_key = format!("{root_str}#{path}");
                if store.should_notify(&debounce_key).await {
                    delivered_notifications += peers
                        .broadcast(&ResourceUpdatedNotificationParam::new(uri))
                        .await?;
                }
            }
        }
    }

    Ok(PushOutcome {
        matched,
        delivered_notifications,
    })
}

/// Build the resource URI for a pushed receipt-chain-verify refusal. Always
/// keyed `"chain"` (distinct from `push_sync_refusal_for_root`'s `"error"`/
/// per-path keys) -- at most one receipt-chain state exists per root at a
/// time, so there is only ever one live entry.
fn receipt_chain_refusal_uri(root: &Path) -> String {
    format!("ggen-sync-refusal://{}#chain", root.display())
}

/// Run a real `ggen_engine::verbs::handlers::handle_receipt_verify_in(root)`
/// and push any refusal it surfaces, into the *same* `SyncRefusalStore` (and
/// `ggen-sync-refusal://` URI scheme) that `push_sync_refusal_for_root` uses.
///
/// Closes a real gap: `push_sync_refusal_for_root` always runs `sync()` with
/// `dry_run: true`, but every `FM-CHAIN-*` code lives inside `write_receipt`
/// (only reached on a real, non-dry-run write) or inside
/// `handle_receipt_verify_in`/`handle_receipt_history` -- so a dry-run-only
/// push path can never surface receipt-chain tampering. This function calls
/// the read-only verify path directly (never writes) so chain integrity
/// becomes a proactively pushed fact instead of something only found by an
/// on-demand `ggen_receipt_verify` call.
///
/// Deliberately does **not** run the CP39 declared-route dispatch check that
/// `push_sync_refusal_for_root` runs for FM-* sync errors -- wiring a new
/// FM-CHAIN-* trigger into the bounded-unattended dispatcher is out of scope
/// here; this function's only job is making the refusal visible/queryable.
///
/// A project that has never been synced (no `.ggen-v2/receipt.json` yet) is
/// explicitly NOT a refusal -- checked up front and skipped with a
/// `matched: 0` outcome, so a fresh project does not spam a permanent
/// "chain" refusal on every watcher tick before its first real sync. Once a
/// receipt exists, any failure of `handle_receipt_verify_in` (missing log,
/// malformed JSON, hash/signature mismatch, ...) is pushed like any other
/// refusal; only the hash/signature-mismatch paths carry an `FM-CHAIN-*`
/// prefix today (see `receipt_verify.rs`'s module doc for the exact set),
/// so `extract_fm_code` correctly returns `None` for the others rather than
/// fabricating a code.
///
/// # Errors
/// Returns an error only if a live peer's `notify_resource_updated` call
/// genuinely fails (see `PeerRegistry::broadcast`) -- never for a receipt
/// check itself failing, which is the very thing this function reports via
/// a successful push.
pub async fn push_receipt_verify_for_root(
    peers: &PeerRegistry, store: &SyncRefusalStore, root: &Path,
) -> anyhow::Result<PushOutcome> {
    let root_str = root.display().to_string();
    let mut matched = 0usize;
    let mut delivered_notifications = 0usize;

    if !root.join(ggen_engine::sync::RECEIPT_REL_PATH).exists() {
        return Ok(PushOutcome {
            matched: 0,
            delivered_notifications: 0,
        });
    }

    if let Err(e) = ggen_engine::verbs::handlers::handle_receipt_verify_in(root) {
        let uri = receipt_chain_refusal_uri(root);
        let message = e.to_string();
        store
            .insert(
                uri.clone(),
                PushedSyncRefusal {
                    root: root_str.clone(),
                    message,
                    kind: "chain".to_string(),
                },
            )
            .await;
        matched += 1;
        let debounce_key = format!("{root_str}#chain");
        if store.should_notify(&debounce_key).await {
            delivered_notifications += peers
                .broadcast(&ResourceUpdatedNotificationParam::new(uri))
                .await?;
        }
    }

    Ok(PushOutcome {
        matched,
        delivered_notifications,
    })
}

/// Build the `Resource` listing entries for every URI currently in `store`
/// (backs `list_resources` for `ggen-sync-refusal://` URIs).
pub async fn list_sync_refusals(store: &SyncRefusalStore) -> Vec<Resource> {
    store
        .list()
        .await
        .into_iter()
        .map(|(uri, refusal)| {
            Resource::new(
                rmcp::model::RawResource::new(uri, format!("{}: {}", refusal.kind, refusal.root)),
                None,
            )
        })
        .collect()
}
