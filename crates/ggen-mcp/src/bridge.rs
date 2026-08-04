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
use std::sync::Arc;
use std::time::{Duration, Instant};

use rmcp::model::{ResourceUpdatedNotificationParam, Resource};
use rmcp::service::Peer;
use rmcp::RoleServer;
use tokio::sync::Mutex;

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
    async fn broadcast(
        &self, param: &ResourceUpdatedNotificationParam,
    ) -> anyhow::Result<usize> {
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
    peers: &PeerRegistry,
    store: &DiagnosticStore,
    root: &Path,
    paths: &[PathBuf],
    codes: &[&str],
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
                delivered_notifications +=
                    peers.broadcast(&ResourceUpdatedNotificationParam::new(uri)).await?;
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

#[cfg(test)]
mod tests {
    use super::*;
    use rmcp::model::{ServerCapabilities, ServerInfo};
    use rmcp::{ClientHandler, ServerHandler, ServiceExt};
    use std::fs;
    use std::sync::Arc as StdArc;
    use tokio::sync::Notify;

    /// A minimal MCP server that just holds a `DiagnosticStore` and exposes
    /// `read_resource` from it -- enough to prove `push_diagnostics_for_root`
    /// really notifies AND that the notified URI resolves to real content,
    /// without pulling in all of `GgenMcpServer`'s tool surface.
    struct BridgeTestServer {
        store: DiagnosticStore,
    }

    impl ServerHandler for BridgeTestServer {
        fn get_info(&self) -> ServerInfo {
            ServerInfo::new(
                ServerCapabilities::builder()
                    .enable_resources()
                    .enable_resources_subscribe()
                    .build(),
            )
        }

        async fn read_resource(
            &self,
            request: rmcp::model::ReadResourceRequestParams,
            _context: rmcp::service::RequestContext<RoleServer>,
        ) -> Result<rmcp::model::ReadResourceResult, rmcp::ErrorData> {
            let Some(diag) = self.store.get(&request.uri).await else {
                return Err(rmcp::ErrorData::resource_not_found(
                    format!("no such diagnostic resource: {}", request.uri),
                    None,
                ));
            };
            let text = serde_json::to_string_pretty(&diag)
                .expect("PushedDiagnostic serializes to JSON");
            Ok(rmcp::model::ReadResourceResult::new(vec![
                rmcp::model::ResourceContents::text(text, request.uri),
            ]))
        }
    }

    /// Real MCP client: records every `notifications/resources/updated`
    /// payload it receives, in order.
    struct RecordingClient {
        received: StdArc<Mutex<Vec<String>>>,
        signal: StdArc<Notify>,
    }

    impl ClientHandler for RecordingClient {
        async fn on_resource_updated(
            &self,
            params: rmcp::model::ResourceUpdatedNotificationParam,
            _context: rmcp::service::NotificationContext<rmcp::RoleClient>,
        ) {
            self.received.lock().await.push(params.uri);
            self.signal.notify_one();
        }
    }

    /// End-to-end: a real `GGEN-TPL-001` violation (same fixture shape as
    /// `ggen-lsp`'s own `root_aware_gate_folds_tpl_001_and_fails` test) is
    /// fed through the real headless gate, pushed over a real
    /// `Peer<RoleServer>` retained past `serve()`, carried over an in-process
    /// duplex transport (mirrors rmcp's own
    /// `tests/test_notification.rs::test_server_notification`), and received
    /// by a real MCP client -- which then does a real `resources/read` for
    /// the notified URI and asserts on the real diagnostic content, not a
    /// mock call count.
    #[tokio::test]
    async fn tpl_001_diagnostic_reaches_a_real_mcp_client() -> anyhow::Result<()> {
        // CP20: install a real tracing subscriber so this test's own
        // `RUST_LOG=trace` run prints the real `mcp.push.fire` /
        // `mcp.push.notify` / `mcp.resource.read` spans added in bridge.rs
        // and lib.rs -- proof the spans actually fire during a real push,
        // not just that the source contains span! calls. `try_init` because
        // other tests in this module may already have installed one.
        let _ = tracing_subscriber::fmt()
            .with_env_filter(tracing_subscriber::EnvFilter::from_default_env())
            .with_test_writer()
            .try_init();

        // Arrange -- a project whose rule SELECTs `?name` but whose template
        // consumes `title`: a genuine GGEN-TPL-001 unbound projection
        // (verbatim fixture shape from ggen-lsp's check.rs test).
        let dir = tempfile::TempDir::new()?;
        let root = dir.path();
        fs::write(root.join("row.tera"), r#"{{ row["title"] }}"#)?;
        let manifest = r#"
[project]
name = "demo"
version = "0.1.0"

[ontology]
source = "model.ttl"

[[generation.rules]]
name = "people"
output_file = "people.rs"
query = { inline = "SELECT ?name WHERE { ?p :name ?name }" }
template = { file = "row.tera" }
"#;
        fs::write(root.join("ggen.toml"), manifest)?;
        let template_path = root.join("row.tera");

        // Sanity check the real gate actually flags it BEFORE wiring the
        // bridge, so a failure downstream is provably the bridge's fault,
        // not a fixture that stopped triggering TPL-001.
        let direct = ggen_lsp::check::check_files_in_root(root, &[template_path.clone()], false);
        assert!(
            direct.has_errors(),
            "fixture must trigger a real GGEN-TPL-001 before the bridge is exercised"
        );

        // Real in-process MCP transport (tokio::io::duplex), mirroring
        // rmcp's own notification test.
        let (server_transport, client_transport) = tokio::io::duplex(8192);
        let store = DiagnosticStore::new();
        let server_store = store.clone();
        let root_owned = root.to_path_buf();

        let server_task = tokio::spawn(async move {
            // Move `dir` (the TempDir) into this task so the fixture files
            // stay on disk for the full lifetime of the push + gate run.
            let _dir = dir;
            let running = BridgeTestServer { store: server_store }
                .serve(server_transport)
                .await?;
            // Retain the peer past the initial serve() call -- the CP12
            // requirement -- then push real diagnostics over it from an
            // independent async context before entering `waiting()`.
            let peers = PeerRegistry::new();
            peers.add(running.peer().clone()).await;
            let outcome = push_diagnostics_for_root(
                &peers,
                &store,
                &root_owned,
                &[template_path.clone()],
                &["GGEN-TPL-001"],
            )
            .await?;
            anyhow::ensure!(
                outcome.delivered_notifications >= 1,
                "expected at least one real TPL-001 push"
            );
            running.waiting().await?;
            anyhow::Ok(())
        });

        let received = StdArc::new(Mutex::new(Vec::new()));
        let signal = StdArc::new(Notify::new());
        let client = RecordingClient {
            received: received.clone(),
            signal: signal.clone(),
        }
        .serve(client_transport)
        .await?;

        // Act -- wait for the real push notification to arrive.
        tokio::time::timeout(std::time::Duration::from_secs(5), signal.notified()).await?;

        let uris = received.lock().await.clone();
        assert_eq!(uris.len(), 1, "expected exactly one push for one diagnostic");
        let uri = &uris[0];
        assert!(
            uri.starts_with("ggen-diagnostic://") && uri.contains("GGEN-TPL-001"),
            "notified URI must name the real code, got {uri:?}"
        );

        // Assert -- the client follows up with a real resources/read for the
        // notified URI and gets the real diagnostic content back, not a mock.
        let read = client
            .read_resource(rmcp::model::ReadResourceRequestParams::new(uri.clone()))
            .await?;
        let rmcp::model::ResourceContents::TextResourceContents { text, .. } =
            &read.contents[0]
        else {
            panic!("expected text resource contents");
        };
        let parsed: PushedDiagnostic = serde_json::from_str(text)?;
        assert_eq!(parsed.code, "GGEN-TPL-001");
        assert!(
            parsed.message.contains("title"),
            "real diagnostic message must name the real unbound variable, got {:?}",
            parsed.message
        );
        assert!(parsed.file.ends_with("row.tera"));

        client.cancel().await?;
        server_task.await??;
        Ok(())
    }

    /// CP19: extend past the single proven signal (GGEN-TPL-001) to
    /// GGEN-OUT-001 -- the same real end-to-end path
    /// (`push_diagnostics_for_root` -> real headless gate -> real retained
    /// `Peer<RoleServer>` -> real MCP client), just a different code in the
    /// caller-supplied `codes` allowlist. Fixture is CP7's own real
    /// `ggen_out_001_living_loop/invalid_project` manifest content
    /// (`crates/ggen-lsp/tests/fixtures/ggen_out_001_living_loop/invalid_project/`),
    /// transcribed verbatim rather than invented: a rule's SPARQL SELECT
    /// projects only `?name`, but `output_file = "out/{{ slug }}.txt"`
    /// consumes the unprojected `slug` -- a genuine GGEN-OUT-001
    /// unbound-output-path defect, independent of GGEN-TPL-001 (the
    /// template body itself only consumes the bound `name`).
    #[tokio::test]
    async fn out_001_diagnostic_reaches_a_real_mcp_client() -> anyhow::Result<()> {
        // Arrange -- verbatim fixture shape from
        // ggen-lsp's tests/fixtures/ggen_out_001_living_loop/invalid_project.
        let dir = tempfile::TempDir::new()?;
        let root = dir.path();
        fs::create_dir_all(root.join("queries"))?;
        fs::create_dir_all(root.join("schema"))?;
        fs::create_dir_all(root.join("templates"))?;
        fs::write(
            root.join("queries/items.rq"),
            "PREFIX ex: <http://example.org/out001#>\n\
             SELECT ?name WHERE {\n\
             \x20\x20\x20\x20?s a ex:Item .\n\
             \x20\x20\x20\x20?s ex:name ?name .\n\
             }\n",
        )?;
        fs::write(
            root.join("schema/domain.ttl"),
            "@prefix ex: <http://example.org/out001#> .\n\
             ex:Item a ex:Class .\n\
             ex:widget a ex:Item ; ex:name \"widget\" .\n",
        )?;
        fs::write(
            root.join("templates/item.tera"),
            "Item: {{ name }}",
        )?;
        let manifest = r#"
[project]
name = "out001-living-loop-invalid"
version = "0.1.0"

[ontology]
source = "schema/domain.ttl"

[generation]

[[generation.rules]]
name = "items"
query = { file = "queries/items.rq" }
template = { file = "templates/item.tera" }
output_file = "out/{{ slug }}.txt"
"#;
        fs::write(root.join("ggen.toml"), manifest)?;
        let manifest_path = root.join("ggen.toml");

        // Sanity check the real gate actually flags it BEFORE wiring the
        // bridge, so a failure downstream is provably the bridge's fault,
        // not a fixture that stopped triggering OUT-001.
        let direct = ggen_lsp::check::check_files_in_root(root, &[manifest_path.clone()], false);
        assert!(
            direct.has_errors(),
            "fixture must trigger a real GGEN-OUT-001 before the bridge is exercised"
        );

        let (server_transport, client_transport) = tokio::io::duplex(8192);
        let store = DiagnosticStore::new();
        let server_store = store.clone();
        let root_owned = root.to_path_buf();

        let server_task = tokio::spawn(async move {
            let _dir = dir;
            let running = BridgeTestServer { store: server_store }
                .serve(server_transport)
                .await?;
            let peers = PeerRegistry::new();
            peers.add(running.peer().clone()).await;
            let outcome = push_diagnostics_for_root(
                &peers,
                &store,
                &root_owned,
                &[manifest_path.clone()],
                &["GGEN-OUT-001"],
            )
            .await?;
            anyhow::ensure!(
                outcome.delivered_notifications >= 1,
                "expected at least one real OUT-001 push"
            );
            running.waiting().await?;
            anyhow::Ok(())
        });

        let received = StdArc::new(Mutex::new(Vec::new()));
        let signal = StdArc::new(Notify::new());
        let client = RecordingClient {
            received: received.clone(),
            signal: signal.clone(),
        }
        .serve(client_transport)
        .await?;

        // Act -- wait for the real push notification to arrive.
        tokio::time::timeout(std::time::Duration::from_secs(5), signal.notified()).await?;

        let uris = received.lock().await.clone();
        assert_eq!(uris.len(), 1, "expected exactly one push for one diagnostic");
        let uri = &uris[0];
        assert!(
            uri.starts_with("ggen-diagnostic://") && uri.contains("GGEN-OUT-001"),
            "notified URI must name the real code, got {uri:?}"
        );

        // Assert -- the client follows up with a real resources/read for the
        // notified URI and gets the real diagnostic content back, not a mock.
        let read = client
            .read_resource(rmcp::model::ReadResourceRequestParams::new(uri.clone()))
            .await?;
        let rmcp::model::ResourceContents::TextResourceContents { text, .. } =
            &read.contents[0]
        else {
            panic!("expected text resource contents");
        };
        let parsed: PushedDiagnostic = serde_json::from_str(text)?;
        assert_eq!(parsed.code, "GGEN-OUT-001");
        assert!(
            parsed.message.contains("slug"),
            "real diagnostic message must name the real unbound output-path \
             variable, got {:?}",
            parsed.message
        );
        assert!(parsed.file.ends_with("ggen.toml"));

        client.cancel().await?;
        server_task.await??;
        Ok(())
    }

    #[tokio::test]
    async fn no_matching_code_pushes_nothing() -> anyhow::Result<()> {
        // A clean project (no unbound var) must push zero notifications --
        // proves the bridge never fabricates a diagnostic to demonstrate
        // itself.
        let dir = tempfile::TempDir::new()?;
        let root = dir.path();
        fs::write(root.join("row.tera"), r#"{{ row["name"] }}"#)?;
        let manifest = r#"
[project]
name = "demo"
version = "0.1.0"

[ontology]
source = "model.ttl"

[[generation.rules]]
name = "people"
output_file = "people.rs"
query = { inline = "SELECT ?name WHERE { ?p :name ?name }" }
template = { file = "row.tera" }
"#;
        fs::write(root.join("ggen.toml"), manifest)?;

        let (server_transport, client_transport) = tokio::io::duplex(8192);
        let store = DiagnosticStore::new();
        let server_store = store.clone();
        let template_path = root.join("row.tera");
        let root_owned = root.to_path_buf();
        let (pushed_tx, pushed_rx) = tokio::sync::oneshot::channel();

        let server_task = tokio::spawn(async move {
            let _dir = dir;
            let running = BridgeTestServer { store: server_store }
                .serve(server_transport)
                .await?;
            let peers = PeerRegistry::new();
            peers.add(running.peer().clone()).await;
            let outcome = push_diagnostics_for_root(
                &peers,
                &store,
                &root_owned,
                &[template_path],
                &["GGEN-TPL-001"],
            )
            .await?;
            let _ = pushed_tx.send(outcome.matched);
            // Keep the session alive until the client explicitly cancels --
            // returning here immediately (dropping `running`) would close
            // the transport before the client finishes its own handshake.
            running.waiting().await?;
            anyhow::Ok(())
        });

        let client = ().serve(client_transport).await?;
        let pushed = pushed_rx.await?;
        assert_eq!(pushed, 0, "a bound template must push zero diagnostics");
        client.cancel().await?;
        server_task.await??;
        Ok(())
    }

    /// CP16 case 1: a disconnected client's retained `Peer` must make
    /// `notify_resource_updated` return a real `Err`, not panic. Proves this
    /// is already true by construction in `rmcp` 1.8.0 rather than assuming
    /// it: `Peer::send_notification` (which `notify_resource_updated`
    /// delegates to, `rmcp-1.8.0/src/service/server.rs:482`) sends over an
    /// `mpsc::Sender` and maps a closed channel to
    /// `Err(ServiceError::TransportClosed)` -- there is no panicking path.
    #[tokio::test]
    async fn notify_on_disconnected_peer_returns_err_not_panic() -> anyhow::Result<()> {
        let (server_transport, client_transport) = tokio::io::duplex(8192);
        let store = DiagnosticStore::new();
        // CP22 fix: rmcp's real server-side `serve()` blocks inside
        // `expect_next_message` until the client sends its `initialize`
        // request (rmcp-1.8.0/src/service/server.rs:179-219) -- awaiting it
        // in-line here, before the client's own `.serve()` is ever called a
        // few lines below, deadlocked every time (confirmed against real
        // rmcp source, not assumed). Spawn it, matching the proven pattern
        // `tpl_001_diagnostic_reaches_a_real_mcp_client` already uses, and
        // hand the resulting `RunningService` back via a oneshot channel so
        // the rest of this test can still use `running`/`peer` directly.
        let (running_tx, running_rx) = tokio::sync::oneshot::channel();
        tokio::spawn(async move {
            let running = BridgeTestServer { store }.serve(server_transport).await?;
            let _ = running_tx.send(running);
            anyhow::Ok(())
        });

        // Act -- disconnect the client, then wait for the server's own
        // read/write loop to observe the closed transport and mark the
        // peer's outbound channel closed (rmcp gives no disconnect
        // callback; `is_transport_closed` is the only real signal).
        let client = ().serve(client_transport).await?;
        let running = running_rx.await?;
        let peer = running.peer().clone();
        client.cancel().await?;
        tokio::time::timeout(std::time::Duration::from_secs(5), async {
            while !peer.is_transport_closed() {
                tokio::time::sleep(std::time::Duration::from_millis(10)).await;
            }
        })
        .await?;

        // Assert -- a push to the now-dead peer is a real Err, not a panic.
        let result = peer
            .notify_resource_updated(ResourceUpdatedNotificationParam::new(
                "ggen-diagnostic://dead-peer-test#GGEN-TPL-001-0",
            ))
            .await;
        assert!(
            result.is_err(),
            "notify_resource_updated on a disconnected peer must return Err"
        );

        // The server's own task must also have wound down cleanly (no
        // panic) once its transport closed.
        running.waiting().await?;
        Ok(())
    }

    /// CP16 case 2: `PeerRegistry` retains more than one live peer and
    /// broadcasts a single push to all of them -- real fix, not true by
    /// construction: CP12's bridge took one bare `Peer` and had no way to
    /// represent a second concurrent client at all.
    #[tokio::test]
    async fn multiple_concurrent_clients_all_receive_the_push() -> anyhow::Result<()> {
        let dir = tempfile::TempDir::new()?;
        let root = dir.path();
        fs::write(root.join("row.tera"), r#"{{ row["title"] }}"#)?;
        let manifest = r#"
[project]
name = "demo"
version = "0.1.0"

[ontology]
source = "model.ttl"

[[generation.rules]]
name = "people"
output_file = "people.rs"
query = { inline = "SELECT ?name WHERE { ?p :name ?name }" }
template = { file = "row.tera" }
"#;
        fs::write(root.join("ggen.toml"), manifest)?;
        let template_path = root.join("row.tera");

        let store = DiagnosticStore::new();
        let peers = PeerRegistry::new();

        // Client 1: real duplex transport, real server-side serve(), a real
        // Peer retained into the shared registry.
        //
        // CP22 fix: rmcp's real server-side `serve()` blocks until the
        // client sends its `initialize` request (confirmed against real
        // rmcp-1.8.0 source) -- awaiting it in-line, before the client's own
        // `.serve()` is called below, deadlocked every time. Spawn it
        // (matching `tpl_001_diagnostic_reaches_a_real_mcp_client`'s already-
        // proven pattern) and hand the `RunningService` back via a oneshot.
        let (s1, c1) = tokio::io::duplex(8192);
        let store1 = store.clone();
        let (running1_tx, running1_rx) = tokio::sync::oneshot::channel();
        tokio::spawn(async move {
            let running1 = BridgeTestServer { store: store1 }.serve(s1).await?;
            let _ = running1_tx.send(running1);
            anyhow::Ok(())
        });
        let received1 = StdArc::new(Mutex::new(Vec::new()));
        let signal1 = StdArc::new(Notify::new());
        let client1 = RecordingClient {
            received: received1.clone(),
            signal: signal1.clone(),
        }
        .serve(c1)
        .await?;
        let running1 = running1_rx.await?;
        peers.add(running1.peer().clone()).await;
        tokio::spawn(async move {
            let _ = running1.waiting().await;
        });

        // Client 2: same, independent transport and Peer.
        let (s2, c2) = tokio::io::duplex(8192);
        let store2 = store.clone();
        let (running2_tx, running2_rx) = tokio::sync::oneshot::channel();
        tokio::spawn(async move {
            let running2 = BridgeTestServer { store: store2 }.serve(s2).await?;
            let _ = running2_tx.send(running2);
            anyhow::Ok(())
        });
        let received2 = StdArc::new(Mutex::new(Vec::new()));
        let signal2 = StdArc::new(Notify::new());
        let client2 = RecordingClient {
            received: received2.clone(),
            signal: signal2.clone(),
        }
        .serve(c2)
        .await?;
        let running2 = running2_rx.await?;
        peers.add(running2.peer().clone()).await;
        tokio::spawn(async move {
            let _ = running2.waiting().await;
        });

        assert_eq!(
            peers.prune_and_count().await,
            2,
            "both connected peers must be retained"
        );

        let root_owned = root.to_path_buf();
        let outcome = push_diagnostics_for_root(
            &peers,
            &store,
            &root_owned,
            &[template_path],
            &["GGEN-TPL-001"],
        )
        .await?;
        assert_eq!(outcome.matched, 1, "exactly one real TPL-001 diagnostic");
        assert_eq!(
            outcome.delivered_notifications, 2,
            "the one diagnostic must be delivered to both live peers"
        );

        tokio::time::timeout(std::time::Duration::from_secs(5), signal1.notified()).await?;
        tokio::time::timeout(std::time::Duration::from_secs(5), signal2.notified()).await?;
        assert_eq!(received1.lock().await.len(), 1);
        assert_eq!(received2.lock().await.len(), 1);

        client1.cancel().await?;
        client2.cancel().await?;
        Ok(())
    }

    /// CP16 case 3: a push with zero retained peers (nobody has connected,
    /// or the only connected client already disconnected) is not an error
    /// and not silently lost -- the diagnostic is still stored (a later
    /// `resources/read` finds it), but no notification is buffered or
    /// queued for a future connection. Real, deliberate decision (documented
    /// on `push_diagnostics_for_root`), not an accidental gap.
    #[tokio::test]
    async fn push_with_no_retained_peers_stores_but_does_not_error_or_buffer(
    ) -> anyhow::Result<()> {
        let dir = tempfile::TempDir::new()?;
        let root = dir.path();
        fs::write(root.join("row.tera"), r#"{{ row["title"] }}"#)?;
        let manifest = r#"
[project]
name = "demo"
version = "0.1.0"

[ontology]
source = "model.ttl"

[[generation.rules]]
name = "people"
output_file = "people.rs"
query = { inline = "SELECT ?name WHERE { ?p :name ?name }" }
template = { file = "row.tera" }
"#;
        fs::write(root.join("ggen.toml"), manifest)?;
        let template_path = root.join("row.tera");

        let store = DiagnosticStore::new();
        let peers = PeerRegistry::new(); // nobody has connected yet
        let root_owned = root.to_path_buf();

        let outcome = push_diagnostics_for_root(
            &peers,
            &store,
            &root_owned,
            &[template_path],
            &["GGEN-TPL-001"],
        )
        .await?;
        assert_eq!(outcome.matched, 1, "the diagnostic is still real and found");
        assert_eq!(
            outcome.delivered_notifications, 0,
            "no peers were retained, so nothing was pushed"
        );

        // The diagnostic is durably stored despite zero live peers -- a
        // client that connects afterward and does resources/list +
        // resources/read still finds it.
        let stored = store.list().await;
        assert_eq!(stored.len(), 1);
        assert_eq!(stored[0].1.code, "GGEN-TPL-001");
        Ok(())
    }

    /// CP16 case 4: reconnect. A fresh `serve()` call after the prior peer
    /// disconnected must (a) be able to retain a fresh `Peer` that
    /// successfully receives pushes, and (b) not leave the dead peer
    /// counted as live in `PeerRegistry` forever -- `prune_and_count`
    /// removes it lazily rather than leaking it.
    #[tokio::test]
    async fn reconnect_retains_fresh_peer_and_prunes_the_dead_one() -> anyhow::Result<()> {
        let store = DiagnosticStore::new();
        let peers = PeerRegistry::new();

        // First connection, then disconnect.
        // CP22 fix: same handshake-ordering bug as the other tests -- spawn
        // the server's serve() so the client's serve() (a few lines below)
        // can actually run concurrently and send the initialize request the
        // server is blocked waiting for.
        let (s1, c1) = tokio::io::duplex(8192);
        let store1 = store.clone();
        let (running1_tx, running1_rx) = tokio::sync::oneshot::channel();
        tokio::spawn(async move {
            let running1 = BridgeTestServer { store: store1 }.serve(s1).await?;
            let _ = running1_tx.send(running1);
            anyhow::Ok(())
        });
        let client1 = ().serve(c1).await?;
        let running1 = running1_rx.await?;
        peers.add(running1.peer().clone()).await;
        assert_eq!(peers.prune_and_count().await, 1);
        client1.cancel().await?;
        running1.waiting().await?;

        // Give the dropped peer's channel a moment to actually close before
        // asserting the prune -- same reasoning as the disconnect test.
        tokio::time::timeout(std::time::Duration::from_secs(5), async {
            while peers.prune_and_count().await != 0 {
                tokio::time::sleep(std::time::Duration::from_millis(10)).await;
            }
        })
        .await?;

        // Reconnect: a fresh serve() call retains a fresh Peer.
        let (s2, c2) = tokio::io::duplex(8192);
        let store2 = store.clone();
        let (running2_tx, running2_rx) = tokio::sync::oneshot::channel();
        tokio::spawn(async move {
            let running2 = BridgeTestServer { store: store2 }.serve(s2).await?;
            let _ = running2_tx.send(running2);
            anyhow::Ok(())
        });
        let received2 = StdArc::new(Mutex::new(Vec::new()));
        let signal2 = StdArc::new(Notify::new());
        let client2 = RecordingClient {
            received: received2.clone(),
            signal: signal2.clone(),
        }
        .serve(c2)
        .await?;
        let running2 = running2_rx.await?;
        peers.add(running2.peer().clone()).await;
        assert_eq!(
            peers.prune_and_count().await,
            1,
            "the fresh peer must be retained after the old one was pruned"
        );

        // Push a real diagnostic through the fresh peer only.
        let dir = tempfile::TempDir::new()?;
        let root = dir.path();
        fs::write(root.join("row.tera"), r#"{{ row["title"] }}"#)?;
        let manifest = r#"
[project]
name = "demo"
version = "0.1.0"

[ontology]
source = "model.ttl"

[[generation.rules]]
name = "people"
output_file = "people.rs"
query = { inline = "SELECT ?name WHERE { ?p :name ?name }" }
template = { file = "row.tera" }
"#;
        fs::write(root.join("ggen.toml"), manifest)?;
        let template_path = root.join("row.tera");

        let outcome = push_diagnostics_for_root(
            &peers,
            &store,
            &root.to_path_buf(),
            &[template_path],
            &["GGEN-TPL-001"],
        )
        .await?;
        assert_eq!(outcome.delivered_notifications, 1, "only the fresh peer receives it");

        tokio::time::timeout(std::time::Duration::from_secs(5), signal2.notified()).await?;
        assert_eq!(received2.lock().await.len(), 1);

        client2.cancel().await?;
        running2.cancel().await?;
        Ok(())
    }

    /// CP18 case 1: bounded growth. Insert far more diagnostics than
    /// `max_entries` directly into a real `DiagnosticStore` (no gate/MCP
    /// machinery needed to prove this -- it's a property of the store
    /// itself) and assert the real entry count, not "it didn't crash": the
    /// store must never exceed its cap, must evict the *oldest* entries
    /// first (FIFO), and must keep the newest ones.
    #[tokio::test]
    async fn diagnostic_store_stays_bounded_under_rapid_growth() {
        let store = DiagnosticStore::with_limits(50, Duration::from_millis(1));
        let sample_range = lsp_max::lsp_types::Range::new(
            lsp_max::lsp_types::Position::new(0, 0),
            lsp_max::lsp_types::Position::new(0, 1),
        );

        for i in 0..500usize {
            store
                .insert(
                    format!("ggen-diagnostic://file-{i}.tera#GGEN-TPL-001-0"),
                    PushedDiagnostic {
                        file: format!("file-{i}.tera"),
                        code: "GGEN-TPL-001".to_string(),
                        message: format!("unbound var #{i}"),
                        range: sample_range,
                    },
                )
                .await;
        }

        assert_eq!(
            store.len().await,
            50,
            "store must be capped at max_entries regardless of how many were inserted"
        );

        // FIFO: the oldest 450 entries were evicted, the newest 50 remain.
        assert!(
            store
                .get("ggen-diagnostic://file-0.tera#GGEN-TPL-001-0")
                .await
                .is_none(),
            "the oldest entry must have been evicted"
        );
        assert!(
            store
                .get("ggen-diagnostic://file-449.tera#GGEN-TPL-001-0")
                .await
                .is_none(),
            "an entry older than the cap window must have been evicted"
        );
        assert!(
            store
                .get("ggen-diagnostic://file-499.tera#GGEN-TPL-001-0")
                .await
                .is_some(),
            "the most recently inserted entry must still be present"
        );
        assert!(
            store
                .get("ggen-diagnostic://file-450.tera#GGEN-TPL-001-0")
                .await
                .is_some(),
            "the oldest entry still inside the cap window must still be present"
        );
    }

    /// CP18 case 2: debouncing. Fire the *same* real `GGEN-TPL-001`
    /// diagnostic through `push_diagnostics_for_root` 150 times in a tight
    /// loop (no sleeps -- a real burst) against a store with a debounce
    /// window generous relative to the loop's real wall-clock duration, and
    /// assert on the real numbers: every call still finds and stores the
    /// real diagnostic (`matched == 150`, detection is never debounced),
    /// but the notification count delivered to a real connected MCP client
    /// is far below 150 -- proving the push side is actually throttled, not
    /// just "didn't crash".
    #[tokio::test]
    async fn rapid_fire_same_diagnostic_is_debounced_not_delivered_1to1() -> anyhow::Result<()> {
        let dir = tempfile::TempDir::new()?;
        let root = dir.path();
        fs::write(root.join("row.tera"), r#"{{ row["title"] }}"#)?;
        let manifest = r#"
[project]
name = "demo"
version = "0.1.0"

[ontology]
source = "model.ttl"

[[generation.rules]]
name = "people"
output_file = "people.rs"
query = { inline = "SELECT ?name WHERE { ?p :name ?name }" }
template = { file = "row.tera" }
"#;
        fs::write(root.join("ggen.toml"), manifest)?;
        let template_path = root.join("row.tera");

        // A debounce window generous enough that a real 150-iteration
        // no-sleep loop (microseconds to low milliseconds of real wall
        // clock per call for a single-file gate run) stays inside it --
        // this is what makes the throttling assertion below meaningful
        // rather than a coincidence of scheduling.
        let store = DiagnosticStore::with_limits(DEFAULT_MAX_ENTRIES, Duration::from_secs(10));

        // CP22 fix: same handshake-ordering bug -- spawn the server's
        // serve() so the client's serve() below can run concurrently.
        let (server_transport, client_transport) = tokio::io::duplex(8192);
        let server_store = store.clone();
        let (running_tx, running_rx) = tokio::sync::oneshot::channel();
        tokio::spawn(async move {
            let running = BridgeTestServer { store: server_store }
                .serve(server_transport)
                .await?;
            let _ = running_tx.send(running);
            anyhow::Ok(())
        });
        let peers = PeerRegistry::new();

        let received = StdArc::new(Mutex::new(Vec::new()));
        let signal = StdArc::new(Notify::new());
        let client = RecordingClient {
            received: received.clone(),
            signal: signal.clone(),
        }
        .serve(client_transport)
        .await?;
        let running = running_rx.await?;
        peers.add(running.peer().clone()).await;
        tokio::spawn(async move {
            let _ = running.waiting().await;
        });

        const ITERATIONS: usize = 150;
        let mut total_matched = 0usize;
        let mut total_delivered = 0usize;
        for _ in 0..ITERATIONS {
            let outcome = push_diagnostics_for_root(
                &peers,
                &store,
                &root.to_path_buf(),
                &[template_path.clone()],
                &["GGEN-TPL-001"],
            )
            .await?;
            total_matched += outcome.matched;
            total_delivered += outcome.delivered_notifications;
        }

        assert_eq!(
            total_matched, ITERATIONS,
            "every call must still detect the real diagnostic -- detection is never debounced"
        );
        assert!(
            total_delivered < ITERATIONS,
            "delivered notification count ({total_delivered}) must be throttled below the \
             1:1 rate of {ITERATIONS} calls"
        );
        assert_eq!(
            total_delivered, 1,
            "within a single debounce window a real burst must coalesce to exactly one \
             delivered notification, got {total_delivered}"
        );

        // The one real notification that did go out must still resolve to
        // real, current content over resources/read.
        tokio::time::timeout(std::time::Duration::from_secs(5), signal.notified()).await?;
        let uris = received.lock().await.clone();
        assert_eq!(uris.len(), 1);
        let read = client
            .read_resource(rmcp::model::ReadResourceRequestParams::new(uris[0].clone()))
            .await?;
        let rmcp::model::ResourceContents::TextResourceContents { text, .. } = &read.contents[0]
        else {
            panic!("expected text resource contents");
        };
        let parsed: PushedDiagnostic = serde_json::from_str(text)?;
        assert_eq!(parsed.code, "GGEN-TPL-001");

        client.cancel().await?;
        Ok(())
    }
}
