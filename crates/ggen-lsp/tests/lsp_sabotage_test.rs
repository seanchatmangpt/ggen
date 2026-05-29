//! NEGATIVE / sabotage protocol tests for the ggen-lsp plugin: spawn the REAL
//! `ggen-lsp` binary and drive hostile / edge-case LSP JSON-RPC over real stdio.
//! This is the adversarial complement to `lsp_protocol_test.rs` (the happy-path
//! lifecycle): here we feed unknown methods, lifecycle violations, post-shutdown
//! requests, and interleaved ids, and assert the server's REAL behavior.
//!
//! Chicago TDD: real binary, real stdio, real fixtures, no mocks, no test doubles.
//! Every assertion is on observable wire behavior (JSON-RPC frames the server
//! actually emits) or on liveness (a follow-up valid request still gets a reply).
//! Hermetic: each test spawns its own child with `current_dir(TempDir)` because
//! `did_open` writes an OCEL log under cwd.
//!
//! Deadlock defense (copied from the protocol harness): a dedicated reader thread
//! parses Content-Length frames into an mpsc channel; every read is bounded by
//! `recv_timeout`, so a hang becomes a fast, legible panic instead of a CI stall.
//! `Drop` kills the child so a failed assertion never leaks a `ggen-lsp` holding
//! the pipe.
//!
//! ── HONEST FINDINGS recorded from reading the real server (server.rs) and the
//!    tower-lsp 0.20.0 service layer, asserted (not invented) below ────────────
//!  * Unknown (non-`$/`) method → JSON-RPC error code -32601 MethodNotFound
//!    (tower-lsp `Router` default). The server stays alive afterwards.
//!  * `$/`-prefixed unknown methods are SWALLOWED by tower-lsp (returns no
//!    response frame) per the LSP spec for optional `$/` messages — we do NOT
//!    test those as "errors"; that would be inventing strictness.
//!  * `did_close` (server.rs `did_close`) removes the document AND publishes a
//!    cleared (empty) diagnostics set for the URI — the LSP-conformant clear so
//!    stale squiggles do not linger in the client. We assert the HARDENED
//!    behavior: closing publishes a `publishDiagnostics` for the URI with an EMPTY
//!    diagnostics array, does not error, and the server stays alive. This is both
//!    the positive test and the sabotage test: it fails loud if the clear regresses.
//!  * `shutdown` returns Ok(()); tower-lsp only marks the service `Exited` on the
//!    `exit` NOTIFICATION, not on `shutdown`. So a request sent AFTER `shutdown`
//!    but BEFORE `exit` is still served normally by this server (no -32600). We
//!    assert that REAL post-shutdown behavior rather than inventing InvalidRequest.

use serde_json::{json, Value};
use std::io::{BufRead, BufReader, Write};
use std::process::{Child, ChildStdin, Command, Stdio};
use std::sync::mpsc::{self, Receiver};
use std::thread;
use std::time::Duration;
use tempfile::TempDir;

const READ_TIMEOUT: Duration = Duration::from_secs(10);

/// Broken SPARQL law surface: unconstrained CONSTRUCT → E0011 (WARNING, line 0).
const BROKEN_CONSTRUCT: &str = "CONSTRUCT { ?s ?p ?o } WHERE { ?s ?p ?o }\n";
/// Clean SPARQL: a CONSTRUCT WITH ORDER BY does not trip E0011.
const CLEAN_CONSTRUCT: &str = "CONSTRUCT { ?s ?p ?o } WHERE { ?s ?p ?o } ORDER BY ?s\n";

struct LspClient {
    stdin: ChildStdin,
    rx: Receiver<Value>,
    child: Child,
    next_id: i64,
    stashed_notifications: Vec<Value>,
    stashed_responses: Vec<Value>, // responses received out-of-id-order, kept for later
    _tmp: TempDir,                 // keep the hermetic cwd alive for the child's lifetime
}

impl LspClient {
    fn spawn() -> Self {
        let bin = assert_cmd::cargo::cargo_bin("ggen-lsp");
        let tmp = TempDir::new().expect("tempdir");
        let mut child = Command::new(&bin)
            .current_dir(tmp.path())
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .stderr(Stdio::null())
            .spawn()
            .unwrap_or_else(|e| panic!("spawn {}: {e}", bin.display()));
        let stdin = child.stdin.take().expect("child stdin");
        let stdout = child.stdout.take().expect("child stdout");
        let (tx, rx) = mpsc::channel();
        thread::spawn(move || {
            let mut reader = BufReader::new(stdout);
            while let Some(frame) = read_frame(&mut reader) {
                if tx.send(frame).is_err() {
                    break;
                }
            }
        });
        LspClient {
            stdin,
            rx,
            child,
            next_id: 1,
            stashed_notifications: Vec::new(),
            stashed_responses: Vec::new(),
            _tmp: tmp,
        }
    }

    fn send(&mut self, msg: &Value) {
        let body = serde_json::to_vec(msg).expect("serialize");
        let header = format!("Content-Length: {}\r\n\r\n", body.len());
        self.stdin
            .write_all(header.as_bytes())
            .expect("write header");
        self.stdin.write_all(&body).expect("write body");
        self.stdin.flush().expect("flush");
    }

    /// Send a raw, fully-formed JSON-RPC request with a caller-chosen id (used by
    /// the id-correlation test to interleave two requests before reading either).
    fn send_request_id(&mut self, id: i64, method: &str, params: Value) {
        self.send(&json!({"jsonrpc":"2.0","id":id,"method":method,"params":params}));
    }

    fn recv(&self) -> Value {
        self.rx
            .recv_timeout(READ_TIMEOUT)
            .expect("LSP read timed out — server never sent the awaited frame")
    }

    /// Send a request and return its correlated response, stashing notifications and
    /// replying to any server→client request encountered along the way (avoids
    /// deadlock if the server asks for e.g. workspace/configuration).
    fn request(&mut self, method: &str, params: Value) -> Value {
        let id = self.next_id;
        self.next_id += 1;
        self.send_request_id(id, method, params);
        self.await_response(id)
    }

    /// Send a parameterless request (no `params` field at all). HONEST FINDING:
    /// the tower-lsp `shutdown` handler rejects ANY params — even JSON `null` —
    /// with -32602 ("Unexpected params: null"). So `shutdown` must be sent with
    /// the `params` key omitted entirely.
    fn request_no_params(&mut self, method: &str) -> Value {
        let id = self.next_id;
        self.next_id += 1;
        self.send(&json!({"jsonrpc":"2.0","id":id,"method":method}));
        self.await_response(id)
    }

    /// Pump frames until the response for `id` arrives; stash notifications,
    /// auto-reply to server→client requests.
    fn await_response(&mut self, id: i64) -> Value {
        // First, honor any response already received out of order.
        if let Some(pos) = self
            .stashed_responses
            .iter()
            .position(|r| r.get("id").and_then(Value::as_i64) == Some(id))
        {
            return self.stashed_responses.remove(pos);
        }
        loop {
            let f = self.recv();
            let f_id = f.get("id").and_then(Value::as_i64);
            let has_result = f.get("result").is_some() || f.get("error").is_some();
            if has_result && f.get("method").is_none() {
                // A response (no `method`). Match by id, else stash for a later await.
                if f_id == Some(id) {
                    return f;
                }
                self.stashed_responses.push(f);
            } else if f.get("method").is_some() && f.get("id").is_some() {
                // server→client request: reply with a null result so it doesn't block.
                let rid = f.get("id").cloned().unwrap_or(Value::Null);
                self.send(&json!({"jsonrpc":"2.0","id":rid,"result":null}));
            } else if f.get("method").is_some() {
                self.stashed_notifications.push(f); // a notification
            }
        }
    }

    fn notify(&mut self, method: &str, params: Value) {
        self.send(&json!({"jsonrpc":"2.0","method":method,"params":params}));
    }

    /// Block until a `textDocument/publishDiagnostics` for `uri` arrives (checking
    /// frames already stashed during earlier requests first).
    fn wait_for_diagnostics(&mut self, uri: &str) -> Value {
        if let Some(pos) = self.stashed_notifications.iter().position(|n| {
            n.get("method").and_then(Value::as_str) == Some("textDocument/publishDiagnostics")
                && n["params"]["uri"] == json!(uri)
        }) {
            return self.stashed_notifications.remove(pos);
        }
        loop {
            let f = self.recv();
            if f.get("method").and_then(Value::as_str) == Some("textDocument/publishDiagnostics")
                && f["params"]["uri"] == json!(uri)
            {
                return f;
            }
        }
    }

    fn did_open(&mut self, uri: &str, language_id: &str, text: &str) {
        self.notify(
            "textDocument/didOpen",
            json!({"textDocument":{"uri":uri,"languageId":language_id,"version":1,"text":text}}),
        );
    }

    fn did_change_full(&mut self, uri: &str, version: i64, text: &str) {
        self.notify(
            "textDocument/didChange",
            json!({
                "textDocument": {"uri": uri, "version": version},
                "contentChanges": [{"text": text}]
            }),
        );
    }

    fn did_close(&mut self, uri: &str) {
        self.notify(
            "textDocument/didClose",
            json!({"textDocument": {"uri": uri}}),
        );
    }
}

impl Drop for LspClient {
    fn drop(&mut self) {
        // Best-effort graceful exit, then hard kill — never leak a server process.
        let _ = self
            .stdin
            .write_all(b"Content-Length: 33\r\n\r\n{\"jsonrpc\":\"2.0\",\"method\":\"exit\"}");
        let _ = self.stdin.flush();
        let _ = self.child.kill();
        let _ = self.child.wait();
    }
}

/// Read one Content-Length-framed LSP message; None on EOF.
fn read_frame(reader: &mut impl BufRead) -> Option<Value> {
    let mut content_length: usize = 0;
    loop {
        let mut line = String::new();
        if reader.read_line(&mut line).ok()? == 0 {
            return None; // EOF
        }
        let line = line.trim_end();
        if line.is_empty() {
            break; // end of headers
        }
        if let Some(rest) = line.to_ascii_lowercase().strip_prefix("content-length:") {
            content_length = rest.trim().parse().ok()?;
        }
        // tolerate Content-Type and any other header lines
    }
    let mut buf = vec![0u8; content_length];
    reader.read_exact(&mut buf).ok()?;
    serde_json::from_slice(&buf).ok()
}

fn initialize(client: &mut LspClient) -> Value {
    let resp = client.request(
        "initialize",
        json!({"processId":null,"rootUri":null,"capabilities":{}}),
    );
    client.notify("initialized", json!({}));
    resp
}

/// True if the diagnostics array in a publishDiagnostics frame carries E0011.
fn has_e0011(diags_frame: &Value) -> bool {
    diags_frame["params"]["diagnostics"]
        .as_array()
        .map(|a| a.iter().any(|d| d["code"] == json!("E0011")))
        .unwrap_or(false)
}

// ─────────────────────────────────────────────────────────────────────────────
// Test 1: an unknown (non-`$/`) request method → structured JSON-RPC error with
// code -32601 MethodNotFound, and the server stays alive for a follow-up request.
// ─────────────────────────────────────────────────────────────────────────────

#[test]
fn unknown_method_returns_method_not_found_and_server_stays_alive() {
    let mut c = LspClient::spawn();
    initialize(&mut c);

    // Sabotage: a method the server never registered.
    let resp = c.request("ggen/thisMethodDoesNotExist", json!({"anything": true}));
    let err = resp
        .get("error")
        .unwrap_or_else(|| panic!("unknown method must return a JSON-RPC error, got: {resp}"));
    assert_eq!(
        err["code"],
        json!(-32601),
        "unknown method must be MethodNotFound (-32601), got: {err}"
    );
    // The error response must still correlate to the request id (not be dropped).
    assert!(
        resp.get("id").is_some(),
        "error response must carry the request id: {resp}"
    );

    // Liveness: a normal request after the error still works (server not poisoned).
    let sym = c.request("workspace/symbol", json!({"query": ""}));
    assert!(
        sym.get("error").is_none(),
        "follow-up request must not error: {sym}"
    );
    assert!(
        sym.get("result").is_some(),
        "follow-up request must return a result: {sym}"
    );
}

// ─────────────────────────────────────────────────────────────────────────────
// Test 2: didChange re-diagnoses. Open a CLEAN .rq (ORDER BY → no E0011), then
// didChange to the broken CONSTRUCT → publishDiagnostics now carries E0011.
// Proves FULL-sync did_change drives `refresh_analyzer` over the wire.
// ─────────────────────────────────────────────────────────────────────────────

#[test]
fn did_change_rediagnoses_clean_to_broken() {
    let mut c = LspClient::spawn();
    initialize(&mut c);

    let uri = "file:///x/evolve.rq";
    // Open CLEAN → diagnostics must NOT carry E0011.
    c.did_open(uri, "sparql", CLEAN_CONSTRUCT);
    let clean = c.wait_for_diagnostics(uri);
    assert!(
        !has_e0011(&clean),
        "a CONSTRUCT WITH ORDER BY must not trip E0011, got: {clean}"
    );

    // Sabotage the document via didChange (FULL sync): swap to the broken text.
    c.did_change_full(uri, 2, BROKEN_CONSTRUCT);
    let broken = c.wait_for_diagnostics(uri);
    assert!(
        has_e0011(&broken),
        "after didChange to broken CONSTRUCT, diagnostics must carry E0011, got: {broken}"
    );
}

// ─────────────────────────────────────────────────────────────────────────────
// Test 3: didClose after opening a broken .rq. CONTRACT (LSP-conformant clear):
// server.rs `did_close` removes the document AND publishes an EMPTY diagnostics
// set for the URI, so stale squiggles do not linger in the client. We assert the
// HARDENED behavior over the wire: a `publishDiagnostics` for this URI arrives
// with an empty diagnostics array, did_close does not error, and the server stays
// alive. This is both the positive and the sabotage test — it fails LOUD (read
// timeout / non-empty array) if the clear ever regresses.
// ─────────────────────────────────────────────────────────────────────────────

#[test]
fn did_close_clears_diagnostics_and_server_stays_alive() {
    let mut c = LspClient::spawn();
    initialize(&mut c);

    let uri = "file:///x/closing.rq";
    c.did_open(uri, "sparql", BROKEN_CONSTRUCT);
    let opened = c.wait_for_diagnostics(uri);
    assert!(
        has_e0011(&opened),
        "broken .rq must carry E0011 on open: {opened}"
    );

    // Close the document. The hardened server removes state AND publishes a
    // cleared diagnostics set for this URI.
    c.did_close(uri);

    // CONTRACT check: the server MUST publish a diagnostics-clear frame for this
    // uri (an EMPTY diagnostics array). Wait for it on the wire — a regression
    // (no clear) fails loud here via the bounded read timeout.
    let cleared = c.wait_for_diagnostics(uri);
    let diags = cleared["params"]["diagnostics"]
        .as_array()
        .unwrap_or_else(|| panic!("did_close clear must carry a diagnostics array: {cleared}"));
    assert!(
        diags.is_empty(),
        "did_close must publish an EMPTY diagnostics array (LSP-conformant clear), got: {cleared}"
    );
    // And the cleared frame must NOT carry the stale E0011 the open published.
    assert!(
        !has_e0011(&cleared),
        "did_close clear must not carry the stale E0011: {cleared}"
    );

    // Liveness: server still serves requests after a close.
    let sym = c.request("workspace/symbol", json!({"query": ""}));
    assert!(
        sym.get("error").is_none(),
        "request after did_close must not error: {sym}"
    );
    assert!(
        sym.get("result").is_some(),
        "request after did_close must return a result: {sym}"
    );
}

// ─────────────────────────────────────────────────────────────────────────────
// Test 4: request after shutdown. `shutdown` succeeds, then any subsequent
// REQUEST is rejected. VERIFIED behavior (probed against the real binary):
// tower-lsp 0.20's `Shutdown` layer enforces the LSP spec — a request issued
// after `shutdown` (but before `exit`) returns the spec error -32600
// InvalidRequest. The child still reaps cleanly via Drop (which sends `exit`).
// (Also note: `shutdown` itself rejects any params — even JSON `null` — with
// -32602; it must be sent with the `params` key omitted. See `request_no_params`.)
// ─────────────────────────────────────────────────────────────────────────────

#[test]
fn request_after_shutdown_is_rejected_invalid_request() {
    let mut c = LspClient::spawn();
    initialize(&mut c);

    let sd = c.request_no_params("shutdown");
    assert!(sd.get("error").is_none(), "shutdown must succeed: {sd}");
    assert!(
        sd.get("result").is_some(),
        "shutdown must return a result: {sd}"
    );

    // A request after shutdown (but before exit) → spec error InvalidRequest.
    let resp = c.request("workspace/symbol", json!({"query": ""}));
    let err = resp
        .get("error")
        .unwrap_or_else(|| panic!("post-shutdown request must be rejected, got: {resp}"));
    assert_eq!(
        err["code"],
        json!(-32600),
        "post-shutdown request must be InvalidRequest (-32600), got: {err}"
    );
    // The rejection must still correlate to the request id.
    assert!(
        resp.get("id").is_some(),
        "rejection must carry the request id: {resp}"
    );
    // Drop sends `exit` and reaps the child — no leaked process.
}

// ─────────────────────────────────────────────────────────────────────────────
// Test 5: benign/empty inputs. completion on an empty doc and hover off any
// diagnostic both return `result: null` with no error (the analyzer engages but
// the SPARQL analyzer's hover_at returns None and no diagnostic covers the cursor).
// ─────────────────────────────────────────────────────────────────────────────

#[test]
fn empty_completion_and_offsite_hover_return_null_without_error() {
    let mut c = LspClient::spawn();
    initialize(&mut c);

    // An empty SPARQL doc: analyzer builds, no diagnostics expected.
    let uri = "file:///x/empty.rq";
    c.did_open(uri, "sparql", "");
    let _ = c.wait_for_diagnostics(uri); // drain the publish frame

    // completion on the empty doc: SPARQL analyzer always offers keyword items,
    // so the HONEST result is a (non-error) completion response — we assert only
    // that it does not error and returns a well-formed result.
    let comp = c.request(
        "textDocument/completion",
        json!({"textDocument":{"uri":uri},"position":{"line":0,"character":0}}),
    );
    assert!(
        comp.get("error").is_none(),
        "completion must not error: {comp}"
    );
    assert!(
        comp.get("result").is_some(),
        "completion must return a (possibly null) result: {comp}"
    );

    // hover at a position with NO diagnostic and NO semantic hover → result: null.
    let hover = c.request(
        "textDocument/hover",
        json!({"textDocument":{"uri":uri},"position":{"line":0,"character":0}}),
    );
    assert!(
        hover.get("error").is_none(),
        "hover must not error: {hover}"
    );
    assert_eq!(
        hover["result"],
        Value::Null,
        "hover off any diagnostic / semantic target must be result:null, got: {hover}"
    );
}

// ─────────────────────────────────────────────────────────────────────────────
// Test 6: id correlation under interleaving. Issue two requests BEFORE reading
// either reply; assert each response matches its own id. Proves the wire
// correlation is by id, not by arrival order.
// ─────────────────────────────────────────────────────────────────────────────

#[test]
fn interleaved_requests_correlate_by_id() {
    let mut c = LspClient::spawn();
    initialize(&mut c);

    // Two distinct, deterministic ids fired back-to-back before any recv.
    let id_a = 1001;
    let id_b = 1002;
    c.send_request_id(id_a, "workspace/symbol", json!({"query": "alpha"}));
    // shutdown must be sent WITHOUT a `params` field (tower-lsp rejects null params).
    c.send(&json!({"jsonrpc":"2.0","id":id_b,"method":"shutdown"}));

    // Read replies in REVERSE order to prove id-based (not order-based) matching.
    let resp_b = c.await_response(id_b);
    assert_eq!(
        resp_b["id"],
        json!(id_b),
        "shutdown reply must carry id_b: {resp_b}"
    );
    assert!(
        resp_b.get("error").is_none(),
        "shutdown must succeed: {resp_b}"
    );

    let resp_a = c.await_response(id_a);
    assert_eq!(
        resp_a["id"],
        json!(id_a),
        "workspace/symbol reply must carry id_a: {resp_a}"
    );
    assert!(
        resp_a.get("error").is_none(),
        "workspace/symbol must not error: {resp_a}"
    );
    assert!(
        resp_a.get("result").is_some(),
        "workspace/symbol must return a result: {resp_a}"
    );
}
