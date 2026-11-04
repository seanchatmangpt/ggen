#![allow(
    clippy::unwrap_used,
    clippy::expect_used,
    clippy::panic,
    clippy::needless_raw_string_hashes,
    clippy::duration_suboptimal_units,
    clippy::branches_sharing_code,
    clippy::used_underscore_binding,
    clippy::single_char_pattern,
    clippy::ignore_without_reason,
    clippy::cloned_ref_to_slice_refs,
    clippy::doc_overindented_list_items,
    clippy::match_wildcard_for_single_variants,
    unused_imports
)]
//! Protocol-level test of the ggen-lsp plugin: spawn the REAL `ggen-lsp` binary and
//! drive the exact LSP JSON-RPC-over-stdio wire contract Claude Code uses. This is the
//! end-to-end face of the plugin — the existing 25 tests exercise library functions;
//! this exercises the spawned server as Claude Code actually drives it.
//!
//! Chicago TDD: real binary, real stdio, real fixtures, no mocks. Hermetic: the server
//! is spawned with `current_dir(TempDir)` because `did_open` writes an OCEL log under cwd.
//!
//! Deadlock defense: a dedicated reader thread parses Content-Length frames into an mpsc
//! channel; every read is bounded by `recv_timeout`, so a hang becomes a fast, legible
//! panic instead of a CI stall. `Drop` kills the child so a failed assertion never leaks
//! a `ggen-lsp` holding the pipe.

use serde_json::{json, Value};
use std::io::{BufRead, BufReader, Read, Write};
use std::process::{Child, ChildStdin, Command, Stdio};
use std::sync::mpsc::{self, Receiver};
use std::thread;
use std::time::{Duration, Instant};
use tempfile::TempDir;

/// Per-frame bound once the server is warm (post-handshake). Genuine protocol
/// hangs surface as a fast panic here.
const READ_TIMEOUT: Duration = Duration::from_secs(10);
/// Bound for the FIRST awaited frame (the initialize handshake). That frame
/// carries the child's fork+exec of the ~145 MB debug `ggen-lsp` binary, dyld
/// page-in, and cold-start of the tokio runtime — under a loaded machine
/// (observed load average >80 during parallel agent fan-out builds) this takes
/// 2-10+s and has no protocol meaning, so it gets a generous ceiling instead of
/// a flaky 10s cutoff that misreports startup starvation as a protocol stall.
const HANDSHAKE_TIMEOUT: Duration = Duration::from_secs(60);

/// The broken law surface: an unconstrained CONSTRUCT → E0011 + route template.values-inline.
const BROKEN_CONSTRUCT: &str = "CONSTRUCT { ?s ?p ?o } WHERE { ?s ?p ?o }\n";
/// A clean Turtle document → no ERROR diagnostics.
const CLEAN_TTL: &str = "@prefix : <http://example.org/> .\n:Thing a :Class .\n";

struct LspClient {
    stdin: ChildStdin,
    rx: Receiver<Value>,
    child: Child,
    next_id: i64,
    stashed_notifications: Vec<Value>,
    _tmp: TempDir, // keep the hermetic cwd alive for the child's lifetime
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

    fn recv(&self) -> Value {
        self.recv_bounded(READ_TIMEOUT)
    }

    /// Wait up to `timeout` for the next frame. The handshake (fork+exec of the
    /// freshly linked ~145 MB debug binary, dyld page-in, first request served)
    /// can take tens of seconds on a loaded machine (observed load average 80
    /// during fan-out builds), so `recv`'s tight READ_TIMEOUT applies only once
    /// the server is warm; use `HANDSHAKE_TIMEOUT` for the first awaited frame.
    fn recv_bounded(&self, timeout: Duration) -> Value {
        self.rx.recv_timeout(timeout).expect(
            "LSP read timed out — server never sent the awaited frame (child still running? \
             check `ps` for ggen-lsp; if alive, this is startup latency, not a protocol bug)",
        )
    }

    /// Send a request and return its correlated response, stashing notifications and
    /// replying to any server→client request encountered along the way (avoids deadlock
    /// if the server asks for e.g. workspace/configuration).
    fn request(&mut self, method: &str, params: Value) -> Value {
        self.request_bounded(method, params, READ_TIMEOUT)
    }

    /// `request` with an explicit frame wait bound — the initialize handshake
    /// uses `HANDSHAKE_TIMEOUT` because the first awaited frame includes the
    /// child's fork+exec+startup, which is load-dependent (see `recv_bounded`).
    fn request_bounded(&mut self, method: &str, params: Value, timeout: Duration) -> Value {
        let id = self.next_id;
        self.next_id += 1;
        // Omit the `params` key when null. tower-lsp's `shutdown` (and other param-less
        // methods) reject an explicit `params: null` with -32602 InvalidParams, so a
        // null here must serialize to NO params member, not `"params":null`.
        let mut msg = json!({"jsonrpc":"2.0","id":id,"method":method});
        if !params.is_null() {
            msg["params"] = params;
        }
        self.send(&msg);
        loop {
            let f = self.recv_bounded(timeout);
            let f_id = f.get("id").and_then(Value::as_i64);
            let has_result = f.get("result").is_some() || f.get("error").is_some();
            if f_id == Some(id) && has_result {
                return f;
            }
            if f.get("method").is_some() && f.get("id").is_some() {
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

    /// Block until a `textDocument/publishDiagnostics` for `uri` arrives (checking frames
    /// already stashed during earlier requests first).
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

    /// Bounded poll for a `client/registerCapability` server→client request,
    /// replying `null` to any other server→client request so nothing blocks.
    /// `None` on timeout (absence witness for the negative negotiation case).
    fn wait_for_register_capability(&mut self, timeout: Duration) -> Option<Value> {
        let deadline = Instant::now() + timeout;
        loop {
            let remaining = deadline.saturating_duration_since(Instant::now());
            if remaining.is_zero() {
                return None;
            }
            match self.rx.recv_timeout(remaining) {
                Ok(f) if f["method"] == json!("client/registerCapability") => return Some(f),
                Ok(f) if f.get("method").is_some() && f.get("id").is_some() => {
                    let rid = f.get("id").cloned().unwrap_or(Value::Null);
                    self.send(&json!({"jsonrpc":"2.0","id":rid,"result":null}));
                }
                Ok(_) => continue,
                Err(_) => return None,
            }
        }
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
    initialize_with(client, json!({}))
}

/// `initialize` with explicit client capabilities, then `initialized`.
fn initialize_with(client: &mut LspClient, capabilities: Value) -> Value {
    let resp = client.request_bounded(
        "initialize",
        json!({"processId":null,"rootUri":null,"capabilities":capabilities}),
        HANDSHAKE_TIMEOUT,
    );
    client.notify("initialized", json!({}));
    resp
}

// ─────────────────────────────────────────────────────────────────────────────
// Test 1: the full lifecycle — initialize, diagnostics, features, shutdown.
// ─────────────────────────────────────────────────────────────────────────────

#[test]
fn lsp_full_lifecycle_over_stdio() {
    let mut c = LspClient::spawn();
    let init = initialize(&mut c);
    let caps = &init["result"]["capabilities"];

    // The advertised contract Claude Code's LSP tool drives.
    assert_eq!(init["result"]["serverInfo"]["name"], "ggen-lsp");
    for cap in [
        "hoverProvider",
        "definitionProvider",
        "referencesProvider",
        "documentSymbolProvider",
        "completionProvider",
        "codeActionProvider",
        "renameProvider",
        "documentFormattingProvider",
        "documentRangeFormattingProvider",
        "foldingRangeProvider",
        "inlayHintProvider",
        "codeLensProvider",
        "semanticTokensProvider",
        "workspaceSymbolProvider",
    ] {
        assert!(!caps[cap].is_null(), "initialize must advertise {cap}");
    }
    // Honest advertise-vs-deliver contract (G1): type hierarchy is implemented
    // but its STATIC advertisement is BLOCKED — `typeHierarchyProvider` is not
    // a field of lsp-types-max 26.6.24's ServerCapabilities (dep not bumped).
    // The server advertises it via dynamic registration when the client
    // declares typeHierarchy.dynamicRegistration (see
    // type_hierarchy_advertised_per_negotiation). Call hierarchy remains
    // statically advertised ONLY when the client declares textDocument
    // .callHierarchy support and NOT dynamicRegistration; this client declares
    // neither, so both static providers are null here.
    assert!(
        caps["typeHierarchyProvider"].is_null(),
        "typeHierarchyProvider has no static form in lsp-types-max 26.6.24 (dynamic-registration only)"
    );
    assert!(
        caps["callHierarchyProvider"].is_null(),
        "this client declares no callHierarchy support, so the static provider is not advertised"
    );

    // didOpen a broken SPARQL surface → diagnostics carry E0011.
    let uri = "file:///x/q.rq";
    c.did_open(uri, "sparql", BROKEN_CONSTRUCT);
    let diags = c.wait_for_diagnostics(uri);
    let arr = diags["params"]["diagnostics"]
        .as_array()
        .expect("diagnostics array");
    let e0011 = arr
        .iter()
        .find(|d| d["code"] == json!("E0011"))
        .unwrap_or_else(|| panic!("expected an E0011 diagnostic, got: {arr:?}"));
    let range = e0011["range"].clone();

    // hover at the diagnostic → the LSP plane projects the E0011 route to the human as an
    // OCEL-TON card. This is the WIRE-LEVEL proof of route projection: the seeded routes are
    // advisory (no mechanical edit), so the route surfaces through hover/envelope. (SPARQL's
    // semantic `hover_at` returns None, so the handler deterministically shows the diag card.)
    let hover = c.request(
        "textDocument/hover",
        json!({"textDocument":{"uri":uri},"position":range["start"]}),
    );
    assert!(
        hover.get("error").is_none(),
        "hover must not error: {hover}"
    );
    let md = hover["result"]["contents"]["value"]
        .as_str()
        .expect("hover returns markdown contents over the E0011 diagnostic");
    assert!(
        md.contains("E0011"),
        "hover card names the E0011 diagnostic: {md}"
    );
    assert!(
        md.contains("template.values-inline"),
        "hover card projects the canonical route for E0011: {md}"
    );

    // codeAction with the E0011 diagnostic. The seeded `template.values-inline` route is
    // ADVISORY (EditTemplate::NoOp — "move VALUES inline into ggen.toml" is a human action,
    // not a mechanical text edit), and the only InsertLine route (declare-prefix) carries
    // unfilled `{prefix}`/`{iri}` placeholders — so the server HONESTLY returns no fillable
    // quickfix rather than fabricating one (a fake-success Oracle Gap). Codify that contract:
    // a well-formed response, and IF any action is offered its `data` is the E0011 envelope.
    let actions = c.request(
        "textDocument/codeAction",
        json!({
            "textDocument":{"uri":uri},
            "range":range,
            "context":{"diagnostics":[e0011]}
        }),
    );
    assert!(
        actions.get("error").is_none(),
        "codeAction must not error: {actions}"
    );
    for a in actions["result"].as_array().into_iter().flatten() {
        if let Some(data) = a.get("data") {
            let env: ggen_lsp::RouteEnvelope = serde_json::from_value(data.clone())
                .expect("code action data deserializes to RouteEnvelope");
            assert_eq!(
                env.diagnostic_code, "E0011",
                "offered route targets the E0011 diagnostic"
            );
        }
    }

    // The remaining feature requests must return well-formed results (not JSON-RPC errors).
    for (method, params) in [
        (
            "textDocument/definition",
            json!({"textDocument":{"uri":uri},"position":range["start"]}),
        ),
        (
            "textDocument/references",
            json!({"textDocument":{"uri":uri},"position":range["start"],"context":{"includeDeclaration":true}}),
        ),
        (
            "textDocument/documentSymbol",
            json!({"textDocument":{"uri":uri}}),
        ),
        (
            "textDocument/completion",
            json!({"textDocument":{"uri":uri},"position":{"line":0,"character":0}}),
        ),
        (
            "textDocument/formatting",
            json!({"textDocument":{"uri":uri},"options":{"tabSize":2,"insertSpaces":true}}),
        ),
        (
            "textDocument/moniker",
            json!({"textDocument":{"uri":uri},"position":range["start"]}),
        ),
        (
            "textDocument/prepareTypeHierarchy",
            json!({"textDocument":{"uri":uri},"position":range["start"]}),
        ),
        (
            "textDocument/prepareCallHierarchy",
            json!({"textDocument":{"uri":uri},"position":range["start"]}),
        ),
        (
            "textDocument/foldingRange",
            json!({"textDocument":{"uri":uri}}),
        ),
        (
            "textDocument/inlayHint",
            json!({"textDocument":{"uri":uri},"range":{"start":{"line":0,"character":0},"end":{"line":5,"character":0}}}),
        ),
        (
            "textDocument/codeLens",
            json!({"textDocument":{"uri":uri}}),
        ),
        (
            "workspace/symbol",
            json!({"query":""}),
        ),
    ] {
        let r = c.request(method, params);
        assert!(r.get("error").is_none(), "{method} must not error: {r}");
        assert!(r.get("result").is_some(), "{method} must return a result");
    }

    // semanticTokens full → full/delta resultId round-trip (G5): the delta
    // request carrying the full request's resultId must be accepted
    // (error-free) and return the delta shape (resultId + edits array).
    let gen_uri = "file:///x/generated.rs";
    c.did_open(
        gen_uri,
        "rust",
        "// Code generated by ggen. DO NOT EDIT.\npub mod generated_commands {\n    pub const COMMANDS: &str = \"build\";\n}\n",
    );
    let full = c.request(
        "textDocument/semanticTokens/full",
        json!({"textDocument":{"uri":gen_uri}}),
    );
    assert!(
        full.get("error").is_none(),
        "semanticTokens/full must not error: {full}"
    );
    let result_id = full["result"]["resultId"]
        .as_str()
        .expect("full must mint a resultId")
        .to_string();
    let delta = c.request(
        "textDocument/semanticTokens/full/delta",
        json!({"textDocument":{"uri":gen_uri},"previousResultId":result_id}),
    );
    assert!(
        delta.get("error").is_none(),
        "semanticTokens/full/delta must not error: {delta}"
    );
    assert!(
        delta["result"]["edits"].is_array(),
        "delta result carries an edits array: {delta}"
    );
    assert!(
        delta["result"]["resultId"].is_string(),
        "delta result carries a resultId: {delta}"
    );

    // A clean Turtle surface → no ERROR-severity diagnostics.
    let clean_uri = "file:///x/clean.ttl";
    c.did_open(clean_uri, "turtle", CLEAN_TTL);
    let clean = c.wait_for_diagnostics(clean_uri);
    let errors = clean["params"]["diagnostics"]
        .as_array()
        .map(|a| a.iter().filter(|d| d["severity"] == json!(1)).count())
        .unwrap_or(0);
    assert_eq!(errors, 0, "a clean .ttl must produce no ERROR diagnostics");

    // shutdown / exit handshake.
    let sd = c.request("shutdown", json!(null));
    assert!(sd.get("error").is_none(), "shutdown must succeed");
    // exit is sent by Drop; the child is reaped there.
}

// ─────────────────────────────────────────────────────────────────────────────
// Test 2: each law-surface extension routes to its analyzer (the analyzer engages
// and publishes diagnostics for that file type over the wire).
// ─────────────────────────────────────────────────────────────────────────────

#[test]
fn lsp_each_law_surface_engages_its_analyzer() {
    let mut c = LspClient::spawn();
    initialize(&mut c);

    // (uri, languageId, text) — every recognized surface must yield a
    // publishDiagnostics frame, proving FileType→analyzer dispatch over the wire.
    let cases = [
        ("file:///x/a.ttl", "turtle", CLEAN_TTL),
        ("file:///x/b.rq", "sparql", BROKEN_CONSTRUCT),
        ("file:///x/c.tera", "tera", "{{ value }}\n"),
        ("file:///x/ggen.toml", "toml", "[project]\nname = \"x\"\n"),
    ];
    for (uri, lang, text) in cases {
        c.did_open(uri, lang, text);
        let diags = c.wait_for_diagnostics(uri);
        assert!(
            diags["params"]["diagnostics"].is_array(),
            "{uri}: analyzer must engage and publish a (possibly empty) diagnostics array"
        );
        // The SPARQL surface specifically must carry E0011 (precise-code proof).
        if uri.ends_with(".rq") {
            let has_e0011 = diags["params"]["diagnostics"]
                .as_array()
                .map(|a| a.iter().any(|d| d["code"] == json!("E0011")))
                .unwrap_or(false);
            assert!(has_e0011, "broken .rq must carry E0011");
        }
    }
}

// ─────────────────────────────────────────────────────────────────────────────
// Test 3 (G1): type hierarchy advertisement is negotiated, not static.
// lsp-types-max 26.6.24 has no typeHierarchyProvider capability field, so the
// server advertises textDocument/prepareTypeHierarchy ONLY via dynamic
// registration, and ONLY when the client declares dynamicRegistration.
// ─────────────────────────────────────────────────────────────────────────────

#[test]
fn type_hierarchy_advertised_per_negotiation() {
    // Client A declares typeHierarchy dynamicRegistration → the server must
    // send client/registerCapability with the prepareTypeHierarchy method
    // after `initialized`.
    let mut a = LspClient::spawn();
    let init_a = initialize_with(
        &mut a,
        json!({"textDocument":{"typeHierarchy":{"dynamicRegistration":true}}}),
    );
    assert!(
        init_a["result"]["capabilities"]["typeHierarchyProvider"].is_null(),
        "static typeHierarchyProvider is null in both cases (no field in lsp-types-max 26.6.24)"
    );
    let reg = a
        .wait_for_register_capability(Duration::from_secs(10))
        .expect("declaring typeHierarchy.dynamicRegistration must yield client/registerCapability");
    let registrations = reg["params"]["registrations"]
        .as_array()
        .expect("registrations array");
    assert!(
        registrations
            .iter()
            .any(|r| r["method"] == json!("textDocument/prepareTypeHierarchy")),
        "registerCapability must carry prepareTypeHierarchy: {registrations:?}"
    );

    // Client B declares nothing → no registerCapability frame. Absence is
    // witnessed by a bounded wait AFTER a completed request/response
    // round-trip (the server's initialized handler has long finished by then).
    let mut b = LspClient::spawn();
    let init_b = initialize_with(&mut b, json!({}));
    assert!(
        init_b["result"]["capabilities"]["typeHierarchyProvider"].is_null(),
        "static typeHierarchyProvider is null in both cases"
    );
    let sym = b.request("workspace/symbol", json!({"query":""}));
    assert!(
        sym.get("error").is_none(),
        "follow-up request must succeed: {sym}"
    );
    assert!(
        b.wait_for_register_capability(Duration::from_secs(1)).is_none(),
        "a client without dynamicRegistration must not receive client/registerCapability"
    );
}
