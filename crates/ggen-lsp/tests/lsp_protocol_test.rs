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
use std::time::Duration;
use tempfile::TempDir;

const READ_TIMEOUT: Duration = Duration::from_secs(10);

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
        LspClient { stdin, rx, child, next_id: 1, stashed_notifications: Vec::new(), _tmp: tmp }
    }

    fn send(&mut self, msg: &Value) {
        let body = serde_json::to_vec(msg).expect("serialize");
        let header = format!("Content-Length: {}\r\n\r\n", body.len());
        self.stdin.write_all(header.as_bytes()).expect("write header");
        self.stdin.write_all(&body).expect("write body");
        self.stdin.flush().expect("flush");
    }

    fn recv(&self) -> Value {
        self.rx
            .recv_timeout(READ_TIMEOUT)
            .expect("LSP read timed out — server never sent the awaited frame")
    }

    /// Send a request and return its correlated response, stashing notifications and
    /// replying to any server→client request encountered along the way (avoids deadlock
    /// if the server asks for e.g. workspace/configuration).
    fn request(&mut self, method: &str, params: Value) -> Value {
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
            let f = self.recv();
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
}

impl Drop for LspClient {
    fn drop(&mut self) {
        // Best-effort graceful exit, then hard kill — never leak a server process.
        let _ = self.stdin.write_all(b"Content-Length: 33\r\n\r\n{\"jsonrpc\":\"2.0\",\"method\":\"exit\"}");
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
    // Codified advertise-vs-deliver gap: call/type hierarchy are IMPLEMENTED in
    // server.rs but NOT advertised. If they are ever advertised, flip these asserts.
    assert!(caps["callHierarchyProvider"].is_null(), "call hierarchy is implemented but not advertised");
    assert!(caps["typeHierarchyProvider"].is_null(), "type hierarchy is implemented but not advertised");

    // didOpen a broken SPARQL surface → diagnostics carry E0011.
    let uri = "file:///x/q.rq";
    c.did_open(uri, "sparql", BROKEN_CONSTRUCT);
    let diags = c.wait_for_diagnostics(uri);
    let arr = diags["params"]["diagnostics"].as_array().expect("diagnostics array");
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
    assert!(hover.get("error").is_none(), "hover must not error: {hover}");
    let md = hover["result"]["contents"]["value"]
        .as_str()
        .expect("hover returns markdown contents over the E0011 diagnostic");
    assert!(md.contains("E0011"), "hover card names the E0011 diagnostic: {md}");
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
    assert!(actions.get("error").is_none(), "codeAction must not error: {actions}");
    for a in actions["result"].as_array().into_iter().flatten() {
        if let Some(data) = a.get("data") {
            let env: ggen_lsp::RouteEnvelope = serde_json::from_value(data.clone())
                .expect("code action data deserializes to RouteEnvelope");
            assert_eq!(env.diagnostic_code, "E0011", "offered route targets the E0011 diagnostic");
        }
    }

    // The remaining feature requests must return well-formed results (not JSON-RPC errors).
    for (method, params) in [
        ("textDocument/definition", json!({"textDocument":{"uri":uri},"position":range["start"]})),
        ("textDocument/references", json!({"textDocument":{"uri":uri},"position":range["start"],"context":{"includeDeclaration":true}})),
        ("textDocument/documentSymbol", json!({"textDocument":{"uri":uri}})),
        ("textDocument/completion", json!({"textDocument":{"uri":uri},"position":{"line":0,"character":0}})),
        ("textDocument/formatting", json!({"textDocument":{"uri":uri},"options":{"tabSize":2,"insertSpaces":true}})),
    ] {
        let r = c.request(method, params);
        assert!(r.get("error").is_none(), "{method} must not error: {r}");
        assert!(r.get("result").is_some(), "{method} must return a result");
    }

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
