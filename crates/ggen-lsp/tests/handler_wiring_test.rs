//! Handler-wiring integration court: proves the three landed modules
//! (R80-1 tokens, R80-2 moniker, R80-12 diagnostics_rules) are actually
//! served by the `LanguageServer` surface, not merely advertised. Drives the
//! REAL `ggen-lsp` binary over real stdio JSON-RPC (Chicago: real
//! collaborators, real state). Harness follows `lsp_protocol_test.rs`.

#![allow(clippy::unwrap_used, clippy::expect_used)]
use serde_json::{json, Value};
use std::io::{BufRead, BufReader, Read, Write};
use std::process::{Child, ChildStdin, Command, Stdio};
use std::sync::mpsc::{self, Receiver};
use std::thread;
use std::time::Duration;
use tempfile::TempDir;

const READ_TIMEOUT: Duration = Duration::from_secs(15);
const HANDSHAKE_TIMEOUT: Duration = Duration::from_secs(60);

fn generated_commands_path() -> String {
    std::fs::canonicalize(concat!(
        env!("CARGO_MANIFEST_DIR"),
        "/../ggen-cli/src/generated_commands.rs"
    ))
    .expect("canonicalize fixture")
    .to_string_lossy()
    .into_owned()
}

struct LspClient {
    stdin: ChildStdin,
    rx: Receiver<Value>,
    child: Child,
    next_id: i64,
    stashed: Vec<Value>,
    _tmp: TempDir,
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
            stashed: Vec::new(),
            _tmp: tmp,
        }
    }

    fn send(&mut self, msg: &Value) {
        let body = serde_json::to_vec(msg).expect("serialize");
        let header = format!("Content-Length: {}\r\n\r\n", body.len());
        let _ = self.stdin.write_all(header.as_bytes());
        let _ = self.stdin.write_all(&body);
        let _ = self.stdin.flush();
    }

    fn recv_bounded(&self, timeout: Duration) -> Value {
        self.rx
            .recv_timeout(timeout)
            .expect("LSP read timed out; server never sent the awaited frame")
    }

    fn request_bounded(&mut self, method: &str, params: Value, timeout: Duration) -> Value {
        let id = self.next_id;
        self.next_id += 1;
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
                let rid = f.get("id").cloned().unwrap_or(Value::Null);
                self.send(&json!({"jsonrpc":"2.0","id":rid,"result":null}));
            } else if f.get("method").is_some() {
                self.stashed.push(f);
            }
        }
    }

    fn request(&mut self, method: &str, params: Value) -> Value {
        self.request_bounded(method, params, READ_TIMEOUT)
    }

    fn notify(&mut self, method: &str, params: Value) {
        self.send(&json!({"jsonrpc":"2.0","method":method,"params":params}));
    }

    /// Wait for a publishDiagnostics notification for `uri` carrying `code`.
    /// Checks already-stashed frames first, then reads bounded frames.
    fn wait_for_diagnostic(&mut self, uri: &str, code: &str) -> Value {
        let matches = |n: &Value| {
            n.get("method").and_then(Value::as_str) == Some("textDocument/publishDiagnostics")
                && n["params"]["uri"] == json!(uri)
                && n["params"]["diagnostics"].as_array().is_some_and(|ds| {
                    ds.iter().any(|d| {
                        d["code"].as_str() == Some(code)
                            || d["code"]["string"].as_str() == Some(code)
                    })
                })
        };
        if let Some(pos) = self.stashed.iter().position(matches) {
            return self.stashed.remove(pos);
        }
        loop {
            let f = self.recv_bounded(READ_TIMEOUT);
            if matches(&f) {
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
        let _ = self.child.kill();
        let _ = self.child.wait();
    }
}

fn read_frame(reader: &mut impl BufRead) -> Option<Value> {
    let mut content_length: usize = 0;
    loop {
        let mut line = String::new();
        if reader.read_line(&mut line).ok()? == 0 {
            return None;
        }
        let line = line.trim_end();
        if line.is_empty() {
            break;
        }
        if let Some(rest) = line.to_ascii_lowercase().strip_prefix("content-length:") {
            content_length = rest.trim().parse().ok()?;
        }
    }
    let mut buf = vec![0u8; content_length];
    reader.read_exact(&mut buf).ok()?;
    serde_json::from_slice(&buf).ok()
}

/// Initialize against a real root so the server rebuilds the token engine
/// from the real workspace ontology and ggen.toml generation rules.
fn initialize_at(client: &mut LspClient, root_uri: Option<&str>) -> Value {
    let params = if let Some(root) = root_uri {
        json!({"processId":null,"rootUri":root,"capabilities":{}})
    } else {
        json!({"processId":null,"rootUri":null,"capabilities":{}})
    };
    let resp = client.request_bounded("initialize", params, HANDSHAKE_TIMEOUT);
    client.notify("initialized", json!({}));
    resp
}

fn flat_data(tokens_result: &Value) -> Vec<u32> {
    tokens_result["result"]["data"]
        .as_array()
        .expect("semantic tokens data array")
        .iter()
        .map(|v| v.as_u64().expect("u32 element") as u32)
        .collect()
}

// ─────────────────────────────────────────────────────────────────────────────
// Court 1: semanticTokens/full over the real generated_commands.rs returns
// legend-classified tokens, and full→delta round-trips through apply_edits.
// ─────────────────────────────────────────────────────────────────────────────

#[test]
fn semantic_tokens_full_then_delta_over_generated_commands() {
    let fixture = generated_commands_path();
    let source = std::fs::read_to_string(&fixture).expect("read fixture");
    let uri = format!("file://{fixture}");
    let mut c = LspClient::spawn();
    let init = initialize_at(&mut c, Some("file:///Users/sac/ggen"));
    let legend = &init["result"]["capabilities"]["semanticTokensProvider"]["legend"];
    let legend_types: Vec<&str> = legend["tokenTypes"]
        .as_array()
        .expect("legend tokenTypes")
        .iter()
        .map(|t| t.as_str().expect("legend type str"))
        .collect();
    let const_idx = legend_types
        .iter()
        .position(|t| *t == "const")
        .expect("legend carries the const token type");
    let generated_bit = legend["tokenModifiers"]
        .as_array()
        .expect("legend tokenModifiers")
        .iter()
        .position(|m| m.as_str() == Some("generated"))
        .expect("legend carries the generated modifier");

    c.did_open(&uri, "rust", &source);

    let full = c.request(
        "textDocument/semanticTokens/full",
        json!({
            "textDocument": {"uri": uri}
        }),
    );
    let data = flat_data(&full);
    assert!(!data.is_empty(), "full tokens must not be empty");
    assert_eq!(data.len() % 5, 0, "bit-packed array, 5 fields per token");
    let token_count = data.len() / 5;
    let mut saw_const = false;
    let mut saw_generated = false;
    for chunk in data.chunks_exact(5) {
        let (delta_line, delta_start, length, token_type, mods) =
            (chunk[0], chunk[1], chunk[2], chunk[3], chunk[4]);
        assert!(
            token_type < legend_types.len() as u32,
            "token_type {token_type} indexes the advertised legend"
        );
        if token_type == const_idx as u32 {
            saw_const = true;
        }
        if generated_bit < 32 && (mods >> generated_bit) & 1 == 1 {
            saw_generated = true;
        }
        let _ = (delta_line, delta_start, length);
    }
    assert!(
        saw_const,
        "COMMANDS_REFERENCE/DEFAULT_VERBS consts must classify as `const`"
    );
    assert!(
        saw_generated,
        "ontology-declared generated surface must carry the generated modifier"
    );

    // Delta: request with the minted result_id, apply, compare with a re-full.
    let result_id = full["result"]["resultId"]
        .as_str()
        .expect("resultId")
        .to_string();
    let delta = c.request(
        "textDocument/semanticTokens/full/delta",
        json!({
            "textDocument": {"uri": uri},
            "previousResultId": result_id,
        }),
    );
    let delta_result = &delta["result"];
    let edits = delta_result["edits"].as_array().expect("delta edits");
    // Unchanged text with a matching result_id legitimately yields zero
    // edits; the applied array must still equal a fresh full.
    let mut applied = data.clone();
    for e in edits {
        let start = e["start"].as_u64().expect("edit start") as usize;
        let delete_count = e["deleteCount"].as_u64().expect("deleteCount") as usize;
        let mut next = applied[..start].to_vec();
        if let Some(d) = e["data"].as_array() {
            next.extend(d.iter().map(|v| v.as_u64().expect("u32") as u32));
        }
        next.extend_from_slice(&applied[start + delete_count..]);
        applied = next;
    }
    // The engine classifies the same text twice identically; the delta
    // applied to the previous generation must equal a fresh full.
    let full2 = c.request(
        "textDocument/semanticTokens/full",
        json!({
            "textDocument": {"uri": uri}
        }),
    );
    let data2 = flat_data(&full2);
    assert_eq!(
        applied, data2,
        "applied delta must equal a fresh full on unchanged text"
    );
    let _ = token_count;
}

// ─────────────────────────────────────────────────────────────────────────────
// Court 2: textDocument/moniker at COMMANDS_REFERENCE over the real
// generated_commands.rs resolves to the ggen: scheme moniker.
// ─────────────────────────────────────────────────────────────────────────────

#[test]
fn moniker_on_generated_commands_reference() {
    let fixture = generated_commands_path();
    let source = std::fs::read_to_string(&fixture).expect("read fixture");
    let uri = format!("file://{fixture}");
    let ident_line = source
        .lines()
        .position(|l| l.starts_with("pub const COMMANDS_REFERENCE"))
        .expect("COMMANDS_REFERENCE line");
    let ident_col = "pub const ".len() as u32;

    let mut c = LspClient::spawn();
    initialize_at(&mut c, Some("file:///Users/sac/ggen"));
    c.did_open(&uri, "rust", &source);

    let resp = c.request(
        "textDocument/moniker",
        json!({
            "textDocument": {"uri": uri},
            "position": {"line": ident_line, "character": ident_col + 2}
        }),
    );
    let monikers = resp["result"]
        .as_array()
        .expect("moniker array (got null? handler refused the surface)");
    assert_eq!(monikers.len(), 1, "one moniker for the cursor symbol");
    let m = &monikers[0];
    assert_eq!(m["scheme"], json!("ggen"));
    let identifier = m["identifier"].as_str().expect("identifier");
    assert!(
        identifier.starts_with("ggen:ggen-cli:"),
        "crate segment derives from the crates/ path component: {identifier}"
    );
    assert!(
        identifier.ends_with("COMMANDS_REFERENCE"),
        "leaf identifier resolves: {identifier}"
    );

    // Negative: a hand-written (banner-free, ontology-undeclared) document
    // must refuse the moniker surface.
    let hand_uri = "file:///tmp-not-used/hand_written.rs";
    c.did_open(hand_uri, "rust", "// hand-written\npub fn nothing() {}\n");
    let neg = c.request(
        "textDocument/moniker",
        json!({
            "textDocument": {"uri": hand_uri},
            "position": {"line": 1, "character": 10}
        }),
    );
    assert!(
        neg["result"].is_null(),
        "hand-written surface must not mint monikers"
    );
}

// ─────────────────────────────────────────────────────────────────────────────
// Court 3: diagnostics_rules::run_rules is on the publish path — a stale
// banner fixture (ontology source newer than the generated file) publishes
// GGEN-DIAG-001 over the wire.
// ─────────────────────────────────────────────────────────────────────────────

#[test]
fn diagnostics_rules_fire_on_stale_banner_fixture() {
    let tmp = TempDir::new().expect("tempdir");
    let gen_path = tmp.path().join("generated.rs");
    let source_path = tmp.path().join("ontology.ttl");

    // Write the generated file FIRST, the ontology source SECOND, so the
    // source's mtime is strictly newer (APFS has ns resolution; sequential
    // writes suffice, no sleeps).
    let banner = format!(
        "//! GENERATED by `ggen sync run` from `{}`.\npub struct S;\n",
        source_path.display()
    );
    std::fs::write(&gen_path, &banner).expect("write generated file");
    std::fs::write(&source_path, "@prefix : <http://example.org/> .\n")
        .expect("write ontology source");

    let uri = format!("file://{}", gen_path.display());
    let mut c = LspClient::spawn();
    initialize_at(&mut c, Some(&format!("file://{}", tmp.path().display())));
    c.did_open(&uri, "rust", &banner);

    let frame = c.wait_for_diagnostic(&uri, "GGEN-DIAG-001");
    let diags = frame["params"]["diagnostics"].as_array().expect("diags");
    let hit = diags
        .iter()
        .find(|d| d["code"] == json!("GGEN-DIAG-001"))
        .expect("GGEN-DIAG-001 present in publication");
    assert_eq!(hit["severity"], json!(2), "WARNING severity");
    let message = hit["message"].as_str().expect("message");
    assert!(
        message.contains("ontology.ttl"),
        "message names the stale ontology source: {message}"
    );
}
