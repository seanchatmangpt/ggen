//! CP15 real end-to-end proof: a real, running `ggen-mcp` binary, a real
//! file edit on disk (not a unit test calling `push_diagnostics_for_root`
//! directly), and a real MCP client observing the resulting
//! `notifications/resources/updated` push.
//!
//! Mirrors `mcp_protocol_test.rs`'s subprocess/stdio harness -- real binary,
//! real newline-delimited JSON-RPC, no mocks.

use std::io::{BufRead, BufReader, Write};
use std::process::{Child, ChildStdin, Command, Stdio};
use std::sync::mpsc::{self, Receiver};
use std::thread;
use std::time::Duration;

use serde_json::{json, Value};
use tempfile::TempDir;

/// Generous: the watcher's own debounce window is 500ms, plus real process
/// startup/gate-run time.
const READ_TIMEOUT: Duration = Duration::from_secs(20);

struct McpClient {
    stdin: ChildStdin,
    rx: Receiver<Value>,
    child: Child,
    next_id: i64,
    tmp: TempDir,
}

impl McpClient {
    fn spawn(tmp: TempDir) -> Self {
        let bin = assert_cmd::cargo::cargo_bin("ggen-mcp");
        let mut child = Command::new(&bin)
            .current_dir(tmp.path())
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .stderr(Stdio::null())
            .spawn()
            .unwrap_or_else(|e| panic!("spawn {}: {e}", bin.display()));
        let stdin = child.stdin.take().expect("stdin");
        let stdout = child.stdout.take().expect("stdout");
        let (tx, rx) = mpsc::channel();
        thread::spawn(move || {
            for line in BufReader::new(stdout).lines() {
                let Ok(line) = line else { break };
                let trimmed = line.trim();
                if trimmed.is_empty() {
                    continue;
                }
                if let Ok(v) = serde_json::from_str::<Value>(trimmed) {
                    if tx.send(v).is_err() {
                        break;
                    }
                }
            }
        });
        McpClient {
            stdin,
            rx,
            child,
            next_id: 1,
            tmp,
        }
    }

    fn send(&mut self, msg: &Value) {
        let line = serde_json::to_string(msg).expect("serialize");
        self.stdin.write_all(line.as_bytes()).expect("write");
        self.stdin.write_all(b"\n").expect("newline");
        self.stdin.flush().expect("flush");
    }

    fn request(&mut self, method: &str, params: Value) -> Value {
        let id = self.next_id;
        self.next_id += 1;
        let mut msg = json!({"jsonrpc":"2.0","id":id,"method":method});
        if !params.is_null() {
            msg["params"] = params;
        }
        self.send(&msg);
        loop {
            let frame = self
                .rx
                .recv_timeout(READ_TIMEOUT)
                .expect("MCP read timed out -- server never responded");
            if frame.get("id").and_then(Value::as_i64) == Some(id)
                && (frame.get("result").is_some() || frame.get("error").is_some())
            {
                return frame;
            }
        }
    }

    fn initialize(&mut self) -> Value {
        let resp = self.request(
            "initialize",
            json!({"protocolVersion":"2024-11-05",
                   "capabilities":{"resources":{"subscribe":true}},
                   "clientInfo":{"name":"ggen-mcp-watcher-e2e-test","version":"1.0"}}),
        );
        self.send(&json!({"jsonrpc":"2.0","method":"notifications/initialized"}));
        resp
    }

    /// Block until a real `notifications/resources/updated` frame arrives
    /// (any JSON-RPC *notification*, i.e. no `id`, whose `method` matches),
    /// or time out. Ignores every other frame in between (e.g. late
    /// responses to prior requests).
    fn wait_for_resource_update(&mut self) -> Value {
        loop {
            let frame = self
                .rx
                .recv_timeout(READ_TIMEOUT)
                .expect("timed out waiting for notifications/resources/updated");
            if frame.get("id").is_none()
                && frame.get("method").and_then(Value::as_str)
                    == Some("notifications/resources/updated")
            {
                return frame;
            }
        }
    }

    fn read_resource(&mut self, uri: &str) -> Value {
        self.request("resources/read", json!({"uri": uri}))
    }
}

impl Drop for McpClient {
    fn drop(&mut self) {
        let _ = self.child.kill();
        let _ = self.child.wait();
    }
}

/// The real end-to-end path CP15 exists to prove: start a real `ggen-mcp`
/// process over a real project directory with NO violation yet, let it
/// finish its handshake, then perform a real filesystem write introducing a
/// genuine `GGEN-TPL-001` unbound-projection violation (same fixture shape
/// `crate::bridge`'s own proof and `ggen-lsp`'s `check.rs` tests use) --
/// and observe the server's own background watcher notice the real change
/// and push a real `notifications/resources/updated` over the wire, with
/// the notified URI resolving to the real diagnostic content via a real
/// `resources/read` follow-up. No test code calls
/// `push_diagnostics_for_root` directly.
#[test]
fn real_file_edit_triggers_a_real_mcp_push_notification() {
    let tmp = TempDir::new().expect("tempdir");
    // Start clean: a valid manifest + template whose SELECT covers every
    // variable the template consumes, so the initial gate is quiet.
    std::fs::write(
        tmp.path().join("row.tera"),
        r#"{{ row["name"] }}"#,
    )
    .expect("write initial template");
    std::fs::write(
        tmp.path().join("ggen.toml"),
        r#"
[project]
name = "watcher-e2e"
version = "0.1.0"

[ontology]
source = "model.ttl"

[[generation.rules]]
name = "people"
output_file = "people.rs"
query = { inline = "SELECT ?name WHERE { ?p :name ?name }" }
template = { file = "row.tera" }
"#,
    )
    .expect("write ggen.toml");

    let mut c = McpClient::spawn(tmp);
    let init = c.initialize();
    assert_eq!(init["result"]["serverInfo"]["name"], "ggen-mcp");

    // Give the watcher time to finish `notify::RecommendedWatcher`
    // construction (it happens synchronously inside `start_stdio`, before
    // `initialize` can even be answered, but the OS-level inotify/FSEvents
    // registration is a real syscall worth a short grace period).
    thread::sleep(Duration::from_millis(300));

    // The real trigger: edit the template on disk to consume a variable
    // (`title`) the rule's SPARQL SELECT never binds -- a genuine
    // GGEN-TPL-001 violation, written by the OS filesystem, not manufactured
    // in-process.
    let template_path = c.tmp.path().join("row.tera");
    std::fs::write(&template_path, r#"{{ row["title"] }}"#).expect("write violation");

    let notification = c.wait_for_resource_update();
    let uri = notification["params"]["uri"]
        .as_str()
        .expect("notification carries a uri")
        .to_string();
    assert!(
        uri.starts_with("ggen-diagnostic://"),
        "unexpected notified uri: {uri}"
    );
    assert!(
        uri.contains("GGEN-TPL-001"),
        "expected a GGEN-TPL-001 push, got: {uri}"
    );

    // Real `resources/read` follow-up: the notified URI must resolve to the
    // actual diagnostic content, not just an empty ack.
    let read = c.read_resource(&uri);
    let text = read["result"]["contents"][0]["text"]
        .as_str()
        .unwrap_or_else(|| panic!("resources/read returned no text: {read}"));
    assert!(
        text.contains("GGEN-TPL-001"),
        "resource content missing GGEN-TPL-001: {text}"
    );
    assert!(
        text.contains("row.tera"),
        "resource content missing the real file path: {text}"
    );
}
