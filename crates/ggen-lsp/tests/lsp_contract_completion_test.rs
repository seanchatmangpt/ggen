#![allow(
    clippy::expect_used,
    clippy::panic,
    clippy::unwrap_used,
    clippy::needless_raw_string_hashes
)]
//! Completion contract for the LSP hierarchy and gate lifecycle.
//!
//! This test spawns the real `ggen-lsp` binary and drives JSON-RPC over stdio.
//! It proves negotiated hierarchy capabilities, all four hierarchy follow-up
//! methods, workspace-folder root selection, and orderly gate cleanup.

use serde_json::{json, Value};
use std::io::{BufRead, BufReader, Read, Write};
use std::process::{Child, ChildStdin, Command, Stdio};
use std::sync::mpsc::{self, Receiver};
use std::thread;
use std::time::Duration;
use tempfile::TempDir;

const READ_TIMEOUT: Duration = Duration::from_secs(10);

struct LspClient {
    stdin: ChildStdin,
    rx: Receiver<Value>,
    child: Child,
    next_id: i64,
    process_root: TempDir,
}

impl LspClient {
    fn spawn() -> Self {
        let binary = assert_cmd::cargo::cargo_bin("ggen-lsp");
        let process_root = TempDir::new().expect("process root");
        let mut child = Command::new(&binary)
            .current_dir(process_root.path())
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .stderr(Stdio::null())
            .spawn()
            .unwrap_or_else(|error| panic!("spawn {}: {error}", binary.display()));
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
        Self {
            stdin,
            rx,
            child,
            next_id: 1,
            process_root,
        }
    }

    fn send(&mut self, message: &Value) {
        let body = serde_json::to_vec(message).expect("serialize LSP frame");
        let header = format!("Content-Length: {}\r\n\r\n", body.len());
        self.stdin
            .write_all(header.as_bytes())
            .expect("write LSP header");
        self.stdin.write_all(&body).expect("write LSP body");
        self.stdin.flush().expect("flush LSP frame");
    }

    fn receive(&self) -> Value {
        self.rx
            .recv_timeout(READ_TIMEOUT)
            .expect("LSP response timed out")
    }

    fn request(&mut self, method: &str, params: Value) -> Value {
        let id = self.next_id;
        self.next_id += 1;
        let mut message = json!({"jsonrpc": "2.0", "id": id, "method": method});
        if !params.is_null() {
            message["params"] = params;
        }
        self.send(&message);

        loop {
            let frame = self.receive();
            let frame_id = frame.get("id").and_then(Value::as_i64);
            if frame_id == Some(id)
                && (frame.get("result").is_some() || frame.get("error").is_some())
            {
                return frame;
            }
            if frame.get("method").is_some() && frame.get("id").is_some() {
                let request_id = frame.get("id").cloned().unwrap_or(Value::Null);
                self.send(&json!({"jsonrpc": "2.0", "id": request_id, "result": null}));
            }
        }
    }

    fn notify(&mut self, method: &str, params: Value) {
        self.send(&json!({"jsonrpc": "2.0", "method": method, "params": params}));
    }

    fn accept_server_request(&mut self, method: &str) -> Value {
        loop {
            let frame = self.receive();
            if frame.get("method").and_then(Value::as_str) == Some(method)
                && frame.get("id").is_some()
            {
                let id = frame.get("id").cloned().unwrap_or(Value::Null);
                self.send(&json!({"jsonrpc": "2.0", "id": id, "result": null}));
                return frame;
            }
        }
    }
}

impl Drop for LspClient {
    fn drop(&mut self) {
        let _ = self.stdin.write_all(
            b"Content-Length: 33\r\n\r\n{\"jsonrpc\":\"2.0\",\"method\":\"exit\"}",
        );
        let _ = self.stdin.flush();
        let _ = self.child.kill();
        let _ = self.child.wait();
        let _ = self.process_root.path();
    }
}

fn read_frame(reader: &mut impl BufRead) -> Option<Value> {
    let mut content_length = 0usize;
    loop {
        let mut line = String::new();
        if reader.read_line(&mut line).ok()? == 0 {
            return None;
        }
        let line = line.trim_end();
        if line.is_empty() {
            break;
        }
        if let Some(value) = line
            .to_ascii_lowercase()
            .strip_prefix("content-length:")
        {
            content_length = value.trim().parse().ok()?;
        }
    }
    let mut body = vec![0u8; content_length];
    reader.read_exact(&mut body).ok()?;
    serde_json::from_slice(&body).ok()
}

fn hierarchy_item(uri: &str) -> Value {
    json!({
        "name": "Example",
        "kind": 5,
        "uri": uri,
        "range": {
            "start": {"line": 0, "character": 0},
            "end": {"line": 0, "character": 7}
        },
        "selectionRange": {
            "start": {"line": 0, "character": 0},
            "end": {"line": 0, "character": 7}
        }
    })
}

#[test]
fn negotiated_hierarchy_and_workspace_gate_are_live_over_stdio() {
    let mut client = LspClient::spawn();
    let workspace = TempDir::new().expect("workspace root");
    let workspace_uri = url::Url::from_directory_path(workspace.path())
        .expect("workspace file URI")
        .to_string();

    let initialize = client.request(
        "initialize",
        json!({
            "processId": null,
            "rootUri": null,
            "workspaceFolders": [{"uri": workspace_uri, "name": "workspace"}],
            "capabilities": {
                "textDocument": {
                    "callHierarchy": {"dynamicRegistration": true},
                    "typeHierarchy": {"dynamicRegistration": true}
                }
            }
        }),
    );
    assert!(
        initialize.get("error").is_none(),
        "initialize must succeed: {initialize}"
    );
    client.notify("initialized", json!({}));
    let registration = client.accept_server_request("client/registerCapability");
    let methods: Vec<&str> = registration["params"]["registrations"]
        .as_array()
        .expect("registration array")
        .iter()
        .filter_map(|entry| entry["method"].as_str())
        .collect();
    assert!(
        methods.contains(&"textDocument/prepareCallHierarchy"),
        "call hierarchy must be dynamically registered: {registration}"
    );
    assert!(
        methods.contains(&"textDocument/prepareTypeHierarchy"),
        "type hierarchy must be dynamically registered: {registration}"
    );

    let item = hierarchy_item("file:///workspace/example.ttl");
    for (method, params) in [
        ("callHierarchy/incomingCalls", json!({"item": item.clone()})),
        ("callHierarchy/outgoingCalls", json!({"item": item.clone()})),
        ("typeHierarchy/supertypes", json!({"item": item.clone()})),
        ("typeHierarchy/subtypes", json!({"item": item.clone()})),
    ] {
        let response = client.request(method, params);
        assert!(
            response.get("error").is_none(),
            "{method} must not return a JSON-RPC error: {response}"
        );
        assert!(
            response["result"].is_array(),
            "{method} must return a relation array: {response}"
        );
    }

    let shutdown = client.request("shutdown", Value::Null);
    assert!(
        shutdown.get("error").is_none(),
        "shutdown must succeed: {shutdown}"
    );
    let gate = workspace.path().join(".ggen/lambda_cd.gate");
    assert_eq!(
        std::fs::read_to_string(&gate).expect("shutdown gate file"),
        "0",
        "shutdown must create and open the gate under workspaceFolders[0]"
    );
}
