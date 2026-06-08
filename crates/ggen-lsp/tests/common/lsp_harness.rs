use serde_json::{json, Value};
use std::io::{BufRead, BufReader, Write};
use std::process::{Child, ChildStdin, Command, Stdio};
use std::sync::mpsc::{self, Receiver};
use std::thread;
use std::time::Duration;
use tempfile::TempDir;

const READ_TIMEOUT: Duration = Duration::from_secs(10);

pub struct LspHarness {
    stdin: ChildStdin,
    rx: Receiver<Value>,
    child: Child,
    next_id: i64,
    stashed_notifications: Vec<Value>,
    stashed_responses: Vec<Value>,
    diagnostics_by_uri: std::collections::HashMap<String, Vec<Value>>,
    last_diagnostics: Vec<Value>,
    _tmp: TempDir,
}

impl LspHarness {
    pub fn new() -> Self {
        let bin = assert_cmd::cargo::cargo_bin("ggen-lsp");
        let tmp = TempDir::new().expect("tempdir");
        let routes_dir = tmp.path().join(".agent-admissibility").join("powl");
        std::fs::create_dir_all(&routes_dir).expect("create_dir_all");
        let routes_file = routes_dir.join("repair-routes.json");
        let routes_json = json!({
            "version": 1,
            "source_log_hash": "x",
            "routes": [
                {
                    "id": "mined.proven",
                    "family": "TemplateFailure",
                    "steps": {
                        "nodes": [
                            {
                                "id": "fix",
                                "title": "Apply mined quickfix",
                                "edit": {
                                    "ReplaceSite": {
                                        "text": "# Clean query\n"
                                    }
                                }
                            }
                        ],
                        "edges": []
                    },
                    "description": "mined fix",
                    "provenance": {
                        "Mined": {
                            "confidence": 0.9,
                            "support": 5,
                            "success_rate": 0.8,
                            "first_seen": "2026-05-28T00:00:00+00:00",
                            "last_seen": "2026-05-28T01:00:00+00:00",
                            "source_report_hash": "h"
                        }
                    },
                    "priority": 1
                }
            ]
        });
        {
            let mut file = std::fs::File::create(&routes_file).expect("create routes file");
            file.write_all(serde_json::to_string(&routes_json).unwrap().as_bytes()).expect("write routes");
        }

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
        LspHarness {
            stdin,
            rx,
            child,
            next_id: 1,
            stashed_notifications: Vec::new(),
            stashed_responses: Vec::new(),
            diagnostics_by_uri: std::collections::HashMap::new(),
            last_diagnostics: Vec::new(),
            _tmp: tmp,
        }
    }

    pub fn initialize(&mut self) -> Value {
        let resp = self.request(
            "initialize",
            json!({"processId": null, "rootUri": null, "capabilities": {}}),
        );
        self.notify("initialized", json!({}));
        resp
    }

    pub fn did_open(&mut self, uri: &str, text: &str) {
        let language_id = if uri.ends_with(".ttl") {
            "turtle"
        } else if uri.ends_with(".rq") {
            "sparql"
        } else if uri.ends_with(".tera") {
            "tera"
        } else if uri.ends_with(".toml") {
            "toml"
        } else {
            "plaintext"
        };
        self.notify(
            "textDocument/didOpen",
            json!({
                "textDocument": {
                    "uri": uri,
                    "languageId": language_id,
                    "version": 1,
                    "text": text
                }
            }),
        );
    }

    pub fn did_change(&mut self, uri: &str, new_text: &str) {
        self.notify(
            "textDocument/didChange",
            json!({
                "textDocument": {
                    "uri": uri,
                    "version": 2
                },
                "contentChanges": [
                    {
                        "text": new_text
                    }
                ]
            }),
        );
    }

    pub fn request_code_action(&mut self, uri: &str, range: Value) -> Value {
        let diags = self.diagnostics_by_uri.get(uri).cloned().unwrap_or_default();
        let mut matching_diags = Vec::new();
        if range.is_null() {
            matching_diags = diags.clone();
        } else {
            for d in &diags {
                let same_line = d.get("range")
                    .and_then(|r| r.get("start"))
                    .and_then(|s| s.get("line"))
                    == range.get("start").and_then(|s| s.get("line"));
                if same_line {
                    matching_diags.push(d.clone());
                }
            }
            if matching_diags.is_empty() {
                matching_diags = diags.clone();
            }
        }

        println!("DEBUG: request_code_action - diags={:?}", diags);
        println!("DEBUG: request_code_action - range={:?}", range);
        println!("DEBUG: request_code_action - matching_diags={:?}", matching_diags);

        let actions_resp = self.request(
            "textDocument/codeAction",
            json!({
                "textDocument": { "uri": uri },
                "range": range,
                "context": { "diagnostics": matching_diags }
            }),
        );

        println!("DEBUG: request_code_action - actions_resp={:?}", actions_resp);
        actions_resp
    }

    pub fn execute_command(&mut self, command: &str, args: Value) -> Value {
        let params = if args.is_null() {
            json!({ "command": command })
        } else if args.is_array() {
            json!({ "command": command, "arguments": args })
        } else {
            json!({ "command": command, "arguments": [args] })
        };
        self.request("workspace/executeCommand", params)
    }

    pub fn wait_for_publish_diagnostics(&mut self, uri: &str) -> Value {
        let notification = if let Some(pos) = self.stashed_notifications.iter().position(|n| {
            n.get("method").and_then(Value::as_str) == Some("textDocument/publishDiagnostics")
                && n["params"]["uri"] == json!(uri)
        }) {
            let n = self.stashed_notifications.remove(pos);
            let diags = n["params"]["diagnostics"]
                .as_array()
                .cloned()
                .unwrap_or_default();
            self.diagnostics_by_uri.insert(uri.to_string(), diags.clone());
            self.last_diagnostics = diags;
            n
        } else {
            loop {
                let f = self.recv();
                if f.get("method").and_then(Value::as_str) == Some("textDocument/publishDiagnostics")
                    && f["params"]["uri"] == json!(uri)
                {
                    let diags = f["params"]["diagnostics"]
                        .as_array()
                        .cloned()
                        .unwrap_or_default();
                    self.diagnostics_by_uri.insert(uri.to_string(), diags.clone());
                    self.last_diagnostics = diags;
                    break f;
                } else if f.get("method").is_some() && f.get("id").is_none() {
                    self.stashed_notifications.push(f);
                } else {
                    self.stashed_responses.push(f);
                }
            }
        };
        self.sync_intel_log();
        notification
    }

    pub fn assert_diagnostic_code(&self, code: &str) {
        let found = self.last_diagnostics.iter().any(|d| {
            if let Some(c) = d.get("code") {
                if let Some(s) = c.as_str() {
                    s == code
                } else if let Some(i) = c.as_i64() {
                    i.to_string() == code
                } else {
                    false
                }
            } else {
                false
            }
        });
        assert!(
            found,
            "Expected diagnostic code '{}', but it was not found. Current diagnostics: {:?}",
            code, self.last_diagnostics
        );
    }

    pub fn assert_source_id(&self, source_id: &str) {
        let found = self.last_diagnostics.iter().any(|d| {
            let is_source = d.get("source")
                .and_then(|s| s.as_str())
                .map(|s| s == source_id)
                .unwrap_or(false);
            let is_data_source_id = d.get("data")
                .and_then(|data| data.get("source_id"))
                .and_then(|s| s.as_str())
                .map(|s| s == source_id)
                .unwrap_or(false);
            is_source || is_data_source_id
        });
        assert!(
            found,
            "Expected diagnostic source/source_id '{}', but it was not found. Current diagnostics: {:?}",
            source_id, self.last_diagnostics
        );
    }

    pub fn assert_no_diagnostic_from(&self, source_id: &str) {
        let found = self.last_diagnostics.iter().any(|d| {
            let is_source = d.get("source")
                .and_then(|s| s.as_str())
                .map(|s| s == source_id)
                .unwrap_or(false);
            let is_data_source_id = d.get("data")
                .and_then(|data| data.get("source_id"))
                .and_then(|s| s.as_str())
                .map(|s| s == source_id)
                .unwrap_or(false);
            is_source || is_data_source_id
        });
        assert!(
            !found,
            "Expected NO diagnostics from source/source_id '{}', but found some. Current diagnostics: {:?}",
            source_id, self.last_diagnostics
        );
    }

    pub fn last_diagnostics(&self) -> &Vec<Value> {
        &self.last_diagnostics
    }

    pub fn temp_dir(&self) -> &std::path::Path {
        self._tmp.path()
    }

    fn sync_intel_log(&self) {
        let src = self._tmp.path().join(".ggen").join("ocel").join("agent-edit-events.ocel.jsonl");
        let dest = self._tmp.path().join(".intel").join("log.jsonl");
        if src.exists() {
            if let Some(parent) = dest.parent() {
                let _ = std::fs::create_dir_all(parent);
            }
            if let Ok(content) = std::fs::read_to_string(&src) {
                if let Ok(mut file) = std::fs::File::create(&dest) {
                    let _ = file.write_all(content.as_bytes());
                }
            }
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
        self.rx
            .recv_timeout(READ_TIMEOUT)
            .expect("LSP read timed out — server never sent the awaited frame")
    }

    fn request(&mut self, method: &str, params: Value) -> Value {
        let id = self.next_id;
        self.next_id += 1;
        let mut msg = json!({"jsonrpc":"2.0","id":id,"method":method});
        if !params.is_null() {
            msg["params"] = params;
        }
        self.send(&msg);

        if let Some(pos) = self.stashed_responses.iter().position(|r| {
            r.get("id").and_then(Value::as_i64) == Some(id)
        }) {
            return self.stashed_responses.remove(pos);
        }

        loop {
            let f = self.recv();
            let f_id = f.get("id").and_then(Value::as_i64);
            let has_result = f.get("result").is_some() || f.get("error").is_some();
            if f_id == Some(id) && has_result {
                return f;
            }

            if f.get("method").is_some() && f.get("id").is_some() {
                let rid = f.get("id").cloned().unwrap_or(Value::Null);
                self.send(&json!({"jsonrpc":"2.0","id":rid,"result":null}));
            } else if f.get("method").is_some() {
                self.stashed_notifications.push(f);
            } else if f_id.is_some() && has_result {
                self.stashed_responses.push(f);
            }
        }
    }

    fn notify(&mut self, method: &str, params: Value) {
        self.send(&json!({"jsonrpc":"2.0","method":method,"params":params}));
    }
}

impl Drop for LspHarness {
    fn drop(&mut self) {
        let _ = self
            .stdin
            .write_all(b"Content-Length: 33\r\n\r\n{\"jsonrpc\":\"2.0\",\"method\":\"exit\"}");
        let _ = self.stdin.flush();
        let _ = self.child.kill();
        let _ = self.child.wait();
    }
}

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
    }
    let mut buf = vec![0u8; content_length];
    reader.read_exact(&mut buf).ok()?;
    serde_json::from_slice(&buf).ok()
}
