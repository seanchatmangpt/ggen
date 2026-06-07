use serde_json::{json, Value};
use std::io::{BufRead, BufReader, Read, Write};
use std::process::{Child, ChildStdin, Command, Stdio};
use std::sync::mpsc::{self, Receiver};
use std::{thread, fs};
use std::time::Duration;
use tower_lsp::lsp_types::Url;

const READ_TIMEOUT: Duration = Duration::from_secs(10);

struct LspClient {
    stdin: ChildStdin,
    rx: Receiver<Value>,
    child: Child,
    next_id: i64,
    stashed_messages: Vec<Value>,
}

impl LspClient {
    fn new() -> Self {
        let bin = env!("CARGO_BIN_EXE_wasm4pm-lsp");
        let mut child = Command::new(bin)
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .stderr(Stdio::inherit())
            .spawn()
            .expect("spawn wasm4pm-lsp");

        let stdout = child.stdout.take().expect("take stdout");
        let stdin = child.stdin.take().expect("take stdin");
        let (tx, rx) = mpsc::channel();

        thread::spawn(move || {
            let mut reader = BufReader::new(stdout);
            loop {
                let mut line = String::new();
                if reader.read_line(&mut line).is_err() || line.is_empty() { break; }
                if line.starts_with("Content-Length: ") {
                    let len: usize = line.trim_start_matches("Content-Length: ").trim().parse().unwrap();
                    reader.read_line(&mut line).unwrap(); // consume empty line
                    let mut body = vec![0u8; len];
                    reader.read_exact(&mut body).unwrap();
                    let msg: Value = serde_json::from_slice(&body).unwrap();
                    tx.send(msg).unwrap();
                }
            }
        });

        Self { stdin, rx, child, next_id: 1, stashed_messages: Vec::new() }
    }

    fn send(&mut self, msg: Value) {
        let body = msg.to_string();
        let frame = format!("Content-Length: {}\r\n\r\n{}", body.len(), body);
        self.stdin.write_all(frame.as_bytes()).unwrap();
        self.stdin.flush().unwrap();
    }

    fn request(&mut self, method: &str, params: Value) -> Value {
        let id = self.next_id;
        self.next_id += 1;
        self.send(json!({ "jsonrpc": "2.0", "id": id, "method": method, "params": params }));
        loop {
            let msg = self.rx.recv_timeout(READ_TIMEOUT).expect("LSP request timeout");
            if msg.get("id") == Some(&json!(id)) { return msg; }
            self.stashed_messages.push(msg);
        }
    }

    fn notify(&mut self, method: &str, params: Value) {
        self.send(json!({ "jsonrpc": "2.0", "method": method, "params": params }));
    }

    fn wait_for_notification(&mut self, method: &str) -> Value {
        if let Some(pos) = self.stashed_messages.iter().position(|n| n.get("method") == Some(&json!(method)) && n.get("id").is_none()) {
            return self.stashed_messages.remove(pos);
        }
        loop {
            let msg = self.rx.recv_timeout(READ_TIMEOUT).expect("LSP notification timeout");
            if msg.get("method") == Some(&json!(method)) && msg.get("id").is_none() { return msg; }
            self.stashed_messages.push(msg);
        }
    }

    fn wait_for_request(&mut self, method: &str) -> Value {
        if let Some(pos) = self.stashed_messages.iter().position(|n| n.get("method") == Some(&json!(method)) && n.get("id").is_some()) {
            return self.stashed_messages.remove(pos);
        }
        loop {
            let msg = self.rx.recv_timeout(READ_TIMEOUT).expect("LSP request timeout");
            if msg.get("method") == Some(&json!(method)) && msg.get("id").is_some() { return msg; }
            self.stashed_messages.push(msg);
        }
    }
}

impl Drop for LspClient {
    fn drop(&mut self) {
        let _ = self.child.kill();
    }
}

#[test]
fn test_gc007_sealed_authority_replay_receipt() {
    let mut client = LspClient::new();
    client.request("initialize", json!({ "capabilities": {} })).get("result").expect("initialize result");
    client.notify("initialized", json!({}));

    // 1. Prepare evidence without the verdict receipt
    let current_dir = std::env::current_dir().unwrap();
    let tower_lsp_max_root = current_dir.parent().unwrap().parent().unwrap().to_path_buf();
    let ocel_path = tower_lsp_max_root.join("crates/playground/ocel/admitted_evidence.ocel.json");
    
    let mut ocel: Value = serde_json::from_str(&fs::read_to_string(ocel_path).unwrap()).unwrap();
    if let Some(events) = ocel.get_mut("events").and_then(|e| e.as_array_mut()) {
        events.retain(|e| e.get("event_type") != Some(&json!("ConformanceVerdictEmitted")));
    }
    
    // We write this to a temporary file because the LSP `executeCommand` needs to read it
    let tmp_path = std::env::temp_dir().join("unreceipted_evidence.ocel.json");
    let content = serde_json::to_string(&ocel).unwrap();
    fs::write(&tmp_path, &content).unwrap();

    let uri = Url::from_file_path(&tmp_path).unwrap();

    // 2. Open document and get diagnostics
    client.notify("textDocument/didOpen", json!({
        "textDocument": { "uri": uri.clone(), "languageId": "json", "version": 1, "text": content }
    }));
    
    let notif = client.wait_for_notification("textDocument/publishDiagnostics");
    let diags = notif.get("params").unwrap().get("diagnostics").unwrap().as_array().unwrap();
    
    // It should STILL be FIT (the adapter calculates it in real-time)
    let fit_diag = diags.iter().find(|d| d.get("code").unwrap() == "WASM4PM-VERDICT-FIT").expect("Must have FIT diagnostic");

    // 3. Request code action
    let actions_response = client.request("textDocument/codeAction", json!({
        "textDocument": { "uri": uri.clone() },
        "range": fit_diag.get("range").unwrap(),
        "context": { "diagnostics": vec![fit_diag.clone()] }
    }));

    let actions = actions_response.get("result").unwrap().as_array().unwrap();
    let bind_action = actions.iter().find(|a| a.get("title").unwrap() == "Bind Conformance Receipt").expect("Must have Bind action");

    // 4. Verify the command intent maps to CLAP noun/verb syntax
    let command = bind_action.get("command").unwrap();
    let command_name = command.get("command").unwrap().as_str().unwrap();
    assert_eq!(command_name, "conformance-receipt.bind", "Action must propose proper CLAP noun/verb intent");
}