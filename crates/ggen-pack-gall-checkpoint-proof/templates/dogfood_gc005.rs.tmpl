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
    stashed_notifications: Vec<Value>,
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

        Self { stdin, rx, child, next_id: 1, stashed_notifications: Vec::new() }
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
            self.stashed_notifications.push(msg);
        }
    }

    fn notify(&mut self, method: &str, params: Value) {
        self.send(json!({ "jsonrpc": "2.0", "method": method, "params": params }));
    }

    fn wait_for_notification(&mut self, method: &str) -> Value {
        if let Some(pos) = self.stashed_notifications.iter().position(|n| n.get("method") == Some(&json!(method))) {
            return self.stashed_notifications.remove(pos);
        }
        loop {
            let msg = self.rx.recv_timeout(READ_TIMEOUT).expect("LSP notification timeout");
            if msg.get("method") == Some(&json!(method)) { return msg; }
            self.stashed_notifications.push(msg);
        }
    }
}

impl Drop for LspClient {
    fn drop(&mut self) {
        let _ = self.child.kill();
    }
}

#[test]
fn test_gc005_wasm4pm_lsp_observation() {
    let mut client = LspClient::new();
    client.request("initialize", json!({ "capabilities": {} })).get("result").expect("initialize result");
    client.notify("initialized", json!({}));

    let ocel_path = "../playground/ocel/admitted_evidence.ocel.json";
    
    // Test FIT verdict
    {
        let uri = Url::parse("file:///tmp/valid.ocel.json").unwrap();
        let content = fs::read_to_string(ocel_path).expect("read ocel");
        client.notify("textDocument/didOpen", json!({
            "textDocument": { "uri": uri, "languageId": "json", "version": 1, "text": content }
        }));
        
        let notif = client.wait_for_notification("textDocument/publishDiagnostics");
        let diags = notif.get("params").unwrap().get("diagnostics").unwrap().as_array().unwrap();
        
        let fit_diag = diags.iter().find(|d| d.get("code").unwrap() == "WASM4PM-VERDICT-FIT").expect("Must have FIT verdict diagnostic");
        assert!(fit_diag.get("message").unwrap().as_str().unwrap().contains("FIT"));
    }
    
    // Test DEVIATION verdict
    {
        let uri = Url::parse("file:///tmp/deviated.ocel.json").unwrap();
        let mut ocel: Value = serde_json::from_str(&fs::read_to_string(ocel_path).unwrap()).unwrap();
        
        if let Some(events) = ocel.get_mut("events").and_then(|e| e.as_array_mut()) {
            events.retain(|e| !e.get("relationships").and_then(|r| r.as_array()).map(|r| r.iter().any(|o| o.get("objectId") == Some(&json!("GALL-CHECKPOINT-003")))).unwrap_or(false));
        }
        
        let content = serde_json::to_string(&ocel).unwrap();
        client.notify("textDocument/didOpen", json!({
            "textDocument": { "uri": uri, "languageId": "json", "version": 1, "text": content }
        }));
        
        let notif = client.wait_for_notification("textDocument/publishDiagnostics");
        let diags = notif.get("params").unwrap().get("diagnostics").unwrap().as_array().unwrap();
        
        let dev_diag = diags.iter().find(|d| d.get("code").unwrap() == "WASM4PM-VERDICT-DEVIATION").expect("Must have DEVIATION verdict diagnostic");
        assert!(dev_diag.get("message").unwrap().as_str().unwrap().contains("DEVIATION"));
    }

    // Test BLOCKED verdict
    {
        let uri = Url::parse("file:///tmp/blocked.ocel.json").unwrap();
        let mut ocel: Value = serde_json::from_str(&fs::read_to_string(ocel_path).unwrap()).unwrap();
        
        if let Some(events) = ocel.get_mut("events").and_then(|e| e.as_array_mut()) {
            if events.len() > 1 {
                if let Some(attrs) = events[1].get_mut("attributes").and_then(|a| a.as_array_mut()) {
                    attrs.push(json!({"name": "previous_receipt", "value": "tampered_uuid"}));
                }
            }
        }
        
        let content = serde_json::to_string(&ocel).unwrap();
        client.notify("textDocument/didOpen", json!({
            "textDocument": { "uri": uri, "languageId": "json", "version": 1, "text": content }
        }));
        
        let notif = client.wait_for_notification("textDocument/publishDiagnostics");
        let diags = notif.get("params").unwrap().get("diagnostics").unwrap().as_array().unwrap();
        
        let blocked_diag = diags.iter().find(|d| d.get("code").unwrap() == "WASM4PM-VERDICT-BLOCKED").expect("Must have BLOCKED verdict diagnostic");
        assert!(blocked_diag.get("message").unwrap().as_str().unwrap().contains("BLOCKED"));
    }
}
