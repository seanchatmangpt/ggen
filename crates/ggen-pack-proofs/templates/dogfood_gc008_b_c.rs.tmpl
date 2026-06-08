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
    fn new(bin_name: &str) -> Self {
        let mut child = Command::new(bin_name)
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .stderr(Stdio::inherit())
            .spawn()
            .unwrap_or_else(|_| panic!("spawn {}", bin_name));

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
}

impl Drop for LspClient {
    fn drop(&mut self) {
        let _ = self.child.kill();
    }
}

#[test]
fn test_gc008_clap_governed_mutation_route_active() {
    let mut client = LspClient::new(env!("CARGO_BIN_EXE_clap-noun-verb-pack-lsp"));
    client.request("initialize", json!({ "capabilities": {} })).get("result").expect("initialize result");
    client.notify("initialized", json!({}));

    // 1. CLAP Command Grammar Admission (GC008B)
    {
        // Positive Case: Valid CLAP Noun/Verb syntax
        client.request("workspace/executeCommand", json!({
            "command": "conformance-receipt.bind",
            "arguments": ["file:///tmp/valid.ocel.json"]
        }));
        
        let log_msg = client.wait_for_notification("window/logMessage");
        let msg_text = log_msg.get("params").unwrap().get("message").unwrap().as_str().unwrap();
        assert!(msg_text.contains("CLAP Validated"), "Valid CLAP intent should be admitted");

        let log_msg_routing = client.wait_for_notification("window/logMessage");
        let msg_text_routing = log_msg_routing.get("params").unwrap().get("message").unwrap().as_str().unwrap();
        assert!(msg_text_routing.contains("Routing to PackPlan -> Staging -> MutationGate"), "Mutation must route through the lawful spine");

        // Negative Case: Invalid Fake Grammar
        client.request("workspace/executeCommand", json!({
            "command": "wasm4pm.bind_receipt",
            "arguments": ["file:///tmp/valid.ocel.json"]
        }));
        
        let log_msg2 = client.wait_for_notification("window/logMessage");
        let msg_text2 = log_msg2.get("params").unwrap().get("message").unwrap().as_str().unwrap();
        assert!(msg_text2.contains("CLAP Rejected"), "Invalid intent should be rejected by CLAP");
    }
}
