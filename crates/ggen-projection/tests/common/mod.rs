#![allow(dead_code)]
use serde_json::{json, Value};
use std::io::{BufRead, BufReader, Write};
use std::path::{Path, PathBuf};
use std::process::{Child, ChildStdin, Command, Stdio};
use std::sync::mpsc::{self, Receiver};
use std::thread;
use std::time::Duration;
use tempfile::TempDir;

const READ_TIMEOUT: Duration = Duration::from_secs(10);

/// Create a new temporary directory for hermetic test isolation.
pub fn create_temp_dir() -> Result<TempDir, std::io::Error> {
    TempDir::new()
}

/// Write a file with the given relative path and content within a base directory.
pub fn write_template_file(
    dir: &Path, rel_path: &str, content: &str,
) -> Result<PathBuf, std::io::Error> {
    let target_path = dir.join(rel_path);
    if let Some(parent) = target_path.parent() {
        std::fs::create_dir_all(parent)?;
    }
    std::fs::write(&target_path, content)?;
    Ok(target_path)
}

/// A real, non-mocked LSP client that spawns the ggen-lsp server and communicates with it via stdio JSON-RPC.
pub struct TestLspClient {
    stdin: ChildStdin,
    rx: Receiver<Value>,
    child: Child,
    next_id: i64,
    stashed_notifications: Vec<Value>,
    _tmp: TempDir,
}

impl TestLspClient {
    /// Spawn a specific LSP binary with a hermetic working directory.
    pub fn spawn_bin(bin_name: &str) -> Result<Self, anyhow::Error> {
        let bin = if bin_name == "ggen-lsp" {
            assert_cmd::cargo::cargo_bin(bin_name)
        } else {
            // Find the binary in target/debug
            let mut path = std::env::current_exe()?;
            path.pop(); // deps
            path.pop(); // debug
            path.join(bin_name)
        };
        eprintln!("SPAWNING BINARY: {}", bin.display());
        let tmp = TempDir::new()?;

        let mut child = Command::new(&bin)
            .current_dir(tmp.path())
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .stderr(Stdio::null())
            .spawn()
            .map_err(|e| anyhow::anyhow!("Failed to spawn {}: {}", bin.display(), e))?;

        let stdin = child
            .stdin
            .take()
            .ok_or_else(|| anyhow::anyhow!("Failed to open child stdin"))?;
        let stdout = child
            .stdout
            .take()
            .ok_or_else(|| anyhow::anyhow!("Failed to open child stdout"))?;

        let (tx, rx) = mpsc::channel();
        thread::spawn(move || {
            let mut reader = BufReader::new(stdout);
            while let Some(frame) = read_frame_internal(&mut reader) {
                if tx.send(frame).is_err() {
                    break;
                }
            }
        });

        Ok(TestLspClient {
            stdin,
            rx,
            child,
            next_id: 1,
            stashed_notifications: Vec::new(),
            _tmp: tmp,
        })
    }

    /// Spawn the real `ggen-lsp` binary with a hermetic working directory.
    pub fn spawn() -> Result<Self, anyhow::Error> {
        Self::spawn_bin("ggen-lsp")
    }

    /// Send a raw JSON-RPC message.
    pub fn send(&mut self, msg: &Value) -> Result<(), anyhow::Error> {
        let body = serde_json::to_vec(msg)?;
        let header = format!("Content-Length: {}\r\n\r\n", body.len());
        self.stdin.write_all(header.as_bytes())?;
        self.stdin.write_all(&body)?;
        self.stdin.flush()?;
        Ok(())
    }

    /// Receive the next JSON-RPC frame from the server, blocks up to READ_TIMEOUT.
    pub fn recv(&self) -> Result<Value, anyhow::Error> {
        self.rx
            .recv_timeout(READ_TIMEOUT)
            .map_err(|e| anyhow::anyhow!("LSP read timed out or channel closed: {}", e))
    }

    /// Receive the next JSON-RPC frame with custom timeout.
    pub fn recv_timeout(&self, timeout: Duration) -> Result<Value, anyhow::Error> {
        self.rx
            .recv_timeout(timeout)
            .map_err(|e| anyhow::anyhow!("LSP read timed out or channel closed: {}", e))
    }

    /// Send a request and wait for the correlated response.
    pub fn request(&mut self, method: &str, params: Value) -> Result<Value, anyhow::Error> {
        let id = self.next_id;
        self.next_id += 1;

        let mut msg = json!({
            "jsonrpc": "2.0",
            "id": id,
            "method": method
        });
        if !params.is_null() {
            msg["params"] = params;
        }

        self.send(&msg)?;

        loop {
            let f = self.recv()?;
            let f_id = f.get("id").and_then(Value::as_i64);
            let has_result = f.get("result").is_some() || f.get("error").is_some();

            if f_id == Some(id) && has_result {
                return Ok(f);
            }

            if f.get("method").is_some() && f.get("id").is_some() {
                // Server-to-client request: reply with null so server doesn't hang.
                let rid = f.get("id").cloned().unwrap_or(Value::Null);
                self.send(&json!({
                    "jsonrpc": "2.0",
                    "id": rid,
                    "result": null
                }))?;
            } else if f.get("method").is_some() {
                self.stashed_notifications.push(f);
            }
        }
    }

    /// Send a notification to the LSP server.
    pub fn notify(&mut self, method: &str, params: Value) -> Result<(), anyhow::Error> {
        self.send(&json!({
            "jsonrpc": "2.0",
            "method": method,
            "params": params
        }))
    }

    /// Open a virtual file in the LSP server.
    pub fn did_open(
        &mut self, uri: &str, language_id: &str, text: &str,
    ) -> Result<(), anyhow::Error> {
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
        )
    }

    /// Wait for a notification with the given method.
    pub fn wait_for_notification(&mut self, method: &str) -> Result<Value, anyhow::Error> {
        if let Some(pos) = self
            .stashed_notifications
            .iter()
            .position(|n| n.get("method").and_then(Value::as_str) == Some(method))
        {
            return Ok(self.stashed_notifications.remove(pos));
        }

        loop {
            let f = self.recv()?;
            if f.get("method").and_then(Value::as_str) == Some(method) {
                return Ok(f);
            }
        }
    }

    /// Wait for a notification with a custom timeout.
    pub fn wait_for_notification_timeout(
        &mut self, method: &str, timeout: Duration,
    ) -> Result<Value, anyhow::Error> {
        if let Some(pos) = self
            .stashed_notifications
            .iter()
            .position(|n| n.get("method").and_then(Value::as_str) == Some(method))
        {
            return Ok(self.stashed_notifications.remove(pos));
        }

        let start = std::time::Instant::now();
        loop {
            let elapsed = start.elapsed();
            if elapsed >= timeout {
                return Err(anyhow::anyhow!(
                    "Timeout waiting for notification {}",
                    method
                ));
            }
            let f = self.recv_timeout(timeout - elapsed)?;
            if f.get("method").and_then(Value::as_str) == Some(method) {
                return Ok(f);
            }
        }
    }
}

impl Drop for TestLspClient {
    fn drop(&mut self) {
        let _ = self
            .stdin
            .write_all(b"Content-Length: 33\r\n\r\n{\"jsonrpc\":\"2.0\",\"method\":\"exit\"}");
        let _ = self.stdin.flush();
        let _ = self.child.kill();
        let _ = self.child.wait();
    }
}

fn read_frame_internal(reader: &mut impl BufRead) -> Option<Value> {
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
