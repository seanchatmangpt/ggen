//! Real end-to-end proof that receipt-chain integrity is a proactively
//! *pushed* fact, not only something `ggen_receipt_verify` finds on demand:
//! a real, running `ggen-mcp` binary, a real prior sync (real
//! `.ggen-v2/receipt.json` on disk), a real on-disk tamper of the receipt's
//! stored chain hash, and a real MCP client observing the resulting
//! `notifications/resources/updated` push for a `ggen-sync-refusal://...#chain`
//! resource carrying the real `FM-CHAIN-014` code.
//!
//! Mirrors `sync_refusal_push_e2e_test.rs`'s real subprocess/stdio harness
//! exactly -- real binary, real newline-delimited JSON-RPC, no mocks, no
//! unit-level call to `push_receipt_verify_for_root` directly. This is the
//! proof that closes the gap `bridge::push_receipt_verify_for_root`'s doc
//! comment describes: `push_sync_refusal_for_root` alone can never surface
//! `FM-CHAIN-*` (it only ever runs a dry-run sync), so this test exercises
//! the sibling push path instead.

mod common;

use std::io::{BufRead, BufReader, Write};
use std::process::{Child, ChildStdin, Command, Stdio};
use std::sync::mpsc::{self, Receiver};
use std::thread;
use std::time::Duration;

use common::write_frontmatter_project;
use serde_json::{json, Value};
use tempfile::TempDir;

/// Generous: the sync watcher's own debounce window is 2s (coarser than the
/// lint gate's 500ms -- see `watcher.rs::SYNC_DEBOUNCE_WINDOW`), plus real
/// process startup/dry-run time (two full `GraphEngine` rebuilds per push).
const READ_TIMEOUT: Duration = Duration::from_secs(30);

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
                   "clientInfo":{"name":"ggen-mcp-receipt-chain-push-e2e-test","version":"1.0"}}),
        );
        self.send(&json!({"jsonrpc":"2.0","method":"notifications/initialized"}));
        resp
    }

    fn call_tool(&mut self, name: &str, arguments: Value) -> Value {
        self.request("tools/call", json!({"name": name, "arguments": arguments}))
    }

    /// Block until a real `notifications/resources/updated` frame whose
    /// `uri` ends with `#chain` (the fixed key `push_receipt_verify_for_root`
    /// uses -- see `bridge::receipt_chain_refusal_uri`) arrives, or time out.
    /// Ignores every other frame in between (late responses, the lint
    /// watcher's own `ggen-diagnostic://` pushes, or a `#error`/per-path
    /// `ggen-sync-refusal://` push from the sibling dry-run check, none of
    /// which this fixture intentionally triggers but none of which rule
    /// this test out either).
    fn wait_for_chain_refusal_update(&mut self) -> Value {
        loop {
            let frame = self.rx.recv_timeout(READ_TIMEOUT).expect(
                "timed out waiting for a ggen-sync-refusal://...#chain resources/updated push",
            );
            if frame.get("id").is_none()
                && frame.get("method").and_then(Value::as_str)
                    == Some("notifications/resources/updated")
                && frame["params"]["uri"]
                    .as_str()
                    .is_some_and(|u| u.starts_with("ggen-sync-refusal://") && u.ends_with("#chain"))
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

/// The real end-to-end path this gap-closure exists to prove: a real prior
/// sync produces a real signed receipt, a real on-disk tamper of its
/// `chain_hash_hex` is introduced directly (receipt writes are excluded
/// from the watcher's own relevant-paths filter -- see
/// `watcher.rs::ignores_dotggen_and_git_paths` -- so the tamper alone would
/// never be observed), then a real, unrelated, watched file change fires
/// the background watcher, which runs BOTH `push_sync_refusal_for_root`
/// (quiet: the dry run itself still succeeds, nothing tampered there) AND
/// `push_receipt_verify_for_root` (which re-reads the receipt fresh from
/// disk every call and catches the tamper), pushing a real
/// `notifications/resources/updated` for a `ggen-sync-refusal://...#chain`
/// resource whose content carries `FM-CHAIN-014`. No test code calls
/// `push_receipt_verify_for_root` directly.
#[test]
fn corrupted_receipt_chain_triggers_a_real_chain_refusal_push() {
    let tmp = TempDir::new().expect("tempdir");
    write_frontmatter_project(tmp.path());

    let mut c = McpClient::spawn(tmp);
    let init = c.initialize();
    assert_eq!(init["result"]["serverInfo"]["name"], "ggen-mcp");

    // Give both watchers time to finish real OS-level watch registration
    // (synchronous inside `start_stdio`, before `initialize` can even be
    // answered, but the syscalls are worth a grace period).
    thread::sleep(Duration::from_millis(300));

    // Real prior sync: dry-run for a real graph_hash, then a real
    // `ggen_write_apply` producing a real, signed `.ggen-v2/receipt.json`.
    // Tool results come back as a JSON-encoded string inside
    // `result.content[0].text` (`lib.rs::dispatch` -- there is no
    // `structuredContent` field), so this parses that string, not the outer
    // frame directly.
    let dry = c.call_tool("ggen_sync_dry_run", json!({"root": "."}));
    let dry_text = dry["result"]["content"][0]["text"]
        .as_str()
        .unwrap_or_else(|| panic!("ggen_sync_dry_run returned no text: {dry}"));
    let dry_value: Value = serde_json::from_str(dry_text).expect("parse dry-run result JSON");
    let graph_hash = dry_value["graph_hash"]
        .as_str()
        .unwrap_or_else(|| panic!("ggen_sync_dry_run missing graph_hash: {dry_text}"))
        .to_string();

    let apply = c.call_tool(
        "ggen_write_apply",
        json!({"root": ".", "confirm": true, "expected_graph_hash": graph_hash}),
    );
    let apply_is_error = apply["result"]["isError"].as_bool().unwrap_or(false);
    assert!(
        !apply_is_error,
        "ggen_write_apply must succeed on a clean project: {apply}"
    );

    let receipt_path = c.tmp.path().join(".ggen-v2/receipt.json");
    assert!(
        receipt_path.exists(),
        "write_apply must have produced a real receipt file"
    );

    // Real on-disk tamper: flip one hex character of the stored chain hash,
    // exactly as `receipt_verify_test.rs`'s own tamper does, so
    // `handle_receipt_verify_in`'s chain-hash recompute no longer matches.
    let raw = std::fs::read_to_string(&receipt_path).expect("read real receipt");
    let value: Value = serde_json::from_str(&raw).expect("parse real receipt");
    let chain_hash = value["record"]["chain_hash_hex"]
        .as_str()
        .expect("receipt record has chain_hash_hex")
        .to_string();
    let mut tampered = chain_hash.clone();
    let flipped_char = if tampered.starts_with('0') { '1' } else { '0' };
    tampered.replace_range(0..1, &flipped_char.to_string());
    assert_ne!(
        tampered, chain_hash,
        "tamper must actually change the value"
    );
    let occurrences = raw.matches(chain_hash.as_str()).count();
    assert_eq!(
        occurrences, 1,
        "chain_hash_hex must appear exactly once in the raw receipt for an unambiguous \
         substitution -- got {occurrences} occurrences"
    );
    let tampered_raw = raw.replacen(chain_hash.as_str(), tampered.as_str(), 1);
    std::fs::write(&receipt_path, tampered_raw).expect("write tampered receipt");

    // The tamper alone is invisible to the watcher (`.ggen-v2/*` is
    // excluded from `relevant_paths`) -- a real, unrelated, watched write
    // is required to fire the background push. Re-writing the project
    // template with identical content is enough: `notify` still emits a
    // real Modify event regardless of content equality.
    let tmpl_path = c.tmp.path().join("templates/names.tmpl");
    let tmpl_content = std::fs::read_to_string(&tmpl_path).expect("read template");
    std::fs::write(&tmpl_path, tmpl_content).expect("re-write template to trigger the watcher");

    let notification = c.wait_for_chain_refusal_update();
    let uri = notification["params"]["uri"]
        .as_str()
        .expect("notification carries a uri")
        .to_string();
    assert!(
        uri.starts_with("ggen-sync-refusal://") && uri.ends_with("#chain"),
        "unexpected notified uri: {uri}"
    );

    // Real `resources/read` follow-up: the notified URI must resolve to the
    // actual refusal content, carrying the real FM-CHAIN-014 code, not just
    // an empty ack.
    let read = c.read_resource(&uri);
    let text = read["result"]["contents"][0]["text"]
        .as_str()
        .unwrap_or_else(|| panic!("resources/read returned no text: {read}"));
    assert!(
        text.contains("FM-CHAIN-014"),
        "resource content missing the real FM-CHAIN-014 chain-hash-mismatch code: {text}"
    );
    assert!(
        text.contains("chain hash mismatch"),
        "resource content missing the engine's own chain-hash-mismatch message: {text}"
    );
}
