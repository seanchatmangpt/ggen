//! Real MCP wire-protocol test: spawns the actual `ggen-mcp` binary as a
//! subprocess and speaks newline-delimited JSON-RPC over its stdio.
//!
//! This is the only place the tool ANNOTATIONS (`readOnlyHint`,
//! `destructiveHint`, `title`) are verified, because they exist only in the
//! `tools/list` payload a client actually receives -- an in-process call of
//! a tool's pure function never sees them.
//!
//! It also proves the "stdout is sacred" invariant: if anything logged to
//! stdout before or during the session, the frames below would fail to
//! parse as JSON and every assertion here would fail.
//!
//! Chicago TDD: real binary, real stdio, real fixture project on disk.

use std::io::{BufRead, BufReader, Write};
use std::process::{Child, ChildStdin, Command, Stdio};
use std::sync::mpsc::{self, Receiver};
use std::thread;
use std::time::Duration;

use serde_json::{json, Value};
use tempfile::TempDir;

const READ_TIMEOUT: Duration = Duration::from_secs(20);

const GGEN_TOML: &str = r#"
[project]
name = "wire-demo"

[ontology]
source = "ontology.ttl"

[templates]
dir = "templates"
"#;

const ONTOLOGY: &str = r#"
@prefix ex: <http://example.org/> .
ex:alice ex:hasName "alice" .
"#;

struct McpClient {
    stdin: ChildStdin,
    rx: Receiver<Value>,
    child: Child,
    next_id: i64,
    tmp: TempDir,
}

impl McpClient {
    fn spawn() -> Self {
        let bin = assert_cmd::cargo::cargo_bin("ggen-mcp");
        let tmp = TempDir::new().expect("tempdir");
        std::fs::write(tmp.path().join("ggen.toml"), GGEN_TOML).expect("ggen.toml");
        std::fs::write(tmp.path().join("ontology.ttl"), ONTOLOGY).expect("ontology");
        std::fs::create_dir_all(tmp.path().join("templates")).expect("templates dir");

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
                // Any non-JSON line here would mean something polluted
                // stdout -- it is dropped, and the resulting missing frame
                // fails the test loudly via READ_TIMEOUT.
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

    fn root(&self) -> String {
        self.tmp.path().display().to_string()
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
            json!({"protocolVersion":"2024-11-05","capabilities":{},
                   "clientInfo":{"name":"ggen-mcp-wire-test","version":"1.0"}}),
        );
        self.send(&json!({"jsonrpc":"2.0","method":"notifications/initialized"}));
        resp
    }

    /// Call a tool and return its parsed JSON payload (tools return their
    /// result as a JSON string in `content[0].text`).
    fn call_tool(&mut self, name: &str, args: Value) -> Value {
        let resp = self.request("tools/call", json!({"name": name, "arguments": args}));
        let text = resp["result"]["content"][0]["text"]
            .as_str()
            .unwrap_or_else(|| panic!("{name} returned no text content: {resp}"));
        serde_json::from_str(text)
            .unwrap_or_else(|e| panic!("{name} content was not JSON ({e}): {text}"))
    }
}

impl Drop for McpClient {
    fn drop(&mut self) {
        let _ = self.child.kill();
        let _ = self.child.wait();
    }
}

#[test]
fn handshake_advertises_identity_and_all_nine_tools() {
    let mut c = McpClient::spawn();
    let init = c.initialize();
    assert_eq!(init["result"]["serverInfo"]["name"], "ggen-mcp");
    assert_eq!(
        init["result"]["serverInfo"]["version"],
        env!("CARGO_PKG_VERSION")
    );

    let tools = c.request("tools/list", Value::Null);
    let names: Vec<String> = tools["result"]["tools"]
        .as_array()
        .expect("tools array")
        .iter()
        .filter_map(|t| t["name"].as_str().map(String::from))
        .collect();

    for expected in [
        "ggen_query_preview",
        "ggen_config_classify",
        "ggen_frontmatter_schema",
        "ggen_frontmatter_lint",
        "ggen_sync_dry_run",
        "ggen_check_project",
        "ggen_rule_graph",
        "ggen_capability_status",
        "ggen_write_apply",
    ] {
        assert!(
            names.contains(&expected.to_string()),
            "{expected} missing from {names:?}"
        );
    }
    assert_eq!(names.len(), 9, "exactly nine tools, got {names:?}");
}

/// The annotation contract, verifiable only over the wire: every tool is
/// read-only except `ggen_write_apply`, which is the sole destructive one.
#[test]
fn tool_annotations_mark_exactly_one_destructive_tool() {
    let mut c = McpClient::spawn();
    c.initialize();
    let tools = c.request("tools/list", Value::Null);

    let mut destructive = Vec::new();
    for tool in tools["result"]["tools"].as_array().expect("tools array") {
        let name = tool["name"].as_str().expect("name");
        let ann = &tool["annotations"];
        assert!(!ann.is_null(), "{name} must carry annotations");
        assert!(ann["title"].is_string(), "{name} must have a title");
        if ann["destructiveHint"] == json!(true) {
            destructive.push(name.to_string());
        } else {
            assert_eq!(
                ann["readOnlyHint"],
                json!(true),
                "{name} is not destructive, so it must be explicitly read-only"
            );
        }
    }
    assert_eq!(destructive, vec!["ggen_write_apply".to_string()]);
}

/// End-to-end over the real wire: the zero-row case that motivated this
/// crate must round-trip as a SUCCESS carrying `row_count: 0`.
#[test]
fn query_preview_zero_rows_round_trips_over_the_wire() {
    let mut c = McpClient::spawn();
    c.initialize();
    let root = c.root();

    let body = c.call_tool(
        "ggen_query_preview",
        json!({
            "root": root,
            // ex:hasX appears zero times in the fixture ontology.
            "sparql": "SELECT ?s ?v WHERE { ?s <http://example.org/hasX> ?v }"
        }),
    );

    assert_eq!(
        body["ok"],
        json!(true),
        "zero rows is a success, not an error: {body}"
    );
    assert_eq!(body["row_count"], json!(0));
    assert_eq!(body["truncated"], json!(false));
}

/// The success path over the wire, so the zero-row assertion above is
/// discriminating rather than vacuous.
#[test]
fn query_preview_matching_rows_round_trips_over_the_wire() {
    let mut c = McpClient::spawn();
    c.initialize();
    let root = c.root();

    let body = c.call_tool(
        "ggen_query_preview",
        json!({
            "root": root,
            "sparql": "SELECT ?name WHERE { ?s <http://example.org/hasName> ?name }"
        }),
    );
    assert_eq!(body["ok"], json!(true));
    assert_eq!(
        body["row_count"],
        json!(1),
        "the fixture has exactly one hasName"
    );
}

/// A malformed query must come back as a structured, categorized failure
/// over the wire -- not a crash, not an untyped string.
#[test]
fn malformed_query_returns_structured_error_over_the_wire() {
    let mut c = McpClient::spawn();
    c.initialize();
    let root = c.root();

    let resp = c.request(
        "tools/call",
        json!({"name":"ggen_query_preview",
                                       "arguments":{"root":root,"sparql":"SELECT ?s WHERE { ?s"}}),
    );
    let text = resp["result"]["content"][0]["text"]
        .as_str()
        .expect("text content");
    let body: Value = serde_json::from_str(text).expect("error payload is JSON");
    assert_eq!(body["category"], json!("syntax_error"), "got {body}");
}

/// An unknown tool name is a protocol-level error, distinct from a tool
/// that ran and failed.
#[test]
fn unknown_tool_is_a_protocol_error() {
    let mut c = McpClient::spawn();
    c.initialize();
    let resp = c.request(
        "tools/call",
        json!({"name":"ggen_not_a_tool","arguments":{}}),
    );
    assert!(
        resp.get("error").is_some(),
        "expected a JSON-RPC error, got {resp}"
    );
}

/// `ggen_frontmatter_schema` takes no required arguments and must surface
/// `for_each` -- the key the verified friction case never discovered.
#[test]
fn frontmatter_schema_round_trips_and_surfaces_for_each() {
    let mut c = McpClient::spawn();
    c.initialize();
    let body = c.call_tool("ggen_frontmatter_schema", json!({}));
    let keys: Vec<String> = body["keys"]
        .as_array()
        .expect("keys array")
        .iter()
        .filter_map(|k| k["name"].as_str().map(String::from))
        .collect();
    assert!(keys.contains(&"for_each".to_string()), "got {keys:?}");
    assert!(keys.contains(&"to".to_string()));
    assert_eq!(body["projection_modes"].as_array().map(Vec::len), Some(3));
}
