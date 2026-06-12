#![allow(
    clippy::unwrap_used,
    clippy::expect_used,
    clippy::panic,
    clippy::needless_raw_string_hashes,
    clippy::duration_suboptimal_units,
    clippy::branches_sharing_code,
    clippy::used_underscore_binding,
    clippy::single_char_pattern,
    clippy::ignore_without_reason,
    clippy::cloned_ref_to_slice_refs,
    clippy::doc_overindented_list_items,
    clippy::match_wildcard_for_single_variants,
    clippy::ignored_unit_patterns,
    clippy::needless_collect,
    clippy::unnecessary_map_or,
    clippy::manual_flatten,
    clippy::manual_strip,
    clippy::future_not_send,
    clippy::unnested_or_patterns,
    clippy::no_effect_underscore_binding,
    clippy::literal_string_with_formatting_args
)]
//! Protocol-level test of the ggen-lsp-mcp plugin: spawn the REAL `ggen-lsp-mcp`
//! binary and drive the MCP JSON-RPC-over-stdio handshake an MCP client (Claude Code,
//! via `.mcp.json`) performs — initialize → tools/list → tools/call. The existing
//! library tests prove route parity; this proves the MCP *wire protocol*.
//!
//! rmcp's stdio transport is NEWLINE-delimited JSON-RPC (one JSON object per line),
//! not Content-Length framed. Chicago TDD: real binary, real stdio, real fixture.

use serde_json::{json, Value};
use std::io::{BufRead, BufReader, Write};
use std::process::{Child, ChildStdin, Command, Stdio};
use std::sync::mpsc::{self, Receiver};
use std::thread;
use std::time::Duration;
use tempfile::TempDir;

const READ_TIMEOUT: Duration = Duration::from_secs(10);
const BROKEN_CONSTRUCT: &str = "CONSTRUCT { ?s ?p ?o } WHERE { ?s ?p ?o }\n";
/// A SPARQL SELECT with no VALUES and no CONSTRUCT — the sparql_analyzer raises
/// neither E0010 nor E0011, so it is a clean law surface (envelopes empty).
const CLEAN_SELECT: &str = "SELECT ?s WHERE { ?s ?p ?o }\n";

struct McpClient {
    stdin: ChildStdin,
    rx: Receiver<Value>,
    child: Child,
    next_id: i64,
    _tmp: TempDir,
}

impl McpClient {
    fn spawn() -> Self {
        let bin = assert_cmd::cargo::cargo_bin("ggen-lsp-mcp");
        let tmp = TempDir::new().expect("tempdir");
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
            let reader = BufReader::new(stdout);
            for line in reader.lines() {
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
                // non-JSON lines (shouldn't occur — NoSubscriber tracing) are ignored
            }
        });
        McpClient {
            stdin,
            rx,
            child,
            next_id: 1,
            _tmp: tmp,
        }
    }

    fn send(&mut self, msg: &Value) {
        let line = serde_json::to_string(msg).expect("serialize");
        self.stdin.write_all(line.as_bytes()).expect("write");
        self.stdin.write_all(b"\n").expect("newline");
        self.stdin.flush().expect("flush");
    }

    fn recv(&self) -> Value {
        self.rx
            .recv_timeout(READ_TIMEOUT)
            .expect("MCP read timed out — server never responded")
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
            let f = self.recv();
            if f.get("id").and_then(Value::as_i64) == Some(id)
                && (f.get("result").is_some() || f.get("error").is_some())
            {
                return f;
            }
            // skip notifications / unrelated frames
        }
    }

    fn notify(&mut self, method: &str) {
        self.send(&json!({"jsonrpc":"2.0","method":method}));
    }

    fn initialize(&mut self) -> Value {
        let resp = self.request(
            "initialize",
            json!({"protocolVersion":"2024-11-05","capabilities":{},
                   "clientInfo":{"name":"mcp-protocol-test","version":"1.0"}}),
        );
        self.notify("notifications/initialized");
        resp
    }
}

impl Drop for McpClient {
    fn drop(&mut self) {
        let _ = self.child.kill();
        let _ = self.child.wait();
    }
}

#[test]
fn mcp_handshake_lists_three_tools_with_current_identity() {
    let mut c = McpClient::spawn();
    let init = c.initialize();
    assert_eq!(init["result"]["serverInfo"]["name"], "ggen-lsp-mcp");
    assert_eq!(
        init["result"]["serverInfo"]["version"], env!("CARGO_PKG_VERSION"),
        "MCP server advertises the workspace version identity"
    );

    let tools = c.request("tools/list", Value::Null);
    let names: Vec<String> = tools["result"]["tools"]
        .as_array()
        .expect("tools array")
        .iter()
        .filter_map(|t| t["name"].as_str().map(str::to_string))
        .collect();
    for expected in [
        "ggen.lsp.repair_route",
        "ggen.lsp.replay_case",
        "ggen.lsp.metrics",
    ] {
        assert!(
            names.contains(&expected.to_string()),
            "tools/list must include {expected}; got {names:?}"
        );
    }
    assert_eq!(names.len(), 3, "exactly three delivery tools");
}

#[test]
fn mcp_repair_route_returns_e0011_envelope() {
    let mut c = McpClient::spawn();
    c.initialize();

    let resp = c.request(
        "tools/call",
        json!({
            "name":"ggen.lsp.repair_route",
            "arguments":{"file_path":"q.rq","file_content":BROKEN_CONSTRUCT}
        }),
    );
    assert!(
        resp.get("error").is_none(),
        "repair_route must not error: {resp}"
    );
    // The tool result content[0].text is the RouteEnvelope-bearing JSON document.
    let text = resp["result"]["content"][0]["text"]
        .as_str()
        .expect("tool result carries text content");
    let doc: Value = serde_json::from_str(text).expect("tool text is JSON");
    assert_eq!(
        doc["is_law_surface"],
        json!(true),
        "the .rq is a law surface"
    );
    let env = &doc["envelopes"][0];
    assert_eq!(
        env["diagnostic_code"],
        json!("E0011"),
        "MCP route carries E0011"
    );
    assert_eq!(
        env["route_id"],
        json!("template.values-inline"),
        "MCP route_id matches the LSP/A2A engine"
    );
}

#[test]
fn mcp_repair_route_refuses_empty_path_without_panicking() {
    let mut c = McpClient::spawn();
    c.initialize();
    let resp = c.request(
        "tools/call",
        json!({"name":"ggen.lsp.repair_route","arguments":{"file_path":"","file_content":"x"}}),
    );
    // A structured refusal — an error result or an isError tool result — NOT a crash.
    let refused = resp.get("error").is_some()
        || resp["result"]["isError"] == json!(true)
        || resp["result"]["content"][0]["text"]
            .as_str()
            .map(|t| t.to_lowercase().contains("empty") || t.to_lowercase().contains("invalid"))
            .unwrap_or(false);
    assert!(
        refused,
        "empty file_path must be a structured refusal, got: {resp}"
    );
    // Server still alive: a follow-up request still works.
    let tools = c.request("tools/list", Value::Null);
    assert!(
        tools["result"]["tools"].is_array(),
        "server survives a refusal"
    );
}

/// Parse the structured JSON document carried in a tool result's `content[0].text`.
/// `call_tool` serializes each tool's result via `to_string_pretty`, so every
/// success result is a single text Content whose body is the real result JSON.
fn tool_doc(resp: &Value) -> Value {
    assert!(
        resp.get("error").is_none(),
        "tool call must not JSON-RPC error: {resp}"
    );
    let text = resp["result"]["content"][0]["text"]
        .as_str()
        .unwrap_or_else(|| panic!("tool result carries text content: {resp}"));
    serde_json::from_str(text).unwrap_or_else(|e| panic!("tool text must be JSON ({e}): {text}"))
}

#[test]
fn mcp_replay_case_returns_structured_case_replay() {
    // The server runs in a fresh TempDir (no .ggen OCEL log), so a known-absent
    // case_id must replay to the honest "not found" CaseReplay — found=false,
    // event_count=0 — proving replay_case_result -> ggen_lsp::replay_case is wired
    // end to end and returns the REAL CaseReplay shape (serde field names).
    let mut c = McpClient::spawn();
    c.initialize();

    let resp = c.request(
        "tools/call",
        json!({
            "name":"ggen.lsp.replay_case",
            "arguments":{"case_id":"c:does-not-exist","root":"."}
        }),
    );
    let doc = tool_doc(&resp);
    // Real CaseReplay fields (crates/ggen-lsp/src/intel/replay.rs).
    assert_eq!(
        doc["case_id"],
        json!("c:does-not-exist"),
        "echoes the requested case_id"
    );
    assert_eq!(
        doc["found"],
        json!(false),
        "no OCEL log in a fresh project => not found"
    );
    assert_eq!(doc["event_count"], json!(0), "zero events reconstructed");
    assert_eq!(
        doc["conformant"],
        json!(false),
        "no closure can be proven from no events"
    );
    assert!(
        doc.get("diagnostic_code").is_some(),
        "CaseReplay carries diagnostic_code (null here)"
    );
    assert!(
        doc.get("route_id").is_some(),
        "CaseReplay carries route_id (null here)"
    );
    assert!(
        doc.get("receipt_id").is_some(),
        "CaseReplay carries receipt_id (null here)"
    );
}

#[test]
fn mcp_replay_case_without_case_id_verifies_promotion_binding() {
    // Omitting case_id switches replay_case to the promotion tamper-check
    // (ggen_lsp::verify_promotion). A fresh project has no promoted-route artifact,
    // so the honest answer is matches=false with a reason naming the absence.
    let mut c = McpClient::spawn();
    c.initialize();

    let resp = c.request(
        "tools/call",
        json!({"name":"ggen.lsp.replay_case","arguments":{"root":"."}}),
    );
    let doc = tool_doc(&resp);
    // Real PromotionReplay fields (crates/ggen-lsp/src/intel/replay.rs).
    assert_eq!(
        doc["matches"],
        json!(false),
        "no promoted-route artifact => no match"
    );
    let reason = doc["reason"]
        .as_str()
        .expect("PromotionReplay carries a reason string");
    assert!(
        reason.contains("no promoted-route artifact"),
        "reason names the missing artifact honestly, got: {reason}"
    );
}

#[test]
fn mcp_metrics_returns_improve_metrics_refusing_absent_evidence() {
    // metrics_result -> ggen_lsp::compute_metrics. With no OCEL log / promotion
    // history the IMPROVE-1 contract REFUSES every metric (insufficient_evidence)
    // and the verdict is earned-not-assumed => insufficient_evidence, cycles=0.
    let mut c = McpClient::spawn();
    c.initialize();

    let resp = c.request(
        "tools/call",
        json!({"name":"ggen.lsp.metrics","arguments":{"root":"."}}),
    );
    let doc = tool_doc(&resp);
    // Real ImproveMetrics fields (crates/ggen-lsp/src/intel/metrics.rs).
    assert_eq!(
        doc["verdict"],
        json!("insufficient_evidence"),
        "verdict is refused by default"
    );
    assert_eq!(
        doc["cycles"],
        json!(0),
        "no promotion history => zero cycles"
    );
    // MetricValue::InsufficientEvidence serializes to the string "insufficient_evidence".
    for metric in [
        "route_hit_rate",
        "promoted_route_hit_rate",
        "seed_displacement_rate",
        "repair_cycle_time_secs",
        "gate_pass_rate_seed",
        "gate_pass_rate_mined",
        "repeat_failure_rate",
        "promotion_survival_rate",
        "promotion_churn",
        "receipt_density",
    ] {
        assert_eq!(
            doc[metric],
            json!("insufficient_evidence"),
            "metric {metric} must refuse (no backing events), got: {}",
            doc[metric]
        );
    }
}

#[test]
fn mcp_every_tool_advertises_a_non_empty_typed_input_schema() {
    // An MCP client (Claude Code) only knows how to *call* a tool from its
    // inputSchema. Each of the three tools must advertise an object schema with a
    // `properties` map — i.e. the schemars-derived params are actually surfaced.
    let mut c = McpClient::spawn();
    c.initialize();

    let tools = c.request("tools/list", Value::Null);
    let arr = tools["result"]["tools"].as_array().expect("tools array");
    assert_eq!(arr.len(), 3, "exactly three tools");
    for t in arr {
        let name = t["name"].as_str().expect("tool name");
        let schema = &t["inputSchema"];
        assert!(
            schema.is_object(),
            "{name} inputSchema must be a JSON object, got: {schema}"
        );
        assert_eq!(
            schema["type"],
            json!("object"),
            "{name} inputSchema describes an object"
        );
        let props = schema["properties"]
            .as_object()
            .unwrap_or_else(|| panic!("{name} inputSchema must carry a properties map: {schema}"));
        assert!(
            !props.is_empty(),
            "{name} inputSchema.properties must be non-empty"
        );
    }
}

#[test]
fn mcp_initialize_echoes_a_protocol_version_string() {
    // The handshake must agree a protocol version; the server pins V_2024_11_05.
    let mut c = McpClient::spawn();
    let init = c.initialize();
    let pv = init["result"]["protocolVersion"]
        .as_str()
        .unwrap_or_else(|| panic!("initialize result must echo a protocolVersion string: {init}"));
    assert!(
        !pv.is_empty(),
        "protocolVersion is a non-empty string, got: {pv:?}"
    );
}

#[test]
fn mcp_malformed_args_are_refused_and_the_server_survives() {
    // Garbled arguments (wrong types) for replay_case and metrics must produce a
    // STRUCTURED refusal — a JSON-RPC error or an isError tool result — never a
    // crash. After each, tools/list must still answer: the server stayed alive.
    let mut c = McpClient::spawn();
    c.initialize();

    let is_refusal = |resp: &Value| -> bool {
        resp.get("error").is_some()
            || resp["result"]["isError"] == json!(true)
            || resp["result"]["content"][0]["text"]
                .as_str()
                .map(|t| {
                    let t = t.to_lowercase();
                    t.contains("invalid") || t.contains("error") || t.contains("param")
                })
                .unwrap_or(false)
    };

    // `root` must be a string; an integer makes serde_json::from_value fail =>
    // McpError::invalid_params (a structured JSON-RPC error, not a panic).
    let bad_replay = c.request(
        "tools/call",
        json!({"name":"ggen.lsp.replay_case","arguments":{"root":12345}}),
    );
    assert!(
        is_refusal(&bad_replay),
        "malformed replay_case args must be refused, got: {bad_replay}"
    );
    // Server survives.
    let after_replay = c.request("tools/list", Value::Null);
    assert!(
        after_replay["result"]["tools"].is_array(),
        "server survives bad replay_case args"
    );

    let bad_metrics = c.request(
        "tools/call",
        json!({"name":"ggen.lsp.metrics","arguments":{"root":["not","a","string"]}}),
    );
    assert!(
        is_refusal(&bad_metrics),
        "malformed metrics args must be refused, got: {bad_metrics}"
    );
    // Server survives a second time.
    let after_metrics = c.request("tools/list", Value::Null);
    assert!(
        after_metrics["result"]["tools"].is_array(),
        "server survives bad metrics args"
    );

    // SEMANTIC refusal (not just type-level): a well-TYPED but empty `root` string
    // parses fine yet is now refused by the same path-bound discipline repair_route
    // applies — closing the fail-open this test previously could not reach. The
    // server must survive these too.
    let semantic_replay = c.request(
        "tools/call",
        json!({"name":"ggen.lsp.replay_case","arguments":{"root":""}}),
    );
    assert!(
        is_refusal(&semantic_replay),
        "an empty (but well-typed) replay_case root must be SEMANTICALLY refused, got: {semantic_replay}"
    );
    let semantic_metrics = c.request(
        "tools/call",
        json!({"name":"ggen.lsp.metrics","arguments":{"root":""}}),
    );
    assert!(
        is_refusal(&semantic_metrics),
        "an empty (but well-typed) metrics root must be SEMANTICALLY refused, got: {semantic_metrics}"
    );
    let after_semantic = c.request("tools/list", Value::Null);
    assert!(
        after_semantic["result"]["tools"].is_array(),
        "server survives semantic refusals"
    );
}

#[test]
fn mcp_replay_case_and_metrics_refuse_empty_and_oversized_root() {
    // SABOTAGE: an UNTRUSTED MCP client probes the read-path surface by passing an
    // empty or oversized `root` to replay_case/metrics. Before MCP-REPLAY hardening
    // these tools performed NO semantic validation of `root` (any string accepted,
    // defaulting to ".") — an asymmetric fail-open vs. repair_route, which bounds
    // its path inputs. The fix mirrors repair_route's discipline (empty / >4096
    // bytes => McpError::invalid_params), so each refusal below must be STRUCTURED
    // (JSON-RPC error or isError), never silent acceptance, and the server survives.
    let mut c = McpClient::spawn();
    c.initialize();

    let is_refusal = |resp: &Value| -> bool {
        resp.get("error").is_some()
            || resp["result"]["isError"] == json!(true)
            || resp["result"]["content"][0]["text"]
                .as_str()
                .map(|t| {
                    let t = t.to_lowercase();
                    t.contains("invalid") || t.contains("empty") || t.contains("too long")
                })
                .unwrap_or(false)
    };
    // 4097 bytes — one over MAX_PATH_BYTES (the same bound repair_route applies to
    // file_path). repair_route refuses an over-length path; replay_case/metrics must too.
    let oversized_root = "a".repeat(4097);

    // --- replay_case: empty root ---
    let r = c.request(
        "tools/call",
        json!({"name":"ggen.lsp.replay_case","arguments":{"root":""}}),
    );
    assert!(
        is_refusal(&r),
        "replay_case must SEMANTICALLY refuse an empty root, got: {r}"
    );
    // --- replay_case: oversized root ---
    let r = c.request(
        "tools/call",
        json!({"name":"ggen.lsp.replay_case","arguments":{"root":oversized_root}}),
    );
    assert!(
        is_refusal(&r),
        "replay_case must refuse an oversized root, got: {r}"
    );
    // --- replay_case: empty case_id (present-but-empty) ---
    let r = c.request(
        "tools/call",
        json!({"name":"ggen.lsp.replay_case","arguments":{"root":".","case_id":""}}),
    );
    assert!(
        is_refusal(&r),
        "replay_case must refuse an empty case_id, got: {r}"
    );

    // --- metrics: empty root ---
    let m = c.request(
        "tools/call",
        json!({"name":"ggen.lsp.metrics","arguments":{"root":""}}),
    );
    assert!(
        is_refusal(&m),
        "metrics must SEMANTICALLY refuse an empty root, got: {m}"
    );
    // --- metrics: oversized root ---
    let oversized_root2 = "b".repeat(4097);
    let m = c.request(
        "tools/call",
        json!({"name":"ggen.lsp.metrics","arguments":{"root":oversized_root2}}),
    );
    assert!(
        is_refusal(&m),
        "metrics must refuse an oversized root, got: {m}"
    );

    // Server survived every semantic refusal: tools/list still answers.
    let after = c.request("tools/list", Value::Null);
    assert!(
        after["result"]["tools"].is_array(),
        "server survives semantic refusals on replay_case/metrics"
    );
}

#[test]
fn mcp_replay_case_and_metrics_accept_valid_root_after_hardening() {
    // REGRESSION GUARD: the semantic-validation hardening must NOT break the happy
    // path. A valid root (".") and a valid case_id still produce the real structured
    // result documents (not a refusal) — proving validation only rejects bad input.
    let mut c = McpClient::spawn();
    c.initialize();

    // Valid replay_case: real CaseReplay (found=false in a fresh project, but a
    // genuine result document — NOT an error).
    let replay = c.request(
        "tools/call",
        json!({"name":"ggen.lsp.replay_case","arguments":{"root":".","case_id":"c:valid-shape"}}),
    );
    let doc = tool_doc(&replay);
    assert_eq!(
        doc["case_id"],
        json!("c:valid-shape"),
        "valid call echoes the case_id"
    );
    assert_eq!(
        doc["found"],
        json!(false),
        "fresh project => not found, but a real CaseReplay"
    );

    // Valid metrics: real ImproveMetrics document, not a refusal.
    let metrics = c.request(
        "tools/call",
        json!({"name":"ggen.lsp.metrics","arguments":{"root":"."}}),
    );
    let mdoc = tool_doc(&metrics);
    assert_eq!(
        mdoc["verdict"],
        json!("insufficient_evidence"),
        "valid metrics call returns the real refused-by-default verdict, not an arg error"
    );
}

#[test]
fn mcp_repair_route_on_clean_law_surface_reports_no_diagnostics() {
    // The honest "all clear" shape: a clean SPARQL SELECT IS a law surface
    // (is_law_surface=true) but raises no diagnostics, so envelopes is empty.
    // An agent must be able to tell "law surface, nothing to fix" apart from
    // "not a law surface" — both are proven here against the REAL build output.
    let mut c = McpClient::spawn();
    c.initialize();

    let resp = c.request(
        "tools/call",
        json!({
            "name":"ggen.lsp.repair_route",
            "arguments":{"file_path":"clean.rq","file_content":CLEAN_SELECT}
        }),
    );
    let doc = tool_doc(&resp);
    assert_eq!(doc["is_law_surface"], json!(true), "a .rq is a law surface");
    assert_eq!(
        doc["envelopes"].as_array().map(Vec::len),
        Some(0),
        "a clean SELECT raises no routed diagnostics, got: {}",
        doc["envelopes"]
    );
    assert_eq!(
        doc["refusals"].as_array().map(Vec::len),
        Some(0),
        "no diagnostics => no refusals either, got: {}",
        doc["refusals"]
    );
}
