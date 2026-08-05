//! CP39 real end-to-end proof: the sync-refusal push path genuinely calls
//! `route_signal` (with the real extracted FM-* code) and, when a project
//! declares `rf:dispatchRoute "bounded-unattended"` for that code, genuinely
//! invokes `try_unattended_apply` -- not a mock, the real function.
//!
//! **A real, honest limitation, not a gap in this test**: a project-wide
//! `sync()` refusal (the `Err` branch this wiring lives in) structurally
//! means CP33's own whole-project-clean precondition can never hold at the
//! same moment -- `try_unattended_apply` runs its OWN fresh dry-run, which
//! observes the exact same refusal, so a genuine `Applied` outcome from
//! THIS specific call site is near-impossible to construct honestly (it
//! would require something else fixing the refusal in the narrow window
//! between the two dry-runs). CP33/34's own tests already prove
//! `try_unattended_apply` applies for real when its conditions genuinely
//! hold; this test proves CP39's wiring genuinely REACHES and INVOKES that
//! function with the real code, observable via the real, shared
//! `.ggen/unattended-dispatch-log.jsonl` audit log CP33 already writes to
//! every attempt, not just successes.

use std::io::{BufRead, BufReader, Write};
use std::process::{Child, ChildStdin, Command, Stdio};
use std::sync::mpsc::{self, Receiver};
use std::thread;
use std::time::Duration;

use serde_json::{json, Value};
use tempfile::TempDir;

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
                   "clientInfo":{"name":"ggen-mcp-signal-dispatch-e2e-test","version":"1.0"}}),
        );
        self.send(&json!({"jsonrpc":"2.0","method":"notifications/initialized"}));
        resp
    }

    fn wait_for_sync_refusal_update(&mut self) -> Value {
        loop {
            let frame = self
                .rx
                .recv_timeout(READ_TIMEOUT)
                .expect("timed out waiting for a ggen-sync-refusal:// resources/updated push");
            if frame.get("id").is_none()
                && frame.get("method").and_then(Value::as_str)
                    == Some("notifications/resources/updated")
                && frame["params"]["uri"]
                    .as_str()
                    .is_some_and(|u| u.starts_with("ggen-sync-refusal://"))
            {
                return frame;
            }
        }
    }
}

impl Drop for McpClient {
    fn drop(&mut self) {
        let _ = self.child.kill();
        let _ = self.child.wait();
    }
}

/// A real project whose `[templates].dir` references a directory that does
/// not exist -- a genuine sync refusal (`discover_templates` fails closed on
/// an unreadable templates dir) -- with NO `.specify/repo-facts.ttl` at all.
/// The real, honest default-case proof: absent any declared fact, CP39 must
/// never invoke the dispatcher, verified by the audit log's absence.
#[test]
fn absent_route_declaration_never_invokes_the_dispatcher() {
    let tmp = TempDir::new().expect("tempdir");
    std::fs::write(
        tmp.path().join("ggen.toml"),
        "[project]\nname = \"signal-dispatch-e2e\"\n\
         [ontology]\nsource = \"model.ttl\"\n\
         [templates]\ndir = \"templates-does-not-exist\"\n",
    )
    .expect("write ggen.toml");
    std::fs::write(
        tmp.path().join("model.ttl"),
        "@prefix ex: <http://example.org/> .\nex:owner a ex:Person .\n",
    )
    .expect("write ontology");
    // Deliberately no `templates-does-not-exist/` directory.

    let mut c = McpClient::spawn(tmp);
    let init = c.initialize();
    assert_eq!(init["result"]["serverInfo"]["name"], "ggen-mcp");
    thread::sleep(Duration::from_millis(300));

    // Trigger the sync watcher: touch a file so the coarse debouncer fires.
    std::fs::write(
        c.tmp.path().join("model.ttl"),
        "@prefix ex: <http://example.org/> .\nex:owner a ex:Person .\nex:touch a ex:NoOp .\n",
    )
    .expect("touch to trigger watcher");

    let notification = c.wait_for_sync_refusal_update();
    let uri = notification["params"]["uri"]
        .as_str()
        .expect("notification carries a uri")
        .to_string();
    assert!(uri.starts_with("ggen-sync-refusal://"));

    // Give the CP39 dispatch attempt (spawned inside the same async task,
    // after the push above) a moment to run and write its audit log entry.
    thread::sleep(Duration::from_millis(500));

    // CP39's wiring only ACTS when a declared route exists -- this fixture
    // never wrote a `.specify/repo-facts.ttl`, so route_signal must have
    // resolved to Attended, and no dispatch attempt (hence no audit log)
    // should exist. This is the real, honest default-case proof: absent a
    // declared fact, nothing new happens beyond the existing push.
    let audit_log = c.tmp.path().join(".ggen/unattended-dispatch-log.jsonl");
    assert!(
        !audit_log.exists(),
        "with no declared rf:dispatchRoute fact, CP39 must never invoke the \
         dispatcher at all -- fail closed to attended, no audit log entry"
    );
}

/// The positive-wiring proof: a real refusal fires, the project's OWN
/// `.specify/repo-facts.ttl` declares `rf:dispatchRoute "bounded-unattended"`
/// for the EXACT FM code that refusal carries (read back from the pushed
/// resource's own content, not guessed), a fresh change re-fires the
/// watcher, and CP39's wiring genuinely reaches and invokes
/// `try_unattended_apply` -- observable via the real, shared
/// `.ggen/unattended-dispatch-log.jsonl` audit log CP33 already writes on
/// every attempt. Per this test file's own module doc: a genuine `Applied`
/// outcome is structurally near-impossible here (the refusal that triggers
/// CP39's check is, by definition, the same one `try_unattended_apply`'s own
/// fresh dry-run would also observe), so this asserts a real `not_eligible`
/// log entry exists -- proof the dispatcher was actually invoked with the
/// real code, not proof it applied.
#[test]
fn declared_bounded_unattended_route_reaches_a_real_dispatch_attempt() {
    let tmp = TempDir::new().expect("tempdir");
    std::fs::write(
        tmp.path().join("ggen.toml"),
        "[project]\nname = \"signal-dispatch-e2e-2\"\n\
         [ontology]\nsource = \"model.ttl\"\n\
         [templates]\ndir = \"templates-does-not-exist\"\n",
    )
    .expect("write ggen.toml");
    std::fs::write(
        tmp.path().join("model.ttl"),
        "@prefix ex: <http://example.org/> .\nex:owner a ex:Person .\n",
    )
    .expect("write ontology");

    let mut c = McpClient::spawn(tmp);
    let init = c.initialize();
    assert_eq!(init["result"]["serverInfo"]["name"], "ggen-mcp");
    thread::sleep(Duration::from_millis(300));

    // Phase 1: trigger the real refusal once, to learn its real FM code.
    std::fs::write(
        c.tmp.path().join("model.ttl"),
        "@prefix ex: <http://example.org/> .\nex:owner a ex:Person .\nex:touch a ex:NoOp .\n",
    )
    .expect("touch to trigger watcher");
    let first = c.wait_for_sync_refusal_update();
    let uri = first["params"]["uri"]
        .as_str()
        .expect("notification carries a uri")
        .to_string();
    let read = c.request("resources/read", json!({"uri": uri}));
    let text = read["result"]["contents"][0]["text"]
        .as_str()
        .expect("resources/read returns text")
        .to_string();
    let fm_code = text
        .split_once('[')
        .and_then(|(_, rest)| rest.split_once(']'))
        .map(|(code, _)| code.to_string())
        .unwrap_or_else(|| panic!("no bracketed FM code found in refusal text: {text}"));
    assert!(
        fm_code.starts_with("FM-"),
        "unexpected code shape: {fm_code}"
    );

    // Phase 2: declare the route for that exact code, in the project's own
    // facts file -- a real author reacting to an observed refusal.
    std::fs::create_dir_all(c.tmp.path().join(".specify")).expect("mkdir .specify");
    std::fs::write(
        c.tmp.path().join(".specify/repo-facts.ttl"),
        format!(
            "@prefix rf: <http://ggen.org/repo-facts#> .\n\
             rf:diag_e2e a rf:DiagnosticCode ;\n    \
             rf:code \"**{fm_code}**\" ;\n    \
             rf:dispatchRoute \"bounded-unattended\" .\n"
        ),
    )
    .expect("write repo-facts.ttl");

    // Phase 3: re-fire the watcher (the underlying refusal is unchanged, but
    // the route is now declared).
    std::fs::write(
        c.tmp.path().join("model.ttl"),
        "@prefix ex: <http://example.org/> .\nex:owner a ex:Person .\nex:touch2 a ex:NoOp .\n",
    )
    .expect("second touch to re-trigger watcher");
    c.wait_for_sync_refusal_update();

    // Give the spawned CP39 dispatch attempt time to run and write its
    // audit log entry.
    thread::sleep(Duration::from_millis(800));

    let audit_log = c.tmp.path().join(".ggen/unattended-dispatch-log.jsonl");
    assert!(
        audit_log.exists(),
        "a declared bounded-unattended route must reach a real dispatch \
         attempt, evidenced by the audit log CP33 writes on every attempt"
    );
    let log_content = std::fs::read_to_string(&audit_log).expect("read audit log");
    assert!(
        log_content.contains("\"outcome\":\"not_eligible\""),
        "expected a real not_eligible attempt (the active project-wide refusal \
         makes a genuine Applied outcome structurally impossible here), got: {log_content}"
    );
}
