//! CP28 real end-to-end proof: a real, running `ggen-mcp` binary, a real
//! cross-pack SPARQL-gate violation introduced by a real filesystem write
//! into a *composed pack* (not the project's own ontology), and a real MCP
//! client observing the resulting `notifications/resources/updated` push
//! for a `ggen-sync-refusal://` resource.
//!
//! Mirrors `watcher_e2e_test.rs`'s real subprocess/stdio harness exactly --
//! real binary, real newline-delimited JSON-RPC, no mocks, no unit-level
//! call to `push_sync_refusal_for_root` directly.

use std::io::{BufRead, BufReader, Write};
use std::process::{Child, ChildStdin, Command, Stdio};
use std::sync::mpsc::{self, Receiver};
use std::thread;
use std::time::Duration;

use serde_json::{json, Value};
use tempfile::TempDir;

/// Generous: the sync watcher's own debounce window is 2s (coarser than the
/// lint gate's 500ms -- see `watcher.rs::SYNC_DEBOUNCE_WINDOW`), plus real
/// process startup/dry-run time (two full `GraphEngine` rebuilds).
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
                   "clientInfo":{"name":"ggen-mcp-sync-refusal-e2e-test","version":"1.0"}}),
        );
        self.send(&json!({"jsonrpc":"2.0","method":"notifications/initialized"}));
        resp
    }

    /// Block until a real `notifications/resources/updated` frame whose
    /// `uri` starts with `ggen-sync-refusal://` arrives, or time out.
    /// Ignores every other frame in between (e.g. late responses, or the
    /// lint watcher's own `ggen-diagnostic://` pushes, which this fixture
    /// does not intentionally trigger but does not rule out either).
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

/// Write a minimal, valid local pack at `pack_dir`: `pack.toml` +
/// `ontology.ttl` (a single triple, distinct per pack via `subject`) +
/// exactly one `templates/*.tmpl` file (packs refuse with zero templates --
/// `[FM-PACK-005]` -- so this is required even though this fixture's
/// project never actually resolves a pack-sourced query/template).
fn write_pack(pack_dir: &std::path::Path, name: &str, subject: &str) {
    std::fs::create_dir_all(pack_dir.join("templates")).expect("mkdir pack templates");
    std::fs::write(
        pack_dir.join("pack.toml"),
        format!(
            "[pack]\nname = \"{name}\"\nversion = \"0.1.0\"\ndescription = \"CP28 e2e fixture pack\"\n"
        ),
    )
    .expect("write pack.toml");
    std::fs::write(
        pack_dir.join("ontology.ttl"),
        format!("@prefix ex: <http://example.org/> .\nex:{subject} a ex:Dog .\n"),
    )
    .expect("write pack ontology.ttl");
    std::fs::write(
        pack_dir.join("templates/unused.tmpl"),
        "---\nto: unused/{{name}}.txt\nsparql:\n  rows: SELECT ?s WHERE { ?s a <http://example.org/Dog> }\n---\nunused\n",
    )
    .expect("write pack template");
}

/// The real end-to-end path CP28 exists to prove: a project composing two
/// real local packs (frontmatter `[packs]` schema), started clean (no
/// license-violation dog yet), then a real filesystem write introduces a
/// genuine cross-pack SPARQL-gate violation into ONE of the composed
/// packs -- not the project's own ontology -- and the server's own
/// background sync-refusal watcher notices the change, re-runs a real
/// dry-run `sync()`, gets a real `Err` carrying `FM-PACK-013` (the pack
/// gate `evaluate_gate` violation code -- see `ggen-engine/src/sync.rs`),
/// and pushes a real `notifications/resources/updated` for a
/// `ggen-sync-refusal://` resource over the wire. The notified URI resolves
/// to the real refusal content (containing the FM-* code) via a real
/// `resources/read` follow-up. No test code calls
/// `push_sync_refusal_for_root` directly.
#[test]
fn cross_pack_gate_violation_triggers_a_real_sync_refusal_push() {
    let tmp = TempDir::new().expect("tempdir");

    // Two composed packs: `alpha` ships the gate that will fire once
    // `beta`'s ontology introduces an unlicensed Dog; `beta` starts clean
    // (a Cat, not a Dog, so the gate is quiet at server start).
    write_pack(&tmp.path().join("packs/alpha"), "alpha", "placeholder");
    std::fs::create_dir_all(tmp.path().join("packs/alpha/gates")).expect("mkdir gates");
    std::fs::write(
        tmp.path().join("packs/alpha/gates/licensed-dog.rq"),
        "# MESSAGE: every Dog must carry an ex:license\n\
         PREFIX ex: <http://example.org/>\n\
         SELECT ?dog WHERE {\n\
         \x20 ?dog a ex:Dog .\n\
         \x20 FILTER NOT EXISTS { ?dog ex:license ?lic }\n\
         }\n\
         ORDER BY ?dog\n",
    )
    .expect("write pack gate");

    write_pack(&tmp.path().join("packs/beta"), "beta", "rex_placeholder");
    // Overwrite beta's ontology with a clean starting fact (no Dog yet) so
    // the initial dry run at server start is quiet.
    std::fs::write(
        tmp.path().join("packs/beta/ontology.ttl"),
        "@prefix ex: <http://example.org/> .\nex:whiskers a ex:Cat .\n",
    )
    .expect("write beta ontology.ttl (clean)");

    // Project ontology + manifest: frontmatter schema, composing both packs
    // via `[packs]` (table-of-tables, `PackRef::Path`).
    std::fs::write(
        tmp.path().join("model.ttl"),
        "@prefix ex: <http://example.org/> .\nex:owner a ex:Person .\n",
    )
    .expect("write project ontology");
    std::fs::create_dir_all(tmp.path().join("templates")).expect("mkdir templates");
    std::fs::write(
        tmp.path().join("templates/owner.tmpl"),
        "---\nto: out/owner.txt\nsparql:\n  people: SELECT ?p WHERE { ?p a <http://example.org/Person> }\n---\nowner\n",
    )
    .expect("write project template");
    std::fs::write(
        tmp.path().join("ggen.toml"),
        r#"
[project]
name = "sync-refusal-e2e"

[ontology]
source = "model.ttl"

[packs]
alpha = { path = "packs/alpha" }
beta = { path = "packs/beta" }

[templates]
dir = "templates"
"#,
    )
    .expect("write ggen.toml");

    let mut c = McpClient::spawn(tmp);
    let init = c.initialize();
    assert_eq!(init["result"]["serverInfo"]["name"], "ggen-mcp");

    // Give both watchers time to finish real OS-level watch registration
    // (synchronous inside `start_stdio`, before `initialize` can even be
    // answered, but the syscalls are worth a grace period).
    thread::sleep(Duration::from_millis(300));

    // The real trigger: write an unlicensed Dog into `beta`'s ontology --
    // a genuine cross-pack gate violation (the gate lives in `alpha`, the
    // violating fact lands in `beta`; `sync()`'s pack gates run against the
    // UNION graph, so this is a real cross-pack refusal, not a same-pack
    // one).
    std::fs::write(
        c.tmp.path().join("packs/beta/ontology.ttl"),
        "@prefix ex: <http://example.org/> .\nex:whiskers a ex:Cat .\nex:rex a ex:Dog .\n",
    )
    .expect("write cross-pack violation");

    let notification = c.wait_for_sync_refusal_update();
    let uri = notification["params"]["uri"]
        .as_str()
        .expect("notification carries a uri")
        .to_string();
    assert!(
        uri.starts_with("ggen-sync-refusal://"),
        "unexpected notified uri: {uri}"
    );

    // Real `resources/read` follow-up: the notified URI must resolve to the
    // actual refusal content, carrying the real FM-PACK-013 code, not just
    // an empty ack.
    let read = c.read_resource(&uri);
    let text = read["result"]["contents"][0]["text"]
        .as_str()
        .unwrap_or_else(|| panic!("resources/read returned no text: {read}"));
    assert!(
        text.contains("FM-PACK-013"),
        "resource content missing the real FM-PACK-013 gate-violation code: {text}"
    );
    assert!(
        text.contains("every Dog must carry an ex:license"),
        "resource content missing the gate's own MESSAGE text: {text}"
    );
}
