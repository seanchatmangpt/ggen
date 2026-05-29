//! SESSION-ATTRIBUTION-1 — agent identity across LSP / MCP / A2A.
//!
//! The same failure family, captured over three transports with distinct agents
//! and sessions, yields separable episodes that each carry agent_id + transport +
//! session_id and each replay independently. Attribution is transport-safe.

use std::fs;

use ggen_lsp::intel::events::obj_type;
use ggen_lsp::{check_files_in_root, replay_case, Attribution, IntelLog};
use tempfile::TempDir;

const E0011_SRC: &str = "CONSTRUCT { ?s ?p ?o } WHERE { ?s ?p ?o }\n";

#[test]
fn three_transports_are_attributed_and_separable() {
    let dir = TempDir::new().expect("tempdir");
    let root = dir.path();
    let rq = root.join("q.rq");
    fs::write(&rq, E0011_SRC).expect("write");

    // Same family, three transports — distinct agents + sessions.
    let cases = [("alpha", "lsp", "s1"), ("beta", "mcp", "s2"), ("gamma", "a2a", "s3")];
    for (agent, transport, session) in cases {
        check_files_in_root(root, &[rq.clone()], true)
            .capture_attributed(root, &Attribution::new(agent, transport, session));
    }

    let log = IntelLog::at_root(root).read();

    // Each capture is fully attributed (agent + transport + session).
    for (agent, transport, session) in cases {
        let ev = log
            .events
            .iter()
            .find(|e| e.attributes.get("agent_id").map(String::as_str) == Some(agent))
            .unwrap_or_else(|| panic!("events for agent {agent}"));
        assert_eq!(ev.attributes.get("transport").map(String::as_str), Some(transport));
        assert_eq!(ev.attributes.get("session_id").map(String::as_str), Some(session));
    }

    // Separable: each transport's episode is a distinct case (no collision).
    let episode_for = |agent: &str| -> Option<String> {
        log.events
            .iter()
            .find(|e| e.attributes.get("agent_id").map(String::as_str) == Some(agent))
            .and_then(|e| {
                e.objects
                    .iter()
                    .find(|o| o.r#type == obj_type::EPISODE)
                    .map(|o| o.id.clone())
            })
    };
    let a = episode_for("alpha").expect("alpha episode");
    let b = episode_for("beta").expect("beta episode");
    let g = episode_for("gamma").expect("gamma episode");
    assert_ne!(a, b);
    assert_ne!(b, g);
    assert_ne!(a, g);

    // Each episode replays independently from the shared log.
    assert!(replay_case(root, &a).found, "alpha episode replays");
    assert!(replay_case(root, &b).found, "beta episode replays");
    assert!(replay_case(root, &g).found, "gamma episode replays");
}
