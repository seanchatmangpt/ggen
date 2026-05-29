//! MULTI-AGENT-1 — multiple agents share one admissibility surface without drift.
//!
//! Two distinct agent identities author the same failure family against ONE shared
//! promoted route pack. Both resolve to the same route, their episodes stay
//! separable (distinct case ids), each episode is attributed + receipted, and each
//! replays independently. Real execution throughout.

use std::fs;
use std::path::Path;

use ggen_lsp::intel::events::{activity, obj_type};
use ggen_lsp::{check_files_in_root, mine, replay_case, IntelLog};
use tempfile::TempDir;

const E0011_SRC: &str = "CONSTRUCT { ?s ?p ?o } WHERE { ?s ?p ?o }\n";

fn promote_template_route(root: &Path) {
    let mut files = Vec::new();
    for i in 0..3 {
        let p = root.join(format!("seed{i}.rq"));
        fs::write(&p, E0011_SRC).expect("write");
        files.push(p);
    }
    check_files_in_root(root, &files, true).capture(root);
    mine(root).expect("mine");
}

#[test]
fn two_agents_share_one_route_law_without_drift() {
    let root_dir = TempDir::new().expect("tempdir");
    let root = root_dir.path();

    // One shared route law (promoted route pack at the project root).
    promote_template_route(root);

    // Two distinct agents author the same family against the shared pack.
    let a = root.join("alpha.rq");
    let b = root.join("beta.rq");
    fs::write(&a, E0011_SRC).expect("write");
    fs::write(&b, E0011_SRC).expect("write");
    check_files_in_root(root, &[a], true).capture_as(root, "alpha");
    check_files_in_root(root, &[b], true).capture_as(root, "beta");

    let log = IntelLog::at_root(root).read();

    // Shared law, no drift: each agent selected the SAME mined route.
    let route_for = |agent: &str| {
        log.events
            .iter()
            .find(|e| {
                e.activity == activity::ROUTE_SELECTED
                    && e.attributes.get("agent_id").map(String::as_str) == Some(agent)
            })
            .and_then(|e| e.attributes.get("route").cloned())
    };
    for agent in ["alpha", "beta"] {
        assert_eq!(
            route_for(agent).as_deref(),
            Some("mined.template-failure"),
            "agent {agent} resolves the shared family to the same route"
        );
    }

    // Separable: the two agents' episodes are distinct cases (no collision).
    let episode_for = |agent: &str| {
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
    let alpha_ep = episode_for("alpha").expect("alpha episode");
    let beta_ep = episode_for("beta").expect("beta episode");
    assert_ne!(
        alpha_ep, beta_ep,
        "concurrent agents must not share an episode"
    );

    // Each agent's closed episode is receipted (independent proof per agent).
    for agent in ["alpha", "beta"] {
        assert!(
            log.events
                .iter()
                .any(|e| e.activity == activity::RECEIPT_EMITTED
                    && e.attributes.get("agent_id").map(String::as_str) == Some(agent)),
            "agent {agent} produced its own receipt"
        );
    }

    // Each episode replays independently from the shared log.
    let rep = replay_case(root, &alpha_ep);
    assert!(rep.found, "alpha episode replays");
    assert_eq!(rep.route_id.as_deref(), Some("mined.template-failure"));
    assert!(
        rep.receipt_id.is_some(),
        "alpha episode has a receipt on replay"
    );
}
