//! Agent-edit OCEL event schema + builders.
//!
//! Reuses ggen-graph's `OCELEvent`/`OCELObject`/`OCELObjectRef` verbatim — the
//! LSP invents no new event log. Each builder produces one OCEL event in the
//! agent-edit lifecycle so the offline miner can discover failure edges.

use std::collections::HashMap;

use chrono::Utc;
use ggen_graph::ocel::{OCELEvent, OCELObjectRef};

/// Activity names (== OCEL `activity` strings; not an enum so they compare
/// string-equal in SPARQL/mining).
pub mod activity {
    /// A diagnostic was surfaced for a law-surface file.
    pub const DIAGNOSTIC_RAISED: &str = "DiagnosticRaised";
    /// A repair route was chosen for a diagnostic (carries route_id + source).
    pub const ROUTE_SELECTED: &str = "RouteSelected";
    /// A repair route was offered to the agent for a diagnostic.
    pub const REPAIR_SUGGESTED: &str = "RepairSuggested";
    /// An edit implementing a route was applied (editor-only: did_change).
    pub const REPAIR_APPLIED: &str = "RepairApplied";
    /// An edit was applied to a file.
    pub const EDIT_APPLIED: &str = "EditApplied";
    /// The headless gate re-ran over a file.
    pub const VALIDATION_RERUN: &str = "ValidationRerun";
    /// The gate passed (no errors) for a file/diagnostic.
    pub const GATE_PASSED: &str = "GatePassed";
    /// The gate failed (errors remain).
    pub const GATE_FAILED: &str = "GateFailed";
    /// A receipt was emitted.
    pub const RECEIPT_EMITTED: &str = "ReceiptEmitted";
    /// An edit/commit was refused.
    pub const REFUSAL_EMITTED: &str = "RefusalEmitted";
}

/// Object types referenced by agent-edit events.
pub mod obj_type {
    /// The acting agent/harness.
    pub const AGENT: &str = "agent";
    /// A law-surface file.
    pub const FILE: &str = "file";
    /// A diagnostic code (E00XX) — used for per-family grouping in mining.
    pub const DIAGNOSTIC_CODE: &str = "diagnostic_code";
    /// One diagnostic's repair lifecycle within ONE check run — the DFG/case
    /// object. Distinct per (file, code, run) so separate runs never collapse.
    pub const EPISODE: &str = "episode";
    /// A repair route id.
    pub const REPAIR_ROUTE: &str = "repair_route";
    /// A receipt.
    pub const RECEIPT: &str = "receipt";
}

/// Qualifier correlating events to their diagnostic code (family grouping).
pub const DIAG_QUALIFIER: &str = "diag";
/// Qualifier correlating events to their episode (the DFG/conformance case).
pub const EPISODE_QUALIFIER: &str = "episode";

/// Generate a unique check-run id.
///
/// Combines a process-global monotonic counter
/// with the wall clock so distinct `ggen lsp check` invocations (and distinct
/// in-process capture calls) never share an episode case. Not a workflow script,
/// so `SystemTime` is acceptable here.
#[must_use]
pub fn new_run_id() -> String {
    use std::sync::atomic::{AtomicU64, Ordering};
    use std::time::{SystemTime, UNIX_EPOCH};
    static COUNTER: AtomicU64 = AtomicU64::new(0);
    let n = COUNTER.fetch_add(1, Ordering::Relaxed);
    let nanos = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .map(|d| d.as_nanos())
        .unwrap_or(0);
    let mut h = blake3::Hasher::new();
    h.update(&n.to_le_bytes());
    h.update(&nanos.to_le_bytes());
    h.finalize().to_hex()[..16].to_string()
}

fn event_id(activity: &str, file: &str, run_id: &str, seq: u64) -> String {
    let mut h = blake3::Hasher::new();
    h.update(activity.as_bytes());
    h.update(file.as_bytes());
    h.update(run_id.as_bytes());
    h.update(&seq.to_le_bytes());
    h.finalize().to_hex()[..16].to_string()
}

/// Episode case id = `blake3(file ‖ code ‖ run_id)` — the composite that keeps
/// separate runs (and separate files/codes) in separate DFG cases.
fn episode_id(file: &str, code: &str, run_id: &str) -> String {
    let mut h = blake3::Hasher::new();
    h.update(file.as_bytes());
    h.update(code.as_bytes());
    h.update(run_id.as_bytes());
    h.finalize().to_hex()[..16].to_string()
}

fn diag_ref(code: &str) -> OCELObjectRef {
    OCELObjectRef {
        id: code.to_string(),
        r#type: obj_type::DIAGNOSTIC_CODE.to_string(),
        qualifier: Some(DIAG_QUALIFIER.to_string()),
    }
}

fn episode_ref(file: &str, code: &str, run_id: &str) -> OCELObjectRef {
    OCELObjectRef {
        id: episode_id(file, code, run_id),
        r#type: obj_type::EPISODE.to_string(),
        qualifier: Some(EPISODE_QUALIFIER.to_string()),
    }
}

fn file_ref(file: &str) -> OCELObjectRef {
    OCELObjectRef {
        id: file.to_string(),
        r#type: obj_type::FILE.to_string(),
        qualifier: Some("file".to_string()),
    }
}

/// The standard object set for an episode event: file + diagnostic_code + episode.
fn episode_objects(file: &str, code: &str, run_id: &str) -> Vec<OCELObjectRef> {
    vec![
        file_ref(file),
        diag_ref(code),
        episode_ref(file, code, run_id),
    ]
}

fn route_ref(route_id: &str) -> OCELObjectRef {
    OCELObjectRef {
        id: route_id.to_string(),
        r#type: obj_type::REPAIR_ROUTE.to_string(),
        qualifier: Some("route".to_string()),
    }
}

/// The acting agent/harness object.
///
/// Attaching it to every event in a capture run
/// makes episode attribution explicit (which agent authored which episode) while
/// episode identity (file|code|run_id) keeps concurrent agents' traces separable.
#[must_use]
pub fn agent_ref(agent_id: &str) -> OCELObjectRef {
    OCELObjectRef {
        id: agent_id.to_string(),
        r#type: obj_type::AGENT.to_string(),
        qualifier: Some("agent".to_string()),
    }
}

/// Who authored a capture run, over which transport, in which session.
///
/// Attached to
/// every event so route success can be sliced by transport/agent and replayed with
/// its provenance intact — the same route law, attribution-safe across LSP, MCP,
/// and A2A.
#[derive(Debug, Clone)]
pub struct Attribution {
    /// Acting agent identity.
    pub agent_id: String,
    /// Transport the events arrived over: `lsp` | `mcp` | `a2a` | `headless`.
    pub transport: String,
    /// Session grouping for multi-step interactions.
    pub session_id: String,
}

impl Attribution {
    /// New attribution from explicit parts.
    #[must_use]
    pub fn new(
        agent_id: impl Into<String>, transport: impl Into<String>, session_id: impl Into<String>,
    ) -> Self {
        Self {
            agent_id: agent_id.into(),
            transport: transport.into(),
            session_id: session_id.into(),
        }
    }

    /// The default headless-gate attribution (`local` agent, fresh session).
    #[must_use]
    pub fn headless() -> Self {
        Self::new("local", "headless", new_run_id())
    }

    /// A named agent over the headless gate (a fresh session per capture).
    #[must_use]
    pub fn for_agent(agent_id: &str) -> Self {
        Self::new(agent_id, "headless", new_run_id())
    }

    /// The editor (LSP) transport, grouped under an explicit session.
    #[must_use]
    pub fn lsp(session_id: &str) -> Self {
        Self::new("editor", "lsp", session_id)
    }

    /// A non-editor request (MCP/A2A) by `agent_id` over `transport`, with a fresh
    /// session id. Used by the field-evidence gauge so MCP/A2A route requests leave
    /// attributed traces.
    #[must_use]
    pub fn request(agent_id: &str, transport: &str) -> Self {
        Self::new(agent_id, transport, new_run_id())
    }
}

/// Attach `attribution` (agent object + `agent_id`/`transport`/`session_id`
/// attributes) to every event in a capture batch.
pub fn attach_attribution(events: &mut [OCELEvent], attribution: &Attribution) {
    let agent = agent_ref(&attribution.agent_id);
    for ev in events.iter_mut() {
        ev.objects.push(agent.clone());
        ev.attributes
            .insert("agent_id".to_string(), attribution.agent_id.clone());
        ev.attributes
            .insert("transport".to_string(), attribution.transport.clone());
        ev.attributes
            .insert("session_id".to_string(), attribution.session_id.clone());
    }
}

/// Build a `DiagnosticRaised` event. `span` is the diagnostic range as
/// `"sl:sc-el:ec"` (a binding `repeat_failure_rate` keys on).
#[must_use]
pub fn diagnostic_raised(
    file: &str, code: &str, severity: &str, span: &str, run_id: &str, seq: u64,
) -> OCELEvent {
    let mut attributes = HashMap::new();
    attributes.insert("severity".to_string(), severity.to_string());
    attributes.insert("span".to_string(), span.to_string());
    OCELEvent {
        id: event_id(activity::DIAGNOSTIC_RAISED, file, run_id, seq),
        activity: activity::DIAGNOSTIC_RAISED.to_string(),
        timestamp: Utc::now(),
        objects: episode_objects(file, code, run_id),
        attributes,
    }
}

/// Build a `RouteSelected` event: which route (and its source) was chosen for
/// this diagnostic. Backing event for `route_hit_rate` / `seed_displacement_rate`.
#[must_use]
pub fn route_selected(
    file: &str, code: &str, route_id: &str, route_source: &str, run_id: &str, seq: u64,
) -> OCELEvent {
    let mut attributes = HashMap::new();
    attributes.insert("route".to_string(), route_id.to_string());
    attributes.insert("route_source".to_string(), route_source.to_string());
    let mut objects = episode_objects(file, code, run_id);
    objects.push(route_ref(route_id));
    OCELEvent {
        id: event_id(activity::ROUTE_SELECTED, file, run_id, seq),
        activity: activity::ROUTE_SELECTED.to_string(),
        timestamp: Utc::now(),
        objects,
        attributes,
    }
}

/// Build a `RepairSuggested` event linking a diagnostic to a route.
#[must_use]
pub fn repair_suggested(
    file: &str, code: &str, route_id: &str, run_id: &str, seq: u64,
) -> OCELEvent {
    let mut attributes = HashMap::new();
    attributes.insert("route".to_string(), route_id.to_string());
    let mut objects = episode_objects(file, code, run_id);
    objects.push(route_ref(route_id));
    OCELEvent {
        id: event_id(activity::REPAIR_SUGGESTED, file, run_id, seq),
        activity: activity::REPAIR_SUGGESTED.to_string(),
        timestamp: Utc::now(),
        objects,
        attributes,
    }
}

/// Build a `RepairApplied` event (editor-only: the agent applied an edit).
#[must_use]
pub fn repair_applied(file: &str, code: &str, route_id: &str, run_id: &str, seq: u64) -> OCELEvent {
    let mut attributes = HashMap::new();
    attributes.insert("route".to_string(), route_id.to_string());
    let mut objects = episode_objects(file, code, run_id);
    objects.push(route_ref(route_id));
    OCELEvent {
        id: event_id(activity::REPAIR_APPLIED, file, run_id, seq),
        activity: activity::REPAIR_APPLIED.to_string(),
        timestamp: Utc::now(),
        objects,
        attributes,
    }
}

/// Build a `ReceiptEmitted` event binding a closed episode to its receipt id.
#[must_use]
pub fn receipt_emitted(
    file: &str, code: &str, receipt_id: &str, run_id: &str, seq: u64,
) -> OCELEvent {
    let mut attributes = HashMap::new();
    attributes.insert("receipt_id".to_string(), receipt_id.to_string());
    let mut objects = episode_objects(file, code, run_id);
    objects.push(OCELObjectRef {
        id: receipt_id.to_string(),
        r#type: obj_type::RECEIPT.to_string(),
        qualifier: Some("receipt".to_string()),
    });
    OCELEvent {
        id: event_id(activity::RECEIPT_EMITTED, file, run_id, seq),
        activity: activity::RECEIPT_EMITTED.to_string(),
        timestamp: Utc::now(),
        objects,
        attributes,
    }
}

/// Build a `GatePassed` or `GateFailed` event for one (file, code) within `run_id`.
#[must_use]
pub fn gate_result(
    file: &str, code: &str, passed: bool, error_count: usize, run_id: &str, seq: u64,
) -> OCELEvent {
    let act = if passed {
        activity::GATE_PASSED
    } else {
        activity::GATE_FAILED
    };
    let mut attributes = HashMap::new();
    attributes.insert("error_count".to_string(), error_count.to_string());
    OCELEvent {
        id: event_id(act, file, run_id, seq),
        activity: act.to_string(),
        timestamp: Utc::now(),
        objects: episode_objects(file, code, run_id),
        attributes,
    }
}

/// Build a `RefusalEmitted` event for one (file, code) within `run_id`.
#[must_use]
pub fn refusal_emitted(
    file: &str, code: &str, error_count: usize, run_id: &str, seq: u64,
) -> OCELEvent {
    let mut attributes = HashMap::new();
    attributes.insert("error_count".to_string(), error_count.to_string());
    OCELEvent {
        id: event_id(activity::REFUSAL_EMITTED, file, run_id, seq),
        activity: activity::REFUSAL_EMITTED.to_string(),
        timestamp: Utc::now(),
        objects: episode_objects(file, code, run_id),
        attributes,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn diagnostic_raised_carries_correlation_objects() {
        let e = diagnostic_raised("spec.ttl", "E0010", "error", "0:0-0:1", "run1", 1);
        assert_eq!(e.activity, activity::DIAGNOSTIC_RAISED);
        assert!(e.objects.iter().any(|o| o.r#type == obj_type::FILE));
        assert!(e
            .objects
            .iter()
            .any(|o| o.r#type == obj_type::DIAGNOSTIC_CODE && o.id == "E0010"));
        assert!(e.objects.iter().any(|o| o.r#type == obj_type::EPISODE));
        assert_eq!(
            e.attributes.get("severity").map(String::as_str),
            Some("error")
        );
    }

    #[test]
    fn separate_runs_yield_distinct_episodes() {
        // The episode-collapse fix: same (file, code) in two runs → two cases.
        let e1 = diagnostic_raised("a.ttl", "E0010", "error", "0:0-0:1", &new_run_id(), 1);
        let e2 = diagnostic_raised("a.ttl", "E0010", "error", "0:0-0:1", &new_run_id(), 1);
        let ep = |e: &OCELEvent| {
            e.objects
                .iter()
                .find(|o| o.r#type == obj_type::EPISODE)
                .map(|o| o.id.clone())
                .unwrap_or_default()
        };
        assert_ne!(ep(&e1), ep(&e2), "distinct runs must not share an episode");
    }

    #[test]
    fn same_run_same_file_code_shares_episode() {
        let run = new_run_id();
        let raised = diagnostic_raised("a.ttl", "E0010", "error", "0:0-0:1", &run, 1);
        let gate = gate_result("a.ttl", "E0010", false, 1, &run, 2);
        let ep = |e: &OCELEvent| {
            e.objects
                .iter()
                .find(|o| o.r#type == obj_type::EPISODE)
                .map(|o| o.id.clone())
                .unwrap_or_default()
        };
        assert_eq!(
            ep(&raised),
            ep(&gate),
            "one diagnostic's lifecycle in a run is one episode"
        );
    }

    #[test]
    fn event_ids_are_stable_per_inputs() {
        assert_eq!(
            event_id("A", "f.ttl", "r", 3),
            event_id("A", "f.ttl", "r", 3),
            "same inputs → same id"
        );
        assert_ne!(
            event_id("A", "f.ttl", "r", 3),
            event_id("A", "f.ttl", "r", 4)
        );
    }
}
