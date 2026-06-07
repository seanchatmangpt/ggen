use std::io::{self, Read};
use serde_json::{json, Value};

fn main() {
    let mut content = String::new();
    if let Err(_) = io::stdin().read_to_string(&mut content) {
        println!("{}", json!({ "issues": [{ "code": "WASM4PM-EVIDENCE-MISSING", "message": "Failed to read stdin" }] }));
        return;
    }

    let content = content.trim();

    if content.is_empty() {
        println!("{}", json!({ "issues": [{ "code": "WASM4PM-EVIDENCE-MISSING", "message": "Process evidence file is empty" }] }));
        return;
    }

    // Parse as JSONL
    let lines: Vec<&str> = content.lines().collect();
    let mut parsed_events = Vec::new();
    
    for line in &lines {
        match serde_json::from_str::<Value>(line) {
            Ok(v) => parsed_events.push(v),
            Err(_) => {
                println!("{}", json!({ "issues": [{ "code": "WASM4PM-OCEL-SHAPE-INVALID", "message": "Evidence does not conform to OCEL schema or JSON is malformed" }] }));
                return;
            }
        }
    }
    
    if parsed_events.is_empty() {
        println!("{}", json!({ "issues": [{ "code": "WASM4PM-EVIDENCE-MISSING", "message": "No events found" }] }));
        return;
    }

    let mut issues = Vec::new();
    let mut events = Vec::new();

    // Check basic OCEL shape
    for ev in &parsed_events {
        if let Some(obj) = ev.as_object() {
            if !obj.contains_key("event_type") {
                issues.push(json!({ "code": "WASM4PM-OCEL-SHAPE-INVALID", "message": "Missing event_type" }));
                break;
            } else {
                events.push(obj.get("event_type").and_then(|v| v.as_str()).unwrap_or(""));
            }
            if obj.contains_key("digest_mismatch") && obj.get("digest_mismatch").and_then(|v| v.as_bool()).unwrap_or(false) {
                issues.push(json!({ "code": "WASM4PM-DIGEST-CHAIN-BROKEN", "message": "Digest chain does not match event sequence" }));
            }
        } else {
            issues.push(json!({ "code": "WASM4PM-OCEL-SHAPE-INVALID", "message": "Event is not an object" }));
            break;
        }
    }

    if !issues.is_empty() {
        println!("{}", json!({ "issues": issues }));
        return;
    }

    // Process Conformance Mining Rules
    // Expected sequence: BoundaryDeclared -> PackPlanConstructed -> StagingPrepared -> MutationGateAdmitted -> ArtifactWritten

    let mut has_boundary = false;
    let mut has_staging = false;
    let mut has_mutation = false;
    let mut has_written = false;
    
    for e in &events {
        match *e {
            "BoundaryDeclared" => has_boundary = true,
            "StagingPrepared" => has_staging = true,
            "MutationGateAdmitted" => has_mutation = true,
            "ArtifactWritten" => {
                has_written = true;
                if !has_staging {
                    issues.push(json!({ "code": "WASM4PM-REPLAY-DEVIATION", "message": "Execution path deviated: ArtifactWritten before StagingPrepared" }));
                }
                if !has_mutation {
                    issues.push(json!({ "code": "WASM4PM-CONFORMANCE-BLOCKED", "message": "Cannot evaluate conformance: ArtifactWritten without MutationGateAdmitted" }));
                }
            }
            _ => {}
        }
    }

    if !has_boundary {
        issues.push(json!({ "code": "WASM4PM-CONFORMANCE-BLOCKED", "message": "Cannot evaluate conformance: No BoundaryDeclared" }));
    }

    if !issues.is_empty() {
        println!("{}", json!({ "issues": issues }));
        return;
    }

    if has_written {
        println!("{}", json!({ "issues": [{ "code": "WASM4PM-VERDICT-FIT", "message": "Process execution conforms to declared laws" }] }));
        return;
    }

    // Default inconclusive
    println!("{}", json!({ "issues": [{ "code": "WASM4PM-VERDICT-INCONCLUSIVE", "message": "Process execution cannot be definitively evaluated" }] }));
}