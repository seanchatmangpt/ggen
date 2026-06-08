use std::io::{self, Read};
use wasm4pm_algos::gall::{check_gall_conformance, GallVerdict};
use serde_json::{json, Value};
use ocel_core::{OCEL, OCELEvent, OCELRelationship, OCELEventAttribute, OCELAttributeValue};

fn main() {
    let mut content = String::new();
    if io::stdin().read_to_string(&mut content).is_err() {
        print_issue("WASM4PM-CONFORMANCE-BLOCKED", "Failed to read input from stdin.");
        return;
    }

    match parse_and_validate_ocel(&content) {
        Ok(ocel) => {
            let verdict = check_gall_conformance(ocel);
            match verdict {
                GallVerdict::Blocked { reason } => {
                    println!("{}", json!({
                        "issues": [
                            { "code": "WASM4PM-VERDICT-BLOCKED", "message": format!("BLOCKED: {}", reason) }
                        ]
                    }));
                }
                GallVerdict::Fit { fitness } => {
                    println!("{}", json!({
                        "issues": [
                            { "code": "WASM4PM-VERDICT-FIT", "message": format!("FIT: (Fitness: {:.1})", fitness) }
                        ]
                    }));
                }
                GallVerdict::Deviation { fitness, missing } => {
                    println!("{}", json!({
                        "issues": [
                            { "code": "WASM4PM-VERDICT-DEVIATION", "message": format!("DEVIATION: (Fitness: {:.1}). Missing: {}", fitness, missing.join(", ")) }
                        ]
                    }));
                }
                GallVerdict::Inconclusive { reason } => {
                    println!("{}", json!({
                        "issues": [
                            { "code": "WASM4PM-VERDICT-INCONCLUSIVE", "message": format!("INCONCLUSIVE: {}", reason) }
                        ]
                    }));
                }
            }
        }
        Err((code, msg)) => {
            print_issue(&code, &msg);
        }
    }
}

fn parse_and_validate_ocel(content: &str) -> Result<OCEL, (String, String)> {
    let content_trimmed = content.trim();
    if content_trimmed.is_empty() {
        return Err(("WASM4PM-EVIDENCE-MISSING".to_string(), "Stdin input is empty.".to_string()));
    }

    // Attempt to parse as JSON Value
    let json_val: Value = match serde_json::from_str(content_trimmed) {
        Ok(v) => v,
        Err(_) => {
            // Check if it's JSONL
            let mut ocel_events = Vec::new();
            let mut parse_failed = false;
            for line in content_trimmed.lines() {
                let line_t = line.trim();
                if line_t.is_empty() { continue; }
                match serde_json::from_str::<Value>(line_t) {
                    Ok(v) => {
                        ocel_events.push(v);
                    }
                    Err(_) => {
                        parse_failed = true;
                        break;
                    }
                }
            }
            if parse_failed || ocel_events.is_empty() {
                return Err(("WASM4PM-OCEL-SHAPE-INVALID".to_string(), "Failed to parse input as JSON or JSONL.".to_string()));
            }
            json!({ "events": ocel_events })
        }
    };

    let mut ocel = OCEL {
        event_types: Vec::new(),
        object_types: Vec::new(),
        events: Vec::new(),
        objects: Vec::new(),
    };

    // Extract events
    let mut raw_events = Vec::new();
    if let Some(arr) = json_val.as_array() {
        raw_events.extend(arr.clone());
    } else if let Some(events_arr) = json_val.get("events").and_then(|e| e.as_array()) {
        raw_events.extend(events_arr.clone());
    } else if json_val.is_object() {
        raw_events.push(json_val.clone());
    } else {
        return Err(("WASM4PM-OCEL-SHAPE-INVALID".to_string(), "Invalid top-level JSON structure.".to_string()));
    }

    // Convert raw events to ocel_core::OCELEvent
    for raw_ev in &raw_events {
        if !raw_ev.is_object() {
            return Err(("WASM4PM-OCEL-SHAPE-INVALID".to_string(), "Event is not a JSON object.".to_string()));
        }

        // 1. Check ID
        let id_val = raw_ev.get("id");
        if id_val.is_none() || id_val.unwrap().is_null() {
            return Err(("WASM4PM-EVIDENCE-MISSING-ID".to_string(), "Event lacks a valid 'id' field.".to_string()));
        }
        let id = match id_val.unwrap().as_str() {
            Some(s) if !s.trim().is_empty() => s.to_string(),
            _ => return Err(("WASM4PM-EVIDENCE-MISSING-ID".to_string(), "Event lacks a non-empty string 'id' field.".to_string())),
        };

        // 2. Check Type / Event Type
        let type_val = raw_ev.get("type").or_else(|| raw_ev.get("event_type"));
        if type_val.is_none() || type_val.unwrap().is_null() {
            return Err(("WASM4PM-OCEL-SHAPE-INVALID".to_string(), "Event lacks a valid 'type' or 'event_type' field.".to_string()));
        }
        let event_type_raw = match type_val.unwrap().as_str() {
            Some(s) if !s.trim().is_empty() => s.to_string(),
            _ => return Err(("WASM4PM-OCEL-SHAPE-INVALID".to_string(), "Event lacks a non-empty string 'type' field.".to_string())),
        };

        // 3. Check Time
        let time_val = raw_ev.get("time");
        if time_val.is_none() || time_val.unwrap().is_null() {
            return Err(("WASM4PM-EVIDENCE-MISSING-TIME".to_string(), "Event lacks a valid 'time' field.".to_string()));
        }
        let time_str = match time_val.unwrap().as_str() {
            Some(s) if !s.trim().is_empty() => s.to_string(),
            _ => return Err(("WASM4PM-EVIDENCE-MISSING-TIME".to_string(), "Event lacks a non-empty string 'time' field.".to_string())),
        };
        let time = match chrono::DateTime::parse_from_rfc3339(&time_str) {
            Ok(dt) => dt,
            Err(_) => return Err(("WASM4PM-OCEL-SHAPE-INVALID".to_string(), format!("Event has invalid RFC3339 time format: {}", time_str))),
        };

        // 4. Parse attributes
        let mut attributes = Vec::new();
        if let Some(attrs_val) = raw_ev.get("attributes") {
            if let Some(attrs_arr) = attrs_val.as_array() {
                for attr_item in attrs_arr {
                    if let (Some(name), Some(val)) = (attr_item.get("name").and_then(|n| n.as_str()), attr_item.get("value")) {
                        let attr_val = match val {
                            Value::String(s) => OCELAttributeValue::String(s.clone()),
                            Value::Number(n) => {
                                if let Some(iv) = n.as_i64() {
                                    OCELAttributeValue::Integer(iv)
                                } else {
                                    OCELAttributeValue::Float(n.as_f64().unwrap_or(0.0))
                                }
                            }
                            Value::Bool(b) => OCELAttributeValue::Boolean(*b),
                            _ => OCELAttributeValue::Null,
                        };
                        attributes.push(OCELEventAttribute {
                            name: name.to_string(),
                            value: attr_val,
                        });
                    }
                }
            }
        }

        // Also check if previous_receipt is on the raw event object itself, and map it
        if let Some(pr) = raw_ev.get("previous_receipt") {
            if let Some(pr_str) = pr.as_str() {
                // Avoid duplicating if already present in attributes
                if !attributes.iter().any(|a| a.name == "previous_receipt") {
                    attributes.push(OCELEventAttribute {
                        name: "previous_receipt".to_string(),
                        value: OCELAttributeValue::String(pr_str.to_string()),
                    });
                }
            }
        }

        // 5. Parse relationships
        let mut relationships = Vec::new();
        if let Some(rels_val) = raw_ev.get("relationships") {
            if let Some(rels_arr) = rels_val.as_array() {
                for rel_item in rels_arr {
                    if let Some(obj_id) = rel_item.get("objectId").and_then(|o| o.as_str()).or_else(|| rel_item.get("object_id").and_then(|o| o.as_str())) {
                        relationships.push(OCELRelationship {
                            object_id: obj_id.to_string(),
                            qualifier: rel_item.get("qualifier").and_then(|q| q.as_str()).unwrap_or("proves").to_string(),
                        });
                    }
                }
            }
        }

        // Translate already-admissible shape (e.g. BoundaryDeclared etc.)
        let event_type = match event_type_raw.as_str() {
            "BoundaryDeclared" => {
                relationships.push(OCELRelationship {
                    object_id: "GALL-CHECKPOINT-001".to_string(),
                    qualifier: "proves".to_string(),
                });
                "checkpoint.admitted".to_string()
            }
            "StagingPrepared" => {
                relationships.push(OCELRelationship {
                    object_id: "GALL-CHECKPOINT-002".to_string(),
                    qualifier: "proves".to_string(),
                });
                "checkpoint.admitted".to_string()
            }
            "MutationGateAdmitted" => {
                relationships.push(OCELRelationship {
                    object_id: "GALL-CHECKPOINT-003".to_string(),
                    qualifier: "proves".to_string(),
                });
                "checkpoint.admitted".to_string()
            }
            "ArtifactWritten" => {
                relationships.push(OCELRelationship {
                    object_id: "GALL-CHECKPOINT-004".to_string(),
                    qualifier: "proves".to_string(),
                });
                "checkpoint.admitted".to_string()
            }
            other => other.to_string(),
        };

        ocel.events.push(OCELEvent {
            id,
            event_type,
            time,
            attributes,
            relationships,
        });
    }

    Ok(ocel)
}

fn print_issue(code: &str, msg: &str) {
    println!("{}", json!({
        "issues": [
            {
                "code": code,
                "message": msg
            }
        ]
    }));
}
