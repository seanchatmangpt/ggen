use serde::{Deserialize, Serialize};
use std::fs;
use std::collections::{HashMap, HashSet};
use chrono::{DateTime, FixedOffset};
use ocel_core::{OCEL, OCELEvent, OCELObject, OCELRelationship, OCELType};

#[derive(Debug, Deserialize, Serialize, Clone)]
struct RawEvent {
    event_id: String,
    event_type: String,
    timestamp: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    object_id: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    checkpoint_id: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    artifact_digest: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    status: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pack_id: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    source_id: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    code: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    previous_receipt: Option<String>,
}

fn process_evidence_to_ocel(content: &str) -> Result<OCEL, Box<dyn std::error::Error>> {
    let mut events = Vec::new();
    let mut objects = HashMap::new();
    let mut seen_objects = HashSet::new();
    let mut seen_event_types = HashSet::new();
    let mut seen_object_types = HashSet::new();

    for line in content.lines() {
        if line.trim().is_empty() { continue; }
        let raw: RawEvent = serde_json::from_str(line)?;
        
        seen_event_types.insert(raw.event_type.clone());

        let time = DateTime::parse_from_rfc3339(&raw.timestamp)
            .unwrap_or_else(|_| DateTime::parse_from_rfc3339("2026-06-06T00:00:00Z").unwrap())
            .with_timezone(&FixedOffset::east_opt(0).unwrap());

        let mut relationships = Vec::new();
        
        if let Some(ref obj_id) = raw.object_id {
            let otype = "Artifact".to_string();
            seen_object_types.insert(otype.clone());
            if !seen_objects.contains(obj_id) {
                objects.insert(obj_id.clone(), OCELObject {
                    id: obj_id.clone(),
                    object_type: otype,
                    attributes: vec![],
                    relationships: vec![],
                });
                seen_objects.insert(obj_id.clone());
            }
            relationships.push(OCELRelationship {
                object_id: obj_id.clone(),
                qualifier: "subject".to_string(),
            });
        }

        if let Some(ref cp_id) = raw.checkpoint_id {
            let otype = "Checkpoint".to_string();
            seen_object_types.insert(otype.clone());
            if !seen_objects.contains(cp_id) {
                objects.insert(cp_id.clone(), OCELObject {
                    id: cp_id.clone(),
                    object_type: otype,
                    attributes: vec![],
                    relationships: vec![],
                });
                seen_objects.insert(cp_id.clone());
            }
            relationships.push(OCELRelationship {
                object_id: cp_id.clone(),
                qualifier: "proves".to_string(),
            });
        }

        let mut attributes = Vec::new();
        if let Some(pr) = raw.previous_receipt {
             attributes.push(ocel_core::OCELEventAttribute {
                name: "previous_receipt".to_string(),
                value: ocel_core::OCELAttributeValue::String(pr),
            });
        }

        events.push(OCELEvent {
            id: raw.event_id,
            event_type: raw.event_type,
            time,
            attributes,
            relationships,
        });
    }

    Ok(OCEL {
        event_types: seen_event_types.into_iter().map(|n| OCELType { name: n, attributes: vec![] }).collect(),
        object_types: seen_object_types.into_iter().map(|n| OCELType { name: n, attributes: vec![] }).collect(),
        events,
        objects: objects.into_values().collect(),
    })
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let evidence_path = "docs/reports/GC005_PROCESS_EVIDENCE.jsonl";
    if !std::path::Path::new(evidence_path).exists() {
        eprintln!("Error: Evidence file {} not found.", evidence_path);
        std::process::exit(1);
    }
    let original_content = fs::read_to_string(evidence_path)?;

    // 1. Production Run
    {
        let ocel_log = process_evidence_to_ocel(&original_content)?;
        let json = serde_json::to_string_pretty(&ocel_log)?;
        let output_path = "crates/playground/ocel/admitted_evidence.ocel.json";
        fs::create_dir_all("crates/playground/ocel")?;
        fs::write(output_path, json)?;
        println!("Successfully emitted wasm4pm-admitted OCEL to {}", output_path);
    }

    Ok(())
}
