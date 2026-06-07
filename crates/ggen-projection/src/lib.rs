use chrono::{DateTime, Utc};
use serde::{Deserialize, Serialize};
use std::collections::{BTreeMap, BTreeSet};

pub mod boundary;
pub mod descriptor;
pub mod mapping;
pub mod pipeline;
pub mod plan;
pub mod receipt;
pub mod protocol;

pub use boundary::BoundaryLedger;
pub use descriptor::{PackDescriptor, PackTemplateDescriptor};
pub use mapping::{CustomizationMap, ProjectionMap, ProjectionMapping};
pub use pipeline::{sync, StagingGate};
pub use plan::{
    is_compatible, DependencyCycleError, DependencyNotFoundError, PackPlan, VersionConflictError,
};
pub use protocol::{PackObservation, PackFinding, ProjectionSignature, CustomizationPoint, PackActionIntent, GgenObservedDiagnostic};
pub use receipt::{CryptographicReceipt, Receipt, ReceiptIndex, EquationContext, ReceiptValidationError};

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct Pair2 {
    pub source: String,   // e.g., "event:e1" or "object:o1"
    pub target: String,   // e.g., "object:o1", "item", or a literal
    pub rel_type: String, // e.g., "ocel:type", "ocel:time", "color", "hasStatus"
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct RelationPage {
    pub page_id: String,
    pub timestamp: DateTime<Utc>,
    pub pairs: Vec<Pair2>,
}

/// Normalizes resource identifiers to absolute URI/IRI format or blank nodes.
pub fn normalize_resource(val: &str) -> String {
    let trimmed = val.trim();
    let stripped = trimmed
        .strip_prefix("event:")
        .or_else(|| trimmed.strip_prefix("object:"))
        .unwrap_or(trimmed);
    if stripped.starts_with('<') && stripped.ends_with('>') {
        stripped.to_string()
    } else if stripped.starts_with("_:") {
        stripped.to_string()
    } else if stripped.starts_with('"') {
        stripped.to_string()
    } else if stripped.contains("://") || stripped.starts_with("urn:") {
        format!("<{}>", stripped)
    } else {
        format!("<http://knhk.io/resource/{}>", stripped)
    }
}

/// Normalizes relationship predicate identifiers to URI/IRI format.
pub fn normalize_predicate(val: &str) -> String {
    let trimmed = val.trim();
    if trimmed.starts_with('<') && trimmed.ends_with('>') {
        trimmed.to_string()
    } else if trimmed.contains("://") || trimmed.starts_with("urn:") {
        format!("<{}>", trimmed)
    } else {
        format!("<http://knhk.io/vocab/{}>", trimmed)
    }
}

/// Escapes double quotes and backslashes in RDF literals.
pub fn escape_literal(val: &str) -> String {
    let mut escaped = String::new();
    for c in val.chars() {
        match c {
            '\\' => escaped.push_str("\\\\"),
            '"' => escaped.push_str("\\\""),
            '\n' => escaped.push_str("\\n"),
            '\r' => escaped.push_str("\\r"),
            _ => escaped.push(c),
        }
    }
    escaped
}

/// Normalizes objects/values to URIs or formatted literals.
pub fn normalize_object(val: &str) -> String {
    let trimmed = val.trim();
    let stripped = trimmed
        .strip_prefix("event:")
        .or_else(|| trimmed.strip_prefix("object:"))
        .unwrap_or(trimmed);
    if stripped.starts_with('<') && stripped.ends_with('>') {
        stripped.to_string()
    } else if stripped.starts_with("_:") {
        stripped.to_string()
    } else if stripped.starts_with('"') {
        stripped.to_string()
    } else if stripped.contains("://") || stripped.starts_with("urn:") {
        format!("<{}>", stripped)
    } else {
        if stripped == "true" || stripped == "false" {
            format!(
                "\"{}\"^^<http://www.w3.org/2001/XMLSchema#boolean>",
                stripped
            )
        } else if let Ok(_) = stripped.parse::<i64>() {
            format!(
                "\"{}\"^^<http://www.w3.org/2001/XMLSchema#integer>",
                stripped
            )
        } else if let Ok(_) = stripped.parse::<f64>() {
            format!(
                "\"{}\"^^<http://www.w3.org/2001/XMLSchema#double>",
                stripped
            )
        } else {
            format!(
                "\"{}\"^^<http://www.w3.org/2001/XMLSchema#string>",
                escape_literal(stripped)
            )
        }
    }
}

fn infer_attribute_type(val: &str) -> &'static str {
    if val == "true" || val == "false" {
        "boolean"
    } else if val.parse::<i64>().is_ok() {
        "integer"
    } else if val.parse::<f64>().is_ok() {
        "float"
    } else if chrono::DateTime::parse_from_rfc3339(val).is_ok() {
        "date-time"
    } else {
        "string"
    }
}

struct OcelEvent {
    activity: String,
    time: String,
    attributes: BTreeMap<String, String>,
    relationships: Vec<(String, String)>,
}

struct OcelObject {
    object_type: String,
    attributes: BTreeMap<String, (String, String)>, // name -> (value, time)
    relationships: Vec<(String, String)>,
}

/// Projects a set of RelationPages into OCEL v2 JSON format conforming to the official schema.
pub fn project_ocel2(pages: &[RelationPage]) -> serde_json::Value {
    let mut events_map: BTreeMap<String, OcelEvent> = BTreeMap::new();
    let mut objects_map: BTreeMap<String, OcelObject> = BTreeMap::new();

    for page in pages {
        for pair in &page.pairs {
            let src = pair.source.trim();
            let tgt = pair.target.trim();
            let rel = pair.rel_type.trim();

            let is_event_src =
                src.starts_with("event:") || src.starts_with("e_") || src.contains("/event/");
            let is_object_src =
                src.starts_with("object:") || src.starts_with("o_") || src.contains("/object/");

            let clean_src = src
                .strip_prefix("event:")
                .or_else(|| src.strip_prefix("object:"))
                .unwrap_or(src)
                .to_string();
            let clean_tgt = tgt
                .strip_prefix("event:")
                .or_else(|| tgt.strip_prefix("object:"))
                .unwrap_or(tgt)
                .to_string();

            if is_event_src {
                let event = events_map
                    .entry(clean_src.clone())
                    .or_insert_with(|| OcelEvent {
                        activity: "Activity".to_string(),
                        time: page.timestamp.to_rfc3339(),
                        attributes: BTreeMap::new(),
                        relationships: Vec::new(),
                    });

                if rel == "ocel:type" || rel == "type" {
                    event.activity = clean_tgt;
                } else if rel == "ocel:time" || rel == "time" {
                    event.time = clean_tgt;
                } else if rel == "ocel:object"
                    || tgt.starts_with("object:")
                    || tgt.starts_with("o_")
                    || tgt.contains("/object/")
                {
                    let qualifier = if rel == "ocel:object" {
                        "relatesTo".to_string()
                    } else {
                        rel.to_string()
                    };
                    event.relationships.push((clean_tgt.clone(), qualifier));
                    // Ensure object exists
                    objects_map.entry(clean_tgt).or_insert_with(|| OcelObject {
                        object_type: "Object".to_string(),
                        attributes: BTreeMap::new(),
                        relationships: Vec::new(),
                    });
                } else {
                    event.attributes.insert(rel.to_string(), clean_tgt);
                }
            } else if is_object_src {
                let object = objects_map
                    .entry(clean_src.clone())
                    .or_insert_with(|| OcelObject {
                        object_type: "Object".to_string(),
                        attributes: BTreeMap::new(),
                        relationships: Vec::new(),
                    });

                if rel == "ocel:type" || rel == "type" {
                    object.object_type = clean_tgt;
                } else if tgt.starts_with("object:")
                    || tgt.starts_with("o_")
                    || tgt.contains("/object/")
                {
                    object
                        .relationships
                        .push((clean_tgt.clone(), rel.to_string()));
                    // Ensure target object exists
                    objects_map.entry(clean_tgt).or_insert_with(|| OcelObject {
                        object_type: "Object".to_string(),
                        attributes: BTreeMap::new(),
                        relationships: Vec::new(),
                    });
                } else {
                    object
                        .attributes
                        .insert(rel.to_string(), (clean_tgt, page.timestamp.to_rfc3339()));
                }
            }
        }
    }

    // Infer schemas for eventTypes and objectTypes
    let mut event_types_attrs: BTreeMap<String, BTreeMap<String, &'static str>> = BTreeMap::new();
    for event in events_map.values() {
        let entry = event_types_attrs.entry(event.activity.clone()).or_default();
        for (name, val) in &event.attributes {
            let inferred = infer_attribute_type(val);
            entry.insert(name.clone(), inferred);
        }
    }

    let mut object_types_attrs: BTreeMap<String, BTreeMap<String, &'static str>> = BTreeMap::new();
    for object in objects_map.values() {
        let entry = object_types_attrs
            .entry(object.object_type.clone())
            .or_default();
        for (name, (val, _)) in &object.attributes {
            let inferred = infer_attribute_type(val);
            entry.insert(name.clone(), inferred);
        }
    }

    let mut event_types_arr = Vec::new();
    for (name, attrs) in event_types_attrs {
        let mut type_obj = serde_json::Map::new();
        type_obj.insert("name".to_string(), serde_json::Value::String(name));
        let mut attrs_arr = Vec::new();
        for (attr_name, attr_type) in attrs {
            let mut attr_obj = serde_json::Map::new();
            attr_obj.insert("name".to_string(), serde_json::Value::String(attr_name));
            attr_obj.insert(
                "type".to_string(),
                serde_json::Value::String(attr_type.to_string()),
            );
            attrs_arr.push(serde_json::Value::Object(attr_obj));
        }
        type_obj.insert(
            "attributes".to_string(),
            serde_json::Value::Array(attrs_arr),
        );
        event_types_arr.push(serde_json::Value::Object(type_obj));
    }

    let mut object_types_arr = Vec::new();
    for (name, attrs) in object_types_attrs {
        let mut type_obj = serde_json::Map::new();
        type_obj.insert("name".to_string(), serde_json::Value::String(name));
        let mut attrs_arr = Vec::new();
        for (attr_name, attr_type) in attrs {
            let mut attr_obj = serde_json::Map::new();
            attr_obj.insert("name".to_string(), serde_json::Value::String(attr_name));
            attr_obj.insert(
                "type".to_string(),
                serde_json::Value::String(attr_type.to_string()),
            );
            attrs_arr.push(serde_json::Value::Object(attr_obj));
        }
        type_obj.insert(
            "attributes".to_string(),
            serde_json::Value::Array(attrs_arr),
        );
        object_types_arr.push(serde_json::Value::Object(type_obj));
    }

    // Build events list
    let mut events_arr = Vec::new();
    for (id, event) in events_map {
        let mut ev_obj = serde_json::Map::new();
        ev_obj.insert("id".to_string(), serde_json::Value::String(id));
        ev_obj.insert(
            "type".to_string(),
            serde_json::Value::String(event.activity),
        );
        ev_obj.insert("time".to_string(), serde_json::Value::String(event.time));

        let mut attrs_arr = Vec::new();
        for (name, val) in event.attributes {
            let mut attr = serde_json::Map::new();
            attr.insert("name".to_string(), serde_json::Value::String(name));
            attr.insert("value".to_string(), serde_json::Value::String(val));
            attrs_arr.push(serde_json::Value::Object(attr));
        }
        ev_obj.insert(
            "attributes".to_string(),
            serde_json::Value::Array(attrs_arr),
        );

        let mut rels_arr = Vec::new();
        for (obj_id, qualifier) in event.relationships {
            let mut rel = serde_json::Map::new();
            rel.insert("objectId".to_string(), serde_json::Value::String(obj_id));
            rel.insert(
                "qualifier".to_string(),
                serde_json::Value::String(qualifier),
            );
            rels_arr.push(serde_json::Value::Object(rel));
        }
        ev_obj.insert(
            "relationships".to_string(),
            serde_json::Value::Array(rels_arr),
        );

        events_arr.push(serde_json::Value::Object(ev_obj));
    }

    // Build objects list
    let mut objects_arr = Vec::new();
    for (id, object) in objects_map {
        let mut obj_item = serde_json::Map::new();
        obj_item.insert("id".to_string(), serde_json::Value::String(id));
        obj_item.insert(
            "type".to_string(),
            serde_json::Value::String(object.object_type),
        );

        let mut attrs_arr = Vec::new();
        for (name, (val, time)) in object.attributes {
            let mut attr = serde_json::Map::new();
            attr.insert("name".to_string(), serde_json::Value::String(name));
            attr.insert("value".to_string(), serde_json::Value::String(val));
            attr.insert("time".to_string(), serde_json::Value::String(time));
            attrs_arr.push(serde_json::Value::Object(attr));
        }
        obj_item.insert(
            "attributes".to_string(),
            serde_json::Value::Array(attrs_arr),
        );

        let mut rels_arr = Vec::new();
        for (obj_id, qualifier) in object.relationships {
            let mut rel = serde_json::Map::new();
            rel.insert("objectId".to_string(), serde_json::Value::String(obj_id));
            rel.insert(
                "qualifier".to_string(),
                serde_json::Value::String(qualifier),
            );
            rels_arr.push(serde_json::Value::Object(rel));
        }
        obj_item.insert(
            "relationships".to_string(),
            serde_json::Value::Array(rels_arr),
        );

        objects_arr.push(serde_json::Value::Object(obj_item));
    }

    let mut root = serde_json::Map::new();
    root.insert(
        "eventTypes".to_string(),
        serde_json::Value::Array(event_types_arr),
    );
    root.insert(
        "objectTypes".to_string(),
        serde_json::Value::Array(object_types_arr),
    );
    root.insert("events".to_string(), serde_json::Value::Array(events_arr));
    root.insert("objects".to_string(), serde_json::Value::Array(objects_arr));

    serde_json::Value::Object(root)
}

/// Projects a set of RelationPages into RDF/N-Quads representation.
pub fn project_nquads(pages: &[RelationPage]) -> String {
    let mut out = String::new();
    for page in pages {
        let page_node = normalize_resource(&page.page_id);
        out.push_str(&format!(
            "{} <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://knhk.io/vocab/RelationPage> {} .\n",
            page_node, page_node
        ));
        out.push_str(&format!(
            "{} <http://knhk.io/vocab/timestamp> \"{}\"^^<http://www.w3.org/2001/XMLSchema#dateTime> {} .\n",
            page_node, page.timestamp.to_rfc3339(), page_node
        ));

        for pair in &page.pairs {
            let s = normalize_resource(&pair.source);
            let p = normalize_predicate(&pair.rel_type);
            let o = normalize_object(&pair.target);
            out.push_str(&format!("{} {} {} {} .\n", s, p, o, page_node));
            out.push_str(&format!(
                "{} <http://knhk.io/vocab/containedInPage> {} {} .\n",
                s, page_node, page_node
            ));
        }
    }
    out
}

/// Projects RelationPages and local receipts into W3C PROV provenance metadata (Turtle format).
pub fn project_prov(pages: &[RelationPage], receipts: &[knhk_construct8::Receipt]) -> String {
    let mut out = String::new();

    out.push_str("@prefix prov: <http://www.w3.org/ns/prov#> .\n");
    out.push_str("@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .\n");
    out.push_str("@prefix knhk: <http://knhk.io/vocab/> .\n\n");

    for (idx, page) in pages.iter().enumerate() {
        let page_uri = normalize_resource(&page.page_id);
        let activity_uri = format!("<http://knhk.io/activity/materialize_{}>", page.page_id);
        let agent_uri = "<http://knhk.io/agent/genesis_engine>";

        out.push_str(&format!(
            "{} a prov:Entity ;\n  prov:wasGeneratedBy {} .\n\n",
            page_uri, activity_uri
        ));
        out.push_str(&format!(
            "{} a prov:Activity ;\n  prov:wasAssociatedWith {} ;\n  prov:startedAtTime \"{}\"^^xsd:dateTime .\n\n",
            activity_uri, agent_uri, page.timestamp.to_rfc3339()
        ));
        out.push_str(&format!("{} a prov:Agent .\n\n", agent_uri));

        if let Some(receipt) = receipts.get(idx) {
            let receipt_uri = format!("<http://knhk.io/receipt/{}>", receipt.hash);
            out.push_str(&format!(
                "{} a prov:Entity ;\n  knhk:packetCount {} ;\n  knhk:tripleCount {} ;\n  prov:wasGeneratedBy {} ;\n  prov:wasDerivedFrom {} .\n\n",
                receipt_uri, receipt.packet_count, receipt.triple_count, activity_uri, page_uri
            ));
        }

        // Trace and project activities/entities from page pairs
        let mut declared_entities = BTreeSet::new();
        let mut declared_activities = BTreeSet::new();

        for pair in &page.pairs {
            let src = pair.source.trim();
            let tgt = pair.target.trim();

            let is_event_src =
                src.starts_with("event:") || src.starts_with("e_") || src.contains("/event/");
            let is_object_src =
                src.starts_with("object:") || src.starts_with("o_") || src.contains("/object/");

            let clean_src = src
                .strip_prefix("event:")
                .or_else(|| src.strip_prefix("object:"))
                .unwrap_or(src);
            let clean_tgt = tgt
                .strip_prefix("event:")
                .or_else(|| tgt.strip_prefix("object:"))
                .unwrap_or(tgt);

            if is_event_src {
                let event_uri = format!("<http://knhk.io/event/{}>", clean_src);
                if declared_activities.insert(event_uri.clone()) {
                    out.push_str(&format!("{} a prov:Activity .\n", event_uri));
                }
                if tgt.starts_with("object:") || tgt.starts_with("o_") || tgt.contains("/object/") {
                    let object_uri = format!("<http://knhk.io/object/{}>", clean_tgt);
                    if declared_entities.insert(object_uri.clone()) {
                        out.push_str(&format!("{} a prov:Entity .\n", object_uri));
                    }
                    out.push_str(&format!("{} prov:used {} .\n", event_uri, object_uri));
                }
            } else if is_object_src {
                let object_uri = format!("<http://knhk.io/object/{}>", clean_src);
                if declared_entities.insert(object_uri.clone()) {
                    out.push_str(&format!("{} a prov:Entity .\n", object_uri));
                }
                if tgt.starts_with("object:") || tgt.starts_with("o_") || tgt.contains("/object/") {
                    let target_uri = format!("<http://knhk.io/object/{}>", clean_tgt);
                    if declared_entities.insert(target_uri.clone()) {
                        out.push_str(&format!("{} a prov:Entity .\n", target_uri));
                    }
                    out.push_str(&format!(
                        "{} prov:wasDerivedFrom {} .\n",
                        target_uri, object_uri
                    ));
                }
            }
        }
    }

    out
}

/// Projects RelationPages and local receipts into a DCAT dataset catalog.
pub fn project_dcat(pages: &[RelationPage], receipts: &[knhk_construct8::Receipt]) -> String {
    let mut out = String::new();

    out.push_str("@prefix dcat: <http://www.w3.org/ns/dcat#> .\n");
    out.push_str("@prefix dct: <http://purl.org/dc/terms/> .\n");
    out.push_str("@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .\n");
    out.push_str("@prefix foaf: <http://xmlns.com/foaf/0.1/> .\n");
    out.push_str("@prefix prov: <http://www.w3.org/ns/prov#> .\n");
    out.push_str("@prefix knhk: <http://knhk.io/vocab/> .\n\n");

    let catalog_uri = "<http://knhk.io/catalog/ggen_projection_catalog>";
    out.push_str(&format!(
        "{} a dcat:Catalog ;\n  dct:title \"KNHK ggen Projection Catalog\" ;\n  dct:publisher <http://knhk.io/agent/genesis_engine> .\n\n",
        catalog_uri
    ));
    out.push_str("<http://knhk.io/agent/genesis_engine> a prov:Agent, foaf:Agent ;\n  foaf:name \"KNHK Genesis Engine\" .\n\n");

    for (idx, page) in pages.iter().enumerate() {
        let dataset_uri = format!("<http://knhk.io/dataset/{}>", page.page_id);
        out.push_str(&format!(
            "{} dcat:dataset {} .\n\n",
            catalog_uri, dataset_uri
        ));
        out.push_str(&format!(
            "{} a dcat:Dataset ;\n  dct:title \"Relation Page Dataset {}\" ;\n  dct:issued \"{}\"^^xsd:dateTime .\n\n",
            dataset_uri, page.page_id, page.timestamp.to_rfc3339()
        ));

        if let Some(receipt) = receipts.get(idx) {
            let dist_uri = format!("<http://knhk.io/distribution/{}>", receipt.hash);
            let download_url = format!("<http://knhk.io/distribution/download/{}>", receipt.hash);
            out.push_str(&format!(
                "{} dct:identifier \"{}\" ;\n  dcat:distribution {} .\n\n",
                dataset_uri, receipt.hash, dist_uri
            ));
            out.push_str(&format!(
                "{} a dcat:Distribution ;\n  dcat:mediaType <https://www.iana.org/assignments/media-types/application/n-quads> ;\n  dcat:accessURL {} ;\n  dcat:downloadURL {} ;\n  knhk:integrityHash \"{}\" .\n\n",
                dist_uri, download_url, download_url, receipt.hash
            ));
        }
    }

    out
}

/// Projects RelationPages structure audit results into a SHACL validation report format.
pub fn project_shacl_refusal(pages: &[RelationPage]) -> String {
    let mut conforms = true;
    let mut results = Vec::new();

    for (p_idx, page) in pages.iter().enumerate() {
        if page.page_id.trim().is_empty() {
            conforms = false;
            results.push(format!(
                "_:res_{} a sh:ValidationResult ;\n\
                 sh:resultSeverity sh:Violation ;\n\
                 sh:focusNode _:page_{} ;\n\
                 sh:resultPath <http://knhk.io/vocab/page_id> ;\n\
                 sh:message \"Relation page identifier is empty\" .",
                results.len(),
                p_idx
            ));
        }

        for (pr_idx, pair) in page.pairs.iter().enumerate() {
            let src = pair.source.trim();
            let tgt = pair.target.trim();
            let rel = pair.rel_type.trim();

            if src.is_empty() {
                conforms = false;
                results.push(format!(
                    "_:res_{} a sh:ValidationResult ;\n\
                     sh:resultSeverity sh:Violation ;\n\
                     sh:focusNode _:page_{}_pair_{} ;\n\
                     sh:resultPath <http://knhk.io/vocab/source> ;\n\
                     sh:message \"Pair source identifier is empty\" .",
                    results.len(),
                    p_idx,
                    pr_idx
                ));
            }

            if tgt.is_empty() {
                conforms = false;
                results.push(format!(
                    "_:res_{} a sh:ValidationResult ;\n\
                     sh:resultSeverity sh:Violation ;\n\
                     sh:focusNode _:page_{}_pair_{} ;\n\
                     sh:resultPath <http://knhk.io/vocab/target> ;\n\
                     sh:message \"Pair target value is empty\" .",
                    results.len(),
                    p_idx,
                    pr_idx
                ));
            }

            if rel.is_empty() {
                conforms = false;
                results.push(format!(
                    "_:res_{} a sh:ValidationResult ;\n\
                     sh:resultSeverity sh:Violation ;\n\
                     sh:focusNode _:page_{}_pair_{} ;\n\
                     sh:resultPath <http://knhk.io/vocab/rel_type> ;\n\
                     sh:message \"Pair relationship/predicate type is empty\" .",
                    results.len(),
                    p_idx,
                    pr_idx
                ));
            }

            if !src.is_empty()
                && !src.starts_with('_')
                && !src.starts_with('<')
                && !src.contains(':')
            {
                conforms = false;
                results.push(format!(
                    "_:res_{} a sh:ValidationResult ;\n\
                     sh:resultSeverity sh:Violation ;\n\
                     sh:focusNode _:page_{}_pair_{} ;\n\
                     sh:resultPath <http://knhk.io/vocab/source> ;\n\
                     sh:value \"{}\" ;\n\
                     sh:message \"Pair source must be a prefixed name, blank node, or IRI\" .",
                    results.len(),
                    p_idx,
                    pr_idx,
                    src.escape_debug()
                ));
            }
        }
    }

    let mut report = String::new();
    report.push_str("@prefix sh: <http://www.w3.org/ns/shacl#> .\n");
    report.push_str("@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .\n\n");
    report.push_str("_:report a sh:ValidationReport ;\n");
    report.push_str(&format!("    sh:conforms {} ^^xsd:boolean", conforms));

    if conforms {
        report.push_str(" .\n");
    } else {
        report.push_str(" ;\n");
        let refs: Vec<String> = (0..results.len())
            .map(|idx| format!("_:res_{}", idx))
            .collect();
        report.push_str(&format!("    sh:result {} .\n\n", refs.join(" , ")));
        for res in results {
            report.push_str(&res);
            report.push_str("\n\n");
        }
    }

    report
}

#[cfg(test)]
mod tests {
    use super::*;
    use chrono::TimeZone;
    use knhk_construct8::Receipt;

    fn get_sample_page() -> RelationPage {
        RelationPage {
            page_id: "page_1001".to_string(),
            timestamp: Utc.with_ymd_and_hms(2026, 5, 27, 0, 0, 0).unwrap(),
            pairs: vec![
                Pair2 {
                    source: "event:e1".to_string(),
                    target: "order".to_string(),
                    rel_type: "ocel:type".to_string(),
                },
                Pair2 {
                    source: "event:e1".to_string(),
                    target: "object:o1".to_string(),
                    rel_type: "ocel:object".to_string(),
                },
                Pair2 {
                    source: "event:e1".to_string(),
                    target: "99.9".to_string(),
                    rel_type: "price".to_string(),
                },
                Pair2 {
                    source: "object:o1".to_string(),
                    target: "item".to_string(),
                    rel_type: "ocel:type".to_string(),
                },
                Pair2 {
                    source: "object:o1".to_string(),
                    target: "red".to_string(),
                    rel_type: "color".to_string(),
                },
            ],
        }
    }

    #[test]
    fn test_ocel2_projection() {
        let pages = vec![get_sample_page()];
        let projected = project_ocel2(&pages);

        let events = projected.get("events").unwrap().as_array().unwrap();
        let e1 = events
            .iter()
            .find(|e| e.get("id").unwrap().as_str().unwrap() == "e1")
            .unwrap()
            .as_object()
            .unwrap();
        assert_eq!(e1.get("type").unwrap().as_str().unwrap(), "order");

        let rels = e1.get("relationships").unwrap().as_array().unwrap();
        assert_eq!(
            rels[0]
                .as_object()
                .unwrap()
                .get("objectId")
                .unwrap()
                .as_str()
                .unwrap(),
            "o1"
        );

        let attrs = e1.get("attributes").unwrap().as_array().unwrap();
        let price_attr = attrs
            .iter()
            .find(|a| a.get("name").unwrap().as_str().unwrap() == "price")
            .unwrap()
            .as_object()
            .unwrap();
        assert_eq!(price_attr.get("value").unwrap().as_str().unwrap(), "99.9");

        let objects = projected.get("objects").unwrap().as_array().unwrap();
        let o1 = objects
            .iter()
            .find(|o| o.get("id").unwrap().as_str().unwrap() == "o1")
            .unwrap()
            .as_object()
            .unwrap();
        assert_eq!(o1.get("type").unwrap().as_str().unwrap(), "item");

        let o1_attrs = o1.get("attributes").unwrap().as_array().unwrap();
        let color_attr = o1_attrs
            .iter()
            .find(|a| a.get("name").unwrap().as_str().unwrap() == "color")
            .unwrap()
            .as_object()
            .unwrap();
        assert_eq!(color_attr.get("value").unwrap().as_str().unwrap(), "red");
    }

    #[test]
    fn test_nquads_projection() {
        let pages = vec![get_sample_page()];
        let projected = project_nquads(&pages);

        assert!(projected.contains("<http://knhk.io/resource/page_1001> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://knhk.io/vocab/RelationPage> <http://knhk.io/resource/page_1001> ."));
        assert!(projected
            .contains("<http://knhk.io/resource/e1> <http://knhk.io/vocab/price> \"99.9\"^^<http://www.w3.org/2001/XMLSchema#double> <http://knhk.io/resource/page_1001> ."));
    }

    #[test]
    fn test_prov_projection() {
        let pages = vec![get_sample_page()];
        let receipt = Receipt::new(blake3::hash(b"dummy_hash"), 5, 10);
        let projected = project_prov(&pages, &[receipt]);

        assert!(projected.contains("a prov:Entity"));
        assert!(projected.contains("a prov:Activity"));
        assert!(projected.contains("prov:wasGeneratedBy"));
        assert!(projected.contains("knhk:packetCount 5"));
        assert!(projected.contains("knhk:tripleCount 10"));
        assert!(projected.contains("<http://knhk.io/event/e1> a prov:Activity"));
        assert!(projected.contains("<http://knhk.io/object/o1> a prov:Entity"));
        assert!(
            projected.contains("<http://knhk.io/event/e1> prov:used <http://knhk.io/object/o1>")
        );
    }

    #[test]
    fn test_dcat_projection() {
        let pages = vec![get_sample_page()];
        let receipt = Receipt::new(blake3::hash(b"dummy_hash"), 5, 10);
        let projected = project_dcat(&pages, &[receipt]);

        assert!(projected.contains("a dcat:Catalog"));
        assert!(projected.contains("a dcat:Dataset"));
        assert!(projected.contains("a dcat:Distribution"));
        assert!(projected.contains("dcat:mediaType"));
        assert!(projected.contains("dcat:accessURL"));
    }

    #[test]
    fn test_shacl_refusal_projection() {
        let pages = vec![get_sample_page()];
        let report = project_shacl_refusal(&pages);
        assert!(report.contains("sh:conforms true"));

        // Let's test a violation/refusal case
        let bad_page = RelationPage {
            page_id: "page_bad".to_string(),
            timestamp: Utc::now(),
            pairs: vec![Pair2 {
                source: "invalid_unprefixed_source".to_string(),
                target: "some_target".to_string(),
                rel_type: "some_rel".to_string(),
            }],
        };
        let bad_report = project_shacl_refusal(&[bad_page]);
        assert!(bad_report.contains("sh:conforms false"));
        assert!(bad_report.contains("sh:ValidationResult"));
        assert!(bad_report.contains("Pair source must be a prefixed name, blank node, or IRI"));
    }
}

// Structs and functions are now implemented in submodules and re-exported above.

#[cfg(test)]
mod projection_models_tests {
    use super::*;

    #[test]
    fn test_pack_descriptor_from_toml() {
        let content = r#"
            id = "pack_1"
            name = "Test Pack"
            version = "26.6.6"
            description = "A test pack"
            license = "MIT"
            dependencies = { "pack_2" = ">=26.6.6" }
            query_aliases = { "alias_1" = "query_1" }

            [[templates]]
            path = "templates/test.tmpl"
            description = "Test template"
            variables = ["var1", "var2"]
        "#;
        let desc = PackDescriptor::from_toml(content).unwrap();
        assert_eq!(desc.id, "pack_1");
        assert_eq!(desc.dependencies.get("pack_2").unwrap(), ">=26.6.6");
        assert_eq!(
            desc.templates[0].path.to_str().unwrap(),
            "templates/test.tmpl"
        );
    }

    #[test]
    fn test_pack_plan_resolve_success() {
        let p2 = PackDescriptor {
            id: "pack_2".to_string(),
            name: "Pack 2".to_string(),
            version: "26.6.6".to_string(),
            description: "No deps".to_string(),
            license: "MIT".to_string(),
            dependencies: std::collections::BTreeMap::new(),
            templates: vec![],
            query_aliases: std::collections::BTreeMap::new(),
        };
        let mut deps = std::collections::BTreeMap::new();
        deps.insert("pack_2".to_string(), ">=26.6.6".to_string());
        let p1 = PackDescriptor {
            id: "pack_1".to_string(),
            name: "Pack 1".to_string(),
            version: "26.6.6".to_string(),
            description: "Depends on pack_2".to_string(),
            license: "MIT".to_string(),
            dependencies: deps,
            templates: vec![],
            query_aliases: std::collections::BTreeMap::new(),
        };

        let descriptors = vec![p1, p2];
        let plan = PackPlan::resolve(&descriptors).unwrap();
        assert_eq!(plan.resolution_order, vec!["pack_2", "pack_1"]);
        assert!(plan.checksums.contains_key("pack_1"));
        assert!(plan.checksums.contains_key("pack_2"));
    }

    #[test]
    fn test_pack_plan_resolve_cycle() {
        let mut deps1 = std::collections::BTreeMap::new();
        deps1.insert("pack_2".to_string(), "*".to_string());
        let p1 = PackDescriptor {
            id: "pack_1".to_string(),
            name: "Pack 1".to_string(),
            version: "26.6.6".to_string(),
            description: "Depends on pack_2".to_string(),
            license: "MIT".to_string(),
            dependencies: deps1,
            templates: vec![],
            query_aliases: std::collections::BTreeMap::new(),
        };

        let mut deps2 = std::collections::BTreeMap::new();
        deps2.insert("pack_1".to_string(), "*".to_string());
        let p2 = PackDescriptor {
            id: "pack_2".to_string(),
            name: "Pack 2".to_string(),
            version: "26.6.6".to_string(),
            description: "Depends on pack_1".to_string(),
            license: "MIT".to_string(),
            dependencies: deps2,
            templates: vec![],
            query_aliases: std::collections::BTreeMap::new(),
        };

        let descriptors = vec![p1, p2];
        let plan_res = PackPlan::resolve(&descriptors);
        assert!(plan_res.is_err());
        assert!(plan_res.unwrap_err().to_string().contains("Cycle detected"));
    }

    #[test]
    fn test_maps_serialization() {
        let mut mappings = std::collections::HashMap::new();
        mappings.insert(
            std::path::PathBuf::from("src/main.rs"),
            ProjectionMapping {
                pack_id: "pack_1".to_string(),
                template_path: std::path::PathBuf::from("templates/main.tmpl"),
                query_path: None,
                bound_variables: vec!["var1".to_string()],
                merge_strategy: "Exclusive".to_string(),
                start_line: None,
                end_line: None,
            },
        );
        let map = ProjectionMap { mappings };
        let serialized = serde_json::to_string(&map).unwrap();
        let deserialized: ProjectionMap = serde_json::from_str(&serialized).unwrap();
        assert_eq!(map, deserialized);
    }
}
