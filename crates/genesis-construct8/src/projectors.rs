use crate::models::{Construct8Packet, ReplayBundle, SymbolTable};
use crate::receipt::Receipt;

/// Custom error indicating a projection failure.
#[derive(Debug, thiserror::Error)]
pub enum ProjectorError {
    #[error("Symbol handle {0} not found in symbol table")]
    SymbolNotFound(u32),
    #[error("Serialization error: {0}")]
    Serialization(String),
    #[error("JSON error: {0}")]
    Json(#[from] serde_json::Error),
}

/// Project packets into N-Quads format.
pub fn project_nquads(
    packets: &[Construct8Packet],
    symbols: &SymbolTable,
) -> Result<String, ProjectorError> {
    let mut out = String::new();

    for packet in packets {
        for i in 0..8 {
            if (packet.emit_mask & (1 << i)) != 0 {
                let s_id = packet.subjects[i];
                let p_id = packet.predicates[i];
                let o_id = packet.objects[i];

                let s = symbols
                    .lookup(s_id)
                    .ok_or(ProjectorError::SymbolNotFound(s_id))?;
                let p = symbols
                    .lookup(p_id)
                    .ok_or(ProjectorError::SymbolNotFound(p_id))?;
                let o = symbols
                    .lookup(o_id)
                    .ok_or(ProjectorError::SymbolNotFound(o_id))?;

                let s_norm = if s.starts_with('_') || s.starts_with('<') {
                    s
                } else {
                    format!("<{}>", s)
                };
                let p_norm = if p.starts_with('<') {
                    p
                } else {
                    format!("<{}>", p)
                };
                let o_norm = if o.starts_with('_') || o.starts_with('<') || o.starts_with('"') {
                    o
                } else {
                    if o.starts_with("http://") || o.starts_with("https://") {
                        format!("<{}>", o)
                    } else {
                        format!("\"{}\"", o)
                    }
                };

                out.push_str(&format!("{} {} {} .\n", s_norm, p_norm, o_norm));
            }
        }
    }

    Ok(out)
}

/// Project packets into Turtle format (grouped subject-predicate-object structure).
pub fn project_turtle(
    packets: &[Construct8Packet],
    symbols: &SymbolTable,
) -> Result<String, ProjectorError> {
    let mut triples = Vec::new();

    for packet in packets {
        for i in 0..8 {
            if (packet.emit_mask & (1 << i)) != 0 {
                let s_id = packet.subjects[i];
                let p_id = packet.predicates[i];
                let o_id = packet.objects[i];

                let s = symbols
                    .lookup(s_id)
                    .ok_or(ProjectorError::SymbolNotFound(s_id))?;
                let p = symbols
                    .lookup(p_id)
                    .ok_or(ProjectorError::SymbolNotFound(p_id))?;
                let o = symbols
                    .lookup(o_id)
                    .ok_or(ProjectorError::SymbolNotFound(o_id))?;

                let s_norm = if s.starts_with('_') || s.starts_with('<') {
                    s
                } else {
                    format!("<{}>", s)
                };
                let p_norm = if p.starts_with('<') {
                    p
                } else {
                    format!("<{}>", p)
                };
                let o_norm = if o.starts_with('_') || o.starts_with('<') || o.starts_with('"') {
                    o
                } else {
                    if o.starts_with("http://") || o.starts_with("https://") {
                        format!("<{}>", o)
                    } else {
                        format!("\"{}\"", o)
                    }
                };

                triples.push((s_norm, p_norm, o_norm));
            }
        }
    }

    // Group by Subject -> Predicate -> Objects
    let mut grouped: std::collections::BTreeMap<
        String,
        std::collections::BTreeMap<String, std::collections::BTreeSet<String>>,
    > = std::collections::BTreeMap::new();
    for (s, p, o) in triples {
        grouped
            .entry(s)
            .or_default()
            .entry(p)
            .or_default()
            .insert(o);
    }

    let mut out = String::new();
    for (s, predicates) in grouped {
        out.push_str(&format!("{} \n", s));
        let pred_list: Vec<String> = predicates
            .into_iter()
            .map(|(p, objects)| {
                let objs: Vec<String> = objects.into_iter().collect();
                format!("    {} {}", p, objs.join(" , "))
            })
            .collect();
        out.push_str(&format!("{} .\n\n", pred_list.join(" ;\n")));
    }

    Ok(out)
}

fn clean_uri(uri: &str) -> String {
    let s = uri.trim();
    if let Some(stripped) = s.strip_prefix('<').and_then(|s| s.strip_suffix('>')) {
        stripped.to_string()
    } else {
        s.to_string()
    }
}

fn clean_val(o: &str) -> serde_json::Value {
    let trimmed = o.trim();
    if let Some(pos) = trimmed.rfind("\"^^<") {
        if trimmed.ends_with('>') {
            let val_str = trimmed[1..pos].to_string();
            let dt = &trimmed[pos + 4..trimmed.len() - 1];
            if dt.contains("double") || dt.contains("integer") || dt.contains("decimal") {
                if let Ok(num) = val_str.parse::<f64>() {
                    if let Some(n) = serde_json::Number::from_f64(num) {
                        return serde_json::Value::Number(n);
                    }
                }
            } else if dt.contains("boolean") {
                if let Ok(b) = val_str.parse::<bool>() {
                    return serde_json::Value::Bool(b);
                }
            }
            return serde_json::Value::String(val_str);
        }
    }

    if let Some(stripped) = trimmed.strip_prefix('"').and_then(|s| s.strip_suffix('"')) {
        serde_json::Value::String(stripped.to_string())
    } else {
        serde_json::Value::String(trimmed.to_string())
    }
}

/// Project packets into reconstructed OCEL2 event log.
pub fn project_ocel2(
    packets: &[Construct8Packet],
    symbols: &SymbolTable,
) -> Result<serde_json::Value, ProjectorError> {
    let mut events = Vec::new();
    let mut objects = Vec::new();
    let mut event_types_set = std::collections::HashSet::new();
    let mut object_types_set = std::collections::HashSet::new();

    let mut triples = Vec::new();
    for packet in packets {
        for i in 0..8 {
            if (packet.emit_mask & (1 << i)) != 0 {
                let s_id = packet.subjects[i];
                let p_id = packet.predicates[i];
                let o_id = packet.objects[i];

                let s = symbols
                    .lookup(s_id)
                    .ok_or(ProjectorError::SymbolNotFound(s_id))?;
                let p = symbols
                    .lookup(p_id)
                    .ok_or(ProjectorError::SymbolNotFound(p_id))?;
                let o = symbols
                    .lookup(o_id)
                    .ok_or(ProjectorError::SymbolNotFound(o_id))?;
                triples.push((s, p, o));
            }
        }
    }

    let mut subject_triples: std::collections::HashMap<String, Vec<(String, String)>> =
        std::collections::HashMap::new();
    for (s, p, o) in triples {
        subject_triples.entry(s).or_default().push((p, o));
    }

    for (sub, preds) in subject_triples {
        let mut is_event = false;
        let mut is_object = false;
        for (p, o) in &preds {
            if p == "<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>"
                || p == "<http://www.ocel-standard.org/ns#type>"
            {
                if o == "<http://www.ocel-standard.org/ns#Event>" || o == "<urn:ocel:type:Event>" {
                    is_event = true;
                } else if o == "<http://www.ocel-standard.org/ns#Object>"
                    || o == "<urn:ocel:type:Object>"
                {
                    is_object = true;
                }
            }
        }

        let sub_clean = clean_uri(&sub);

        if is_event {
            let mut event_obj = serde_json::Map::new();
            let mut attributes = Vec::new();
            let mut relationships = Vec::new();
            let mut event_type = String::new();
            let mut event_time = String::new();

            let event_id =
                if let Some(stripped) = sub_clean.strip_prefix("http://example.org/event/") {
                    stripped.to_string()
                } else if let Some(stripped) = sub_clean.strip_prefix("urn:ocel:event:") {
                    stripped.to_string()
                } else {
                    sub_clean.clone()
                };

            for (p, o) in preds {
                let p_clean = clean_uri(&p);
                let o_clean = clean_uri(&o);

                if p == "<http://www.ocel-standard.org/ns#type>"
                    || p == "<http://www.ocel-standard.org/ns#eventType>"
                    || p == "<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>"
                {
                    let type_val = if let Some(stripped) =
                        o_clean.strip_prefix("http://example.org/event-type/")
                    {
                        stripped.to_string()
                    } else if let Some(stripped) = o_clean.strip_prefix("urn:ocel:type:") {
                        stripped.to_string()
                    } else {
                        o_clean.clone()
                    };
                    if type_val != "Event"
                        && type_val != "http://www.ocel-standard.org/ns#Event"
                        && type_val != "Object"
                        && type_val != "http://www.ocel-standard.org/ns#Object"
                    {
                        event_type = type_val;
                    }
                } else if p == "<http://www.ocel-standard.org/ns#time>" {
                    let time_val = if o.contains("^^") {
                        let parts: Vec<&str> = o.split("^^").collect();
                        let t = parts[0].trim();
                        if let Some(stripped) =
                            t.strip_prefix('"').and_then(|s| s.strip_suffix('"'))
                        {
                            stripped.to_string()
                        } else {
                            t.to_string()
                        }
                    } else {
                        let t = o.trim();
                        if let Some(stripped) =
                            t.strip_prefix('"').and_then(|s| s.strip_suffix('"'))
                        {
                            stripped.to_string()
                        } else {
                            t.to_string()
                        }
                    };
                    event_time = time_val;
                } else if let Some(qual) =
                    p_clean.strip_prefix("http://ocel-standard.org/ns#qualifier:")
                {
                    let obj_id = if let Some(stripped) = o_clean.strip_prefix("urn:ocel:object:") {
                        stripped.to_string()
                    } else if let Some(stripped) =
                        o_clean.strip_prefix("http://example.org/object/")
                    {
                        stripped.to_string()
                    } else {
                        o_clean.clone()
                    };
                    let mut rel = serde_json::Map::new();
                    rel.insert("objectId".to_string(), serde_json::Value::String(obj_id));
                    rel.insert(
                        "qualifier".to_string(),
                        serde_json::Value::String(qual.to_string()),
                    );
                    relationships.push(serde_json::Value::Object(rel));
                } else if p != "<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>" {
                    let attr_name = if let Some(stripped) =
                        p_clean.strip_prefix("http://example.org/attribute/")
                    {
                        stripped.to_string()
                    } else if let Some(stripped) =
                        p_clean.strip_prefix("http://ocel-standard.org/ns#attribute:")
                    {
                        stripped.to_string()
                    } else if let Some(pos) = p_clean.rfind('/') {
                        p_clean[pos + 1..].to_string()
                    } else {
                        p_clean.clone()
                    };
                    let mut attr = serde_json::Map::new();
                    attr.insert("name".to_string(), serde_json::Value::String(attr_name));
                    attr.insert("value".to_string(), clean_val(&o));
                    attributes.push(serde_json::Value::Object(attr));
                }
            }

            event_obj.insert(
                "id".to_string(),
                serde_json::Value::String(event_id.clone()),
            );
            if !event_type.is_empty() {
                event_obj.insert(
                    "type".to_string(),
                    serde_json::Value::String(event_type.clone()),
                );
                event_types_set.insert(event_type);
            }
            if !event_time.is_empty() {
                event_obj.insert("time".to_string(), serde_json::Value::String(event_time));
            }
            if !attributes.is_empty() {
                event_obj.insert(
                    "attributes".to_string(),
                    serde_json::Value::Array(attributes),
                );
            }
            if !relationships.is_empty() {
                event_obj.insert(
                    "relationships".to_string(),
                    serde_json::Value::Array(relationships),
                );
            }
            events.push(serde_json::Value::Object(event_obj));
        } else if is_object {
            let mut object_obj = serde_json::Map::new();
            let mut attributes = Vec::new();
            let mut relationships = Vec::new();
            let mut object_type = String::new();

            let object_id =
                if let Some(stripped) = sub_clean.strip_prefix("http://example.org/object/") {
                    stripped.to_string()
                } else if let Some(stripped) = sub_clean.strip_prefix("urn:ocel:object:") {
                    stripped.to_string()
                } else {
                    sub_clean.clone()
                };

            for (p, o) in preds {
                let p_clean = clean_uri(&p);
                let o_clean = clean_uri(&o);

                if p == "<http://www.ocel-standard.org/ns#type>"
                    || p == "<http://www.ocel-standard.org/ns#objectType>"
                    || p == "<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>"
                {
                    let type_val = if let Some(stripped) =
                        o_clean.strip_prefix("http://example.org/object-type/")
                    {
                        stripped.to_string()
                    } else if let Some(stripped) = o_clean.strip_prefix("urn:ocel:type:") {
                        stripped.to_string()
                    } else {
                        o_clean.clone()
                    };
                    if type_val != "Event"
                        && type_val != "http://www.ocel-standard.org/ns#Event"
                        && type_val != "Object"
                        && type_val != "http://www.ocel-standard.org/ns#Object"
                    {
                        object_type = type_val;
                    }
                } else if let Some(qual) =
                    p_clean.strip_prefix("http://ocel-standard.org/ns#qualifier:")
                {
                    let rel_id = if let Some(stripped) = o_clean.strip_prefix("urn:ocel:object:") {
                        stripped.to_string()
                    } else if let Some(stripped) =
                        o_clean.strip_prefix("http://example.org/object/")
                    {
                        stripped.to_string()
                    } else {
                        o_clean.clone()
                    };
                    let mut rel = serde_json::Map::new();
                    rel.insert("objectId".to_string(), serde_json::Value::String(rel_id));
                    rel.insert(
                        "qualifier".to_string(),
                        serde_json::Value::String(qual.to_string()),
                    );
                    relationships.push(serde_json::Value::Object(rel));
                } else if p != "<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>" {
                    let attr_name = if let Some(stripped) =
                        p_clean.strip_prefix("http://example.org/attribute/")
                    {
                        stripped.to_string()
                    } else if let Some(stripped) =
                        p_clean.strip_prefix("http://ocel-standard.org/ns#attribute:")
                    {
                        stripped.to_string()
                    } else if let Some(pos) = p_clean.rfind('/') {
                        p_clean[pos + 1..].to_string()
                    } else {
                        p_clean.clone()
                    };
                    let mut attr = serde_json::Map::new();
                    attr.insert("name".to_string(), serde_json::Value::String(attr_name));
                    attr.insert("value".to_string(), clean_val(&o));
                    attributes.push(serde_json::Value::Object(attr));
                }
            }

            object_obj.insert("id".to_string(), serde_json::Value::String(object_id));
            if !object_type.is_empty() {
                object_obj.insert(
                    "type".to_string(),
                    serde_json::Value::String(object_type.clone()),
                );
                object_types_set.insert(object_type);
            }
            if !attributes.is_empty() {
                object_obj.insert(
                    "attributes".to_string(),
                    serde_json::Value::Array(attributes),
                );
            }
            if !relationships.is_empty() {
                object_obj.insert(
                    "relationships".to_string(),
                    serde_json::Value::Array(relationships),
                );
            }
            objects.push(serde_json::Value::Object(object_obj));
        }
    }

    let mut root = serde_json::Map::new();
    let mut event_types = Vec::new();
    let mut object_types = Vec::new();
    for et in event_types_set {
        event_types.push(serde_json::Value::String(et));
    }
    for ot in object_types_set {
        object_types.push(serde_json::Value::String(ot));
    }
    if !event_types.is_empty() {
        root.insert(
            "eventTypes".to_string(),
            serde_json::Value::Array(event_types),
        );
    }
    if !object_types.is_empty() {
        root.insert(
            "objectTypes".to_string(),
            serde_json::Value::Array(object_types),
        );
    }
    root.insert("events".to_string(), serde_json::Value::Array(events));
    root.insert("objects".to_string(), serde_json::Value::Array(objects));
    Ok(serde_json::Value::Object(root))
}

/// Project receipt hash metadata into receipt representation.
pub fn project_receipt(
    packets: &[Construct8Packet],
    hash: blake3::Hash,
    triple_count: usize,
) -> Receipt {
    Receipt::new(hash, packets.len(), triple_count)
}

/// Project packets and symbol tables into a Replay Bundle.
pub fn project_replay_bundle(packets: &[Construct8Packet], symbols: &SymbolTable) -> ReplayBundle {
    ReplayBundle {
        symbols: symbols.get_all_symbols(),
        packets: packets.to_vec(),
    }
}

/// Project validation results into SHACL validation report format (Turtle).
pub fn project_shacl_report(packets: &[Construct8Packet], symbols: &SymbolTable) -> String {
    let mut conforms = true;
    let mut results = Vec::new();

    for (p_idx, packet) in packets.iter().enumerate() {
        for i in 0..8 {
            let is_valid = (packet.emit_mask & (1 << i)) != 0;

            let s_id = packet.subjects[i];
            let p_id = packet.predicates[i];
            let o_id = packet.objects[i];

            if is_valid {
                let s_str = symbols.lookup(s_id);
                let p_str = symbols.lookup(p_id);
                let o_str = symbols.lookup(o_id);

                match s_str {
                    None => {
                        conforms = false;
                        results.push(format!(
                            "_:res_{} a sh:ValidationResult ;\n\
                             sh:resultSeverity sh:Violation ;\n\
                             sh:focusNode _:packet_{}_lane_{} ;\n\
                             sh:resultPath <http://www.w3.org/1999/02/22-rdf-syntax-ns#subject> ;\n\
                             sh:value {} ;\n\
                             sh:message \"Subject handle {} does not exist in symbol table\" .",
                            results.len(),
                            p_idx,
                            i,
                            s_id,
                            s_id
                        ));
                    }
                    Some(s) => {
                        if !s.starts_with('<') && !s.starts_with('_') {
                            conforms = false;
                            results.push(format!(
                                "_:res_{} a sh:ValidationResult ;\n\
                                 sh:resultSeverity sh:Violation ;\n\
                                 sh:focusNode _:packet_{}_lane_{} ;\n\
                                 sh:resultPath <http://www.w3.org/1999/02/22-rdf-syntax-ns#subject> ;\n\
                                 sh:value \"{}\" ;\n\
                                 sh:message \"Subject is not a valid IRI or blank node\" .",
                                results.len(), p_idx, i, s.escape_debug()
                            ));
                        }
                    }
                }

                match p_str {
                    None => {
                        conforms = false;
                        results.push(format!(
                            "_:res_{} a sh:ValidationResult ;\n\
                             sh:resultSeverity sh:Violation ;\n\
                             sh:focusNode _:packet_{}_lane_{} ;\n\
                             sh:resultPath <http://www.w3.org/1999/02/22-rdf-syntax-ns#predicate> ;\n\
                             sh:value {} ;\n\
                             sh:message \"Predicate handle {} does not exist in symbol table\" .",
                            results.len(), p_idx, i, p_id, p_id
                        ));
                    }
                    Some(p) => {
                        if !p.starts_with('<') {
                            conforms = false;
                            results.push(format!(
                                "_:res_{} a sh:ValidationResult ;\n\
                                 sh:resultSeverity sh:Violation ;\n\
                                 sh:focusNode _:packet_{}_lane_{} ;\n\
                                 sh:resultPath <http://www.w3.org/1999/02/22-rdf-syntax-ns#predicate> ;\n\
                                 sh:value \"{}\" ;\n\
                                 sh:message \"Predicate is not a valid IRI\" .",
                                results.len(), p_idx, i, p.escape_debug()
                            ));
                        }
                    }
                }

                if o_str.is_none() {
                    conforms = false;
                    results.push(format!(
                        "_:res_{} a sh:ValidationResult ;\n\
                         sh:resultSeverity sh:Violation ;\n\
                         sh:focusNode _:packet_{}_lane_{} ;\n\
                         sh:resultPath <http://www.w3.org/1999/02/22-rdf-syntax-ns#object> ;\n\
                         sh:value {} ;\n\
                         sh:message \"Object handle {} does not exist in symbol table\" .",
                        results.len(),
                        p_idx,
                        i,
                        o_id,
                        o_id
                    ));
                }
            } else {
                if s_id != 0 || p_id != 0 || o_id != 0 {
                    conforms = false;
                    results.push(format!(
                        "_:res_{} a sh:ValidationResult ;\n\
                         sh:resultSeverity sh:Violation ;\n\
                         sh:focusNode _:packet_{}_lane_{} ;\n\
                         sh:message \"Inactive lane contains non-zero handles (subject={}, predicate={}, object={})\" .",
                        results.len(), p_idx, i, s_id, p_id, o_id
                    ));
                }
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
    use crate::adapters::parse_input_to_packets;
    use crate::models::{Construct8Packet, SymbolTable};

    #[test]
    fn test_project_nquads_success() {
        let symbols = SymbolTable::new();
        let input = b"<http://example.org/s> <http://example.org/p> <http://example.org/o> .\n";
        let packets = parse_input_to_packets(input, "nquads", &symbols).unwrap();
        let projected = project_nquads(&packets, &symbols).unwrap();
        assert_eq!(
            projected,
            "<http://example.org/s> <http://example.org/p> <http://example.org/o> .\n"
        );
    }

    #[test]
    fn test_project_turtle_success() {
        let symbols = SymbolTable::new();
        let input = b"<http://example.org/s> <http://example.org/p> <http://example.org/o> .\n";
        let packets = parse_input_to_packets(input, "nquads", &symbols).unwrap();
        let projected = project_turtle(&packets, &symbols).unwrap();
        assert!(projected.contains("<http://example.org/s>"));
        assert!(projected.contains("<http://example.org/p> <http://example.org/o>"));
    }

    #[test]
    fn test_project_ocel2_success() {
        let symbols = SymbolTable::new();
        let ocel2_data = b"{
            \"events\": {
                \"e1\": {
                    \"ocel:type\": \"order\",
                    \"ocel:time\": \"2026-05-27T00:00:00Z\",
                    \"ocel:o_map\": [\"o1\"],
                    \"ocel:v_map\": {
                        \"price\": 99.9
                    }
                }
            },
            \"objects\": {
                \"o1\": {
                    \"ocel:type\": \"item\",
                    \"ocel:v_map\": {
                        \"color\": \"red\"
                    }
                }
            }
        }";
        let packets = parse_input_to_packets(ocel2_data, "ocel2", &symbols).unwrap();
        let projected = project_ocel2(&packets, &symbols).unwrap();

        let events = projected.get("events").unwrap().as_array().unwrap();
        assert!(!events.is_empty());
        let e1 = events
            .iter()
            .find(|e| e.get("id").unwrap().as_str().unwrap() == "e1")
            .unwrap()
            .as_object()
            .unwrap();
        assert_eq!(e1.get("type").unwrap().as_str().unwrap(), "order");
        assert_eq!(
            e1.get("time").unwrap().as_str().unwrap(),
            "2026-05-27T00:00:00Z"
        );

        let objects = projected.get("objects").unwrap().as_array().unwrap();
        assert!(!objects.is_empty());
        let o1 = objects
            .iter()
            .find(|o| o.get("id").unwrap().as_str().unwrap() == "o1")
            .unwrap()
            .as_object()
            .unwrap();
        assert_eq!(o1.get("type").unwrap().as_str().unwrap(), "item");
    }

    #[test]
    fn test_project_receipt_success() {
        let packets = vec![Construct8Packet::new()];
        let hash = blake3::hash(b"dummy");
        let receipt = project_receipt(&packets, hash, 0);
        assert_eq!(receipt.hash, hash.to_hex().to_string());
        assert_eq!(receipt.packet_count, 1);
        assert_eq!(receipt.triple_count, 0);
    }

    #[test]
    fn test_project_replay_bundle_success() {
        let symbols = SymbolTable::new();
        let s_id = symbols.get_or_insert("s");
        let p_id = symbols.get_or_insert("p");
        let o_id = symbols.get_or_insert("o");
        let mut packet = Construct8Packet::new();
        packet.push(s_id, p_id, o_id).unwrap();
        let packets = vec![packet];
        let bundle = project_replay_bundle(&packets, &symbols);
        assert_eq!(bundle.packets.len(), 1);
        assert_eq!(bundle.symbols.get(&s_id).unwrap(), "s");
    }

    #[test]
    fn test_project_shacl_report_conformance() {
        let symbols = SymbolTable::new();
        let s_id = symbols.get_or_insert("<http://example.org/s>");
        let p_id = symbols.get_or_insert("<http://example.org/p>");
        let o_id = symbols.get_or_insert("<http://example.org/o>");
        let mut packet = Construct8Packet::new();
        packet.push(s_id, p_id, o_id).unwrap();
        let report = project_shacl_report(&[packet], &symbols);
        assert!(report.contains("sh:conforms true"));
    }

    #[test]
    fn test_project_shacl_report_violation() {
        let symbols = SymbolTable::new();
        let mut packet = Construct8Packet::new();
        // Inactive lanes have non-zero values (violation of structural invariants)
        packet.subjects[0] = 999;
        packet.predicates[0] = 999;
        packet.objects[0] = 999;
        let report = project_shacl_report(&[packet], &symbols);
        assert!(report.contains("sh:conforms false"));
        assert!(report.contains("sh:ValidationResult"));
    }
}
