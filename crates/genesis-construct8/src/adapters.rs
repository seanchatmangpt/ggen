// Adapters for Construct8
use crate::models::{materialize_block, BlockArtifact, Construct8Packet, SymbolTable};
use crate::stream::Construct8StreamExt;
use rio_api::model::{NamedNode, Subject, Term};
use rio_api::parser::{QuadsParser, TriplesParser};
use rio_turtle::{NQuadsParser, TurtleParser};

/// Simulates a Row from SQL or Parquet
pub struct Row {
    pub id: u32,
    pub columns: Vec<(u32, u32)>, // (Predicate, Object)
}

/// SQL/Parquet to Construct8 Adapter
pub struct SqlParquetAdapter<'a, I> {
    iter: I,
    current_row: Option<(u32, std::vec::IntoIter<(u32, u32)>)>,
    symbols: &'a SymbolTable,
}

impl<'a, I> SqlParquetAdapter<'a, I>
where
    I: Iterator<Item = Row>,
{
    pub fn new(iter: I, symbols: &'a SymbolTable) -> Self {
        Self {
            iter,
            current_row: None,
            symbols,
        }
    }
}

impl<'a, I> Iterator for SqlParquetAdapter<'a, I>
where
    I: Iterator<Item = Row>,
{
    type Item = (u32, u32, u32);

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            if let Some((subject, cols)) = self.current_row.as_mut() {
                let subject_val = *subject;
                if let Some((predicate, object)) = cols.next() {
                    for &id in &[subject_val, predicate, object] {
                        if self.symbols.lookup(id).is_none() {
                            self.symbols
                                .insert_custom(id, &format!("<http://example.org/id/{}>", id));
                        }
                    }
                    return Some((subject_val, predicate, object));
                }
            }

            if let Some(row) = self.iter.next() {
                self.current_row = Some((row.id, row.columns.into_iter()));
            } else {
                return None;
            }
        }
    }
}

/// Simulates a Log Entry
pub struct LogEntry {
    pub timestamp: u32, // Object for timestamp predicate
    pub source_id: u32, // Subject
    pub event_id: u32,  // Object for event predicate
    pub severity: u32,  // Object for severity predicate
}

/// Log to Construct8 Adapter
pub struct LogAdapter<'a, I> {
    iter: I,
    current_log: Option<std::vec::IntoIter<(u32, u32, u32)>>,
    symbols: &'a SymbolTable,
}

impl<'a, I> LogAdapter<'a, I>
where
    I: Iterator<Item = LogEntry>,
{
    pub fn new(iter: I, symbols: &'a SymbolTable) -> Self {
        Self {
            iter,
            current_log: None,
            symbols,
        }
    }
}

impl<'a, I> Iterator for LogAdapter<'a, I>
where
    I: Iterator<Item = LogEntry>,
{
    type Item = (u32, u32, u32);

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            if let Some(ref mut triples) = self.current_log {
                if let Some(triple) = triples.next() {
                    for &id in &[triple.0, triple.1, triple.2] {
                        if self.symbols.lookup(id).is_none() {
                            self.symbols
                                .insert_custom(id, &format!("<http://example.org/id/{}>", id));
                        }
                    }
                    return Some(triple);
                }
            }

            if let Some(log) = self.iter.next() {
                // Hardcoded predicates: 1 = timestamp, 2 = event_id, 3 = severity
                let triples = vec![
                    (log.source_id, 1, log.timestamp),
                    (log.source_id, 2, log.event_id),
                    (log.source_id, 3, log.severity),
                ];
                self.current_log = Some(triples.into_iter());
            } else {
                return None;
            }
        }
    }
}

// ==========================================
// Parsing Logic for Turtle, N-Quads, CSV, JSON, OCEL2
// ==========================================

fn validate_rdf_subject(s: &str) -> Result<(), String> {
    let s_trimmed = s.trim();
    if s_trimmed.starts_with('"') {
        return Err(format!("Subject cannot be a literal: {}", s_trimmed));
    }
    if !s_trimmed.starts_with('<') && !s_trimmed.starts_with('_') && !s_trimmed.contains("://") {
        return Err(format!(
            "Subject must be a URI or blank node: {}",
            s_trimmed
        ));
    }
    Ok(())
}

fn validate_rdf_predicate(p: &str) -> Result<(), String> {
    let p_trimmed = p.trim();
    if p_trimmed.starts_with('"') || p_trimmed.starts_with('_') {
        return Err(format!("Predicate must be an IRI: {}", p_trimmed));
    }
    Ok(())
}

fn format_named_node(node: NamedNode<'_>) -> String {
    format!("<{}>", node.iri)
}

fn format_subject(sub: Subject<'_>) -> String {
    match sub {
        Subject::NamedNode(n) => format_named_node(n),
        Subject::BlankNode(b) => format!("_:{}", b.id),
        Subject::Triple(_t) => "<<unknown_rdf_star_subject>>".to_string(),
    }
}

fn format_term(term: Term<'_>) -> String {
    match term {
        Term::NamedNode(n) => format_named_node(n),
        Term::BlankNode(b) => format!("_:{}", b.id),
        Term::Literal(l) => match l {
            rio_api::model::Literal::Simple { value } => {
                format!("\"{}\"", value.escape_debug())
            }
            rio_api::model::Literal::LanguageTaggedString { value, language } => {
                format!("\"{}\"@{}", value.escape_debug(), language)
            }
            rio_api::model::Literal::Typed { value, datatype } => {
                format!("\"{}\"^^<{}>", value.escape_debug(), datatype.iri)
            }
        },
        Term::Triple(_t) => "<<unknown_rdf_star_term>>".to_string(),
    }
}

pub fn parse_turtle(raw_input: &[u8]) -> Result<Vec<(String, String, String)>, String> {
    let mut triples = Vec::new();
    let mut parser = TurtleParser::new(raw_input, None);
    let parse_res: Result<(), rio_turtle::TurtleError> = parser.parse_all(&mut |t| {
        triples.push((
            format_subject(t.subject),
            format_named_node(t.predicate),
            format_term(t.object),
        ));
        Ok(())
    });
    parse_res
        .map(|_| triples)
        .map_err(|e| format!("Turtle parsing error: {}", e))
}

pub fn parse_nquads(raw_input: &[u8]) -> Result<Vec<(String, String, String)>, String> {
    let mut triples = Vec::new();
    let mut parser = NQuadsParser::new(raw_input);
    let parse_res: Result<(), rio_turtle::TurtleError> = parser.parse_all(&mut |q| {
        triples.push((
            format_subject(q.subject),
            format_named_node(q.predicate),
            format_term(q.object),
        ));
        Ok(())
    });
    parse_res
        .map(|_| triples)
        .map_err(|e| format!("N-Quads parsing error: {}", e))
}

fn parse_csv_row(row: &str) -> Result<Vec<String>, String> {
    let mut cols = Vec::new();
    let mut chars = row.chars().peekable();

    while chars.peek().is_some() {
        while let Some(&wc) = chars.peek() {
            if wc.is_whitespace() {
                chars.next();
            } else {
                break;
            }
        }
        if chars.peek().is_none() {
            break;
        }

        let &c = chars.peek().unwrap();
        if c == ',' {
            cols.push(String::new());
            chars.next();
        } else if c == '"' {
            chars.next();
            let mut col = String::new();
            let mut closed = false;
            while let Some(nc) = chars.next() {
                if nc == '"' {
                    if let Some(&peek_c) = chars.peek() {
                        if peek_c == '"' {
                            col.push('"');
                            chars.next();
                            continue;
                        }
                    }
                    closed = true;
                    break;
                } else {
                    col.push(nc);
                }
            }
            if !closed {
                return Err("Malformed CSV: unclosed double quote".to_string());
            }
            cols.push(col);

            if let Some(&nc) = chars.peek() {
                if nc == ',' {
                    chars.next();
                } else if nc.is_whitespace() {
                    while let Some(&wc) = chars.peek() {
                        if wc == ',' {
                            chars.next();
                            break;
                        } else if wc.is_whitespace() {
                            chars.next();
                        } else {
                            return Err(format!(
                                "Malformed CSV: unexpected character after closing quote: {}",
                                wc
                            ));
                        }
                    }
                } else {
                    return Err(format!(
                        "Malformed CSV: unexpected character after closing quote: {}",
                        nc
                    ));
                }
            }
        } else {
            let mut col = String::new();
            while let Some(&nc) = chars.peek() {
                if nc == ',' {
                    break;
                } else {
                    col.push(nc);
                    chars.next();
                }
            }
            cols.push(col.trim().to_string());
            if let Some(&nc) = chars.peek() {
                if nc == ',' {
                    chars.next();
                }
            }
        }
    }

    Ok(cols)
}

fn normalize_csv_field(val: &str) -> String {
    let trimmed = val.trim();
    if trimmed.starts_with('"') && trimmed.ends_with('"') && trimmed.len() >= 2 {
        let inner = &trimmed[1..trimmed.len() - 1];
        if inner.starts_with('<') || inner.starts_with('_') {
            inner.to_string()
        } else {
            trimmed.to_string()
        }
    } else if trimmed.starts_with('<') || trimmed.starts_with('_') {
        trimmed.to_string()
    } else if trimmed.starts_with("http://") || trimmed.starts_with("https://") {
        format!("<{}>", trimmed)
    } else {
        format!("\"{}\"", trimmed)
    }
}

pub fn parse_csv(raw_input: &[u8]) -> Result<Vec<(String, String, String)>, String> {
    let content =
        std::str::from_utf8(raw_input).map_err(|e| format!("Invalid UTF-8 sequence: {}", e))?;
    if content.trim().is_empty() {
        return Ok(Vec::new());
    }
    let mut lines = content.lines().enumerate();
    let mut triples = Vec::new();

    let mut sub_idx = 0;
    let mut pred_idx = 1;
    let mut obj_idx = 2;

    if let Some((_, first_line)) = lines.next() {
        let cols = parse_csv_row(first_line)?;
        if cols.len() < 3 {
            return Err("CSV input must contain at least 3 columns".to_string());
        }
        let has_headers = cols.iter().any(|c| {
            let lower = c.to_lowercase();
            lower == "subject" || lower == "predicate" || lower == "object"
        });

        if has_headers {
            let mut s_found = false;
            let mut p_found = false;
            let mut o_found = false;
            for (idx, col) in cols.iter().enumerate() {
                let lower = col.to_lowercase();
                if lower == "subject" {
                    sub_idx = idx;
                    s_found = true;
                } else if lower == "predicate" {
                    pred_idx = idx;
                    p_found = true;
                } else if lower == "object" {
                    obj_idx = idx;
                    o_found = true;
                }
            }
            if !s_found || !p_found || !o_found {
                return Err(
                    "CSV header must contain 'subject', 'predicate', and 'object'".to_string(),
                );
            }
        } else {
            let s_raw = &cols[sub_idx];
            let p_raw = &cols[pred_idx];
            validate_rdf_subject(s_raw)?;
            validate_rdf_predicate(p_raw)?;
            triples.push((s_raw.clone(), p_raw.clone(), cols[obj_idx].clone()));
        }
    } else {
        return Ok(Vec::new());
    }

    for (line_idx, line) in lines {
        let trimmed = line.trim();
        if trimmed.is_empty() {
            continue;
        }
        let cols = parse_csv_row(trimmed)?;
        let max_idx = std::cmp::max(std::cmp::max(sub_idx, pred_idx), obj_idx);
        if cols.len() <= max_idx {
            return Err(format!(
                "Line {}: CSV row does not have enough columns (expected at least {})",
                line_idx + 1,
                max_idx + 1
            ));
        }
        let s_raw = &cols[sub_idx];
        let p_raw = &cols[pred_idx];
        validate_rdf_subject(s_raw)?;
        validate_rdf_predicate(p_raw)?;
        triples.push((s_raw.clone(), p_raw.clone(), cols[obj_idx].clone()));
    }

    // Apply normalization to CSV fields
    let mut normalized_triples = Vec::new();
    for (s, p, o) in triples {
        let s_norm = normalize_csv_field(&s);
        let p_norm = normalize_csv_field(&p);
        let o_norm = normalize_csv_field(&o);
        normalized_triples.push((s_norm, p_norm, o_norm));
    }

    Ok(normalized_triples)
}

fn format_iri(id: &str, namespace: &str) -> String {
    if id.starts_with('<') && id.ends_with('>') {
        id.to_string()
    } else {
        format!("<{}{}>", namespace, id)
    }
}

fn ocel_val_to_string(val: &serde_json::Value) -> String {
    match val {
        serde_json::Value::String(s) => format!("\"{}\"", s),
        serde_json::Value::Number(n) => {
            format!("\"{}\"^^<http://www.w3.org/2001/XMLSchema#double>", n)
        }
        serde_json::Value::Bool(b) => {
            format!("\"{}\"^^<http://www.w3.org/2001/XMLSchema#boolean>", b)
        }
        _ => format!("\"{}\"", val),
    }
}

fn parse_event_node(
    event: &serde_json::Value,
    event_id: &str,
    triples: &mut Vec<(String, String, String)>,
    ocel_ns: &str,
) {
    let event_subject = format!(
        "{}>",
        format_iri(event_id, "http://example.org/event/").trim_end_matches('>')
    );

    triples.push((
        event_subject.clone(),
        format!("{}type>", ocel_ns),
        format!("{}Event>", ocel_ns),
    ));

    if let Some(e_type) = event
        .get("type")
        .or_else(|| event.get("ocel:type"))
        .and_then(|v| v.as_str())
    {
        triples.push((
            event_subject.clone(),
            format!("{}eventType>", ocel_ns),
            format_iri(e_type, "http://example.org/event-type/"),
        ));
    }

    if let Some(e_time) = event
        .get("time")
        .or_else(|| event.get("ocel:time"))
        .and_then(|v| v.as_str())
    {
        triples.push((
            event_subject.clone(),
            format!("{}time>", ocel_ns),
            format!(
                "\"{}\"^^<http://www.w3.org/2001/XMLSchema#dateTime>",
                e_time
            ),
        ));
    }

    if let Some(attrs) = event.get("attributes").and_then(|v| v.as_array()) {
        for attr in attrs {
            if let (Some(attr_name), Some(attr_val)) =
                (attr.get("name").and_then(|v| v.as_str()), attr.get("value"))
            {
                let pred = format!("{}attribute/{}>", ocel_ns, attr_name);
                let obj = ocel_val_to_string(attr_val);
                triples.push((event_subject.clone(), pred, obj));
            }
        }
    }

    if let Some(v_map) = event.get("ocel:v_map").and_then(|v| v.as_object()) {
        for (attr_name, attr_val) in v_map {
            let pred = format!("{}attribute/{}>", ocel_ns, attr_name);
            let obj = ocel_val_to_string(attr_val);
            triples.push((event_subject.clone(), pred, obj));
        }
    }

    if let Some(rels) = event.get("relationships").and_then(|v| v.as_array()) {
        for rel in rels {
            if let Some(obj_id) = rel.get("objectId").and_then(|v| v.as_str()) {
                let qual = rel
                    .get("qualifier")
                    .and_then(|v| v.as_str())
                    .unwrap_or("relatesTo");
                triples.push((
                    event_subject.clone(),
                    format!("{}qualifier:{}>", ocel_ns, qual),
                    format_iri(obj_id, "http://example.org/object/"),
                ));
            }
        }
    }

    if let Some(o_map) = event.get("ocel:o_map").and_then(|v| v.as_array()) {
        for obj_val in o_map {
            if let Some(obj_id) = obj_val.as_str() {
                triples.push((
                    event_subject.clone(),
                    format!("{}qualifier:relatesTo>", ocel_ns),
                    format_iri(obj_id, "http://example.org/object/"),
                ));
            }
        }
    }
}

fn parse_object_node(
    object: &serde_json::Value,
    object_id: &str,
    triples: &mut Vec<(String, String, String)>,
    ocel_ns: &str,
) {
    let object_subject = format!(
        "{}>",
        format_iri(object_id, "http://example.org/object/").trim_end_matches('>')
    );

    triples.push((
        object_subject.clone(),
        format!("{}type>", ocel_ns),
        format!("{}Object>", ocel_ns),
    ));

    if let Some(o_type) = object
        .get("type")
        .or_else(|| object.get("ocel:type"))
        .and_then(|v| v.as_str())
    {
        triples.push((
            object_subject.clone(),
            format!("{}objectType>", ocel_ns),
            format_iri(o_type, "http://example.org/object-type/"),
        ));
    }

    if let Some(attrs) = object.get("attributes").and_then(|v| v.as_array()) {
        for attr in attrs {
            if let (Some(attr_name), Some(attr_val)) =
                (attr.get("name").and_then(|v| v.as_str()), attr.get("value"))
            {
                let pred = format!("{}attribute/{}>", ocel_ns, attr_name);
                let obj = ocel_val_to_string(attr_val);
                triples.push((object_subject.clone(), pred, obj));
            }
        }
    }

    if let Some(v_map) = object.get("ocel:v_map").and_then(|v| v.as_object()) {
        for (attr_name, attr_val) in v_map {
            let pred = format!("{}attribute/{}>", ocel_ns, attr_name);
            let obj = ocel_val_to_string(attr_val);
            triples.push((object_subject.clone(), pred, obj));
        }
    }

    if let Some(rels) = object.get("relationships").and_then(|v| v.as_array()) {
        for rel in rels {
            if let Some(rel_id) = rel.get("objectId").and_then(|v| v.as_str()) {
                let qual = rel
                    .get("qualifier")
                    .and_then(|v| v.as_str())
                    .unwrap_or("relatesTo");
                triples.push((
                    object_subject.clone(),
                    format!("{}qualifier:{}>", ocel_ns, qual),
                    format_iri(rel_id, "http://example.org/object/"),
                ));
            }
        }
    }
}

pub fn parse_ocel2_json(
    value: &serde_json::Value,
) -> Result<Vec<(String, String, String)>, String> {
    let mut triples = Vec::new();
    let ocel_ns = "<http://www.ocel-standard.org/ns#";

    if let Some(events_val) = value.get("events") {
        if let Some(arr) = events_val.as_array() {
            for event in arr {
                if let Some(event_id) = event.get("id").and_then(|v| v.as_str()) {
                    parse_event_node(event, event_id, &mut triples, ocel_ns);
                }
            }
        } else if let Some(map) = events_val.as_object() {
            for (event_id, event) in map {
                parse_event_node(event, event_id, &mut triples, ocel_ns);
            }
        }
    }

    if let Some(objects_val) = value.get("objects") {
        if let Some(arr) = objects_val.as_array() {
            for object in arr {
                if let Some(object_id) = object.get("id").and_then(|v| v.as_str()) {
                    parse_object_node(object, object_id, &mut triples, ocel_ns);
                }
            }
        } else if let Some(map) = objects_val.as_object() {
            for (object_id, object) in map {
                parse_object_node(object, object_id, &mut triples, ocel_ns);
            }
        }
    }

    Ok(triples)
}

pub fn parse_input_to_packets(
    raw_input: &[u8],
    format: &str,
    symbols: &SymbolTable,
) -> Result<Vec<Construct8Packet>, BlockArtifact> {
    if raw_input.is_empty()
        || std::str::from_utf8(raw_input)
            .map(|s| s.trim().is_empty())
            .unwrap_or(false)
    {
        return Ok(Vec::new());
    }

    let triples_res = match format.to_lowercase().as_str() {
        "turtle" | "ttl" => parse_turtle(raw_input),
        "nquads" | "nq" => parse_nquads(raw_input),
        "csv" => parse_csv(raw_input),
        "json" | "ocel2" => {
            if let Ok(val) = serde_json::from_slice::<serde_json::Value>(raw_input) {
                if val.get("events").is_some() || val.get("objects").is_some() {
                    parse_ocel2_json(&val)
                } else if val.is_array() {
                    let mut triples = Vec::new();
                    let mut err = None;
                    if let Some(arr) = val.as_array() {
                        for (idx, item) in arr.iter().enumerate() {
                            let s = item
                                .get("subject")
                                .or_else(|| item.get("s"))
                                .and_then(|v| v.as_str());
                            let p = item
                                .get("predicate")
                                .or_else(|| item.get("p"))
                                .and_then(|v| v.as_str());
                            let o = item
                                .get("object")
                                .or_else(|| item.get("o"))
                                .and_then(|v| v.as_str());
                            if let (Some(s_str), Some(p_str), Some(o_str)) = (s, p, o) {
                                if let Err(e) = validate_rdf_subject(s_str) {
                                    err = Some(e);
                                    break;
                                }
                                if let Err(e) = validate_rdf_predicate(p_str) {
                                    err = Some(e);
                                    break;
                                }
                                // Apply normalization to JSON fields if they aren't RDF formatted
                                let s_norm = if s_str.starts_with('_') || s_str.starts_with('<') {
                                    s_str.to_string()
                                } else {
                                    if s_str.starts_with('"') {
                                        s_str.to_string()
                                    } else {
                                        format!("<{}>", s_str)
                                    }
                                };
                                let p_norm = if p_str.starts_with('<') {
                                    p_str.to_string()
                                } else {
                                    if p_str.starts_with('"') {
                                        p_str.to_string()
                                    } else {
                                        format!("<{}>", p_str)
                                    }
                                };
                                let o_norm = if o_str.starts_with('_')
                                    || o_str.starts_with('<')
                                    || o_str.starts_with('"')
                                {
                                    o_str.to_string()
                                } else {
                                    if o_str.starts_with("http://") || o_str.starts_with("https://")
                                    {
                                        format!("<{}>", o_str)
                                    } else {
                                        format!("\"{}\"", o_str)
                                    }
                                };
                                triples.push((s_norm, p_norm, o_norm));
                            } else {
                                err = Some(format!("JSON item at index {} is missing subject/s, predicate/p, or object/o", idx));
                                break;
                            }
                        }
                    }
                    if let Some(e) = err {
                        Err(e)
                    } else {
                        Ok(triples)
                    }
                } else if let Some(additions) = val.get("additions").and_then(|v| v.as_array()) {
                    // Support additions array format
                    let mut triples = Vec::new();
                    let mut err = None;
                    for (idx, item) in additions.iter().enumerate() {
                        let s = item
                            .get("subject")
                            .or_else(|| item.get("s"))
                            .and_then(|v| v.as_str());
                        let p = item
                            .get("predicate")
                            .or_else(|| item.get("p"))
                            .and_then(|v| v.as_str());
                        let o = item
                            .get("object")
                            .or_else(|| item.get("o"))
                            .and_then(|v| v.as_str());
                        if let (Some(s_str), Some(p_str), Some(o_str)) = (s, p, o) {
                            if let Err(e) = validate_rdf_subject(s_str) {
                                err = Some(e);
                                break;
                            }
                            if let Err(e) = validate_rdf_predicate(p_str) {
                                err = Some(e);
                                break;
                            }
                            let s_norm = if s_str.starts_with('_') || s_str.starts_with('<') {
                                s_str.to_string()
                            } else {
                                if s_str.starts_with('"') {
                                    s_str.to_string()
                                } else {
                                    format!("<{}>", s_str)
                                }
                            };
                            let p_norm = if p_str.starts_with('<') {
                                p_str.to_string()
                            } else {
                                if p_str.starts_with('"') {
                                    p_str.to_string()
                                } else {
                                    format!("<{}>", p_str)
                                }
                            };
                            let o_norm = if o_str.starts_with('_')
                                || o_str.starts_with('<')
                                || o_str.starts_with('"')
                            {
                                o_str.to_string()
                            } else {
                                if o_str.starts_with("http://") || o_str.starts_with("https://") {
                                    format!("<{}>", o_str)
                                } else {
                                    format!("\"{}\"", o_str)
                                }
                            };
                            triples.push((s_norm, p_norm, o_norm));
                        } else {
                            err = Some(format!("JSON addition at index {} is missing subject/s, predicate/p, or object/o", idx));
                            break;
                        }
                    }
                    if let Some(e) = err {
                        Err(e)
                    } else {
                        Ok(triples)
                    }
                } else {
                    Err("Invalid JSON format: expected array of claims, additions, or OCEL2 log object".to_string())
                }
            } else {
                if format.to_lowercase().as_str() == "ocel2" {
                    Err("Malformed JSON syntax for OCEL2".to_string())
                } else {
                    Err("Malformed JSON syntax".to_string())
                }
            }
        }
        _ => Err(format!("Unsupported format: {}", format)),
    };

    match triples_res {
        Ok(triples) => {
            // Validate RDF formatting
            for (idx, (s, p, o)) in triples.iter().enumerate() {
                let s_iri = s.starts_with('<') && s.ends_with('>');
                let s_blank = s.starts_with("_:");
                if !s_iri && !s_blank {
                    return Err(materialize_block(
                        &format!("Item {} has invalid Subject: {}", idx + 1, s),
                        raw_input,
                    ));
                }

                let p_iri = p.starts_with('<') && p.ends_with('>');
                if !p_iri {
                    return Err(materialize_block(
                        &format!("Item {} has invalid Predicate: {}", idx + 1, p),
                        raw_input,
                    ));
                }

                let o_iri = o.starts_with('<') && o.ends_with('>');
                let o_blank = o.starts_with("_:");
                let o_literal = (o.starts_with('"') && o.ends_with('"'))
                    || (o.starts_with('"') && o.contains("\"^^<") && o.ends_with('>'));
                if !o_iri && !o_blank && !o_literal {
                    return Err(materialize_block(
                        &format!("Item {} has invalid Object: {}", idx + 1, o),
                        raw_input,
                    ));
                }
            }

            let mut resolved = Vec::new();
            for (s, p, o) in triples {
                let s_id = symbols.get_or_insert(&s);
                let p_id = symbols.get_or_insert(&p);
                let o_id = symbols.get_or_insert(&o);
                resolved.push((s_id, p_id, o_id));
            }
            Ok(resolved.into_iter().into_construct8_chunks().collect())
        }
        Err(err_msg) => Err(materialize_block(&err_msg, raw_input)),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::models::{Construct8Packet, SymbolTable};
    use crate::stream::Construct8StreamExt;

    #[test]
    fn test_sql_parquet_adapter() {
        let symbols = SymbolTable::new();
        let rows = vec![
            Row {
                id: 100,
                columns: vec![(10, 1), (11, 2)],
            },
            Row {
                id: 200,
                columns: vec![(10, 3)],
            },
        ];

        let adapter = SqlParquetAdapter::new(rows.into_iter(), &symbols);
        let chunks: Vec<Construct8Packet> = adapter.into_construct8_chunks().collect();

        assert_eq!(chunks.len(), 1);
        assert_eq!(chunks[0].len(), 3);
        assert_eq!(chunks[0].subjects[0], 100);
        assert_eq!(chunks[0].predicates[0], 10);
        assert_eq!(chunks[0].objects[0], 1);

        assert_eq!(chunks[0].subjects[2], 200);
        assert_eq!(chunks[0].predicates[2], 10);
        assert_eq!(chunks[0].objects[2], 3);
    }

    #[test]
    fn test_log_adapter() {
        let symbols = SymbolTable::new();
        let logs = vec![
            LogEntry {
                timestamp: 1000,
                source_id: 50,
                event_id: 99,
                severity: 1,
            },
            LogEntry {
                timestamp: 1001,
                source_id: 51,
                event_id: 100,
                severity: 2,
            },
            LogEntry {
                timestamp: 1002,
                source_id: 52,
                event_id: 101,
                severity: 1,
            },
        ];

        let adapter = LogAdapter::new(logs.into_iter(), &symbols);
        let chunks: Vec<Construct8Packet> = adapter.into_construct8_chunks().collect();

        // 3 logs * 3 triples each = 9 triples = 2 chunks (8 and 1)
        assert_eq!(chunks.len(), 2);
        assert_eq!(chunks[0].len(), 8);
        assert_eq!(chunks[1].len(), 1);

        assert_eq!(chunks[0].subjects[0], 50);
        assert_eq!(chunks[0].predicates[0], 1);
        assert_eq!(chunks[0].objects[0], 1000);

        assert_eq!(chunks[1].subjects[0], 52);
        assert_eq!(chunks[1].predicates[0], 3);
        assert_eq!(chunks[1].objects[0], 1);
    }

    #[test]
    fn test_parse_turtle_success() {
        let symbols = SymbolTable::new();
        let ttl_data = b"
            @prefix ex: <http://example.org/> .
            ex:subject1 ex:predicate1 ex:object1 .
            ex:subject1 ex:predicate2 ex:object2 , ex:object3 .
        ";
        let packets = parse_input_to_packets(ttl_data, "turtle", &symbols).unwrap();
        assert!(!packets.is_empty());

        // Let's count total triples across all packets
        let total_triples: usize = packets.iter().map(|p| p.len()).sum();
        assert_eq!(total_triples, 3);
    }

    #[test]
    fn test_parse_nquads_success() {
        let symbols = SymbolTable::new();
        let nq_data = b"
            <http://example.org/s1> <http://example.org/p1> <http://example.org/o1> .
            <http://example.org/s2> <http://example.org/p2> \"literal_value\" .
        ";
        let packets = parse_input_to_packets(nq_data, "nquads", &symbols).unwrap();
        assert!(!packets.is_empty());
        let total_triples: usize = packets.iter().map(|p| p.len()).sum();
        assert_eq!(total_triples, 2);
    }

    #[test]
    fn test_parse_csv_success() {
        let symbols = SymbolTable::new();
        // Headered CSV
        let csv_data = b"subject,predicate,object\nhttp://example.org/s1,http://example.org/p1,http://example.org/o1\n";
        let packets = parse_input_to_packets(csv_data, "csv", &symbols).unwrap();
        assert_eq!(packets.len(), 1);
        assert_eq!(packets[0].len(), 1);

        // Header-less CSV
        let csv_no_header = b"http://example.org/s2,http://example.org/p2,http://example.org/o2\n";
        let packets_no = parse_input_to_packets(csv_no_header, "csv", &symbols).unwrap();
        assert_eq!(packets_no.len(), 1);
        assert_eq!(packets_no[0].len(), 1);
    }

    #[test]
    fn test_parse_json_success() {
        let symbols = SymbolTable::new();
        let json_data = b"[
            {\"subject\": \"http://example.org/s1\", \"predicate\": \"http://example.org/p1\", \"object\": \"http://example.org/o1\"},
            {\"s\": \"http://example.org/s2\", \"p\": \"http://example.org/p2\", \"o\": \"http://example.org/o2\"}
        ]";
        let packets = parse_input_to_packets(json_data, "json", &symbols).unwrap();
        assert_eq!(packets.len(), 1);
        assert_eq!(packets[0].len(), 2);
    }

    #[test]
    fn test_parse_ocel2_success() {
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
        assert!(!packets.is_empty());
    }

    #[test]
    fn test_parse_malformed_refusal_turtle() {
        let symbols = SymbolTable::new();
        let malformed = b"invalid turtle syntax";
        let res = parse_input_to_packets(malformed, "turtle", &symbols);
        assert!(res.is_err());
        let err = res.err().unwrap();
        assert!(err.reason.contains("Turtle parsing error"));
    }

    #[test]
    fn test_parse_malformed_refusal_nquads() {
        let symbols = SymbolTable::new();
        let malformed = b"invalid nquads syntax";
        let res = parse_input_to_packets(malformed, "nquads", &symbols);
        assert!(res.is_err());
        let err = res.err().unwrap();
        assert!(err.reason.contains("N-Quads parsing error"));
    }

    #[test]
    fn test_parse_malformed_refusal_csv() {
        let symbols = SymbolTable::new();
        let malformed = b"too,few";
        let res = parse_input_to_packets(malformed, "csv", &symbols);
        assert!(res.is_err());
        let err = res.err().unwrap();
        assert!(err
            .reason
            .contains("CSV input must contain at least 3 columns"));
    }

    #[test]
    fn test_parse_malformed_refusal_json() {
        let symbols = SymbolTable::new();
        let malformed = b"{\"invalid\": \"json\"";
        let res = parse_input_to_packets(malformed, "json", &symbols);
        assert!(res.is_err());
        let err = res.err().unwrap();
        assert!(err.reason.contains("Malformed JSON syntax"));
    }

    #[test]
    fn test_parse_malformed_refusal_ocel2() {
        let symbols = SymbolTable::new();
        let malformed = b"{\"invalid\": \"json\"";
        let res = parse_input_to_packets(malformed, "ocel2", &symbols);
        assert!(res.is_err());
        let err = res.err().unwrap();
        assert!(err.reason.contains("Malformed JSON syntax for OCEL2"));
    }
}
