//! RDF quad to JSON context conversion for MCP generation
//!
//! This module provides conversion from SPARQL CONSTRUCT query results
//! (Oxigraph Quad iterator) to structured JSON for template rendering.
//!
//! ## Context Structure
//!
//! The conversion groups quads by subject under the `ctx:` prefix:
//!
//! - `ctx:server` - Server metadata (name, version, description)
//! - `ctx:tool/{name}` - Tools with arguments
//! - `ctx:resource/{uri}` - Resources with metadata
//! - `ctx:prompt/{name}` - Prompts with arguments
//! - `ctx:completion/{type}/{name}/{arg}` - Completion providers
//!
//! ## Example
//!
//! ```rust,no_run
//! use ggen_core::mcp::context::convert_quads_to_json;
//! use ggen_core::graph::Graph;
//!
//! # fn main() -> Result<(), Box<dyn std::error::Error>> {
//! let graph = Graph::new()?;
//! // ... load ontology ...
//!
//! let quads = graph.query_construct("CONSTRUCT { ... } WHERE { ... }")?;
//! let json = convert_quads_to_json(quads)?;
//! # Ok(())
//! # }
//! ```

use crate::utils::error::{Error, Result};
use oxigraph::model::{NamedNode, NamedOrBlankNode, Term};
use serde_json::{Map, Value};
use std::collections::BTreeMap;

/// Convert RDF triples from SPARQL CONSTRUCT to flat JSON structure
///
/// This function takes an iterator of Triple objects (from a SPARQL CONSTRUCT query)
/// and groups them by subject to create a nested JSON structure suitable for
/// template rendering.
///
/// # Arguments
///
/// * `triples` - Iterator of Result<Triple, Error> objects from CONSTRUCT query results
///
/// # Returns
///
/// * `Ok(Value)` - JSON Object with server, tools, resources, prompts arrays
/// * `Err(Error)` - Conversion error
///
/// # Example JSON Structure
///
/// ```json
/// {
///   "server": {
///     "name": "my_mcp_server",
///     "version": "0.1.0",
///     "description": "My MCP Server",
///     "transport": "stdio"
///   },
///   "tools": [
///     {
///       "name": "get_weather",
///       "description": "Get current weather",
///       "arguments": [...]
///     }
///   ],
///   "resources": [],
///   "prompts": []
/// }
/// ```
pub fn convert_triples_to_json<I, E>(triples: I) -> Result<Value>
where
    I: IntoIterator<Item = std::result::Result<oxigraph::model::Triple, E>>,
    E: std::fmt::Display,
{
    // Group triples by subject (BTreeMap for deterministic ordering during grouping)
    let mut grouped: BTreeMap<String, BTreeMap<String, Term>> = BTreeMap::new();

    for triple_result in triples {
        let triple = triple_result
            .map_err(|e| Error::new(&format!("Failed to read triple: {}", e)))?;

        let subject = extract_subject_key(&triple.subject)?;
        let predicate = extract_predicate_name(&triple.predicate)?;

        grouped
            .entry(subject)
            .or_default()
            .insert(predicate, triple.object);
    }

    // Build JSON structure
    let mut server = Map::new();
    let mut tools: Vec<Value> = Vec::new();
    let mut resources: Vec<Value> = Vec::new();
    let mut prompts: Vec<Value> = Vec::new();

    for (subject, predicates) in grouped {
        match subject.as_str() {
            "ctx:server" => {
                // Extract server metadata
                server = convert_predicates_to_map(predicates)?;
            }
            s if s.starts_with("ctx:tool/") => {
                // Extract tool definition
                let tool_name = s.strip_prefix("ctx:tool/").unwrap_or(&s);
                tools.push(convert_tool(tool_name, predicates)?);
            }
            s if s.starts_with("ctx:resource/") => {
                // Extract resource definition
                let resource_uri = s.strip_prefix("ctx:resource/").unwrap_or(&s);
                resources.push(convert_resource(resource_uri, predicates)?);
            }
            s if s.starts_with("ctx:prompt/") => {
                // Extract prompt definition
                let prompt_name = s.strip_prefix("ctx:prompt/").unwrap_or(&s);
                prompts.push(convert_prompt(prompt_name, predicates)?);
            }
            _ => {
                // Unknown subject prefix - log warning but continue
                // Could add logging here in production
            }
        }
    }

    // Build final JSON structure
    let mut result = Map::new();
    result.insert("server".to_string(), Value::Object(server));
    result.insert("tools".to_string(), Value::Array(tools));
    result.insert("resources".to_string(), Value::Array(resources));
    result.insert("prompts".to_string(), Value::Array(prompts));

    Ok(Value::Object(result))
}

/// Extract subject key from a Term, handling ctx: prefix
fn extract_subject_key(term: &NamedOrBlankNode) -> Result<String> {
    match term {
        NamedOrBlankNode::NamedNode(nn) => {
            let iri = nn.as_str();
            // Handle ctx: prefix
            if iri.starts_with("http://ggen.dev/mcp/context/") {
                let rest = &iri[("http://ggen.dev/mcp/context/".len())..];
                Ok(format!("ctx:{}", rest))
            } else if iri.starts_with("ctx:") {
                Ok(iri.to_string())
            } else {
                // Use full IRI as fallback
                Ok(iri.to_string())
            }
        }
        NamedOrBlankNode::BlankNode(bn) => Ok(bn.to_string()),
    }
}

/// Extract predicate name from a NamedNode
fn extract_predicate_name(term: &NamedNode) -> Result<String> {
    let iri = term.as_str();

    // Extract local name after last # or /
    if let Some(hash_pos) = iri.rfind('#') {
        Ok(iri[hash_pos + 1..].to_string())
    } else if let Some(slash_pos) = iri.rfind('/') {
        Ok(iri[slash_pos + 1..].to_string())
    } else {
        Ok(iri.to_string())
    }
}

/// Convert predicates map to serde_json::Map
fn convert_predicates_to_map(predicates: BTreeMap<String, Term>) -> Result<Map<String, Value>> {
    let mut result = Map::new();
    for (key, term) in predicates {
        result.insert(key, term_to_json_value(&term));
    }
    Ok(result)
}

/// Convert tool predicates to tool JSON object
fn convert_tool(name: &str, predicates: BTreeMap<String, Term>) -> Result<Value> {
    let mut tool = Map::new();
    tool.insert("name".to_string(), Value::String(name.to_string()));

    // Extract description if present
    if let Some(term) = predicates.get("description") {
        tool.insert("description".to_string(), term_to_json_value(term));
    }

    // Extract arguments array if present
    if let Some(term) = predicates.get("arguments") {
        tool.insert("arguments".to_string(), term_to_json_value(term));
    }

    // Add any additional predicates
    for (key, term) in predicates {
        if key != "description" && key != "arguments" {
            tool.insert(key, term_to_json_value(&term));
        }
    }

    Ok(Value::Object(tool))
}

/// Convert resource predicates to resource JSON object
fn convert_resource(uri: &str, predicates: BTreeMap<String, Term>) -> Result<Value> {
    let mut resource = Map::new();
    resource.insert("uri".to_string(), Value::String(uri.to_string()));

    // Extract standard fields
    for (key, term) in predicates {
        resource.insert(key, term_to_json_value(&term));
    }

    Ok(Value::Object(resource))
}

/// Convert prompt predicates to prompt JSON object
fn convert_prompt(name: &str, predicates: BTreeMap<String, Term>) -> Result<Value> {
    let mut prompt = Map::new();
    prompt.insert("name".to_string(), Value::String(name.to_string()));

    // Extract description if present
    if let Some(term) = predicates.get("description") {
        prompt.insert("description".to_string(), term_to_json_value(term));
    }

    // Extract arguments if present
    if let Some(term) = predicates.get("arguments") {
        prompt.insert("arguments".to_string(), term_to_json_value(term));
    }

    // Add any additional predicates
    for (key, term) in predicates {
        if key != "description" && key != "arguments" {
            prompt.insert(key, term_to_json_value(&term));
        }
    }

    Ok(Value::Object(prompt))
}

/// Convert RDF Term to JSON Value
fn term_to_json_value(term: &Term) -> Value {
    match term {
        Term::Literal(lit) => {
            // Check if it's a boolean literal
            let bool_dt = NamedNode::new("http://www.w3.org/2001/XMLSchema#boolean").unwrap();
            if lit.datatype() == bool_dt {
                let value = lit.value();
                if value == "true" || value == "1" {
                    return Value::Bool(true);
                } else if value == "false" || value == "0" {
                    return Value::Bool(false);
                }
            }

            // Check if it's a numeric literal
            let int_dt = NamedNode::new("http://www.w3.org/2001/XMLSchema#integer").unwrap();
            let xsint_dt = NamedNode::new("http://www.w3.org/2001/XMLSchema#int").unwrap();
            if lit.datatype() == int_dt || lit.datatype() == xsint_dt {
                if let Ok(num) = lit.value().parse::<i64>() {
                    return Value::Number(num.into());
                }
            }

            // Check if it has a language tag
            if lit.language().is_some() {
                let mut map = Map::new();
                map.insert(
                    "@value".to_string(),
                    Value::String(lit.value().to_string()),
                );
                map.insert(
                    "@language".to_string(),
                    Value::String(lit.language().unwrap().to_string()),
                );
                return Value::Object(map);
            }

            // Check if it has a datatype (other than plain string)
            let string_dt = NamedNode::new("http://www.w3.org/2001/XMLSchema#string").unwrap();
            if lit.datatype() != string_dt {
                let dt = lit.datatype();
                if dt.as_str() != "http://www.w3.org/2001/XMLSchema#string" {
                    let mut map = Map::new();
                    map.insert(
                        "@value".to_string(),
                        Value::String(lit.value().to_string()),
                    );
                    map.insert(
                        "@type".to_string(),
                        Value::String(dt.as_str().to_string()),
                    );
                    return Value::Object(map);
                }
            }

            // Default: plain string
            Value::String(lit.value().to_string())
        }
        Term::NamedNode(nn) => Value::String(nn.as_str().to_string()),
        Term::BlankNode(bn) => Value::String(bn.to_string()),
        _ => Value::Null,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use oxigraph::model::{Literal, NamedNode};

    #[test]
    fn test_extract_subject_key_named_node() {
        let nn = NamedNode::new("http://ggen.dev/mcp/context/server").unwrap();
        let node = NamedOrBlankNode::NamedNode(nn);
        let result = extract_subject_key(&node).unwrap();
        assert_eq!(result, "ctx:server");
    }

    #[test]
    fn test_extract_subject_key_blank_node() {
        let bn = oxigraph::model::BlankNode::new("b1").unwrap();
        let node = NamedOrBlankNode::BlankNode(bn);
        let result = extract_subject_key(&node).unwrap();
        // Blank nodes are serialized as "_:id"
        assert_eq!(result, "_:b1");
    }

    #[test]
    fn test_extract_predicate_name() {
        let nn = NamedNode::new("http://ggen.dev/mcp#serverName").unwrap();
        let result = extract_predicate_name(&nn).unwrap();
        assert_eq!(result, "serverName");
    }

    #[test]
    fn test_convert_triples_to_json_server_only() {
        use oxigraph::model::Triple;
        use std::result::Result;

        let triples: Vec<Result<Triple, oxigraph::sparql::QueryEvaluationError>> = vec![
            Ok(Triple::new(
                NamedNode::new("http://ggen.dev/mcp/context/server").unwrap(),
                NamedNode::new("http://ggen.dev/mcp/context/serverName").unwrap(),
                Literal::new_simple_literal("test_server"),
            )),
            Ok(Triple::new(
                NamedNode::new("http://ggen.dev/mcp/context/server").unwrap(),
                NamedNode::new("http://ggen.dev/mcp/context/version").unwrap(),
                Literal::new_simple_literal("0.1.0"),
            )),
        ];

        let result = convert_triples_to_json(triples).unwrap();

        // Verify structure
        assert!(result.is_object());
        let obj = result.as_object().unwrap();

        // Check server object exists
        assert!(obj.contains_key("server"));
        let server = obj.get("server").unwrap().as_object().unwrap();
        assert_eq!(
            server.get("serverName").unwrap().as_str().unwrap(),
            "test_server"
        );
        assert_eq!(server.get("version").unwrap().as_str().unwrap(), "0.1.0");

        // Check empty arrays
        assert_eq!(obj.get("tools").unwrap().as_array().unwrap().len(), 0);
        assert_eq!(obj.get("resources").unwrap().as_array().unwrap().len(), 0);
        assert_eq!(obj.get("prompts").unwrap().as_array().unwrap().len(), 0);
    }

    #[test]
    fn test_convert_triples_to_json_with_tools() {
        use oxigraph::model::Triple;
        use std::result::Result;

        let triples: Vec<Result<Triple, oxigraph::sparql::QueryEvaluationError>> = vec![
            Ok(Triple::new(
                NamedNode::new("http://ggen.dev/mcp/context/server").unwrap(),
                NamedNode::new("http://ggen.dev/mcp/context/serverName").unwrap(),
                Literal::new_simple_literal("test_server"),
            )),
            Ok(Triple::new(
                NamedNode::new("http://ggen.dev/mcp/context/tool/get_weather").unwrap(),
                NamedNode::new("http://ggen.dev/mcp/context/description").unwrap(),
                Literal::new_simple_literal("Get current weather"),
            )),
        ];

        let result = convert_triples_to_json(triples).unwrap();
        let obj = result.as_object().unwrap();

        // Check tools array
        let tools = obj.get("tools").unwrap().as_array().unwrap();
        assert_eq!(tools.len(), 1);

        let tool = tools.get(0).unwrap().as_object().unwrap();
        assert_eq!(tool.get("name").unwrap().as_str().unwrap(), "get_weather");
        assert_eq!(
            tool.get("description").unwrap().as_str().unwrap(),
            "Get current weather"
        );
    }

    #[test]
    fn test_term_to_json_value_literal() {
        let term = Term::Literal(Literal::new_simple_literal("test_value"));
        let result = term_to_json_value(&term);
        assert_eq!(result.as_str().unwrap(), "test_value");
    }

    #[test]
    fn test_term_to_json_value_named_node() {
        let nn = NamedNode::new("http://example.org/test").unwrap();
        let term = Term::NamedNode(nn);
        let result = term_to_json_value(&term);
        assert_eq!(result.as_str().unwrap(), "http://example.org/test");
    }
}
