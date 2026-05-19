//! MCP (Model Context Protocol) generation domain logic
//!
//! This module provides RDF-to-JSON conversion for MCP server generation.
//! It executes SPARQL CONSTRUCT queries against MCP ontologies and transforms
//! the resulting quads into flat JSON structures suitable for template rendering.

use ggen_core::utils::error::{Context, Result};
use oxigraph::io::RdfFormat;
use oxigraph::sparql::QueryResults;
use oxigraph::store::Store;
use serde_json::Value;
use std::collections::{BTreeMap, HashMap};
use std::fs;
use std::io::BufReader;
use std::path::Path;

/// Convert SPARQL CONSTRUCT query results (quads) to flat JSON context
pub fn convert_quads_to_context(
    _store: &Store,
    query_results: QueryResults,
) -> Result<BTreeMap<String, Value>> {
    let quads = match query_results {
        QueryResults::Graph(quad_iter) => quad_iter,
        QueryResults::Solutions(_) => {
            return Err(ggen_core::utils::error::Error::new(
                "Expected CONSTRUCT query (Graph results) but got SELECT query (Solutions)",
            ))
        }
        QueryResults::Boolean(_) => {
            return Err(ggen_core::utils::error::Error::new(
                "Expected CONSTRUCT query (Graph results) but got ASK query (Boolean)",
            ))
        }
    };

    let mut all_quads = Vec::new();
    for quad_result in quads {
        let quad = quad_result
            .map_err(|e| {
                ggen_core::utils::error::Error::new(&format!("Failed to read quad: {}", e))
            })
            .context("Failed to read quad from CONSTRUCT results")?;
        all_quads.push(quad);
    }

    let mut subjects: HashMap<String, Vec<Quad>> = HashMap::new();
    for quad in all_quads {
        let subject = quad.subject.to_string();
        subjects
            .entry(subject.clone())
            .or_insert_with(Vec::new)
            .push(Quad {
                subject: subject.clone(),
                predicate: quad.predicate.to_string(),
                object: quad.object.to_string(),
            });
    }

    let mut context = BTreeMap::new();

    // Process server metadata
    if let Some(server_quads) = subjects.get("<http://ggen.dev/mcp/context/server>") {
        let mut server_obj = serde_json::Map::new();
        for quad in server_quads {
            let pred_short = strip_prefix(&quad.predicate, "http://ggen.dev/mcp/context/");
            match pred_short.as_str() {
                "serverName" => {
                    server_obj.insert("name".to_string(), Value::String(quad.object.clone()));
                }
                "serverVersion" => {
                    server_obj.insert("version".to_string(), Value::String(quad.object.clone()));
                }
                "serverDescription" => {
                    server_obj.insert("description".to_string(), Value::String(quad.object.clone()));
                }
                "protocolVersion" => {
                    server_obj.insert("protocol_version".to_string(), Value::String(quad.object.clone()));
                }
                _ => {
                    server_obj.insert(pred_short, Value::String(quad.object.clone()));
                }
            }
        }
        context.insert("server".to_string(), Value::Object(server_obj));
    }

    // Process capabilities
    if let Some(cap_quads) = subjects.get("<http://ggen.dev/mcp/context/server/capabilities>") {
        let mut caps_obj = serde_json::Map::new();
        for quad in cap_quads {
            let pred_short = strip_prefix(&quad.predicate, "http://ggen.dev/mcp/context/");
            match pred_short.as_str() {
                "hasTools" => {
                    caps_obj.insert("tools".to_string(), Value::String(quad.object.clone()));
                }
                "hasResources" => {
                    caps_obj.insert("resources".to_string(), Value::String(quad.object.clone()));
                }
                "hasPrompts" => {
                    caps_obj.insert("prompts".to_string(), Value::String(quad.object.clone()));
                }
                "hasCompletions" => {
                    caps_obj.insert("completions".to_string(), Value::String(quad.object.clone()));
                }
                "hasLogging" => {
                    caps_obj.insert("logging".to_string(), Value::String(quad.object.clone()));
                }
                "hasSubscriptions" => {
                    caps_obj.insert("subscriptions".to_string(), Value::String(quad.object.clone()));
                }
                _ => {
                    caps_obj.insert(pred_short, Value::String(quad.object.clone()));
                }
            }
        }
        context.insert("capabilities".to_string(), Value::Object(caps_obj));
    }

    // Process transport
    if let Some(trans_quads) = subjects.get("<http://ggen.dev/mcp/context/server/transport>") {
        let mut trans_obj = serde_json::Map::new();
        for quad in trans_quads {
            let pred_short = strip_prefix(&quad.predicate, "http://ggen.dev/mcp/context/");
            match pred_short.as_str() {
                "transportName" => {
                    trans_obj.insert("name".to_string(), Value::String(quad.object.clone()));
                }
                "transportDescription" => {
                    trans_obj.insert("description".to_string(), Value::String(quad.object.clone()));
                }
                _ => {
                    trans_obj.insert(pred_short, Value::String(quad.object.clone()));
                }
            }
        }
        context.insert("transport".to_string(), Value::Object(trans_obj));
    }

    // Process tools
    let mut tools = Vec::new();
    for (subject, quads) in &subjects {
        if subject.starts_with("<http://ggen.dev/mcp/context/tool/")
            && !subject.contains("/arg/")
        {
            let mut tool_obj = serde_json::Map::new();
            let tool_name = subject
                .strip_prefix("<http://ggen.dev/mcp/context/tool/")
                .and_then(|s| s.strip_suffix(">"))
                .unwrap_or(subject);

            for quad in quads {
                let pred_short = strip_prefix(&quad.predicate, "http://ggen.dev/mcp/context/");
                match pred_short.as_str() {
                    "toolName" => {
                        tool_obj.insert("name".to_string(), Value::String(quad.object.clone()));
                    }
                    "toolDescription" => {
                        tool_obj.insert("description".to_string(), Value::String(quad.object.clone()));
                    }
                    "implementedBy" => {
                        tool_obj.insert("implemented_by".to_string(), Value::String(quad.object.clone()));
                    }
                    _ => {
                        tool_obj.insert(pred_short, Value::String(quad.object.clone()));
                    }
                }
            }

            // Collect tool arguments
            let mut args = Vec::new();
            for (arg_subject, arg_quads) in &subjects {
                if arg_subject.starts_with(&format!(
                    "<http://ggen.dev/mcp/context/tool/{}/arg/",
                    tool_name
                )) {
                    let mut arg_obj = serde_json::Map::new();
                    for quad in arg_quads {
                        let pred_short = strip_prefix(&quad.predicate, "http://ggen.dev/mcp/context/");
                        match pred_short.as_str() {
                            "argumentName" => {
                                arg_obj.insert("name".to_string(), Value::String(quad.object.clone()));
                            }
                            "argumentType" => {
                                arg_obj.insert("type".to_string(), Value::String(quad.object.clone()));
                            }
                            "argumentDescription" => {
                                arg_obj.insert("description".to_string(), Value::String(quad.object.clone()));
                            }
                            "isRequired" => {
                                let bool_val = quad.object.parse::<bool>().unwrap_or(false);
                                arg_obj.insert("required".to_string(), Value::Bool(bool_val));
                            }
                            "argumentDefault" => {
                                arg_obj.insert("default".to_string(), Value::String(quad.object.clone()));
                            }
                            _ => {
                                arg_obj.insert(pred_short, Value::String(quad.object.clone()));
                            }
                        }
                    }
                    args.push(Value::Object(arg_obj));
                }
            }

            if !args.is_empty() {
                tool_obj.insert("arguments".to_string(), Value::Array(args));
            }
            tools.push(Value::Object(tool_obj));
        }
    }

    if !tools.is_empty() {
        context.insert("tools".to_string(), Value::Array(tools));
    }

    // Process resources
    let mut resources = Vec::new();
    for (subject, quads) in &subjects {
        if subject.starts_with("<http://ggen.dev/mcp/context/resource/")
            && !subject.contains("-tpl/")
        {
            let mut resource_obj = serde_json::Map::new();
            for quad in quads {
                let pred_short = strip_prefix(&quad.predicate, "http://ggen.dev/mcp/context/");
                match pred_short.as_str() {
                    "resourceName" => {
                        resource_obj.insert("name".to_string(), Value::String(quad.object.clone()));
                    }
                    "resourceUri" => {
                        resource_obj.insert("uri".to_string(), Value::String(quad.object.clone()));
                    }
                    "resourceDescription" => {
                        resource_obj.insert("description".to_string(), Value::String(quad.object.clone()));
                    }
                    "mimeType" => {
                        resource_obj.insert("mime_type".to_string(), Value::String(quad.object.clone()));
                    }
                    "implementedBy" => {
                        resource_obj.insert("implemented_by".to_string(), Value::String(quad.object.clone()));
                    }
                    _ => {
                        resource_obj.insert(pred_short, Value::String(quad.object.clone()));
                    }
                }
            }
            resources.push(Value::Object(resource_obj));
        }
    }

    if !resources.is_empty() {
        context.insert("resources".to_string(), Value::Array(resources));
    }

    // Process resource templates
    let mut resource_templates = Vec::new();
    for (subject, quads) in &subjects {
        if subject.starts_with("<http://ggen.dev/mcp/context/resource-tpl/") {
            let mut template_obj = serde_json::Map::new();
            for quad in quads {
                let pred_short = strip_prefix(&quad.predicate, "http://ggen.dev/mcp/context/");
                match pred_short.as_str() {
                    "templateName" => {
                        template_obj.insert("name".to_string(), Value::String(quad.object.clone()));
                    }
                    "templateDescription" => {
                        template_obj.insert("description".to_string(), Value::String(quad.object.clone()));
                    }
                    "uriTemplate" => {
                        template_obj.insert("uri_template".to_string(), Value::String(quad.object.clone()));
                    }
                    "templateMimeType" => {
                        template_obj.insert("mime_type".to_string(), Value::String(quad.object.clone()));
                    }
                    _ => {
                        template_obj.insert(pred_short, Value::String(quad.object.clone()));
                    }
                }
            }
            resource_templates.push(Value::Object(template_obj));
        }
    }

    if !resource_templates.is_empty() {
        context.insert("resource_templates".to_string(), Value::Array(resource_templates));
    }

    // Process prompts
    let mut prompts = Vec::new();
    for (subject, quads) in &subjects {
        if subject.starts_with("<http://ggen.dev/mcp/context/prompt/")
            && !subject.contains("/arg/")
        {
            let mut prompt_obj = serde_json::Map::new();
            let prompt_name = subject
                .strip_prefix("<http://ggen.dev/mcp/context/prompt/")
                .and_then(|s| s.strip_suffix(">"))
                .unwrap_or(subject);

            for quad in quads {
                let pred_short = strip_prefix(&quad.predicate, "http://ggen.dev/mcp/context/");
                match pred_short.as_str() {
                    "promptName" => {
                        prompt_obj.insert("name".to_string(), Value::String(quad.object.clone()));
                    }
                    "promptDescription" => {
                        prompt_obj.insert("description".to_string(), Value::String(quad.object.clone()));
                    }
                    "implementedBy" => {
                        prompt_obj.insert("implemented_by".to_string(), Value::String(quad.object.clone()));
                    }
                    _ => {
                        prompt_obj.insert(pred_short, Value::String(quad.object.clone()));
                    }
                }
            }

            // Collect prompt arguments
            let mut args = Vec::new();
            for (arg_subject, arg_quads) in &subjects {
                if arg_subject.starts_with(&format!(
                    "<http://ggen.dev/mcp/context/prompt/{}/arg/",
                    prompt_name
                )) {
                    let mut arg_obj = serde_json::Map::new();
                    for quad in arg_quads {
                        let pred_short = strip_prefix(&quad.predicate, "http://ggen.dev/mcp/context/");
                        match pred_short.as_str() {
                            "promptArgumentName" => {
                                arg_obj.insert("name".to_string(), Value::String(quad.object.clone()));
                            }
                            "promptArgumentType" => {
                                arg_obj.insert("type".to_string(), Value::String(quad.object.clone()));
                            }
                            "promptArgumentDescription" => {
                                arg_obj.insert("description".to_string(), Value::String(quad.object.clone()));
                            }
                            "promptIsRequired" => {
                                let bool_val = quad.object.parse::<bool>().unwrap_or(false);
                                arg_obj.insert("required".to_string(), Value::Bool(bool_val));
                            }
                            _ => {
                                arg_obj.insert(pred_short, Value::String(quad.object.clone()));
                            }
                        }
                    }
                    args.push(Value::Object(arg_obj));
                }
            }

            if !args.is_empty() {
                prompt_obj.insert("arguments".to_string(), Value::Array(args));
            }
            prompts.push(Value::Object(prompt_obj));
        }
    }

    if !prompts.is_empty() {
        context.insert("prompts".to_string(), Value::Array(prompts));
    }

    // Process logging policy
    if let Some(log_quads) = subjects.get("<http://ggen.dev/mcp/context/server/logging>") {
        let mut log_obj = serde_json::Map::new();
        for quad in log_quads {
            let pred_short = strip_prefix(&quad.predicate, "http://ggen.dev/mcp/context/");
            match pred_short.as_str() {
                "defaultLevel" => {
                    log_obj.insert("default_level".to_string(), Value::String(quad.object.clone()));
                }
                "supportedLevels" => {
                    let levels: Vec<Value> = quad
                        .object
                        .split(',')
                        .map(|s| Value::String(s.trim().to_string()))
                        .collect();
                    log_obj.insert("supported_levels".to_string(), Value::Array(levels));
                }
                _ => {
                    log_obj.insert(pred_short, Value::String(quad.object.clone()));
                }
            }
        }
        context.insert("logging".to_string(), Value::Object(log_obj));
    }

    Ok(context)
}

/// Generate MCP JSON context from RDF ontology file
pub fn generate_mcp_context(
    ontology_file: &Path,
    query_file: Option<&Path>,
) -> Result<BTreeMap<String, Value>> {
    let store = Store::new().map_err(|e| {
        ggen_core::utils::error::Error::new(&format!("Failed to create RDF store: {}", e))
    })?;

    let file = fs::File::open(ontology_file).map_err(|e| {
        ggen_core::utils::error::Error::new(&format!(
            "Failed to open ontology file {:?}: {}",
            ontology_file, e
        ))
    })?;

    let reader = BufReader::new(file);

    store
        .load_from_reader(
            oxigraph::io::RdfParser::from_format(RdfFormat::Turtle),
            reader,
        )
        .map_err(|e| {
            ggen_core::utils::error::Error::new(&format!("Failed to parse Turtle: {}", e))
        })
        .context("Failed to load RDF ontology into store")?;

    let query_content = if let Some(qf) = query_file {
        fs::read_to_string(qf).map_err(|e| {
            ggen_core::utils::error::Error::new(&format!(
                "Failed to read query file {:?}: {}",
                qf, e
            ))
        })?
    } else {
        "CONSTRUCT { ?s ?p ?o } WHERE { ?s ?p ?o }".to_string()
    };

    #[allow(deprecated)]
    let query_results = store
        .query(&query_content)
        .map_err(|e| {
            ggen_core::utils::error::Error::new(&format!("SPARQL CONSTRUCT query failed: {}", e))
        })
        .context("Failed to execute SPARQL CONSTRUCT query")?;

    convert_quads_to_context(&store, query_results)
}

/// Internal quad representation
#[allow(dead_code)]
struct Quad {
    subject: String,
    predicate: String,
    object: String,
}

/// Strip namespace prefix from IRI
fn strip_prefix(iri: &str, prefix: &str) -> String {
    iri.strip_prefix(prefix)
        .unwrap_or(iri)
        .to_string()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_convert_quads_to_context_basic() {
        let turtle = r#"
            @prefix ctx: <http://ggen.dev/mcp/context/> .
            ctx:server a ctx:McpServer ;
                ctx:serverName "test-server" ;
                ctx:serverVersion "1.0.0" .
        "#;

        let store = Store::new().expect("Failed to create store");
        store
            .load_from_reader(
                oxigraph::io::RdfParser::from_format(RdfFormat::Turtle),
                turtle.as_bytes(),
            )
            .expect("Failed to load Turtle");

        #[allow(deprecated)]
        let query =
            r#"PREFIX ctx: <http://ggen.dev/mcp/context/> CONSTRUCT { ?s ?p ?o } WHERE { ?s ?p ?o }"#;
        #[allow(deprecated)]
        let query_results = store.query(query).expect("Failed to execute query");
        let context = convert_quads_to_context(&store, query_results)
            .expect("Failed to convert quads to context");

        assert!(context.contains_key("server"));
        let server = context.get("server").unwrap().as_object().unwrap();
        assert_eq!(server.get("name").unwrap().as_str().unwrap(), "test-server");
        assert_eq!(server.get("version").unwrap().as_str().unwrap(), "1.0.0");
    }

    #[test]
    fn test_convert_quads_to_context_with_tools() {
        let turtle = r#"
            @prefix ctx: <http://ggen.dev/mcp/context/> .
            ctx:server a ctx:McpServer ;
                ctx:serverName "test-server" .
            ctx:tool/example_tool a ctx:Tool ;
                ctx:toolName "example_tool" ;
                ctx:toolDescription "An example tool" .
            ctx:tool/example_tool/arg/input a ctx:Argument ;
                ctx:argumentName "input" ;
                ctx:argumentType "string" ;
                ctx:isRequired "true"^^<http://www.w3.org/2001/XMLSchema#boolean> .
        "#;

        let store = Store::new().expect("Failed to create store");
        store
            .load_from_reader(
                oxigraph::io::RdfParser::from_format(RdfFormat::Turtle),
                turtle.as_bytes(),
            )
            .expect("Failed to load Turtle");

        #[allow(deprecated)]
        let query =
            r#"PREFIX ctx: <http://ggen.dev/mcp/context/> CONSTRUCT { ?s ?p ?o } WHERE { ?s ?p ?o }"#;
        #[allow(deprecated)]
        let query_results = store.query(query).expect("Failed to execute query");
        let context = convert_quads_to_context(&store, query_results)
            .expect("Failed to convert quads to context");

        assert!(context.contains_key("tools"));
        let tools = context.get("tools").unwrap().as_array().unwrap();
        assert_eq!(tools.len(), 1);

        let tool = &tools[0];
        let tool_obj = tool.as_object().unwrap();
        assert_eq!(
            tool_obj.get("name").unwrap().as_str().unwrap(),
            "example_tool"
        );
        assert!(tool_obj.contains_key("arguments"));

        let args = tool_obj.get("arguments").unwrap().as_array().unwrap();
        assert_eq!(args.len(), 1);

        let arg = &args[0];
        let arg_obj = arg.as_object().unwrap();
        assert_eq!(arg_obj.get("name").unwrap().as_str().unwrap(), "input");
        assert_eq!(arg_obj.get("type").unwrap().as_str().unwrap(), "string");
        assert_eq!(arg_obj.get("required").unwrap().as_bool().unwrap(), true);
    }

    #[test]
    fn test_convert_quads_to_context_invalid_query_type() {
        let store = Store::new().expect("Failed to create store");
        let query = r#"SELECT ?s WHERE { ?s a ?type } LIMIT 1"#;
        #[allow(deprecated)]
        let query_results = store.query(query).expect("Failed to execute query");
        let result = convert_quads_to_context(&store, query_results);

        assert!(result.is_err());
        assert!(result.unwrap_err().to_string().contains("SELECT"));
    }

    #[test]
    fn test_strip_prefix() {
        assert_eq!(
            strip_prefix("http://example.org/name", "http://example.org/"),
            "name"
        );
        assert_eq!(
            strip_prefix("http://other.org/iri", "http://example.org/"),
            "http://other.org/iri"
        );
    }

    #[test]
    fn test_generate_mcp_context_with_file() {
        use std::io::Write;
        use tempfile::NamedTempFile;

        let turtle_content = r#"
            @prefix ctx: <http://ggen.dev/mcp/context/> .
            ctx:server a ctx:McpServer ;
                ctx:serverName "file-test-server" ;
                ctx:serverVersion "2.0.0" ;
                ctx:serverDescription "Test server from file" .
        "#;

        let mut temp_file = NamedTempFile::new().expect("Failed to create temp file");
        temp_file
            .write_all(turtle_content.as_bytes())
            .expect("Failed to write to temp file");

        let context =
            generate_mcp_context(temp_file.path(), None).expect("Failed to generate context");

        assert!(context.contains_key("server"));
        let server = context.get("server").unwrap().as_object().unwrap();
        assert_eq!(
            server.get("name").unwrap().as_str().unwrap(),
            "file-test-server"
        );
        assert_eq!(server.get("version").unwrap().as_str().unwrap(), "2.0.0");
        assert_eq!(
            server.get("description").unwrap().as_str().unwrap(),
            "Test server from file"
        );
    }
}
