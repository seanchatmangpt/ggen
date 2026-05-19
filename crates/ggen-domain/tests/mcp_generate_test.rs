//! Chicago TDD tests for MCP generation domain logic
//!
//! These tests verify MCP server generation functionality using real file I/O
//! and actual RDF operations (no mocks).

use ggen_core::domain::mcp_generate;
use std::fs;
use std::io::Write;
use std::path::PathBuf;
use tempfile::TempDir;

/// Helper to create a temporary Turtle ontology file
fn create_temp_ontology(content: &str) -> (TempDir, PathBuf) {
    let temp_dir = TempDir::new().expect("Failed to create temp dir");
    let ontology_path = temp_dir.path().join("test-mcp.ttl");

    fs::write(&ontology_path, content).expect("Failed to write ontology file");

    (temp_dir, ontology_path)
}

/// Helper to create a temporary SPARQL query file
fn create_temp_query(content: &str) -> (TempDir, PathBuf) {
    let temp_dir = TempDir::new().expect("Failed to create temp dir");
    let query_path = temp_dir.path().join("test-query.rq");

    fs::write(&query_path, content).expect("Failed to write query file");

    (temp_dir, query_path)
}

#[test]
fn test_generate_mcp_context_basic_server() {
    // Arrange
    let turtle_content = r#"
        @prefix ctx: <http://ggen.dev/mcp/context/> .
        @prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

        ctx:server a ctx:McpServer ;
            ctx:serverName "test-server" ;
            ctx:serverVersion "1.0.0" ;
            ctx:serverDescription "A test MCP server" ;
            ctx:protocolVersion "2024-11-05" .
    "#;

    let (_temp_dir, ontology_path) = create_temp_ontology(turtle_content);

    // Act
    let result = mcp_generate::generate_mcp_context(&ontology_path, None);

    // Assert
    assert!(result.is_ok(), "generate_mcp_context should succeed");
    let context = result.unwrap();

    assert!(context.contains_key("server"));
    let server = context.get("server").unwrap().as_object().unwrap();
    assert_eq!(server.get("name").unwrap().as_str().unwrap(), "test-server");
    assert_eq!(server.get("version").unwrap().as_str().unwrap(), "1.0.0");
    assert_eq!(
        server.get("description").unwrap().as_str().unwrap(),
        "A test MCP server"
    );
    assert_eq!(
        server.get("protocol_version").unwrap().as_str().unwrap(),
        "2024-11-05"
    );
}

#[test]
fn test_generate_mcp_context_with_tools() {
    // Arrange
    let turtle_content = r#"
        @prefix ctx: <http://ggen.dev/mcp/context/> .
        @prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

        ctx:server a ctx:McpServer ;
            ctx:serverName "tool-server" ;
            ctx:serverVersion "2.0.0" .

        ctx:tool/validate_pipeline a ctx:Tool ;
            ctx:toolName "validate_pipeline" ;
            ctx:toolDescription "Validate the generation pipeline" ;
            ctx:implementedBy "ggen_validate_pipeline" .

        ctx:tool/validate_pipeline/arg/config_path a ctx:ToolArgument ;
            ctx:belongsToTool "validate_pipeline" ;
            ctx:argumentName "config_path" ;
            ctx:argumentType "string" ;
            ctx:argumentDescription "Path to ggen.toml" ;
            ctx:isRequired "true"^^xsd:boolean .

        ctx:tool/validate_pipeline/arg/strict a ctx:ToolArgument ;
            ctx:belongsToTool "validate_pipeline" ;
            ctx:argumentName "strict" ;
            ctx:argumentType "boolean" ;
            ctx:argumentDescription "Enable strict validation" ;
            ctx:isRequired "false"^^xsd:boolean ;
            ctx:argumentDefault "false"^^xsd:boolean .
    "#;

    let (_temp_dir, ontology_path) = create_temp_ontology(turtle_content);

    // Act
    let result = mcp_generate::generate_mcp_context(&ontology_path, None);

    // Assert
    assert!(result.is_ok(), "generate_mcp_context should succeed");
    let context = result.unwrap();

    assert!(context.contains_key("tools"));
    let tools = context.get("tools").unwrap().as_array().unwrap();
    assert_eq!(tools.len(), 1);

    let tool = &tools[0];
    let tool_obj = tool.as_object().unwrap();
    assert_eq!(
        tool_obj.get("name").unwrap().as_str().unwrap(),
        "validate_pipeline"
    );
    assert_eq!(
        tool_obj.get("implemented_by").unwrap().as_str().unwrap(),
        "ggen_validate_pipeline"
    );
    assert!(tool_obj.contains_key("arguments"));

    let args = tool_obj.get("arguments").unwrap().as_array().unwrap();
    assert_eq!(args.len(), 2);

    // Check first argument (config_path)
    let arg1 = &args[0];
    let arg1_obj = arg1.as_object().unwrap();
    assert_eq!(arg1_obj.get("name").unwrap().as_str().unwrap(), "config_path");
    assert_eq!(arg1_obj.get("type").unwrap().as_str().unwrap(), "string");
    assert_eq!(arg1_obj.get("required").unwrap().as_bool().unwrap(), true);

    // Check second argument (strict)
    let arg2 = &args[1];
    let arg2_obj = arg2.as_object().unwrap();
    assert_eq!(arg2_obj.get("name").unwrap().as_str().unwrap(), "strict");
    assert_eq!(arg2_obj.get("type").unwrap().as_str().unwrap(), "boolean");
    assert_eq!(arg2_obj.get("required").unwrap().as_bool().unwrap(), false);
    assert_eq!(arg2_obj.get("default").unwrap().as_str().unwrap(), "false");
}

#[test]
fn test_generate_mcp_context_with_resources() {
    // Arrange
    let turtle_content = r#"
        @prefix ctx: <http://ggen.dev/mcp/context/> .
        @prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

        ctx:server a ctx:McpServer ;
            ctx:serverName "resource-server" ;
            ctx:serverVersion "1.0.0" .

        ctx:resource/config a ctx:Resource ;
            ctx:resourceName "config" ;
            ctx:resourceUri "config://current" ;
            ctx:resourceDescription "Current ggen.toml configuration" ;
            ctx:mimeType "application/toml" ;
            ctx:implementedBy "ggen_read_config" .
    "#;

    let (_temp_dir, ontology_path) = create_temp_ontology(turtle_content);

    // Act
    let result = mcp_generate::generate_mcp_context(&ontology_path, None);

    // Assert
    assert!(result.is_ok(), "generate_mcp_context should succeed");
    let context = result.unwrap();

    assert!(context.contains_key("resources"));
    let resources = context.get("resources").unwrap().as_array().unwrap();
    assert_eq!(resources.len(), 1);

    let resource = &resources[0];
    let resource_obj = resource.as_object().unwrap();
    assert_eq!(resource_obj.get("name").unwrap().as_str().unwrap(), "config");
    assert_eq!(
        resource_obj.get("uri").unwrap().as_str().unwrap(),
        "config://current"
    );
    assert_eq!(
        resource_obj.get("mime_type").unwrap().as_str().unwrap(),
        "application/toml"
    );
    assert_eq!(
        resource_obj.get("implemented_by").unwrap().as_str().unwrap(),
        "ggen_read_config"
    );
}

#[test]
fn test_generate_mcp_context_with_prompts() {
    // Arrange
    let turtle_content = r#"
        @prefix ctx: <http://ggen.dev/mcp/context/> .
        @prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

        ctx:server a ctx:McpServer ;
            ctx:serverName "prompt-server" ;
            ctx:serverVersion "1.0.0" .

        ctx:prompt/generate_code a ctx:Prompt ;
            ctx:promptName "generate_code" ;
            ctx:promptDescription "Generate code from specification" ;
            ctx:implementedBy "ggen_generate_code" .

        ctx:prompt/generate_code/arg/specification a ctx:PromptArgument ;
            ctx:belongsToPrompt "generate_code" ;
            ctx:promptArgumentName "specification" ;
            ctx:promptArgumentType "string" ;
            ctx:promptArgumentDescription "Specification in Turtle format" ;
            ctx:promptIsRequired "true"^^xsd:boolean .
    "#;

    let (_temp_dir, ontology_path) = create_temp_ontology(turtle_content);

    // Act
    let result = mcp_generate::generate_mcp_context(&ontology_path, None);

    // Assert
    assert!(result.is_ok(), "generate_mcp_context should succeed");
    let context = result.unwrap();

    assert!(context.contains_key("prompts"));
    let prompts = context.get("prompts").unwrap().as_array().unwrap();
    assert_eq!(prompts.len(), 1);

    let prompt = &prompts[0];
    let prompt_obj = prompt.as_object().unwrap();
    assert_eq!(
        prompt_obj.get("name").unwrap().as_str().unwrap(),
        "generate_code"
    );
    assert!(prompt_obj.contains_key("arguments"));

    let args = prompt_obj.get("arguments").unwrap().as_array().unwrap();
    assert_eq!(args.len(), 1);

    let arg = &args[0];
    let arg_obj = arg.as_object().unwrap();
    assert_eq!(
        arg_obj.get("name").unwrap().as_str().unwrap(),
        "specification"
    );
    assert_eq!(arg_obj.get("type").unwrap().as_str().unwrap(), "string");
    assert_eq!(arg_obj.get("required").unwrap().as_bool().unwrap(), true);
}

#[test]
fn test_generate_mcp_context_with_capabilities() {
    // Arrange
    let turtle_content = r#"
        @prefix ctx: <http://ggen.dev/mcp/context/> .
        @prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

        ctx:server a ctx:McpServer ;
            ctx:serverName "cap-server" ;
            ctx:serverVersion "1.0.0" .

        ctx:server/capabilities a ctx:CapabilitySet ;
            ctx:hasTools "true"^^xsd:boolean ;
            ctx:hasResources "false"^^xsd:boolean ;
            ctx:hasPrompts "true"^^xsd:boolean ;
            ctx:hasCompletions "false"^^xsd:boolean ;
            ctx:hasLogging "true"^^xsd:boolean ;
            ctx:hasSubscriptions "false"^^xsd:boolean .
    "#;

    let (_temp_dir, ontology_path) = create_temp_ontology(turtle_content);

    // Act
    let result = mcp_generate::generate_mcp_context(&ontology_path, None);

    // Assert
    assert!(result.is_ok(), "generate_mcp_context should succeed");
    let context = result.unwrap();

    assert!(context.contains_key("capabilities"));
    let capabilities = context.get("capabilities").unwrap().as_object().unwrap();
    assert_eq!(capabilities.get("tools").unwrap().as_str().unwrap(), "true");
    assert_eq!(capabilities.get("resources").unwrap().as_str().unwrap(), "false");
    assert_eq!(capabilities.get("prompts").unwrap().as_str().unwrap(), "true");
    assert_eq!(
        capabilities.get("completions").unwrap().as_str().unwrap(),
        "false"
    );
    assert_eq!(capabilities.get("logging").unwrap().as_str().unwrap(), "true");
    assert_eq!(
        capabilities.get("subscriptions").unwrap().as_str().unwrap(),
        "false"
    );
}

#[test]
fn test_generate_mcp_context_with_transport() {
    // Arrange
    let turtle_content = r#"
        @prefix ctx: <http://ggen.dev/mcp/context/> .

        ctx:server a ctx:McpServer ;
            ctx:serverName "transport-server" ;
            ctx:serverVersion "1.0.0" .

        ctx:server/transport a ctx:Transport ;
            ctx:transportName "stdio" ;
            ctx:transportDescription "Standard input/output transport" .
    "#;

    let (_temp_dir, ontology_path) = create_temp_ontology(turtle_content);

    // Act
    let result = mcp_generate::generate_mcp_context(&ontology_path, None);

    // Assert
    assert!(result.is_ok(), "generate_mcp_context should succeed");
    let context = result.unwrap();

    assert!(context.contains_key("transport"));
    let transport = context.get("transport").unwrap().as_object().unwrap();
    assert_eq!(transport.get("name").unwrap().as_str().unwrap(), "stdio");
    assert_eq!(
        transport.get("description").unwrap().as_str().unwrap(),
        "Standard input/output transport"
    );
}

#[test]
fn test_generate_mcp_context_with_logging() {
    // Arrange
    let turtle_content = r#"
        @prefix ctx: <http://ggen.dev/mcp/context/> .
        @prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

        ctx:server a ctx:McpServer ;
            ctx:serverName "logging-server" ;
            ctx:serverVersion "1.0.0" .

        ctx:server/logging a ctx:LoggingPolicy ;
            ctx:defaultLevel "info" ;
            ctx:supportedLevels "debug,info,warning,error" .
    "#;

    let (_temp_dir, ontology_path) = create_temp_ontology(turtle_content);

    // Act
    let result = mcp_generate::generate_mcp_context(&ontology_path, None);

    // Assert
    assert!(result.is_ok(), "generate_mcp_context should succeed");
    let context = result.unwrap();

    assert!(context.contains_key("logging"));
    let logging = context.get("logging").unwrap().as_object().unwrap();
    assert_eq!(
        logging.get("default_level").unwrap().as_str().unwrap(),
        "info"
    );

    let supported_levels = logging
        .get("supported_levels")
        .unwrap()
        .as_array()
        .unwrap();
    assert_eq!(supported_levels.len(), 4);
    assert_eq!(supported_levels[0].as_str().unwrap(), "debug");
    assert_eq!(supported_levels[1].as_str().unwrap(), "info");
    assert_eq!(supported_levels[2].as_str().unwrap(), "warning");
    assert_eq!(supported_levels[3].as_str().unwrap(), "error");
}

#[test]
fn test_generate_mcp_context_with_custom_query() {
    // Arrange
    let turtle_content = r#"
        @prefix ctx: <http://ggen.dev/mcp/context/> .

        ctx:server a ctx:McpServer ;
            ctx:serverName "query-test-server" ;
            ctx:serverVersion "1.0.0" ;
            ctx:serverDescription "Testing custom SPARQL query" .
    "#;

    let query_content = r#"
        PREFIX ctx: <http://ggen.dev/mcp/context/>
        CONSTRUCT {
            ?s ?p ?o .
        }
        WHERE {
            ?s ?p ?o .
            FILTER(?s = ctx:server)
        }
    "#;

    let (_ontology_dir, ontology_path) = create_temp_ontology(turtle_content);
    let (_query_dir, query_path) = create_temp_query(query_content);

    // Act
    let result =
        mcp_generate::generate_mcp_context(&ontology_path, Some(query_path.as_path()));

    // Assert
    assert!(result.is_ok(), "generate_mcp_context with custom query should succeed");
    let context = result.unwrap();

    // Should only have server info due to FILTER
    assert!(context.contains_key("server"));
    assert!(!context.contains_key("tools"));
    assert!(!context.contains_key("resources"));
}

#[test]
fn test_generate_mcp_context_file_not_found() {
    // Arrange
    let nonexistent_path = PathBuf::from("/nonexistent/path/to/ontology.ttl");

    // Act
    let result = mcp_generate::generate_mcp_context(&nonexistent_path, None);

    // Assert
    assert!(result.is_err(), "Should return error for nonexistent file");
    let error_msg = result.unwrap_err().to_string().to_lowercase();
    assert!(
        error_msg.contains("failed to open") || error_msg.contains("no such file"),
        "Error should mention file not found: {}",
        error_msg
    );
}

#[test]
fn test_generate_mcp_context_invalid_turtle() {
    // Arrange
    let invalid_turtle = "This is not valid Turtle syntax at all!";

    let (_temp_dir, ontology_path) = create_temp_ontology(invalid_turtle);

    // Act
    let result = mcp_generate::generate_mcp_context(&ontology_path, None);

    // Assert
    assert!(result.is_err(), "Should return error for invalid Turtle syntax");
    let error_msg = result.unwrap_err().to_string().to_lowercase();
    assert!(
        error_msg.contains("parse") || error_msg.contains("turtle"),
        "Error should mention parsing failure: {}",
        error_msg
    );
}

#[test]
fn test_convert_quads_to_context_complete_server() {
    // Arrange - Complete MCP server with all components
    use oxigraph::io::RdfFormat;
    use oxigraph::store::Store;

    let turtle_content = r#"
        @prefix ctx: <http://ggen.dev/mcp/context/> .
        @prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

        ctx:server a ctx:McpServer ;
            ctx:serverName "complete-server" ;
            ctx:serverVersion "3.0.0" ;
            ctx:serverDescription "Complete MCP server with all features" ;
            ctx:protocolVersion "2024-11-05" .

        ctx:server/capabilities a ctx:CapabilitySet ;
            ctx:hasTools "true"^^xsd:boolean ;
            ctx:hasResources "true"^^xsd:boolean ;
            ctx:hasPrompts "true"^^xsd:boolean .

        ctx:server/transport a ctx:Transport ;
            ctx:transportName "stdio" ;
            ctx:transportDescription "Standard I/O transport" .

        ctx:server/logging a ctx:LoggingPolicy ;
            ctx:defaultLevel "debug" ;
            ctx:supportedLevels "debug,info,warning,error,critical" .

        ctx:tool/example a ctx:Tool ;
            ctx:toolName "example" ;
            ctx:toolDescription "Example tool" ;
            ctx:implementedBy "ggen_example" .

        ctx:resource/data a ctx:Resource ;
            ctx:resourceName "data" ;
            ctx:resourceUri "data://example" ;
            ctx:resourceDescription "Example resource" ;
            ctx:mimeType "application/json" .
    "#;

    let store = Store::new().expect("Failed to create store");
    store
        .load_from_reader(
            oxigraph::io::RdfParser::from_format(RdfFormat::Turtle),
            turtle_content.as_bytes(),
        )
        .expect("Failed to load Turtle");

    // Act
    #[allow(deprecated)]
    let query = r#"
        PREFIX ctx: <http://ggen.dev/mcp/context/>
        CONSTRUCT { ?s ?p ?o }
        WHERE { ?s ?p ?o }
    "#;
    #[allow(deprecated)]
    let query_results = store.query(query).expect("Failed to execute query");
    let result = mcp_generate::convert_quads_to_context(&store, query_results);

    // Assert
    assert!(result.is_ok());
    let context = result.unwrap();

    // Verify all components are present
    assert!(context.contains_key("server"));
    assert!(context.contains_key("capabilities"));
    assert!(context.contains_key("transport"));
    assert!(context.contains_key("logging"));
    assert!(context.contains_key("tools"));
    assert!(context.contains_key("resources"));

    // Verify server metadata
    let server = context.get("server").unwrap().as_object().unwrap();
    assert_eq!(server.get("name").unwrap().as_str().unwrap(), "complete-server");
    assert_eq!(server.get("version").unwrap().as_str().unwrap(), "3.0.0");

    // Verify tools
    let tools = context.get("tools").unwrap().as_array().unwrap();
    assert_eq!(tools.len(), 1);

    // Verify resources
    let resources = context.get("resources").unwrap().as_array().unwrap();
    assert_eq!(resources.len(), 1);
}

#[test]
fn test_generate_mcp_context_empty_ontology() {
    // Arrange
    let empty_turtle = r#"
        @prefix ctx: <http://ggen.dev/mcp/context/> .
        # Empty ontology with no server definition
    "#;

    let (_temp_dir, ontology_path) = create_temp_ontology(empty_turtle);

    // Act
    let result = mcp_generate::generate_mcp_context(&ontology_path, None);

    // Assert - Should succeed but with empty context
    assert!(result.is_ok(), "generate_mcp_context should succeed even with empty ontology");
    let context = result.unwrap();

    // Context should be empty or have minimal structure
    assert!(!context.contains_key("server"), "Empty ontology should not produce server node");
}
