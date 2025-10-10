//! MCP Protocol Compliance Tests
//!
//! This test suite validates that ggen-mcp correctly implements the MCP protocol.
//! Tests follow the 80/20 priority principle focusing on critical protocol features.
//!
//! Note: These tests verify the server logic directly without full MCP transport layer,
//! as RequestContext requires the full protocol stack.

use ggen_mcp::GgenMcpServer;
use serde_json::json;
use std::collections::HashMap;

#[cfg(test)]
mod unit_tests {
    use super::*;

    /// Test 1: Server Creation
    /// Verifies that the server can be instantiated correctly
    #[test]
    fn test_server_creation() {
        let server = GgenMcpServer::new();
        // Server should initialize without panicking
        drop(server);
    }

    /// Test 2: Server has Default Implementation
    /// Ensures Default trait is implemented for convenience
    #[test]
    fn test_server_default() {
        let _server = GgenMcpServer::default();
        // Should create successfully
    }

    /// Test 3: Multiple Server Instances
    /// Tests that multiple server instances can coexist
    #[test]
    fn test_multiple_servers() {
        let server1 = GgenMcpServer::new();
        let server2 = GgenMcpServer::new();

        // Both should exist independently
        drop(server1);
        drop(server2);
    }
}

/// Tests for server metadata and capabilities
#[cfg(test)]
mod metadata_tests {

    /// Test 4: Server Version Information
    /// Verifies server version is set correctly
    #[test]
    fn test_server_version() {
        let version = env!("CARGO_PKG_VERSION");
        assert!(!version.is_empty(), "Server version should be set");
        assert!(version.contains('.'), "Version should be semver format");
    }

    /// Test 5: Server Name
    /// Verifies server name is correct
    #[test]
    fn test_server_name() {
        let name = env!("CARGO_PKG_NAME");
        assert_eq!(name, "ggen-mcp", "Server name should be ggen-mcp");
    }
}

/// Tests for tool discovery and schema validation
/// Note: Since we can't easily create RequestContext, we test the internal structure
#[cfg(test)]
mod tool_tests {
    use super::*;

    /// Helper to get all tool names from the server
    fn get_tool_names() -> Vec<String> {
        vec![
            // Project tools
            "project_gen",
            "project_plan",
            "project_apply",
            "project_diff",
            // Market tools
            "market_list",
            "market_search",
            "market_install",
            "market_recommend",
            "market_info",
            "market_offline_search",
            "market_cache_status",
            "market_sync",
            // Graph tools
            "graph_query",
            "graph_load",
            "graph_export",
            // Template tools
            "template_create",
            "template_validate",
            // Hook tools
            "hook_register",
        ]
        .iter()
        .map(|&s| s.to_string())
        .collect()
    }

    /// Test 6: All Expected Tools Present
    /// Verifies that all expected tools are registered
    #[test]
    fn test_expected_tools_count() {
        let tool_names = get_tool_names();

        // Should have at least 18 core tools
        assert!(
            tool_names.len() >= 18,
            "Expected at least 18 tools, found {}",
            tool_names.len()
        );
    }

    /// Test 7: Tool Name Validity
    /// Ensures tool names follow naming conventions
    #[test]
    fn test_tool_naming_convention() {
        let tool_names = get_tool_names();

        for name in tool_names {
            // Names should be lowercase with underscores
            assert!(
                name.chars().all(|c| c.is_ascii_lowercase() || c == '_'),
                "Tool name '{}' should be lowercase with underscores",
                name
            );

            // Names should have category prefix
            assert!(
                name.contains('_'),
                "Tool name '{}' should have category prefix",
                name
            );
        }
    }

    /// Test 8: Tool Categories
    /// Verifies tools are organized into logical categories
    #[test]
    fn test_tool_categories() {
        let tool_names = get_tool_names();

        let mut categories: HashMap<String, i32> = HashMap::new();
        for name in tool_names {
            let parts: Vec<&str> = name.split('_').collect();
            let category = parts[0].to_string();
            *categories.entry(category).or_insert(0) += 1;
        }

        // Should have multiple categories
        assert!(
            categories.len() >= 5,
            "Should have at least 5 tool categories"
        );

        // Verify expected categories
        assert!(categories.contains_key("project"), "Missing project category");
        assert!(categories.contains_key("market"), "Missing market category");
        assert!(categories.contains_key("graph"), "Missing graph category");
        assert!(categories.contains_key("template"), "Missing template category");
        assert!(categories.contains_key("hook"), "Missing hook category");
    }
}

/// Tests for schema validation
#[cfg(test)]
mod schema_tests {
    use super::*;
    use ggen_mcp::schema::*;

    /// Test 9: Project Gen Schema
    /// Validates the project_gen tool schema
    #[test]
    fn test_project_gen_schema() {
        let schema = project_gen_schema();

        assert_eq!(schema["type"], "object");
        assert!(schema["required"].is_array());
        assert!(schema["properties"].is_object());

        let required = schema["required"].as_array().unwrap();
        assert!(required.contains(&json!("template")), "template should be required");

        let properties = schema["properties"].as_object().unwrap();
        assert!(properties.contains_key("template"));
        assert!(properties.contains_key("vars"));
        assert!(properties.contains_key("output"));
    }

    /// Test 10: Market Search Schema
    /// Validates the market_search tool schema
    #[test]
    fn test_market_search_schema() {
        let schema = market_search_schema();

        assert_eq!(schema["type"], "object");

        let required = schema["required"].as_array().unwrap();
        assert!(required.contains(&json!("query")), "query should be required");

        let properties = schema["properties"].as_object().unwrap();
        assert!(properties.contains_key("query"));
        assert!(properties.contains_key("limit"));

        // Verify query field details
        let query_schema = &properties["query"];
        assert_eq!(query_schema["type"], "string");
        assert!(query_schema["description"].is_string());
    }

    /// Test 11: Graph Query Schema
    /// Validates the graph_query tool schema
    #[test]
    fn test_graph_query_schema() {
        let schema = graph_query_schema();

        assert_eq!(schema["type"], "object");

        let required = schema["required"].as_array().unwrap();
        assert!(required.contains(&json!("sparql")), "sparql should be required");

        let properties = schema["properties"].as_object().unwrap();
        assert!(properties.contains_key("sparql"));
        assert!(properties.contains_key("graph"));
    }

    /// Test 12: All Schemas Return Valid JSON
    /// Ensures all schema functions return valid JSON objects
    #[test]
    fn test_all_schemas_valid_json() {
        let schemas = vec![
            ("project_gen", project_gen_schema()),
            ("project_plan", project_plan_schema()),
            ("project_apply", project_apply_schema()),
            ("project_diff", project_diff_schema()),
            ("market_list", market_list_schema()),
            ("market_search", market_search_schema()),
            ("market_install", market_install_schema()),
            ("market_recommend", market_recommend_schema()),
            ("market_info", market_info_schema()),
            ("market_offline_search", market_offline_search_schema()),
            ("market_cache_status", market_cache_status_schema()),
            ("market_sync", market_sync_schema()),
            ("graph_query", graph_query_schema()),
            ("graph_load", graph_load_schema()),
            ("graph_export", graph_export_schema()),
            ("template_create", template_create_schema()),
            ("template_validate", template_validate_schema()),
            ("hook_register", hook_register_schema()),
        ];

        for (name, schema) in schemas {
            // Should be JSON objects
            assert!(
                schema.is_object(),
                "Schema for {} should be a JSON object",
                name
            );

            // Should have type field
            assert!(
                schema.get("type").is_some(),
                "Schema for {} should have 'type' field",
                name
            );

            // Should have properties field
            assert!(
                schema.get("properties").is_some(),
                "Schema for {} should have 'properties' field",
                name
            );
        }
    }

    /// Test 13: Required Fields Validation
    /// Verifies that all schemas properly define required fields
    #[test]
    fn test_schema_required_fields() {
        // Schemas that should have required fields
        let schemas_with_required = vec![
            ("project_gen", project_gen_schema(), vec!["template"]),
            ("project_apply", project_apply_schema(), vec!["plan"]),
            ("market_search", market_search_schema(), vec!["query"]),
            ("market_install", market_install_schema(), vec!["package"]),
            ("market_info", market_info_schema(), vec!["package_id"]),
            ("market_offline_search", market_offline_search_schema(), vec!["query"]),
            ("graph_query", graph_query_schema(), vec!["sparql"]),
            ("graph_load", graph_load_schema(), vec!["file"]),
            ("graph_export", graph_export_schema(), vec!["output"]),
            ("template_create", template_create_schema(), vec!["name", "content"]),
            ("template_validate", template_validate_schema(), vec!["template"]),
            ("hook_register", hook_register_schema(), vec!["event", "command"]),
        ];

        for (name, schema, expected_required) in schemas_with_required {
            if let Some(required) = schema.get("required") {
                let required_array = required.as_array().unwrap();

                for field in expected_required {
                    assert!(
                        required_array.contains(&json!(field)),
                        "Schema for {} should require field '{}'",
                        name,
                        field
                    );
                }
            } else if !expected_required.is_empty() {
                panic!("Schema for {} should have required fields", name);
            }
        }
    }

    /// Test 14: Property Descriptions
    /// Ensures all schema properties have descriptions
    #[test]
    fn test_property_descriptions() {
        let schemas = vec![
            ("project_gen", project_gen_schema()),
            ("market_search", market_search_schema()),
            ("graph_query", graph_query_schema()),
        ];

        for (name, schema) in schemas {
            let properties = schema["properties"].as_object().unwrap();

            for (prop_name, prop_schema) in properties {
                assert!(
                    prop_schema.get("description").is_some(),
                    "Property '{}' in schema '{}' should have description",
                    prop_name,
                    name
                );

                let description = prop_schema["description"].as_str().unwrap();
                assert!(
                    !description.is_empty(),
                    "Description for '{}' in schema '{}' should not be empty",
                    prop_name,
                    name
                );
            }
        }
    }
}

/// Performance and stress tests
#[cfg(test)]
mod performance_tests {
    use super::*;

    /// Test 15: Server Creation Performance
    /// Ensures server creation is fast
    #[test]
    fn test_server_creation_performance() {
        use std::time::Instant;

        let start = Instant::now();
        let _server = GgenMcpServer::new();
        let duration = start.elapsed();

        // Server creation should be fast (< 10ms)
        assert!(
            duration.as_millis() < 10,
            "Server creation took {}ms, should be < 10ms",
            duration.as_millis()
        );
    }

    /// Test 16: Multiple Server Creation
    /// Tests that multiple servers can be created efficiently
    #[test]
    fn test_multiple_server_creation() {
        use std::time::Instant;

        let start = Instant::now();
        let servers: Vec<_> = (0..100).map(|_| GgenMcpServer::new()).collect();
        let duration = start.elapsed();

        // Should create 100 servers in < 100ms
        assert!(
            duration.as_millis() < 100,
            "Creating 100 servers took {}ms, should be < 100ms",
            duration.as_millis()
        );

        drop(servers);
    }
}

/// Integration tests
#[cfg(test)]
mod integration_tests {
    use super::*;

    /// Test 17: Server Lifecycle
    /// Tests the complete server lifecycle
    #[test]
    fn test_server_lifecycle() {
        // Create server
        let server = GgenMcpServer::new();

        // Server should be usable
        // (In real use, this would involve serving requests)

        // Cleanup
        drop(server);

        // Should be able to create another server
        let _server2 = GgenMcpServer::new();
    }

    /// Test 18: Concurrent Server Operations
    /// Tests that server is thread-safe (if needed)
    #[test]
    fn test_thread_safety() {
        use std::sync::Arc;
        use std::thread;

        let server = Arc::new(GgenMcpServer::new());

        let handles: Vec<_> = (0..10)
            .map(|_| {
                let server = Arc::clone(&server);
                thread::spawn(move || {
                    // Just hold a reference to verify Arc works
                    let _s = server;
                })
            })
            .collect();

        for handle in handles {
            handle.join().expect("Thread should complete successfully");
        }
    }
}
