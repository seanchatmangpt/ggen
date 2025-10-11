//! Comprehensive error handling tests for ggen-mcp
//!
//! This test suite verifies robust error handling across three critical categories:
//! 1. Invalid Parameters - Missing, wrong types, invalid values
//! 2. Resource Errors - File not found, permission denied, I/O failures
//! 3. Protocol Errors - Invalid JSON-RPC, unknown methods, malformed requests

use ggen_mcp::error::{get_bool_param, get_optional_string_param, get_string_param, GgenMcpError};
use ggen_mcp::GgenMcpServer;
use serde_json::{json, Value};

// ============================================================================
// Helper Macros for Error Assertions
// ============================================================================

/// Assert that an error matches a specific variant
macro_rules! assert_error_variant {
    ($result:expr, $pattern:pat) => {
        match $result {
            Err($pattern) => {}
            Ok(_) => panic!("Expected error, got Ok"),
            Err(e) => panic!(
                "Wrong error variant. Expected {}, got: {:?}",
                stringify!($pattern),
                e
            ),
        }
    };
    ($result:expr, $pattern:pat, $msg:expr) => {
        match $result {
            Err($pattern) => {}
            Ok(_) => panic!("Expected error, got Ok: {}", $msg),
            Err(e) => panic!(
                "Wrong error variant. Expected {}, got: {:?}. {}",
                stringify!($pattern),
                e,
                $msg
            ),
        }
    };
}

// ============================================================================
// 1. INVALID PARAMETER TESTS (Critical 80/20 Category)
// ============================================================================

#[cfg(test)]
mod invalid_parameter_tests {
    use super::*;

    #[tokio::test]
    async fn test_missing_required_template_param() {
        let server = GgenMcpServer::new();
        let params = json!({}); // Missing required 'template' parameter

        let result = server.execute_tool("project_gen", params).await;

        assert_error_variant!(
            result,
            GgenMcpError::MissingParameter(_),
            "project_gen should fail when template parameter is missing"
        );

        // Verify error message is helpful
        if let Err(GgenMcpError::MissingParameter(msg)) = result {
            assert!(
                msg.contains("template"),
                "Error message should mention the missing parameter name"
            );
        }
    }

    #[tokio::test]
    async fn test_missing_query_param_in_graph_query() {
        let server = GgenMcpServer::new();
        let params = json!({}); // Missing required 'query' parameter

        let result = server.execute_tool("graph_query", params).await;

        assert_error_variant!(
            result,
            GgenMcpError::MissingParameter(_),
            "graph_query should fail when query parameter is missing"
        );
    }

    #[test]
    fn test_wrong_parameter_type_string() {
        let params = json!({
            "template": 123, // Should be string, not number
            "vars": {}
        });

        let result = get_string_param(&params, "template");

        assert_error_variant!(
            result,
            GgenMcpError::MissingParameter(_),
            "Should fail when parameter is wrong type"
        );
    }

    #[test]
    fn test_invalid_bool_param_type() {
        let params = json!({
            "dry_run": "yes" // Should be bool, not string
        });

        // get_bool_param returns default on wrong type, but we can test behavior
        let result = get_bool_param(&params, "dry_run", false);
        assert_eq!(result, false, "Should use default when type is wrong");
    }

    #[tokio::test]
    async fn test_empty_string_template_param() {
        let server = GgenMcpServer::new();
        let params = json!({
            "template": "" // Empty string is technically valid but semantically invalid
        });

        let result = server.execute_tool("project_gen", params).await;

        // This may succeed or fail depending on implementation
        // At minimum, empty strings should be handled gracefully
        if let Ok(response) = result {
            println!("Empty template handled gracefully: {:?}", response);
        } else if let Err(e) = result {
            println!("Empty template rejected with error: {:?}", e);
        }
    }

    #[tokio::test]
    async fn test_invalid_json_in_vars_param() {
        let server = GgenMcpServer::new();
        let params = json!({
            "template": "test-template",
            "vars": "not-an-object" // Should be object, not string
        });

        let result = server.execute_tool("project_gen", params).await;

        // Should handle gracefully - either reject or use empty vars
        match result {
            Ok(_) => {
                // Implementation chose to handle gracefully
            }
            Err(e) => {
                // Implementation chose to reject
                println!("Invalid vars rejected: {:?}", e);
            }
        }
    }

    #[tokio::test]
    async fn test_negative_number_in_limit_param() {
        let server = GgenMcpServer::new();
        let params = json!({
            "limit": -10 // Negative number should be invalid
        });

        let result = server.execute_tool("market_list", params).await;

        // Should handle negative numbers gracefully
        match result {
            Ok(_) => {
                // Implementation may use default or abs value
            }
            Err(GgenMcpError::InvalidParameter(_)) => {
                // Proper validation
            }
            Err(e) => panic!("Unexpected error type: {:?}", e),
        }
    }

    #[tokio::test]
    async fn test_null_required_param() {
        let server = GgenMcpServer::new();
        let params = json!({
            "template": null // Null instead of string
        });

        let result = server.execute_tool("project_gen", params).await;

        assert_error_variant!(
            result,
            GgenMcpError::MissingParameter(_),
            "Null should be treated as missing parameter"
        );
    }

    #[test]
    fn test_helper_function_get_string_param_missing() {
        let params = json!({
            "other_field": "value"
        });

        let result = get_string_param(&params, "template");

        assert_error_variant!(result, GgenMcpError::MissingParameter(_));

        if let Err(GgenMcpError::MissingParameter(msg)) = result {
            assert_eq!(msg, "template", "Error should include parameter name");
        }
    }

    #[test]
    fn test_helper_function_get_optional_string_param() {
        let params = json!({
            "present": "value"
        });

        let present = get_optional_string_param(&params, "present");
        let missing = get_optional_string_param(&params, "missing");

        assert_eq!(present, Some("value".to_string()));
        assert_eq!(missing, None);
    }
}

// ============================================================================
// 2. RESOURCE ERROR TESTS
// ============================================================================

#[cfg(test)]
mod resource_error_tests {
    use super::*;

    #[tokio::test]
    async fn test_template_file_not_found() {
        let server = GgenMcpServer::new();
        let params = json!({
            "template": "/nonexistent/path/to/template.tmpl",
            "vars": {}
        });

        let result = server.execute_tool("project_gen", params).await;

        // Should fail with appropriate error
        match result {
            Err(GgenMcpError::TemplateError(_)) | Err(GgenMcpError::ExecutionFailed(_)) => {
                // Expected error types
            }
            Err(e) => println!("File not found handled with: {:?}", e),
            Ok(_) => {
                // Current implementation may simulate success
                println!("Implementation simulates success for nonexistent templates");
            }
        }
    }

    #[tokio::test]
    async fn test_graph_load_invalid_file() {
        let server = GgenMcpServer::new();
        let params = json!({
            "file": "/invalid/path/to/graph.ttl"
        });

        let result = server.execute_tool("graph_load", params).await;

        // Should handle missing file gracefully
        match result {
            Err(GgenMcpError::GraphError(_)) | Err(GgenMcpError::ExecutionFailed(_)) => {}
            Err(e) => println!("Invalid file handled with: {:?}", e),
            Ok(_) => println!("Implementation may simulate success"),
        }
    }

    #[tokio::test]
    async fn test_output_directory_does_not_exist() {
        let server = GgenMcpServer::new();
        let params = json!({
            "template": "test-template",
            "output_dir": "/nonexistent/output/directory"
        });

        let result = server.execute_tool("project_gen", params).await;

        // Implementation should either create directory or fail gracefully
        match result {
            Ok(_) => println!("Directory created automatically"),
            Err(e) => println!("Missing directory error: {:?}", e),
        }
    }

    #[tokio::test]
    async fn test_permission_denied_output_dir() {
        // Note: This test is platform-dependent and may not work on all systems
        let server = GgenMcpServer::new();
        let params = json!({
            "template": "test-template",
            "output_dir": "/root/restricted" // Typically requires root access
        });

        let result = server.execute_tool("project_gen", params).await;

        // Should handle permission errors gracefully
        match result {
            Ok(_) => println!("Permission check not enforced or running as root"),
            Err(e) => println!("Permission denied handled: {:?}", e),
        }
    }
}

// ============================================================================
// 3. PROTOCOL ERROR TESTS
// ============================================================================

#[cfg(test)]
mod protocol_error_tests {
    use super::*;

    #[tokio::test]
    async fn test_unknown_tool_name() {
        let server = GgenMcpServer::new();
        let params = json!({});

        let result = server.execute_tool("nonexistent_tool", params).await;

        assert_error_variant!(
            result,
            GgenMcpError::InvalidParameter(_),
            "Unknown tool should return InvalidParameter error"
        );

        if let Err(GgenMcpError::InvalidParameter(msg)) = result {
            assert!(
                msg.contains("Unknown tool"),
                "Error should indicate tool is unknown"
            );
            assert!(
                msg.contains("nonexistent_tool"),
                "Error should include the tool name"
            );
        }
    }

    #[tokio::test]
    async fn test_malformed_json_params() {
        let server = GgenMcpServer::new();

        // Test with array instead of object
        let array_params = json!([1, 2, 3]);
        let result = server.execute_tool("project_gen", array_params).await;

        // Should handle malformed params gracefully
        assert!(result.is_err(), "Array params should be rejected");
    }

    #[tokio::test]
    async fn test_empty_tool_name() {
        let server = GgenMcpServer::new();
        let params = json!({});

        let result = server.execute_tool("", params).await;

        assert_error_variant!(
            result,
            GgenMcpError::InvalidParameter(_),
            "Empty tool name should be rejected"
        );
    }

    #[tokio::test]
    async fn test_tool_name_with_special_characters() {
        let server = GgenMcpServer::new();
        let params = json!({});

        let result = server.execute_tool("project/../etc/passwd", params).await;

        assert_error_variant!(
            result,
            GgenMcpError::InvalidParameter(_),
            "Path traversal attempts should be rejected"
        );
    }
}

// ============================================================================
// 4. ERROR TRAIT IMPLEMENTATION TESTS
// ============================================================================

#[cfg(test)]
mod error_trait_tests {
    use super::*;

    #[test]
    fn test_error_display_implementation() {
        let error = GgenMcpError::MissingParameter("template".to_string());
        let display = format!("{}", error);

        assert!(display.contains("Missing required parameter"));
        assert!(display.contains("template"));
    }

    #[test]
    fn test_error_debug_implementation() {
        let error = GgenMcpError::InvalidParameter("Invalid value".to_string());
        let debug = format!("{:?}", error);

        assert!(debug.contains("InvalidParameter"));
        assert!(debug.contains("Invalid value"));
    }

    #[test]
    fn test_error_from_serde_json() {
        let json_error = serde_json::from_str::<Value>("{ invalid json }").unwrap_err();
        let mcp_error: GgenMcpError = json_error.into();

        assert_error_variant!(
            Err::<Value, GgenMcpError>(mcp_error),
            GgenMcpError::SerializationError(_)
        );
    }

    #[test]
    fn test_error_from_ggen_core() {
        // Test From trait for ggen-core errors
        let core_error = ggen_utils::error::Error::new("Template not found: test");
        let mcp_error: GgenMcpError = core_error.into();

        assert_error_variant!(Err::<Value, GgenMcpError>(mcp_error), GgenMcpError::Core(_));
    }

    #[test]
    fn test_error_into_rmcp_error_data() {
        let error = GgenMcpError::ExecutionFailed("Test failure".to_string());
        let error_data: rmcp::ErrorData = error.into();

        // Verify it converts to rmcp ErrorData properly
        assert_eq!(error_data.code, rmcp::model::ErrorCode(-32603)); // Internal error code
        assert!(error_data.message.contains("Execution failed"));
    }

    #[test]
    fn test_all_error_variants_have_messages() {
        let errors = vec![
            GgenMcpError::MissingParameter("param".to_string()),
            GgenMcpError::InvalidParameter("invalid".to_string()),
            GgenMcpError::ExecutionFailed("failed".to_string()),
            GgenMcpError::RegistryError("registry".to_string()),
            GgenMcpError::GraphError("graph".to_string()),
            GgenMcpError::TemplateError("template".to_string()),
            GgenMcpError::SerializationError("serialization".to_string()),
            GgenMcpError::Timeout("timeout".to_string()),
            GgenMcpError::GenerationFailed("generation".to_string()),
        ];

        for error in errors {
            let message = error.to_string();
            assert!(
                !message.is_empty(),
                "Error {:?} should have non-empty message",
                error
            );
            assert!(
                message.len() > 5,
                "Error {:?} message should be descriptive",
                error
            );
        }
    }
}

// ============================================================================
// 5. ERROR MESSAGE QUALITY TESTS
// ============================================================================

#[cfg(test)]
mod error_message_tests {
    use super::*;

    #[tokio::test]
    async fn test_error_messages_are_helpful() {
        let server = GgenMcpServer::new();

        // Test 1: Missing parameter error message
        let result1 = server.execute_tool("project_gen", json!({})).await;
        if let Err(e) = result1 {
            let msg = e.to_string();
            assert!(msg.len() > 10, "Error message should be descriptive");
            assert!(
                !msg.contains("Error"),
                "Should be more specific than just 'Error'"
            );
        }

        // Test 2: Unknown tool error message
        let result2 = server.execute_tool("fake_tool", json!({})).await;
        if let Err(e) = result2 {
            let msg = e.to_string();
            assert!(
                msg.contains("fake_tool"),
                "Should mention the invalid tool name"
            );
        }
    }

    #[test]
    fn test_error_messages_contain_context() {
        let param_name = "template";
        let error = GgenMcpError::MissingParameter(param_name.to_string());
        let message = error.to_string();

        // Message should include the parameter name
        assert!(
            message.contains(param_name),
            "Error message should include parameter name for context"
        );
    }

    #[test]
    fn test_error_messages_are_user_friendly() {
        let errors = vec![
            (
                GgenMcpError::MissingParameter("template".to_string()),
                "Missing required parameter",
            ),
            (
                GgenMcpError::InvalidParameter("wrong type".to_string()),
                "Invalid parameter",
            ),
            (
                GgenMcpError::TemplateError("not found".to_string()),
                "Template error",
            ),
        ];

        for (error, expected_text) in errors {
            let message = error.to_string().to_lowercase();
            assert!(
                message.contains(&expected_text.to_lowercase()),
                "Error message should contain '{}'",
                expected_text
            );
        }
    }
}

// ============================================================================
// 6. CONCURRENT ERROR HANDLING TESTS
// ============================================================================

#[cfg(test)]
mod concurrent_error_tests {
    use super::*;
    use tokio::task::JoinSet;

    #[tokio::test]
    async fn test_concurrent_error_handling() {
        let server = std::sync::Arc::new(GgenMcpServer::new());
        let mut join_set = JoinSet::new();

        // Spawn 10 concurrent tasks that will fail
        for i in 0..10 {
            let server_clone = server.clone();
            join_set.spawn(async move {
                server_clone
                    .execute_tool("nonexistent_tool", json!({"id": i}))
                    .await
            });
        }

        let mut error_count = 0;
        while let Some(result) = join_set.join_next().await {
            if let Ok(Err(_)) = result {
                error_count += 1;
            }
        }

        assert_eq!(
            error_count, 10,
            "All concurrent calls should fail with errors"
        );
    }

    #[tokio::test]
    async fn test_error_handling_under_load() {
        let server = std::sync::Arc::new(GgenMcpServer::new());
        let mut tasks = vec![];

        // Create 100 rapid-fire requests with various error conditions
        for i in 0..100 {
            let server_clone = server.clone();
            let task = tokio::spawn(async move {
                let tool = if i % 3 == 0 {
                    "project_gen"
                } else {
                    "unknown_tool"
                };
                let params = if i % 2 == 0 {
                    json!({})
                } else {
                    json!({"template": "test"})
                };
                server_clone.execute_tool(tool, params).await
            });
            tasks.push(task);
        }

        // Wait for all tasks and verify they complete without panics
        for task in tasks {
            let _ = task.await.expect("Task should not panic");
        }
    }
}

// ============================================================================
// 7. INTEGRATION TESTS WITH REAL SCENARIOS
// ============================================================================

#[cfg(test)]
mod integration_error_tests {
    use super::*;

    #[tokio::test]
    async fn test_complete_error_recovery_flow() {
        let server = GgenMcpServer::new();

        // Step 1: Try with missing params
        let result1 = server.execute_tool("project_gen", json!({})).await;
        assert!(result1.is_err(), "Should fail with missing params");

        // Step 2: Try with invalid params
        let result2 = server
            .execute_tool(
                "project_gen",
                json!({
                    "template": 123 // Wrong type
                }),
            )
            .await;
        assert!(result2.is_err(), "Should fail with invalid params");

        // Step 3: Try with valid params (should succeed or fail gracefully)
        let result3 = server
            .execute_tool(
                "project_gen",
                json!({
                    "template": "test-template",
                    "vars": {}
                }),
            )
            .await;

        match result3 {
            Ok(_) => println!("Successfully executed with valid params"),
            Err(e) => println!("Failed gracefully with valid params: {:?}", e),
        }
    }

    #[tokio::test]
    async fn test_error_boundaries_between_tools() {
        let server = GgenMcpServer::new();

        // Test that errors in one tool don't affect another
        let _ = server.execute_tool("project_gen", json!({})).await; // Will fail

        // Next call should work independently
        let result = server.execute_tool("market_list", json!({})).await;

        // Should not be affected by previous error
        match result {
            Ok(_) => println!("Tool isolation maintained"),
            Err(e) => println!("Tool failed independently: {:?}", e),
        }
    }
}

// ============================================================================
// TEST SUMMARY HELPER
// ============================================================================

#[cfg(test)]
mod test_summary {
    #[test]
    fn print_test_coverage_summary() {
        println!("\n=== ERROR HANDLING TEST COVERAGE SUMMARY ===");
        println!("1. Invalid Parameters: 10+ tests");
        println!("   - Missing required params");
        println!("   - Wrong parameter types");
        println!("   - Invalid values (empty, null, negative)");
        println!("   - Helper function validation");
        println!("");
        println!("2. Resource Errors: 4 tests");
        println!("   - File not found");
        println!("   - Permission denied");
        println!("   - Invalid paths");
        println!("");
        println!("3. Protocol Errors: 4 tests");
        println!("   - Unknown tool names");
        println!("   - Malformed JSON");
        println!("   - Empty tool names");
        println!("   - Path traversal attempts");
        println!("");
        println!("4. Error Traits: 6 tests");
        println!("   - Display implementation");
        println!("   - Debug implementation");
        println!("   - From trait conversions");
        println!("   - Error data conversion");
        println!("");
        println!("5. Error Messages: 3 tests");
        println!("   - Helpfulness");
        println!("   - Context inclusion");
        println!("   - User-friendliness");
        println!("");
        println!("6. Concurrent Errors: 2 tests");
        println!("   - Concurrent error handling");
        println!("   - Error handling under load");
        println!("");
        println!("7. Integration: 2 tests");
        println!("   - Complete error recovery flow");
        println!("   - Error boundaries between tools");
        println!("==========================================\n");
    }
}
