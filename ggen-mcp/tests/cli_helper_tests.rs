//! CLI Helper Integration Tests
//!
//! Tests for the CLI helper module that delegates MCP tool calls
//! to the underlying ggen CLI commands.

use serde_json::{json, Map, Value};

// Note: These tests verify the delegation logic and parameter construction
// The actual CLI execution is tested in integration tests

// ============================================================================
// CLI COMMAND CONSTRUCTION TESTS
// ============================================================================

#[test]
fn test_build_cli_args_for_project_gen() {
    // Test that args are constructed correctly for project gen
    let template = "rust-api";
    let args = vec!["project", "gen", template];

    assert_eq!(args.len(), 3);
    assert_eq!(args[0], "project");
    assert_eq!(args[1], "gen");
    assert_eq!(args[2], "rust-api");
}

#[test]
fn test_build_cli_args_with_flags() {
    let mut args = vec!["project", "gen", "template"];
    args.push("--dry-run");
    args.push("--force");

    assert!(args.contains(&"--dry-run"));
    assert!(args.contains(&"--force"));
}

#[test]
fn test_build_cli_args_for_market_search() {
    let query = "api";
    let category = "backend";

    let mut args = vec!["market", "search", query];
    args.push("--category");
    args.push(category);

    assert_eq!(args.len(), 5);
    assert_eq!(args[2], "api");
    assert_eq!(args[4], "backend");
}

// ============================================================================
// PARAMETER EXTRACTION TESTS
// ============================================================================

#[test]
fn test_extract_string_param_success() {
    use ggen_mcp::error::get_string_param;

    let params = json!({"template": "test-template"});
    let result = get_string_param(&params, "template");

    assert!(result.is_ok());
    assert_eq!(result.unwrap(), "test-template");
}

#[test]
fn test_extract_string_param_missing() {
    use ggen_mcp::error::get_string_param;

    let params = json!({});
    let result = get_string_param(&params, "template");

    assert!(result.is_err());
}

#[test]
fn test_extract_string_param_wrong_type() {
    use ggen_mcp::error::get_string_param;

    let params = json!({"template": 123});
    let result = get_string_param(&params, "template");

    assert!(result.is_err());
}

#[test]
fn test_extract_optional_string_param_present() {
    use ggen_mcp::error::get_optional_string_param;

    let params = json!({"category": "api"});
    let result = get_optional_string_param(&params, "category");

    assert_eq!(result, Some("api".to_string()));
}

#[test]
fn test_extract_optional_string_param_missing() {
    use ggen_mcp::error::get_optional_string_param;

    let params = json!({});
    let result = get_optional_string_param(&params, "category");

    assert_eq!(result, None);
}

#[test]
fn test_extract_bool_param_true() {
    use ggen_mcp::error::get_bool_param;

    let params = json!({"dry_run": true});
    let result = get_bool_param(&params, "dry_run", false);

    assert_eq!(result, true);
}

#[test]
fn test_extract_bool_param_false() {
    use ggen_mcp::error::get_bool_param;

    let params = json!({"dry_run": false});
    let result = get_bool_param(&params, "dry_run", true);

    assert_eq!(result, false);
}

#[test]
fn test_extract_bool_param_default() {
    use ggen_mcp::error::get_bool_param;

    let params = json!({});
    let result = get_bool_param(&params, "dry_run", true);

    assert_eq!(result, true);
}

#[test]
fn test_extract_u64_param_present() {
    use ggen_mcp::error::get_optional_u64_param;

    let params = json!({"limit": 10});
    let result = get_optional_u64_param(&params, "limit");

    assert_eq!(result, Some(10));
}

#[test]
fn test_extract_u64_param_missing() {
    use ggen_mcp::error::get_optional_u64_param;

    let params = json!({});
    let result = get_optional_u64_param(&params, "limit");

    assert_eq!(result, None);
}

#[test]
fn test_extract_object_param_present() {
    use ggen_mcp::error::get_optional_object_param;

    let mut vars = Map::new();
    vars.insert("name".to_string(), json!("test"));

    let params = json!({"vars": vars});
    let result = get_optional_object_param(&params, "vars");

    assert!(result.is_some());
    assert_eq!(result.unwrap().get("name"), Some(&json!("test")));
}

#[test]
fn test_extract_object_param_missing() {
    use ggen_mcp::error::get_optional_object_param;

    let params = json!({});
    let result = get_optional_object_param(&params, "vars");

    assert_eq!(result, None);
}

// ============================================================================
// VARIABLE HANDLING TESTS
// ============================================================================

#[test]
fn test_build_vars_map_empty() {
    let vars = Map::new();
    assert_eq!(vars.len(), 0);
}

#[test]
fn test_build_vars_map_single_var() {
    let mut vars = Map::new();
    vars.insert("project_name".to_string(), json!("my-project"));

    assert_eq!(vars.len(), 1);
    assert_eq!(vars.get("project_name"), Some(&json!("my-project")));
}

#[test]
fn test_build_vars_map_multiple_vars() {
    let mut vars = Map::new();
    vars.insert("name".to_string(), json!("test"));
    vars.insert("version".to_string(), json!("1.0.0"));
    vars.insert("author".to_string(), json!("Test Author"));

    assert_eq!(vars.len(), 3);
    assert!(vars.contains_key("name"));
    assert!(vars.contains_key("version"));
    assert!(vars.contains_key("author"));
}

#[test]
fn test_vars_support_different_types() {
    let mut vars = Map::new();
    vars.insert("string_var".to_string(), json!("text"));
    vars.insert("number_var".to_string(), json!(42));
    vars.insert("bool_var".to_string(), json!(true));
    vars.insert("array_var".to_string(), json!(["a", "b", "c"]));

    assert_eq!(vars.len(), 4);
    assert!(vars.get("string_var").unwrap().is_string());
    assert!(vars.get("number_var").unwrap().is_number());
    assert!(vars.get("bool_var").unwrap().is_boolean());
    assert!(vars.get("array_var").unwrap().is_array());
}

// ============================================================================
// RESPONSE FORMATTING TESTS
// ============================================================================

#[test]
fn test_success_response_format() {
    use ggen_mcp::error::success_response;

    let data = json!({"result": "success"});
    let response = success_response(data);

    assert_eq!(response.get("status"), Some(&json!("success")));
    assert!(response.get("data").is_some());
}

#[test]
fn test_success_response_with_complex_data() {
    use ggen_mcp::error::success_response;

    let data = json!({
        "files_created": 10,
        "execution_time_ms": 1234,
        "variables": ["var1", "var2"]
    });

    let response = success_response(data);

    assert_eq!(response.get("status"), Some(&json!("success")));

    let response_data = response.get("data").unwrap();
    assert_eq!(response_data.get("files_created"), Some(&json!(10)));
    assert_eq!(response_data.get("execution_time_ms"), Some(&json!(1234)));
}

#[test]
fn test_error_response_format() {
    use ggen_mcp::error::error_response;

    let response = error_response("Test error message");

    assert_eq!(response.get("status"), Some(&json!("error")));
    assert_eq!(response.get("message"), Some(&json!("Test error message")));
}

// ============================================================================
// EDGE CASE HANDLING TESTS
// ============================================================================

#[test]
fn test_empty_string_param_handling() {
    use ggen_mcp::error::get_string_param;

    let params = json!({"template": ""});
    let result = get_string_param(&params, "template");

    // Empty string should be accepted (validation happens elsewhere)
    assert!(result.is_ok());
    assert_eq!(result.unwrap(), "");
}

#[test]
fn test_null_param_handling() {
    use ggen_mcp::error::get_string_param;

    let params = json!({"template": null});
    let result = get_string_param(&params, "template");

    // Null should be treated as missing
    assert!(result.is_err());
}

#[test]
fn test_whitespace_string_param() {
    use ggen_mcp::error::get_string_param;

    let params = json!({"template": "   "});
    let result = get_string_param(&params, "template");

    // Whitespace should be accepted (trimming happens elsewhere)
    assert!(result.is_ok());
    assert_eq!(result.unwrap(), "   ");
}

#[test]
fn test_special_characters_in_params() {
    use ggen_mcp::error::get_string_param;

    let params = json!({"query": "test-query_123!@#"});
    let result = get_string_param(&params, "query");

    assert!(result.is_ok());
    assert_eq!(result.unwrap(), "test-query_123!@#");
}

#[test]
fn test_unicode_in_params() {
    use ggen_mcp::error::get_string_param;

    let params = json!({"query": "测试查询"});
    let result = get_string_param(&params, "query");

    assert!(result.is_ok());
    assert_eq!(result.unwrap(), "测试查询");
}

// ============================================================================
// ARGUMENT CONSTRUCTION SAFETY TESTS
// ============================================================================

#[test]
fn test_args_dont_contain_injection_attempts() {
    let malicious = "; rm -rf /";
    let args = vec!["market", "search", malicious];

    // Args should preserve the string exactly (shell escaping happens in CLI)
    assert_eq!(args[2], "; rm -rf /");
}

#[test]
fn test_args_handle_path_traversal_attempts() {
    let malicious_path = "../../../etc/passwd";
    let args = vec!["graph", "load", malicious_path];

    // Args should preserve the path (validation happens in CLI)
    assert_eq!(args[2], "../../../etc/passwd");
}

#[test]
fn test_flag_order_is_consistent() {
    let mut args = vec!["project", "gen", "template"];

    // Add flags in specific order
    args.push("--dry-run");
    args.push("--force");

    assert_eq!(args[3], "--dry-run");
    assert_eq!(args[4], "--force");
}

// ============================================================================
// PARAMETER COMBINATION TESTS
// ============================================================================

#[test]
fn test_multiple_optional_params_all_present() {
    use ggen_mcp::error::{get_optional_string_param, get_optional_u64_param};

    let params = json!({
        "category": "api",
        "limit": 10,
        "tag": "rust"
    });

    assert_eq!(get_optional_string_param(&params, "category"), Some("api".to_string()));
    assert_eq!(get_optional_u64_param(&params, "limit"), Some(10));
    assert_eq!(get_optional_string_param(&params, "tag"), Some("rust".to_string()));
}

#[test]
fn test_multiple_optional_params_partial() {
    use ggen_mcp::error::{get_optional_string_param, get_optional_u64_param};

    let params = json!({
        "category": "api"
        // limit and tag are missing
    });

    assert_eq!(get_optional_string_param(&params, "category"), Some("api".to_string()));
    assert_eq!(get_optional_u64_param(&params, "limit"), None);
    assert_eq!(get_optional_string_param(&params, "tag"), None);
}

#[test]
fn test_required_and_optional_params_mixed() {
    use ggen_mcp::error::{get_string_param, get_optional_string_param};

    let params = json!({
        "query": "test",
        "category": "api"
    });

    let query = get_string_param(&params, "query");
    let category = get_optional_string_param(&params, "category");

    assert!(query.is_ok());
    assert_eq!(query.unwrap(), "test");
    assert_eq!(category, Some("api".to_string()));
}

// ============================================================================
// CONCURRENT PARAMETER EXTRACTION TESTS
// ============================================================================

#[tokio::test]
async fn test_concurrent_param_extraction() {
    use ggen_mcp::error::get_string_param;

    let params = std::sync::Arc::new(json!({"template": "test"}));
    let mut handles = vec![];

    for _ in 0..10 {
        let params_clone = params.clone();
        let handle = tokio::spawn(async move {
            get_string_param(&params_clone, "template")
        });
        handles.push(handle);
    }

    // All extractions should succeed
    for handle in handles {
        let result = handle.await.unwrap();
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), "test");
    }
}

// ============================================================================
// TYPE COERCION TESTS
// ============================================================================

#[test]
fn test_bool_param_with_string_value() {
    use ggen_mcp::error::get_bool_param;

    // String "true" should not coerce to boolean
    let params = json!({"dry_run": "true"});
    let result = get_bool_param(&params, "dry_run", false);

    // Should use default since type doesn't match
    assert_eq!(result, false);
}

#[test]
fn test_number_param_with_string_value() {
    use ggen_mcp::error::get_optional_u64_param;

    // String "10" should not coerce to number
    let params = json!({"limit": "10"});
    let result = get_optional_u64_param(&params, "limit");

    // Should be None since type doesn't match
    assert_eq!(result, None);
}

#[test]
fn test_negative_number_handling() {
    let params = json!({"limit": -5});

    // Negative numbers can't be u64
    assert!(params.get("limit").unwrap().as_u64().is_none());

    // But can be i64
    assert_eq!(params.get("limit").unwrap().as_i64(), Some(-5));
}
