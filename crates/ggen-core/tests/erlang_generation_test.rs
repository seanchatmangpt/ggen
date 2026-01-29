//! Integration tests for Erlang code generation
//!
//! These tests verify end-to-end Erlang code generation using
//! template helpers, SPARQL queries, and validation functions.

use ggen_core::sparql::erlang::{query_modules, query_supervision_tree, validate_query};
use ggen_core::templates::helpers::erlang::{
    format_app_resource, format_record, format_supervisor_child, snake_case_to_module,
};
use ggen_core::validation::erlang::{
    validate_function_name, validate_module_name, validate_module_structure,
};
use serde_json::json;
use std::collections::BTreeMap;

#[test]
fn test_erlang_module_generation() {
    // Arrange: Module name
    let module = "job_worker";

    // Act: Validate and format
    let result = snake_case_to_module(module);

    // Assert: Valid module name
    assert!(result.is_ok());
    assert_eq!(result.unwrap(), "job_worker");

    // Act: Validate module name
    let validation_result = validate_module_name(module);

    // Assert: Passes validation
    assert!(validation_result.is_ok());
}

#[test]
fn test_erlang_record_generation() {
    // Arrange: Record fields
    let mut fields = BTreeMap::new();
    fields.insert("jobs".to_string(), "queue:queue()".to_string());
    fields.insert("max_jobs".to_string(), "integer()".to_string());
    fields.insert("processed".to_string(), "integer()".to_string());

    // Act: Generate record
    let result = format_record("worker_state", &fields);

    // Assert: Valid record definition
    assert!(result.is_ok());
    let record = result.unwrap();
    assert!(record.contains("-record(worker_state"));
    assert!(record.contains("jobs :: queue:queue()"));
    assert!(record.contains("max_jobs :: integer()"));
    assert!(record.contains("processed :: integer()"));
}

#[test]
fn test_supervisor_child_spec_generation() {
    // Arrange: Child spec parameters
    let id = "job_worker";
    let module = "job_worker";
    let args = json!([]);
    let child_type = "worker";

    // Act: Generate child spec
    let result = format_supervisor_child(id, module, &args, child_type);

    // Assert: Valid child spec
    assert!(result.is_ok());
    let spec = result.unwrap();
    assert!(spec.contains("id => job_worker"));
    assert!(spec.contains("start => {job_worker, start_link"));
    assert!(spec.contains("type => worker"));
    assert!(spec.contains("restart => permanent"));
}

#[test]
fn test_app_resource_generation() {
    // Arrange: Application metadata
    let app_name = "job_processor";
    let version = "1.0.0";
    let description = "Job processing application";
    let modules = vec![
        "job_app".to_string(),
        "job_sup".to_string(),
        "job_worker".to_string(),
    ];
    let apps = vec!["kernel".to_string(), "stdlib".to_string()];

    // Act: Generate .app file
    let result = format_app_resource(app_name, version, description, &modules, &apps);

    // Assert: Valid .app file
    assert!(result.is_ok());
    let app = result.unwrap();
    assert!(app.contains("{application, job_processor"));
    assert!(app.contains("\"1.0.0\""));
    assert!(app.contains("job_app"));
    assert!(app.contains("job_sup"));
    assert!(app.contains("job_worker"));
    assert!(app.contains("kernel"));
    assert!(app.contains("stdlib"));
}

#[test]
fn test_module_structure_validation() {
    // Arrange: Valid Erlang module
    let valid_module = r#"
-module(job_worker).
-export([start_link/0, init/1]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    {ok, #worker_state{}}.
"#;

    // Act: Validate structure
    let result = validate_module_structure(valid_module);

    // Assert: Passes validation
    assert!(result.is_ok());
}

#[test]
fn test_module_structure_validation_missing_export() {
    // Arrange: Invalid module (no export)
    let invalid_module = r#"
-module(job_worker).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
"#;

    // Act: Validate structure
    let result = validate_module_structure(invalid_module);

    // Assert: Fails validation
    assert!(result.is_err());
    assert!(result.unwrap_err().to_string().contains("-export()"));
}

#[test]
fn test_sparql_query_modules() {
    // Arrange & Act: Generate SPARQL query
    let query = query_modules();

    // Assert: Valid SPARQL query
    assert!(validate_query(&query).is_ok());
    assert!(query.contains("SELECT"));
    assert!(query.contains("?module"));
    assert!(query.contains("erlang:GenServer"));
    assert!(query.contains("erlang:Supervisor"));
}

#[test]
fn test_sparql_query_supervision_tree() {
    // Arrange & Act: Generate SPARQL query
    let query = query_supervision_tree();

    // Assert: Valid SPARQL query
    assert!(validate_query(&query).is_ok());
    assert!(query.contains("CONSTRUCT"));
    assert!(query.contains("erlang:Supervisor"));
    assert!(query.contains("erlang:hasChild"));
}

#[test]
fn test_function_name_validation() {
    // Arrange: Valid function names
    let valid_names = vec!["init", "handle_call", "handle_cast", "terminate"];

    // Act & Assert: All valid
    for name in valid_names {
        assert!(validate_function_name(name).is_ok());
    }

    // Arrange: Invalid function name (uppercase start)
    let invalid_name = "HandleCall";

    // Act: Validate
    let result = validate_function_name(invalid_name);

    // Assert: Fails validation
    assert!(result.is_err());
}

#[test]
fn test_end_to_end_genserver_generation() {
    // Arrange: Complete GenServer specification
    let module_name = "job_worker";
    let mut state_fields = BTreeMap::new();
    state_fields.insert("jobs".to_string(), "queue:queue()".to_string());
    state_fields.insert("max_jobs".to_string(), "integer()".to_string());

    // Act: Generate components
    let module = snake_case_to_module(module_name).unwrap();
    let record = format_record("worker_state", &state_fields).unwrap();

    // Assert: Valid components
    assert_eq!(module, "job_worker");
    assert!(record.contains("-record(worker_state"));
    assert!(record.contains("jobs :: queue:queue()"));

    // Act: Validate module name
    let validation = validate_module_name(&module);

    // Assert: Passes all validation
    assert!(validation.is_ok());
}
