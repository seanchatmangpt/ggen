# `tests/integration/marketplace_nextjs_ontology_e2e.rs`

Source SHA-256: `aac1ec586007e27d762a2bfd61b55bc3b0ad33ebf49f4015e9a644b537ff7951`

```mermaid
classDiagram
    class mod_common {
      <<mod>>
    }
    class fn_test_nextjs_setup_and_build {
      <<fn>>
    }
    class fn_run_setup_and_build_test {
      <<fn>>
    }
    class fn_test_nextjs_package_installation {
      <<fn>>
    }
    class fn_run_package_installation_test {
      <<fn>>
    }
    class fn_test_nextjs_initial_generation {
      <<fn>>
    }
    class fn_run_initial_generation_test {
      <<fn>>
    }
    class fn_test_nextjs_type_check_and_build {
      <<fn>>
    }
    class fn_run_type_check_and_build_test {
      <<fn>>
    }
    class fn_test_nextjs_ontology_modification {
      <<fn>>
    }
    class fn_run_ontology_modification_test {
      <<fn>>
    }
    class fn_test_nextjs_regeneration_idempotency {
      <<fn>>
    }
    class fn_run_idempotency_test {
      <<fn>>
    }
    class fn_setup_container_environment {
      <<fn>>
    }
    class fn_build_ggen_from_source_in_container {
      <<fn>>
    }
    class fn_install_nextjs_package {
      <<fn>>
    }
    class fn_validate_package_structure {
      <<fn>>
    }
    class fn_install_npm_dependencies {
      <<fn>>
    }
    class fn_regenerate_code_from_ontology {
      <<fn>>
    }
    class fn_verify_generated_files_comprehensive {
      <<fn>>
    }
    class fn_verify_typescript_types {
      <<fn>>
    }
    class fn_verify_zod_schemas {
      <<fn>>
    }
    class fn_verify_api_routes {
      <<fn>>
    }
    class fn_verify_crud_components {
      <<fn>>
    }
    class fn_verify_crud_pages {
      <<fn>>
    }
    class fn_run_typescript_type_check {
      <<fn>>
    }
    class fn_build_nextjs_application {
      <<fn>>
    }
    class fn_modify_ontology_with_multiple_properties {
      <<fn>>
    }
    class fn_regenerate_with_modified_ontology {
      <<fn>>
    }
    class fn_verify_new_properties_in_generated_code {
      <<fn>>
    }
    class fn_verify_regeneration_idempotency {
      <<fn>>
    }
    class fn_capture_host_snapshot {
      <<fn>>
    }
    class struct_HostSnapshot {
      <<struct>>
      +"file_count: usize"
      +"dir_count: usize"
    }
    class fn_test_nextjs_error_invalid_package_name {
      <<fn>>
    }
    class fn_run_error_path_invalid_package_test {
      <<fn>>
    }
    class fn_test_nextjs_error_installation_failure_recovery {
      <<fn>>
    }
    class fn_run_error_path_installation_recovery_test {
      <<fn>>
    }
    class fn_test_nextjs_boundary_empty_ontology {
      <<fn>>
    }
    class fn_run_boundary_empty_ontology_test {
      <<fn>>
    }
    class fn_test_nextjs_resource_cleanup_isolation {
      <<fn>>
    }
    class fn_run_resource_cleanup_container1 {
      <<fn>>
    }
    class fn_run_resource_cleanup_container2 {
      <<fn>>
    }
    class fn_test_nextjs_error_package_name_validation_comprehensive {
      <<fn>>
    }
    class fn_run_error_path_comprehensive_validation_test {
      <<fn>>
    }
    class fn_test_nextjs_error_already_installed {
      <<fn>>
    }
    class fn_run_error_path_already_installed_test {
      <<fn>>
    }
    class fn_test_nextjs_boundary_large_ontology {
      <<fn>>
    }
    class fn_run_boundary_large_ontology_test {
      <<fn>>
    }
    class fn_test_nextjs_concurrency_multiple_installs {
      <<fn>>
    }
    class fn_run_concurrent_install_container {
      <<fn>>
    }
```

## Dependencies

- `chicago_tdd_tools::testcontainers::{ exec::SUCCESS_EXIT_CODE, ContainerClient, GenericContainer, TestcontainersResult, }`
- `common::require_docker`
- `std::fs`
- `std::path::Path`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
