# `tests/generator_core_tests.rs`

Source SHA-256: `98175c2cb074c21c992035d89c17662972d6d85999e67b37340a060cbf3389dd`

```mermaid
classDiagram
    class fn_test_gen_context_new {
      <<fn>>
    }
    class fn_test_gen_context_with_vars {
      <<fn>>
    }
    class fn_test_gen_context_with_prefixes {
      <<fn>>
    }
    class fn_test_gen_context_dry_mode {
      <<fn>>
    }
    class fn_test_gen_context_builder_chain {
      <<fn>>
    }
    class fn_test_gen_context_empty_vars {
      <<fn>>
    }
    class fn_test_gen_context_empty_prefixes {
      <<fn>>
    }
    class fn_test_gen_context_multiple_vars {
      <<fn>>
    }
    class fn_test_gen_context_paths {
      <<fn>>
    }
    class fn_test_gen_context_immutability {
      <<fn>>
    }
    class fn_create_test_template {
      <<fn>>
    }
    class fn_test_generate_simple_template {
      <<fn>>
    }
    class fn_test_generate_with_default_output {
      <<fn>>
    }
    class fn_test_generate_dry_run {
      <<fn>>
    }
    class fn_test_generate_with_nested_output {
      <<fn>>
    }
    class fn_test_generate_missing_template {
      <<fn>>
    }
    class fn_test_generate_invalid_template_syntax {
      <<fn>>
    }
    class fn_test_generate_missing_variable {
      <<fn>>
    }
    class fn_test_generate_with_env_vars {
      <<fn>>
    }
    class fn_test_generate_multiple_variables {
      <<fn>>
    }
    class fn_test_generate_with_filters {
      <<fn>>
    }
    class fn_test_generate_empty_template {
      <<fn>>
    }
    class fn_test_generate_creates_parent_dirs {
      <<fn>>
    }
    class fn_test_generate_overwrites_existing {
      <<fn>>
    }
    class fn_test_generate_with_conditionals {
      <<fn>>
    }
    class fn_test_generate_with_loops {
      <<fn>>
    }
    class fn_test_streaming_generator_new {
      <<fn>>
    }
    class fn_test_streaming_generator_with_cache_capacity {
      <<fn>>
    }
    class fn_test_streaming_generator_single_file {
      <<fn>>
    }
    class fn_test_streaming_generator_multiple_files {
      <<fn>>
    }
    class fn_test_streaming_generator_nested_output {
      <<fn>>
    }
    class fn_test_streaming_generator_cache_reuse {
      <<fn>>
    }
    class fn_test_streaming_generator_error_resilience {
      <<fn>>
    }
    class fn_test_streaming_generator_empty_directory {
      <<fn>>
    }
    class fn_test_generation_result_success_rate {
      <<fn>>
    }
    class fn_test_generation_result_throughput {
      <<fn>>
    }
    class fn_test_generation_result_total_count {
      <<fn>>
    }
    class fn_test_generation_result_zero_duration {
      <<fn>>
    }
    class fn_test_streaming_generator_large_batch {
      <<fn>>
    }
    class fn_test_streaming_generator_cache_stats {
      <<fn>>
    }
    class fn_test_streaming_generator_with_variables {
      <<fn>>
    }
    class fn_test_streaming_generator_default_output_name {
      <<fn>>
    }
    class fn_test_streaming_generator_subdirectories {
      <<fn>>
    }
    class fn_test_streaming_generator_non_template_files_ignored {
      <<fn>>
    }
    class fn_test_streaming_generator_performance_metrics {
      <<fn>>
    }
    class fn_test_streaming_generator_cache_overflow {
      <<fn>>
    }
    class fn_test_streaming_generator_error_messages {
      <<fn>>
    }
    class fn_test_streaming_generator_mixed_success_and_errors {
      <<fn>>
    }
    class fn_test_streaming_generator_creates_output_dirs {
      <<fn>>
    }
    class fn_test_generate_very_large_template {
      <<fn>>
    }
    class fn_test_generate_many_variables {
      <<fn>>
    }
    class fn_test_streaming_generator_1000_files {
      <<fn>>
    }
    class fn_test_generate_with_very_long_path {
      <<fn>>
    }
    class fn_test_generate_with_unicode_content {
      <<fn>>
    }
    class fn_test_streaming_generator_concurrent_generations {
      <<fn>>
    }
    class fn_test_generate_resource_cleanup {
      <<fn>>
    }
    class fn_test_streaming_generator_memory_efficiency {
      <<fn>>
    }
    class fn_test_generate_special_characters_in_filename {
      <<fn>>
    }
    class fn_test_generate_variable_sanitization {
      <<fn>>
    }
    class fn_test_streaming_throughput_calculation {
      <<fn>>
    }
    class fn_test_generation_result_empty {
      <<fn>>
    }
    class fn_test_streaming_generator_path_traversal_prevention {
      <<fn>>
    }
    class fn_test_generate_with_complex_filters {
      <<fn>>
    }
    class fn_test_streaming_generator_cache_hit_rate {
      <<fn>>
    }
```

## Dependencies

- `ggen_core::generator::{GenContext, Generator}`
- `ggen_core::pipeline::Pipeline`
- `ggen_core::streaming_generator::{GenerationResult, StreamingGenerator}`
- `ggen_core::utils::error::Result`
- `std::collections::BTreeMap`
- `std::fs`
- `std::path::PathBuf`
- `tempfile::TempDir`
- `tera::Context`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
