# `tests/template_systems_tests.rs`

Source SHA-256: `c8b3c30833c5963c2a7aee82a79bac9bfc87163c969c1097b45dac205b6603ae`

```mermaid
classDiagram
    class fn_test_template_parse_basic {
      <<fn>>
    }
    class fn_test_template_parse_with_variables {
      <<fn>>
    }
    class fn_test_template_parse_empty_frontmatter {
      <<fn>>
    }
    class fn_test_template_parse_invalid_yaml {
      <<fn>>
    }
    class fn_test_template_parse_multiline_frontmatter {
      <<fn>>
    }
    class fn_test_template_parse_empty_body {
      <<fn>>
    }
    class fn_test_template_parse_with_rdf {
      <<fn>>
    }
    class fn_test_template_parse_with_sparql {
      <<fn>>
    }
    class fn_test_template_parse_with_prefixes {
      <<fn>>
    }
    class fn_test_template_parse_complex {
      <<fn>>
    }
    class fn_test_template_parse_liquid_syntax {
      <<fn>>
    }
    class fn_test_template_parse_liquid_loops {
      <<fn>>
    }
    class fn_test_template_parse_unicode {
      <<fn>>
    }
    class fn_test_template_parse_special_characters {
      <<fn>>
    }
    class fn_test_template_render_basic {
      <<fn>>
    }
    class fn_test_template_render_with_variable {
      <<fn>>
    }
    class fn_test_template_render_missing_variable {
      <<fn>>
    }
    class fn_test_template_render_with_filter {
      <<fn>>
    }
    class fn_test_template_render_conditional {
      <<fn>>
    }
    class fn_test_template_render_loop {
      <<fn>>
    }
    class fn_test_template_render_multiple_variables {
      <<fn>>
    }
    class fn_test_template_render_empty {
      <<fn>>
    }
    class fn_test_template_render_complex_expression {
      <<fn>>
    }
    class fn_test_template_render_nested_conditionals {
      <<fn>>
    }
    class fn_test_template_render_comments {
      <<fn>>
    }
    class fn_test_template_render_whitespace_control {
      <<fn>>
    }
    class fn_test_template_render_array_iteration {
      <<fn>>
    }
    class fn_test_template_render_inheritance {
      <<fn>>
    }
    class fn_test_frozen_merger_has_frozen_sections_none {
      <<fn>>
    }
    class fn_test_frozen_merger_has_frozen_sections_present {
      <<fn>>
    }
    class fn_test_frozen_merger_merge_no_frozen {
      <<fn>>
    }
    class fn_test_frozen_merger_merge_preserves_frozen {
      <<fn>>
    }
    class fn_test_frozen_merger_multiple_sections {
      <<fn>>
    }
    class fn_test_frozen_merger_empty_frozen_section {
      <<fn>>
    }
    class fn_test_frozen_merger_unclosed_frozen {
      <<fn>>
    }
    class fn_test_frozen_merger_nested_markers {
      <<fn>>
    }
    class fn_test_frozen_merger_whitespace_handling {
      <<fn>>
    }
    class fn_test_frozen_merger_line_endings {
      <<fn>>
    }
    class fn_test_context_creation {
      <<fn>>
    }
    class fn_test_context_insert_string {
      <<fn>>
    }
    class fn_test_context_insert_number {
      <<fn>>
    }
    class fn_test_context_insert_boolean {
      <<fn>>
    }
    class fn_test_context_insert_array {
      <<fn>>
    }
    class fn_test_context_from_serialize {
      <<fn>>
    }
    class fn_test_context_multiple_inserts {
      <<fn>>
    }
    class fn_test_context_overwrite {
      <<fn>>
    }
    class fn_test_context_nested_structure {
      <<fn>>
    }
    class fn_test_context_empty {
      <<fn>>
    }
```

## Dependencies

- `ggen_core::pipeline::Pipeline`
- `ggen_core::template::Template`
- `ggen_core::templates::frozen::FrozenMerger`
- `ggen_core::utils::error::Result`
- `serde_json::json`
- `std::collections::HashMap`
- `tera::Context`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
