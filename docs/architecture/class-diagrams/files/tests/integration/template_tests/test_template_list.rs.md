# `tests/integration/template_tests/test_template_list.rs`

Source SHA-256: `7ba91a115f3a2b995b6c9b2b60c68fb322fe38f35337c19a97e78c4c7437d19e`

```mermaid
classDiagram
    class fn_test_list_empty_directory {
      <<fn>>
    }
    class fn_test_list_real_templates {
      <<fn>>
    }
    class fn_test_list_with_pattern_filter {
      <<fn>>
    }
    class fn_test_list_nonexistent_directory {
      <<fn>>
    }
    class fn_test_list_extracts_real_descriptions {
      <<fn>>
    }
```

## Dependencies

- `ggen_cli::domain::template::{list_templates, ListFilters, TemplateSource}`
- `std::fs`
- `tempfile::TempDir`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
