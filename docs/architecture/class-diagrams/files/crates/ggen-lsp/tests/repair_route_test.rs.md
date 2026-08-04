# `crates/ggen-lsp/tests/repair_route_test.rs`

Source SHA-256: `1f4fb724fb055212698989fc2ba89a78d7c5d97e033e96868764591bc3fb1756`

```mermaid
classDiagram
    class fn_invalid_enum_value_yields_an_advisory_route_not_a_destructive_edit {
      <<fn>>
    }
    class fn_ggen_does_not_flag_llm_or_unknown_sections {
      <<fn>>
    }
    class fn_clean_config_has_no_diagnostics {
      <<fn>>
    }
```

## Dependencies

- `ggen_lsp::RouteRegistry`
- `ggen_lsp::analyzers::build_analyzer`
- `ggen_lsp::route::{family_of_diagnostic, route_plan_for_diagnostic, RepairFamily}`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
