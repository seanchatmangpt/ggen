# `crates/ggen-mcp/tests/introspection_tools_test.rs`

Source SHA-256: `45844f6fc9a3a94129072570ad6975827d0424053e3835f512964e403509a50b`

```mermaid
classDiagram
    class mod_common {
      <<mod>>
    }
    class fn_classify_distinguishes_the_two_real_schemas {
      <<fn>>
    }
    class fn_classify_does_not_touch_the_project {
      <<fn>>
    }
    class fn_dir_listing {
      <<fn>>
    }
    class fn_walk {
      <<fn>>
    }
    class fn_frontmatter_schema_matches_the_live_derive {
      <<fn>>
    }
    class fn_frontmatter_schema_surfaces_to_and_for_each {
      <<fn>>
    }
    class fn_frontmatter_schema_states_all_three_projection_modes {
      <<fn>>
    }
    class fn_frontmatter_schema_unknown_key_lists_the_real_keys {
      <<fn>>
    }
    class fn_lint_reports_bound_vars_on_a_good_template {
      <<fn>>
    }
    class fn_lint_catches_a_consumed_but_unprojected_root_var {
      <<fn>>
    }
    class fn_lint_flags_jinja_ternary_that_tera_cannot_parse {
      <<fn>>
    }
    class fn_lint_refuses_path_traversal {
      <<fn>>
    }
    class fn_rule_graph_maps_rules_to_queries_and_outputs {
      <<fn>>
    }
    class fn_rule_graph_unknown_rule_names_the_available_rules {
      <<fn>>
    }
    class fn_capability_status_detects_actual_use_of_an_inert_field {
      <<fn>>
    }
    class fn_capability_status_reports_unaffected_when_nothing_uses_them {
      <<fn>>
    }
```

## Dependencies

- `common::{write_declarative_project, write_frontmatter_project}`
- `ggen_mcp::error::ErrorCategory`
- `ggen_mcp::tools::{ capability_status::{capability_status, CapabilityStatusParams}, config_classify::{config_classify, ConfigClassifyParams}, frontmatter_lint::{frontmatter_lint, FrontmatterLintParams}, frontmatter_schema::{frontmatter_schema, FrontmatterSchemaParams}, rule_graph::{rule_graph, RuleGraphParams}, }`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
