# `crates/ggen-engine/tests/validate_hardening_test.rs`

Source SHA-256: `ff53eaa4dd726b95c3a9e2584d567138d6708fa75e154c3ce1b3553deea7bc83`

```mermaid
classDiagram
    class fn_utf8 {
      <<fn>>
    }
    class fn_shacl_node_chain {
      <<fn>>
    }
    class fn_shacl_property_chain {
      <<fn>>
    }
    class fn_validate_reaches_validator {
      <<fn>>
    }
    class fn_validate_does_not_panic {
      <<fn>>
    }
    class fn_validate_shacl_depth_guard_is_safe_on_small_stack_thread {
      <<fn>>
    }
```

## Dependencies

- `camino::Utf8PathBuf`
- `ggen_engine::verbs::handlers::handle_graph_validate`
- `tempfile::TempDir`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
