# `tests/security/fuzzing_targets.rs`

Source SHA-256: `7c8498eb044f43a45bd0acf29e22c595f5b676a170d37d0a30ed7c48c16c05a2`

```mermaid
classDiagram
    class fn_fuzz_rdf_parser {
      <<fn>>
    }
    class fn_parse_rdf_structure {
      <<fn>>
    }
    class fn_fuzz_sparql_parser {
      <<fn>>
    }
    class fn_validate_sparql_structure {
      <<fn>>
    }
    class fn_fuzz_template_validator {
      <<fn>>
    }
    class fn_validate_template_syntax {
      <<fn>>
    }
    class fn_fuzz_config_parser {
      <<fn>>
    }
```

## Dependencies

- `libfuzzer_sys::fuzz_target`
- `std::collections::HashMap`
- `super::*`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
