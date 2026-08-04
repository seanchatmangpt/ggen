# `crates/praxis-graphlaw/tests/shex_validation_logic.rs`

Source SHA-256: `cc16e08365a4f58cd2750b1a09c36bf5c6d6b5cb831f941cc45457a2678db944`

```mermaid
classDiagram
    class fn_build_data_index {
      <<fn>>
    }
    class fn_test_one_of_disjunction {
      <<fn>>
    }
    class fn_test_shape_and {
      <<fn>>
    }
    class fn_test_shape_or {
      <<fn>>
    }
    class fn_test_shape_not {
      <<fn>>
    }
    class fn_test_closed_shape_with_extra {
      <<fn>>
    }
```

## Dependencies

- `praxis_graphlaw::parser::{Parser, Syntax}`
- `praxis_graphlaw::shex::validate_shex`
- `praxis_graphlaw::tripleindex::TripleIndex`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
