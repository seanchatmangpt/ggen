# `crates/praxis-graphlaw/tests/shacl_nk_study.rs`

Source SHA-256: `c74e338a777aebb7f5493e24fa9a98de9ce44e2ae042f7a448c94f49224e947e`

```mermaid
classDiagram
    class fn_build_data_index {
      <<fn>>
    }
    class enum_Kind {
      <<enum>>
    }
    class fn_representative_pool {
      <<fn>>
    }
    class fn_run_real_engine {
      <<fn>>
    }
    class fn_independent_oracle {
      <<fn>>
    }
    class fn_boundary_adjacent_value_sets {
      <<fn>>
    }
    class fn_nk_study_k2_exhaustive {
      <<fn>>
    }
    note "Kind"
```

## Dependencies

- `praxis_graphlaw::parser::{Parser, Syntax}`
- `praxis_graphlaw::shacl::{ShapesGraph, Validator}`
- `praxis_graphlaw::tripleindex::TripleIndex`
- `proptest::prelude::*`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
