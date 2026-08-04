# `playground/src/checker.rs`

Source SHA-256: `c11948667700e0fd2d93048b975bc085cc5c994ac86e03dcf2024140869d57c4`

```mermaid
classDiagram
    class fn_check_thesis {
      <<fn>>
    }
    class fn_has_cycle {
      <<fn>>
    }
    class fn_dfs_cycle_check {
      <<fn>>
    }
    class fn_check_status_consistency {
      <<fn>>
    }
    class fn_check_order_preservation {
      <<fn>>
    }
    class mod_tests {
      <<mod>>
    }
```

## Dependencies

- `crate::Result`
- `crate::models::*`
- `crate::ontology`
- `std::collections::{HashMap, HashSet}`
- `super::*`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
