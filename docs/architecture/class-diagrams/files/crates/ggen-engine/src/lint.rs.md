# `crates/ggen-engine/src/lint.rs`

Source SHA-256: `3ea033ea7ecc2464f828e1bba1c2afcfafa550687be6275eaa7b81f61575a140`

```mermaid
classDiagram
    class enum_Projection {
      <<enum>>
    }
    class fn_is_identifier {
      <<fn>>
    }
    class fn_root_ident {
      <<fn>>
    }
    class fn_consumed_vars {
      <<fn>>
    }
    class fn_projected_vars {
      <<fn>>
    }
    class enum_SelectProjection {
      <<enum>>
    }
    class fn_select_projection {
      <<fn>>
    }
```

## Dependencies

- `crate::{ error::AppError, template::{Frontmatter, Template}, }`
- `std::{collections::BTreeSet, path::Path}`
- `super::*`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
