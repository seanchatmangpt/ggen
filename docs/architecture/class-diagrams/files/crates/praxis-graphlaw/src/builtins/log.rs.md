# `crates/praxis-graphlaw/src/builtins/log.rs`

Source SHA-256: `ecb67dbf6e1aeae48bc7ee289ccb75a0d9733f36863e8f843850121a71c24c07`

```mermaid
classDiagram
    class fn_eval_equal_to {
      <<fn>>
    }
    class fn_eval_not_equal_to {
      <<fn>>
    }
    class fn_eval_dtlit {
      <<fn>>
    }
    class fn_eval_raw_type {
      <<fn>>
    }
    class fn_eval_uri {
      <<fn>>
    }
    class fn_eval_local_name {
      <<fn>>
    }
    class fn_eval_bound {
      <<fn>>
    }
    class fn_eval_n3_string {
      <<fn>>
    }
    class fn_eval_parsed_as_n3 {
      <<fn>>
    }
    class fn_eval_conjunction {
      <<fn>>
    }
```

## Dependencies

- `crate::{Binding, Encoder, Term, Triple, VarOrTerm}`
- `super::{ eval_functional, eval_row_constraint, intern_string, lexical_value, numeric_value, resolve_operand, subject_list_members, }`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
