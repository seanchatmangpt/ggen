# `crates/praxis-graphlaw/src/backwardchaining_test.rs`

Source SHA-256: `ad264b7d0e227b106687d7439c341450d5344c957ba2db55ab9ee8e9aa50ccbc`

```mermaid
classDiagram
    class fn_test {
      <<fn>>
    }
    class fn_test_eval_backward_rule {
      <<fn>>
    }
    class fn_test_cyclic_rules_terminate {
      <<fn>>
    }
    class fn_test_solve_peano_variable_goal {
      <<fn>>
    }
    class fn_test_prove_rejects_ground_goal_whose_body_constraint_is_actually_false {
      <<fn>>
    }
```

## Dependencies

- `crate::{BackwardChainer, Encoder, Syntax, Triple, TripleStore, VarOrTerm}`
- `std::collections::HashMap`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
