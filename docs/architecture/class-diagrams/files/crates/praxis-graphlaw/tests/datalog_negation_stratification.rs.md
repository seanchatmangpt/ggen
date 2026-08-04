# `crates/praxis-graphlaw/tests/datalog_negation_stratification.rs`

Source SHA-256: `a1ca26c90e87bbe58dc7699ed4acb51e5cfeead96a226a25c2850d8415c48ca8`

```mermaid
classDiagram
    class fn_test_fixpoint_terminates_on_recursive_ruleset {
      <<fn>>
    }
    class fn_test_negation_unbound_vars_rejected {
      <<fn>>
    }
    class fn_test_long_unstratifiable_cycle_rejected {
      <<fn>>
    }
    class fn_test_three_layer_stratification_chain {
      <<fn>>
    }
    class fn_test_union_semantics_multiple_rules_same_head {
      <<fn>>
    }
```

## Dependencies

- `praxis_graphlaw::TripleStore`
- `praxis_graphlaw::datalog::validate_rules`
- `praxis_graphlaw::triples::{Aggregate, BodyLiteral, Rule, Triple}`
- `std::collections::HashMap`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
