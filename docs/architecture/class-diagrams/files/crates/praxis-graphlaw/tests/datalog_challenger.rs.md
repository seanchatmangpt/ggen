# `crates/praxis-graphlaw/tests/datalog_challenger.rs`

Source SHA-256: `a0cc2805f36a665eca9f235090cebd32c6361b88fdf58791e03438375e121a9d`

```mermaid
classDiagram
    class fn_test_empty_relations_negation {
      <<fn>>
    }
    class fn_test_empty_relations_aggregation {
      <<fn>>
    }
    class fn_test_unbound_aggregate_source_var {
      <<fn>>
    }
    class fn_test_boundary_numeric_inputs_aggregation {
      <<fn>>
    }
    class fn_test_aggregate_skips_non_numeric_source_values {
      <<fn>>
    }
```

## Dependencies

- `praxis_graphlaw::TripleStore`
- `praxis_graphlaw::triples::{Aggregate, AggregateFunction, BodyLiteral, Rule, Triple}`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
