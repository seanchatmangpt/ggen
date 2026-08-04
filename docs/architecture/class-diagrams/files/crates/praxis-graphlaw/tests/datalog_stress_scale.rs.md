# `crates/praxis-graphlaw/tests/datalog_stress_scale.rs`

Source SHA-256: `baa9c42e1fc3c1fde3fe7d390f79aa68c2c4cfdbccde017f1dd102fc716913b2`

```mermaid
classDiagram
    class fn_decode_all {
      <<fn>>
    }
    class fn_pred {
      <<fn>>
    }
    class fn_test_large_scale_grouped_aggregation {
      <<fn>>
    }
    class fn_test_stratified_negation_over_aggregate_derived_predicate {
      <<fn>>
    }
```

## Dependencies

- `praxis_graphlaw::TripleStore`
- `praxis_graphlaw::encoding::Encoder`
- `praxis_graphlaw::triples::{ Aggregate, AggregateFunction, BodyLiteral, Rule, Triple, VarOrTerm, }`
- `std::time::{Duration, Instant}`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
