# `crates/praxis-graphlaw/src/sparql/plan.rs`

Source SHA-256: `f24f5870f5465a2e795139c4b8f9e0b12e6a3cbae9c5ff2ae556ae1c5cef8fb1`

```mermaid
classDiagram
    class fn_extract_triples {
      <<fn>>
    }
    class fn_strip_variable_prefix {
      <<fn>>
    }
    class enum_PlanExpression {
      <<enum>>
    }
    class enum_PlanNode {
      <<enum>>
    }
    class struct_PlanAggregation {
      <<struct>>
      +"function: PlanAggregationFunction"
      +"distinct: bool"
      +"variable: Option~Variable~"
    }
    class enum_PlanAggregationFunction {
      <<enum>>
    }
    class fn_new_join {
      <<fn>>
    }
    class fn_extract_query_plan {
      <<fn>>
    }
    class fn_build_for_aggregate {
      <<fn>>
    }
    class fn_extract_expression {
      <<fn>>
    }
```

## Dependencies

- `crate::{Encoder, Term, Triple}`
- `spargebra::algebra::*`
- `spargebra::term::Variable`
- `std::fmt::Error`
- `std::rc::Rc`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
