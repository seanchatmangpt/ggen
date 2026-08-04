# `crates/praxis-graphlaw/src/sparql/mod.rs`

Source SHA-256: `8ad516c6c1358cf0e155437b2bf43c0a3113c42c05f76a5972ccff37a262569b`

```mermaid
classDiagram
    class mod_accumulators {
      <<mod>>
    }
    class mod_eval {
      <<mod>>
    }
    class mod_plan {
      <<mod>>
    }
    class struct_Binding {
      <<struct>>
      +"var: String"
      +"val: String"
    }
    class fn_decode {
      <<fn>>
    }
    class fn_evaluate_plan_and_debug {
      <<fn>>
    }
    class fn_evaluate_plan {
      <<fn>>
    }
    class struct_QueryResults {
      <<struct>>
      +"plan: PlanNode"
      +"iterator: Box~dyn Iterator~Item = Vec~EncodedBinding~~~"
    }
    class fn_eval_query {
      <<fn>>
    }
    class mod_sparql_test {
      <<mod>>
    }
    note "Iterator for QueryResults"
```

## Dependencies

- `accumulators::Accumulator`
- `accumulators::{ AccumulatorImpl, AvgAccumulator, CountAccumulator, MaxAccumulator, MinAccumulator, SumAccumulator, }`
- `crate::tripleindex::EncodedBinding`
- `crate::{Encoder, TripleIndex}`
- `eval::{encode_term, eval_expression, to_bool, EncodedTerm}`
- `plan::{ extract_expression, extract_query_plan, strip_variable_prefix, PlanAggregation, PlanAggregationFunction, PlanExpression, PlanNode, }`
- `spargebra::Query`
- `std::cell::RefCell`
- `std::collections::HashMap`
- `std::iter::empty`
- `std::rc::Rc`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
