# `crates/praxis-graphlaw/src/sparql/eval.rs`

Source SHA-256: `7614ac3e732af9e8991290c94d26abc29ddd2ea9a9f8e34a6fa290c33c700b7e`

```mermaid
classDiagram
    class enum_EncodedTerm {
      <<enum>>
    }
    class fn_encode_term {
      <<fn>>
    }
    class fn_eval_expression {
      <<fn>>
    }
    class fn_partial_compare_helper {
      <<fn>>
    }
    class fn_to_bool {
      <<fn>>
    }
    note "From~String~ for EncodedTerm"
    note "From~bool~ for EncodedTerm"
    note "From~i64~ for EncodedTerm"
```

## Dependencies

- `crate::Encoder`
- `crate::tripleindex::EncodedBinding`
- `std::cmp::Ordering`
- `super::plan::PlanExpression`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
