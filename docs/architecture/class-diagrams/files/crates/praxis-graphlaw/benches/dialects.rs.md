# `crates/praxis-graphlaw/benches/dialects.rs`

Source SHA-256: `4084bda98bf1c4f4e33a0bcfdd7e018f9b8526dd4bc015563eb98f8590db6c46`

```mermaid
classDiagram
    class fn_build_data_index {
      <<fn>>
    }
    class fn_shacl_validate_n {
      <<fn>>
    }
    class fn_shacl_validate_100 {
      <<fn>>
    }
    class fn_shacl_validate_1000 {
      <<fn>>
    }
    class fn_shacl_validate_5000 {
      <<fn>>
    }
    class fn_shacl_validate_complex_n {
      <<fn>>
    }
    class fn_shacl_validate_complex_100 {
      <<fn>>
    }
    class fn_shacl_validate_complex_1000 {
      <<fn>>
    }
    class fn_shex_validate_n {
      <<fn>>
    }
    class fn_shex_validate_100 {
      <<fn>>
    }
    class fn_shex_validate_1000 {
      <<fn>>
    }
    class fn_shex_validate_5000 {
      <<fn>>
    }
    class fn_shex_validate_complex_n {
      <<fn>>
    }
    class fn_shex_validate_complex_100 {
      <<fn>>
    }
    class fn_shex_validate_complex_1000 {
      <<fn>>
    }
    class fn_shexc_parse_benchmark {
      <<fn>>
    }
    class fn_build_chain {
      <<fn>>
    }
    class fn_n3_chain_n {
      <<fn>>
    }
    class fn_n3_chain_depth_50 {
      <<fn>>
    }
    class fn_n3_chain_depth_150 {
      <<fn>>
    }
    class fn_n3_chain_depth_400 {
      <<fn>>
    }
    class fn_n3_parse_benchmark {
      <<fn>>
    }
    class fn_build_negation_chain {
      <<fn>>
    }
    class fn_datalog_stratify_n {
      <<fn>>
    }
    class fn_datalog_stratify_layers_20 {
      <<fn>>
    }
    class fn_datalog_stratify_layers_50 {
      <<fn>>
    }
    class fn_datalog_stratify_layers_200 {
      <<fn>>
    }
    class fn_datalog_aggregate_n {
      <<fn>>
    }
    class fn_datalog_aggregate_facts_1000 {
      <<fn>>
    }
```

## Dependencies

- `bencher::Bencher`
- `praxis_graphlaw::TripleStore`
- `praxis_graphlaw::datalog::validate_rules`
- `praxis_graphlaw::parser::{Parser, Syntax}`
- `praxis_graphlaw::shacl::{ShapesGraph, Validator as ShaclValidator}`
- `praxis_graphlaw::shex::validate_shex`
- `praxis_graphlaw::tripleindex::TripleIndex`
- `praxis_graphlaw::triples::{Aggregate, AggregateFunction, BodyLiteral, Rule, Triple}`
- `std::collections::HashMap`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
