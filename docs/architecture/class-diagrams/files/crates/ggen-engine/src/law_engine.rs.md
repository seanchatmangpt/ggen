# `crates/ggen-engine/src/law_engine.rs`

Source SHA-256: `028635aa17a8eeee7d50d69568567c7c8e07b9a26a1ea64490f72d197b4269d8`

```mermaid
classDiagram
    class trait_LawEngine {
      <<trait>>
      +"materialize(&self, facts_ntriples: &str, rules_n3: &str) -~ Result~MaterializeOutcome~"
      +"validate_shacl(&self, facts_ntriples: &str, shapes_ttl: &str) -~ Result~ShaclOutcome~"
      +"check_denials(&self, facts_ntriples: &str, rules_n3: &str) -~ Result~Vec~String~~"
    }
    class struct_GraphLawEngine {
      <<struct>>
    }
    note "GraphLawEngine"
    note "LawEngine for GraphLawEngine"
```

## Dependencies

- `crate::error::{AppError, Result}`
- `crate::graph::{MaterializeOutcome, ShaclOutcome}`
- `praxis_graphlaw::parser::Syntax`
- `std::collections::BTreeSet`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
