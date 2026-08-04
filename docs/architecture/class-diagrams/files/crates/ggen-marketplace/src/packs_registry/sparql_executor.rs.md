# `crates/ggen-marketplace/src/packs_registry/sparql_executor.rs`

Source SHA-256: `fdb9975388b75fcb2edfedc2a0cf8ef585dd3ad4c8dfc4ee44a0f6e19f937129`

```mermaid
classDiagram
    class struct_SparqlExecutor {
      <<struct>>
      +"store: Store"
      +"cache: HashMap~String"
    }
    class struct_CachedResult {
      <<struct>>
      +"result: SparqlResult"
      +"timestamp: Instant"
      +"ttl: Duration"
    }
    class struct_SparqlResult {
      <<struct>>
      +"columns: Vec~String~"
      +"rows: Vec~Vec~Value~~"
      +"execution_time: Duration"
    }
    class enum_Value {
      <<enum>>
    }
    class struct_CompiledQuery {
      <<struct>>
      +"query_string: String"
    }
    class struct_CacheStats {
      <<struct>>
      +"total_entries: usize"
      +"valid_entries: usize"
      +"expired_entries: usize"
    }
    class mod_tests {
      <<mod>>
    }
    note "Default for SparqlExecutor"
    note "SparqlExecutor"
```

## Dependencies

- `crate::marketplace::error::{Error, Result}`
- `crate::packs_registry::types::Pack`
- `crate::packs_registry::types::{PackDependency, PackMetadata, PackTemplate}`
- `oxigraph::model::*`
- `oxigraph::sparql::QueryResults`
- `oxigraph::store::Store`
- `serde::{Deserialize, Serialize}`
- `std::collections::HashMap`
- `std::time::{Duration, Instant}`
- `super::*`
- `tracing::{debug, info}`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
