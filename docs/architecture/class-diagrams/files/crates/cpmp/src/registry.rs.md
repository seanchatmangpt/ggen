# `crates/cpmp/src/registry.rs`

Source SHA-256: `ee768d7f8aabf38181c4229ccfe61d73487955fd05de204b343f43bed040b4c8`

```mermaid
classDiagram
    class enum_RegistryError {
      <<enum>>
    }
    class type_RegistryResult {
      <<type>>
    }
    class struct_OntologyRegistry {
      <<struct>>
      +"catalog_store: Store"
      +"entries: HashMap~String"
    }
    class fn_load_ttl_into_store {
      <<fn>>
    }
    class fn_build_entries {
      <<fn>>
    }
    class fn_tier0 {
      <<fn>>
    }
    class fn_cached {
      <<fn>>
    }
    class fn_referenced {
      <<fn>>
    }
    note "OntologyRegistry"
```

## Dependencies

- `crate::entry::{Capability, OntologyContent, OntologyEntry}`
- `crate::tier::{OntologyAuthority, OntologyTier}`
- `oxigraph::io::{RdfFormat, RdfParser}`
- `oxigraph::store::Store`
- `std::collections::HashMap`
- `std::sync::OnceLock`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
