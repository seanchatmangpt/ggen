# `tools/pack-gall/src/verify.rs`

Source SHA-256: `ea2a8fa577ab6cba49245218f92e97b16364ebc5bac804e54835abd2f3cedd7c`

```mermaid
classDiagram
    class fn_verify {
      <<fn>>
    }
    class fn_issue_receipt {
      <<fn>>
    }
    class fn_cycle_is_refused {
      <<fn>>
    }
    class fn_verify_schema_shape {
      <<fn>>
    }
    class fn_checkpoint {
      <<fn>>
    }
```

## Dependencies

- `crate::io::{digest_json, digest_path}`
- `crate::model::{ Catalog, Checkpoint, Observation, Receipt, VerifierReport, OBSERVATION_SCHEMA, RECEIPT_SCHEMA, VERIFIER_SCHEMA, }`
- `crate::observe::{load_contract, observe}`
- `crate::resolver::resolve`
- `serde_json::Value`
- `std::collections::{BTreeMap, BTreeSet}`
- `std::fs`
- `std::path::Path`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
