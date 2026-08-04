# `crates/praxis-core/src/ocel_export.rs`

Source SHA-256: `0dfc251aa249066a750e67cddc741db62dc8fe5a21fd82eb7da11ccd8909c3e5`

```mermaid
classDiagram
    class fn_ts_ns_to_rfc3339 {
      <<fn>>
    }
    class fn_andon_label {
      <<fn>>
    }
    class fn_chain_object_id {
      <<fn>>
    }
    class fn_to_ocel {
      <<fn>>
    }
    class mod_tests {
      <<mod>>
    }
```

## Dependencies

- `chrono::{DateTime, FixedOffset, TimeZone, Utc}`
- `crate::{law::Andon, receipt_record::ReceiptRecord}`
- `std::collections::BTreeSet`
- `super::*`
- `wasm4pm_compat::ocel::{ OCELEvent, OCELEventAttribute, OCELObject, OCELRelationship, OCELType, OCEL, }`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
