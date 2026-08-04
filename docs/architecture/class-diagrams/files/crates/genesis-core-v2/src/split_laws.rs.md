# `crates/genesis-core-v2/src/split_laws.rs`

Source SHA-256: `c4801814c870a2ee178f3749e99a1faedb2b4029ad266d63ae29b414e89f67a1`

```mermaid
classDiagram
    class struct_SplitResult {
      <<struct>>
      +"left_page: RelationPage~HALF~"
      +"right_page: RelationPage~HALF~"
      +"left_receipt: Receipt"
      +"right_receipt: Receipt"
    }
    class fn_need9_split {
      <<fn>>
    }
    class fn_need257_split {
      <<fn>>
    }
    class fn_receipt_over_page {
      <<fn>>
    }
    class mod_tests {
      <<mod>>
    }
```

## Dependencies

- `crate::primitives::{Construct8, Pair2, Receipt, Refusal, RefusalReason, RelationPage}`
- `crate::primitives::{Pair2, RefusalReason, RelationPage}`
- `super::*`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
