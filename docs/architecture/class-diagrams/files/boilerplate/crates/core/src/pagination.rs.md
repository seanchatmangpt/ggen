# `boilerplate/crates/core/src/pagination.rs`

Source SHA-256: `d2930f98860c8eb382605e197cc85a46fe8a7cc93abdf6ea39dcc5cdb04af8c4`

```mermaid
classDiagram
    class struct_Page {
      <<struct>>
      +"offset: u64"
      +"limit: u64"
    }
    class struct_PagedResult {
      <<struct>>
      +"items: Vec~T~"
      +"total: u64"
      +"offset: u64"
      +"limit: u64"
    }
    class struct_Cursor {
      <<struct>>
    }
    class struct_CursorPage {
      <<struct>>
      +"after: Option~Cursor~"
      +"limit: u64"
    }
    class struct_CursorPagedResult {
      <<struct>>
      +"items: Vec~T~"
      +"next_cursor: Option~Cursor~"
      +"has_next: bool"
    }
    class mod_tests {
      <<mod>>
    }
    note "Cursor"
    note "CursorPage"
    note "CursorPagedResult~T~"
    note "Default for Page"
    note "Page"
    note "PagedResult~T~"
```

## Dependencies

- `crate::{CoreError, Result}`
- `proptest::prelude::*`
- `serde::{Deserialize, Serialize}`
- `std::fmt::Write as _`
- `super::*`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
