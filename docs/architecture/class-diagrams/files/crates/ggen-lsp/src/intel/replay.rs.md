# `crates/ggen-lsp/src/intel/replay.rs`

Source SHA-256: `234e9c173ce17804188b1ee017f96f41f32826c53e0e0ccab1a259bcc3d29bb2`

```mermaid
classDiagram
    class struct_CaseReplay {
      <<struct>>
      +"case_id: String"
      +"found: bool"
      +"diagnostic_code: Option~String~"
      +"route_id: Option~String~"
      +"route_source: Option~String~"
      +"gate_outcome: Option~String~"
      +"conformant: bool"
      +"receipt_id: Option~String~"
      +"event_count: usize"
    }
    class fn_replay_case {
      <<fn>>
    }
    class struct_PromotionReplay {
      <<struct>>
      +"matches: bool"
      +"reason: String"
    }
    class fn_verify_promotion {
      <<fn>>
    }
    class fn_find_promotion_receipt {
      <<fn>>
    }
    class mod_tests {
      <<mod>>
    }
```

## Dependencies

- `crate::intel::IntelLog`
- `crate::intel::events::{ diagnostic_raised, gate_result, new_run_id, receipt_emitted, route_selected, }`
- `crate::route::{default_pack_routes_path, PromotedRoutes}`
- `serde::Serialize`
- `std::io::Write`
- `std::path::Path`
- `super::*`
- `super::events::{activity, obj_type}`
- `super::log::IntelLog`
- `tempfile::TempDir`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
