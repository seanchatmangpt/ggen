# `crates/praxis-graphlaw/src/imars_reasoner.rs`

Source SHA-256: `525dcaab48884874763d9c2a7b5838cd2ed999da9e90b2b549a64741d987b217`

```mermaid
classDiagram
    class struct_ImarsReasoner {
      <<struct>>
      +"store: CSprite"
      +"new: Vec~(i32"
      +"old: Vec~(i32"
      +"window: RefCell~Weak~RefCell~ImarsWindow~Triple~~~~"
    }
    class fn_test_integration {
      <<fn>>
    }
    class fn_test_transitive {
      <<fn>>
    }
    note "ImarsReasoner"
    note "WindowConsumer~Triple~ for ImarsReasoner"
```

## Dependencies

- `crate::Parser`
- `crate::Triple`
- `crate::csprite::CSprite`
- `crate::imars_window::{ImarsWindow, WindowConsumer}`
- `std::cell::RefCell`
- `std::rc::{Rc, Weak}`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
