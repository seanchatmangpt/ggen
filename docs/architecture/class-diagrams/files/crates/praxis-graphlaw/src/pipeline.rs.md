# `crates/praxis-graphlaw/src/pipeline.rs`

Source SHA-256: `63521e4df43d8536b42a73605bbfea66042cf901cf829acb4446c6ec60538ce9`

```mermaid
classDiagram
    class struct_WindowReasoner {
      <<struct>>
      +"store: CSprite"
      +"prev: Vec~(i32"
    }
    class fn_test_transitive {
      <<fn>>
    }
    class fn_test_compute_diff {
      <<fn>>
    }
    note "Default for WindowReasoner"
    note "TimeWindowConsumer~Triple~ for WindowReasoner"
    note "WindowReasoner"
```

## Dependencies

- `crate::Parser`
- `crate::Triple`
- `crate::csprite::CSprite`
- `crate::time_window::TimeWindow`
- `crate::time_window::TimeWindowConsumer`
- `std::cell::RefCell`
- `std::rc::Rc`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
