# `benches/memory_profiling.rs`

Source SHA-256: `e92d4abd1b625f54be96f861839dcd8cf6998223f7d23fb3e9b263a01aef09e9`

```mermaid
classDiagram
    class struct_TrackingAllocator {
      <<struct>>
    }
    class fn_get_current_memory_usage {
      <<fn>>
    }
    class fn_reset_memory_tracking {
      <<fn>>
    }
    class mod_memory_tests {
      <<mod>>
    }
    class fn_run_memory_comparison {
      <<fn>>
    }
    class fn_main {
      <<fn>>
    }
    class mod_integration_tests {
      <<mod>>
    }
```

## Dependencies

- `lazy_static::lazy_static`
- `std::alloc::{GlobalAlloc, Layout, System}`
- `std::sync::atomic::{AtomicUsize, Ordering}`
- `super::*`
- `tokio::runtime::{Builder, Runtime}`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
