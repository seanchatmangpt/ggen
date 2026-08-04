# `crates/chicago-tdd-tools/src/core/test_utils.rs`

Source SHA-256: `e54fd88b3681d86299c5be8a353fd0ab0e145f16a1c3b0cc7c7a2d3ceed2501c`

```mermaid
classDiagram
    class struct_RetryConfig {
      <<struct>>
      +"max_attempts: usize"
      +"delay: Duration"
      +"exponential_backoff: bool"
    }
    class struct_TempDir {
      <<struct>>
      +"path: PathBuf"
    }
    class struct_TestTimer {
      <<struct>>
      +"start: Instant"
    }
    class struct_TestData {
      <<struct>>
    }
    class mod_tests {
      <<mod>>
    }
    note "Default for RetryConfig"
    note "Drop for TempDir"
    note "RetryConfig"
    note "TempDir"
    note "TestData"
    note "TestTimer"
```

## Dependencies

- `crate::test`
- `std::path::PathBuf`
- `std::time::{Duration, Instant}`
- `super::*`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
