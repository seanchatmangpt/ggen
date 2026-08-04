# `crates/chicago-tdd-tools/src/core/poka_yoke.rs`

Source SHA-256: `c0f6626d1675636e5c00704838542e59fcca15a328b3d50861d0d43c27b58a49`

```mermaid
classDiagram
    class struct_AssertOkCalled {
      <<struct>>
    }
    class struct_BehaviorVerified {
      <<struct>>
    }
    class struct_BehaviorVerification {
      <<struct>>
      +"value: T"
      +"_state: PhantomData~State~"
    }
    class struct_TestResult {
      <<struct>>
      +"result: Result~T"
    }
    class mod_tests {
      <<mod>>
    }
    note "BehaviorVerification~AssertOkCalled"
    note "BehaviorVerification~BehaviorVerified"
    note "From~Result~T"
    note "TestResult~T"
```

## Dependencies

- `std::marker::PhantomData`
- `super::*`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
