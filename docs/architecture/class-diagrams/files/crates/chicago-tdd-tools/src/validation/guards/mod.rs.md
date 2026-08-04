# `crates/chicago-tdd-tools/src/validation/guards/mod.rs`

Source SHA-256: `db4001fd3f3656c4132de0db1060df8765b20497c4bea193acdc805bb2e86149`

```mermaid
classDiagram
    class enum_GuardConstraintError {
      <<enum>>
    }
    class type_GuardConstraintResult {
      <<type>>
    }
    class struct_GuardValidator {
      <<struct>>
      +"max_run_len: usize"
      +"max_batch_size: usize"
    }
    class fn_assert_guard_run_len {
      <<fn>>
    }
    class fn_assert_guard_batch_size {
      <<fn>>
    }
    class mod_validated {
      <<mod>>
    }
    class mod_tests {
      <<mod>>
    }
    note "Default for GuardValidator"
    note "GuardValidator"
```

## Dependencies

- `super::*`
- `thiserror::Error`
- `validated::{AssertBatchSize, AssertRunLen, ValidatedBatch, ValidatedRun}`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
