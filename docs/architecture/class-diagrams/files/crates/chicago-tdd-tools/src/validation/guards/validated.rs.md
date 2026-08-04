# `crates/chicago-tdd-tools/src/validation/guards/validated.rs`

Source SHA-256: `50e254ff2e54da32f563310abc5f52317b3915d4d5f3019d37f1c7095e49a801`

```mermaid
classDiagram
    class struct_ValidatedRun {
      <<struct>>
      +"inner: Validated~Vec~u8~~"
    }
    class trait_AssertRunLen {
      <<trait>>
    }
    class trait_Valid {
      <<trait>>
    }
    class struct_ValidatedBatch {
      <<struct>>
      +"inner: Validated~Vec~u8~~"
    }
    class trait_AssertBatchSize {
      <<trait>>
    }
    class mod_tests {
      <<mod>>
    }
    note "AssertBatchSize~0~"
    note "AssertBatchSize~1000~"
    note "AssertBatchSize~100~"
    note "AssertBatchSize~200~"
    note "AssertBatchSize~300~"
    note "AssertBatchSize~400~"
    note "AssertBatchSize~500~"
    note "AssertBatchSize~600~"
    note "AssertBatchSize~700~"
    note "AssertBatchSize~800~"
    note "AssertBatchSize~900~"
    note "AssertRunLen~0~"
    note "AssertRunLen~1~"
    note "AssertRunLen~2~"
    note "AssertRunLen~3~"
    note "AssertRunLen~4~"
    note "AssertRunLen~5~"
    note "AssertRunLen~6~"
    note "AssertRunLen~7~"
    note "AssertRunLen~8~"
    note "Valid"
    note "ValidatedBatch~SIZE~"
    note "ValidatedRun~LEN~"
```

## Dependencies

- `crate::core::const_assert::Validated`
- `crate::validation::guards::GuardConstraintError`
- `super::*`
- `super::{MAX_BATCH_SIZE, MAX_RUN_LEN}`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
