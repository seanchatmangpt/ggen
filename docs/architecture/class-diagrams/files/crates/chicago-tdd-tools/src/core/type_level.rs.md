# `crates/chicago-tdd-tools/src/core/type_level.rs`

Source SHA-256: `76f4aa6c35bc36326ae88a5a784bb80703899323e7f565f42b0054341d49a13e`

```mermaid
classDiagram
    class struct_ValidatedSize {
      <<struct>>
    }
    class trait_ConstSizeValid {
      <<trait>>
    }
    class struct_ValidatedRange {
      <<struct>>
    }
    class trait_ConstRangeValid {
      <<trait>>
    }
    class mod_arithmetic {
      <<mod>>
    }
    class struct_SizeValidatedArray {
      <<struct>>
      +"data: [u8; SIZE]"
    }
    class mod_tests {
      <<mod>>
    }
    note "SizeValidatedArray~SIZE"
```

## Dependencies

- `crate::assert_eq_msg`
- `super::SizeValidatedArray`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
