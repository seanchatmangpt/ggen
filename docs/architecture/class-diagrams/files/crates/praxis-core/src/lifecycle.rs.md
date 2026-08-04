# `crates/praxis-core/src/lifecycle.rs`

Source SHA-256: `0df4f00c5a81376ff542588361aa11f523f29cb0510f076787edd8494eabdb4c`

```mermaid
classDiagram
    class mod_sealed {
      <<mod>>
    }
    class struct_Raw {
      <<struct>>
    }
    class struct_Validated {
      <<struct>>
    }
    class struct_Admitted {
      <<struct>>
    }
    class struct_Receipted {
      <<struct>>
    }
    note "sealed::Stage for Admitted"
    note "sealed::Stage for Raw"
    note "sealed::Stage for Receipted"
    note "sealed::Stage for Validated"
```

## Dependencies

- None observed.

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
