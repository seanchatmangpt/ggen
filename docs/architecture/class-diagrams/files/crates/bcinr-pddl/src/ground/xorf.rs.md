# `crates/bcinr-pddl/src/ground/xorf.rs`

Source SHA-256: `5c8207a2e51180fe58dced27cbadeb7e3d824001de3f9e735f0caa78f2caf461`

```mermaid
classDiagram
    class struct_XorFilter {
      <<struct>>
      +"seed: u64"
      +"block_length: u32"
      +"fingerprints: Vec~u8~"
      +"size: usize"
    }
    class fn_mix {
      <<fn>>
    }
    class fn_reduce {
      <<fn>>
    }
    class mod_tests {
      <<mod>>
    }
    note "XorFilter"
```

## Dependencies

- `super::*`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
