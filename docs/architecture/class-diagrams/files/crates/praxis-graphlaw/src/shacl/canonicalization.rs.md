# `crates/praxis-graphlaw/src/shacl/canonicalization.rs`

Source SHA-256: `6a570a3f456b2eb27154bde0cec840698cc87cb68a0c465179f367187993c369`

```mermaid
classDiagram
    class struct_UnionFind {
      <<struct>>
      +"parent: HashMap~usize"
      +"rank: HashMap~usize"
    }
    class mod_tests {
      <<mod>>
    }
    note "Default for UnionFind"
    note "UnionFind"
```

## Dependencies

- `std::collections::HashMap`
- `super::*`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
