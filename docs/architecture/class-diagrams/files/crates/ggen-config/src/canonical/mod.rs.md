# `crates/ggen-config/src/canonical/mod.rs`

Source SHA-256: `7c8811a93a4950fdc59ee4b74469ac2e2bc63191386cc2f4e6d03c6a1b32ca5a`

```mermaid
classDiagram
    class mod_json {
      <<mod>>
    }
    class enum_CanonicalError {
      <<enum>>
    }
    class type_Result {
      <<type>>
    }
    class trait_Canonicalizer {
      <<trait>>
      +"canonicalize(&self, input: Self::Input) -~ Result~Self::Output~"
      +"hash(&self, input: Self::Input) -~ Result~String~"
    }
    class struct_Canonical {
      <<struct>>
      +"inner: T"
    }
    class mod_tests {
      <<mod>>
    }
    note "Canonical~T~"
    note "std::fmt::Display for Canonical~T~"
```

## Dependencies

- `crate::canonical::json::JsonCanonicalizer`
- `super::*`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
