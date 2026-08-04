# `crates/ggen-marketplace/src/packs_registry/validate.rs`

Source SHA-256: `9b56127cc3b27aa868b1d01b033cbdb10441dec872f079c1152fe0dee2c5c6f8`

```mermaid
classDiagram
    class struct_ValidationResult {
      <<struct>>
      +"pack_id: String"
      +"valid: bool"
      +"score: f64"
      +"errors: Vec~String~"
      +"warnings: Vec~String~"
      +"checks: Vec~ValidationCheck~"
    }
    class struct_ValidationCheck {
      <<struct>>
      +"name: String"
      +"passed: bool"
      +"message: String"
    }
    class fn_validate_pack {
      <<fn>>
    }
    class fn_is_valid_semver {
      <<fn>>
    }
    class mod_tests {
      <<mod>>
    }
```

## Dependencies

- `crate::marketplace::error::Result`
- `crate::packs_registry::metadata::load_pack_metadata`
- `serde::{Deserialize, Serialize}`
- `super::*`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
