# `docs/examples/signature_validation.rs`

Source SHA-256: `7cc7f76db87de23716b339b06d4564f125ed99770c7b1bc29a5a0630bde6095a`

```mermaid
classDiagram
    class struct_Signature {
      <<struct>>
      +"name: String"
      +"description: String"
      +"inputs: Vec~Field~"
      +"outputs: Vec~Field~"
    }
    class struct_Field {
      <<struct>>
      +"name: String"
      +"type_annotation: String"
      +"description: String"
      +"required: bool"
      +"constraints: Vec~Constraint~"
    }
    class enum_Constraint {
      <<enum>>
    }
    class struct_SignatureValidator {
      <<struct>>
      +"signature: Signature"
    }
    class struct_ValidationResult {
      <<struct>>
      +"valid: bool"
      +"errors: Vec~String~"
    }
    class fn_main {
      <<fn>>
    }
    note "Field"
    note "Signature"
    note "SignatureValidator"
```

## Dependencies

- `serde_json::json`
- `std::collections::HashMap`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
