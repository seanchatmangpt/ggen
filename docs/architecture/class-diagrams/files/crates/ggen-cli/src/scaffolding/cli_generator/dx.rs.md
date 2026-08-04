# `crates/ggen-cli/src/scaffolding/cli_generator/dx.rs`

Source SHA-256: `ac06b5bc540c7a5cdaa796c52db40401ef5cbbd6e26615e9169c2d9888e21afb`

```mermaid
classDiagram
    class struct_ErrorEnhancer {
      <<struct>>
    }
    class enum_ErrorContext {
      <<enum>>
    }
    class struct_CodeHints {
      <<struct>>
    }
    class struct_TemplatePreview {
      <<struct>>
    }
    class struct_AutoFix {
      <<struct>>
    }
    class struct_ProgressiveDisclosure {
      <<struct>>
    }
    note "AutoFix"
    note "CodeHints"
    note "ErrorEnhancer"
    note "ProgressiveDisclosure"
    note "TemplatePreview"
```

## Dependencies

- `crate::error::GgenError`
- `crate::scaffolding::cli_generator::types::{Argument, CliProject, Noun, Verb}`
- `std::fmt::Write`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
