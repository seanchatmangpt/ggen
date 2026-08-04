# `crates/ggen-cli/src/scaffolding/cli_generator/domain_layer.rs`

Source SHA-256: `dbaee7df8f0313d784f7e5228087da9aa265dfb9d1d23c0d289ab838baf2d576`

```mermaid
classDiagram
    class struct_DomainLayerGenerator {
      <<struct>>
      +"tera: Tera"
    }
    note "DomainLayerGenerator"
```

## Dependencies

- `crate::error::{GgenError, Result}`
- `crate::scaffolding::cli_generator::types::{CliProject, Noun, Verb}`
- `std::path::Path`
- `tera::{Context, Tera}`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
