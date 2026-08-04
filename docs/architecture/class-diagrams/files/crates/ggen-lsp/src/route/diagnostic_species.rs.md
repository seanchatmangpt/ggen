# `crates/ggen-lsp/src/route/diagnostic_species.rs`

Source SHA-256: `c9f30f714d9b4b078042b379a27eb4cdb3a972cfd36c2e99f30cfcf450aced66`

```mermaid
classDiagram
    class struct_DiagnosticSpecies {
      <<struct>>
      +"code: &'static str"
      +"failure_class: &'static str"
      +"surfaces: &'static [&'static str]"
      +"severity_policy: &'static str"
      +"route: &'static str"
      +"origin: &'static str"
      +"actuation_boundary: &'static str"
      +"receipt_requirement: &'static str"
      +"detector_active: bool"
    }
    class fn_species_registry {
      <<fn>>
    }
    class fn_species_for {
      <<fn>>
    }
    class mod_tests {
      <<mod>>
    }
```

## Dependencies

- `super::*`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
