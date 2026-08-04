# `crates/ggen-architecture/src/certification/case_study.rs`

Source SHA-256: `0c9e4d8fbd7c1664d5ab50af8b197778c4d883d9a02958a691f1daf312ecff0b`

```mermaid
classDiagram
    class struct_TaiCaseStudy {
      <<struct>>
      +"id: String"
      +"version: String"
      +"observed_enterprise: String"
      +"target_enterprise: String"
      +"source_pack: String"
      +"source_example: String"
      +"root_broker: String"
      +"governing_equation: String"
      +"building_block_families: Vec~String~"
      +"reconstruction_phases: Vec~String~"
      +"certification_horizons: Vec~String~"
      +"counterfactual_scenarios: Vec~String~"
      +"recent_capability_rails: u16"
      +"exclusions: Vec~String~"
    }
    class fn_tai_case_study {
      <<fn>>
    }
    class enum_TaiCaseStudyRefusal {
      <<enum>>
    }
    note "TaiCaseStudy"
```

## Dependencies

- `serde::{Deserialize, Serialize}`
- `thiserror::Error`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
