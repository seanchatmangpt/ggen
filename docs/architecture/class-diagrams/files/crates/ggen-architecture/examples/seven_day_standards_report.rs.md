# `crates/ggen-architecture/examples/seven_day_standards_report.rs`

Source SHA-256: `060c1328618a06cb3053b35b3e3e4ede3d3ff3a52e70c72d1a27b1ec60952ee5`

```mermaid
classDiagram
    class struct_VerifierReport {
      <<struct>>
      +"schema: &'static str"
      +"profile_id: String"
      +"profile_version: String"
      +"profile_digest: String"
      +"checkpoints: usize"
      +"admitted: usize"
      +"inherited: usize"
      +"pending: usize"
      +"bounded_certification_ready: bool"
      +"standards_crown_ready: bool"
      +"testing_protocol_id: String"
      +"testing_suites: usize"
      +"testing_suites_alive: usize"
      +"testing_suites_pending: usize"
      +"testing_bblock_standing: TestingBblockStanding"
      +"required_broker: String"
      +"direct_actuation: bool"
    }
    class fn_main {
      <<fn>>
    }
```

## Dependencies

- `ggen_architecture::{seven_day_standards_profile, StandardStatus, TestingBblockStanding}`
- `serde::Serialize`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
