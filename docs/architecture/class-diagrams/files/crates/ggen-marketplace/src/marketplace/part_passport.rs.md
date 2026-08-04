# `crates/ggen-marketplace/src/marketplace/part_passport.rs`

Source SHA-256: `757f6d97c726df742019af30a1eb4de60ea5bfa0f2dae19fbb95f843a50572d8`

```mermaid
classDiagram
    class struct_PartPassport {
      <<struct>>
      +"schema_version: u16"
      +"identity: PartIdentity"
      +"input: InputEnvelope"
      +"output: OutputContract"
      +"polarity: CausalPolarity"
      +"resources: ResourceEnvelope"
      +"isolation: IsolationClass"
      +"host_profiles: BTreeSet~HostProfile~"
      +"jurisdiction_profiles: BTreeSet~String~"
      +"conformity: Vec~ConformityMark~"
      +"verifiers: Vec~VerifierMark~"
      +"noninterference: NonInterferenceProfile"
      +"lifecycle: LifecyclePolicy"
    }
    class struct_PartIdentity {
      <<struct>>
      +"part_id: String"
      +"family: String"
      +"version: String"
      +"manufacturer: String"
      +"artifact_digest: String"
      +"source_graph_digest: String"
      +"build_receipt: String"
      +"batch_id: Option~String~"
    }
    class struct_InputEnvelope {
      <<struct>>
      +"representation: String"
      +"schema: String"
      +"protocol: ProtocolRange"
      +"max_payload_bytes: u64"
      +"accepted_event_types: BTreeSet~String~"
      +"required_fields: BTreeSet~String~"
      +"temporal: TemporalProfile"
    }
    class struct_ProtocolRange {
      <<struct>>
      +"minimum: String"
      +"maximum: String"
    }
    class struct_TemporalProfile {
      <<struct>>
      +"clock: ClockDiscipline"
      +"max_events_per_second: u64"
      +"timeout_semantics: TimeoutSemantics"
      +"wall_clock_access: bool"
    }
    class enum_ClockDiscipline {
      <<enum>>
    }
    class enum_TimeoutSemantics {
      <<enum>>
    }
    class struct_OutputContract {
      <<struct>>
      +"representation: String"
      +"schema: String"
      +"artifact_type: String"
      +"deterministic_serialization: bool"
      +"receipt_required: bool"
      +"max_artifacts: u32"
    }
    class struct_CausalPolarity {
      <<struct>>
      +"consumes: BTreeSet~String~"
      +"produces: BTreeSet~String~"
      +"requires_authorities: BTreeSet~String~"
      +"emits_intents: BTreeSet~String~"
    }
    class struct_ResourceEnvelope {
      <<struct>>
      +"max_memory_pages: u32"
      +"max_fuel: u64"
      +"max_execution_ms: u64"
      +"max_queue_depth: u32"
      +"max_concurrency: u16"
      +"ambient_io: bool"
    }
    class enum_IsolationClass {
      <<enum>>
    }
    class struct_HostProfile {
      <<struct>>
    }
    class struct_ConformityMark {
      <<struct>>
      +"profile: String"
      +"issuer: String"
      +"evidence_uri: String"
      +"artifact_digest: String"
      +"self_declared: bool"
    }
    class enum_VerifierStatus {
      <<enum>>
    }
    class struct_VerifierMark {
      <<struct>>
      +"verifier: String"
      +"property: String"
      +"evidence_uri: String"
      +"artifact_digest: String"
      +"status: VerifierStatus"
    }
    class struct_NonInterferenceProfile {
      <<struct>>
      +"may_read: BTreeSet~String~"
      +"may_write: BTreeSet~String~"
      +"may_emit: BTreeSet~String~"
      +"forbidden: BTreeSet~String~"
    }
    class enum_LifecycleState {
      <<enum>>
    }
    class struct_LifecyclePolicy {
      <<struct>>
      +"state: LifecycleState"
      +"policy_version: String"
      +"retirement: RetirementPolicy"
    }
    class struct_RetirementPolicy {
      <<struct>>
      +"revoke_credentials: bool"
      +"preserve_receipts: bool"
      +"migration_target: Option~String~"
      +"disposal_instructions: String"
    }
    class struct_PassportBinding {
      <<struct>>
      +"package_id: PackageId"
      +"package_version: PackageVersion"
      +"passport: PartPassport"
    }
    class enum_NameplateMark {
      <<enum>>
    }
    class enum_PassportViolationCode {
      <<enum>>
    }
    class struct_PassportViolation {
      <<struct>>
      +"code: PassportViolationCode"
      +"field: String"
      +"message: String"
    }
    class struct_PassportValidationReport {
      <<struct>>
      +"violations: Vec~PassportViolation~"
    }
    class enum_SubstitutionViolationCode {
      <<enum>>
    }
    class struct_SubstitutionViolation {
      <<struct>>
      +"code: SubstitutionViolationCode"
      +"message: String"
    }
    class struct_SubstitutionReport {
      <<struct>>
      +"compatible: bool"
      +"violations: Vec~SubstitutionViolation~"
    }
    class fn_require_non_empty {
      <<fn>>
    }
    class fn_require_non_empty_set {
      <<fn>>
    }
    class fn_require_positive {
      <<fn>>
    }
    class fn_validate_digest {
      <<fn>>
    }
    class fn_is_supported_digest {
      <<fn>>
    }
    class fn_validate_evidence_digest {
      <<fn>>
    }
    class fn_validate_noninterference {
      <<fn>>
    }
    class fn_validate_noninterference_conflicts {
      <<fn>>
    }
    class fn_compare_equal {
      <<fn>>
    }
    class fn_resources_fit {
      <<fn>>
    }
    class fn_noninterference_is_substitutable {
      <<fn>>
    }
    class mod_tests {
      <<mod>>
    }
    note "HostProfile"
    note "LifecycleState"
    note "PartPassport"
    note "PassportBinding"
    note "PassportValidationReport"
    note "PassportViolation"
    note "ProtocolRange"
    note "SubstitutionReport"
    note "fmt::Display for HostProfile"
    note "fmt::Display for NameplateMark"
```

## Dependencies

- `crate::marketplace::models::PackageMetadata`
- `crate::marketplace::models::{Manifest, PackageId, PackageVersion}`
- `indexmap::IndexMap`
- `semver::Version`
- `serde::{Deserialize, Serialize}`
- `std::collections::BTreeSet`
- `std::fmt::{self, Write}`
- `super::*`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
