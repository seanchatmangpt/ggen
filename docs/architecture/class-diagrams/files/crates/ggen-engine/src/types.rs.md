# `crates/ggen-engine/src/types.rs`

Source SHA-256: `df08c727d57464ab9df22ca86df8da31a70b3aa9689413b50e40e2332900517a`

```mermaid
classDiagram
    class struct_Blake3Hash {
      <<struct>>
    }
    class struct_ObjectRef {
      <<struct>>
      +"id: String"
      +"type_: String"
      +"qualifier: Option~String~"
    }
    class struct_ObjectRefParseError {
      <<struct>>
      +"input: String"
      +"reason: &'static str"
    }
    class mod_object_ref_parse_tests {
      <<mod>>
    }
    class fn_canonical_bytes {
      <<fn>>
    }
    class fn_sort_value {
      <<fn>>
    }
    class mod_sealed {
      <<mod>>
    }
    class struct_Raw {
      <<struct>>
    }
    class struct_Validated {
      <<struct>>
    }
    class struct_Admitted {
      <<struct>>
    }
    class struct_Evidence {
      <<struct>>
      +"inner: T"
      +"_state: PhantomData~State~"
      +"_witness: PhantomData~Witness~"
    }
    class trait_Admit {
      <<trait>>
      +"admit(
        input: Evidence~Self::Input, Raw, Self::Witness~,
    ) -~ Result~Evidence~Self::Input, Admitted, Self::Witness~, Self::Error~"
    }
    class type_RawEvidence {
      <<type>>
    }
    class type_ValidatedEvidence {
      <<type>>
    }
    class type_AdmittedEvidence {
      <<type>>
    }
    class struct_AdmittedReceipt {
      <<struct>>
      +"chain_hash: [u8; 32]"
      +"timestamp: u64"
      +"_seal: ()"
    }
    class enum_ProfileId {
      <<enum>>
    }
    class enum_PolicyVerdict {
      <<enum>>
    }
    class struct_PolicyConfig {
      <<struct>>
      +"behind_threshold: usize"
      +"dirty_threshold: usize"
      +"evidence_staleness_secs: u64"
    }
    class trait_CicdPolicy {
      <<trait>>
      +"name(&self) -~ &'static str"
      +"evaluate(&self, config: &PolicyConfig) -~ PolicyVerdict"
    }
    class mod_layout_assertions {
      <<mod>>
    }
    class struct_StageOutcome {
      <<struct>>
      +"stage: String"
      +"passed: bool"
      +"reason: Option~String~"
    }
    class struct_Verdict {
      <<struct>>
      +"accepted: bool"
      +"stage_outcomes: Vec~StageOutcome~"
    }
    class mod_verdict_tests {
      <<mod>>
    }
    class struct_CicdPolicyRunner {
      <<struct>>
      +"policies: Vec~Box~dyn CicdPolicy~~"
    }
    class struct_PolicyFinding {
      <<struct>>
      +"policy_name: &'static str"
      +"verdict: PolicyVerdict"
    }
    class mod_policy_runner_tests {
      <<mod>>
    }
    class mod_tests {
      <<mod>>
    }
    class mod_canonical_determinism_tests {
      <<mod>>
    }
    class struct_HashWitness {
      <<struct>>
    }
    class enum_InvalidHash {
      <<enum>>
    }
    class struct_HashAdmit {
      <<struct>>
    }
    class mod_hash_admit_tests {
      <<mod>>
    }
    note "Admit for HashAdmit"
    note "AdmittedReceipt"
    note "Blake3Hash"
    note "CicdPolicyRunner"
    note "Default for PolicyConfig"
    note "Evidence~T"
    note "From~Blake3Hash~ for String"
    note "From~String~ for Blake3Hash"
    note "ObjectRef"
    note "PolicyConfig"
    note "PolicyVerdict"
    note "ProfileId"
    note "StageOutcome"
    note "Verdict"
    note "fmt::Display for Blake3Hash"
    note "fmt::Display for ObjectRef"
    note "fmt::Display for Verdict"
    note "sealed::LifecycleState for Admitted"
    note "sealed::LifecycleState for Raw"
    note "sealed::LifecycleState for Validated"
    note "std::str::FromStr for ObjectRef"
```

## Dependencies

- `PolicyVerdict::*`
- `serde::Serialize`
- `serde::{Deserialize, Serialize}`
- `serde_json::Value`
- `std::mem::{align_of, size_of}`
- `std::{fmt, marker::PhantomData}`
- `super::*`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
