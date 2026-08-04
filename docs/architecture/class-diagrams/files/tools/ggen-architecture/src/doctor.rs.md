# `tools/ggen-architecture/src/doctor.rs`

Source SHA-256: `cd7dee22640f9ba3e8f39deff392c87d84ed4685fa0db39550150065836001cb`

```mermaid
classDiagram
    class enum_DoctorStatus {
      <<enum>>
    }
    class struct_DoctorFinding {
      <<struct>>
      +"code: String"
      +"severity: Severity"
      +"subject: String"
      +"message: String"
      +"remediation: String"
    }
    class struct_DoctorReport {
      <<struct>>
      +"status: DoctorStatus"
      +"findings: Vec~DoctorFinding~"
      +"metrics: BTreeMap~String"
      +"receipt_hash: String"
    }
    class struct_DoctorReceiptBody {
      <<struct>>
      +"status: DoctorStatus"
      +"findings: &'a [DoctorFinding]"
      +"metrics: &'a BTreeMap~String"
    }
    note "DoctorReport"
```

## Dependencies

- `crate::{ capacity::{CapacityEnvelope, CapacityLevel}, error::Result, model::{AssetKind, LifecycleState, Severity, Standing}, receipt::deterministic_hash, state::ArchitectureState, }`
- `serde::{Deserialize, Serialize}`
- `std::{collections::BTreeMap, fmt::Write as _}`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
