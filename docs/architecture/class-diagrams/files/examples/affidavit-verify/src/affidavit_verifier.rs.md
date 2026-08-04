# `examples/affidavit-verify/src/affidavit_verifier.rs`

Source SHA-256: `a8501886233d2486cae22d488d1f566518c49970796b8d6af6deca625587e5a6`

```mermaid
classDiagram
    class fn_is_well_formed_hash {
      <<fn>>
    }
    class fn_verify {
      <<fn>>
    }
    class fn_stage_decode {
      <<fn>>
    }
    class fn_stage_check_format {
      <<fn>>
    }
    class fn_stage_chain_integrity {
      <<fn>>
    }
    class fn_stage_continuity {
      <<fn>>
    }
    class fn_stage_verify_commitments {
      <<fn>>
    }
    class fn_stage_evaluate_profile {
      <<fn>>
    }
    class mod_tests {
      <<mod>>
    }
```

## Dependencies

- `crate::affidavit_chain::{recompute_chain, FORMAT_VERSION}`
- `crate::affidavit_types::{Blake3Hash, ObjectRef, OperationEvent}`
- `crate::affidavit_types::{CheckOutcome, ProfileId, Receipt, Verdict}`
- `std::collections::BTreeSet`
- `super::*`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
