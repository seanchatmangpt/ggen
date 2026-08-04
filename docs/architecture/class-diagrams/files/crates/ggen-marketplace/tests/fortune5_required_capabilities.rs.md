# `crates/ggen-marketplace/tests/fortune5_required_capabilities.rs`

Source SHA-256: `77753e6294356bdbd0b2a224d02326bd15d304e4c96794e0cdd9eed7f3d87f9d`

```mermaid
classDiagram
    class fn_full_fortune5_contract_crosses_real_boundaries {
      <<fn>>
    }
    class fn_one_missing_surface_prevents_crown_promotion {
      <<fn>>
    }
    class fn_evidence_tamper_and_duplicate_identity_fail_closed {
      <<fn>>
    }
    class fn_independent_reference_roots_replay_to_same_crown_digest {
      <<fn>>
    }
```

## Dependencies

- `ggen_marketplace::marketplace::fortune5::{ Fortune5Capability, Fortune5EvidenceLedger, Fortune5EvidenceOutcome, Fortune5EvidenceRecord, Fortune5ProofSurface, Fortune5Reference, Fortune5Standing, ALL_FORTUNE5_CAPABILITIES, REQUIRED_PROOF_SURFACES, }`
- `std::fs`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
