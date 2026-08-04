# `crates/praxis-graphlaw/tests/chatman_static_gates.rs`

Source SHA-256: `535d85da372928bc17076c4ea81a9a3203f31065621aa68010431d6474f287b5`

```mermaid
classDiagram
    class fn_crate_root {
      <<fn>>
    }
    class fn_rs_files_under {
      <<fn>>
    }
    class fn_read {
      <<fn>>
    }
    class fn_is_comment_line {
      <<fn>>
    }
    class fn_scan_forbidden_tokens {
      <<fn>>
    }
    class fn_scan_duplicate_canonical_types {
      <<fn>>
    }
    class fn_scan_broad_allow {
      <<fn>>
    }
    class fn_scan_n3_default_on {
      <<fn>>
    }
    class fn_scan_silent_fallback {
      <<fn>>
    }
    class fn_assert_clean {
      <<fn>>
    }
    class fn_gate_no_forbidden_tokens_in_chatman {
      <<fn>>
    }
    class fn_gate_no_duplicate_canonical_types_in_crate {
      <<fn>>
    }
    class fn_gate_no_broad_allow_in_chatman {
      <<fn>>
    }
    class fn_gate_n3_not_default_in_chatman {
      <<fn>>
    }
    class fn_gate_no_silent_fallback_in_chatman {
      <<fn>>
    }
    class fn_gates_can_fail {
      <<fn>>
    }
    class fn_gate_schema_refusal_enums_match_abi {
      <<fn>>
    }
    class fn_gate_refusal_name_matches_const_list {
      <<fn>>
    }
    class fn_receipt_from_sorted_nquads_verifies {
      <<fn>>
    }
    class fn_receipt_refuses_unsorted_nquads {
      <<fn>>
    }
    class fn_envelope_hash_is_order_insensitive_over_handles {
      <<fn>>
    }
```

## Dependencies

- `praxis_graphlaw::chatman::abi::{ GraphSnapshotId, InputHandles, InvocationEnvelope, InvocationId, OperatorId, ProfileId, }`
- `praxis_graphlaw::chatman::abi::{Refusal, ALL_REFUSAL_NAMES}`
- `std::collections::BTreeSet`
- `std::fs`
- `std::path::{Path, PathBuf}`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
