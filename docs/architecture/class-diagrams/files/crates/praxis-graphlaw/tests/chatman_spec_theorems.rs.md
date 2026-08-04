# `crates/praxis-graphlaw/tests/chatman_spec_theorems.rs`

Source SHA-256: `df407da4b09fc5bf13d6f4cb2c5d8c0340e0b82e6d9103352e43be9bde990fe6`

```mermaid
classDiagram
    class fn_provocation_table_has_a_row_for_every_refusal_variant {
      <<fn>>
    }
    class fn_covered_variant_count_matches_this_sessions_honest_total {
      <<fn>>
    }
    class fn_covered_rows_cite_non_empty_unique_test_names {
      <<fn>>
    }
    class fn_provocation_table_hash_is_deterministic_within_one_run {
      <<fn>>
    }
```

## Dependencies

- `chicago_tdd_tools::prelude::*`
- `praxis_graphlaw::chatman::abi::ALL_REFUSAL_NAMES`
- `std::collections::BTreeSet`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
