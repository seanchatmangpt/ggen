# `crates/praxis-graphlaw/tests/business_logic_cases_suite4.rs`

Source SHA-256: `7d2971b85968b3ee1f6522359729f4a2eaee4e33964a9ebee3ba912260c9ae7e`

```mermaid
classDiagram
    class fn_test_suite4_idempotency_same_key_twice {
      <<fn>>
    }
    class fn_test_suite4_idempotency_no_duplicate_triple {
      <<fn>>
    }
    class fn_test_suite4_idempotency_receipt_sharing_key {
      <<fn>>
    }
    class fn_test_suite4_idempotency_malformed_missing_field {
      <<fn>>
    }
```

## Dependencies

- `praxis_graphlaw::TripleStore`
- `praxis_graphlaw::parser::Syntax`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
