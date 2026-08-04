# `crates/ggen-cheat-scanner/tests/verification_subsystem_evidence_test.rs`

Source SHA-256: `081511d77b32668b180d6be25457e293611c4ecb9a5b7fbba14499a2d7e3c0b7`

```mermaid
classDiagram
    class fn_rule_ids {
      <<fn>>
    }
    class fn_verification_scanner_detects_a_freshly_planted_cheat_pattern {
      <<fn>>
    }
    class fn_verification_scanner_rejects_a_false_positive_on_clean_code {
      <<fn>>
    }
    class fn_verification_scanner_does_not_conflate_t01_with_a_missing_assertion {
      <<fn>>
    }
```

## Dependencies

- `ggen_cheat_scanner::scan_source`
- `std::path::PathBuf`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
