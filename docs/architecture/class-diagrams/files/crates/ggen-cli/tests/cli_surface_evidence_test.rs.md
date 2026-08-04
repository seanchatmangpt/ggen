# `crates/ggen-cli/tests/cli_surface_evidence_test.rs`

Source SHA-256: `86df125c38a8310654bd91066eb8afbde344c8fbb132c029d47c2080ecccc990`

```mermaid
classDiagram
    class fn_receipt_noun_with_no_receipt_present_fails_closed_with_actionable_message {
      <<fn>>
    }
    class fn_receipt_default_verb_and_explicit_verb_are_equivalent_at_the_binary_boundary {
      <<fn>>
    }
    class fn_sync_run_fails_closed_on_corrupt_manifest {
      <<fn>>
    }
    class fn_doctor_default_verb_matches_the_live_run_verb_not_the_dead_check_mapping {
      <<fn>>
    }
```

## Dependencies

- `assert_cmd::Command`
- `predicates::prelude::*`
- `tempfile::TempDir`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
