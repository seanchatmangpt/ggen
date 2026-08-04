# `crates/chicago-tdd-tools/src/cli_proof/mod.rs`

Source SHA-256: `cf686fea40e84c0100e999681d4a4da4be0188cfaa7857b164dfa5302d9dd53c`

```mermaid
classDiagram
    class mod_harness {
      <<mod>>
    }
    class mod_receipt {
      <<mod>>
    }
    class mod_sabotage {
      <<mod>>
    }
    class mod_workspace {
      <<mod>>
    }
```

## Dependencies

- `harness::{CliHarness, CliOutput}`
- `receipt::ReceiptAssertions`
- `sabotage::SabotageFixture`
- `workspace::TempWorkspace`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
