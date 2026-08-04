# `crates/ggen-cli/tests/agent_lifecycle_test.rs`

Source SHA-256: `784faede75d106dafcd103c2629a841bc1a91ce3c1e98fcc1d1f743675a2e7ea`

```mermaid
classDiagram
    class struct_World {
      <<struct>>
      +"home: TempDir"
      +"project: TempDir"
      +"registry: TempDir"
    }
    class fn_run_json {
      <<fn>>
    }
    class fn_agi_completes_project_lifecycle_through_ggen_agent {
      <<fn>>
    }
    class fn_agent_install_nonexistent_pack_is_fail_closed {
      <<fn>>
    }
    class fn_agent_verify_tampered_receipt_is_invalid_via_cli {
      <<fn>>
    }
    class fn_agent_install_dry_run_writes_no_durable_state {
      <<fn>>
    }
    note "World"
```

## Dependencies

- `assert_cmd::Command`
- `serde_json::Value`
- `std::fs`
- `std::path::Path`
- `tempfile::TempDir`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
