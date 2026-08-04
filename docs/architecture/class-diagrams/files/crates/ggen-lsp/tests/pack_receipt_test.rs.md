# `crates/ggen-lsp/tests/pack_receipt_test.rs`

Source SHA-256: `d6b87b23627508ff57a18a5723228e7917bc9a9282d11470e5b07c6c30a3b46f`

```mermaid
classDiagram
    class fn_pack_binds_scan_to_pack_then_tamper_breaks_verification {
      <<fn>>
    }
    class fn_pack_without_scan_still_has_a_verifiable_self_receipt {
      <<fn>>
    }
```

## Dependencies

- `ggen_lsp::{emit_pack, verify_pack, PackOptions}`
- `tempfile::TempDir`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
