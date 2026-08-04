# `crates/ggen-cli/src/cmds/receipt.rs`

Source SHA-256: `a3759e3d199181132deeff8abd4974af4a02906a0fe497d417effe01adb66e0f`

```mermaid
classDiagram
    class struct_VerifyOutput {
      <<struct>>
      +"receipt_file: String"
      +"is_valid: bool"
      +"message: String"
      +"operation_id: Option~String~"
      +"timestamp: Option~String~"
      +"input_hashes: Option~usize~"
      +"output_hashes: Option~usize~"
      +"chain_position: Option~String~"
    }
    class struct_InfoOutput {
      <<struct>>
      +"receipt_file: String"
      +"operation_id: String"
      +"timestamp: String"
      +"input_hashes: usize"
      +"output_hashes: usize"
      +"has_previous: bool"
      +"signature_present: bool"
    }
    class fn_load_verifying_key {
      <<fn>>
    }
    class fn_load_receipt {
      <<fn>>
    }
    class fn_resolve_key_path {
      <<fn>>
    }
    class fn_do_verify {
      <<fn>>
    }
    class fn_do_info {
      <<fn>>
    }
    class fn_verify {
      <<fn>>
    }
    class fn_info {
      <<fn>>
    }
```

## Dependencies

- `clap_noun_verb::{NounVerbError, Result}`
- `clap_noun_verb_macros::verb`
- `ed25519_dalek::VerifyingKey`
- `ggen_config::receipt::Receipt`
- `serde::Serialize`
- `std::{fs, path::PathBuf}`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
