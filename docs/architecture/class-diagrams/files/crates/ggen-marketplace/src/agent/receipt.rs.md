# `crates/ggen-marketplace/src/agent/receipt.rs`

Source SHA-256: `f6b1334e97c0b21bf3c5da6778008a5ef4c4ac0c4d015592aefc733cc1401eb5`

```mermaid
classDiagram
    class enum_PackReceiptError {
      <<enum>>
    }
    class type_Result {
      <<type>>
    }
    class struct_PackInstallClosure {
      <<struct>>
      +"pack_id: &'a str"
      +"pack_version: &'a str"
      +"pack_digest: &'a str"
      +"packages_installed: &'a [String]"
      +"artifact_paths: &'a [PathBuf]"
    }
    class fn_emit_install_receipt {
      <<fn>>
    }
    class fn_read_artifact_bytes {
      <<fn>>
    }
    class fn_load_keypair {
      <<fn>>
    }
    class fn_save_keypair {
      <<fn>>
    }
    class fn_verify_install_receipt {
      <<fn>>
    }
    class fn_load_verifying_key {
      <<fn>>
    }
    note "std::error::Error for PackReceiptError"
    note "std::fmt::Display for PackReceiptError"
```

## Dependencies

- `ed25519_dalek::SECRET_KEY_LENGTH`
- `ed25519_dalek::{SecretKey, SigningKey, VerifyingKey}`
- `ggen_config::receipt::Receipt`
- `ggen_config::receipt::{hash_data, Receipt}`
- `std::fs`
- `std::path::{Path, PathBuf}`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
