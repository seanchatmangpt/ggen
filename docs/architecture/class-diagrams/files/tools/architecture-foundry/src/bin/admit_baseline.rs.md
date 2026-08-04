# `tools/architecture-foundry/src/bin/admit_baseline.rs`

Source SHA-256: `ef14ab17dff68779bb2a8e336c9771c58d06b7aa8b5c45731c8e935b0ac1bb17`

```mermaid
classDiagram
    class struct_Cli {
      <<struct>>
      +"program: PathBuf"
      +"source: PathBuf"
      +"corpus: PathBuf"
    }
    class struct_BaselineAdmissionReport {
      <<struct>>
      +"schema_version: String"
      +"workstream_id: String"
      +"verifier: String"
      +"source_head: String"
      +"corpus_head: String"
      +"source_tree_digest: String"
      +"corpus_tree_digest: String"
      +"program_digest: String"
      +"foundry_manifest_digest: String"
      +"initialization_receipt_digest: String"
      +"initialization_parent_is_ancestor: bool"
      +"receipts_replayed: usize"
      +"predicates: BTreeMap~String"
    }
    class fn_main {
      <<fn>>
    }
    class fn_git_is_ancestor {
      <<fn>>
    }
    class fn_canonical_json {
      <<fn>>
    }
    class fn_digest_bytes {
      <<fn>>
    }
    class fn_digest_named_outputs {
      <<fn>>
    }
    class fn_hash_named_bytes {
      <<fn>>
    }
    class fn_write_new {
      <<fn>>
    }
    class fn_write_replace {
      <<fn>>
    }
```

## Dependencies

- `anyhow::{bail, Context, Result}`
- `blake3::Hasher`
- `clap::Parser`
- `ggen_architecture_foundry::{ digest_file, load_program, replay_all_receipts, snapshot_repository, validate_program, FoundryManifest, Receipt, WorkstreamStateFile, CORPUS_SCHEMA, RECEIPT_SCHEMA, }`
- `serde::Serialize`
- `serde_yaml::Value as YamlValue`
- `std::collections::BTreeMap`
- `std::fs`
- `std::path::{Path, PathBuf}`
- `std::process::Command`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
