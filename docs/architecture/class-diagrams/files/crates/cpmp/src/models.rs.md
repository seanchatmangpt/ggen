# `crates/cpmp/src/models.rs`

Source SHA-256: `e653463ae7085b02c21df58ae267398820666bcb946a1786690f0ea37b5b16d1`

```mermaid
classDiagram
    class enum_Language {
      <<enum>>
    }
    class struct_FileEntry {
      <<struct>>
      +"path: String"
      +"language: Language"
      +"size_bytes: u64"
      +"hash: String"
      +"modified_time: SystemTime"
      +"git_root: Option~String~"
      +"is_test: bool"
      +"is_binary: bool"
    }
    class struct_Symbol {
      <<struct>>
      +"file_path: String"
      +"name: String"
      +"kind: String"
      +"line: usize"
    }
    class struct_DetectedCapability {
      <<struct>>
      +"file_path: String"
      +"capability: String"
      +"matched_term: String"
      +"name: String"
      +"confidence: f64"
      +"evidence_type: String"
      +"classification: String"
      +"line_number: i64"
    }
    class struct_Receipt {
      <<struct>>
      +"id: String"
      +"timestamp: String"
      +"scan_roots: Vec~String~"
      +"file_count: usize"
      +"total_bytes: u64"
      +"aggregate_hash: String"
      +"commands_run: Vec~String~"
      +"output_artifacts: Vec~String~"
      +"files: Vec~FileEntry~"
    }
    class struct_VerificationReport {
      <<struct>>
      +"unchanged_files: usize"
      +"added_files: Vec~String~"
      +"modified_files: Vec~String~"
      +"missing_files: Vec~String~"
    }
    note "Language"
```

## Dependencies

- `serde::{Deserialize, Serialize}`
- `std::time::SystemTime`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
