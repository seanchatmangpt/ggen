# `crates/ggen-marketplace/src/marketplace/cache.rs`

Source SHA-256: `5efcbc0f1eda9329ebca3c0036e6bfc6de33dbdb7cf73b3ed93a829cd4d91490`

```mermaid
classDiagram
    class struct_CachedPack {
      <<struct>>
      +"package_id: PackageId"
      +"version: PackageVersion"
      +"digest: String"
      +"size_bytes: u64"
      +"downloaded_at: DateTime~Utc~"
      +"last_accessed: DateTime~Utc~"
      +"cache_path: PathBuf"
      +"access_count: u64"
    }
    class struct_CacheConfig {
      <<struct>>
      +"max_size_bytes: u64"
      +"max_packs: usize"
      +"cache_dir: PathBuf"
      +"persistent: bool"
    }
    class struct_PackCache {
      <<struct>>
      +"config: CacheConfig"
      +"packs: Arc~RwLock~HashMap~String"
      +"current_size: Arc~RwLock~u64~~"
    }
    class struct_CacheStats {
      <<struct>>
      +"total_packs: usize"
      +"total_size_bytes: u64"
      +"max_size_bytes: u64"
      +"max_packs: usize"
      +"utilization_percent: f64"
    }
    class mod_tests {
      <<mod>>
    }
    note "CachedPack"
    note "Default for CacheConfig"
    note "PackCache"
    note "std::fmt::Display for CacheStats"
```

## Dependencies

- `chrono::{DateTime, Utc}`
- `crate::marketplace::error::{Error, Result}`
- `crate::marketplace::models::{PackageId, PackageVersion}`
- `serde::{Deserialize, Serialize}`
- `sha2::{Digest, Sha256}`
- `std::collections::HashMap`
- `std::fs`
- `std::io::{BufReader, BufWriter}`
- `std::path::PathBuf`
- `std::sync::{Arc, RwLock}`
- `super::*`
- `tempfile::TempDir`
- `tracing::{debug, info, warn}`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
