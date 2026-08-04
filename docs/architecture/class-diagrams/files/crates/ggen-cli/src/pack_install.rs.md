# `crates/ggen-cli/src/pack_install.rs`

Source SHA-256: `074f9ee59a6f372426f2fec886eb2a26323ec746dba711632bd06a69bc1377f7`

```mermaid
classDiagram
    class struct_PackInstaller {
      <<struct>>
      +"repository: Box~dyn AsyncRepository~PackageIterator = std::vec::IntoIter~Package~~~"
      +"cache: PackCache"
      +"progress: ProgressReporter"
      +"max_concurrent_downloads: usize"
    }
    class struct_TestRepository {
      <<struct>>
    }
    class enum_InstallResult {
      <<enum>>
    }
    class struct_InstallationResult {
      <<struct>>
      +"pack_id: String"
      +"installed_packages: Vec~PackageId~"
      +"cache_status: CacheStatus"
      +"total_size_mb: f64"
      +"duration_ms: u64"
    }
    class mod_tests {
      <<mod>>
    }
    note "AsyncRepository for TestRepository"
    note "From~InstallResult~ for crate::cmds::pack::InstallOutput"
    note "PackInstaller"
```

## Dependencies

- `async_trait::async_trait`
- `crate::error::GgenError`
- `crate::progress::{CacheStatus, InstallationPlan, PlanStep, ProgressReporter}`
- `futures::future::join_all`
- `ggen_marketplace::marketplace::{ cache::{CacheConfig, CachedPack, PackCache}, error::Error as MarketplaceError, AsyncRepository, Package, PackageId, PackageVersion, }`
- `std::sync::Arc`
- `super::*`
- `tokio::sync::Semaphore`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
