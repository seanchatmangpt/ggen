# `crates/ggen-marketplace/src/marketplace/registry.rs`

Source SHA-256: `c40b93d99ca393b11a0ff46720f2e3684eec19790a3f701d13c905c6ac03a09a`

```mermaid
classDiagram
    class struct_Registry {
      <<struct>>
      +"packages: Arc~DashMap~PackageId"
      +"version_index: Arc~DashMap~String"
      +"query_cache: Arc~AsyncCache~String"
      +"cache_hits: Arc~std::sync::atomic::AtomicU64~"
      +"cache_misses: Arc~std::sync::atomic::AtomicU64~"
    }
    class struct_CacheStats {
      <<struct>>
      +"hits: u64"
      +"misses: u64"
      +"hit_rate: f64"
    }
    class mod_tests {
      <<mod>>
    }
    note "AsyncRepository for Registry"
    note "Registry"
    note "std::fmt::Display for CacheStats"
```

## Dependencies

- `async_trait::async_trait`
- `crate::marketplace::error::Result`
- `crate::marketplace::models::{Package, PackageId, PackageVersion}`
- `crate::marketplace::models::{PackageId, PackageMetadata}`
- `crate::marketplace::traits::AsyncRepository`
- `dashmap::DashMap`
- `moka::future::Cache as AsyncCache`
- `std::sync::Arc`
- `super::*`
- `tracing::{debug, info, warn}`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
