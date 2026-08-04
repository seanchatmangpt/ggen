# `crates/ggen-marketplace/tests/marketplace_phase4_integration.rs`

Source SHA-256: `ab91ba74f52c4522ce8e233ecf1ce0ee298b7780e5810739897e2a1375de840c`

```mermaid
classDiagram
    class fn_test_marketplace_client_creation {
      <<fn>>
    }
    class fn_test_marketplace_client_with_timeout {
      <<fn>>
    }
    class fn_test_marketplace_client_with_cache {
      <<fn>>
    }
    class fn_test_package_id_parsing {
      <<fn>>
    }
    class fn_test_package_version_parsing {
      <<fn>>
    }
    class fn_test_cache_operations {
      <<fn>>
    }
```

## Dependencies

- `ggen_marketplace::marketplace::cache::{CacheConfig, PackCache}`
- `ggen_marketplace::marketplace::network::MarketplaceClient`
- `ggen_marketplace::marketplace::{PackageId, PackageVersion}`
- `std::sync::Arc`
- `std::time::Duration`
- `tempfile::TempDir`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
