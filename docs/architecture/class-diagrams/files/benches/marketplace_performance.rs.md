# `benches/marketplace_performance.rs`

Source SHA-256: `6f5d864447ff851ee67a986b1b1a45340207e7d1e6224ec7eaa64f56db61eef7`

```mermaid
classDiagram
    class struct_TestPackage {
      <<struct>>
      +"name: String"
      +"version: String"
      +"description: String"
      +"author: String"
      +"tags: Vec~String~"
      +"dependencies: HashMap~String"
      +"size_bytes: u64"
    }
    class struct_TestRegistry {
      <<struct>>
      +"packages: Vec~TestPackage~"
      +"index_path: PathBuf"
      +"cache_dir: PathBuf"
    }
    class fn_setup_test_registry {
      <<fn>>
    }
    class fn_setup_deep_dependency_tree {
      <<fn>>
    }
    class fn_bench_registry_loading {
      <<fn>>
    }
    class fn_bench_search_performance {
      <<fn>>
    }
    class fn_bench_installation_performance {
      <<fn>>
    }
    class fn_bench_dependency_resolution {
      <<fn>>
    }
    class fn_bench_cache_performance {
      <<fn>>
    }
    class fn_bench_concurrent_operations {
      <<fn>>
    }
```

## Dependencies

- `criterion::{criterion_group, criterion_main, BenchmarkId, Criterion, Throughput}`
- `serde::{Deserialize, Serialize}`
- `std::collections::HashMap`
- `std::hint::black_box`
- `std::path::PathBuf`
- `std::sync::Arc`
- `tempfile::TempDir`
- `tokio::runtime::Runtime`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
