# `crates/ggen-marketplace/src/marketplace/network.rs`

Source SHA-256: `23946c76987efdb8820b0eb9085b3823a0aa6440080c388a6c7ad87d20d0a585`

```mermaid
classDiagram
    class type_DownloadProgressCallback {
      <<type>>
    }
    class struct_PackageMetadata {
      <<struct>>
      +"id: PackageId"
      +"version: PackageVersion"
      +"description: String"
      +"author: String"
      +"license: String"
      +"download_url: String"
      +"digest: String"
      +"size_bytes: u64"
      +"dependencies: Vec~String~"
      +"published_at: String"
    }
    class struct_MarketplaceClient {
      <<struct>>
      +"http_client: Client"
      +"registry_url: String"
      +"request_timeout: Duration"
      +"cache: Option~Arc~PackCache~~"
    }
    class fn_sha256 {
      <<fn>>
    }
    class mod_tests {
      <<mod>>
    }
    note "MarketplaceClient"
```

## Dependencies

- `crate::marketplace::cache::PackCache`
- `crate::marketplace::error::{Error, Result}`
- `crate::marketplace::models::{PackageId, PackageVersion}`
- `reqwest::Client`
- `serde::{Deserialize, Serialize}`
- `sha2::{Digest, Sha256}`
- `std::sync::Arc`
- `std::time::Duration`
- `super::*`
- `tracing::{debug, info, instrument, span, warn}`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
