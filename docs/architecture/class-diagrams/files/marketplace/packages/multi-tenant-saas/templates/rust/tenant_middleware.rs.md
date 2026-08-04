# `marketplace/packages/multi-tenant-saas/templates/rust/tenant_middleware.rs`

Source SHA-256: `ef4f93623dd3539230d4dba04a4453ce5749f44868b194bdc899bf2252ee5bdd`

```mermaid
classDiagram
    class struct_TenantContext {
      <<struct>>
      +"tenant_id: String"
      +"tenant_slug: String"
      +"isolation_strategy: IsolationStrategy"
      +"subscription_tier: SubscriptionTier"
      +"feature_flags: Vec~String~"
    }
    class enum_IsolationStrategy {
      <<enum>>
    }
    class enum_SubscriptionTier {
      <<enum>>
    }
    class struct_TenantResolver {
      <<struct>>
      +"tenant_cache: Arc~RwLock~std::collections::HashMap~String"
    }
    class mod_tests {
      <<mod>>
    }
    note "TenantResolver"
```

## Dependencies

- `axum::{ extract::{Extension, Request}, http::{HeaderMap, StatusCode}, middleware::Next, response::Response, }`
- `std::sync::Arc`
- `super::*`
- `tokio::sync::RwLock`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
