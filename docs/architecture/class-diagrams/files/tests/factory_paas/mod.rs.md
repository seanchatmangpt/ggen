# `tests/factory_paas/mod.rs`

Source SHA-256: `c507528271b764b5286855eefbec8b4e022c09686053e0fd6c33da8c5751ee8d`

```mermaid
classDiagram
    class mod_integration_tests {
      <<mod>>
    }
    class mod_load_tests {
      <<mod>>
    }
    class mod_property_tests {
      <<mod>>
    }
    class struct_TestDatabase {
      <<struct>>
      +"_container: testcontainers::Container~'static"
      +"connection_string: String"
    }
    class struct_TestContext {
      <<struct>>
      +"route_resolver: Arc~RwLock~affiliate::RouteResolver~~"
      +"click_tracker: Arc~RwLock~click_tracking::ClickTracker~~"
      +"content_pipeline: Arc~RwLock~content::ContentPipeline~~"
      +"revenue_attribution: Arc~RwLock~revenue::RevenueAttribution~~"
      +"subscription_manager: Arc~RwLock~subscription::SubscriptionManager~~"
    }
    note "Default for TestContext"
    note "TestContext"
    note "TestDatabase"
```

## Dependencies

- `ggen_saas::factory_paas::*`
- `std::sync::Arc`
- `testcontainers::{clients::Cli, RunnableImage}`
- `testcontainers_modules::postgres::Postgres`
- `tokio::sync::RwLock`
- `uuid::Uuid`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
