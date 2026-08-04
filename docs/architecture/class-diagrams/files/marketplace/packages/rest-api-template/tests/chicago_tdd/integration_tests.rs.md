# `marketplace/packages/rest-api-template/tests/chicago_tdd/integration_tests.rs`

Source SHA-256: `36c0b92e87e9bf4554c0719fd22acbbc3cf55ff15d003c3ed18cb6df14fac979`

```mermaid
classDiagram
    class struct_AppState {
      <<struct>>
      +"db: std::sync::Arc~DatabasePool~"
    }
    class struct_DatabasePool {
      <<struct>>
      +"users: std::sync::Arc~tokio::sync::RwLock~Vec~User~~~"
    }
    class struct_User {
      <<struct>>
      +"id: Option~i64~"
      +"username: String"
      +"email: String"
      +"created_at: Option~String~"
    }
    class fn_create_test_router {
      <<fn>>
    }
    note "DatabasePool"
```

## Dependencies

- `axum::{ body::Body, http::{Request, StatusCode}, }`
- `axum::{ extract::{Path, State}, response::Json, routing::{delete, get, post}, }`
- `serde_json::{json, Value}`
- `testcontainers::{clients::Cli, images::postgres::Postgres, Container}`
- `tower::ServiceExt`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
