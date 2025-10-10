# Recipe: REST API Endpoint Generator with Fan-Out

**Time:** 20 minutes
**Difficulty:** Intermediate
**Patterns:** Fan-out generation, freeze blocks, multi-file output, REST API scaffolding

## What You'll Build

A REST API endpoint generator that creates multiple files per endpoint:
- Handler function (with freeze blocks for business logic)
- Route registration
- Request/response types
- OpenAPI/Swagger documentation
- Integration tests

All from a single graph definition, using fan-out to generate multiple related files.

## Prerequisites

- Completed [CLI Command Scaffold](./cli_command_scaffold.md)
- Familiarity with REST APIs and Rust/Axum (or your preferred framework)

## The Problem

Creating a new API endpoint requires:
- Handler boilerplate (parsing, validation, error handling)
- Type definitions (request/response structs)
- Route registration
- API documentation
- Tests

Doing this manually for every endpoint is tedious and error-prone. Let's automate it.

## The Recipe

### Step 1: Define API Endpoints

Create `data/api_endpoints.ttl`:

```turtle
@prefix api: <http://example.org/api#> .
@prefix http: <http://www.w3.org/2011/http#> .

api:create_user
  a api:Endpoint ;
  api:path "/users" ;
  api:method "POST" ;
  api:handler_name "create_user" ;
  api:description "Create a new user account" ;
  api:request_type "CreateUserRequest" ;
  api:response_type "UserResponse" ;
  api:status_code "201" ;
  api:has_field [
    api:field_name "email" ;
    api:field_type "String" ;
    api:field_required true ;
    api:field_validation "email_format"
  ] ;
  api:has_field [
    api:field_name "username" ;
    api:field_type "String" ;
    api:field_required true ;
    api:field_validation "min_length:3"
  ] ;
  api:has_field [
    api:field_name "password" ;
    api:field_type "String" ;
    api:field_required true ;
    api:field_validation "min_length:8"
  ] .

api:get_user
  a api:Endpoint ;
  api:path "/users/:id" ;
  api:method "GET" ;
  api:handler_name "get_user" ;
  api:description "Get user by ID" ;
  api:response_type "UserResponse" ;
  api:status_code "200" ;
  api:has_path_param [
    api:param_name "id" ;
    api:param_type "i64"
  ] .

api:list_users
  a api:Endpoint ;
  api:path "/users" ;
  api:method "GET" ;
  api:handler_name "list_users" ;
  api:description "List all users with pagination" ;
  api:response_type "Vec<UserResponse>" ;
  api:status_code "200" ;
  api:has_query_param [
    api:param_name "page" ;
    api:param_type "u32" ;
    api:param_default "1"
  ] ;
  api:has_query_param [
    api:param_name "limit" ;
    api:param_type "u32" ;
    api:param_default "20"
  ] .

api:update_user
  a api:Endpoint ;
  api:path "/users/:id" ;
  api:method "PUT" ;
  api:handler_name "update_user" ;
  api:description "Update user information" ;
  api:request_type "UpdateUserRequest" ;
  api:response_type "UserResponse" ;
  api:status_code "200" ;
  api:has_path_param [
    api:param_name "id" ;
    api:param_type "i64"
  ] ;
  api:has_field [
    api:field_name "email" ;
    api:field_type "Option<String>" ;
    api:field_required false
  ] ;
  api:has_field [
    api:field_name "username" ;
    api:field_type "Option<String>" ;
    api:field_required false
  ] .

api:delete_user
  a api:Endpoint ;
  api:path "/users/:id" ;
  api:method "DELETE" ;
  api:handler_name "delete_user" ;
  api:description "Delete a user account" ;
  api:status_code "204" ;
  api:has_path_param [
    api:param_name "id" ;
    api:param_type "i64"
  ] .
```

### Step 2: Create Handler Template

Create `templates/api_handler.tmpl`:

```handlebars
{{#each endpoints}}
//! {{description}}
//!
//! Generated: {{timestamp}}
//! Method: {{method}} {{path}}

use axum::{
    extract::{Path, Query, State},
    http::StatusCode,
    response::{IntoResponse, Json},
};
use serde::{Deserialize, Serialize};

{{#if has_field}}
/// Request payload for {{method}} {{path}}
#[derive(Debug, Deserialize, Validate)]
pub struct {{request_type}} {
{{#each fields}}
    {{#if field_validation}}
    #[validate({{field_validation}})]
    {{/if}}
    pub {{field_name}}: {{field_type}},
{{/each}}
}
{{/if}}

{{#if has_query_param}}
/// Query parameters for {{method}} {{path}}
#[derive(Debug, Deserialize)]
pub struct {{handler_name}}Query {
{{#each query_params}}
    #[serde(default{{#if param_default}} = "{{param_default}}"{{/if}})]
    pub {{param_name}}: {{param_type}},
{{/each}}
}
{{/if}}

{{#if has_path_param}}
/// Path parameters for {{method}} {{path}}
#[derive(Debug, Deserialize)]
pub struct {{handler_name}}Params {
{{#each path_params}}
    pub {{param_name}}: {{param_type}},
{{/each}}
}
{{/if}}

// <<<FREEZE_START:{{handler_name}}_handler>>>
/// Handler: {{description}}
pub async fn {{handler_name}}(
    State(app_state): State<AppState>,
{{#if has_path_param}}
    Path(params): Path<{{handler_name}}Params>,
{{/if}}
{{#if has_query_param}}
    Query(query): Query<{{handler_name}}Query>,
{{/if}}
{{#if request_type}}
    Json(payload): Json<{{request_type}}>,
{{/if}}
) -> Result<impl IntoResponse, ApiError> {
    // TODO: Implement {{handler_name}} logic

{{#if has_field}}
    // Validate request
    payload.validate()
        .map_err(|e| ApiError::ValidationError(e.to_string()))?;

{{/if}}
    // TODO: Add your business logic here
    // Example:
    // let user = app_state.db.create_user(payload).await?;

    Ok((
        StatusCode::{{uppercase status_code}},
        Json(serde_json::json!({
            "message": "{{description}}",
{{#if has_path_param}}
{{#each path_params}}
            "{{param_name}}": params.{{param_name}},
{{/each}}
{{/if}}
        }))
    ))
}
// <<<FREEZE_END:{{handler_name}}_handler>>>

{{/each}}
```

### Step 3: Create Routes Template

Create `templates/api_routes.tmpl`:

```handlebars
//! API Route Registration
//! Generated: {{timestamp}}
//!
//! This file is fully regenerated - do not edit manually

use axum::{routing::{get, post, put, delete}, Router};
use crate::handlers::*;

pub fn routes(app_state: AppState) -> Router {
    Router::new()
{{#each endpoints}}
        .route("{{path}}", {{lowercase method}}({{handler_name}}))
{{/each}}
        .with_state(app_state)
}

// Route summary for documentation
pub const ROUTES: &[(&str, &str, &str)] = &[
{{#each endpoints}}
    ("{{method}}", "{{path}}", "{{description}}"),
{{/each}}
];
```

### Step 4: Create OpenAPI Documentation Template

Create `templates/api_openapi.tmpl`:

```handlebars
{
  "openapi": "3.0.0",
  "info": {
    "title": "Generated API",
    "version": "1.0.0",
    "description": "Auto-generated REST API documentation"
  },
  "paths": {
{{#each endpoints}}
    "{{path}}": {
      "{{lowercase method}}": {
        "summary": "{{description}}",
        "operationId": "{{handler_name}}",
{{#if request_type}}
        "requestBody": {
          "required": true,
          "content": {
            "application/json": {
              "schema": {
                "$ref": "#/components/schemas/{{request_type}}"
              }
            }
          }
        },
{{/if}}
        "responses": {
          "{{status_code}}": {
            "description": "Successful response"{{#if response_type}},
            "content": {
              "application/json": {
                "schema": {
                  "$ref": "#/components/schemas/{{response_type}}"
                }
              }
            }
{{/if}}
          }
        }
      }
    }{{#unless @last}},{{/unless}}
{{/each}}
  },
  "components": {
    "schemas": {
{{#each endpoints}}
{{#if request_type}}
      "{{request_type}}": {
        "type": "object",
        "required": [
{{#each fields}}
{{#if field_required}}
          "{{field_name}}"{{#unless @last}},{{/unless}}
{{/if}}
{{/each}}
        ],
        "properties": {
{{#each fields}}
          "{{field_name}}": {
            "type": "{{json_type field_type}}"
          }{{#unless @last}},{{/unless}}
{{/each}}
        }
      }{{#unless @last}},{{/unless}}
{{/if}}
{{/each}}
    }
  }
}
```

### Step 5: Create Test Template

Create `templates/api_tests.tmpl`:

```handlebars
{{#each endpoints}}
#[cfg(test)]
mod {{handler_name}}_tests {
    use super::*;
    use axum::http::StatusCode;
    use serde_json::json;

    // <<<FREEZE_START:{{handler_name}}_test_setup>>>
    async fn setup_test_app() -> Router {
        // TODO: Setup test database and app state
        let app_state = AppState::new_test();
        routes(app_state)
    }
    // <<<FREEZE_END:{{handler_name}}_test_setup>>>

    #[tokio::test]
    async fn test_{{handler_name}}_success() {
        let app = setup_test_app().await;

{{#if request_type}}
        let payload = json!({
{{#each fields}}
            "{{field_name}}": {{test_value field_type}},
{{/each}}
        });

        let response = app
            .oneshot(
                Request::builder()
                    .method("{{method}}")
                    .uri("{{path}}")
                    .header("content-type", "application/json")
                    .body(Body::from(payload.to_string()))
                    .unwrap()
            )
            .await
            .unwrap();
{{else}}
        let response = app
            .oneshot(
                Request::builder()
                    .method("{{method}}")
                    .uri("{{path}}")
                    .body(Body::empty())
                    .unwrap()
            )
            .await
            .unwrap();
{{/if}}

        assert_eq!(response.status(), StatusCode::{{uppercase status_code}});
    }

    // <<<FREEZE_START:{{handler_name}}_custom_tests>>>
    // TODO: Add custom test cases here
    // <<<FREEZE_END:{{handler_name}}_custom_tests>>>
}

{{/each}}
```

### Step 6: Generate All Files (Fan-Out)

Create a generation script `scripts/generate_api.sh`:

```bash
#!/bin/bash
set -e

DATA="data/api_endpoints.ttl"

echo "🚀 Generating API handlers..."
ggen exec \
  --template templates/api_handler.tmpl \
  --data "$DATA" \
  --output src/handlers.rs \
  --freeze

echo "🛣️  Generating routes..."
ggen exec \
  --template templates/api_routes.tmpl \
  --data "$DATA" \
  --output src/routes.rs

echo "📖 Generating OpenAPI spec..."
ggen exec \
  --template templates/api_openapi.tmpl \
  --data "$DATA" \
  --output openapi.json

echo "🧪 Generating tests..."
ggen exec \
  --template templates/api_tests.tmpl \
  --data "$DATA" \
  --output tests/api_tests.rs \
  --freeze

echo "✅ API generation complete!"
echo ""
echo "Generated files:"
echo "  - src/handlers.rs (with freeze blocks)"
echo "  - src/routes.rs (fully regenerated)"
echo "  - openapi.json (fully regenerated)"
echo "  - tests/api_tests.rs (with freeze blocks)"
```

Make it executable and run:

```bash
chmod +x scripts/generate_api.sh
./scripts/generate_api.sh
```

### Step 7: Implement Custom Handler Logic

Edit `src/handlers.rs` and add your business logic:

```rust
// Find the create_user_handler freeze block
// <<<FREEZE_START:create_user_handler>>>
pub async fn create_user(
    State(app_state): State<AppState>,
    Json(payload): Json<CreateUserRequest>,
) -> Result<impl IntoResponse, ApiError> {
    // Validate request
    payload.validate()
        .map_err(|e| ApiError::ValidationError(e.to_string()))?;

    // Check if email already exists
    if app_state.db.user_exists(&payload.email).await? {
        return Err(ApiError::Conflict("Email already registered".into()));
    }

    // Hash password
    let password_hash = bcrypt::hash(&payload.password, 12)
        .map_err(|e| ApiError::Internal(e.to_string()))?;

    // Create user in database
    let user = app_state.db.create_user(
        &payload.email,
        &payload.username,
        &password_hash,
    ).await?;

    // Generate JWT token
    let token = generate_token(&user)?;

    Ok((
        StatusCode::CREATED,
        Json(UserResponse {
            id: user.id,
            email: user.email,
            username: user.username,
            token,
            created_at: user.created_at,
        })
    ))
}
// <<<FREEZE_END:create_user_handler>>>
```

### Step 8: Add a New Endpoint and Regenerate

Add to `data/api_endpoints.ttl`:

```turtle
api:search_users
  a api:Endpoint ;
  api:path "/users/search" ;
  api:method "GET" ;
  api:handler_name "search_users" ;
  api:description "Search users by query" ;
  api:response_type "Vec<UserResponse>" ;
  api:status_code "200" ;
  api:has_query_param [
    api:param_name "q" ;
    api:param_type "String" ;
    api:param_description "Search query"
  ] ;
  api:has_query_param [
    api:param_name "limit" ;
    api:param_type "u32" ;
    api:param_default "20"
  ] .
```

Regenerate:

```bash
./scripts/generate_api.sh
```

**Result:**
- New handler added to `src/handlers.rs` with freeze blocks
- New route added to `src/routes.rs`
- New endpoint in `openapi.json`
- New test scaffolding in `tests/api_tests.rs`
- Existing custom implementations PRESERVED

## What's Happening?

### Fan-Out Pattern

One graph → Multiple templates → Multiple output files:

```
api_endpoints.ttl
    ├── api_handler.tmpl    → src/handlers.rs
    ├── api_routes.tmpl     → src/routes.rs
    ├── api_openapi.tmpl    → openapi.json
    └── api_tests.tmpl      → tests/api_tests.rs
```

Each template focuses on one concern, all share the same source of truth.

### Freeze Block Strategy

- **Handlers**: Business logic is frozen (custom)
- **Routes**: Fully regenerated (boilerplate)
- **OpenAPI**: Fully regenerated (documentation)
- **Tests**: Test setup frozen, test cases can be added

### Template Helpers Used

- `{{lowercase method}}`: "POST" → "post" for Axum routing
- `{{uppercase status_code}}`: "201" → "CREATED" for StatusCode enum
- `{{test_value field_type}}`: Generates sample test data based on type
- `{{json_type field_type}}`: Rust type → JSON schema type

## Complete Project Structure

```
my-api/
├── data/
│   └── api_endpoints.ttl
├── templates/
│   ├── api_handler.tmpl
│   ├── api_routes.tmpl
│   ├── api_openapi.tmpl
│   └── api_tests.tmpl
├── scripts/
│   └── generate_api.sh
├── src/
│   ├── main.rs
│   ├── handlers.rs (generated with freeze)
│   ├── routes.rs (generated)
│   └── error.rs (manually written)
├── tests/
│   └── api_tests.rs (generated with freeze)
├── openapi.json (generated)
└── Cargo.toml
```

## Testing the Generated API

### Step 1: Create AppState and Error Types

Create `src/error.rs`:

```rust
use axum::http::StatusCode;
use axum::response::{IntoResponse, Response, Json};

#[derive(Debug)]
pub enum ApiError {
    NotFound(String),
    Conflict(String),
    ValidationError(String),
    Internal(String),
}

impl IntoResponse for ApiError {
    fn into_response(self) -> Response {
        let (status, message) = match self {
            ApiError::NotFound(msg) => (StatusCode::NOT_FOUND, msg),
            ApiError::Conflict(msg) => (StatusCode::CONFLICT, msg),
            ApiError::ValidationError(msg) => (StatusCode::BAD_REQUEST, msg),
            ApiError::Internal(msg) => (StatusCode::INTERNAL_SERVER_ERROR, msg),
        };

        (status, Json(serde_json::json!({ "error": message }))).into_response()
    }
}
```

### Step 2: Run the Server

```rust
// src/main.rs
mod handlers;
mod routes;
mod error;

use axum::Router;

#[derive(Clone)]
pub struct AppState {
    db: DatabasePool,
}

#[tokio::main]
async fn main() {
    let app_state = AppState::new().await;
    let app = routes::routes(app_state);

    let listener = tokio::net::TcpListener::bind("127.0.0.1:3000")
        .await
        .unwrap();

    println!("🚀 Server listening on http://127.0.0.1:3000");
    println!("📖 OpenAPI docs: http://127.0.0.1:3000/openapi.json");

    axum::serve(listener, app).await.unwrap();
}
```

### Step 3: Test with curl

```bash
# Create user
curl -X POST http://localhost:3000/users \
  -H "Content-Type: application/json" \
  -d '{
    "email": "alice@example.com",
    "username": "alice",
    "password": "secret123"
  }'

# Get user
curl http://localhost:3000/users/1

# List users
curl "http://localhost:3000/users?page=1&limit=10"

# Search users
curl "http://localhost:3000/users/search?q=alice"

# Update user
curl -X PUT http://localhost:3000/users/1 \
  -H "Content-Type: application/json" \
  -d '{
    "username": "alice_updated"
  }'

# Delete user
curl -X DELETE http://localhost:3000/users/1
```

## Advanced Patterns

### 1. Middleware Generation

Add to graph:

```turtle
api:create_user
  api:requires_auth true ;
  api:rate_limit "10/minute" ;
  api:cors_enabled true .
```

Template:

```handlebars
{{#if requires_auth}}
    .layer(AuthMiddleware)
{{/if}}
{{#if rate_limit}}
    .layer(RateLimitMiddleware::new("{{rate_limit}}"))
{{/if}}
```

### 2. Database Query Generation

Create `templates/api_queries.tmpl`:

```handlebars
{{#each endpoints}}
{{#if db_query}}
// <<<FREEZE_START:{{handler_name}}_query>>>
pub async fn {{handler_name}}_query(
    pool: &PgPool,
{{#each query_params}}
    {{param_name}}: {{param_type}},
{{/each}}
) -> Result<{{response_type}}, DbError> {
    sqlx::query_as!(
        {{response_type}},
        r#"
        {{db_query}}
        "#,
{{#each query_params}}
        {{param_name}},
{{/each}}
    )
    .fetch_one(pool)
    .await
}
// <<<FREEZE_END:{{handler_name}}_query>>>
{{/if}}
{{/each}}
```

### 3. Versioned APIs

```turtle
api:create_user_v1
  api:version "v1" ;
  api:path "/v1/users" ;
  api:deprecated true ;
  api:successor api:create_user_v2 .

api:create_user_v2
  api:version "v2" ;
  api:path "/v2/users" ;
  api:breaking_changes "Added required field: phone_number" .
```

## Troubleshooting

### Missing Fields in Generated Code

**Problem:** Template expects fields but they don't appear

**Solution:** Check graph structure:
```bash
# Query your graph to see what's actually there
ggen query --data data/api_endpoints.ttl \
  --query "SELECT ?s ?p ?o WHERE { ?s ?p ?o } LIMIT 100"
```

### Freeze Blocks Overwritten

**Problem:** Custom code lost on regeneration

**Solution:** Verify freeze block markers:
```bash
# Check freeze blocks are properly formatted
grep -n "FREEZE_" src/handlers.rs
```

### Routes Not Matching

**Problem:** 404 errors for valid endpoints

**Solution:** Check generated routes:
```rust
// src/routes.rs should have exact path matches
.route("/users/:id", get(get_user))  // ✅ Correct
.route("/users/{id}", get(get_user)) // ❌ Wrong - Axum uses :id
```

### Type Mismatches

**Problem:** Compilation errors in generated types

**Solution:** Ensure Rust types in graph are valid:
```turtle
api:field_type "String"        # ✅ Valid
api:field_type "str"           # ❌ Invalid - needs String for owned
api:field_type "Option<i64>"   # ✅ Valid for optional fields
```

## Performance Optimization

### Parallel Generation

Modify `scripts/generate_api.sh`:

```bash
#!/bin/bash
set -e

DATA="data/api_endpoints.ttl"

# Run all generations in parallel
(ggen exec --template templates/api_handler.tmpl --data "$DATA" --output src/handlers.rs --freeze) &
(ggen exec --template templates/api_routes.tmpl --data "$DATA" --output src/routes.rs) &
(ggen exec --template templates/api_openapi.tmpl --data "$DATA" --output openapi.json) &
(ggen exec --template templates/api_tests.tmpl --data "$DATA" --output tests/api_tests.rs --freeze) &

# Wait for all to complete
wait

echo "✅ All files generated in parallel!"
```

### Incremental Generation

Only regenerate changed endpoints:

```bash
# Add checksum tracking
ggen exec --template templates/api_handler.tmpl \
  --data data/api_endpoints.ttl \
  --output src/handlers.rs \
  --freeze \
  --incremental \
  --checksum .ggen/checksums.json
```

## Next Steps

- **Authentication**: Add JWT middleware generation ([Module Wiring](./idempotent_module_wiring.md))
- **Database**: Generate database migrations from the same graph
- **Frontend**: Generate TypeScript API client from OpenAPI spec
- **Monitoring**: Add observability hooks ([Knowledge Hooks](./docs_sync_hook.md))

## Related Patterns

- [Fan-Out Generation](../patterns/fanout_generation.md)
- [Freeze Blocks](../patterns/freeze_blocks.md)
- [Multi-File Output](../patterns/multi_file_output.md)
- [API Design Patterns](../patterns/api_patterns.md)

---

**Success checkpoint:** You should have:
1. ✅ Multiple files generated from one graph
2. ✅ Handlers with custom logic preserved
3. ✅ Routes automatically updated
4. ✅ OpenAPI docs in sync
5. ✅ Tests scaffolded and ready

If endpoints aren't appearing, validate your graph with `ggen validate` and check for RDF syntax errors.
