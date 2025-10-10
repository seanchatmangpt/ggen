# Example Claude Desktop Conversations with ggen MCP

## Example 1: Generate a Rust Module

**User:**
> Create a Rust authentication module with login and logout functions

**Claude (using ggen MCP):**

I'll help you generate a Rust authentication module using ggen.

```
[Tool: ggen_market_search]
{
  "query": "rust module authentication",
  "limit": 5
}

Found: io.ggen.rust.auth-module (4.7⭐, 1,234 downloads)

[Tool: ggen_gen_with_vars]
{
  "template": "~/.ggen/gpacks/io.ggen.rust.auth-module/auth.tmpl",
  "vars": {
    "module_name": "auth",
    "functions": ["login", "logout", "verify_token"],
    "use_jwt": true,
    "determinism": 42
  },
  "output": "src/auth.rs"
}

Generated: src/auth.rs (156 lines)
```

**Result:**
```rust
// src/auth.rs
use jsonwebtoken::{encode, decode, Header, Validation, EncodingKey, DecodingKey};
use serde::{Serialize, Deserialize};

#[derive(Debug, Serialize, Deserialize)]
pub struct Claims {
    pub sub: String,
    pub exp: usize,
}

pub struct Auth {
    secret: String,
}

impl Auth {
    pub fn new(secret: String) -> Self {
        Self { secret }
    }

    pub fn login(&self, username: &str) -> Result<String, AuthError> {
        let claims = Claims {
            sub: username.to_owned(),
            exp: 10000000000,
        };

        encode(
            &Header::default(),
            &claims,
            &EncodingKey::from_secret(self.secret.as_ref())
        )
        .map_err(|e| AuthError::TokenGenerationFailed(e.to_string()))
    }

    pub fn logout(&self, token: &str) -> Result<(), AuthError> {
        // Token invalidation logic
        Ok(())
    }

    pub fn verify_token(&self, token: &str) -> Result<Claims, AuthError> {
        decode::<Claims>(
            token,
            &DecodingKey::from_secret(self.secret.as_ref()),
            &Validation::default()
        )
        .map(|data| data.claims)
        .map_err(|e| AuthError::InvalidToken(e.to_string()))
    }
}

#[derive(Debug, thiserror::Error)]
pub enum AuthError {
    #[error("Token generation failed: {0}")]
    TokenGenerationFailed(String),
    #[error("Invalid token: {0}")]
    InvalidToken(String),
}
```

---

## Example 2: Generate REST API from RDF

**User:**
> I have an RDF ontology defining Product, Order, and Customer entities. Generate a complete REST API in Rust.

**Claude (using ggen MCP):**

I'll load your RDF ontology and generate a complete REST API.

```
[Tool: ggen_graph_load]
{
  "file": "ontology/ecommerce.ttl",
  "format": "turtle"
}

Loaded 127 triples

[Tool: ggen_graph_query]
{
  "query": "PREFIX owl: <http://www.w3.org/2002/07/owl#> PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#> SELECT ?entity ?label ?property WHERE { ?entity a owl:Class ; rdfs:label ?label . OPTIONAL { ?entity :hasProperty ?property } }"
}

Found entities:
- Product (id, name, price, description)
- Order (orderId, customerId, products, total)
- Customer (customerId, email, name, address)

[Tool: ggen_gen_batch]
{
  "templates": [
    "templates/rust-axum-model.tmpl",
    "templates/rust-axum-routes.tmpl",
    "templates/rust-axum-handlers.tmpl",
    "templates/rust-database-schema.tmpl"
  ],
  "vars": {
    "entities": [
      {"name": "Product", "fields": ["id", "name", "price", "description"]},
      {"name": "Order", "fields": ["orderId", "customerId", "products", "total"]},
      {"name": "Customer", "fields": ["customerId", "email", "name", "address"]}
    ]
  },
  "parallel": true
}

Generated:
- src/models.rs (Data models with Serialize/Deserialize)
- src/routes.rs (REST endpoints: GET, POST, PUT, DELETE)
- src/handlers.rs (Request handlers with validation)
- migrations/001_schema.sql (PostgreSQL tables)
```

Your REST API is ready! Start with `cargo run` and test at http://localhost:3000

---

## Example 3: Multi-Language Code Generation

**User:**
> Generate TypeScript interfaces, Python dataclasses, and Rust structs from the same User model

**Claude (using ggen MCP):**

I'll generate the User model in all three languages with deterministic output.

```
[Tool: ggen_graph_add_triple]
{
  "subject": "http://api.example.org/User",
  "predicate": "http://www.w3.org/1999/02/22-rdf-syntax-ns#type",
  "object": "http://www.w3.org/2002/07/owl#Class"
}

[Tool: ggen_graph_add_triple]
{
  "subject": "http://api.example.org/User",
  "predicate": "http://api.example.org/hasField",
  "object": "id"
}

[Tool: ggen_graph_add_triple]
{
  "subject": "http://api.example.org/User",
  "predicate": "http://api.example.org/hasField",
  "object": "email"
}

[Tool: ggen_graph_add_triple]
{
  "subject": "http://api.example.org/User",
  "predicate": "http://api.example.org/hasField",
  "object": "name"
}

[Tool: ggen_gen_batch]
{
  "templates": [
    "templates/typescript-interface.tmpl",
    "templates/python-dataclass.tmpl",
    "templates/rust-struct.tmpl"
  ],
  "vars": {
    "entity": "User",
    "fields": ["id", "email", "name"],
    "determinism": 42
  }
}

Generated:
- frontend/types/user.ts
- backend/models/user.py
- api/src/models/user.rs
```

**Result:**

TypeScript:
```typescript
// frontend/types/user.ts
export interface User {
  id: string;
  email: string;
  name: string;
}
```

Python:
```python
# backend/models/user.py
from dataclasses import dataclass

@dataclass
class User:
    id: str
    email: str
    name: str
```

Rust:
```rust
// api/src/models/user.rs
use serde::{Serialize, Deserialize};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct User {
    pub id: String,
    pub email: String,
    pub name: String,
}
```

All three are semantically identical and generated from the same RDF graph!

---

## Example 4: Inject Code into Existing File

**User:**
> Add a new route handler to my existing router.rs file without duplicating

**Claude (using ggen MCP):**

I'll inject the new route handler using idempotent injection.

```
[Tool: ggen_inject_idempotent]
{
  "template": "templates/axum-route-injection.tmpl",
  "target": "src/router.rs",
  "vars": {
    "route_path": "/api/products",
    "handler": "product_handlers::list_products",
    "method": "get"
  }
}

Injected at line 42 (skipped duplicate check passed)
```

**Before:**
```rust
// src/router.rs
use axum::{Router, routing::*};

pub fn create_router() -> Router {
    Router::new()
        .route("/api/users", get(user_handlers::list_users))
        // Routes end
}
```

**After:**
```rust
// src/router.rs
use axum::{Router, routing::*};

pub fn create_router() -> Router {
    Router::new()
        .route("/api/users", get(user_handlers::list_users))
        .route("/api/products", get(product_handlers::list_products))
        // Routes end
}
```

If you run the injection again, ggen will detect the duplicate and skip it!

---

## Example 5: Generate Tests from Code

**User:**
> Generate comprehensive tests for my authentication module

**Claude (using ggen MCP):**

I'll analyze your auth module and generate unit and integration tests.

```
[Tool: ggen_graph_load]
{
  "file": "src/auth.rs",
  "format": "rust"
}

Parsed 3 functions: login, logout, verify_token

[Tool: ggen_gen]
{
  "template": "templates/rust-test-suite.tmpl",
  "output": "tests/auth_test.rs"
}

Generated: tests/auth_test.rs (89 lines, 12 test cases)
```

**Result:**
```rust
// tests/auth_test.rs
use my_app::auth::{Auth, AuthError};

#[test]
fn test_login_success() {
    let auth = Auth::new("secret".to_string());
    let token = auth.login("user@example.com").unwrap();
    assert!(!token.is_empty());
}

#[test]
fn test_login_invalid_credentials() {
    let auth = Auth::new("secret".to_string());
    let result = auth.login("");
    assert!(result.is_err());
}

#[test]
fn test_verify_token_valid() {
    let auth = Auth::new("secret".to_string());
    let token = auth.login("user@example.com").unwrap();
    let claims = auth.verify_token(&token).unwrap();
    assert_eq!(claims.sub, "user@example.com");
}

#[test]
fn test_verify_token_invalid() {
    let auth = Auth::new("secret".to_string());
    let result = auth.verify_token("invalid_token");
    assert!(matches!(result, Err(AuthError::InvalidToken(_))));
}

// ... 8 more tests
```

Run with: `cargo test auth_test`
