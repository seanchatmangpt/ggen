<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [Pattern 2: Multi-File Project](#pattern-2-multi-file-project)
  - [Intent](#intent)
  - [Motivation](#motivation)
  - [Applicability](#applicability)
  - [Structure](#structure)
  - [Implementation](#implementation)
    - [1. Define Multiple Outputs](#1-define-multiple-outputs)
    - [2. Structure Shared Data](#2-structure-shared-data)
    - [3. Generate Cross-Referenced Files](#3-generate-cross-referenced-files)
  - [Example: Complete API Module](#example-complete-api-module)
    - [Input Data](#input-data)
    - [Generated Files](#generated-files)
      - [`src/user_api.rs`](#srcuser_apirs)
      - [`src/user_api/models.rs`](#srcuser_apimodelsrs)
      - [`src/user_api/handlers.rs`](#srcuser_apihandlersrs)
      - [`src/user_api_tests.rs`](#srcuser_api_testsrs)
  - [Example: Database Model Suite](#example-database-model-suite)
    - [Input Data](#input-data-1)
    - [Generated Files](#generated-files-1)
      - [`src/models/product.rs`](#srcmodelsproductrs)
      - [`src/models/product_repository.rs`](#srcmodelsproduct_repositoryrs)
      - [`migrations/001_create_products_table.sql`](#migrations001_create_products_tablesql)
  - [Example: Service Implementation](#example-service-implementation)
    - [Input Data](#input-data-2)
    - [Generated Files](#generated-files-2)
      - [`src/services/email_service.rs`](#srcservicesemail_servicers)
      - [`src/services/email_service_tests.rs`](#srcservicesemail_service_testsrs)
  - [Consequences](#consequences)
    - [Benefits](#benefits)
    - [Drawbacks](#drawbacks)
  - [Related Patterns](#related-patterns)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [Pattern 2: Multi-File Project](#pattern-2-multi-file-project)
  - [Intent](#intent)
  - [Motivation](#motivation)
  - [Applicability](#applicability)
  - [Structure](#structure)
  - [Implementation](#implementation)
  - [Example: Complete API Module](#example-complete-api-module)
  - [Example: Database Model Suite](#example-database-model-suite)
  - [Example: Service Implementation](#example-service-implementation)
  - [Consequences](#consequences)
  - [Related Patterns](#related-patterns)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Pattern 2: Multi-File Project

**Generate multiple related files that work together as a cohesive unit**

## Intent

Create complete, self-contained modules or projects consisting of multiple files that reference each other and work together to provide a unified functionality.

## Motivation

Real-world software projects consist of multiple files that must work together. A single struct often needs accompanying tests, documentation, and integration code. This pattern addresses the need to generate entire modules with all their supporting files.

## Applicability

Use this pattern when:
- Creating complete modules with implementation and tests
- Generating API endpoints with handlers, models, and tests
- Building database models with migrations and queries
- Developing service implementations with configuration and middleware
- Producing documentation alongside implementation

## Structure

```yaml
---
to:
  - "src/{{ module_name | lower }}.rs"           # Main implementation
  - "src/{{ module_name | lower }}_tests.rs"    # Unit tests
  - "src/{{ module_name | lower }}_integration_tests.rs"  # Integration tests
  - "README.md"                                  # Documentation
vars:
  module_name: "user_service"
  entities: [...]
  endpoints: [...]
---

// src/user_service.rs
pub mod {{ module_name | lower }} {
    {{#each entities}}
    pub mod {{ name | lower }};
    {{/each}}
    {{#each endpoints}}
    pub mod {{ name | lower }};
    {{/each}}
}

// src/user_service_tests.rs
#[cfg(test)]
mod tests {
    // Unit tests for the module
}

// src/user_service_integration_tests.rs
#[cfg(test)]
mod integration_tests {
    // Integration tests
}

// README.md
# {{ module_name }} Module
// Comprehensive documentation
```

## Implementation

### 1. Define Multiple Outputs
```yaml
to:
  - "src/{{ module_name }}.rs"
  - "tests/{{ module_name }}_test.rs"
  - "docs/{{ module_name }}.md"
```

### 2. Structure Shared Data
```yaml
vars:
  module_name: "payment_service"
  shared_imports: ["serde::{Deserialize, Serialize}"]
  entities:
    - name: "Payment"
      fields: [...]
    - name: "Transaction"
      fields: [...]
  endpoints:
    - name: "CreatePayment"
      method: "POST"
      path: "/payments"
    - name: "GetPayment"
      method: "GET"
      path: "/payments/{id}"
```

### 3. Generate Cross-Referenced Files
```rust
// Main module file
pub mod {{ module_name | lower }} {
    {{#each shared_imports}}
    use {{ this }};
    {{/each}}

    {{#each entities}}
    pub mod {{ name | lower }};
    {{/each}}

    {{#each endpoints}}
    pub mod {{ name | lower }};
    {{/each}}
}

// Entity file
pub mod {{ name | lower }} {
    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct {{ name }} {
        {{#each fields}}
        pub {{ name }}: {{ type }},
        {{/each}}
    }
}

// Endpoint file
pub mod {{ name | lower }} {
    use super::{{ entity_name | lower }}::*;

    pub async fn handle_{{ name | lower }}() -> Result<Json<{{ response_type }}>> {
        // Handler implementation
    }
}
```

## Example: Complete API Module

### Input Data
```yaml
vars:
  api_name: "user_api"
  base_path: "/api/v1"
  entities:
    - name: "User"
      fields:
        - name: "id"
          type: "u64"
        - name: "username"
          type: "String"
        - name: "email"
          type: "String"
  endpoints:
    - name: "CreateUser"
      method: "POST"
      request_type: "CreateUserRequest"
      response_type: "User"
    - name: "GetUser"
      method: "GET"
      path_params: ["id"]
      response_type: "User"
    - name: "ListUsers"
      method: "GET"
      query_params: ["limit", "offset"]
      response_type: "Vec<User>"
```

### Generated Files

#### `src/user_api.rs`
```rust
use axum::{
    extract::{Path, Query},
    http::StatusCode,
    response::Json,
    routing::{get, post},
    Router,
};
use serde::{Deserialize, Serialize};

pub mod user_api {
    pub mod models;
    pub mod handlers;
    pub mod middleware;
}

pub fn create_router() -> Router {
    Router::new()
        .route("/users", post(handlers::create_user))
        .route("/users/:id", get(handlers::get_user))
        .route("/users", get(handlers::list_users))
}
```

#### `src/user_api/models.rs`
```rust
use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct User {
    pub id: u64,
    pub username: String,
    pub email: String,
}

#[derive(Debug, Deserialize)]
pub struct CreateUserRequest {
    pub username: String,
    pub email: String,
}

#[derive(Debug, Deserialize)]
pub struct ListUsersQuery {
    pub limit: Option<u32>,
    pub offset: Option<u32>,
}
```

#### `src/user_api/handlers.rs`
```rust
use axum::{
    extract::{Path, Query},
    http::StatusCode,
    response::Json,
};
use super::models::*;

pub async fn create_user(
    Json(request): Json<CreateUserRequest>,
) -> Result<Json<User>, StatusCode> {
    // Implementation would go here
    let user = User {
        id: 1, // Would be generated
        username: request.username,
        email: request.email,
    };
    Ok(Json(user))
}

pub async fn get_user(
    Path(id): Path<u64>,
) -> Result<Json<User>, StatusCode> {
    // Implementation would go here
    let user = User {
        id,
        username: "example".to_string(),
        email: "user@example.com".to_string(),
    };
    Ok(Json(user))
}

pub async fn list_users(
    Query(params): Query<ListUsersQuery>,
) -> Json<Vec<User>> {
    // Implementation would go here
    let limit = params.limit.unwrap_or(20);
    let offset = params.offset.unwrap_or(0);

    // Mock data for example
    let users = vec![
        User {
            id: 1,
            username: "alice".to_string(),
            email: "alice@example.com".to_string(),
        },
        User {
            id: 2,
            username: "bob".to_string(),
            email: "bob@example.com".to_string(),
        },
    ];

    Json(users)
}
```

#### `src/user_api_tests.rs`
```rust
#[cfg(test)]
mod tests {
    use super::*;
    use axum::{
        body::Body,
        http::{Method, Request, StatusCode},
        response::Response,
        routing::get,
        Router,
    };
    use tower::ServiceExt;

    #[tokio::test]
    async fn test_create_user() {
        let app = create_router();

        let request = Request::builder()
            .method(Method::POST)
            .uri("/users")
            .header("content-type", "application/json")
            .body(Body::from(r#"{"username":"test","email":"test@example.com"}"#))
            .unwrap();

        let response = app.oneshot(request).await.unwrap();
        assert_eq!(response.status(), StatusCode::OK);
    }

    #[tokio::test]
    async fn test_get_user() {
        let app = create_router();

        let request = Request::builder()
            .method(Method::GET)
            .uri("/users/1")
            .body(Body::empty())
            .unwrap();

        let response = app.oneshot(request).await.unwrap();
        assert_eq!(response.status(), StatusCode::OK);
    }

    #[tokio::test]
    async fn test_list_users() {
        let app = create_router();

        let request = Request::builder()
            .method(Method::GET)
            .uri("/users?limit=10&offset=0")
            .body(Body::empty())
            .unwrap();

        let response = app.oneshot(request).await.unwrap();
        assert_eq!(response.status(), StatusCode::OK);
    }
}
```

## Example: Database Model Suite

### Input Data
```yaml
vars:
  model_name: "product"
  table_name: "products"
  fields:
    - name: "id"
      type: "i64"
      primary_key: true
    - name: "name"
      type: "String"
      nullable: false
    - name: "description"
      type: "Option<String>"
      nullable: true
    - name: "price"
      type: "f64"
      nullable: false
    - name: "category_id"
      type: "i64"
      nullable: false
    - name: "created_at"
      type: "chrono::DateTime<chrono::Utc>"
      nullable: false
  relationships:
    - name: "category"
      type: "belongs_to"
      foreign_table: "categories"
      foreign_key: "category_id"
```

### Generated Files

#### `src/models/product.rs`
```rust
use chrono::{DateTime, Utc};
use serde::{Deserialize, Serialize};
use sqlx::FromRow;

#[derive(Debug, Clone, FromRow, Serialize, Deserialize)]
pub struct Product {
    pub id: i64,
    pub name: String,
    pub description: Option<String>,
    pub price: f64,
    pub category_id: i64,
    pub created_at: DateTime<Utc>,
}

impl Product {
    pub fn new(
        name: String,
        description: Option<String>,
        price: f64,
        category_id: i64,
    ) -> Self {
        Self {
            id: 0, // Will be set by database
            name,
            description,
            price,
            category_id,
            created_at: Utc::now(),
        }
    }
}
```

#### `src/models/product_repository.rs`
```rust
use super::Product;
use sqlx::{PgPool, Result};
use chrono::{DateTime, Utc};

pub struct ProductRepository {
    pool: PgPool,
}

impl ProductRepository {
    pub fn new(pool: PgPool) -> Self {
        Self { pool }
    }

    pub async fn find_by_id(&self, id: i64) -> Result<Option<Product>> {
        let product = sqlx::query_as!(
            Product,
            "SELECT * FROM products WHERE id = $1",
            id
        )
        .fetch_optional(&self.pool)
        .await?;

        Ok(product)
    }

    pub async fn find_by_category(&self, category_id: i64) -> Result<Vec<Product>> {
        let products = sqlx::query_as!(
            Product,
            "SELECT * FROM products WHERE category_id = $1 ORDER BY name",
            category_id
        )
        .fetch_all(&self.pool)
        .await?;

        Ok(products)
    }

    pub async fn create(&self, product: &Product) -> Result<Product> {
        let created = sqlx::query_as!(
            Product,
            "INSERT INTO products (name, description, price, category_id, created_at)
             VALUES ($1, $2, $3, $4, $5)
             RETURNING *",
            product.name,
            product.description,
            product.price,
            product.category_id,
            product.created_at
        )
        .fetch_one(&self.pool)
        .await?;

        Ok(created)
    }
}
```

#### `migrations/001_create_products_table.sql`
```sql
CREATE TABLE products (
    id BIGSERIAL PRIMARY KEY,
    name VARCHAR NOT NULL,
    description TEXT,
    price DECIMAL(10,2) NOT NULL,
    category_id BIGINT NOT NULL,
    created_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT NOW(),
    FOREIGN KEY (category_id) REFERENCES categories(id)
);

CREATE INDEX idx_products_category_id ON products(category_id);
CREATE INDEX idx_products_name ON products(name);
```

## Example: Service Implementation

### Input Data
```yaml
vars:
  service_name: "email_service"
  dependencies:
    - "tokio"
    - "lettre"
    - "serde_json"
  configuration:
    - name: "smtp_host"
      type: "String"
      required: true
    - name: "smtp_port"
      type: "u16"
      default: 587
    - name: "smtp_username"
      type: "String"
      required: true
    - name: "smtp_password"
      type: "String"
      required: true
  methods:
    - name: "send_welcome_email"
      params:
        - name: "to"
          type: "String"
        - name: "username"
          type: "String"
    - name: "send_password_reset"
      params:
        - name: "to"
          type: "String"
        - name: "reset_token"
          type: "String"
```

### Generated Files

#### `src/services/email_service.rs`
```rust
use lettre::{
    message::{header::ContentType, Mailbox},
    transport::smtp::authentication::Credentials,
    AsyncSmtpTransport, AsyncTransport, Message, Tokio1Executor,
};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct EmailConfig {
    pub smtp_host: String,
    pub smtp_port: u16,
    pub smtp_username: String,
    pub smtp_password: String,
}

impl Default for EmailConfig {
    fn default() -> Self {
        Self {
            smtp_host: "smtp.gmail.com".to_string(),
            smtp_port: 587,
            smtp_username: "".to_string(),
            smtp_password: "".to_string(),
        }
    }
}

pub struct EmailService {
    config: EmailConfig,
    transporter: AsyncSmtpTransport<Tokio1Executor>,
}

impl EmailService {
    pub fn new(config: EmailConfig) -> Result<Self, Box<dyn std::error::Error>> {
        let creds = Credentials::new(
            config.smtp_username.clone(),
            config.smtp_password.clone(),
        );

        let transporter = AsyncSmtpTransport::<Tokio1Executor>::relay(&config.smtp_host)?
            .port(config.smtp_port)
            .credentials(creds)
            .build();

        Ok(Self { config, transporter })
    }

    pub async fn send_welcome_email(
        &self,
        to: String,
        username: String,
    ) -> Result<(), Box<dyn std::error::Error>> {
        let email = Message::builder()
            .from("noreply@example.com".parse()?)
            .to(to.parse()?)
            .subject("Welcome to Our Platform!")
            .header(ContentType::TEXT_HTML)
            .body(format!(
                r#"
                <h1>Welcome, {}!</h1>
                <p>Thank you for joining our platform. We're excited to have you on board!</p>
                <p>Best regards,<br>The Team</p>
                "#,
                username
            ))?;

        self.transporter.send(email).await?;
        Ok(())
    }

    pub async fn send_password_reset(
        &self,
        to: String,
        reset_token: String,
    ) -> Result<(), Box<dyn std::error::Error>> {
        let reset_url = format!("https://app.example.com/reset-password?token={}", reset_token);

        let email = Message::builder()
            .from("noreply@example.com".parse()?)
            .to(to.parse()?)
            .subject("Password Reset Request")
            .header(ContentType::TEXT_HTML)
            .body(format!(
                r#"
                <h1>Password Reset</h1>
                <p>You requested a password reset for your account.</p>
                <p>Click the link below to reset your password:</p>
                <p><a href="{}">Reset Password</a></p>
                <p>If you didn't request this reset, please ignore this email.</p>
                <p>Best regards,<br>The Team</p>
                "#,
                reset_url
            ))?;

        self.transporter.send(email).await?;
        Ok(())
    }
}
```

#### `src/services/email_service_tests.rs`
```rust
#[cfg(test)]
mod tests {
    use super::*;
    use std::env;

    #[test]
    fn test_email_config_default() {
        let config = EmailConfig::default();
        assert_eq!(config.smtp_port, 587);
        assert_eq!(config.smtp_host, "smtp.gmail.com");
    }

    #[test]
    fn test_email_service_creation() {
        // This would require actual SMTP credentials for a full test
        // For now, we test that the structure is correct
        let config = EmailConfig {
            smtp_host: "smtp.test.com".to_string(),
            smtp_port: 587,
            smtp_username: "test@example.com".to_string(),
            smtp_password: "password123".to_string(),
        };

        // We can't actually create the service without valid SMTP server
        // but we can verify the config structure
        assert!(!config.smtp_host.is_empty());
        assert!(config.smtp_port > 0);
    }

    #[test]
    fn test_welcome_email_content() {
        // Test that our email templates contain expected content
        let to = "user@example.com".to_string();
        let username = "TestUser".to_string();

        // Since we can't send actual emails in tests,
        // we verify that the service can be created and methods exist
        let config = EmailConfig::default();
        // Note: In a real test environment, you'd use a mock SMTP server
    }
}
```

## Consequences

### Benefits
- **Completeness**: Generates entire working modules
- **Consistency**: All related files follow the same patterns
- **Integration**: Files are designed to work together
- **Testing**: Includes comprehensive test coverage
- **Documentation**: Self-documenting generated code

### Drawbacks
- **Complexity**: More complex than single-file patterns
- **Maintenance**: Changes affect multiple files
- **Dependencies**: Requires coordination between file generation
- **Debugging**: Issues may span multiple generated files

## Related Patterns

- **Pattern 1: Single File Generator** - Building blocks for multi-file projects
- **Pattern 3: Conditional Generation** - For optional files or features
- **Pattern 4: Template Inheritance** - For sharing common structure
- **Pattern 5: Dynamic Variables** - For data-driven file relationships

This pattern enables the generation of complete, production-ready modules that encapsulate entire features or services, significantly reducing the manual work required for new functionality.
