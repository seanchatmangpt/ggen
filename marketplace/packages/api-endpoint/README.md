# API Endpoint Templates

Complete REST API endpoint templates with OpenAPI documentation for ggen.

## Features

- **Axum-based HTTP handlers** - Production-ready web framework
- **Request/response validation** - Comprehensive input validation
- **Error handling and status codes** - Proper HTTP status codes and error responses
- **OpenAPI specification generation** - Automatic API documentation
- **Rate limiting and security** - Built-in security features

## Installation

```bash
# Add to your project
ggen market add api-endpoint-templates

# Generate API endpoints
ggen template generate api-endpoint-templates --vars '{"name":"users","path":"/api/v1/users"}'
```

## Quick Start

1. **Install the package:**
   ```bash
   ggen market add api-endpoint-templates
   ```

2. **Generate API endpoints:**
   ```bash
   # Generate user API endpoints
   ggen template generate api-endpoint-templates \
     --vars '{"name":"users","description":"User management API","base_path":"/api/v1"}'

   # Generate product API endpoints
   ggen template generate api-endpoint-templates \
     --vars '{"name":"products","description":"Product catalog API","base_path":"/api/v1"}'
   ```

3. **Use in your application:**
   ```rust
   use api_endpoint_templates::{create_router, create_api_state};

   #[tokio::main]
   async fn main() -> anyhow::Result<()> {
       let state = create_api_state();
       let app = create_router().with_state(state);

       let listener = tokio::net::TcpListener::bind("0.0.0.0:3000").await?;
       axum::serve(listener, app).await?;

       Ok(())
   }
   ```

## Template Variables

| Variable | Type | Description | Example |
|----------|------|-------------|---------|
| `name` | string | Entity name (plural) | "users", "products" |
| `description` | string | API description | "User management API" |
| `base_path` | string | API base path | "/api/v1" |

## Generated Structure

The template generates:

```
src/api/
├── users.rs           # Complete API implementation
├── mod.rs             # API module declaration
└── openapi.json       # OpenAPI 3.0 specification
```

## API Features

### CRUD Operations
- ✅ **POST** `/{name}s` - Create new entity
- ✅ **GET** `/{name}s` - List all entities (with pagination)
- ✅ **GET** `/{name}s/{id}` - Get specific entity
- ✅ **PUT** `/{name}s/{id}` - Update entity
- ✅ **DELETE** `/{name}s/{id}` - Delete entity

### Request/Response Handling
- ✅ JSON request/response serialization
- ✅ Input validation with detailed error messages
- ✅ Proper HTTP status codes
- ✅ Request timeout handling
- ✅ Structured error responses

### Security Features
- ✅ Rate limiting (configurable)
- ✅ Input sanitization
- ✅ SQL injection protection
- ✅ CORS configuration
- ✅ Security headers

### Performance Features
- ✅ Async/await throughout
- ✅ Connection pooling
- ✅ Request deduplication
- ✅ Caching support
- ✅ Database query optimization

## OpenAPI Documentation

The template automatically generates OpenAPI 3.0 specifications:

```json
{
  "openapi": "3.0.0",
  "info": {
    "title": "Users API",
    "version": "1.0.0",
    "description": "User management API"
  },
  "paths": {
    "/api/v1/users": {
      "get": {
        "summary": "Get users",
        "responses": {
          "200": {
            "description": "Success"
          }
        }
      }
    }
  }
}
```

## Configuration

### Environment Variables

```bash
# API Configuration
API_PORT=3000
API_TIMEOUT_SECONDS=30
RATE_LIMIT_REQUESTS=1000
RATE_LIMIT_WINDOW_SECONDS=60

# CORS Configuration
CORS_ORIGINS=https://app.example.com,https://admin.example.com

# Database Configuration
DATABASE_URL=postgresql://localhost/app_db
```

### Runtime Configuration

```rust
let config = UsersApiConfig {
    timeout_seconds: 30,
    rate_limit_requests: 1000,
    rate_limit_window_seconds: 60,
    enable_cors: true,
    enable_compression: true,
};
```

## Error Handling

The API provides comprehensive error handling:

```rust
// Validation errors (400)
{
  "success": false,
  "error": "ValidationError",
  "message": "Invalid input data",
  "details": {
    "field": "email",
    "reason": "Invalid email format"
  }
}

// Not found errors (404)
{
  "success": false,
  "error": "NotFound",
  "message": "Resource not found",
  "request_id": "uuid"
}
```

## Testing

The template includes comprehensive tests:

```bash
# Run unit tests
cargo test api::users

# Run integration tests
cargo test api_integration

# Run performance tests
cargo test api_performance
```

## Examples

### Basic Usage

```bash
# Install the package
ggen market add api-endpoint-templates

# Generate users API
ggen template generate api-endpoint-templates \
  --vars '{"name":"users","description":"User management endpoints"}'

# Generate products API
ggen template generate api-endpoint-templates \
  --vars '{"name":"products","description":"Product catalog endpoints"}'

# Build and run
cargo build --release
cargo run
```

### Advanced Configuration

```bash
# Generate API with custom configuration
ggen template generate api-endpoint-templates \
  --vars '{
    "name":"orders",
    "description":"Order management API",
    "base_path":"/api/v2",
    "rate_limit":5000,
    "timeout":60
  }'
```

## Integration

### With Database

```rust
// Add database integration
use sqlx::PgPool;

// In your main application
let db_pool = PgPool::connect(&database_url).await?;
let api_state = UsersApiState {
    config: UsersApiConfig::default(),
    db_pool,
};
```

### With Authentication

```rust
// Add JWT authentication middleware
use axum::middleware;
use tower::ServiceBuilder;

// Apply authentication middleware
let app = Router::new()
    .nest("/api/v1", create_router())
    .layer(ServiceBuilder::new()
        .layer(middleware::from_fn(auth_middleware))
    );
```

## Best Practices

1. **Use proper error types** - Define custom error types for your domain
2. **Implement rate limiting** - Protect against abuse with configurable limits
3. **Add request validation** - Validate all inputs before processing
4. **Use async database operations** - Leverage async/await for better performance
5. **Implement proper logging** - Use structured logging for observability
6. **Add health checks** - Include health check endpoints for monitoring

## Support

- **Issues**: https://github.com/seanchatmangpt/ggen/issues
- **Documentation**: https://seanchatmangpt.github.io/ggen/
- **Examples**: See the generated code for complete examples

## License

MIT License - see LICENSE file for details.
