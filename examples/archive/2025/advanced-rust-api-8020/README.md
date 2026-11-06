# Advanced Rust API - 80/20 Production Example

**Production-ready REST API demonstrating ggen lifecycle + AI-powered generation**

This example showcases the complete ggen development workflow following 80/20 principles:
- ✅ Universal lifecycle management with `make.toml`
- ✅ AI-powered code generation from SPARQL specifications
- ✅ Production readiness tracking
- ✅ Comprehensive error handling (no `.expect()` in production code)
- ✅ Structured logging and tracing
- ✅ Health checks and monitoring
- ✅ Docker containerization
- ✅ Complete test coverage

## 🚀 Quick Start

### 1. Initialize Project

```bash
# Initialize the project structure
ggen lifecycle run init

# This creates:
# - Rust library structure
# - API, models, database directories
# - Test directories
# - RDF specification files
```

### 2. Setup Dependencies

```bash
# Install all required dependencies
ggen lifecycle run setup

# This installs:
# - axum (web framework)
# - sqlx (database)
# - tracing (logging)
# - JWT authentication
# - And more...
```

### 3. Generate Code with AI

```bash
# Generate API endpoints from SPARQL specifications
ggen lifecycle run generate

# This uses ggen-ai to:
# - Parse RDF specifications
# - Generate REST endpoints
# - Create database models
# - Generate tests
```

### 4. Build and Test

```bash
# Run complete build and test pipeline
ggen lifecycle pipeline build test security

# Or use the shortcut:
npm run check
```

### 5. Check Production Readiness

```bash
# Check production readiness status
ggen lifecycle readiness --detailed

# Update requirement status
ggen lifecycle readiness-update auth-jwt complete
```

### 6. Deploy

```bash
# Deploy to production (requires approval)
ggen lifecycle run deploy

# Or run the full release pipeline:
npm run release
```

## 📋 Lifecycle Phases

| Phase | Description | Commands |
|-------|-------------|----------|
| `init` | Initialize project structure | cargo init, mkdir |
| `setup` | Install dependencies | cargo add ... |
| `generate` | AI-powered code generation | ai-generator |
| `validate` | Validate generated code | cargo check, clippy |
| `build` | Build optimized binary | cargo build --release |
| `test` | Run test suite | cargo test |
| `security` | Security audit | cargo audit, clippy |
| `docs` | Generate documentation | cargo doc |
| `readiness` | Check production readiness | ggen lifecycle readiness |
| `deploy` | Deploy to production | docker build |

## 🤖 AI-Powered Generation

### Generate New Endpoint

```bash
# Set endpoint name
export NAME=Product

# Generate endpoint from template
ggen lifecycle run ai:generate-endpoint

# This creates:
# - src/api/product.rs (endpoint handlers)
# - src/models/product.rs (data models)
# - tests/product_tests.rs (test suite)
```

### Generate Database Model

```bash
export NAME=Order
ggen lifecycle run ai:generate-model
```

## 📊 Production Readiness (80/20 Rule)

### Critical Requirements (20% effort, 80% value)

- ✅ JWT Authentication
- ✅ Comprehensive Error Handling
- ✅ Structured Logging with Tracing
- ✅ Health Check Endpoints
- ✅ Input Validation & Sanitization

### Important Requirements (30% effort, 15% value)

- 🚧 OpenAPI Documentation
- 🚧 Unit Tests (>80% coverage)
- 🚧 Integration Tests
- ✅ Docker Containerization

### Nice-to-Have (50% effort, 5% value)

- 🚧 API Rate Limiting
- 🚧 Redis Caching Layer

**Current Score: 65%** - Ready for MVP deployment

## 🏗️ Architecture

```
advanced-rust-api/
├── src/
│   ├── api/           # REST endpoints (generated from SPARQL)
│   ├── models/        # Database models (generated)
│   ├── db/            # Database connection and queries
│   ├── auth/          # JWT authentication
│   ├── middleware/    # Axum middleware
│   └── lib.rs         # Library entry point
├── data/
│   ├── api-spec.ttl   # REST API specification (RDF)
│   ├── db-schema.ttl  # Database schema (RDF)
│   └── test-spec.ttl  # Test specifications (RDF)
├── templates/
│   ├── endpoint.tmpl  # Endpoint generation template
│   ├── model.tmpl     # Model generation template
│   └── test.tmpl      # Test generation template
├── tests/
│   └── integration/   # Integration tests
├── make.toml          # Universal lifecycle configuration
└── Cargo.toml         # Rust project manifest
```

## 🔧 Environment Configuration

### Development

```bash
export GGEN_ENV=development
export DATABASE_URL=postgres://localhost/dev_api
export RUST_LOG=debug

ggen lifecycle run build
```

### Staging

```bash
export GGEN_ENV=staging
export DATABASE_URL=postgres://staging-db/api

ggen lifecycle run build
```

### Production

```bash
export GGEN_ENV=production
export DATABASE_URL=$DATABASE_URL
export RUST_LOG=warn

ggen lifecycle run deploy
```

## 📖 Key Features

### 1. Production-Ready Error Handling

```rust
// ❌ BAD - Crashes on error
let config = Config::load().expect("Failed to load config");

// ✅ GOOD - Returns error that can be handled
let config = Config::load()
    .map_err(|e| AppError::ConfigError(e.to_string()))?;
```

### 2. Structured Logging

```rust
#[tracing::instrument(skip(pool))]
pub async fn create_user(
    State(pool): State<DbPool>,
    Json(payload): Json<CreateUserRequest>,
) -> Result<impl IntoResponse, AppError> {
    tracing::info!(email = %payload.email, "Creating user");
    // ...
}
```

### 3. Health Checks

```rust
pub async fn health_check(State(pool): State<DbPool>) -> impl IntoResponse {
    let db_status = sqlx::query("SELECT 1")
        .fetch_one(&pool)
        .await
        .map(|_| "healthy")
        .unwrap_or("unhealthy");

    Json(json!({
        "status": if db_status == "healthy" { "healthy" } else { "degraded" },
        "database": db_status,
        "version": env!("CARGO_PKG_VERSION"),
    }))
}
```

### 4. Input Validation

```rust
pub async fn create_user(
    State(pool): State<DbPool>,
    Json(payload): Json<CreateUserRequest>,
) -> Result<impl IntoResponse, AppError> {
    // Validate email format
    if !payload.email.contains('@') {
        return Err(AppError::BadRequest("Invalid email format".into()));
    }

    // Validate password strength
    if payload.password.len() < 8 {
        return Err(AppError::BadRequest("Password must be at least 8 characters".into()));
    }

    // Continue with creation...
}
```

## 🧪 Testing

### Unit Tests

```bash
cargo test --lib
```

### Integration Tests

```bash
cargo test --test integration_tests
```

### Coverage Report

```bash
ggen lifecycle run check:coverage
open coverage/index.html
```

## 🐳 Docker Deployment

```bash
# Build Docker image
docker build -t advanced-rust-api:latest .

# Run container
docker run -p 8080:8080 \
  -e DATABASE_URL=postgres://db/api \
  -e RUST_LOG=info \
  advanced-rust-api:latest
```

## 📚 Documentation

### Generate API Docs

```bash
ggen lifecycle run docs
open target/doc/advanced_rust_api/index.html
```

### OpenAPI Specification

Generated from RDF specifications in `data/api-spec.ttl`.

## 🔐 Security

### Security Audit

```bash
ggen lifecycle run security
```

### Update Dependencies

```bash
cargo update
ggen lifecycle run test
```

## 📈 Performance

### Benchmarks

```bash
cargo bench
```

### Production Optimization

```toml
[profile.release]
opt-level = 3
lto = true
codegen-units = 1
strip = true
```

## 🎯 80/20 Principles Applied

1. **Focus on Critical Features First**
   - Authentication ✅
   - Error handling ✅
   - Logging ✅
   - Health checks ✅

2. **Defer Nice-to-Have Features**
   - Rate limiting (can add later)
   - Advanced caching (optimize when needed)
   - Complex monitoring (start simple)

3. **Automate Everything**
   - Lifecycle phases handle all operations
   - AI generates boilerplate code
   - Hooks automate workflows

4. **Production Readiness Tracking**
   - Know what's missing
   - Prioritize by impact
   - Track progress

## 🤝 Contributing

This is an example project demonstrating ggen capabilities. For real projects:

1. Customize `make.toml` for your needs
2. Update RDF specifications in `data/`
3. Modify templates in `templates/`
4. Add project-specific requirements

## 📝 License

MIT

## 🔗 Resources

- [ggen Documentation](https://github.com/seanchatmangpt/ggen)
- [ggen Lifecycle Guide](../../docs/lifecycle.md)
- [ggen AI Integration](../../docs/ai.md)
- [80/20 Production Principles](../../docs/production.md)
