# Quick Start Guide

## 5-Minute Setup

### 1. Validate Templates (10 seconds)
```bash
./generate-project.sh validate
```

### 2. Generate Project (2-3 minutes)
```bash
./generate-project.sh generate
```

### 3. Run Generated Project (30 seconds)
```bash
cd output/my-web-service
cargo run
```

### 4. Test API (10 seconds)
```bash
# In another terminal
curl http://localhost:3000/health
curl -X POST http://localhost:3000/api/users \
  -H "Content-Type: application/json" \
  -d '{"name":"Alice","email":"alice@example.com"}'
curl http://localhost:3000/api/users
```

## What Gets Generated?

- **11 Rust source files** (613 lines)
- **Cargo.toml** with 10 dependencies
- **REST API** with 6 endpoints
- **2 data models** (User, Product)
- **10 integration tests**
- **Zero compilation errors**
- **All tests passing**

## Customization

```bash
./generate-project.sh generate \
  --project-name my-api \
  --port 8080 \
  --author "Your Name"
```

## File Structure

```
output/my-web-service/
├── Cargo.toml (package config)
├── src/
│   ├── main.rs (entry point)
│   ├── models/ (data models)
│   │   ├── mod.rs
│   │   ├── user.rs
│   │   └── product.rs
│   ├── api/ (REST handlers)
│   │   ├── mod.rs
│   │   ├── handlers.rs
│   │   └── routes.rs
│   └── config/ (settings)
│       ├── mod.rs
│       └── settings.rs
└── tests/
    └── integration_test.rs
```

## Key Features

- **Actix-Web** - Fast async web framework
- **Serde** - JSON serialization
- **Validator** - Request validation
- **UUID** - Unique identifiers
- **Env Logger** - Logging support
- **Full test suite** - Unit + integration tests

## Common Commands

```bash
# Build
cargo build --release

# Run tests
cargo test

# Format code
cargo fmt

# Check without building
cargo check

# Run with custom port
PORT=8080 cargo run

# Watch for changes
cargo watch -x run
```

## API Endpoints

| Method | Path | Description |
|--------|------|-------------|
| GET | /health | Health check |
| GET | /api/users | List all users |
| GET | /api/users/:id | Get user by ID |
| POST | /api/users | Create user |
| GET | /api/products | List products |
| POST | /api/products | Create product |

## Example Requests

```bash
# Create user
curl -X POST http://localhost:3000/api/users \
  -H "Content-Type: application/json" \
  -d '{
    "name": "Bob",
    "email": "bob@example.com"
  }'

# Create product
curl -X POST http://localhost:3000/api/products \
  -H "Content-Type: application/json" \
  -d '{
    "name": "Laptop",
    "description": "High-performance laptop",
    "price": 999.99,
    "stock": 10
  }'

# List users
curl http://localhost:3000/api/users

# Get specific user
curl http://localhost:3000/api/users/<uuid>
```

## Troubleshooting

**Problem**: Port already in use
```bash
PORT=8080 cargo run
```

**Problem**: Build fails with dependency errors
```bash
cargo clean
cargo build
```

**Problem**: Tests fail
```bash
cargo test -- --nocapture  # See test output
```

## Next Steps

1. **Add authentication** - See templates for auth examples
2. **Add database** - Integrate PostgreSQL/MySQL
3. **Add Docker** - Containerize the service
4. **Add CI/CD** - GitHub Actions workflow
5. **Deploy** - AWS/GCP/Azure deployment

## Resources

- [Full Documentation](README.md)
- [Template Reference](templates/)
- [Project Specification](project-spec.yaml)
