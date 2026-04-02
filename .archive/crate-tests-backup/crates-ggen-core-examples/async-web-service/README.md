# Async Web Service Example

Production-ready REST API with Actix-web, PostgreSQL, Redis, and JWT authentication.

## Features

- **REST API** with OpenAPI/Swagger documentation
- **JWT Authentication** with bcrypt password hashing
- **PostgreSQL** with SQLx for type-safe queries
- **Redis** caching layer
- **Comprehensive Testing** (unit, integration, load tests)
- **Production Ready** with health checks, metrics, logging
- **Docker Support** with multi-stage builds
- **Lifecycle Management** with make.toml

## Quick Start

```bash
# Install dependencies and setup database
cargo make dev-prepare

# Run development server with hot reload
cargo make dev

# Access the API
open http://127.0.0.1:8080/swagger-ui/
```

## API Endpoints

### Authentication
- `POST /api/v1/auth/register` - Register new user
- `POST /api/v1/auth/login` - Login and get JWT token

### Users (Protected)
- `GET /api/v1/users` - List users (paginated)
- `GET /api/v1/users/{id}` - Get user by ID
- `POST /api/v1/users` - Create user
- `PUT /api/v1/users/{id}` - Update user
- `DELETE /api/v1/users/{id}` - Delete user

### Health
- `GET /api/v1/health` - Health check
- `GET /api/v1/metrics` - Prometheus metrics

## Lifecycle Tasks

### Development
```bash
cargo make dev-prepare    # Setup environment
cargo make dev            # Run with hot reload
cargo make check          # Type check
cargo make fmt            # Format code
cargo make lint           # Run clippy
```

### Testing
```bash
cargo make test-unit          # Unit tests
cargo make test-integration   # Integration tests
cargo make test-all           # All tests
cargo make test-coverage      # Coverage report
cargo make load-test          # Load testing
```

### Build
```bash
cargo make build              # Debug build
cargo make build-release      # Optimized build
cargo make docker-build       # Docker image
```

### Deploy
```bash
cargo make deploy-local       # Local deployment with health checks
cargo make deploy-docker      # Docker Compose deployment
```

### Database
```bash
cargo make db-setup          # Setup database
cargo make db-reset          # Reset database
cargo make db-migrate-new    # Create migration
```

## Environment Variables

Copy `.env.example` to `.env`:

```bash
DATABASE_URL=postgresql://postgres:postgres@localhost:5432/async_web_service
REDIS_URL=redis://127.0.0.1:6379
JWT_SECRET=your-secret-key-change-in-production
BIND_ADDRESS=127.0.0.1:8080
RUST_LOG=async_web_service=debug,actix_web=info
```

## Docker Deployment

```bash
# Build and run with Docker Compose
cargo make docker-compose-up

# Stop services
cargo make docker-compose-down
```

## Testing

The example includes comprehensive tests:

1. **Unit Tests**: Business logic validation
2. **Integration Tests**: Full API endpoint testing
3. **Load Tests**: Performance testing with oha

Example API test:
```bash
# Register user
curl -X POST http://localhost:8080/api/v1/auth/register \
  -H "Content-Type: application/json" \
  -d '{"email":"test@example.com","username":"testuser","password":"password123"}'

# Login
curl -X POST http://localhost:8080/api/v1/auth/login \
  -H "Content-Type: application/json" \
  -d '{"email":"test@example.com","password":"password123"}'

# Get users (with token)
curl http://localhost:8080/api/v1/users \
  -H "Authorization: Bearer YOUR_TOKEN"
```

## Architecture

```
src/
├── main.rs              # Server setup and configuration
├── models.rs            # Data models and schemas
├── routes/              # API endpoints
│   ├── mod.rs
│   ├── auth.rs         # Authentication
│   ├── users.rs        # User management
│   └── health.rs       # Health & metrics
├── db/                  # Database layer
│   ├── mod.rs
│   └── users.rs        # User queries
└── middleware/          # Custom middleware
    ├── mod.rs
    └── auth.rs         # JWT validation

migrations/              # SQL migrations
tests/                   # Integration tests
```

## Production Features

- ✅ Graceful shutdown
- ✅ Request logging
- ✅ CORS configuration
- ✅ Connection pooling
- ✅ Health checks
- ✅ Prometheus metrics
- ✅ OpenAPI documentation
- ✅ Input validation
- ✅ Error handling
- ✅ Rate limiting ready
- ✅ Redis caching

## Performance

Load test results (using `cargo make load-test`):
- Handles 100+ concurrent connections
- Sub-millisecond response times for cached endpoints
- Database connection pooling for optimal resource usage

## License

MIT
