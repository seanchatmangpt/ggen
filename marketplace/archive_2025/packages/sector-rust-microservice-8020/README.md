# Sector: Rust Microservice 8020

**Status**: 8020 Certified ✅
**Dark Matter Reduction**: Eliminates ~50% of service scaffolding work (8 hours saved per service)

## Overview

This bundle provides production-ready Rust microservice scaffolding that covers 80% of common service architecture needs. It eliminates the repetitive boilerplate of setting up REST APIs, error handling, observability, and service infrastructure, allowing teams to focus on business logic instead of scaffolding.

## What Gets Generated

- **Service Structure**: Complete Cargo workspace with proper module organization
- **REST API Layer**: Actix-web or Axum handlers with routing configuration
- **Error Handling**: Comprehensive error types with proper HTTP status mapping
- **Observability**: OpenTelemetry instrumentation, structured logging, metrics
- **Configuration**: Environment-based config with validation
- **Health Endpoints**: /health and /ready endpoints with dependency checks
- **Database Layer**: Repository pattern with connection pooling
- **Testing**: Unit test templates, integration test setup, test fixtures
- **Docker**: Multi-stage Dockerfile optimized for Rust
- **CI/CD**: GitHub Actions workflow for build, test, lint, security checks

## Quick Start

```bash
# Install the bundle
ggen install sector-rust-microservice-8020

# Generate a new microservice
ggen generate microservice \
  --name user-service \
  --port 8080 \
  --database postgres \
  --bundle sector-rust-microservice-8020

# Build and run
cd user-service
cargo build --release
cargo run
```

## Dark Matter Eliminated

### Before: 16 hours
- [ ] Create Cargo workspace and dependencies (2 hours)
- [ ] Set up web framework and routing (2 hours)
- [ ] Implement error handling patterns (2 hours)
- [ ] Configure logging and observability (3 hours)
- [ ] Set up database connections and migrations (2 hours)
- [ ] Write health check endpoints (1 hour)
- [ ] Create Docker and CI/CD configuration (2 hours)
- [ ] Write initial tests and test infrastructure (2 hours)

### After: 8 hours
- [x] Service generated in < 5 minutes
- [x] All boilerplate provided and tested
- [ ] Customize business logic (4 hours)
- [ ] Add domain-specific endpoints (2 hours)
- [ ] Implement advanced features (2 hours)

**Result**: 50% reduction in manual scaffolding work

## 8020 Coverage

- ✅ **Web Framework Integration**: Actix-web/Axum setup with routing
- ✅ **Error Handling**: Custom error types with anyhow/thiserror
- ✅ **Observability**: OpenTelemetry + tracing integration
- ✅ **Database**: sqlx/diesel patterns with connection pooling
- ✅ **Configuration**: Config validation with serde + envy
- ✅ **Health Checks**: Kubernetes-ready health endpoints
- ✅ **Testing**: Test utilities and fixtures
- ✅ **Security**: Basic auth guards and request validation
- ✅ **Docker**: Optimized multi-stage builds
- ✅ **CI/CD**: Automated testing and deployment pipelines

## Dependencies

**Required Packages:**
- `sector-base-rust@1.0.0` - Core Rust project scaffolding
- `sector-observability-8020@1.0.0` - Observability and monitoring
- `error-handling-patterns@1.0.0` - Error handling best practices

**Generated Dependencies:**
- `actix-web` or `axum` - Web framework
- `tokio` - Async runtime
- `serde`, `serde_json` - Serialization
- `sqlx` or `diesel` - Database access
- `tracing`, `tracing-subscriber` - Structured logging
- `opentelemetry` - Distributed tracing
- `config`, `envy` - Configuration management
- `anyhow`, `thiserror` - Error handling

## Success Metrics

**Immediate Benefits:**
- ✅ Working service in < 5 minutes
- ✅ All endpoints return proper HTTP status codes
- ✅ Comprehensive error handling out of the box
- ✅ Observability instrumented and ready for production

**Long-term Benefits:**
- 🎯 50% faster time-to-production for new services
- 🎯 Consistent architecture across all microservices
- 🎯 Reduced maintenance burden through standardization
- 🎯 Improved developer onboarding (familiar patterns)
- 🎯 Better operational insights (uniform observability)

---

*Part of the ggen 8020 Marketplace - Focusing on the 20% of features that solve 80% of problems*
