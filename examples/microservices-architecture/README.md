# Microservices Architecture Example

This example demonstrates a complete microservices architecture built with Rust, showcasing all ggen features:

- **Lifecycle Management**: Complete make.toml workflow
- **AI Code Generation**: Template-based service generation
- **SPARQL/RDF Integration**: Domain modeling and querying
- **Advanced Rust Patterns**: Error handling, async/await, testing

## Architecture Overview

```
┌─────────────────┐    ┌─────────────────┐    ┌─────────────────┐
│   API Gateway   │    │  User Service   │    │ Product Service │
│   (Axum)        │◄──►│   (Actix)       │◄──►│   (Warp)        │
└─────────────────┘    └─────────────────┘    └─────────────────┘
         │                       │                       │
         ▼                       ▼                       ▼
┌─────────────────┐    ┌─────────────────┐    ┌─────────────────┐
│  Auth Service   │    │  Order Service  │    │  Payment Service│
│   (Tonic)       │    │   (Axum)        │    │   (Actix)       │
└─────────────────┘    └─────────────────┘    └─────────────────┘
```

## Services

1. **API Gateway** - Axum-based routing and load balancing
2. **User Service** - Actix-web user management
3. **Product Service** - Warp-based product catalog
4. **Auth Service** - Tonic gRPC authentication
5. **Order Service** - Axum order processing
6. **Payment Service** - Actix-web payment handling

## Features Demonstrated

### Lifecycle Management
- Complete make.toml configuration
- Multi-stage builds (dev, test, prod)
- Docker containerization
- Kubernetes deployment
- Health checks and monitoring

### AI Code Generation
- Service templates with AI-generated business logic
- Database schema generation
- API documentation generation
- Test case generation

### SPARQL/RDF Integration
- Domain model in Turtle format
- SPARQL queries for data analysis
- Graph-based service discovery
- Semantic API documentation

### Advanced Rust Patterns
- Error handling with thiserror
- Async/await with tokio
- Structured logging with tracing
- Configuration management
- Comprehensive testing

## Quick Start

```bash
# Generate the entire microservices architecture
ggen ai project -d "E-commerce microservices with user management, product catalog, orders, and payments" -n microservices-architecture --ollama

# Build all services
cargo make build

# Run tests
cargo make test

# Start development environment
cargo make dev

# Deploy to production
cargo make deploy
```

## Project Structure

```
microservices-architecture/
├── make.toml                 # Lifecycle configuration
├── ggen.toml                 # ggen project configuration
├── docker-compose.yml        # Development environment
├── k8s/                      # Kubernetes manifests
├── services/
│   ├── api-gateway/          # Axum API gateway
│   ├── user-service/         # Actix user management
│   ├── product-service/     # Warp product catalog
│   ├── auth-service/         # Tonic gRPC auth
│   ├── order-service/        # Axum order processing
│   └── payment-service/      # Actix payment handling
├── shared/
│   ├── models/               # Shared data models
│   ├── proto/                # gRPC definitions
│   └── utils/                # Common utilities
├── templates/                # ggen templates
├── data/                     # RDF domain models
└── docs/                     # Generated documentation
```

## Development Workflow

1. **Design**: Define domain model in RDF/Turtle
2. **Generate**: Use AI to generate service templates
3. **Develop**: Implement business logic
4. **Test**: Run comprehensive test suite
5. **Deploy**: Use lifecycle management for deployment

This example serves as a comprehensive reference for building production-ready microservices with Rust and ggen.
