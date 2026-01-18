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

## Quick Start - Marketplace-First Workflow

### 1. **Search Marketplace** for existing microservices patterns
```bash
# Search for microservices and web service packages
ggen market search "microservices"
ggen market search "rust web service"
ggen market search "docker compose"
ggen market search "postgresql"

# Browse available categories
ggen market categories
```

### 2. **Install Required Packages** from marketplace
```bash
# Install core packages for microservices architecture
ggen market add "microservices-architecture"    # Base microservices patterns
ggen market add "rust-axum-service"             # Axum web framework
ggen market add "rust-actix-service"            # Actix web framework
ggen market add "rust-warp-service"             # Warp web framework
ggen market add "rust-tonic-service"            # gRPC services
ggen market add "postgresql-database"           # Database integration
ggen market add "redis-cache"                   # Caching layer
ggen market add "docker-compose"                # Container orchestration
ggen market add "kubernetes-deployment"         # K8s manifests
```

### 3. **Initialize Project** using lifecycle management
```bash
# Initialize project structure with marketplace packages
ggen lifecycle run init

# The init phase automatically:
# - Sets up project structure
# - Installs marketplace dependencies
# - Configures build system
# - Creates initial templates
```

### 4. **Generate Services** using marketplace templates
```bash
# Generate API gateway using marketplace template
ggen template generate microservices-architecture:api-gateway.tmpl

# Generate user service
ggen template generate rust-axum-service:user-service.tmpl

# Generate product service
ggen template generate rust-warp-service:product-service.tmpl

# Generate auth service (gRPC)
ggen template generate rust-tonic-service:auth-service.tmpl

# Generate order service
ggen template generate rust-axum-service:order-service.tmpl

# Generate payment service
ggen template generate rust-actix-service:payment-service.tmpl
```

### 5. **Generate Infrastructure** using marketplace packages
```bash
# Generate database schema
ggen template generate postgresql-database:schema.tmpl

# Generate Docker Compose configuration
ggen template generate docker-compose:development.tmpl

# Generate Kubernetes manifests
ggen template generate kubernetes-deployment:k8s-manifests.tmpl

# Generate Redis configuration
ggen template generate redis-cache:config.tmpl
```

### 6. **Build, Test & Deploy** using lifecycle management
```bash
# Build all services
ggen lifecycle run build

# Run comprehensive tests
ggen lifecycle run test

# Start development environment
ggen lifecycle run dev

# Deploy to production
ggen lifecycle run deploy --env production
```

### Alternative: Complete Pipeline
```bash
# Run all phases in sequence
ggen lifecycle pipeline "init setup generate build test deploy"
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

## Development Workflow - Marketplace-First Approach

### 1. **Design Phase** - Define domain model in RDF/Turtle
```bash
# Create domain model using marketplace patterns
ggen market add "domain-modeling"

# Generate domain model template
ggen template generate domain-modeling:ecommerce-domain.tmpl

# Edit data/domain.ttl with your specific business requirements
```

### 2. **Package Discovery** - Find and install required components
```bash
# Search for all required packages
ggen market search "rust web framework"
ggen market search "database"
ggen market search "authentication"
ggen market search "docker"

# Install packages based on search results
ggen market add "rust-axum-service"
ggen market add "postgresql-database"
ggen market add "jwt-authentication"
ggen market add "docker-compose"
```

### 3. **Service Generation** - Generate services using marketplace templates
```bash
# Generate each microservice using marketplace templates
ggen template generate rust-axum-service:user-service.tmpl
ggen template generate rust-actix-service:product-service.tmpl
ggen template generate rust-tonic-service:auth-service.tmpl
ggen template generate rust-axum-service:order-service.tmpl
ggen template generate rust-actix-service:payment-service.tmpl

# Generate API gateway
ggen template generate microservices-architecture:api-gateway.tmpl
```

### 4. **Infrastructure Generation** - Set up deployment and operations
```bash
# Generate database schema and migrations
ggen template generate postgresql-database:schema.tmpl

# Generate Docker configuration
ggen template generate docker-compose:development.tmpl

# Generate Kubernetes deployment
ggen template generate kubernetes-deployment:k8s-manifests.tmpl

# Generate monitoring and logging
ggen template generate monitoring-stack:prometheus-grafana.tmpl
```

### 5. **Customization** - Add business logic to generated code
```bash
# Edit generated services to add your business logic
# - Update API handlers in services/
# - Customize database models in shared/
# - Modify configuration in config/
# - Add custom middleware if needed
```

### 6. **Testing** - Comprehensive test execution
```bash
# Run all tests using lifecycle management
ggen lifecycle run test

# This runs:
# - Unit tests for each service
# - Integration tests between services
# - End-to-end API tests
# - Database migration tests
# - Security vulnerability scans
```

### 7. **Deployment** - Multi-environment deployment
```bash
# Deploy to development environment
ggen lifecycle run deploy --env development

# Deploy to staging for testing
ggen lifecycle run deploy --env staging

# Deploy to production
ggen lifecycle run deploy --env production
```

## Benefits of Marketplace-First Approach

### **Speed** - 80% less boilerplate code
- Marketplace provides proven, production-ready templates
- No need to write authentication, database, logging from scratch
- Focus on business logic, not infrastructure

### **Quality** - Production-ready patterns
- Templates include proper error handling, security, logging
- Follow core team best practices
- Comprehensive test coverage included

### **Consistency** - Standardized architecture
- All services follow same patterns
- Uniform error handling and API design
- Consistent deployment and monitoring

### **Maintainability** - Easier updates and refactoring
- Update marketplace packages for new features
- Consistent patterns across all services
- Automated testing and deployment

This example demonstrates how to build production-ready microservices using ggen's marketplace and lifecycle features, rather than starting from scratch.
