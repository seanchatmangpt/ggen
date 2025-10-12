# Development Workflow Guide

Complete development workflows for ggen projects following marketplace-first principles.

## Marketplace-First Development

### 1. Search & Discover
```bash
# Search for existing solutions first
ggen market search "rust web service"
ggen market search "authentication"
ggen market categories
```

### 2. Install Dependencies
```bash
# Install proven packages from marketplace
ggen market add "rust-axum-service"
ggen market add "postgresql-database"
ggen market add "docker-compose"
```

### 3. Initialize & Generate
```bash
# Initialize project structure
ggen lifecycle run init

# Generate using marketplace templates
ggen template generate rust-axum-service:user-service.tmpl
ggen template generate postgresql-database:schema.tmpl
```

### 4. Develop & Test
```bash
# Run comprehensive testing
ggen lifecycle run test

# Check production readiness
ggen lifecycle readiness
```

### 5. Deploy Safely
```bash
# Validate deployment requirements
ggen lifecycle validate --env production

# Deploy with confidence
ggen lifecycle run deploy --env production
```

## Complete Examples

### Microservices Project
```bash
cd examples/microservices-architecture/
ggen market search "microservices"
ggen market add "microservices-architecture"
ggen lifecycle run init
ggen template generate microservices-architecture:api-gateway.tmpl
ggen lifecycle run deploy --env production
```

### AI-Powered Service
```bash
cd examples/ai-code-generation/
ggen market search "ai service"
ggen market add "ai-service-templates"
ggen lifecycle run init
ggen ai generate "Create book management service"
ggen lifecycle run test
```

## Best Practices

1. **Search Before Building** - Always check marketplace first
2. **Use Proven Patterns** - Leverage marketplace packages
3. **Validate Early** - Use production readiness checks
4. **Deploy Safely** - Use lifecycle validation before deployment
