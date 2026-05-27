# Microservices Architecture Template

Production-grade microservices architecture template with distributed tracing, circuit breakers, service discovery, and comprehensive observability.

## Overview

This template generates a complete microservices system with:

- **API Gateway** (Rust/Axum) - Request routing, load balancing, circuit breakers
- **User Service** (TypeScript/Express) - User management and authentication
- **Product Service** (Python/FastAPI) - Product catalog and inventory
- **Order Service** (Rust/Axum) - Order processing with message queues
- **Infrastructure** - PostgreSQL, MongoDB, RabbitMQ, Jaeger tracing

## Features

### Resilience Patterns
- **Circuit Breakers** - Prevent cascading failures (5 failure threshold, 60s timeout)
- **Retry Policies** - Exponential backoff (3 retries, 100ms-5s delay)
- **Bulkhead Isolation** - Resource pool separation
- **Timeout Policies** - 5s request timeout, 2s connection timeout

### Observability
- **Distributed Tracing** - OpenTelemetry + Jaeger
- **Health Checks** - Liveness and readiness probes
- **Metrics** - Prometheus-compatible endpoints
- **Service Discovery** - DNS-based with Consul/etcd support

### Communication
- **REST APIs** - HTTP/JSON for synchronous communication
- **Message Queues** - RabbitMQ for asynchronous events
- **gRPC Support** - Protocol Buffers for high-performance communication

## Architecture

```
┌─────────────┐
│   Clients   │
└──────┬──────┘
       │
       v
┌─────────────────────────────────────────┐
│         API Gateway (Rust/Axum)         │
│   Circuit Breaker | Load Balancer       │
│   Authentication  | Rate Limiting       │
└──────┬──────────────┬─────────────┬─────┘
       │              │             │
       v              v             v
┌────────────┐ ┌─────────────┐ ┌──────────────┐
│   User     │ │  Product    │ │    Order     │
│  Service   │ │  Service    │ │   Service    │
│ (TypeScript)│ │  (Python)   │ │   (Rust)     │
└─────┬──────┘ └──────┬──────┘ └──────┬───────┘
      │               │               │
      v               v               v
┌──────────┐   ┌──────────┐   ┌──────────────┐
│PostgreSQL│   │ MongoDB  │   │ PostgreSQL + │
│          │   │          │   │  RabbitMQ    │
└──────────┘   └──────────┘   └──────────────┘

All services report to Jaeger for distributed tracing
```

## Quick Start

### Prerequisites

- Docker and Docker Compose
- Rust 1.75+
- Node.js 20+
- Python 3.11+

### Generate Project

```bash
ggen project new my-microservices \
  --template microservices-architecture \
  --output ./my-microservices

cd my-microservices
```

### Run with Docker Compose

```bash
docker-compose up -d

# Wait for services to be healthy
docker-compose ps

# Access services
curl http://localhost:8000/health/ready  # API Gateway
curl http://localhost:8001/health/live   # User Service
curl http://localhost:8002/health/live   # Product Service
curl http://localhost:8003/health/live   # Order Service

# View traces
open http://localhost:16686  # Jaeger UI
```

### Deploy to Kubernetes

```bash
kubectl apply -f infrastructure/kubernetes/

# Check deployment status
kubectl get pods
kubectl get services

# Access API Gateway
kubectl port-forward service/api-gateway 8000:80
```

## API Examples

### Create User

```bash
curl -X POST http://localhost:8000/users \
  -H "Content-Type: application/json" \
  -d '{
    "email": "user@example.com",
    "name": "John Doe"
  }'
```

### Create Product

```bash
curl -X POST http://localhost:8000/products \
  -H "Content-Type: application/json" \
  -d '{
    "id": "prod-123",
    "name": "Widget",
    "price": 29.99,
    "stock": 100
  }'
```

### Create Order

```bash
curl -X POST http://localhost:8000/orders \
  -H "Content-Type: application/json" \
  -d '{
    "userId": "user-abc",
    "productId": "prod-123",
    "quantity": 2
  }'
```

## Service Details

### API Gateway (Port 8000)

**Responsibilities:**
- Request routing to downstream services
- Load balancing (round-robin)
- Circuit breaker protection
- Authentication and authorization
- Rate limiting
- Request/response transformation

**Endpoints:**
- `GET /health/live` - Liveness probe
- `GET /health/ready` - Readiness probe
- `GET /metrics` - Prometheus metrics
- `/*` - Proxied requests to services

**Circuit Breaker Configuration:**
- Failure threshold: 5 consecutive failures
- Timeout: 60 seconds
- Half-open test requests: 3

### User Service (Port 8001)

**Responsibilities:**
- User registration and management
- Authentication and session management
- User profile CRUD operations

**Technology:**
- TypeScript + Express
- PostgreSQL for persistence
- OpenTelemetry tracing

**Endpoints:**
- `GET /users` - List all users
- `GET /users/:id` - Get user by ID
- `POST /users` - Create new user
- `POST /users/login` - User authentication

### Product Service (Port 8002)

**Responsibilities:**
- Product catalog management
- Inventory tracking
- Product search and filtering

**Technology:**
- Python + FastAPI
- MongoDB for document storage
- Async/await with asyncio

**Endpoints:**
- `GET /products` - List products
- `GET /products/:id` - Get product details
- `POST /products` - Create product
- `GET /products/search` - Search products

### Order Service (Port 8003)

**Responsibilities:**
- Order creation and management
- Payment processing coordination
- Order status tracking
- Event publishing to message queue

**Technology:**
- Rust + Axum
- PostgreSQL for transactional data
- RabbitMQ for event streaming

**Endpoints:**
- `GET /orders` - List orders
- `GET /orders/:id` - Get order details
- `POST /orders` - Create new order
- `GET /orders/user/:userId` - User's orders

## Testing

### Chicago TDD Test Suite

Comprehensive integration tests using testcontainers:

```bash
cd tests/chicago_tdd
cargo test --all-features

# Run specific test category
cargo test test_circuit_breaker
cargo test test_distributed_tracing
cargo test test_service_integration
```

**Test Coverage:**
- ✅ Service integration with Docker Compose
- ✅ Inter-service communication
- ✅ Circuit breaker resilience
- ✅ Distributed tracing validation
- ✅ Health checks and readiness probes
- ✅ Load balancing
- ✅ Message queue integration

### Performance Testing

```bash
# Load test API Gateway
ab -n 10000 -c 100 http://localhost:8000/users

# Benchmark with wrk
wrk -t12 -c400 -d30s http://localhost:8000/products
```

## Monitoring

### Jaeger Tracing

Access Jaeger UI at `http://localhost:16686`

**Features:**
- End-to-end request tracing
- Service dependency graph
- Performance analysis
- Error tracking

### Metrics Collection

All services expose Prometheus metrics at `/metrics`:

```bash
# API Gateway metrics
curl http://localhost:8000/metrics

# User Service metrics
curl http://localhost:8001/metrics
```

### Health Monitoring

```bash
# Check all service health
docker-compose ps

# Individual health checks
curl http://localhost:8000/health/live
curl http://localhost:8000/health/ready
```

## Configuration

### Environment Variables

**API Gateway:**
- `USER_SERVICE_URL` - User service endpoint
- `PRODUCT_SERVICE_URL` - Product service endpoint
- `ORDER_SERVICE_URL` - Order service endpoint
- `JAEGER_AGENT_HOST` - Jaeger agent hostname

**User Service:**
- `DATABASE_URL` - PostgreSQL connection string
- `JAEGER_AGENT_HOST` - Jaeger agent hostname
- `PORT` - Service port (default: 8001)

**Product Service:**
- `MONGO_URL` - MongoDB connection string
- `JAEGER_AGENT_HOST` - Jaeger agent hostname
- `PORT` - Service port (default: 8002)

**Order Service:**
- `DATABASE_URL` - PostgreSQL connection string
- `RABBITMQ_URL` - RabbitMQ connection string
- `JAEGER_AGENT_HOST` - Jaeger agent hostname
- `PORT` - Service port (default: 8003)

## Customization

### Adding New Services

1. Define service in `ontology/microservices.ttl`:

```turtle
ms:PaymentService a ms:BusinessService ;
    ggen:name "payment-service" ;
    ggen:language "Go" ;
    ggen:framework "Gin" ;
    ggen:port "8004"^^xsd:integer .
```

2. Add service dependency:

```turtle
ms:OrderToPayment a ms:ServiceDependency ;
    ggen:from ms:OrderService ;
    ggen:to ms:PaymentService ;
    ggen:pattern ms:gRPCCommunication .
```

3. Generate code:

```bash
ggen project gen --service payment-service
```

### Modifying Communication Patterns

Switch from REST to gRPC:

```turtle
ms:UserService ggen:hasCommunication ms:gRPCCommunication .
```

Add message queue:

```turtle
ms:UserService ggen:hasMessaging ms:KafkaCommunication .
```

## Production Deployment

### Kubernetes Production Checklist

- [ ] Configure resource limits and requests
- [ ] Set up horizontal pod autoscaling
- [ ] Configure persistent volumes for databases
- [ ] Set up ingress with TLS
- [ ] Configure service mesh (Istio/Linkerd)
- [ ] Set up centralized logging (ELK/Loki)
- [ ] Configure monitoring (Prometheus/Grafana)
- [ ] Set up secrets management (Vault)
- [ ] Configure network policies
- [ ] Set up backup and disaster recovery

### Security Best Practices

- Use mutual TLS between services
- Implement API authentication (JWT/OAuth2)
- Enable rate limiting on API Gateway
- Use secret management for credentials
- Regular security scanning of container images
- Implement RBAC for Kubernetes

## Troubleshooting

### Circuit Breaker Issues

If circuit breaker is stuck open:

```bash
# Check failure count
curl http://localhost:8000/metrics | grep circuit_breaker

# Reset by restarting gateway
docker-compose restart api-gateway
```

### Tracing Not Working

1. Check Jaeger is running:
   ```bash
   docker-compose ps jaeger
   curl http://localhost:16686
   ```

2. Verify services are sending traces:
   ```bash
   curl http://localhost:14269/metrics
   ```

3. Check service configuration:
   ```bash
   docker-compose logs user-service | grep jaeger
   ```

### Service Discovery Issues

```bash
# Check DNS resolution
docker-compose exec api-gateway nslookup user-service

# Verify network connectivity
docker-compose exec api-gateway ping user-service
```

## License

MIT License - See LICENSE file for details

## Support

- Documentation: https://ggen.tech/docs/templates/microservices
- Issues: https://github.com/ggen/marketplace/issues
- Community: https://discord.gg/ggen
