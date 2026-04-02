# Microservices Architecture Design

## System Overview

This architecture implements a distributed microservices system following Domain-Driven Design (DDD) principles with comprehensive observability, resilience patterns, and cloud-native deployment.

## Architectural Principles

### 1. Bounded Contexts

Each service owns its domain and data:

- **User Service**: User management, authentication, profile operations
- **Product Service**: Product catalog, inventory, search
- **Order Service**: Order processing, payment coordination, order lifecycle
- **API Gateway**: Request routing, authentication, rate limiting

### 2. Service Independence

Services are:
- Independently deployable
- Technology agnostic (polyglot architecture)
- Loosely coupled via well-defined APIs
- Fault-isolated with circuit breakers

### 3. Data Ownership

Each service owns its persistence:
- User Service → PostgreSQL (relational user data)
- Product Service → MongoDB (flexible product schemas)
- Order Service → PostgreSQL + RabbitMQ (transactional + events)

## Component Architecture

### API Gateway

**Technology:** Rust + Axum
**Port:** 8000

```rust
┌─────────────────────────────────────┐
│         API Gateway                 │
│                                     │
│  ┌────────────┐  ┌───────────────┐ │
│  │   Router   │  │ Load Balancer │ │
│  └────────────┘  └───────────────┘ │
│                                     │
│  ┌────────────┐  ┌───────────────┐ │
│  │  Circuit   │  │ Rate Limiter  │ │
│  │  Breaker   │  │               │ │
│  └────────────┘  └───────────────┘ │
│                                     │
│  ┌────────────────────────────────┐ │
│  │   OpenTelemetry Tracing        │ │
│  └────────────────────────────────┘ │
└─────────────────────────────────────┘
```

**Responsibilities:**
1. Request routing to downstream services
2. Load balancing across service instances
3. Circuit breaker protection
4. Authentication and authorization
5. Rate limiting and throttling
6. Request/response transformation
7. Distributed tracing propagation

**Circuit Breaker States:**
- **Closed**: Normal operation, all requests pass
- **Open**: Service failing, requests blocked for 60s
- **Half-Open**: Testing service recovery, limited requests

**Configuration:**
```rust
CircuitBreaker {
    failure_threshold: 5,        // Open after 5 failures
    timeout: Duration::from_secs(60),  // 60s cooldown
    half_open_requests: 3,       // Test with 3 requests
}
```

### User Service

**Technology:** TypeScript + Express
**Port:** 8001
**Database:** PostgreSQL

```typescript
┌─────────────────────────────────────┐
│        User Service                 │
│                                     │
│  ┌────────────┐  ┌───────────────┐ │
│  │    API     │  │  Authentication│ │
│  │  Handlers  │  │     (JWT)      │ │
│  └────────────┘  └───────────────┘ │
│                                     │
│  ┌────────────┐  ┌───────────────┐ │
│  │  Business  │  │   Data Access │ │
│  │   Logic    │  │     Layer     │ │
│  └────────────┘  └───────────────┘ │
│                                     │
│  ┌────────────────────────────────┐ │
│  │      PostgreSQL Schema         │ │
│  │  users (id, email, name, ...)  │ │
│  └────────────────────────────────┘ │
└─────────────────────────────────────┘
```

**Database Schema:**
```sql
CREATE TABLE users (
    id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
    email VARCHAR(255) UNIQUE NOT NULL,
    name VARCHAR(255) NOT NULL,
    password_hash VARCHAR(255) NOT NULL,
    created_at TIMESTAMP DEFAULT NOW(),
    updated_at TIMESTAMP DEFAULT NOW()
);

CREATE INDEX idx_users_email ON users(email);
```

**API Endpoints:**
- `GET /users` - List all users (paginated)
- `GET /users/:id` - Get user by ID
- `POST /users` - Create new user
- `PUT /users/:id` - Update user
- `DELETE /users/:id` - Delete user
- `POST /users/login` - Authenticate user

### Product Service

**Technology:** Python + FastAPI
**Port:** 8002
**Database:** MongoDB

```python
┌─────────────────────────────────────┐
│      Product Service                │
│                                     │
│  ┌────────────┐  ┌───────────────┐ │
│  │  FastAPI   │  │   Pydantic    │ │
│  │  Routers   │  │    Models     │ │
│  └────────────┘  └───────────────┘ │
│                                     │
│  ┌────────────┐  ┌───────────────┐ │
│  │  Inventory │  │    Search     │ │
│  │  Manager   │  │    Engine     │ │
│  └────────────┘  └───────────────┘ │
│                                     │
│  ┌────────────────────────────────┐ │
│  │      MongoDB Collections       │ │
│  │  products { id, name, price }  │ │
│  └────────────────────────────────┘ │
└─────────────────────────────────────┘
```

**MongoDB Schema:**
```json
{
  "_id": "ObjectId",
  "id": "string (product ID)",
  "name": "string",
  "description": "string",
  "price": "decimal",
  "stock": "integer",
  "category": "string",
  "tags": ["array of strings"],
  "images": ["array of URLs"],
  "created_at": "datetime",
  "updated_at": "datetime"
}
```

**API Endpoints:**
- `GET /products` - List products (filtering, pagination)
- `GET /products/:id` - Get product details
- `POST /products` - Create product
- `PUT /products/:id` - Update product
- `DELETE /products/:id` - Delete product
- `GET /products/search?q=...` - Search products

### Order Service

**Technology:** Rust + Axum
**Port:** 8003
**Database:** PostgreSQL + RabbitMQ

```rust
┌─────────────────────────────────────┐
│        Order Service                │
│                                     │
│  ┌────────────┐  ┌───────────────┐ │
│  │   Order    │  │   Payment     │ │
│  │  Handlers  │  │  Coordinator  │ │
│  └────────────┘  └───────────────┘ │
│                                     │
│  ┌────────────┐  ┌───────────────┐ │
│  │   Event    │  │   Saga        │ │
│  │ Publisher  │  │  Orchestrator │ │
│  └────────────┘  └───────────────┘ │
│                                     │
│  ┌────────────────────────────────┐ │
│  │   PostgreSQL + RabbitMQ        │ │
│  │  orders + order_events queue   │ │
│  └────────────────────────────────┘ │
└─────────────────────────────────────┘
```

**Database Schema:**
```sql
CREATE TABLE orders (
    id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
    user_id UUID NOT NULL,
    status VARCHAR(50) NOT NULL,
    total_amount DECIMAL(10,2) NOT NULL,
    created_at TIMESTAMP DEFAULT NOW(),
    updated_at TIMESTAMP DEFAULT NOW()
);

CREATE TABLE order_items (
    id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
    order_id UUID REFERENCES orders(id),
    product_id VARCHAR(255) NOT NULL,
    quantity INTEGER NOT NULL,
    price DECIMAL(10,2) NOT NULL
);
```

**Message Queue Events:**
- `order.created` - New order created
- `order.confirmed` - Payment confirmed
- `order.shipped` - Order shipped
- `order.completed` - Order completed
- `order.cancelled` - Order cancelled

## Communication Patterns

### Synchronous Communication (REST)

```
Client → API Gateway → User Service
                    → Product Service
                    → Order Service
```

**Characteristics:**
- Request-Response pattern
- HTTP/JSON over TCP
- Circuit breaker protection
- Timeout: 5 seconds
- Retry: 3 attempts with exponential backoff

### Asynchronous Communication (Message Queue)

```
Order Service → RabbitMQ → Inventory Service
                         → Notification Service
                         → Analytics Service
```

**Characteristics:**
- Publish-Subscribe pattern
- AMQP protocol
- At-least-once delivery
- Dead letter queue for failures

## Resilience Patterns

### 1. Circuit Breaker

Prevents cascading failures:

```rust
State: Closed → (5 failures) → Open → (60s timeout) → Half-Open → (3 successes) → Closed
```

**Implementation:**
```rust
struct CircuitBreaker {
    state: CircuitState,
    failure_count: u32,
    last_failure_time: Option<Instant>,
}

impl CircuitBreaker {
    fn can_attempt(&self) -> bool {
        match self.state {
            Closed => true,
            Open => self.timeout_expired(),
            HalfOpen => true,
        }
    }
}
```

### 2. Retry Policy

Exponential backoff:

```
Attempt 1: immediate
Attempt 2: 100ms delay
Attempt 3: 200ms delay
Attempt 4: 400ms delay (give up)
```

### 3. Bulkhead Isolation

Resource pools prevent thread exhaustion:

```rust
Connection Pool: 10 connections
Request Queue: 20 requests
Timeout: 5 seconds
```

### 4. Timeout Policy

All requests have timeouts:
- Connection timeout: 2 seconds
- Request timeout: 5 seconds
- Total timeout: 7 seconds maximum

## Distributed Tracing

### OpenTelemetry Integration

All services instrument requests with trace context:

```
Trace ID: 0af7651916cd43dd8448eb211c80319c
├── Span 1: API Gateway (200ms)
│   ├── Span 2: User Service (50ms)
│   │   └── Span 3: PostgreSQL Query (20ms)
│   └── Span 4: Order Service (100ms)
│       ├── Span 5: Product Service (30ms)
│       └── Span 6: RabbitMQ Publish (10ms)
```

**Trace Propagation:**
- W3C Trace Context headers
- `traceparent`: version-trace_id-span_id-flags
- `tracestate`: vendor-specific data

**Exporters:**
- Jaeger (primary)
- OTLP HTTP
- Console (development)

## Service Discovery

### DNS-based Discovery

Services resolve via Docker DNS:

```
user-service → 172.20.0.2:8001
product-service → 172.20.0.3:8002
order-service → 172.20.0.4:8003
```

**Health Checks:**
- Liveness: `/health/live` (service running?)
- Readiness: `/health/ready` (accepting traffic?)

### Consul Integration (Optional)

```
┌─────────────┐
│   Consul    │
│   Server    │
└──────┬──────┘
       │
   ┌───┴────┬────────┬────────┐
   │        │        │        │
┌──v──┐ ┌──v──┐ ┌──v──┐ ┌──v──┐
│User │ │Prod │ │Order│ │Gate │
│Svc  │ │Svc  │ │Svc  │ │way  │
└─────┘ └─────┘ └─────┘ └─────┘
```

## Load Balancing

### Round Robin (Default)

```
Request 1 → Instance A
Request 2 → Instance B
Request 3 → Instance C
Request 4 → Instance A (repeat)
```

### Least Connections

```
Instance A: 5 connections → selected
Instance B: 10 connections
Instance C: 8 connections
```

### Weighted Balancing

```
Instance A: weight 3 (75% traffic)
Instance B: weight 1 (25% traffic)
```

## Security Architecture

### Authentication Flow

```
1. Client → POST /users/login {email, password}
2. User Service → Verify credentials
3. User Service → Generate JWT token
4. Client → Store JWT token
5. Client → GET /orders (Authorization: Bearer <token>)
6. API Gateway → Validate JWT
7. API Gateway → Forward to Order Service
8. Order Service → Process request
```

### JWT Token Structure

```json
{
  "header": {
    "alg": "RS256",
    "typ": "JWT"
  },
  "payload": {
    "sub": "user-123",
    "email": "user@example.com",
    "exp": 1704067200,
    "iat": 1704063600
  }
}
```

## Data Consistency

### Eventual Consistency Pattern

```
1. Order Service → Create order (PENDING)
2. Order Service → Publish order.created event
3. Inventory Service → Reserve stock
4. Payment Service → Process payment
5. Order Service → Update order (CONFIRMED)
6. Order Service → Publish order.confirmed event
```

### Saga Pattern (Distributed Transactions)

```
Create Order Saga:
├── Step 1: Reserve inventory → Compensate: Release inventory
├── Step 2: Process payment → Compensate: Refund payment
└── Step 3: Create shipment → Compensate: Cancel shipment
```

## Deployment Architecture

### Docker Compose (Development)

```
docker-compose.yml
├── api-gateway
├── user-service
├── product-service
├── order-service
├── postgres
├── mongodb
├── rabbitmq
└── jaeger
```

### Kubernetes (Production)

```
Namespace: microservices
├── Deployments (3 replicas each)
│   ├── api-gateway
│   ├── user-service
│   ├── product-service
│   └── order-service
├── Services (ClusterIP)
├── Ingress (TLS termination)
├── StatefulSets (databases)
├── ConfigMaps (configuration)
└── Secrets (credentials)
```

## Performance Characteristics

### Latency Targets

- API Gateway routing: < 5ms
- User Service CRUD: < 50ms
- Product Service search: < 100ms
- Order Service creation: < 200ms
- End-to-end request: < 500ms

### Throughput Targets

- API Gateway: 10,000 req/s
- User Service: 5,000 req/s
- Product Service: 8,000 req/s
- Order Service: 3,000 req/s

### Scalability

- Horizontal scaling: 10+ replicas per service
- Database sharding: User ID-based partitioning
- Caching: Redis for hot data (optional)

## Monitoring and Observability

### Metrics Collection

```
Prometheus scrapes /metrics endpoints:
- Request rate (req/s)
- Error rate (errors/s)
- Latency (p50, p95, p99)
- Circuit breaker state
- Database connection pool
```

### Alerting Rules

```yaml
- alert: HighErrorRate
  expr: rate(http_requests_total{status=~"5.."}[5m]) > 0.05

- alert: CircuitBreakerOpen
  expr: circuit_breaker_state == 1

- alert: HighLatency
  expr: histogram_quantile(0.95, http_request_duration_seconds) > 1
```

## Design Decisions

### Why Rust for API Gateway and Order Service?

- Low latency requirements (< 5ms routing)
- Memory safety without garbage collection
- High concurrency with Tokio async runtime
- Zero-cost abstractions

### Why TypeScript for User Service?

- Rapid development with type safety
- Rich ecosystem (Express, TypeORM)
- Easy integration with authentication libraries
- Good developer experience

### Why Python for Product Service?

- FastAPI performance (comparable to Node.js)
- Excellent data processing libraries
- MongoDB integration (Motor async driver)
- Machine learning integration potential

### Why PostgreSQL for User/Order Services?

- ACID transactions required
- Relational data model fits domain
- JSON support for flexible schemas
- Battle-tested at scale

### Why MongoDB for Product Service?

- Flexible product schemas
- Fast read performance for catalog
- Document model matches domain
- Easy horizontal scaling

## Future Enhancements

1. **Service Mesh (Istio/Linkerd)**
   - mTLS between services
   - Advanced traffic management
   - Observability without code changes

2. **API Gateway Enhancements**
   - GraphQL support
   - WebSocket support
   - gRPC gateway

3. **Event Sourcing**
   - Audit trail for all changes
   - Replay events for debugging
   - CQRS pattern implementation

4. **Caching Layer**
   - Redis for hot data
   - CDN for static assets
   - Database query caching

5. **Advanced Monitoring**
   - Distributed profiling
   - Real user monitoring (RUM)
   - Synthetic monitoring

## Conclusion

This architecture provides a solid foundation for building scalable, resilient microservices systems with comprehensive observability and cloud-native deployment capabilities.
