# Microservices Design Patterns

## Overview

This document explains the architectural patterns and design decisions implemented in this microservices template.

## 1. API Gateway Pattern

### Purpose
Single entry point for all client requests, handling cross-cutting concerns before routing to backend services.

### Implementation

```rust
// Circuit breaker state machine
enum CircuitState {
    Closed,   // Normal operation
    Open,     // Failing, blocking requests
    HalfOpen, // Testing recovery
}

// Gateway routes requests with protection
async fn proxy_request(target: &str) -> Response {
    if !circuit_breaker.can_attempt() {
        return ServiceUnavailable("Circuit breaker open");
    }

    match http_client.get(target).await {
        Ok(response) => {
            circuit_breaker.record_success();
            response
        }
        Err(error) => {
            circuit_breaker.record_failure();
            BadGateway("Service unavailable")
        }
    }
}
```

### Benefits
- Centralized authentication and authorization
- Rate limiting and throttling
- Request/response transformation
- Load balancing across service instances
- Protocol translation (REST to gRPC)
- Circuit breaker protection

### Trade-offs
- Single point of failure (mitigate with HA)
- Potential bottleneck (scale horizontally)
- Added latency (minimize with optimization)

## 2. Circuit Breaker Pattern

### Purpose
Prevent cascading failures by stopping requests to failing services.

### State Transitions

```
Closed (Normal)
    │
    │ 5 consecutive failures
    ▼
Open (Failing)
    │
    │ 60 second timeout
    ▼
Half-Open (Testing)
    │
    ├─ 3 successes ──> Closed
    └─ 1 failure ────> Open
```

### Configuration

```rust
struct CircuitBreaker {
    failure_threshold: u32,      // 5 failures
    timeout: Duration,           // 60 seconds
    half_open_requests: u32,     // 3 test requests
}
```

### When to Use
- External service calls
- Database connections
- Message queue operations
- Any remote I/O operation

### Example

```rust
async fn call_user_service(user_id: &str) -> Result<User> {
    let mut cb = circuit_breaker.write().await;

    if !cb.can_attempt() {
        return Err(Error::CircuitOpen);
    }

    match user_service.get_user(user_id).await {
        Ok(user) => {
            cb.record_success();
            Ok(user)
        }
        Err(e) => {
            cb.record_failure();
            Err(e)
        }
    }
}
```

## 3. Retry Pattern with Exponential Backoff

### Purpose
Handle transient failures by automatically retrying with increasing delays.

### Implementation

```rust
async fn retry_with_backoff<F, T>(
    operation: F,
    max_retries: u32,
) -> Result<T>
where
    F: Fn() -> Future<Output = Result<T>>,
{
    let mut delay = Duration::from_millis(100);
    let backoff_multiplier = 2.0;

    for attempt in 0..max_retries {
        match operation().await {
            Ok(result) => return Ok(result),
            Err(e) if attempt < max_retries - 1 => {
                sleep(delay).await;
                delay = delay.mul_f32(backoff_multiplier);
            }
            Err(e) => return Err(e),
        }
    }
}
```

### Retry Schedule

```
Attempt 1: Immediate
Attempt 2: 100ms delay
Attempt 3: 200ms delay
Attempt 4: 400ms delay
Max attempts: 4
Total time: ~700ms
```

### Best Practices
- Only retry idempotent operations
- Set maximum retry count (3-5)
- Use exponential backoff to prevent thundering herd
- Add jitter to spread retries
- Fail fast on non-retriable errors

## 4. Bulkhead Pattern

### Purpose
Isolate resources to prevent complete system failure when one component fails.

### Resource Pools

```rust
struct BulkheadPool {
    max_connections: usize,     // 10
    max_queue_size: usize,      // 20
    timeout: Duration,          // 5 seconds
}

// Separate pools for different services
let user_service_pool = BulkheadPool::new(10, 20);
let product_service_pool = BulkheadPool::new(10, 20);
let order_service_pool = BulkheadPool::new(10, 20);
```

### Benefits
- Fault isolation between services
- Prevents thread exhaustion
- Limits blast radius of failures
- Better resource utilization

### Implementation

```rust
async fn execute_with_bulkhead<F, T>(
    pool: &BulkheadPool,
    operation: F,
) -> Result<T> {
    // Acquire permit from pool
    let permit = pool.acquire().await?;

    // Execute operation
    let result = operation().await;

    // Release permit
    drop(permit);

    result
}
```

## 5. Service Discovery Pattern

### DNS-based Discovery

**Docker Compose:**
```yaml
services:
  api-gateway:
    environment:
      - USER_SERVICE_URL=http://user-service:8001
```

**Resolution:**
```
user-service → 172.20.0.2:8001 (Docker DNS)
```

### Consul Discovery (Advanced)

```rust
// Register service with Consul
async fn register_service() {
    consul.register(ServiceRegistration {
        id: "user-service-1",
        name: "user-service",
        address: "172.20.0.2",
        port: 8001,
        health_check: HealthCheck {
            http: "http://172.20.0.2:8001/health",
            interval: "10s",
        },
    }).await;
}

// Discover service instances
async fn discover_service(name: &str) -> Vec<ServiceInstance> {
    consul.health_service(name, true).await
}
```

## 6. Distributed Tracing Pattern

### W3C Trace Context Propagation

```http
GET /users/123 HTTP/1.1
traceparent: 00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01
```

**Format:**
- `00`: Version
- `0af7651916cd43dd8448eb211c80319c`: Trace ID (128-bit)
- `b7ad6b7169203331`: Parent Span ID (64-bit)
- `01`: Flags (sampled)

### Span Creation

```rust
use opentelemetry::trace::Tracer;

async fn create_order(order: Order) {
    let tracer = global::tracer("order-service");
    let mut span = tracer.start("create-order");

    // Add attributes
    span.set_attribute("order.id", order.id);
    span.set_attribute("order.total", order.total);
    span.set_attribute("user.id", order.user_id);

    // Child span for database operation
    let mut db_span = tracer.start("insert-order-db");
    let result = db.insert_order(&order).await;
    db_span.end();

    if result.is_err() {
        span.set_attribute("error", true);
    }

    span.end();
}
```

## 7. Saga Pattern (Distributed Transactions)

### Purpose
Manage data consistency across services without distributed transactions.

### Choreography-based Saga

```
1. Order Service → Create order (PENDING)
                 → Publish OrderCreated event

2. Inventory Service → Reserve stock
                     → Publish StockReserved event

3. Payment Service → Process payment
                   → Publish PaymentProcessed event

4. Order Service → Update order (CONFIRMED)
                 → Publish OrderConfirmed event
```

### Orchestration-based Saga

```rust
struct CreateOrderSaga {
    steps: Vec<SagaStep>,
}

async fn execute_saga(saga: CreateOrderSaga, order: Order) {
    let mut completed_steps = Vec::new();

    for step in saga.steps {
        match step.execute(&order).await {
            Ok(_) => completed_steps.push(step),
            Err(e) => {
                // Compensate completed steps in reverse order
                for step in completed_steps.iter().rev() {
                    step.compensate(&order).await;
                }
                return Err(e);
            }
        }
    }
}
```

### Saga Steps

```rust
trait SagaStep {
    async fn execute(&self, order: &Order) -> Result<()>;
    async fn compensate(&self, order: &Order) -> Result<()>;
}

struct ReserveInventoryStep;
impl SagaStep for ReserveInventoryStep {
    async fn execute(&self, order: &Order) -> Result<()> {
        inventory_service.reserve(order.product_id, order.quantity).await
    }

    async fn compensate(&self, order: &Order) -> Result<()> {
        inventory_service.release(order.product_id, order.quantity).await
    }
}
```

## 8. Event-Driven Architecture

### Publish-Subscribe Pattern

```rust
// Publisher (Order Service)
async fn create_order(order: Order) {
    // Save to database
    db.insert_order(&order).await?;

    // Publish event
    let event = OrderCreatedEvent {
        order_id: order.id,
        user_id: order.user_id,
        total: order.total,
        timestamp: Utc::now(),
    };

    rabbitmq.publish("orders", "order.created", event).await?;
}

// Subscriber (Notification Service)
async fn handle_order_created(event: OrderCreatedEvent) {
    let user = user_service.get_user(&event.user_id).await?;
    email_service.send_order_confirmation(&user.email, &event).await?;
}
```

### Event Sourcing (Advanced)

```rust
// Instead of storing current state, store all events
struct OrderEvents {
    events: Vec<OrderEvent>,
}

enum OrderEvent {
    Created { order_id: String, total: f64 },
    ItemAdded { product_id: String, quantity: u32 },
    PaymentProcessed { transaction_id: String },
    Shipped { tracking_number: String },
    Completed,
}

// Rebuild state by replaying events
fn rebuild_order(events: &[OrderEvent]) -> Order {
    let mut order = Order::default();
    for event in events {
        order.apply(event);
    }
    order
}
```

## 9. CQRS (Command Query Responsibility Segregation)

### Separate Read and Write Models

```rust
// Write Model (Commands)
trait OrderCommands {
    async fn create_order(&self, order: Order) -> Result<OrderId>;
    async fn update_order(&self, id: OrderId, updates: OrderUpdates) -> Result<()>;
    async fn cancel_order(&self, id: OrderId) -> Result<()>;
}

// Read Model (Queries)
trait OrderQueries {
    async fn get_order(&self, id: OrderId) -> Result<OrderView>;
    async fn list_user_orders(&self, user_id: UserId) -> Result<Vec<OrderView>>;
    async fn search_orders(&self, criteria: SearchCriteria) -> Result<Vec<OrderView>>;
}
```

### Benefits
- Optimize reads and writes independently
- Different data models for queries (denormalized)
- Scale read and write sides separately
- Complex queries without impacting write performance

## 10. Database per Service Pattern

### Independent Databases

```
User Service → PostgreSQL (users database)
Product Service → MongoDB (products database)
Order Service → PostgreSQL (orders database)
```

### Benefits
- Loose coupling between services
- Independent scaling and optimization
- Technology choice per service
- Isolated schema changes

### Challenges
- No foreign key constraints across services
- Eventual consistency required
- Complex queries across services
- Data duplication acceptable

### Managing Consistency

```rust
// Use eventual consistency with events
async fn create_order(order: Order) {
    // 1. Verify user exists (call User Service)
    let user = user_service.get_user(&order.user_id).await?;

    // 2. Verify product exists (call Product Service)
    let product = product_service.get_product(&order.product_id).await?;

    // 3. Create order in own database
    order_db.insert_order(&order).await?;

    // 4. Publish event for other services
    event_bus.publish(OrderCreated { order }).await?;
}
```

## 11. Health Check Pattern

### Liveness vs Readiness

**Liveness:**
- Is the service running?
- Can it be restarted to fix issues?
- Simple check (HTTP 200)

**Readiness:**
- Can the service handle traffic?
- Are dependencies available?
- More complex check (database, cache, etc.)

### Implementation

```rust
// Liveness - simple check
async fn health_live() -> &'static str {
    "alive"
}

// Readiness - check dependencies
async fn health_ready(db: &DbPool) -> Result<&'static str> {
    // Check database
    db.query("SELECT 1").await?;

    // Check message queue
    rabbitmq.ping().await?;

    Ok("ready")
}
```

## 12. Sidecar Pattern

### Purpose
Deploy helper processes alongside main service for cross-cutting concerns.

### Examples

**Logging Sidecar:**
```yaml
# Kubernetes pod with main container + sidecar
spec:
  containers:
    - name: user-service
      image: user-service:latest

    - name: log-shipper
      image: fluent-bit:latest
      volumeMounts:
        - name: logs
          mountPath: /var/log
```

**Proxy Sidecar (Service Mesh):**
```yaml
# Istio automatically injects Envoy sidecar
spec:
  containers:
    - name: user-service
      image: user-service:latest

    - name: istio-proxy  # Injected automatically
      image: istio/proxyv2:latest
```

## Design Decision Matrix

| Pattern | Use When | Avoid When |
|---------|----------|------------|
| API Gateway | Multiple clients, cross-cutting concerns | Simple single-client system |
| Circuit Breaker | External dependencies | Internal function calls |
| Retry | Transient failures | Non-idempotent operations |
| Bulkhead | Resource protection | Unlimited resources |
| Saga | Multi-service transactions | Single database operations |
| Event-Driven | Loose coupling, async | Immediate consistency required |
| CQRS | Complex queries, high read/write ratio | Simple CRUD |
| Database per Service | Service independence | Strong consistency needed |

## Anti-Patterns to Avoid

### 1. Distributed Monolith
- Services share database
- Tight coupling via synchronous calls
- Coordinated deployments required

**Solution:** Use database per service, async messaging

### 2. Chatty Services
- Too many small HTTP requests
- N+1 query problem across services

**Solution:** Batch operations, use events, denormalize data

### 3. Mega Service
- Service does too much
- Multiple bounded contexts in one service

**Solution:** Split into smaller services with clear boundaries

### 4. Timeout Cascades
- No timeouts configured
- Requests wait indefinitely

**Solution:** Set aggressive timeouts, use circuit breakers

## Conclusion

These patterns provide proven solutions to common microservices challenges. Choose patterns based on your specific requirements and constraints.
