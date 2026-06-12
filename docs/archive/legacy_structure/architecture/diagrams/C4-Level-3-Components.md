<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [C4 Level 3: Component Diagram](#c4-level-3-component-diagram)
  - [Component Details](#component-details)
    - [API Layer](#api-layer)
      - [Handler](#handler)
      - [Middleware](#middleware)
    - [Business Logic](#business-logic)
      - [Policy Service](#policy-service)
      - [Validator](#validator)
      - [Policy Enforcer](#policy-enforcer)
    - [Data Access Layer](#data-access-layer)
      - [Repository Pattern](#repository-pattern)
      - [Cache Layer](#cache-layer)
      - [Firestore Client](#firestore-client)
    - [Infrastructure](#infrastructure)
      - [Logger (Structured Logging)](#logger-structured-logging)
      - [Metrics Emitter](#metrics-emitter)
      - [Vault Client](#vault-client)
  - [Request Flow (Example: ProposePolicy)](#request-flow-example-proposepolicy)
  - [Error Handling](#error-handling)
  - [Key Design Patterns](#key-design-patterns)
  - [Testing Strategy](#testing-strategy)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# C4 Level 3: Component Diagram

Internal structure of TAI services (using Governor as example).

```mermaid
graph TB
    subgraph "Governor Service"
        subgraph "API Layer"
            HANDLER["Handler (gRPC methods)"]
            MIDDLEWARE["Middleware (Auth, Rate Limit)"]
        end

        subgraph "Business Logic"
            POLICY_SVC["Policy Service"]
            VALIDATOR["Validator (Rules)"]
            ENFORCER["Policy Enforcer"]
        end

        subgraph "Data Access"
            REPO["Repository Pattern"]
            CACHE["Cache Layer"]
            FIRESTORE_CLIENT["Firestore Client"]
        end

        subgraph "Infrastructure"
            LOGGER["Logger (OpenTelemetry)"]
            METRICS["Metrics Emitter"]
            VAULT_CLIENT["Vault Client (Secrets)"]
        end
    end

    HANDLER -->|Decrypt token| MIDDLEWARE
    HANDLER -->|Forward request| POLICY_SVC
    POLICY_SVC -->|Validate rules| VALIDATOR
    POLICY_SVC -->|Get cached| CACHE
    CACHE -->|Miss, fetch| REPO
    REPO -->|Query| FIRESTORE_CLIENT
    REPO -->|Store in cache| CACHE
    POLICY_SVC -->|Enforce| ENFORCER
    ENFORCER -->|Get secret| VAULT_CLIENT
    HANDLER -->|Log operation| LOGGER
    POLICY_SVC -->|Emit metric| METRICS

    classDef api fill:#bbdefb,stroke:#1565c0
    classDef logic fill:#c8e6c9,stroke:#2e7d32
    classDef data fill:#f8bbd0,stroke:#c2185b
    classDef infra fill:#e1bee7,stroke:#7b1fa2

    class HANDLER,MIDDLEWARE api
    class POLICY_SVC,VALIDATOR,ENFORCER logic
    class REPO,CACHE,FIRESTORE_CLIENT data
    class LOGGER,METRICS,VAULT_CLIENT infra
```

## Component Details

### API Layer

#### Handler
```rust
#[tonic::async_trait]
impl Governor for GovernorService {
    async fn propose_policy(&self, request: Request<Policy>) -> Result<Response<Receipt>>;
    async fn enforce_policy(&self, request: Request<Policy>) -> Result<Response<Receipt>>;
    async fn get_policies(&self, request: Request<GetPoliciesRequest>) -> Result<Response<GetPoliciesResponse>>;
}
```

#### Middleware
- Authentication (mTLS certificate extraction)
- Rate limiting (token bucket)
- Request validation (proto validation)
- Span creation (OpenTelemetry)

### Business Logic

#### Policy Service
- Parse and validate policy
- Check authorization
- Store policy state
- Publish policy events

#### Validator
- Type checking (policy_type ∈ {finance, security, operational})
- Rule validation (SPARQL expressions)
- Constraint checking (SHACL shapes)

#### Policy Enforcer
- Apply policy rules
- Update system state
- Emit control signals
- Trigger workflows

### Data Access Layer

#### Repository Pattern
```rust
pub trait PolicyRepository {
    async fn create(&mut self, policy: Policy) -> Result<String>;
    async fn read(&self, id: &str) -> Result<Policy>;
    async fn update(&mut self, policy: Policy) -> Result<()>;
    async fn delete(&mut self, id: &str) -> Result<()>;
    async fn list(&self, filter: PolicyFilter) -> Result<Vec<Policy>>;
}
```

#### Cache Layer
- Key: `policy:{id}` → Serialized JSON
- TTL: 5 minutes
- Invalidation: Pub/Sub messages

#### Firestore Client
- Document queries with filters
- Transaction support
- Batch operations

### Infrastructure

#### Logger (Structured Logging)
```rust
#[tracing::instrument]
pub async fn propose_policy(policy: Policy) {
    tracing::info!(policy_id = %policy.id, "proposing policy");
    // Events automatically logged with context
}
```

#### Metrics Emitter
```rust
REQUEST_COUNT.inc();
REQUEST_DURATION.observe(elapsed);
POLICY_ENFORCEMENTS.inc();
```

#### Vault Client
- Fetch encryption keys
- Retrieve API credentials
- Handle token refresh

## Request Flow (Example: ProposePolicy)

```mermaid
sequenceDiagram
    title ProposePolicy Request Flow
    participant Client as gRPC Client
    participant Handler as Handler<br/>(gRPC methods)
    participant MW as Middleware<br/>(Auth, Rate Limit, OTEL)
    participant PS as Policy Service
    participant Val as Validator<br/>(Rules Engine)
    participant Cache as Cache Layer<br/>(Redis)
    participant Repo as Repository Pattern
    participant FS as Firestore Client
    participant Enf as Policy Enforcer
    participant Metrics as Metrics Emitter
    participant Log as Logger<br/>(OpenTelemetry)

    Client->>Handler: ProposePolicy request
    Handler->>MW: Decrypt token, authenticate
    MW-->>Handler: Auth OK, rate limit check
    Note over MW: mTLS cert extraction<br/>Rate limiting (token bucket)<br/>Span creation (OTEL)

    Handler->>PS: Forward request
    PS->>Val: Validate policy structure
    Val-->>PS: policy_type valid<br/>SPARQL rules valid
    Note over Val: Type checking<br/>SPARQL expressions<br/>SHACL shapes

    PS->>Cache: Try get related policies
    Cache-->>PS: Cache miss
    PS->>Repo: Query related policies
    Repo->>FS: Execute Firestore query
    FS-->>Repo: Documents
    Repo-->>PS: Related policies
    Note over Repo,FS: Document queries<br/>Transaction support

    PS->>Enf: Enforce policy (RBAC)
    Enf-->>PS: Policy state updated
    Note over Enf: Apply rules<br/>Update state<br/>Emit signals

    PS->>Metrics: Emit counters/histograms
    Note over Metrics: policy_enforcements<br/>latency histogram

    Handler->>Log: Log operation details
    Handler-->>Client: Receipt (success)
```

## Error Handling

```mermaid
sequenceDiagram
    title Error Handling Flow
    participant Client as gRPC Client
    participant Handler as Handler
    participant BL as Business Logic
    participant Log as Logger<br/>(error level)
    participant Metrics as Metrics<br/>(error counter)
    participant CB as Circuit Breaker<br/>(downstream deps)

    Client->>Handler: Request
    Handler->>BL: Process request
    BL--xHandler: Business Logic Exception

    Handler->>Log: Log error (structured)
    Note over Log: error level<br/>span context

    Handler->>Metrics: Increment error counter

    alt Downstream Service Failure
        Handler->>CB: Check circuit state
        CB-->>Handler: Circuit OPEN → Fail fast
    end

    Handler-->>Client: gRPC Status Code + error details

    Note over Client: Retry with<br/>exponential backoff
    Client->>Handler: Retry request
```

## Key Design Patterns

1. **Layered Architecture:** Separation of concerns (API, business logic, data access)
2. **Repository Pattern:** Data access abstraction
3. **Middleware:** Cross-cutting concerns (auth, rate limiting, logging)
4. **Circuit Breaker:** Fault tolerance for downstream services
5. **Cache-Aside:** Efficiency for frequently accessed data

## Testing Strategy

- **Unit Tests:** Service/validator logic with mocks
- **Integration Tests:** Service + real Firestore (testcontainers)
- **End-to-End Tests:** Full gRPC client → handler → service chain
