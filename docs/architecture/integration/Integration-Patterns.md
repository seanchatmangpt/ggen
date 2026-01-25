<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [TAI Integration Patterns](#tai-integration-patterns)
  - [Service-to-Service Communication (gRPC)](#service-to-service-communication-grpc)
    - [Synchronous gRPC Calls](#synchronous-grpc-calls)
    - [Circuit Breaker Protection](#circuit-breaker-protection)
  - [Asynchronous Communication (Cloud Pub/Sub)](#asynchronous-communication-cloud-pubsub)
    - [Event Publishing Pattern](#event-publishing-pattern)
    - [Subscription Handling](#subscription-handling)
  - [Request/Response with Event Sourcing](#requestresponse-with-event-sourcing)
  - [Caching and Cache Invalidation](#caching-and-cache-invalidation)
    - [Cache-Aside Pattern](#cache-aside-pattern)
    - [Cache Invalidation](#cache-invalidation)
  - [State Synchronization (Multi-Region)](#state-synchronization-multi-region)
  - [Dead Letter Queue (DLQ) Pattern](#dead-letter-queue-dlq-pattern)
  - [Transactional Outbox Pattern](#transactional-outbox-pattern)
  - [Fan-Out Pattern (Parallel Processing)](#fan-out-pattern-parallel-processing)
  - [API Gateway Pattern](#api-gateway-pattern)
  - [Monitoring Integration Points](#monitoring-integration-points)
  - [References](#references)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# TAI Integration Patterns

Communication patterns and integration approaches between TAI components.

## Service-to-Service Communication (gRPC)

### Synchronous gRPC Calls

Used for immediate request/response patterns:

```
Governor → Coordinator
  ProposePolicy() → RequestAction()
  Latency: <20ms p50, <100ms p99
```

**Pattern:**
1. Client makes gRPC call with timeout (10s default)
2. Server processes and returns receipt
3. Client checks receipt status
4. If error, client implements retry logic (3 retries, exponential backoff)

**Implementation:**
```rust
pub async fn propose_policy(policy: Policy) -> Result<Receipt> {
    let mut client = CoordinatorClient::connect("http://coordinator:50052").await?;

    let request = tonic::Request::new(policy);

    let response = tokio::time::timeout(
        Duration::from_secs(10),
        client.request_action(request)
    ).await??;

    Ok(response.into_inner())
}
```

### Circuit Breaker Protection

All gRPC calls protected by circuit breaker:

```
Healthy → 5 consecutive failures → Open (fail fast)
Open → 30 seconds → HalfOpen (test recovery)
HalfOpen → 2 consecutive successes → Healthy
```

**Failure Detection:**
- Status code: `Unavailable`, `Internal`, `Unknown`
- Timeout exceeded
- Connection refused

## Asynchronous Communication (Cloud Pub/Sub)

### Event Publishing Pattern

Services emit events for other services to consume:

```
Governor (publishes)
  ├── policy.proposed
  ├── policy.enforced
  └── policy.revoked
        ↓
Coordinator (subscribes)
  ├── Coordinates policy enforcement
  └── Emits actions
        ↓
Scheduler (subscribes)
  └── Schedules tasks

Audit logging service (subscribes to all)
  └── Stores in audit_logs collection
```

**Pub/Sub Configuration:**
- Topic: `tai-events`
- Subscription: per-service consumer group
- Delivery: At-least-once (handles duplicates)
- Retention: 7 days

**Implementation:**
```rust
pub async fn publish_event(event: DomainEvent) -> Result<String> {
    let publisher = PubsubClient::new()?
        .get_publisher(&format!("projects/{}/topics/tai-events", project_id))
        .await?;

    let message = PubsubMessage {
        data: serde_json::to_vec(&event)?,
        attributes: vec![
            ("event_type".into(), event.event_type.clone()),
            ("aggregate_id".into(), event.aggregate_id.clone()),
        ],
    };

    let message_id = publisher.publish(message).await?;
    Ok(message_id)
}
```

### Subscription Handling

```rust
pub async fn subscribe_to_events() -> Result<()> {
    let subscriber = PubsubClient::new()?
        .get_subscriber(&format!("projects/{}/subscriptions/coordinator-sub", project_id))
        .await?;

    loop {
        match subscriber.pull().await? {
            Some(messages) => {
                for message in messages {
                    // Process event
                    if let Ok(event) = serde_json::from_slice::<DomainEvent>(&message.data) {
                        handle_event(event).await?;
                        message.ack().await?;
                    }
                }
            }
            None => tokio::time::sleep(Duration::from_millis(100)).await,
        }
    }
}
```

## Request/Response with Event Sourcing

Combined synchronous + asynchronous pattern:

```
1. Client calls Governor.ProposePolicy(policy)
   ├── Governor validates
   ├── Governor stores policy
   ├── Governor publishes PolicyProposed event
   └── Governor returns Receipt immediately

2. Coordinator subscribes to PolicyProposed
   ├── Creates coordination task
   ├── Publishes CoordinationStarted event
   └── Coordinator acknowledges

3. Scheduler subscribes to CoordinationStarted
   ├── Creates enforcement task
   ├── Publishes TaskScheduled event
   └── Executes at scheduled time

4. All services log events to audit_logs
   └── Immutable record for compliance
```

## Caching and Cache Invalidation

### Cache-Aside Pattern

```
1. Client requests data
2. Check Redis cache
3. Cache hit → return immediately
4. Cache miss → fetch from Firestore
5. Store in cache (TTL: 5 minutes)
6. Return to client
```

**Implementation:**
```rust
pub async fn get_policy_with_cache(id: &str) -> Result<Policy> {
    // Try cache
    if let Some(cached) = redis.get(&format!("policy:{}", id)).await? {
        return Ok(serde_json::from_slice(&cached)?);
    }

    // Cache miss: fetch from DB
    let policy = firestore.get_policy(id).await?;

    // Store in cache (5 min TTL)
    redis.set_ex(
        &format!("policy:{}", id),
        serde_json::to_vec(&policy)?,
        300
    ).await?;

    Ok(policy)
}
```

### Cache Invalidation

When policy updates, invalidate cache via Pub/Sub:

```rust
pub async fn update_policy(policy: Policy) -> Result<()> {
    // Update in Firestore
    firestore.update_policy(&policy).await?;

    // Publish cache invalidation event
    pubsub.publish(&format!("policy:{} invalidated", policy.id)).await?;

    Ok(())
}

// All services subscribe to invalidation events
pub async fn handle_cache_invalidation(key: String) {
    redis.del(&key).await?;
}
```

## State Synchronization (Multi-Region)

Firestore handles cross-region replication automatically:

```
Region 1 (us-central1)
  ├── Write policy
  ├── Firestore replicates → Region 2
  └── Firestore replicates → Region 3
       ↓ (latency: <100ms)
Region 2 & 3 see consistent state
```

**Conflict Resolution:**
- Last-write-wins for simple cases
- Version vectors for causal consistency
- Manual resolution for concurrent updates

## Dead Letter Queue (DLQ) Pattern

Failed events go to DLQ for manual inspection:

```
Event Subscription
  ├── Process event
  ├── Commit offset (success)
  └── Send to DLQ (failure after 3 retries)
       ↓
DLQ Topic (tai-events-dlq)
  ├── Manual inspection
  ├── Fix underlying issue
  └── Replay from DLQ

DLQ Processing:
1. Alert ops team (PagerDuty)
2. Log error with full context
3. Store in Firestore (audit trail)
4. Await manual fix + replay
```

## Transactional Outbox Pattern

Ensure atomicity across Firestore writes and Pub/Sub publishes:

```rust
pub async fn propose_policy_transactional(policy: Policy) -> Result<Receipt> {
    let mut db = firestore.transaction().await?;

    // 1. Write policy document
    let policy_id = Uuid::new_v4().to_string();
    db.set(&format!("policies/{}", policy_id), policy.clone()).await?;

    // 2. Write outbox message (within same transaction)
    let event = DomainEvent {
        event_type: "PolicyProposed".into(),
        aggregate_id: policy_id.clone(),
        // ...
    };
    db.set(&format!("outbox/{}", event.event_id), event.clone()).await?;

    // 3. Commit transaction (atomic)
    db.commit().await?;

    // 4. Publish event (separate operation, handles failures)
    pubsub.publish(&event).await.ok(); // Fire and forget

    Ok(Receipt { /* ... */ })
}

// Background job processes outbox
pub async fn process_outbox() {
    loop {
        let events = firestore
            .select()
            .from("outbox")
            .obj()
            .await?;

        for event in events {
            // Publish event
            if pubsub.publish(&event).await.is_ok() {
                // Delete from outbox
                firestore.delete(&format!("outbox/{}", event.event_id)).await?;
            }
        }

        tokio::time::sleep(Duration::from_secs(1)).await;
    }
}
```

## Fan-Out Pattern (Parallel Processing)

Single event triggers multiple workflows:

```
PolicyEnforced event
  ├── → Notification service (send alerts)
  ├── → Audit service (log event)
  ├── → Analytics service (update metrics)
  └── → Workflow engine (trigger related tasks)
```

Each subscriber handles the event independently (no dependencies).

## API Gateway Pattern

All external requests go through API Gateway:

```
Client HTTP/JSON
  ↓
Envoy Ingress Gateway
  ├── TLS termination
  ├── Rate limiting
  ├── Authentication
  └── JSON ↔ gRPC transcoding
       ↓
Internal gRPC services
```

**Example (JSON to gRPC):**
```
POST /v1/policies/propose
Content-Type: application/json

{
  "policy_type": "security",
  "policy_name": "Enforce TLS",
  "rules": {"tls_version": "1.3"}
}
  ↓ (Envoy transcoding)
tai.Governor/ProposePolicy(Policy{...})
  ↓
receipt {
  "id": "rec-123",
  "status": "success"
}
```

## Monitoring Integration Points

All integration points emit metrics:

```
Governor → Coordinator
  └── istio_requests_total{source="governor",destination="coordinator"}
  └── istio_request_duration_milliseconds{...}
  └── istio_request_bytes{...}

Governor → Firestore
  └── firestore_read_latency_seconds
  └── firestore_write_latency_seconds
  └── firestore_read_count_total

Governor → Pub/Sub
  └── pubsub_publish_latency_seconds
  └── pubsub_publish_failures_total

Governor → Redis
  └── redis_operation_duration_seconds
  └── redis_cache_hit_ratio
```

## References
- Cloud Pub/Sub: https://cloud.google.com/pubsub/docs
- Firestore: https://firebase.google.com/docs/firestore
- gRPC: https://grpc.io/docs
- Event Sourcing: https://martinfowler.com/eaaDev/EventSourcing.html
