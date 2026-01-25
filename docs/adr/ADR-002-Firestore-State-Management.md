<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [ADR-002: Firestore for Distributed State Management](#adr-002-firestore-for-distributed-state-management)
  - [Problem Statement](#problem-statement)
  - [Decision](#decision)
  - [Rationale](#rationale)
    - [Firestore Strengths](#firestore-strengths)
    - [Alternative Evaluation](#alternative-evaluation)
  - [Data Model](#data-model)
    - [Collections Structure](#collections-structure)
    - [Indexing Strategy](#indexing-strategy)
  - [Implementation](#implementation)
    - [Firestore Rules (Security)](#firestore-rules-security)
    - [Rust Client Code](#rust-client-code)
  - [Cache Strategy](#cache-strategy)
    - [Redis Cache Layer](#redis-cache-layer)
    - [Cache-Aside Pattern](#cache-aside-pattern)
  - [Consequences](#consequences)
    - [Positive](#positive)
    - [Negative](#negative)
  - [Cost Optimization](#cost-optimization)
  - [Monitoring](#monitoring)
  - [Migration Path](#migration-path)
  - [References](#references)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# ADR-002: Firestore for Distributed State Management

**Status:** Accepted
**Date:** 2026-01-25
**Context:** Selecting distributed state store for policy, signal, and coordination data
**Deciders:** Data Architecture Team

## Problem Statement

TAI system needs to store mutable state for:
- Policies (governance rules, versioning)
- Signals (control signals with lifecycle)
- Actions (pending coordination tasks)
- Coordination state (service status, pending items)

Requirements:
- Multi-region consistency
- Real-time updates (pub/sub)
- ACID transactions
- Horizontal scalability
- Minimal operational overhead

## Decision

**Use Google Cloud Firestore (managed NoSQL) as primary state store.**

Additional: Redis cache layer for hot data, DLQ in Pub/Sub for failed events.

## Rationale

### Firestore Strengths
1. **Global Distribution:**
   - Multi-region replication out-of-box
   - Strong consistency within region, eventual globally
   - Built-in failover
   - Read replicas for scaling reads

2. **Transactional Guarantees:**
   - ACID transactions across multiple documents
   - Optimistic locking
   - Isolation levels appropriate for policies/signals
   - No distributed transaction complexity

3. **Scalability:**
   - Horizontal scaling: automatic sharding by document ID
   - No connection pooling needed (serverless)
   - Pay-per-read/write cost model
   - Supports high throughput (1M+ ops/sec)

4. **Real-time Features:**
   - Real-time listeners (push updates)
   - Cloud Pub/Sub integration
   - Change data capture for event sourcing
   - Query snapshots for reactive updates

5. **Operational:**
   - Zero maintenance (Google manages)
   - Built-in backups and point-in-time recovery
   - IAM integration with VPC Service Controls
   - Fine-grained security rules (Firestore Rules)

### Alternative Evaluation

| Store | Consistency | Transactions | Scaling | Ops Burden |
|-------|-------------|-------------|---------|-----------|
| **Firestore** | Strong (regional) | ACID | Automatic | Minimal |
| PostgreSQL | Strong | ACID | Manual sharding | High |
| DynamoDB | Eventually consistent | Limited | Auto | Medium |
| Spanner | Global consistency | ACID | Auto | High cost |
| Bigtable | Eventual | No | Excellent | High |

## Data Model

### Collections Structure
```
/policies
  {policyId}
    - policy_type (string)
    - policy_name (string)
    - rules (map)
    - version (int64)
    - enabled (boolean)
    - created_at (timestamp)
    - updated_at (timestamp)

/signals
  {signalId}
    - signal_type (string)
    - timestamp_ns (int64)
    - metadata (map)
    - payload (bytes)
    - priority (int32)
    - status (string): "pending", "processed", "failed"

/actions
  {actionId}
    - action_type (string)
    - source_service (string)
    - parameters (map)
    - status (string): "pending", "accepted", "failed"
    - created_at (timestamp)
    - updated_at (timestamp)

/coordination_status
  {serviceId}
    - service_name (string)
    - status (string): "healthy", "degraded", "unhealthy"
    - timestamp_ns (int64)
    - pending_signals (int32)
    - pending_actions (int32)
```

### Indexing Strategy
- Composite indexes on (status, created_at) for queries
- Ttl policy on signals collection (auto-delete old entries)
- Custom indexes for high-cardinality lookups

## Implementation

### Firestore Rules (Security)
```
rules_version = '2';
service cloud.firestore {
  match /databases/{database}/documents {
    // Policies: authenticated users can read, admins write
    match /policies/{policyId} {
      allow read: if request.auth != null;
      allow write: if request.auth.token.admin == true;
    }

    // Signals: services can write their own
    match /signals/{signalId} {
      allow read: if request.auth != null;
      allow create: if request.auth.uid != null;
      allow update: if resource.data.source_service == request.auth.uid;
    }

    // Coordination: internal service reads/writes
    match /coordination_status/{serviceId} {
      allow read, write: if request.auth.token.service == true;
    }
  }
}
```

### Rust Client Code
```rust
use firestore::*;

pub async fn store_policy(client: &FirestoreClient, policy: Policy) -> Result<String> {
    let doc_id = Uuid::new_v4().to_string();

    client
        .fluent()
        .update()
        .in_col("policies")
        .document_id(&doc_id)
        .set(serde_json::to_value(&policy)?)
        .execute()
        .await?;

    Ok(doc_id)
}

pub async fn get_policies(
    client: &FirestoreClient,
    policy_type: Option<&str>
) -> Result<Vec<Policy>> {
    let mut query = client.fluent().select().from("policies");

    if let Some(p_type) = policy_type {
        query = query.filter(|q| q.field_path("policy_type").eq(p_type));
    }

    let docs = query.obj().await?;
    Ok(docs)
}

// Real-time listener
pub async fn listen_signals(
    client: &FirestoreClient,
    mut callback: impl FnMut(Signal) + Send + 'static
) -> Result<ListenerHandle> {
    let listener = client
        .fluent()
        .select()
        .from("signals")
        .filter(|q| q.field_path("status").eq("pending"))
        .listen()
        .await?;

    // Handle real-time updates
    Ok(listener)
}
```

## Cache Strategy

### Redis Cache Layer
- Cache tier: Firestore ← Redis ← Client
- TTL: 5 minutes for policies, 1 minute for signals
- Invalidation: Pub/Sub messages trigger cache clear
- Keys: `policy:{id}`, `signal:{id}`, `actions:{service}`

### Cache-Aside Pattern
```rust
async fn get_policy_cached(id: &str) -> Result<Policy> {
    // Try cache first
    if let Some(policy) = redis.get(&format!("policy:{}", id)).await? {
        return Ok(policy);
    }

    // Fetch from Firestore
    let policy = firestore.get_policy(id).await?;

    // Store in cache
    redis.set(
        &format!("policy:{}", id),
        &policy,
        Duration::from_secs(300)
    ).await?;

    Ok(policy)
}
```

## Consequences

### Positive
- Global consistency for policy management
- Transactions ensure state integrity
- Real-time updates without polling
- Low operational overhead
- Excellent scalability story
- RBAC and audit logging built-in

### Negative
- Vendor lock-in (Google Cloud)
- Cost at high scale (pay-per-operation)
- Cold starts after idle periods
- Limited query capabilities (no joins, limited aggregations)
- Requires careful index design for performance

## Cost Optimization

1. **Caching:** Redis reduces Firestore reads by 80-90%
2. **Batch operations:** Bulk writes cheaper than individual ops
3. **TTL policies:** Auto-delete old signals (non-growing storage)
4. **Region selection:** Co-locate with services

## Monitoring

- Document read/write count per collection
- Query latency percentiles (p50, p95, p99)
- Index statistics and unused indexes
- Firestore Rules evaluation metrics

## Migration Path

1. **Phase 1:** Pilot Firestore for signals (high volume, auto-expire)
2. **Phase 2:** Migrate policies and actions
3. **Phase 3:** Implement Redis cache layer
4. **Phase 4:** Optimize indexes based on real workload

## References
- [Firestore Documentation](https://firebase.google.com/docs/firestore)
- [Firestore Best Practices](https://firebase.google.com/docs/firestore/best-practices)
- [Firestore Pricing Model](https://firebase.google.com/pricing)
- [Rust Firestore Client](https://github.com/goldenratio/firestore-rust)
