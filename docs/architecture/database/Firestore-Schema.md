# Firestore Database Schema

Complete Firestore collections structure for TAI system.

## Collections Overview

```
firestore/
├── policies/           # Policy documents (versioned)
├── signals/            # Control signals (time-series)
├── actions/            # Coordination actions
├── coordination_status/# Service coordination state
├── snapshots/          # Event source snapshots
└── audit_logs/         # Immutable audit trail
```

## Collections Detail

### policies Collection

**Purpose:** Store governance policies

**Document ID:** UUID (policy ID)

**Fields:**
```
{
  "id": "pol-abc123",
  "policy_type": "security",        // INDEX: non-clustered
  "policy_name": "Enforce TLS 1.3",
  "version": 1,
  "rules": {
    "tls_version": "1.3",
    "cert_validation": "strict"
  },
  "enabled": true,                  // INDEX: non-clustered
  "created_at_ns": 1642000000000000000,
  "updated_at_ns": 1642000000000000000,
  "created_by": "user@example.com",
  "last_modified_by": "admin@example.com",
  "ttl_seconds": null               // Optional TTL field
}
```

**Indexes:**
- `(policy_type, enabled)` composite
- `(created_at_ns)` single-field
- `(updated_at_ns)` single-field

**TTL:** Never (policies are permanent)

**Scalability:** 1 million+ policies per region

### signals Collection

**Purpose:** Store control signals for coordination

**Document ID:** UUID (signal ID)

**Fields:**
```
{
  "id": "sig-xyz789",
  "signal_type": "alert",           // INDEX: non-clustered
  "timestamp_ns": 1642000000000000000,
  "metadata": {
    "source": "monitoring",
    "severity": "warning"
  },
  "payload": [binary bytes],        // Optional
  "priority": 5,                    // 1-10, INDEX: non-clustered
  "status": "pending",              // INDEX: non-clustered
  "processed_at_ns": null,
  "ttl_seconds": 604800             // Expires in 7 days
}
```

**Indexes:**
- `(status, created_at_ns)` composite
- `(priority, timestamp_ns)` composite

**TTL:** 7 days (auto-delete old signals)

**Scalability:** 100 million+ signals per region

### actions Collection

**Purpose:** Store coordination actions

**Document ID:** UUID (action ID)

**Fields:**
```
{
  "id": "act-def456",
  "action_type": "scale",           // INDEX: non-clustered
  "timestamp_ns": 1642000000000000000,
  "parameters": {
    "service": "governor",
    "replicas": 10
  },
  "payload": [binary bytes],
  "source_service": "coordinator",
  "status": "pending",              // INDEX: non-clustered
  "acknowledged_at_ns": null,
  "completed_at_ns": null,
  "error_message": null,
  "ttl_seconds": 2592000            // Expires in 30 days
}
```

**Indexes:**
- `(status, timestamp_ns)` composite
- `(source_service, status)` composite

**TTL:** 30 days

**Scalability:** 10 million+ actions per region

### coordination_status Collection

**Purpose:** Current state of service coordination

**Document ID:** Service name (e.g., "governor", "coordinator", "scheduler")

**Fields:**
```
{
  "service_name": "governor",
  "status": "healthy",              // INDEX: non-clustered
  "timestamp_ns": 1642000000000000000,
  "pending_signals": 42,
  "pending_actions": 5,
  "last_signal_ns": 1642000000000000000,
  "healthy_replicas": 3,
  "total_replicas": 3,
  "error_rate": 0.001,
  "p99_latency_ms": 145.5
}
```

**Indexes:**
- `(status)` single-field
- `(timestamp_ns)` single-field

**Update Frequency:** Every 10 seconds per service

**Scalability:** Small (5-10 documents total)

### snapshots Collection

**Purpose:** Snapshots of aggregate state for event sourcing

**Document ID:** Aggregate ID (e.g., policy ID)

**Fields:**
```
{
  "aggregate_id": "pol-abc123",
  "aggregate_type": "Policy",
  "version": 47,                    // Last snapshot version
  "state": {
    "id": "pol-abc123",
    "policy_type": "security",
    "rules": {...}
  },
  "timestamp_ns": 1642000000000000000,
  "event_count": 47
}
```

**Indexes:** None (point lookups only)

**Retention:** Keep last 10 snapshots per aggregate

**Scalability:** 1 million+ snapshots

### audit_logs Collection

**Purpose:** Immutable audit trail (GDPR compliance)

**Document ID:** Composite key (timestamp_ns, sequence_number)

**Fields:**
```
{
  "event_id": "evt-unique-id",
  "timestamp_ns": 1642000000000000000,
  "user_id": "user@example.com",
  "action": "propose_policy",
  "resource_type": "Policy",
  "resource_id": "pol-abc123",
  "old_value": {...},
  "new_value": {...},
  "ip_address": "203.0.113.1",
  "user_agent": "Mozilla/5.0...",
  "status": "success",              // "success", "denied", "error"
  "error_message": null
}
```

**Indexes:**
- `(user_id, timestamp_ns)` composite
- `(action, timestamp_ns)` composite
- `(resource_type, resource_id)` composite

**Retention:** 7 years (GDPR requirement)

**Immutability:** No updates, append-only

**Scalability:** 1 billion+ entries over 7 years

## Entity Relationships

```
policies
  ├── Event: PolicyProposed
  │     ├── Snapshot: snapshots/{policy_id}
  │     └── Audit: audit_logs/{event_id}
  │
  ├── Event: PolicyEnforced
  │     ├── Action: actions/{action_id}
  │     └── Audit: audit_logs/{event_id}
  │
  └── Related: signals/{signal_id}
        └── Coordination: coordination_status/{service}

signals
  ├── Event: SignalEmitted
  │     └── Audit: audit_logs/{event_id}
  │
  └── Processing: actions/{action_id}
        └── Coordination: coordination_status/{service}
```

## Storage Estimation

| Collection | Avg Doc Size | Daily Docs | Monthly Docs | Storage (30d) |
|-----------|--------------|-----------|--------------|--------------|
| policies | 2 KB | 1k | 30k | 60 MB |
| signals | 1 KB | 1M | 30M | 30 GB |
| actions | 1.5 KB | 500k | 15M | 22.5 GB |
| coordination_status | 0.5 KB | 1k | 30k | 15 MB |
| snapshots | 5 KB | 100 | 3k | 15 MB |
| audit_logs | 1 KB | 2M | 60M | 60 GB |
| **Total** | | | | **112.6 GB** |

Cost estimate (us-central1): ~$450/month for reads + $300/month for writes

## Query Patterns

### Get Policy by ID
```
Direct lookup: db.collection('policies').doc('pol-abc123').get()
Cost: 1 read operation
```

### List Enabled Policies by Type
```
Query: db.collection('policies')
  .where('policy_type', '==', 'security')
  .where('enabled', '==', true)
  .orderBy('created_at_ns', 'desc')
Cost: Composite index scan
```

### Get Pending Signals
```
Query: db.collection('signals')
  .where('status', '==', 'pending')
  .where('priority', '>=', 7)
  .orderBy('priority', 'desc')
  .orderBy('timestamp_ns', 'asc')
  .limit(100)
Cost: Composite index + limit
```

### Stream Coordination Updates
```
Listener: db.collection('coordination_status')
  .where('status', '!=', 'healthy')
  .onSnapshot((docs) => {...})
Cost: Real-time listener fee (~0.05 per doc per day)
```

## Backup and Recovery

**Automatic Backups:**
- Daily backup to Cloud Storage
- 30-day retention
- Point-in-time recovery available

**Manual Backups:**
```bash
gcloud firestore export gs://backup-bucket/policies-backup-$(date +%Y%m%d)
```

**Export for Compliance:**
```bash
# Export audit_logs for audit
gcloud firestore export gs://audit-bucket/ \
  --collection-ids="audit_logs"
```

## Performance Tuning

1. **Indexing:** Only create indexes for frequently filtered fields
2. **Pagination:** Use startAt() with limit() for large result sets
3. **Caching:** Redis cache for hot data (policies, status)
4. **Denormalization:** Store related fields in documents to avoid joins

## References
- Firestore Documentation: https://firebase.google.com/docs/firestore
- Best Practices: https://firebase.google.com/docs/firestore/best-practices
