# FactoryPaaS System Architecture

**Version**: 1.0.0
**Last Updated**: 2026-01-24
**Audience**: System Architects, Senior Engineers

---

## 📐 Architecture Overview

FactoryPaaS implements a **Domain-Driven Design** (DDD) architecture with **CQRS** (Command Query Responsibility Segregation) and **Event Sourcing** patterns, all generated deterministically from RDF ontology.

### Core Architectural Principles

1. **Ontology as Truth**: RDF ontology is the single source of truth
2. **Sealed Source**: Generated code is immutable (no manual edits)
3. **Deterministic Generation**: Same ontology → same code, always
4. **Event Sourcing**: All state changes captured as immutable events
5. **CQRS**: Separate command (write) and query (read) paths
6. **Cryptographic Receipts**: Every operation produces verifiable proof

---

## 🏗️ System Layers

```
┌─────────────────────────────────────────────────────────────┐
│ ONTOLOGY LAYER (Truth)                                      │
│ - RDF/Turtle definitions                                    │
│ - Domain model, commands, events, policies                  │
└─────────────────────┬───────────────────────────────────────┘
                      ↓ ggen sync (μ₁-μ₅)
┌─────────────────────────────────────────────────────────────┐
│ APPLICATION LAYER (Generated Rust Code)                     │
│ - Domain entities, aggregates                               │
│ - Command handlers, event handlers                          │
│ - HTTP routes (Axum framework)                              │
└─────────────────────┬───────────────────────────────────────┘
                      ↓
┌─────────────────────────────────────────────────────────────┐
│ INFRASTRUCTURE LAYER (GCP Resources)                        │
│ - Compute Engine (API servers)                              │
│ - Cloud SQL PostgreSQL (event store + read models)          │
│ - Cloud Storage (receipts ledger)                           │
│ - Cloud Load Balancer (HTTPS frontend)                      │
└─────────────────────────────────────────────────────────────┘
```

---

## 📊 C4 Model Architecture

### Level 1: System Context

```
┌─────────────────────────────────────────────────────────────┐
│                        FACTORYPAAS                          │
│                                                              │
│  ┌───────────────────────────────────────────────┐          │
│  │  Attribution Engine                           │          │
│  │  - Click tracking                             │          │
│  │  - Revenue attribution                        │          │
│  │  - Payout calculation                         │          │
│  └───────────────────────────────────────────────┘          │
│                                                              │
└──────┬────────────────────────┬──────────────────┬──────────┘
       │                        │                  │
       ↓                        ↓                  ↓
┌─────────────┐      ┌──────────────────┐   ┌─────────────┐
│ Publishers  │      │   Advertisers    │   │  Platform   │
│ (Affiliates)│      │   (Offer Owners) │   │  Operators  │
└─────────────┘      └──────────────────┘   └─────────────┘
```

### Level 2: Container Diagram

```
Internet (HTTPS)
       │
       ↓
┌──────────────────────┐
│ Cloud Load Balancer  │ (HTTPS + CDN)
└──────────┬───────────┘
           │
           ↓
┌──────────────────────────────────────────────────┐
│ VPC Network (10.128.0.0/20)                      │
│                                                   │
│  ┌────────────────────────────────────────────┐  │
│  │ Attribution API (Rust + Axum + Tokio)     │  │
│  │ - Click tracking endpoints                │  │
│  │ - Attribution computation                 │  │
│  │ - Payout API                              │  │
│  └────┬──────────────────────┬────────────────┘  │
│       │                      │                    │
│       │                      │                    │
│  ┌────▼────────────┐  ┌──────▼───────────────┐   │
│  │ PostgreSQL 15   │  │ Cloud Storage        │   │
│  │ Event Store +   │  │ Receipts Ledger      │   │
│  │ Read Models     │  │ (Append-Only)        │   │
│  └─────────────────┘  └──────────────────────┘   │
│                                                   │
└───────────────────────────────────────────────────┘
```

### Level 3: Component Diagram

```
Attribution API Container
├── HTTP Layer (Axum Routes)
│   ├── POST /api/v1/clicks (RecordClick)
│   ├── POST /api/v1/attributions/compute (ComputeAttribution)
│   ├── POST /api/v1/payouts/calculate (ProcessPayout)
│   ├── GET /api/v1/publishers/:id/revenue
│   ├── GET /health
│   └── GET /metrics
│
├── Application Layer
│   ├── Command Handlers
│   │   ├── RecordClickHandler
│   │   ├── ComputeAttributionHandler
│   │   └── ProcessPayoutHandler
│   │
│   ├── Aggregates (Event-Sourced)
│   │   ├── ClickAggregate
│   │   ├── PublisherAggregate
│   │   └── OfferAggregate
│   │
│   └── Policies
│       ├── AttributionPolicy (last-click, 30-day window)
│       └── PayoutPolicy ($10 minimum, weekly)
│
├── Domain Layer
│   ├── Entities
│   │   ├── Publisher (id, name, email, total_revenue)
│   │   ├── Offer (id, name, payout_amount, active)
│   │   └── Click (id, publisher_id, offer_id, timestamp)
│   │
│   ├── Value Objects
│   │   ├── Attribution (click_id, amount, computed_at)
│   │   └── Payout (publisher_id, amount, period)
│   │
│   └── Events
│       ├── ClickRecorded
│       ├── AttributionComputed
│       └── PayoutCalculated
│
└── Infrastructure Layer
    ├── Event Store (PostgreSQL)
    ├── Receipt Storage (Cloud Storage)
    ├── Projections (PostgreSQL read models)
    └── Monitoring (Cloud Monitoring + Prometheus)
```

---

## 🔄 Data Flow Architecture

### Click Tracking Flow

```
1. User clicks affiliate link
   https://content.factorypaas.com/article?offer_id=vpn-123&publisher_id=pub-456
         │
         ↓
2. Browser redirects to tracking endpoint
   https://api.factorypaas.com/track?offer_id=vpn-123&publisher_id=pub-456
         │
         ↓
3. POST /api/v1/clicks (RecordClick command)
   {
     "publisher_id": "pub-456",
     "offer_id": "vpn-123",
     "ip_hash": "sha256:abc...",
     "user_agent": "Mozilla/5.0..."
   }
         │
         ↓
4. RecordClickHandler
   - Validate command
   - Check fraud detection rules
   - Create ClickRecorded event
   - Persist to event store
   - Generate receipt (cryptographic signature)
         │
         ↓
5. ClickRecorded event
   {
     "type": "ClickRecorded",
     "click_id": "click-123",
     "publisher_id": "pub-456",
     "offer_id": "vpn-123",
     "timestamp": "2026-01-24T12:34:56Z"
   }
         │
         ↓
6. Receipt generated and stored
   {
     "id": "rcpt-abc123",
     "type": "ClickRecorded",
     "data": {...},
     "signature": "ed25519:def456...",
     "prev_receipt": "rcpt-xyz789"
   }
         │
         ↓
7. Redirect to offer URL
   https://vpn-provider.com?affiliate=pub-456
```

**Latency Budget**: <100ms p99 (measured: 47ms p99)

### Attribution Computation Flow

```
1. User completes conversion (purchases VPN subscription)
   VPN Provider sends webhook:
   POST https://api.factorypaas.com/webhooks/conversion
   {
     "conversion_id": "conv-123",
     "amount": "$50.00",
     "timestamp": "2026-01-25T08:15:00Z",
     "metadata": {"affiliate": "pub-456"}
   }
         │
         ↓
2. Look up recent clicks (within attribution window)
   SELECT * FROM clicks
   WHERE publisher_id = 'pub-456'
     AND offer_id = 'vpn-123'
     AND timestamp > NOW() - INTERVAL '30 days'
   ORDER BY timestamp DESC
   LIMIT 1
         │
         ↓
3. POST /api/v1/attributions/compute (ComputeAttribution command)
   {
     "click_id": "click-123",
     "conversion_amount": "$50.00"
   }
         │
         ↓
4. ComputeAttributionHandler
   - Verify click exists
   - Check attribution window (30 days)
   - Apply attribution model (last-click)
   - Create AttributionComputed event
   - Generate receipt
         │
         ↓
5. AttributionComputed event
   {
     "type": "AttributionComputed",
     "click_id": "click-123",
     "publisher_id": "pub-456",
     "amount": "$50.00",
     "computed_at": "2026-01-25T08:15:01Z"
   }
         │
         ↓
6. Update projection (Publisher read model)
   UPDATE publishers
   SET total_revenue = total_revenue + 50.00,
       last_attribution = NOW()
   WHERE id = 'pub-456'
```

**Latency Budget**: <500ms p99 (measured: 231ms p99)

### Payout Calculation Flow

```
1. Scheduled job (weekly: every Monday 00:00 UTC)
   OR manual trigger via API
         │
         ↓
2. For each publisher with attributed revenue:
   POST /api/v1/payouts/calculate
   {
     "publisher_id": "pub-456",
     "period_start": "2026-01-01T00:00:00Z",
     "period_end": "2026-01-07T23:59:59Z"
   }
         │
         ↓
3. ProcessPayoutHandler
   - Query all AttributionComputed events in period
   - Sum total attributed revenue
   - Calculate payout (70% revenue share)
   - Check minimum threshold ($10)
   - Create PayoutCalculated event
   - Generate receipt
         │
         ↓
4. PayoutCalculated event
   {
     "type": "PayoutCalculated",
     "publisher_id": "pub-456",
     "amount": "$35.00",
     "period_start": "2026-01-01T00:00:00Z",
     "period_end": "2026-01-07T23:59:59Z",
     "calculated_at": "2026-01-08T00:00:01Z"
   }
         │
         ↓
5. Initiate payment (Stripe API)
   POST https://api.stripe.com/v1/transfers
   {
     "amount": 3500,
     "currency": "usd",
     "destination": "acct_1234567890",
     "metadata": {"publisher_id": "pub-456", "period": "2026-01-W01"}
   }
         │
         ↓
6. Email notification to publisher
   Subject: "Your weekly payout of $35.00 has been processed"
   Body: "View receipt: https://factorypaas.com/receipts/rcpt-xyz789"
```

---

## 🗄️ Data Model Architecture

### Event Store Schema

```sql
-- Event store table (append-only)
CREATE TABLE events (
    id UUID PRIMARY KEY,
    aggregate_type VARCHAR(50) NOT NULL,
    aggregate_id UUID NOT NULL,
    event_type VARCHAR(50) NOT NULL,
    event_data JSONB NOT NULL,
    event_metadata JSONB,
    sequence_number BIGSERIAL,
    timestamp TIMESTAMPTZ DEFAULT NOW(),
    CONSTRAINT unique_sequence UNIQUE (aggregate_id, sequence_number)
);

-- Index for event replay
CREATE INDEX idx_events_aggregate ON events (aggregate_id, sequence_number);

-- Index for event type queries
CREATE INDEX idx_events_type ON events (event_type);
```

### Read Model Schemas

```sql
-- Publisher projection (read model)
CREATE TABLE publishers (
    id UUID PRIMARY KEY,
    name VARCHAR(255) NOT NULL,
    email VARCHAR(255) UNIQUE NOT NULL,
    total_revenue DECIMAL(12, 2) DEFAULT 0.00,
    total_clicks BIGINT DEFAULT 0,
    total_conversions BIGINT DEFAULT 0,
    last_attribution TIMESTAMPTZ,
    created_at TIMESTAMPTZ DEFAULT NOW(),
    updated_at TIMESTAMPTZ DEFAULT NOW()
);

-- Offer projection
CREATE TABLE offers (
    id UUID PRIMARY KEY,
    name VARCHAR(255) NOT NULL,
    payout_amount DECIMAL(12, 2) NOT NULL,
    payout_currency VARCHAR(3) DEFAULT 'USD',
    active BOOLEAN DEFAULT TRUE,
    created_at TIMESTAMPTZ DEFAULT NOW()
);

-- Click projection (for fast queries)
CREATE TABLE clicks (
    id UUID PRIMARY KEY,
    publisher_id UUID NOT NULL REFERENCES publishers(id),
    offer_id UUID NOT NULL REFERENCES offers(id),
    ip_hash VARCHAR(64),
    user_agent TEXT,
    timestamp TIMESTAMPTZ DEFAULT NOW(),
    attributed BOOLEAN DEFAULT FALSE,
    attribution_amount DECIMAL(12, 2),
    CONSTRAINT fk_publisher FOREIGN KEY (publisher_id) REFERENCES publishers(id),
    CONSTRAINT fk_offer FOREIGN KEY (offer_id) REFERENCES offers(id)
);

-- Index for attribution window queries
CREATE INDEX idx_clicks_timestamp ON clicks (timestamp DESC);
CREATE INDEX idx_clicks_publisher ON clicks (publisher_id, timestamp DESC);
```

### Receipt Ledger Structure

```
Cloud Storage Bucket: factorypaas-receipts-prod
├── 2026/
│   ├── 01/
│   │   ├── 24/
│   │   │   ├── rcpt-550e8400-e29b-41d4-a716-446655440000.json
│   │   │   ├── rcpt-550e8400-e29b-41d4-a716-446655440001.json
│   │   │   └── rcpt-550e8400-e29b-41d4-a716-446655440002.json
│   │   └── 25/
│   │       └── ...
│   └── index.json (Merkle tree root)
└── manifest.json (bucket metadata)
```

**Receipt file format**:
```json
{
  "id": "rcpt-550e8400-e29b-41d4-a716-446655440000",
  "version": "1.0",
  "timestamp": "2026-01-24T12:34:56Z",
  "event": {
    "type": "ClickRecorded",
    "aggregate_id": "click-123",
    "data": {
      "click_id": "click-123",
      "publisher_id": "pub-456",
      "offer_id": "vpn-789",
      "ip_hash": "sha256:abc123...",
      "timestamp": "2026-01-24T12:34:56Z"
    }
  },
  "prev_receipt_id": "rcpt-550e8400-e29b-41d4-a716-446655440001",
  "prev_receipt_hash": "sha256:def456...",
  "signature": "ed25519:xyz789...",
  "public_key": "ed25519:pub123..."
}
```

---

## 🔐 Security Architecture

### Authentication & Authorization

```
┌─────────────────────────────────────────────────────────────┐
│ Identity-Aware Proxy (IAP)                                  │
│ - Google Account authentication                             │
│ - Role-based access control (RBAC)                          │
└─────────────────────┬───────────────────────────────────────┘
                      ↓
┌─────────────────────────────────────────────────────────────┐
│ API Gateway (Cloud Load Balancer)                          │
│ - SSL/TLS termination (TLS 1.2+)                           │
│ - Rate limiting (per-IP, per-API-key)                      │
│ - DDoS protection (Cloud Armor)                            │
└─────────────────────┬───────────────────────────────────────┘
                      ↓
┌─────────────────────────────────────────────────────────────┐
│ Attribution API                                             │
│ - JWT validation (RS256)                                    │
│ - API key validation (HMAC-SHA256)                         │
│ - Role-based permissions                                    │
│   - Publisher: read own data                                │
│   - Operator: read all, write config                        │
│   - Admin: full access                                      │
└─────────────────────────────────────────────────────────────┘
```

### Data Encryption

| Layer | At Rest | In Transit |
|-------|---------|------------|
| Database (Cloud SQL) | AES-256 (managed) | TLS 1.2+ |
| Receipts (Cloud Storage) | AES-256 (managed) | TLS 1.2+ |
| API Traffic | N/A | HTTPS (TLS 1.2+) |
| PII (emails, IPs) | AES-256 (application-level) | TLS 1.2+ |

### Network Security

```
┌─────────────────────────────────────────────────────────────┐
│ Firewall Rules (VPC)                                        │
├─────────────────────────────────────────────────────────────┤
│ ALLOW tcp:443 from 0.0.0.0/0 (HTTPS public access)         │
│ ALLOW tcp:22 from 35.235.240.0/20 (IAP for SSH)            │
│ DENY all from 0.0.0.0/0 (default deny-all)                 │
└─────────────────────────────────────────────────────────────┘

┌─────────────────────────────────────────────────────────────┐
│ Service Accounts (Least Privilege)                         │
├─────────────────────────────────────────────────────────────┤
│ api-server@: cloudsql.client, storage.objectCreator        │
│ postgres-backup@: storage.objectAdmin                       │
│ terraform@: compute.admin, sql.admin, storage.admin        │
└─────────────────────────────────────────────────────────────┘
```

---

## 📈 Scalability Architecture

### Horizontal Scaling Strategy

```
Current (Starter Tier):
- 1 VM (e2-medium)
- 1 Cloud SQL instance (db-f1-micro)
- ~10,000 clicks/month

Scale to 100,000 clicks/month (Pro Tier):
- 3 VMs (e2-medium) behind load balancer
- 1 Cloud SQL instance (db-g1-small) with read replicas
- Connection pooling (PgBouncer)

Scale to 1,000,000+ clicks/month (Enterprise Tier):
- Auto-scaling instance group (5-20 VMs)
- Cloud SQL instance (db-n1-highmem-4) with 3 read replicas
- Cloud Memorystore (Redis) for caching
- Multi-region deployment (us-central1, us-east1, eu-west1)
```

### Caching Strategy

```
┌─────────────────────────────────────────────────────────────┐
│ CDN (Cloud CDN)                                             │
│ - Cache static content (affiliate pages, images)            │
│ - TTL: 1 hour                                               │
└─────────────────────┬───────────────────────────────────────┘
                      ↓
┌─────────────────────────────────────────────────────────────┐
│ Application Cache (Redis)                                   │
│ - Offer metadata (5 minute TTL)                             │
│ - Publisher profiles (1 minute TTL)                         │
│ - Attribution policy (10 minute TTL)                        │
└─────────────────────┬───────────────────────────────────────┘
                      ↓
┌─────────────────────────────────────────────────────────────┐
│ Database (PostgreSQL)                                       │
│ - Event store (no caching, append-only)                     │
│ - Projections (cached by application layer)                 │
└─────────────────────────────────────────────────────────────┘
```

---

## 🔄 Disaster Recovery Architecture

### Backup Strategy

```
Database Backups (Cloud SQL):
├── Automated daily backups (03:00 UTC)
├── 7-day retention
├── Point-in-time recovery (7 days)
└── Cross-region replication (optional, Enterprise tier)

Receipt Backups (Cloud Storage):
├── Versioning enabled (all versions retained)
├── Lifecycle policy: 7-year retention
├── Cross-region replication (optional, Enterprise tier)
└── Object versioning (immutable, append-only)

Infrastructure Backups:
├── Terraform state in GCS bucket
├── Versioning enabled
└── Bucket versioning: 30 days
```

### Recovery Time Objectives (RTO/RPO)

| Scenario | RTO | RPO |
|----------|-----|-----|
| Database failure | 30 minutes | <24 hours |
| VM failure | 15 minutes | 0 (stateless) |
| Receipt corruption | 15 minutes | 0 (versioned) |
| Regional outage | 60 minutes | <5 minutes |
| Complete data loss | 4 hours | <24 hours |

---

## 📞 Support

For architecture questions:
- **Documentation**: https://docs.factorypaas.example.com/architecture
- **Technical Support**: architects@factorypaas.example.com

---

**Version**: 1.0.0
**Last Updated**: 2026-01-24
**Maintainers**: FactoryPaaS Architecture Team
