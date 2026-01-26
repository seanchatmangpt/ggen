# Architecture Guide

System architecture overview for the value-based pricing onboarding platform.

## System Overview

```
┌─────────────────────────────────────────────────────────────────────┐
│                           Frontend (React)                           │
│  - Setup Wizard                                                      │
│  - Value Definition UI                                              │
│  - Dashboard                                                        │
│  - Approval Interface                                               │
│  - Support Ticketing                                                │
└─────────────────────┬───────────────────────────────────────────────┘
                      │ HTTPS / REST API
┌─────────────────────▼───────────────────────────────────────────────┐
│                      API Gateway (Nginx/ALB)                         │
│  - Request routing                                                  │
│  - Rate limiting                                                    │
│  - Authentication                                                   │
│  - CORS handling                                                    │
└─────────────────────┬───────────────────────────────────────────────┘
                      │
        ┌─────────────┼──────────────┐
        │             │              │
┌───────▼──────┐ ┌───▼────────┐ ┌──▼───────────┐
│   Backend    │ │ Redis      │ │ PostgreSQL   │
│   (Node.js)  │ │ Cache      │ │ Database     │
│              │ │            │ │              │
│ Controllers  │ │ Sessions   │ │ Customers    │
│ Services     │ │ Caching    │ │ Receipts     │
│ Middleware   │ │ Queues     │ │ Approvals    │
└────┬─────────┘ └────────────┘ │ Alerts       │
     │                          │ Tickets      │
     └──────────────────────────└──────────────┘
```

## Component Architecture

### Frontend Layer (React SPA)

**Responsibilities**:
- User interface for all customer interactions
- Real-time dashboard updates
- Form validation and submission
- State management with Zustand
- Data fetching with React Query

**Key Components**:
- **SetupWizard** - 5-step deployment guide with validation
- **ValueDefinition** - Configure business metrics
- **Dashboard** - Real-time value metrics and progress
- **ApprovalWorkflow** - Multi-stakeholder sign-off interface
- **SupportTicketing** - Help desk integration
- **ReceiptViewer** - Cryptographic proof verification

**Technology Stack**:
- React 18
- TypeScript
- Tailwind CSS
- Zustand (state management)
- React Query (data fetching)
- Zod (validation)

### API Layer (Express.js)

**Responsibilities**:
- REST API endpoint handling
- Request/response validation
- Authentication/authorization
- Business logic orchestration
- Database interaction
- Error handling and logging

**Architecture**:
```
Request
  ↓
Middleware (Auth, Logging, Tracing)
  ↓
Route Handler (Controller)
  ↓
Service Layer (Business Logic)
  ↓
Data Access Layer (Repository)
  ↓
Database
```

**Key Services**:
- **CustomerService** - Account management
- **SetupService** - Deployment workflow
- **ValueService** - Metric calculation and validation
- **ApprovalService** - Multi-stage approval orchestration
- **ReceiptService** - Cryptographic proof generation
- **AlertService** - Anomaly detection
- **TicketService** - Support ticket management

### Data Layer (PostgreSQL)

**Schema Design**:

**Core Tables**:
- `customers` - Account information and status
- `users` - User accounts and roles
- `setup_steps` - Deployment progress tracking
- `value_definitions` - Customer's value metrics
- `value_calculations` - Immutable calculation ledger
- `receipts` - Cryptographic ledger (chain of custody)

**Approval Tables**:
- `approvals` - Multi-stage approval workflow
- `approvers` - Stakeholders involved in approvals

**Support Tables**:
- `support_tickets` - Customer support tickets
- `ticket_messages` - Ticket conversations

**Configuration Tables**:
- `api_keys` - API authentication
- `integration_configs` - Third-party integrations
- `measurement_accuracy_feedback` - Continuous improvement

**Audit Tables**:
- `audit_log` - All data changes for compliance

### Cache Layer (Redis)

**Use Cases**:
- Session storage for authenticated users
- Caching database queries (dashboard data)
- Rate limiting counters
- Job queues for async processing
- Real-time metrics caching

**TTL Strategy**:
- Sessions: 24 hours
- Dashboard cache: 5 minutes
- Calculation results: 1 hour
- Temporary locks: 5 minutes

## Data Flow

### Setup Wizard Flow

```
Customer Setup
    ↓
Step 1: API Integration ─→ Validate credentials
    ↓
Step 2: Data Source ─→ Test connection
    ↓
Step 3: Metrics Config ─→ Map data fields
    ↓
Step 4: Baseline ─→ Establish starting point
    ↓
Step 5: Testing ─→ Verify accuracy
    ↓
Receipt Generated ─→ Stored in immutable ledger
```

### Value Calculation Flow

```
Scheduled Job (hourly/daily)
    ↓
Query data from integrations
    ↓
Apply calculation formulas
    ↓
Generate Value Calculation Receipt
    ↓
Store with cryptographic signature
    ↓
Check for anomalies
    ↓
Create alerts if needed
    ↓
Update dashboard cache
    ↓
Send webhook notification
```

### Approval Workflow

```
Value Definition Created
    ↓
Invite Approvers (Finance + Technical)
    ↓
Finance Review Stage
    ├─ Review measurement methodology
    ├─ Approve or Request Changes
    ↓
Technical Review Stage
    ├─ Verify data accuracy
    ├─ Approve or Request Changes
    ↓
Final Approval
    ├─ C-level sign-off
    ├─ Generate approval receipt
    ↓
Customer notified
```

### Go-Live Switch Flow

```
Pre-flight Checks
    ├─ Setup completion ✓
    ├─ Approvals complete ✓
    ├─ Measurement accuracy ≥ 85% ✓
    ├─ No critical alerts ✓
    ↓
Go-Live Receipt Generated
    ↓
Value-based billing enabled
    ↓
Real-time metrics activated
    ↓
Continuous monitoring begins
```

## Security Architecture

### Authentication

```
User Login
    ↓
OAuth2 / JWT Token Generation
    ↓
Token stored in secure HttpOnly cookie
    ↓
JWT validated on every API request
    ↓
Claims extracted (userId, customerId, role)
    ↓
Route-level authorization check
```

### Authorization

```
RBAC Model:
  - ADMIN: Full system access
  - CUSTOMER_ADMIN: Customer account administration
  - FINANCE: Financial review and approvals
  - TECHNICAL: Technical setup and integration

Resource-level checks:
  - Customers can only access their own data
  - Approvers limited to assigned approvals
  - Finance users can only modify financial data
```

### Data Protection

```
Encryption at Rest:
  - Database encryption (TDE/pgcrypto)
  - Secrets encrypted with AWS KMS or Google Secret Manager

Encryption in Transit:
  - TLS 1.2+ for all communications
  - Certificate pinning for API clients

Cryptographic Receipts:
  - RSA-2048 key pairs for signing
  - SHA-256 hashing for integrity
  - Chain of custody (Merkle tree-like structure)
```

## Integration Architecture

### Supported Integrations

```
Data Sources:
  ├─ Salesforce (revenue, customers)
  ├─ HubSpot (pipeline, deals)
  ├─ Stripe (payments, subscriptions)
  ├─ Segment (event data)
  └─ Custom Webhooks

Notification Channels:
  ├─ Email (SendGrid)
  ├─ Slack (incoming webhooks)
  ├─ PagerDuty (critical alerts)
  └─ Webhooks (customer systems)
```

### Webhook Architecture

```
Event Triggered
    ↓
Create Event Payload (signed)
    ↓
Retry Queue (exponential backoff)
    ├─ Immediate send
    ├─ Retry after 5 min
    ├─ Retry after 30 min
    ├─ Retry after 2 hours
    └─ Finally mark failed
    ↓
Track delivery status
    ↓
Log for audit trail
```

## Monitoring & Observability

### Metrics

```
Application Metrics:
  - HTTP request rate, latency, errors
  - Database query performance
  - Cache hit/miss rates
  - Value calculation accuracy
  - Approval workflow duration

Business Metrics:
  - Customers onboarding/month
  - Time to go-live (average)
  - Measurement accuracy (%)
  - Approval success rate
  - Customer satisfaction
```

### Distributed Tracing

```
Request Entry
    ↓
Trace ID generated (propagated through system)
    ↓
Spans created for each operation:
    ├─ API handler
    ├─ Auth check
    ├─ Service layer
    ├─ Database query
    ├─ Cache operations
    └─ External API calls
    ↓
Traces exported to Jaeger
    ↓
Visualized in tracing UI
```

### Alerting

```
Alert Rules:
  - High error rate (>5%)
  - Slow requests (p99 > 1s)
  - Database query slow (>2s)
  - Cache miss rate spike
  - Integration failures
  - Data anomalies

Escalation:
  - Slack notification
  - PagerDuty incident
  - Email to on-call
  - Auto-remediation attempts
```

## Deployment Architecture

### Development

```
docker-compose up
├─ PostgreSQL (5432)
├─ Redis (6379)
├─ Backend (3001)
├─ Frontend (3000)
├─ Prometheus (9090)
├─ Grafana (3005)
└─ Jaeger (16686)
```

### Staging

```
Kubernetes Cluster (2-node)
├─ Backend Deployment (2 replicas)
├─ Frontend Deployment (2 replicas)
├─ PostgreSQL StatefulSet
├─ Redis StatefulSet
├─ Prometheus DaemonSet
├─ Grafana Deployment
└─ Jaeger Deployment
```

### Production

```
GCP/AWS (Multi-region)
├─ Cloud Run / ECS
│  ├─ Backend service (auto-scaling)
│  └─ Frontend service (CDN + auto-scaling)
├─ Cloud SQL / RDS
│  ├─ Primary database
│  ├─ Read replicas
│  └─ Automated backups
├─ Cloud Memorystore / ElastiCache
│  ├─ Redis cluster
│  └─ Automated failover
├─ Cloud Monitoring / CloudWatch
├─ Cloud Logging / CloudWatch Logs
└─ Firestore (optional ledger)
```

## Scaling Strategy

### Horizontal Scaling

```
Auto-scaling Rules:
  - Backend: CPU > 70% or Memory > 80% → Add replica
  - Frontend: CPU > 60% or Memory > 75% → Add replica
  - Database: Connection pool > 80% → Add read replica
```

### Data Partitioning

```
By Customer ID:
  - Separate data per customer where possible
  - Enables per-customer scaling
  - Improves query performance

By Time:
  - Partition value_calculations by month
  - Archive old receipts
  - Maintain hot data in fast storage
```

## Disaster Recovery

### Backup Strategy

```
Database:
  - Full backup: Daily at 2 AM UTC
  - Incremental: Every 4 hours
  - Retention: 30 days
  - Cross-region replication

Receipts:
  - Immutable ledger backed up immediately
  - Distributed ledger (multiple replicas)
  - Cryptographic verification of backups

RTO: 15 minutes
RPO: 4 hours
```

### Recovery Procedures

```
Database Failure:
  1. Detect failure (health check fails)
  2. Promote read replica to primary
  3. Update connection strings
  4. Run verification tests
  5. Notify customers

Receipt Corruption:
  1. Detect using cryptographic verification
  2. Restore from distributed ledger
  3. Rebuild affected chains
  4. Generate audit report
  5. Notify compliance team
```

---

**Last Updated**: 2024-01-25
**Version**: 1.0.0
