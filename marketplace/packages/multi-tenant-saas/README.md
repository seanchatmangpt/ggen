# Multi-Tenant SaaS Package

Production-ready multi-tenant SaaS architecture with RDF ontology, SPARQL queries, and code templates.

## Overview

This package provides a comprehensive solution for building multi-tenant SaaS applications with:
- **Tenant Isolation**: Schema, database, row-level, and hybrid strategies
- **Subscription Management**: Free, Basic, Pro, Enterprise tiers
- **Usage Metering**: Track API calls, storage, and compute
- **Billing Engine**: Automated billing with overage charges
- **Security & Compliance**: GDPR, HIPAA, SOC2 support

## Features

- ✅ RDF ontology (320+ lines) defining multi-tenancy concepts
- ✅ 12 SPARQL queries for tenant operations
- ✅ Rust middleware for tenant identification (Axum)
- ✅ TypeScript/React admin dashboard
- ✅ Python billing and analytics engine
- ✅ Chicago TDD tests with real RDF/SPARQL

## Quick Start

### 1. Generate Tenant Management Code

```bash
ggen template render multi-tenant-saas/templates/rust/tenant_middleware.rs \
  --output src/middleware/tenant.rs
```

### 2. Query Active Tenants

```bash
ggen query execute multi-tenant-saas/sparql/queries.rq \
  --graph tenants.ttl \
  --query "Extract All Active Tenants"
```

### 3. Calculate Billing

```bash
python multi-tenant-saas/templates/python/billing_analytics.py
```

## Architecture

### Tenant Isolation Strategies

| Strategy | Description | Use Case |
|----------|-------------|----------|
| **Row-Level** | Shared schema with `tenant_id` column | Small tenants, low cost |
| **Schema** | Dedicated schema per tenant | Medium tenants, good isolation |
| **Database** | Dedicated database per tenant | Enterprise, strict compliance |
| **Hybrid** | Mix strategies by tier | Flexible, cost-optimized |

### Subscription Tiers

| Tier | Base Price | API Calls | Storage | Overage |
|------|-----------|-----------|---------|---------|
| **Free** | $0 | 1,000 | 1 GB | None allowed |
| **Basic** | $29 | 50,000 | 10 GB | $0.50/1K calls |
| **Pro** | $99 | 500,000 | 100 GB | $0.30/1K calls |
| **Enterprise** | $499 | 5M | 1 TB | $0.10/1K calls |

## RDF Ontology Classes

- `mt:Tenant` - Customer organization
- `mt:SubscriptionTier` - Pricing tier (Free/Basic/Pro/Enterprise)
- `mt:TenantIsolationStrategy` - Isolation method
- `mt:FeatureFlag` - Feature enablement toggle
- `mt:UsageMetric` - Consumption tracking (API/storage/compute)
- `mt:BillingRecord` - Invoice data
- `mt:DataResidency` - Geographic compliance
- `mt:TenantSecurityPolicy` - Security rules

## SPARQL Query Examples

### Extract Active Tenants
```sparql
PREFIX mt: <http://example.org/multi-tenant#>
SELECT ?tenantId ?tierLabel WHERE {
    ?tenant mt:tenantId ?tenantId ;
            mt:hasLifecycleState mt:ActiveTenant ;
            mt:hasSubscriptionTier ?tier .
    ?tier rdfs:label ?tierLabel .
}
```

### Calculate Usage Metrics
```sparql
PREFIX mt: <http://example.org/multi-tenant#>
SELECT ?tenantId (SUM(?apiCalls) AS ?total) WHERE {
    ?tenant mt:tenantId ?tenantId ;
            mt:hasUsageMetric ?metric .
    ?metric a mt:ApiCallMetric ;
            mt:metricValue ?apiCalls .
}
GROUP BY ?tenantId
```

## Code Examples

### Rust: Tenant Middleware
```rust
use axum::{Extension, middleware};

let resolver = Arc::new(TenantResolver::new());
let app = Router::new()
    .route("/api/data", get(handler))
    .layer(middleware::from_fn(tenant_middleware))
    .layer(Extension(resolver));
```

### TypeScript: Admin Dashboard
```tsx
import { TenantListDashboard } from './admin-ui';

function App() {
  return <TenantListDashboard />;
}
```

### Python: Billing Calculation
```python
from billing_analytics import TenantBillingEngine

engine = TenantBillingEngine()
billing_record = engine.calculate_billing(
    tenant_id="tenant_acme",
    tier_name="Pro",
    period_start=datetime(2025, 11, 1),
    period_end=datetime(2025, 11, 30),
)
```

## Testing

Run Chicago TDD tests:
```bash
cargo test --package multi-tenant-saas
```

All tests use real RDF graphs and SPARQL queries for authentic validation.

## License

MIT
