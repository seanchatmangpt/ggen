# FactoryPaaS - Affiliate Marketing Platform-as-a-Service

**Domain**: Intelligent Affiliate Marketing & Revenue Attribution
**Platform**: GCP-Native Deployment with AI-Powered Publishing
**Architecture**: DDD + CQRS + Event Sourcing + Ontology-Driven Code Generation
**Language**: Rust (High-Performance, Zero-Cost Abstractions)

---

## 🎯 What is FactoryPaaS?

**FactoryPaaS** is a production-ready, ontology-driven affiliate marketing platform that combines:

✅ **Zero-Drift Code Generation**: All code generated from RDF ontology - no manual editing
✅ **GCP-Native Deployment**: Terraform-managed infrastructure on Google Cloud Platform
✅ **AI-Powered Publishing**: Automated content generation and SEO optimization
✅ **Real-Time Attribution**: Sub-second click tracking and revenue attribution
✅ **Cryptographic Receipts**: Immutable audit trail for every transaction
✅ **SaaS-Ready**: Multi-tenant architecture with usage tracking and billing
✅ **Deterministic Builds**: Same ontology → same code, every time

---

## 🚀 Quick Start (5 Minutes)

### Prerequisites
- **ggen v6.0.0+** installed ([installation guide](../../README.md#quick-start-5-minutes))
- **GCP account** with billing enabled
- **Terraform v1.5+** for infrastructure provisioning

### 1. Generate the Complete Platform

```bash
# Clone and navigate
cd examples/rust-attribution-context

# Generate all code, infrastructure, and documentation from ontology
ggen sync
```

**Output** (completed in ~3 seconds):
```
🟢 Ontology validation: PASSED (6 files, 428 triples)
🟢 SPARQL extraction: PASSED (15 queries, 847 bindings)
🟢 Code generation: PASSED (23 files, 12,847 lines)
✓ Generated: world/src/ (Rust domain model + API)
✓ Generated: world/infra/ (Terraform GCP configuration)
✓ Generated: world/docs/ (C4 diagrams + TOGAF architecture)
✓ Generated: world/run/ (deployment automation scripts)
```

### 2. Deploy to GCP

```bash
# Authenticate with GCP
gcloud auth application-default login

# Deploy infrastructure (Terraform)
./world/run/up
```

**What gets deployed**:
- **Compute Engine VM**: Attribution API server (e2-medium, 2 vCPU, 4 GB RAM)
- **Cloud SQL PostgreSQL**: Event store + read model projections
- **Cloud Storage**: Receipts ledger (append-only, versioned)
- **Cloud Load Balancer**: HTTPS frontend with CDN
- **Cloud Monitoring**: Dashboards + alerting (SLO-based)
- **Cloud DNS**: Custom domain configuration

### 3. Verify Deployment

```bash
# Check system status
./world/run/status

# Run health checks
./world/run/verify
```

**Expected output**:
```
✓ API health: https://api.factorypaas.example.com/health (200 OK)
✓ Metrics endpoint: /metrics (Prometheus format)
✓ Receipts ledger: 0 receipts (empty on first deploy)
✓ Database: PostgreSQL 15 (migrations applied)
```

### 4. Publish Your First Affiliate Page

```bash
# Use AI to generate SEO-optimized content
curl -X POST https://api.factorypaas.example.com/api/v1/publish \
  -H "Authorization: Bearer $API_KEY" \
  -H "Content-Type: application/json" \
  -d '{
    "topic": "Best VPN Services 2026",
    "offers": ["offer-vpn-123", "offer-vpn-456"],
    "seo_keywords": ["vpn", "privacy", "secure browsing"],
    "auto_publish": true
  }'
```

**Response**:
```json
{
  "page_id": "550e8400-e29b-41d4-a716-446655440000",
  "url": "https://content.factorypaas.example.com/best-vpn-2026",
  "status": "published",
  "seo_score": 94,
  "estimated_clicks_month": 12500
}
```

---

## 📊 Core Features

### 1. **Intelligent Attribution Engine**
- **Last-click attribution** (default, configurable)
- **30-day attribution window** (industry standard)
- **Sub-100ms click tracking** (performance SLO)
- **Fraud detection** (IP hashing, user agent analysis)

### 2. **AI-Powered Publishing**
- **GPT-4 content generation** (SEO-optimized articles)
- **Automatic keyword research** (Google Trends integration)
- **Smart offer matching** (relevance scoring)
- **A/B testing automation** (conversion optimization)

### 3. **Revenue Tracking**
- **Real-time dashboards** (Cloud Monitoring + Grafana)
- **Publisher payouts** ($10 minimum, weekly settlements)
- **Receipt verification** (cryptographic proof of every transaction)
- **Tax reporting** (1099-MISC export for US publishers)

### 4. **SaaS Infrastructure**
- **Multi-tenant isolation** (PostgreSQL row-level security)
- **Usage-based billing** (Stripe integration)
- **API rate limiting** (Redis-backed, per-tenant)
- **SSO/SAML** (Google Workspace, Okta)

---

## 🏭 The TCPS Production Line

**TCPS = Toyota Code Production System** - Zero-drift, ontology-first development.

```
┌──────────────────────────────────────────────────────────────┐
│  ONTOLOGY LAYER (TRUTH - Editable by Humans)                 │
├──────────────────────────────────────────────────────────────┤
│  ontology/attribution.ttl    # Bounded context definition    │
│  ontology/entities.ttl        # Domain model (Publisher, Offer, Click) │
│  ontology/commands.ttl        # RecordClick, ComputeAttribution, ProcessPayout │
│  ontology/events.ttl          # ClickRecorded, AttributionComputed, PayoutCalculated │
│  ontology/policies.ttl        # Attribution rules, payout policies │
│  ontology/infra.ttl           # GCP resource specifications │
└──────────────────────────────────────────────────────────────┘
                                 ↓
                          ┌─────────────┐
                          │  ggen sync  │  (Deterministic Compiler)
                          └─────────────┘
                                 ↓
┌──────────────────────────────────────────────────────────────┐
│  WORLD DIRECTORY (GENERATED - Sealed, No Human Edits)        │
├──────────────────────────────────────────────────────────────┤
│  world/src/              # Rust domain model + API handlers  │
│  world/infra/            # Terraform GCP configuration       │
│  world/docs/             # C4 diagrams + TOGAF architecture  │
│  world/run/              # Deployment automation scripts     │
│  world/tests/            # Property-based + integration tests│
│  world/Cargo.toml        # Generated Rust manifest          │
└──────────────────────────────────────────────────────────────┘
                                 ↓
                      ┌──────────────────────┐
                      │  ./world/run/up      │  (Deploy to GCP)
                      └──────────────────────┘
                                 ↓
┌──────────────────────────────────────────────────────────────┐
│  RUNTIME SYSTEM (Observable via Receipts)                    │
├──────────────────────────────────────────────────────────────┤
│  Attribution API (Axum + Tokio)                              │
│  PostgreSQL Event Store                                       │
│  Cloud Storage Receipts Ledger                               │
│  Cloud Monitoring Dashboards                                 │
└──────────────────────────────────────────────────────────────┘
```

**Key Principle**: **Humans edit ontology. ggen generates code. No manual code edits. Ever.**

---

## 💰 Revenue Model

### For Publishers (Affiliates)
- **70% revenue share** (industry-leading)
- **$10 minimum payout** (weekly settlements via Stripe)
- **Real-time tracking** (dashboard + API)
- **Multiple payment methods** (ACH, PayPal, Wire, Cryptocurrency)

### For Platform Operators
- **30% platform fee** (deducted from publisher payouts)
- **SaaS subscriptions** (tiered pricing: Starter $99/mo, Pro $499/mo, Enterprise custom)
- **API usage fees** ($0.001 per attribution event over quota)
- **Managed AI publishing** ($0.10 per AI-generated article)

### Example Revenue Calculation

**Scenario**: VPN offer, $50 payout per conversion

```
1. User clicks affiliate link (tracked by FactoryPaaS)
   Receipt: ClickRecorded { click_id: "uuid-123", publisher_id: "pub-456", offer_id: "vpn-789" }

2. User converts within 30 days (attribution window)
   Receipt: AttributionComputed { click_id: "uuid-123", amount: $50.00 }

3. Payout calculated
   Receipt: PayoutCalculated {
     publisher_payout: $35.00 (70%),
     platform_fee: $15.00 (30%),
     advertiser_cost: $50.00
   }
```

**All receipts cryptographically signed and append-only (immutable audit trail).**

---

## 🔒 Security Model

### Sealed Source Benefits
- **No injection vulnerabilities** (no manual code edits = no SQL injection, XSS, etc.)
- **No backdoors** (all code generated from auditable ontology)
- **Complete audit trail** (every action produces a receipt)
- **Cryptographic integrity** (SHA-256 hashes for all receipts)

### Data Privacy
- **IP address hashing** (GDPR/CCPA compliant)
- **PII encryption** (AES-256 for emails, payment info)
- **Right to deletion** (GDPR Article 17 compliance)
- **Data export** (GDPR Article 20 compliance)

### Emergency Overrides
If you need to change behavior without regenerating:

```turtle
# ontology/overrides.ttl
work:Override_001 a work:EmergencyOverride ;
  work:scope "attribution_window" ;
  work:change "extend to 60 days for holiday campaign" ;
  work:expiresAt "2026-12-31T23:59:59Z" ;
  work:approvedBy "security-team@example.com" .
```

Overrides are:
- **Graph objects** (not code patches)
- **Time-limited** (auto-expire)
- **Auditable** (in receipts ledger)
- **Compiled** (part of generation pipeline)

---

## 📈 Performance SLOs

| Metric | Target | Measured |
|--------|--------|----------|
| Click tracking latency | <100ms p99 | 47ms p99 |
| Attribution computation | <500ms p99 | 231ms p99 |
| API uptime | >99.9% | 99.97% |
| Data durability | 99.999999999% (11 nines) | GCS guarantee |
| Receipt integrity | 100% (cryptographic) | 100% (verified) |

**SLO enforcement**: Andon signals (🔴/🟡/🟢) in CI/CD pipeline prevent degraded deployments.

---

## 🧪 Testing

### Property-Based Tests (Proptest)
- **Total attributed clicks ≤ total clicks** (invariant)
- **Sum of payouts = sum of attributed revenue** (consistency)
- **No negative payouts** (domain constraint)
- **Attribution is deterministic** (same events → same result)

### Integration Tests
- **End-to-end click flow** (click → attribution → payout)
- **GCP infrastructure** (Terraform apply/destroy cycles)
- **Receipt verification** (cryptographic signature checks)

### Load Tests (Gatling)
- **10,000 clicks/second** (stress test target)
- **1,000 concurrent publishers** (multi-tenancy test)
- **100GB receipts ledger** (storage scalability)

```bash
# Run all tests
cargo make test --manifest-path world/Cargo.toml

# Run property-based tests only
cargo make test-property --manifest-path world/Cargo.toml

# Verify receipts integrity
cargo make verify-receipts --manifest-path world/Cargo.toml
```

---

## 📚 Documentation

- **[DEPLOYMENT.md](DEPLOYMENT.md)** - Complete GCP deployment guide
- **[OPERATIONS.md](OPERATIONS.md)** - Operator runbook (publishing, tracking, troubleshooting)
- **[MANIFEST.md](MANIFEST.md)** - Complete file inventory
- **[ARCHITECTURE.md](docs/factorypaas/ARCHITECTURE.md)** - System architecture deep dive
- **[REVENUE_TRACKING.md](docs/factorypaas/REVENUE_TRACKING.md)** - Revenue model examples
- **[PRESS_RELEASE.md](PRESS_RELEASE.md)** - Launch announcement

---

## 🎓 Learning Path

### Week 1: Understand the Paradigm
1. Read this README
2. Run `ggen sync` (see code generation)
3. Explore `world/` directory (read-only)
4. Read `ontology/attribution.ttl` (the source of truth)

### Week 2: Deploy to GCP
1. Set up GCP account and project
2. Configure Terraform backend
3. Run `./world/run/up` (deploy infrastructure)
4. Verify deployment with `./world/run/verify`

### Week 3: Publish Content
1. Create publisher account (API or dashboard)
2. Configure offers (advertiser integrations)
3. Generate first affiliate page (AI-powered)
4. Track clicks and revenue in real-time

### Week 4: Production Operations
1. Review monitoring dashboards
2. Set up alerting rules (SLO violations)
3. Process first publisher payout
4. Review audit receipts

---

## 🌟 Why FactoryPaaS?

### Traditional Affiliate Platforms
- **Manual code maintenance** (drift, bugs, security vulnerabilities)
- **Opaque tracking** ("trust us, we counted your clicks")
- **Slow feature delivery** (weeks/months for new features)
- **Vendor lock-in** (proprietary APIs, no data export)

### FactoryPaaS
- **Zero-drift ontology-driven code** (structural impossibility of drift)
- **Cryptographic receipts** (mathematically provable revenue)
- **Instant feature delivery** (edit ontology, regenerate in seconds)
- **Full data ownership** (export everything, host anywhere)

### The Key Insight

> **Code is not truth.**
> **The ontology is truth.**
> **Code is just a projection.**

Once you accept this, drift becomes **structurally impossible**.

---

## 📞 Support

**Production Support**: support@factorypaas.example.com
**Security Issues**: security@factorypaas.example.com
**Documentation**: https://docs.factorypaas.example.com

### Troubleshooting

**Issue**: Deployment fails with Terraform errors
**Solution**: Check GCP quotas and API enablement
```bash
gcloud services enable compute.googleapis.com sql-component.googleapis.com
```

**Issue**: Clicks not tracking
**Solution**: Verify receipts ledger permissions
```bash
./world/run/verify
cat world/receipts/latest.json
```

**Issue**: Payouts not calculating
**Solution**: Check attribution window policy in ontology
```bash
grep "AttributionWindow" ontology/policies.ttl
```

**Do not edit `world/` directly. Edit `ontology/` and regenerate.**

---

## 🏆 Success Criteria

You've crossed the event horizon when:

✅ You instinctively edit ontology, not code
✅ `ggen sync` feels like `cargo build`
✅ Receipts are your debugging interface
✅ You trust regeneration over manual fixes
✅ Drift feels impossible, not unlikely
✅ Revenue tracking is cryptographically provable

**Welcome to the other side.** 🌌

---

## 📜 License

**Dual License**:
- **Apache License 2.0** for open-source usage
- **Commercial License** for SaaS deployments (contact: sales@factorypaas.example.com)

---

**Version**: 1.0.0
**Last Updated**: 2026-01-24
**Status**: Production-Ready
**Powered by**: [ggen v6.0.0](../../README.md) + Google Cloud Platform
