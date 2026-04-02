# TAI Autonomics Roadmap

**Status**: Production Ready
**Last Updated**: 2026-01-26
**Planning Horizon**: 12 months (Q1 2026 - Q4 2026)

---

## Vision

TAI Autonomics evolves from evaluation-mode MCP-only support to insurance-backed, multi-tenant production platform with 75+ production recipes and customer support doctrine.

---

## Phase 1: MCP-Only Evaluation (Current - Q1 2026)

**Status**: Production Ready
**Target End**: March 31, 2026

### Scope
- MCP server with 4 core tools
- HTTP endpoints for marketplace integration
- Receipt generation to stdout
- Stop-the-line safety gates
- Deterministic remediation via runbooks
- Early access partner evaluation

### Key Deliverables
- ✅ MCP server implementation (348 LOC)
- ✅ Health check tool (256 LOC)
- ✅ Entitlement gate tool (241 LOC)
- ✅ Receipt verification tool (213 LOC)
- ✅ Support model tool (205 LOC)
- ✅ Governor integration (state machine)
- ✅ Bounded action executor
- ✅ HTTP endpoints (/health, /marketplace, /pubsub)
- ✅ Docker container build
- ✅ Cloud Run deployment scripts
- ✅ Test suite (33 test cases)
- ✅ Documentation (API, install, support model, quick reference)

### Success Criteria
- MCP tools operational and tested
- Deterministic remediation validated
- Stop-the-line safety gates enforced
- Receipt chain integrity verified
- Early partners in active evaluation
- Zero critical defects in production
- Documentation complete and customer-ready

### Support Model
- Best effort response times
- No SLA commitment
- MCP tools only (no human support)
- Evaluation/free tier

### Pricing
- Free (evaluation tier)
- No billing, no contracts

---

## Phase 2: Insurance-Backed Production (Q2 2026)

**Target Launch**: April 1, 2026
**Duration**: April - June 2026

### Scope
Transition from evaluation to production-ready platform with:
- Firestore receipt persistence
- Multi-tenant billing
- Contracted SLAs
- Insurance-backed support guarantees
- Root cause analysis recipes
- Kaizen-driven improvement cycle

### Key Deliverables

#### Infrastructure
- Firestore schema and data model
- Receipt ledger migrations
- Multi-tenant isolation and billing
- Cloud Monitoring dashboards
- Alerting and PagerDuty integration
- Backup and disaster recovery

#### Support Operations
- Insurance underwriting
- SLA contract templates
- Escalation procedures
- 24/7 on-call rotation (Enterprise tier)
- Root cause analysis workflow
- Kaizen improvement loop

#### Product
- Tier-based feature matrix
- Rate limiting and quota enforcement
- Usage analytics and reporting
- Custom recipe capability
- Advanced gates and policies

#### Documentation
- Phase 2 operations runbook
- SLA definitions and enforcement
- Billing and usage guide
- Incident response procedures
- Root cause analysis templates
- Migration guide from Phase 1

### Support Model
- **Standard Tier**: 2-hour response, 99.5% uptime SLA
- **Professional Tier**: 1-hour response, 99.9% uptime SLA
- **Enterprise Tier**: 15-minute response, 99.99% uptime SLA, 24/7

### Pricing (Estimated)
- **Standard**: $499/month + usage (first month free for Phase 1 participants)
- **Professional**: $1,999/month + usage
- **Enterprise**: Custom (starting at $9,999/month)
- Usage: $0.01 per action/decision/receipt

### Success Criteria
- Firestore fully operational with 99.9% uptime
- Multi-tenant billing proven with 3+ customers
- Insurance policy active and claims process validated
- SLA compliance: >99.5% for Standard tier
- Zero escalations to insurance
- 10+ recipes in standard work library
- Kaizen improvements deployed to >50% of install base

---

## Phase 3: Support Doctrine Deployment (Q3 2026)

**Target Launch**: July 1, 2026
**Duration**: July - September 2026

### Scope
Mature support system with:
- 75+ production recipes
- Autonomous Kaizen cycle
- Customer success metrics
- Advanced diagnosis capabilities
- Cross-tenant learning
- Continuous improvement

### Key Deliverables

#### Recipes
- 20 new infrastructure recipes (Cloud Run, Pub/Sub, Firestore)
- 15 new data recipes (backup, migration, retention)
- 15 new entitlement recipes (quota, tier, gate management)
- 15 new troubleshooting recipes (common errors, resolutions)
- 10 new optimization recipes (performance, cost, reliability)
- Plus 75 total curated in standard work library

#### Observability
- OTEL trace instrumentation
- Metrics dashboard expansion
- Log aggregation and search
- Trace-to-receipt correlation
- Kaizen metrics tracking

#### Customer Success
- Customer health scores
- Predictive failure detection
- Proactive recommendation engine
- Usage trend analysis
- Cost optimization alerts

#### Process Improvements
- Kaizen submission workflow
- Continuous recipe updates
- Metric-driven prioritization
- Cross-tenant learning share (anonymized)
- Prevention test automation

### Support Model
Enhanced with:
- 24/7 availability (Enterprise)
- Proactive monitoring and alerting
- Predictive failure prevention
- Custom recipe development
- Engineering resource access

### Pricing
- **Standard**: $599/month + usage (10% discount for Phase 2 customers)
- **Professional**: $2,299/month + usage (10% discount)
- **Enterprise**: Custom (10% discount)
- Usage: $0.01 per action (same as Phase 2)

### Success Criteria
- 75 recipes in production
- Average diagnosis latency <100ms
- Recipe success rate >95%
- Customer health scores improving month-over-month
- Kaizen submissions from >80% of customers
- Proactive alerts prevent >60% of potential issues
- Cross-tenant learning integrated into 20 recipes

---

## Phase 4: Capability Packs & Variants (Q4 2026)

**Target Launch**: October 1, 2026
**Duration**: October - December 2026

### Scope
Specialized product variants for different markets:
- Marketplace capability packs
- Vertical-specific variants
- Partner ecosystem
- Licensing models

### Key Deliverables

#### Marketplace Packs
- **FinTech Pack**: Enhanced security, audit, compliance recipes
- **Healthcare Pack**: HIPAA, data residency, security gate recipes
- **Enterprise Pack**: Advanced governance, delegation, audit recipes
- **Startup Pack**: Cost optimization, quota management, scaling recipes

#### Vertical Variants
- **Banking & Finance**: Specialized entitlement gates, audit controls
- **Healthcare**: Privacy gates, data residency enforcement
- **Government**: Advanced compliance, retention, decommissioning
- **Enterprise SaaS**: Multi-org delegation, policy inheritance

#### Partnerships
- Cloud provider integrations (GCP, AWS, Azure)
- Observability integrations (DataDog, New Relic, Splunk)
- Ticketing system integrations (Jira, Zendesk)
- Custom SI partnerships

#### Licensing
- Perpetual licenses for early customers
- Tiered license models
- Volume discounts
- Partner channel program

### Support Model
Variant-specific:
- **FinTech Pack**: Compliance-focused, audit trail, regulatory liaison
- **Healthcare Pack**: Privacy officer support, breach response team
- **Enterprise Pack**: Compliance manager, policy development, training
- **Startup Pack**: Growth-focused, scaling advisory, cost reduction

### Pricing
- Base tier pricing + pack premium (15-30% premium)
- Volume discounts: 10% at 5+ instances, 20% at 20+ instances
- Partner pricing: 30-40% margin

### Success Criteria
- 4+ capability packs launched
- 10+ vertical partnerships signed
- 100+ customers across variants
- ARR $1M+ across all variants
- NPS >70 across all packs
- <10% churn rate

---

## Quarterly Milestones

### Q1 2026 (Current)
- ✅ Phase 1 production ready
- ✅ MCP server operational
- ✅ Early partners in evaluation
- → Target: 3-5 active evaluation partners

### Q2 2026
- Firestore implementation
- Multi-tenant billing
- Insurance underwriting
- Phase 2 launch (April 1)
- SLA contracts active
- → Target: 10-15 Phase 2 customers

### Q3 2026
- 75+ recipes deployed
- Kaizen cycle operational
- Customer health metrics
- Phase 3 launch (July 1)
- Support doctrine live
- → Target: 30-50 customers

### Q4 2026
- Marketplace packs launched
- Vertical variants available
- Partnership program active
- Phase 4 launch (October 1)
- 100+ customers
- → Target: $1M ARR

---

## Investment & Resource Plan

### Phase 1 (Q1 2026)
- **Team**: 3 engineers (core dev), 1 PM, 1 sales
- **Infrastructure**: GCP Cloud Run, Pub/Sub, Cloud Build
- **Cost**: ~$5K/month ops + 6 FTE (~$120K salary burn)

### Phase 2 (Q2 2026)
- **Team**: +2 ops engineers, +1 support engineer, +1 insurance specialist
- **Infrastructure**: Firestore, Cloud Monitoring, +regional replicas
- **Cost**: ~$15K/month ops + 10 FTE (~$200K salary burn)

### Phase 3 (Q3 2026)
- **Team**: +1 recipe engineer, +1 customer success manager
- **Infrastructure**: OTEL backend, metrics store, expanded monitoring
- **Cost**: ~$25K/month ops + 12 FTE (~$240K salary burn)

### Phase 4 (Q4 2026)
- **Team**: +2 SI engineers (partnerships), +1 vertical sales
- **Infrastructure**: Multi-region, partner integrations
- **Cost**: ~$40K/month ops + 15 FTE ($300K salary burn)

**Projected Cumulative Spending**: $900K (2026)
**Target ROI Break-Even**: Q3 2026 (if Phase 3 pricing realized)

---

## Technology Roadmap

### Runtime & Language
- Erlang/OTP 26+ (current: stable)
- Potential future: Elixir wrapper (2027+)

### Data Storage
- Phase 1: In-memory + stdout
- Phase 2: Firestore (primary)
- Phase 3: Multi-region Firestore, Cloud Datastore options
- Phase 4: Customer-managed storage options (S3, GCS)

### Observability
- Phase 1: Cowboy access logs, JSON structured logging
- Phase 2: Cloud Logging, Cloud Monitoring, basic metrics
- Phase 3: Full OTEL instrumentation, trace correlation
- Phase 4: Custom observability backends, partner integrations

### Compute
- Phase 1: Single Cloud Run (2 vCPU, 4GB RAM)
- Phase 2: Auto-scaling group (2-10 instances)
- Phase 3: Regional deployment (3 regions)
- Phase 4: Global deployment (6+ regions) + edge

### Security
- Phase 1: Basic authentication (API key placeholder)
- Phase 2: mTLS, service accounts, IAM
- Phase 3: SAML/OIDC, advanced IAM, encryption at rest
- Phase 4: HSM-backed signing, compliance-specific encryption

---

## Risk Mitigations

### Market Risk
- **Risk**: Customers unwilling to transition from Phase 1 to Phase 2
- **Mitigation**: Free trial extension (90 days), clear migration path, no surprise pricing

### Technical Risk
- **Risk**: Firestore performance degrades under load
- **Mitigation**: Load testing Phase 2 with 100+ concurrent tenants, Spanner fallback
- **Risk**: Insurance claims increase unexpectedly
- **Mitigation**: Cap claims at 10% of revenue Year 1; reinsurance for large claims

### Operational Risk
- **Risk**: Recipe quality varies; some cause more issues
- **Mitigation**: Peer review process, canary deployment (5% of install base first)

### Competitive Risk
- **Risk**: Competitors offer cheaper/similar service
- **Mitigation**: Focus on deterministic + insurance (hard to copy); build brand moat

---

## Success Metrics

### Phase 1
- 5 active evaluation partners
- 100% uptime (within Phase 1 constraints)
- Zero critical defects
- Customer satisfaction >4/5

### Phase 2
- 15 Phase 2 customers by Q2 end
- 99.5% uptime SLA compliance
- <2% SLA breach rate
- Customer satisfaction >4.2/5
- Gross margin >70%

### Phase 3
- 50 customers by Q3 end
- 75 recipes in production
- Kaizen submissions from 40+ customers
- Recipe success rate >95%
- Customer satisfaction >4.3/5
- Net retention >110%

### Phase 4
- 100 customers by Q4 end
- $1M ARR
- 4+ capability packs
- NPS >70
- Gross margin >75%
- Net retention >120%

---

## Dependency Timeline

```
Phase 1 (Now)
  ↓
Phase 2 (Apr 1) ← Requires: Firestore, insurance, SLA contracts
  ↓
Phase 3 (Jul 1) ← Requires: OTEL, recipes, Kaizen process
  ↓
Phase 4 (Oct 1) ← Requires: Packs, verticals, partnerships
```

All phases are on the critical path. Delays in Phase 2 delay Phases 3 & 4.

---

## Document Control

| Field | Value |
|-------|-------|
| **Version** | 1.0 |
| **Audience** | Investors, customers, internal team |
| **Last Review** | 2026-01-26 |
| **Next Review** | 2026-04-01 (Phase 2 launch readiness) |

---

## Appendix: Feature Matrix by Phase

| Feature | Phase 1 | Phase 2 | Phase 3 | Phase 4 |
|---------|---------|---------|---------|---------|
| **MCP Tools** | 4 | 4 | 6+ | 8+ |
| **HTTP Endpoints** | 3 | 3 | 4 | 5+ |
| **Recipes** | 5 (basic) | 20 | 75 | 100+ |
| **Multi-Tenant** | No | Yes | Yes | Yes |
| **SLA** | None | 99.5%+ | 99.5%-99.99% | 99.5%-99.99% |
| **Storage** | Stdout | Firestore | Firestore+ | Multi-backend |
| **Insurance** | No | Yes | Yes | Yes |
| **Regions** | 1 | 1 | 3 | 6+ |
| **Auto-Scaling** | Manual | 2-10 | 2-20 | 5-50 |
| **24/7 Support** | No | Enterprise only | Enterprise | Professional+ |
| **Custom Recipes** | No | No | Yes (Enterprise) | Yes (all tiers) |
| **Kaizen Cycle** | No | Manual | Automated | Predictive |

---

**Created by**: Documentation Specialist (Agent 19)
**For**: TAI Autonomics Marketplace Launch
**Distribution**: Public / Marketplace listings
