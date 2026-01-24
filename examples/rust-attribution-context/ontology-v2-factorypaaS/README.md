# FactoryPaaS Ontology v2.0 - Affiliate Marketing Platform

## Overview

This ontology defines a comprehensive **FactoryPaaS** affiliate marketing platform using RDF/Turtle following the TCPS (Type-Code-Policy-System) paradigm. The ontology serves as the **single source of truth** for code generation, infrastructure provisioning, and business logic.

## Core Equation

**A = μ(O)** — Code (A) precipitates from RDF ontology (O) via five-stage transformation pipeline (μ).

The ontology IS the application. All code, infrastructure, policies, and observability are generated deterministically from these TTL files.

## Domain Model

### Bounded Context: FactoryPaaS

**Vision**: AI-powered platform for generating, deploying, and monetizing SEO-optimized affiliate sites on GCP infrastructure.

**Core Workflow**:
1. **Create Site** → Initialize affiliate site with niche targeting
2. **Generate Content** → AI-powered SEO content creation with sideways keyword strategy
3. **Publish Pages** → Deploy optimized pages with affiliate links
4. **Deploy to GCP** → Cloud Run + CDN + Storage infrastructure
5. **Track Clicks** → Real-time outbound click tracking with attribution
6. **Record Revenue** → Commission tracking across affiliate networks

## Ontology Files

### 1. `factorypaaS.ttl` - Bounded Context & Invariants

**Purpose**: Define the bounded context, ubiquitous language, domain invariants, and business rules.

**Key Concepts**:
- **Site**: Affiliate website targeting specific niche
- **NichePage**: SEO-optimized content page with affiliate links
- **SideWaysKeyword**: Low-competition, high-intent keyword strategy
- **AffiliateLinkRoute**: Tracked URL for click attribution
- **ConversionFlow**: User journey from landing to affiliate click
- **InternalLinkGraph**: Contextual linking for topical authority

**Domain Invariants**:
- Unique page slugs per site
- Valid affiliate link format with tracking parameters
- Minimum 800-word content for SEO effectiveness
- Keyword density between 0.5% - 2.5%
- Maximum 3-click internal link depth
- Content freshness requirement (90-day update cycle)

### 2. `entities.ttl` - Domain Entities & Value Objects

**Purpose**: Define the complete domain model with type-safe field specifications.

**Entities**:
- **Site** (Aggregate Root)
  - Domain, niche, GCP deployment metadata
  - Revenue, click, and page counters
  - Cloud Run service, CDN, storage bucket references

- **NichePage** (Entity)
  - SEO metadata (title, keywords, meta description, canonical URL)
  - Content (Markdown source, HTML output, word count)
  - Affiliate links, internal links
  - Analytics (clicks, conversions, revenue)

- **ContentCluster** (Entity)
  - Pillar page + supporting cluster pages
  - Topical authority scoring

- **CommissionRecord** (Entity)
  - Click-to-commission attribution
  - Affiliate network transaction tracking
  - Status workflow (pending → approved → paid)

**Value Objects**:
- **SideWaysKeyword**: Difficulty score, search volume, CPC, intent
- **AffiliateLinkRoute**: Tracking slug, destination URL, commission rate
- **InternalLink**: Source/target pages, semantic relevance score

### 3. `commands.ttl` - Write Operations

**Purpose**: Define CQRS commands that trigger domain events.

**Key Commands**:
- **CreateSite**: Initialize new affiliate site
- **GenerateContent**: AI content generation with keyword targeting
- **PublishPage**: Transition draft → published with HTML rendering
- **DeployToGCP**: Infrastructure provisioning (Cloud Run, Storage, CDN)
- **TrackOutboundClick**: Record affiliate link click with user metadata
- **OptimizeKeywords**: Sideways keyword research and selection
- **BuildInternalLinks**: Semantic internal linking structure
- **RecordCommission**: Manual commission entry from affiliate networks
- **ScheduleContentGeneration**: Automated content pipeline

### 4. `events.ttl` - Domain Events

**Purpose**: Define state change events emitted by aggregates.

**Key Events**:
- **SiteCreated**: New site initialized
- **ContentGenerated**: AI content creation completed
- **PagePublished**: Page live on production
- **SiteDeployed**: GCP infrastructure provisioned
- **ClickTracked**: Outbound click recorded
- **CommissionRecorded**: Revenue attributed to page/link
- **SaaSSubscriptionCreated**: Platform subscription activated
- **KeywordsOptimized**: Keyword research completed
- **InternalLinksBuilt**: Link graph constructed

### 5. `policies.ttl` - Business Rules & SEO Strategy

**Purpose**: Define business rules, SEO policies, and operational constraints.

**Key Policies**:

#### Keyword Targeting Policy
- Low competition (difficulty < 30)
- Minimum search volume (100/month)
- Commercial/transactional intent priority
- Unique primary keyword per page

#### Internal Linking Policy
- Max 3-click depth from homepage
- Semantic relevance threshold (0.7 cosine similarity)
- 3-7 internal links per page
- Cluster pages link to pillar

#### Conversion Flow Policy
- 300+ words before first affiliate link
- Primary link in top 30% of content
- Action-oriented CTA (not "click here")
- Mandatory FTC affiliate disclosure

#### SEO Optimization Policy
- Minimum 800-word content
- Optimal keyword density (0.5% - 2.5%)
- Meta description length (150-160 chars)
- Title tag optimization (50-60 chars, includes keyword)
- Canonical URL enforcement

#### Content Freshness Policy
- 90-day update cycle
- 180-day review flag for outdated content
- Annual refresh for year-specific content

#### Revenue Attribution Policy
- 30-day attribution window
- Last-click attribution model
- Cross-site attribution isolation

#### Content Quality Policy
- Minimum AI quality score (0.7)
- Plagiarism threshold (< 15% similarity)
- Readability score (60-70 Flesch Reading Ease)
- Verifiable citations for statistics

### 6. `infra.ttl` - GCP Infrastructure Topology

**Purpose**: Define cloud infrastructure using TOGAF architecture building blocks.

**GCP Services**:

#### Compute
- **Cloud Run (factorypaaS-api)**: Main API service, 0-100 autoscaling
- **Cloud Run (content-gen)**: AI content generation, 1-50 instances
- **Cloud Run (site-renderer)**: Public site serving, 0-200 instances

#### Storage
- **Cloud Storage (content-{site-id})**: Per-site content buckets
- **Cloud Storage (assets-cdn)**: Global CDN-backed assets
- **Cloud Storage (backups)**: Nearline encrypted backups

#### Database
- **Cloud SQL (PostgreSQL 15)**: Primary datastore with HA
- **Memorystore Redis**: Session cache, rate limiting

#### Messaging
- **Pub/Sub (events)**: Domain event bus
- **Pub/Sub (clicks)**: High-throughput click tracking
- **Pub/Sub (content-jobs)**: Async content generation queue

#### Networking
- **Cloud CDN**: Global edge caching
- **Cloud Load Balancing**: HTTPS with SSL termination
- **Cloud Armor**: WAF with SQLi/XSS protection
- **VPC**: Private networking for Cloud SQL/Redis

#### Observability
- **Cloud Monitoring**: SLO alerting (PagerDuty, Slack)
- **Cloud Trace**: Distributed request tracing

#### Security
- **Secret Manager**: Credentials, API keys, DB passwords
- **Cloud Armor**: Rate limiting (100 req/min per IP)

#### Automation
- **Cloud Scheduler**: Daily content generation (2am PST)
- **Cloud Functions**: Sitemap generation, image optimization

### 7. `observability.ttl` - OpenTelemetry Instrumentation

**Purpose**: Define comprehensive observability with traces, metrics, and logs.

**Traces**:
- **CreateSiteTrace**: Site initialization end-to-end
- **GenerateContentTrace**: AI content generation pipeline
- **TrackClickTrace**: Click tracking → event publish → redirect
- **DeploymentTrace**: Container build → push → Cloud Run update

**Metrics** (Business KPIs):
- `factorypaaS.sites.created` (Counter)
- `factorypaaS.pages.published` (Counter)
- `factorypaaS.clicks.tracked` (Counter)
- `factorypaaS.commission.amount` (Histogram)
- `factorypaaS.revenue.per_site` (Gauge)
- `factorypaaS.seo.keyword_rank` (Gauge)
- `factorypaaS.conversion.rate` (Gauge)

**Metrics** (Technical):
- `factorypaaS.content.generation.duration` (Histogram)
- `factorypaaS.llm.tokens.consumed` (Counter)
- `factorypaaS.deployment.duration` (Histogram)
- `factorypaaS.api.error_rate` (Gauge)
- `factorypaaS.cache.hit_ratio` (Gauge)

**Sampling Strategy**:
- 100% errors and slow requests (> 2s)
- 10% normal traffic

**SLOs**:
- **Availability**: 99.9% uptime (30-day window)
- **Latency**: p95 < 500ms (7-day window)
- **Error Rate**: < 1% (24-hour window)

**Logs**:
- **AuditLog**: Compliance and security audit trail
- **ErrorLog**: Application errors with stack traces
- **PerformanceLog**: Performance monitoring (DEBUG level)

## Sideways SEO Strategy

**Philosophy**: Target "sideways keywords" — low-competition, high-intent phrases that established competitors overlook.

**Selection Criteria**:
- **Difficulty**: < 30 (easy to rank)
- **Search Volume**: 100+ monthly searches (viable traffic)
- **Intent**: Commercial or transactional (ready to buy)
- **SERP Features**: Identify featured snippet opportunities

**Content Strategy**:
1. Generate 800+ word pillar content around primary keyword
2. Build content cluster with 5-10 supporting pages
3. Semantic internal linking (0.7+ relevance score)
4. Embed affiliate links in top 30% of content
5. FTC disclosure compliance

**Topical Authority**:
- Cluster pages link to pillar
- Maximum 3-click depth from homepage
- 3-7 internal links per page
- Regular content freshness updates (90-day cycle)

## Revenue Model

### Affiliate Networks
- **Impact**: SaaS affiliate programs
- **ShareASale**: Digital products, subscriptions
- **CJ Affiliate**: Enterprise tools

### Attribution
- **Model**: Last-click attribution
- **Window**: 30 days from click to commission
- **Tracking**: SHA-256 hashed IP + session tracking

### Commission Flow
1. User clicks affiliate link → `ClickTracked` event
2. User converts on merchant site
3. Affiliate network webhook → `RecordCommission` command
4. `CommissionRecorded` event → Update page/site revenue

### Platform Monetization
- **Starter**: $49/month (5 sites, 100 pages)
- **Professional**: $149/month (20 sites, 500 pages)
- **Enterprise**: $499/month (unlimited sites, 5000 pages)

## Code Generation Workflow

### Step 1: Ontology → Domain Model
```bash
ggen sync --ontology factorypaaS.ttl,entities.ttl,commands.ttl,events.ttl
```

**Generates**:
- `src/domain/site.rs` (Site aggregate)
- `src/domain/niche_page.rs` (NichePage entity)
- `src/domain/sideways_keyword.rs` (SideWaysKeyword value object)
- `src/domain/affiliate_link_route.rs` (AffiliateLinkRoute value object)
- `src/commands/create_site.rs` (CreateSite command handler)
- `src/events/site_created.rs` (SiteCreated event)

### Step 2: Policies → Business Logic
```bash
ggen sync --ontology policies.ttl
```

**Generates**:
- `src/policies/keyword_targeting.rs` (Keyword selection rules)
- `src/policies/internal_linking.rs` (Link graph construction)
- `src/policies/seo_optimization.rs` (Content validation)
- `src/policies/revenue_attribution.rs` (Commission attribution)

### Step 3: Infrastructure → Terraform/Pulumi
```bash
ggen sync --ontology infra.ttl --output-format terraform
```

**Generates**:
- `infra/cloud_run.tf` (Cloud Run services)
- `infra/storage.tf` (GCS buckets)
- `infra/pubsub.tf` (Pub/Sub topics)
- `infra/monitoring.tf` (Alerting policies)

### Step 4: Observability → OpenTelemetry Instrumentation
```bash
ggen sync --ontology observability.ttl
```

**Generates**:
- `src/observability/traces.rs` (Trace instrumentation)
- `src/observability/metrics.rs` (Metric collectors)
- `src/observability/logs.rs` (Structured logging)

## Deployment

### Local Development
```bash
docker-compose up -d  # PostgreSQL, Redis
cargo run --bin factorypaaS-api
```

### GCP Production
```bash
# Deploy infrastructure
cd infra && terraform apply

# Deploy services
gcloud run deploy factorypaaS-api \
  --image gcr.io/PROJECT_ID/factorypaaS-api:latest \
  --region us-central1 \
  --allow-unauthenticated

# Deploy content generator
gcloud run deploy factorypaaS-content-gen \
  --image gcr.io/PROJECT_ID/content-generator:latest \
  --region us-central1 \
  --no-allow-unauthenticated
```

## Testing Strategy

### Chicago TDD (State-Based Testing)
- **Arrange**: Set up site, pages, affiliate links
- **Act**: Execute command (PublishPage, TrackClick)
- **Assert**: Verify state changes (page.status = "published", click_count incremented)

### Integration Tests
- Test full workflows (CreateSite → GenerateContent → PublishPage → DeployToGCP)
- Use `testcontainers` for PostgreSQL, Redis
- Mock external APIs (OpenAI, SEMrush, affiliate networks)

### Property-Based Tests
- Keyword selection always respects difficulty < 30
- Internal links always have semantic relevance > 0.7
- Commission amounts always positive

## Validation

```bash
# Validate ontology syntax
cargo make speckit-validate

# Generate diagrams
cargo make speckit-render

# Run SHACL validation
ggen validate factorypaaS.ttl entities.ttl commands.ttl events.ttl policies.ttl infra.ttl observability.ttl
```

## Architecture Diagrams

### Bounded Context Map
```
┌─────────────────────────────────────────────────────┐
│           FactoryPaaS Bounded Context               │
│                                                      │
│  ┌──────────────┐  ┌──────────────┐                 │
│  │    Site      │  │  NichePage   │                 │
│  │  Aggregate   │──│   Entity     │                 │
│  └──────────────┘  └──────────────┘                 │
│         │                  │                         │
│         │         ┌────────┴─────────┐              │
│         │         │  AffiliateLinkRoute│             │
│         │         │   (Value Object)  │             │
│         │         └──────────────────┘              │
│         │                                            │
│  ┌──────▼───────────────────────┐                   │
│  │  Revenue Aggregate           │                   │
│  │  (Click, Commission)         │                   │
│  └──────────────────────────────┘                   │
└─────────────────────────────────────────────────────┘
```

### Infrastructure Topology
```
┌───────────────────────────────────────────────────────┐
│                  Cloud Load Balancer                  │
│                   (HTTPS + SSL)                       │
└─────────────┬─────────────────────────┬───────────────┘
              │                         │
     ┌────────▼─────────┐      ┌───────▼────────────┐
     │  Cloud Run API   │      │ Cloud Run Renderer │
     │  (0-100 inst)    │      │  (0-200 inst)      │
     └────────┬─────────┘      └────────┬───────────┘
              │                         │
              │                         │
     ┌────────▼──────────────────────┬──▼───────────┐
     │                               │              │
┌────▼────┐  ┌──────▼──────┐  ┌─────▼─────┐  ┌────▼─────┐
│Cloud SQL│  │  Redis      │  │  Pub/Sub  │  │   GCS    │
│PostgeSQL│  │ Memorystore │  │  Topics   │  │ Buckets  │
└─────────┘  └─────────────┘  └───────────┘  └──────────┘
```

## License

MIT License - See LICENSE file for details.

## Contributing

1. Edit TTL files (source of truth)
2. Run `ggen sync` to regenerate code
3. Run `cargo make test` to validate
4. Submit PR with TTL changes only (generated code excluded from commits)

## Contact

- **Platform**: https://factorypaaS.io
- **Documentation**: https://docs.factorypaaS.io
- **GitHub**: https://github.com/factorypaaS/platform
- **Discord**: https://discord.gg/factorypaaS
