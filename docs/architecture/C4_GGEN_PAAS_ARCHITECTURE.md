# ggen PaaS - C4 Architecture Documentation

## Overview

This document describes the complete architecture of **ggen PaaS** (Platform-as-a-Service) using the C4 model for visualizing software architecture. The C4 model consists of four nested levels of diagrams, each providing increasing detail.

---

## C1: System Context Diagram

**File**: `c4_ggen_paas.puml`

### Purpose
Shows the highest-level view of the ggen PaaS system, external actors, and external systems it interacts with.

### Key Elements

#### Actors
- **Development Team**: Uses ggen PaaS to generate code from RDF specifications
- **Platform Admin**: Deploys, maintains, and manages the ggen PaaS infrastructure

#### Central System
- **ggen PaaS**: Semantic code generation platform powered by RDF, SPARQL, and Tera templates

#### External Systems
1. **RDF Store (Oxigraph)**: Persistent triple store for specifications and ontologies
2. **Git Repository**: Version control for generated code and specifications
3. **Package Registry** (Cargo, npm): Distribution of generated packages
4. **CI/CD Pipeline** (GitHub Actions): Automated testing and deployment

### Key Relationships
- Development teams submit RDF specs and templates to ggen PaaS
- Platform admins deploy and maintain the system
- ggen PaaS stores and queries specifications in the RDF store
- Generated code is committed to Git repositories
- Packages are published to registries (Cargo for Rust, npm for Node.js)
- Spec changes trigger CI/CD pipelines for automated validation

---

## C2: Container Diagram

**File**: `c4_ggen_containers.puml`

### Purpose
Details the major structural building blocks and internal components that form the ggen PaaS system.

### Container Architecture

#### Frontend Layer
- **Web UI** (TypeScript/React)
  - Interactive spec editor
  - Code generation dashboard
  - Project management interface

#### API & Authentication Layer
- **API Gateway** (Rust/Axum)
  - REST API for spec management
  - Code generation orchestration
  - Request routing and validation

- **Auth Service** (Rust/JWT/OAuth2)
  - User authentication
  - Token management
  - Authorization policies

#### Core Generation Engine
- **ggen Engine** (Rust)
  - Orchestrates the five-stage transformation pipeline
  - Coordinates all generation components

- **RDF Processor** (Rust/Oxigraph)
  - Parses Turtle and N3 RDF formats
  - Validates specifications
  - Manages RDF graph state

- **SPARQL Engine** (Rust/Oxigraph)
  - Executes CONSTRUCT patterns
  - Extracts data from RDF specifications
  - Query optimization and caching

- **Template Engine** (Rust/Tera)
  - Renders Jinja2-like templates
  - Injects extracted data
  - Generates source code

- **Code Generator** (Rust)
  - Output validation
  - Code formatting
  - Language-specific optimizations

#### Background Processing
- **Job Scheduler** (Bree/Node.js)
  - Manages background code generation jobs
  - Handles scheduled tasks
  - Job queue management

- **Notification Service** (Node.js)
  - Event notifications
  - Webhook integrations
  - Real-time updates to clients

#### Data Layer
- **Specification Store** (Oxigraph RDF Store)
  - Persistent RDF ontologies
  - Job specifications
  - Domain models

- **Metadata DB** (PostgreSQL)
  - User accounts
  - Projects
  - Audit logs
  - Generation history

- **Cache** (Redis)
  - Generated code caching
  - Session storage
  - Query result caching

#### External Integrations
- **Git Integration** (GitHub API)
  - Commits generated code
  - Creates branches/PRs
  - Manages repository state

- **Registry Integration** (Cargo/npm API)
  - Publishes generated packages
  - Version management
  - Dependency resolution

### Data Flow
1. User submits RDF spec via Web UI
2. API Gateway validates and routes request
3. ggen Engine orchestrates generation pipeline
4. RDF Processor parses and validates specifications
5. SPARQL Engine extracts data using CONSTRUCT patterns
6. Template Engine renders code with extracted data
7. Code Generator validates and formats output
8. Job Scheduler queues async generation jobs
9. Results are cached in Redis
10. Generated code is committed to Git
11. Packages are published to registries

---

## C3: Component Diagram

**File**: `c4_ggen_components.puml`

### Purpose
Shows the internal structure of the most critical containers, revealing the fine-grained components and their dependencies.

### Component Boundaries

#### ggen-core Container
**Core RDF and graph processing**

- **Graph** (Rust)
  - In-memory RDF graph representation
  - Triple storage and retrieval
  - Index management

- **Query Executor** (Rust)
  - SPARQL query execution engine
  - Pattern matching
  - Result set construction

- **RDF Parser** (Rust/n3-Turtle)
  - Parse Turtle (TTL) format
  - Parse N3 format
  - Namespace/prefix resolution

#### Code Generation Container
**Five-stage transformation pipeline**

- **Spec Validator** (Rust/SHACL)
  - Validate specs against shapes
  - Constraint checking
  - Schema conformance

- **Normalization** (Rust)
  - Expand namespace prefixes
  - Merge multiple RDF files
  - Flatten graph hierarchies

- **Extraction** (Rust)
  - Execute SPARQL CONSTRUCT patterns
  - Transform RDF to structured data
  - Map ontology to code model

- **Emission** (Rust/Tera)
  - Render templates with extracted data
  - Generate source code
  - Apply language-specific rules

- **Canonicalization** (Rust)
  - Format and validate output
  - Ensure deterministic results
  - Apply code style rules

- **Receipt Generator** (Rust/JSON)
  - Generate verification manifests
  - Create audit trails
  - Produce checksums

#### Infrastructure Container
**Cross-cutting concerns**

- **API Framework** (Rust/Axum)
  - HTTP server and routing
  - Middleware stack
  - Request/response handling

- **Job Queue** (Redis/RabbitMQ)
  - Async job processing
  - Task distribution
  - Retry logic

- **State Management** (PostgreSQL)
  - Project state persistence
  - User state
  - Generation history

- **Logging & Tracing** (Rust/tracing)
  - Observability instrumentation
  - Distributed tracing
  - Performance metrics

#### Marketplace Container
**Community and monetization**

- **Template Registry** (Rust/DB)
  - Community templates
  - Ontology discovery
  - Version management

- **Monetization** (Rust)
  - Billing and usage tracking
  - Subscription management
  - Revenue analytics

- **Discovery Engine** (Rust)
  - Search and recommendation
  - Template ranking
  - Usage analytics

#### Example Frameworks Container
**Reference implementations**

- **Bree Scheduler** (Node.js/Rust)
  - Job scheduling framework
  - Semantic-driven scheduler
  - Integration example

- **API Endpoint** (Rust)
  - RESTful API generation
  - OpenAPI integration
  - Client SDK generation

- **CLI Tool** (Rust)
  - Command-line application generation
  - Clap integration
  - Subcommand generation

### Component Dependencies

The pipeline flows sequentially through generation stages:

```
Normalization → Validation → Extraction → Emission → Canonicalization → Receipt
     ↓               ↓            ↓           ↓             ↓               ↓
  Merges      Validates      Uses RDF    Renders      Formats         Verifies
  files       against        SPARQL      templates    output          output
              shapes         queries
```

Cross-component dependencies:

- **RDF Parser** → **Graph**: Populates the in-memory graph
- **Graph** ← **Query Executor**: Queries the graph
- **Spec Validator** → **RDF Parser**: Validates parsed input
- **Extraction** → **Query Executor**: Uses SPARQL for data extraction
- **Emission** → **Extraction**: Consumes extracted data
- **Template Registry** → **Emission**: Provides template libraries
- **Logging** → **API Framework**: Instruments all operations

---

## C4: Deployment Diagram

**File**: `c4_ggen_deployment.puml`

### Purpose
Shows how the software system is deployed across physical infrastructure, including servers, containers, databases, and networks.

### Infrastructure Topology

#### Content Delivery
- **CDN** (AWS CloudFront)
  - Serves static assets globally
  - Web bundles and JavaScript
  - Caching and edge distribution

#### Compute Layer (AWS ECS)

**Load Balancer** (ALB - Application Load Balancer)
- Routes HTTPS traffic to services
- Health checking
- SSL/TLS termination

**Application Tier** (ECS Cluster)
- **API Service** (Docker container)
  - Rust/Axum API server
  - Handles spec submission and generation requests
  - Horizontal auto-scaling

- **Web Service** (Docker container)
  - React frontend
  - TypeScript compilation
  - Static asset serving

**Worker Tier** (ECS Cluster)
- **Job Worker** (Docker container)
  - Code Generator (Rust)
  - Async generation pipeline
  - Long-running jobs

- **Scheduler Worker** (Docker container)
  - Bree Scheduler (Node.js)
  - Job orchestration
  - Cron and interval scheduling

#### Data Tier

**Relational Database** (RDS)
- PostgreSQL cluster
- User data and metadata
- Audit logs
- Project state

**NoSQL Store** (DynamoDB)
- Specification cache
- RDF graph snapshots
- Session data

**Cache Tier** (ElastiCache)
- Redis cluster
- Generated code cache
- Session storage
- Query result caching

**Graph Database** (Neptune)
- Oxigraph RDF Store backend
- Persistent triple store
- SPARQL query index

#### Storage
- **Object Storage** (S3)
  - Generated code artifacts
  - Template libraries
  - Backup/archival

#### Message Queue
- **SQS** (Simple Queue Service)
  - Job queue for async tasks
  - Decouples API from workers
  - Retry handling

#### External Services Integration
- **GitHub API**
  - Repository management
  - Code commits
  - PR creation

- **Package Registries**
  - Crates.io (Rust packages)
  - npm Registry (Node.js packages)
  - Published artifacts

### Deployment Patterns

#### High Availability
- Multi-AZ deployments
- Auto-scaling groups
- RDS read replicas
- ElastiCache failover

#### Disaster Recovery
- S3 cross-region replication
- Database automated backups
- CloudFront distribution
- SQS dead-letter queues

#### Monitoring & Observability
- CloudWatch metrics
- X-Ray distributed tracing
- VPC Flow Logs
- Application-level logging

---

## Architectural Principles

### 1. Specification-First (Big Bang 80/20)
- RDF specifications are the source of truth
- Specifications are validated for closure before generation
- Changes to `.ttl` files trigger pipeline execution
- Generated code is deterministic and reproducible

### 2. Deterministic Outputs
- Same input → Same output (bit-perfect)
- No random elements in code generation
- Reproducible builds across all environments
- Cryptographic receipt validation

### 3. RDF-First Semantics
- All domain logic is expressed in RDF/Turtle
- SPARQL patterns encode extraction rules
- Ontologies drive code structure
- Semantic validation ensures correctness

### 4. Zero-Cost Abstractions
- Rust for all performance-critical paths
- No unnecessary serialization
- Memory-efficient RDF graph storage
- Fast SPARQL query execution

### 5. Chicago TDD
- Real objects (no mocks) in tests
- State-based assertions
- Observable behavior verification
- Integration test coverage

### 6. Poka-Yoke Error Proofing
- Specification validation before generation
- SHACL shape constraints
- Type-safe Rust error handling
- Automatic andon signals for SLO violations

---

## Integration Points

### Web UI → API Gateway
- REST API calls
- OAuth2 token exchange
- WebSocket for real-time updates

### API Gateway → ggen Engine
- Orchestration pattern
- Queue-based async jobs
- Result caching

### ggen Engine → RDF Store
- Turtle file upload and parsing
- SPARQL query execution
- Graph updates

### Job Scheduler → Code Generator
- Queue-based job distribution
- Progress tracking
- Retry and fallback handling

### Code Generator → Git Integration
- Commit generated code
- Create feature branches
- Push to repository

### Notification Service → Web UI
- WebSocket events
- Job completion notifications
- Error alerts

---

## Technology Stack by Layer

| Layer | Technology | Purpose |
|-------|-----------|---------|
| **Frontend** | TypeScript, React, Tailwind | Interactive UI |
| **API Gateway** | Rust, Axum, Tokio | HTTP server, async runtime |
| **Auth** | JWT, OAuth2, Argon2 | Secure authentication |
| **RDF Processing** | Oxigraph, n3-rs | RDF storage, SPARQL |
| **Code Generation** | Tera, Proc Macros | Template rendering |
| **Job Scheduler** | Bree (Node.js), Redis | Background jobs |
| **Database** | PostgreSQL | Relational data |
| **Cache** | Redis | Session, query cache |
| **Graph DB** | Neptune/Oxigraph | RDF triple store |
| **Storage** | S3 | Generated artifacts |
| **Queue** | SQS | Job distribution |
| **Monitoring** | CloudWatch, Tracing | Observability |

---

## Rendering the Diagrams

To render the PlantUML diagrams:

```bash
# Install PlantUML
brew install plantuml  # macOS
# or apt-get install plantuml  # Linux

# Render to PNG
plantuml c4_ggen_paas.puml
plantuml c4_ggen_containers.puml
plantuml c4_ggen_components.puml
plantuml c4_ggen_deployment.puml

# Render to SVG (recommended for web)
plantuml -tsvg c4_ggen_paas.puml
plantuml -tsvg c4_ggen_containers.puml
plantuml -tsvg c4_ggen_components.puml
plantuml -tsvg c4_ggen_deployment.puml
```

### Online Rendering
Visit [PlantUML Online Editor](https://www.plantuml.com/plantuml/uml/) and paste the diagram content.

---

## Next Steps

### Specification Development
1. Create `.specify/ggen-paas-ontology.ttl` - Define the ggen PaaS domain model
2. Define Bree scheduler job templates
3. Document marketplace ontology
4. Create example generation specs

### Implementation Priorities
1. **Phase 1**: Core ggen Engine with RDF/SPARQL support
2. **Phase 2**: Bree Scheduler integration for async jobs
3. **Phase 3**: Marketplace and template registry
4. **Phase 4**: Monetization and billing system
5. **Phase 5**: Advanced observability and optimization

### Testing Strategy
- Unit tests for each component (Chicago TDD)
- Integration tests for data flows
- End-to-end tests with real RDF specs
- Performance benchmarks for SPARQL queries

### Documentation
- Ontology reference guide
- API specification (OpenAPI 3.0)
- User guides for each generation pattern
- Architecture decision records (ADRs)

---

## References

- [C4 Model Documentation](https://c4model.com/)
- [PlantUML C4 Extensions](https://github.com/RicardoNiepel/C4-PlantUML)
- [ggen Core Architecture](GGEN_ARCHITECTURE_OVERVIEW.md)
- [Bree Scheduler Integration](../../examples/bree-semantic-scheduler/)
- [SPARQL Query Guide](SPARQL_INFERENCE_GUIDE.md)
