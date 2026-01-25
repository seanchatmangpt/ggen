# TAI Architecture Documentation Index

Complete reference for TAI system design, decisions, and implementation.

## Architecture Decision Records (ADRs)

**Strategic decisions affecting system design:**

1. **[ADR-001: gRPC vs HTTP API Architecture](../adr/ADR-001-gRPC-Architecture.md)**
   - Hybrid approach: gRPC internal, HTTP external
   - Trade-offs: Performance vs accessibility
   - Implementation: Protocol buffers, proto definitions

2. **[ADR-002: Firestore for Distributed State Management](../adr/ADR-002-Firestore-State-Management.md)**
   - Primary data store: Google Firestore (multi-region)
   - ACID transactions, real-time updates
   - Cache layer: Redis for hot data
   - Evaluation: Compared to PostgreSQL, DynamoDB, Spanner

3. **[ADR-003: Istio Service Mesh Integration](../adr/ADR-003-Istio-Service-Mesh.md)**
   - mTLS enforcement (automatic certificate rotation)
   - Traffic management (circuit breakers, retries)
   - Observability (distributed tracing, metrics)
   - Canary deployments with traffic splitting

4. **[ADR-004: Canary Deployment Strategy with KEDA](../adr/ADR-004-Canary-Deployments.md)**
   - Progressive delivery: 10% → 50% → 100% traffic
   - Metrics-based promotion: Error rate, latency, resource usage
   - Automatic rollback: <2 minutes on anomaly
   - Flagger integration for orchestration

5. **[ADR-005: Role-Based Access Control (RBAC) Implementation](../adr/ADR-005-RBAC-Implementation.md)**
   - Three-layer authorization: Kubernetes, Istio, Application
   - Zero-trust architecture (deny by default)
   - Audit logging per action
   - mTLS certificate-based service identity

6. **[ADR-006: Secret Management with Vault and Cloud KMS](../adr/ADR-006-Secret-Management.md)**
   - HashiCorp Vault as secret broker
   - Cloud KMS for root key encryption (HSM-backed)
   - Automatic credential rotation: 30-90 days
   - Zero-knowledge: App never sees unencrypted secrets at rest

7. **[ADR-007: Observability Strategy (CloudTrace, CloudProfiler, OpenTelemetry)](../adr/ADR-007-Observability-Strategy.md)**
   - Three-pillar: Metrics (Prometheus), Traces (Jaeger), Profiles (Cloud Profiler)
   - Automatic instrumentation (OpenTelemetry)
   - Distributed tracing for request flows
   - Performance monitoring and regression detection

8. **[ADR-008: Circuit Breaker Pattern for gRPC Services](../adr/ADR-008-Circuit-Breaker-Pattern.md)**
   - Dual-implementation: Istio + Application level
   - Three states: Closed (normal), Open (fail fast), Half-open (recovery test)
   - Configuration per service (failure threshold, timeout, recovery criteria)
   - Health check integration

9. **[ADR-009: Rate Limiting and Quota Implementation](../adr/ADR-009-Rate-Limiting-Quotas.md)**
   - Token bucket algorithm (Redis backing)
   - Three tiers: API Gateway, Service Mesh, Application
   - Per-client quotas (API keys, service accounts)
   - Graceful degradation (429 with Retry-After)

10. **[ADR-010: Event Sourcing for Audit Trail and Event Log](../adr/ADR-010-Event-Sourcing.md)**
    - Immutable event log (Cloud Pub/Sub)
    - State snapshots (Firestore) for efficiency
    - Event replay for debugging
    - GDPR compliance: right to be forgotten

11. **[ADR-011: Multi-Region Active-Active Topology](../adr/ADR-011-Multi-Region-Architecture.md)**
    - Three regions: us-central1, europe-west1, asia-southeast1
    - Active-active (all regions serve traffic)
    - Firestore multi-region replication
    - DNS geolocation routing
    - RTO <5 min, RPO = 0

12. **[ADR-012: Kubernetes Autoscaling with HPA and KEDA](../adr/ADR-012-Kubernetes-Autoscaling.md)**
    - HPA: CPU/memory-based scaling
    - KEDA: Event-driven scaling (Kafka lag, queue depth)
    - VPA: Right-sizing recommendations
    - Per-service policies (min/max replicas)

## C4 Architecture Diagrams

**System overview at multiple abstraction levels:**

1. **[C4 Level 1: System Context](diagrams/C4-Level-1-System-Context.md)**
   - External users (admin, operator, external clients)
   - TAI system as black box
   - External systems (GCP, OAuth, monitoring, logging)
   - Key interactions and data flows

2. **[C4 Level 2: Container Diagram](diagrams/C4-Level-2-Containers.md)**
   - API Gateway (Envoy/Istio)
   - Three microservices (Governor, Coordinator, Scheduler)
   - Data layer (Firestore, Redis)
   - Event streaming (Cloud Pub/Sub, Kafka)
   - Infrastructure (Vault, KMS, Jaeger, Prometheus)
   - Inter-container communication patterns

3. **[C4 Level 3: Component Diagram](diagrams/C4-Level-3-Components.md)**
   - Internal structure of services (Governor example)
   - API layer (handlers, middleware)
   - Business logic (services, validators, enforcers)
   - Data access layer (repositories, cache, clients)
   - Infrastructure components (logging, metrics, secrets)
   - Request flow walkthrough

## API Reference

**Complete gRPC and HTTP API documentation:**

- **[gRPC API Reference](api-reference/GRPC-API-Reference.md)**
  - Governor service: ProposePolicy, EnforcePolicy, RevokePolicy, GetPolicies, StreamPolicies
  - Coordinator service: SubmitSignal, RequestAction, StreamActions, AcknowledgeAction, GetStatus
  - Scheduler service: SubmitTask, CancelTask, GetTaskStatus, StreamTaskUpdates
  - Error handling, retry strategies, rate limiting headers
  - Performance characteristics, testing with grpcurl
  - mTLS certificate requirements

## Database Schema

**Data model and storage design:**

- **[Firestore Schema](database/Firestore-Schema.md)**
  - Collections: policies, signals, actions, coordination_status, snapshots, audit_logs
  - Document structure, fields, indexes
  - Entity relationships and ERDs
  - Storage estimation and cost analysis
  - Query patterns and performance
  - Backup and recovery procedures
  - TTL policies and retention

## Deployment Architecture

**Infrastructure, Kubernetes, and deployment strategies:**

- **[Deployment Architecture](deployment/Deployment-Architecture.md)**
  - 3-region active-active topology (GCP regions)
  - Kubernetes cluster configuration (GKE)
  - Node pools (general, memory-intensive, spot instances)
  - Namespace organization (istio-system, monitoring, logging, tai-system)
  - Service deployments (Governor, Coordinator, Scheduler, Redis)
  - Ingress configuration and global load balancer
  - DNS setup (geolocation routing)
  - Helm chart structure
  - Deployment procedures and rollout strategy

## Integration Patterns

**Communication and synchronization patterns between components:**

- **[Integration Patterns](integration/Integration-Patterns.md)**
  - Synchronous communication: gRPC calls with circuit breaker
  - Asynchronous communication: Cloud Pub/Sub event publishing
  - Request/response with event sourcing
  - Cache-aside pattern with Redis
  - Cache invalidation via Pub/Sub
  - State synchronization across regions
  - Dead letter queue (DLQ) pattern
  - Transactional outbox pattern
  - Fan-out parallel processing
  - API gateway pattern (JSON ↔ gRPC transcoding)
  - Monitoring integration points

## Security Architecture

**Authentication, authorization, encryption, and compliance:**

- **[Security Architecture](security/Security-Architecture.md)**
  - Five security layers: Network, Authentication, Authorization, Data Protection, Audit
  - TLS 1.3 encryption (external and service-to-service mTLS)
  - Authentication: OAuth2 (external), mTLS (internal)
  - Authorization: RBAC with Istio AuthorizationPolicy
  - Secret management: Vault + Cloud KMS
  - Data encryption: At rest (Cloud KMS), in transit (TLS), field-level (sensitive data)
  - Audit logging: Immutable logs with full context
  - GDPR compliance: Right to be forgotten, data retention
  - SOC 2 compliance: Control framework
  - Security scanning: Container images, code, dependencies
  - Incident response: Alerts, playbooks

## Key Architectural Principles

### 1. **Resilience**
- Circuit breakers prevent cascading failures
- Multi-region failover (RTO <5 min)
- Automatic retries with exponential backoff
- Health checks on critical paths

### 2. **Observability**
- Distributed tracing for request flows
- Prometheus metrics for real-time monitoring
- Structured logging with context
- Custom dashboards for business metrics

### 3. **Security**
- Zero-trust architecture (deny by default)
- Encryption at rest and in transit
- Automatic secret rotation
- Audit trail for compliance

### 4. **Scalability**
- Horizontal pod autoscaling (HPA)
- Event-driven autoscaling (KEDA)
- Multi-region distribution
- Cache layers (Redis) to reduce load

### 5. **Maintainability**
- Clear separation of concerns (layered architecture)
- Interface-based design (trait objects in Rust)
- Configuration as code (Kubernetes manifests)
- Automated testing (unit, integration, end-to-end)

## Technology Stack Summary

| Component | Technology | Version | Purpose |
|-----------|-----------|---------|---------|
| Language | Rust | 1.91.1 | Type-safe, zero-cost abstractions |
| RPC | gRPC + Protocol Buffers | 1.0 | Service communication |
| Container | Kubernetes (GKE) | 1.27+ | Orchestration |
| Service Mesh | Istio | 1.18+ | Traffic management, mTLS |
| Primary Data | Google Firestore | Native | Distributed state |
| Cache | Redis | 7 | Hot data cache |
| Event Streaming | Cloud Pub/Sub | Standard | Async messaging |
| Secrets | HashiCorp Vault | 1.15+ | Secret management |
| Key Management | Google Cloud KMS | Standard | Root key encryption |
| Tracing | Jaeger | Latest | Distributed tracing |
| Metrics | Prometheus | 2.40+ | Time-series metrics |
| Logging | Cloud Logging | Native | Centralized logs |
| CI/CD | Cloud Build | Standard | Automated deployments |

## Performance SLOs

| Metric | Target | Measurement |
|--------|--------|-------------|
| Request Latency (p50) | <20 ms | Governor.ProposePolicy |
| Request Latency (p99) | <100 ms | Governor.ProposePolicy |
| Error Rate | <0.1% | Across all services |
| Availability | 99.99% | Per-region SLA |
| Failover Time | <5 min | Multi-region detection + switching |
| Cache Hit Rate | >80% | Policy lookups |
| First Build | <15s | Development iteration |

## Maintenance and Operations

### Monitoring
- 24/7 monitoring via Cloud Monitoring
- Alerting via Cloud Alerting + PagerDuty
- Incident response procedures
- On-call rotation

### Upgrades
- Canary deployments for all changes
- Blue-green for critical updates
- Rolling updates via Kubernetes
- Automated rollback on failures

### Backup & Recovery
- Automated daily Firestore backups
- Point-in-time recovery available
- Multi-region replication for HA
- DR drills monthly

## Related Documentation

- Specification: `.specify/specs/` directory (RDF ontology)
- Implementation: Code in `crates/` directory
- Examples: `examples/` directory
- Tests: `tests/` directory + crate-level tests
- Configuration: Helm charts in `helm/tai-chart/`

## Document Version

- **Version:** 1.0
- **Last Updated:** 2026-01-25
- **Status:** Production Ready
- **Audience:** Architects, Engineers, Operators

## Getting Started

1. **Understand the system:** Start with C4 Level 1 (System Context)
2. **Explore architecture:** Review C4 Level 2 (Containers) and Level 3 (Components)
3. **Deep dive on decisions:** Read relevant ADRs for your area of interest
4. **Implement features:** Reference API Reference and Integration Patterns
5. **Deploy changes:** Follow Deployment Architecture and ADR-004 (Canary)
6. **Troubleshoot issues:** Check Security Architecture and Integration Patterns

## Questions & Updates

- **Architecture questions:** Submit to architecture review board
- **Documentation issues:** Create GitHub issue with details
- **Implementation questions:** Consult API Reference and examples
- **Deployment questions:** Reference Deployment Architecture and Helm charts
