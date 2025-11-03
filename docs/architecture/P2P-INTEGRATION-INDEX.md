# Ggen Marketplace P2P Integration Architecture

**Version:** 2.4.0
**Status:** Design Complete
**Last Updated:** 2025-11-02

## Overview

This directory contains comprehensive architecture documentation for integrating P2P (libp2p) marketplace functionality into ggen. The design enables decentralized package discovery and distribution while maintaining backward compatibility with the existing file-based registry.

---

## Document Index

### 1. [P2P Integration Architecture](./p2p-integration-architecture.md)
**Main Design Document** - 800+ lines

**Contents:**
- System Context (C4 Level 1)
- Container Architecture (C4 Level 2)
- Component Architecture (C4 Level 3)
- Data Flow Architecture
- Module Boundaries & Responsibilities
- Failure Modes & Resilience
- Scalability Considerations
- Security Architecture
- API Contracts
- Migration Path & Backward Compatibility
- Observability & Monitoring
- Testing Strategy
- Deployment Architecture
- Future Enhancements

**Key Sections:**
- ✅ Dual-Registry Pattern (file + P2P)
- ✅ Registry Trait Abstraction
- ✅ Clean Layer Separation (CLI → Domain → Backend)
- ✅ Feature-Gated P2P (`--features p2p`)
- ✅ Graceful Degradation Strategy

**Use This For:**
- Understanding the overall system design
- Making architectural decisions
- Onboarding new developers
- System design reviews

---

### 2. [Integration Flow Diagrams](./p2p-integration-flow-diagram.md)
**Visual Documentation** - Complete Flows

**Contents:**
- Package Discovery and Installation Flow (CLI → Domain → Backend)
- P2P Package Discovery Flow (Local → DHT → Merge)
- P2P Package Publishing Flow (Store → DHT → Gossipsub)
- Error Handling and Fallback Flow (P2P → File Registry)

**Key Diagrams:**
- ✅ Installation pipeline with dependency resolution
- ✅ P2P search with reputation filtering
- ✅ Publishing with DHT replication
- ✅ Automatic fallback mechanisms

**Use This For:**
- Understanding data flow between components
- Debugging installation issues
- Implementing new features
- Explaining system behavior

---

### 3. [API Contracts](./p2p-api-contracts.md)
**Interface Specifications**

**Contents:**
- Layer Boundaries (CLI / Domain / Backend)
- CLI to Domain Contracts (search, install, p2p commands)
- Domain to Backend Contracts (`Registry` trait)
- Error Handling Contracts
- Data Structure Contracts (Package, Query, Metadata)
- P2P Network Contracts (Config, Events, Reputation)

**Key Specifications:**
- ✅ Registry trait (8 async methods)
- ✅ Error type hierarchy (MarketplaceError → GgenError)
- ✅ Package model (with signatures)
- ✅ P2P configuration and events
- ✅ Versioning and compatibility guarantees

**Use This For:**
- Implementing new backends
- Understanding error propagation
- Creating mock implementations for tests
- Ensuring API stability across versions

---

### 4. [Failure Modes Analysis](./p2p-failure-modes.md)
**Resilience & Recovery**

**Contents:**
- Network Failures (9 scenarios)
- Storage Failures (3 scenarios)
- Dependency Resolution Failures (2 scenarios)
- Failure Recovery Procedures
- Monitoring and Alerting
- Risk Matrix
- Testing Failure Scenarios
- Incident Response Playbook

**Key Features:**
- ✅ Detection mechanisms for each failure
- ✅ Mitigation strategies with code examples
- ✅ Automatic recovery procedures
- ✅ Manual recovery commands
- ✅ Risk assessment matrix

**Use This For:**
- Understanding system resilience
- Planning disaster recovery
- Writing chaos engineering tests
- Incident response
- Security audits

---

## Quick Start

### For System Architects
**Start with:** [P2P Integration Architecture](./p2p-integration-architecture.md)
- Section 1-3: System overview and containers
- Section 5: Module boundaries
- Section 10: Migration path

### For Developers Implementing Features
**Start with:** [Integration Flow Diagrams](./p2p-integration-flow-diagram.md)
- Review relevant flow diagram
- Check [API Contracts](./p2p-api-contracts.md) for interfaces
- Implement backend trait method

### For QA/Testing Engineers
**Start with:** [Failure Modes Analysis](./p2p-failure-modes.md)
- Section 8: Testing failure scenarios
- Section 9: Incident response playbook
- Write chaos engineering tests

### For DevOps/SRE
**Start with:** [P2P Integration Architecture](./p2p-integration-architecture.md)
- Section 13: Deployment architecture
- Section 11: Observability & monitoring
- [Failure Modes Analysis](./p2p-failure-modes.md) - Section 6: Monitoring

---

## Architecture Decisions (ADRs)

| ADR | Title | Status | Impact |
|-----|-------|--------|--------|
| ADR-001 | Use Trait Abstraction for Registry Backend | ✅ Accepted | Enables pluggable backends |
| ADR-002 | Feature-Gate P2P Functionality | ✅ Accepted | Opt-in P2P, smaller binaries |
| ADR-003 | Use Gossipsub for Package Announcements | ✅ Accepted | Efficient broadcast |
| ADR-004 | Implement Peer Reputation Tracking | ✅ Accepted | Sybil attack mitigation |
| ADR-005 | Defer Signature Verification to v2.5.0 | ✅ Accepted | Faster MVP delivery |

---

## Implementation Roadmap

### Phase 2: P2P Foundation (v2.4.0) 🔄 Current
- [x] P2P backend implementation
- [x] CLI commands (`ggen marketplace p2p ...`)
- [x] Feature flag (`--features p2p`)
- [x] Graceful fallback
- [x] Architecture documentation ✅ **COMPLETE**
- [ ] London TDD test suite for P2P
- [ ] E2E integration tests
- [ ] Performance benchmarks

---

## Documentation Summary

| Document | Size | Lines | Sections | Diagrams |
|----------|------|-------|----------|----------|
| P2P Integration Architecture | 53K | 800+ | 14 | 5 |
| Integration Flow Diagrams | 49K | 700+ | 3 flows | 4 |
| API Contracts | 22K | 500+ | 6 | 2 |
| Failure Modes Analysis | 28K | 600+ | 9 | 1 |
| **TOTAL** | **152K** | **2600+** | **32** | **12** |

---

**Architecture Version:** 1.0
**Document Maintainer:** System Architect Agent
**Last Review:** 2025-11-02
**Next Review:** 2025-12-02
