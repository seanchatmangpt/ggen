# FactoryPaaS Documentation Index

**Framework**: [Diataxis](https://diataxis.fr/) - A systematic approach to technical documentation

---

## 📚 The Four Documentation Types

```
                           PRACTICAL                    THEORETICAL
                              ↓                              ↓
           ┌──────────────────────────────────────────────────────────┐
LEARNING → │  📖 TUTORIALS          💡 EXPLANATION                      │
           │  Learning-oriented     Understanding-oriented             │
           │  "Take me by the hand" "Why does it work this way?"      │
           └──────────────────────────────────────────────────────────┘
           ┌──────────────────────────────────────────────────────────┐
WORKING  → │  🔧 HOW-TO GUIDES      📋 REFERENCE                       │
           │  Task-oriented          Information-oriented              │
           │  "Show me how to..."    "Tell me the facts"              │
           └──────────────────────────────────────────────────────────┘
```

---

## 📖 Tutorials (Learning-Oriented)

**Goal**: Help beginners learn through guided practice

### Core Tutorials
1. [**Getting Started**](tutorials/GETTING_STARTED.md) ⭐
   - Install ggen
   - Generate your first world
   - Deploy to GCP
   - Verify it works
   - **Time**: 30 minutes

2. [**Crossing the Event Horizon**](tutorials/CROSSING_THE_EVENT_HORIZON.md) ⭐
   - Understand the paradigm shift
   - RDF-first vs code-first development
   - Why ontology is truth
   - How sealed source prevents drift
   - **Time**: 45 minutes

3. [**Build Your First Affiliate Site**](tutorials/FIRST_AFFILIATE_SITE.md)
   - Define your niche in ontology
   - Add affiliate offers
   - Generate content
   - Track clicks and revenue
   - **Time**: 2 hours

4. [**Understanding Receipts**](tutorials/UNDERSTANDING_RECEIPTS.md)
   - What are cryptographic receipts?
   - How to read receipt JSON
   - Verify receipt integrity
   - Debug with receipts
   - **Time**: 30 minutes

---

## 🔧 How-To Guides (Task-Oriented)

**Goal**: Solve specific problems step-by-step

### Deployment & Operations
5. [**Deploy to GCP**](../DEPLOYMENT.md) ⭐ (already exists)
   - GCP project setup
   - Terraform deployment
   - DNS configuration
   - SSL certificates

6. [**Setup Monitoring**](howto/SETUP_MONITORING.md)
   - Configure OpenTelemetry
   - Set up GCP Cloud Monitoring
   - Create alert policies
   - Build custom dashboards

7. [**Backup and Restore**](howto/BACKUP_AND_RESTORE.md)
   - PostgreSQL automated backups
   - Receipt ledger snapshots
   - Disaster recovery procedures
   - Point-in-time recovery

### Revenue & Content
8. [**Track Revenue**](howto/TRACK_REVENUE.md)
   - Configure attribution rules
   - Set up payout schedules
   - Generate revenue reports
   - Export data for taxes

9. [**Publish Content**](howto/PUBLISH_CONTENT.md)
   - AI-powered content generation
   - SEO optimization
   - A/B testing setup
   - Bulk publishing

10. [**Manage Affiliate Offers**](howto/MANAGE_OFFERS.md)
    - Add new offers
    - Configure commission rates
    - Set up conversion tracking
    - Handle offer expiration

### Development
11. [**Extend the Ontology**](howto/EXTEND_ONTOLOGY.md)
    - Add new entities
    - Define custom commands
    - Create business policies
    - Validate with SHACL

12. [**Debug Generated Code**](howto/DEBUG_GENERATED_CODE.md)
    - Read SPARQL query results
    - Understand Tera templates
    - Trace code generation
    - Fix generation errors

---

## 📋 Reference (Information-Oriented)

**Goal**: Provide precise, complete information for lookup

### Ontology Reference
13. [**Ontology Schema**](reference/ONTOLOGY.md) ⭐
    - All RDF classes and properties
    - Entity definitions
    - Command specifications
    - Event schemas
    - Policy rules

14. [**SPARQL Queries**](reference/SPARQL.md)
    - All query files documented
    - Query parameters
    - Expected results
    - Example outputs

15. [**Tera Templates**](reference/TEMPLATES.md)
    - Template catalog
    - Template variables
    - Filters and functions
    - Generation rules

### API Reference
16. [**HTTP API**](reference/API.md)
    - All endpoints documented
    - Request/response schemas
    - Authentication
    - Rate limits
    - Error codes

17. [**Metrics Reference**](reference/METRICS.md) ⭐
    - All OpenTelemetry metrics
    - Metric types (Counter, Histogram, Gauge)
    - Labels and dimensions
    - Aggregation examples

18. [**Receipt Schema**](reference/RECEIPTS.md)
    - Receipt JSON structure
    - Field definitions
    - Cryptographic algorithms
    - Verification process

### Infrastructure Reference
19. [**GCP Resources**](reference/GCP_RESOURCES.md)
    - All Terraform resources
    - Resource dependencies
    - Cost estimates
    - Scaling limits

20. [**Configuration**](reference/CONFIGURATION.md)
    - Environment variables
    - ggen.toml options
    - Kernel configuration
    - Feature flags

---

## 💡 Explanation (Understanding-Oriented)

**Goal**: Explain concepts, design decisions, and trade-offs

### Architecture
21. [**System Architecture**](explanation/ARCHITECTURE.md) ⭐ (already exists as docs/factorypaas/ARCHITECTURE.md)
    - High-level system design
    - C4 diagrams (Context, Container, Component)
    - Data flow diagrams
    - Integration patterns

22. [**TCPS Paradigm**](explanation/TCPS.md) ⭐
    - Toyota Code Production System explained
    - Why ontology-driven development?
    - Graph truth vs code projection
    - Andon signals and quality gates

23. [**Sealed Source**](explanation/SEALED_SOURCE.md) ⭐
    - Why prevent manual edits?
    - How drift is eliminated structurally
    - Failure modes prevented
    - Emergency overrides

### Domain Concepts
24. [**Revenue Attribution**](explanation/REVENUE_TRACKING.md) (already exists as docs/factorypaas/REVENUE_TRACKING.md)
    - Last-click attribution model
    - Attribution window logic
    - Commission calculation
    - Payout policies

25. [**Event Sourcing**](explanation/EVENT_SOURCING.md)
    - Why event sourcing for receipts?
    - Command-Event flow
    - Aggregate rebuilding
    - Projection mechanics

26. [**Cryptographic Receipts**](explanation/CRYPTOGRAPHIC_RECEIPTS.md)
    - Why cryptographic proofs?
    - SHA-256 hashing
    - Merkle chain linking
    - Audit trail immutability

### Technology Choices
27. [**Why Rust?**](explanation/WHY_RUST.md)
    - Type safety benefits
    - Zero-cost abstractions
    - Memory safety guarantees
    - Async performance

28. [**Why GCP?**](explanation/WHY_GCP.md)
    - Cloud Run vs alternatives
    - Cloud SQL performance
    - Cloud Monitoring advantages
    - Cost optimization

29. [**Why RDF/Turtle?**](explanation/WHY_RDF.md)
    - Graph databases for domain models
    - SPARQL query power
    - SHACL validation
    - OWL inference

---

## 🔍 Quick Navigation by Role

### For Developers
- Start: [Getting Started Tutorial](tutorials/GETTING_STARTED.md)
- Reference: [Ontology Schema](reference/ONTOLOGY.md), [API Reference](reference/API.md)
- Debug: [Debug Generated Code](howto/DEBUG_GENERATED_CODE.md)
- Extend: [Extend the Ontology](howto/EXTEND_ONTOLOGY.md)

### For Operators
- Start: [Deploy to GCP](../DEPLOYMENT.md)
- Operations: [Operations Guide](../OPERATIONS.md)
- Monitoring: [Setup Monitoring](howto/SETUP_MONITORING.md)
- Troubleshoot: [Backup and Restore](howto/BACKUP_AND_RESTORE.md)

### For Publishers (Affiliates)
- Start: [Build Your First Affiliate Site](tutorials/FIRST_AFFILIATE_SITE.md)
- Revenue: [Track Revenue](howto/TRACK_REVENUE.md)
- Content: [Publish Content](howto/PUBLISH_CONTENT.md)
- Offers: [Manage Affiliate Offers](howto/MANAGE_OFFERS.md)

### For Architects
- Paradigm: [TCPS Paradigm](explanation/TCPS.md), [Sealed Source](explanation/SEALED_SOURCE.md)
- Architecture: [System Architecture](explanation/ARCHITECTURE.md)
- Design: [Why Rust?](explanation/WHY_RUST.md), [Why RDF?](explanation/WHY_RDF.md)

---

## 📊 Documentation Status

| Category | Files | Status |
|----------|-------|--------|
| 📖 Tutorials | 4 | ⚠️ 1/4 complete |
| 🔧 How-To Guides | 8 | ⚠️ 2/8 complete |
| 📋 Reference | 8 | ⚠️ 3/8 complete |
| 💡 Explanation | 9 | ⚠️ 4/9 complete |
| **TOTAL** | **29** | **10/29 (34%)** |

**Next Priority**:
1. ⭐ [Getting Started Tutorial](tutorials/GETTING_STARTED.md)
2. ⭐ [TCPS Paradigm](explanation/TCPS.md)
3. ⭐ [Ontology Schema](reference/ONTOLOGY.md)

---

## 📝 Contributing to Documentation

### Documentation Guidelines
- **Tutorials**: Step-by-step, beginner-friendly, include screenshots
- **How-To**: Task-focused, assume competence, show expected output
- **Reference**: Complete, precise, no fluff, update with code changes
- **Explanation**: Conceptual, discuss alternatives, explain trade-offs

### Template Locations
- Tutorial template: `docs/templates/TUTORIAL_TEMPLATE.md`
- How-To template: `docs/templates/HOWTO_TEMPLATE.md`
- Reference template: `docs/templates/REFERENCE_TEMPLATE.md`
- Explanation template: `docs/templates/EXPLANATION_TEMPLATE.md`

### Validation
```bash
# Check all documentation links
find docs/ -name "*.md" -exec markdown-link-check {} \;

# Spell check
find docs/ -name "*.md" -exec aspell check {} \;

# Validate code examples compile
cargo make test-docs
```

---

## 🔗 External Resources

- [Diataxis Framework](https://diataxis.fr/)
- [ggen Documentation](../../README.md)
- [RDF/Turtle Specification](https://www.w3.org/TR/turtle/)
- [SPARQL 1.1 Specification](https://www.w3.org/TR/sparql11-query/)
- [GCP Documentation](https://cloud.google.com/docs)

---

**Last Updated**: 2026-01-24
**Maintainer**: FactoryPaaS Documentation Team
