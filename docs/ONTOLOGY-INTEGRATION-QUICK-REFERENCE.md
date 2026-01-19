# ChatmanGPT Ontology Integration Quick Reference
## Practical Developer Guide

**Purpose**: One-page reference for integrating canonical ontologies into ChatmanGPT
**Audience**: Rust developers, RDF specialists
**Last Updated**: 2026-01-19

---

## 1. Quick Namespace Reference

```turtle
# Core namespaces
@prefix chatman: <https://chatmangpt.io/ontology/> .
@prefix legal: <https://chatmangpt.io/ontology/legal/> .
@prefix security: <https://chatmangpt.io/ontology/security/> .
@prefix cloud: <https://chatmangpt.io/ontology/cloud/> .
@prefix it: <https://chatmangpt.io/ontology/it/> .
@prefix qudt: <http://qudt.org/schema/qudt/> .

# External ontologies
@prefix lkif: <http://www.w3.org/2000/10/lkif#> .
@prefix stix: <https://oasis-open.github.io/cti-stix/v2.1/> .
@prefix tosca: <http://docs.oasis-open.org/tosca/TOSCA-v1.3/> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
@prefix owl: <http://www.w3.org/2002/07/owl#> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .
```

---

## 2. Core Equivalences (Alignment)

```turtle
# Legal mappings
legal:Policy owl:equivalentClass lkif:Norm .
legal:ComplianceRequirement rdfs:subClassOf lkif:Obligation .
legal:ContractObligation owl:equivalentClass lkif:ContractualObligation .

# Security mappings
security:Threat owl:equivalentClass stix:Threat .
security:Vulnerability owl:equivalentClass stix:Vulnerability .
security:AccessControl rdfs:subClassOf stix:CourseOfAction .

# Cloud mappings
cloud:ComputeService owl:equivalentClass tosca:Compute .
cloud:Storage owl:equivalentClass tosca:BlockStorage .
cloud:Network owl:equivalentClass tosca:network.Root .
cloud:Topology owl:equivalentClass tosca:Topology .
```

---

## 3. Jurisdiction Quick Lookup

```
HIPAA        → Healthcare data (PHI)        → legal:HIPAA
SOX          → Financial records            → legal:SOX
GDPR         → EU personal data             → legal:GDPR
CCPA         → California consumer data     → legal:CCPA
PCI-DSS      → Payment card data            → legal:PCI_DSS
FedRAMP      → US government cloud          → legal:FedRAMP
ISO27001     → Information security         → legal:ISO27001
```

### Data Classification Levels
```
Public       → legal:Public
Internal     → legal:Internal
Confidential  → legal:Confidential
Restricted    → legal:Restricted
PII          → legal:PII (Personally Identifiable Information)
PHI          → legal:PHI (Protected Health Information)
PCI          → legal:PCI (Payment Card Industry)
```

---

## 4. Security Severity Levels

### CVSS Score Ranges (CVSS 3.1)
```
0.0           → None              (security:CVSS-None)
0.1-3.9       → Low               (security:CVSS-Low)
4.0-6.9       → Medium            (security:CVSS-Medium)
7.0-8.9       → High              (security:CVSS-High)
9.0-10.0      → Critical          (security:CVSS-Critical)
```

### Severity Levels (Generic)
```
Low     → security:Low
Medium  → security:Medium
High    → security:High
Critical → security:Critical
```

---

## 5. SLA Metrics with QUDT Units

| Metric | Value | Unit | Class | QUDT Unit |
|--------|-------|------|-------|-----------|
| Availability | 99.99 | % | it:AvailabilityTarget-99_99 | qudt:Percent |
| Response Time (P95) | 500 | ms | it:ResponseTimeTarget-500ms | qudt:Unit-Millisecond |
| Error Rate | 0.1 | % | it:ErrorRateTarget-0_1pct | qudt:Percent |
| Throughput | 1000 | req/s | it:ThroughputTarget-1k | qudt:Unit-RequestsPerSecond |
| Storage | 100 | GB | cloud:StorageCapacity | qudt:Unit-Gigabyte |
| Memory | 4 | GB | cloud:MemoryLimit | qudt:Unit-Gigabyte |

### Example: Define SLA
```turtle
myapp:SLA a it:ServiceLevelAgreement ;
  it:availabilityTarget [
    qudt:value 99.99 ;
    qudt:unit qudt:Percent
  ] ;
  it:responseTimeTarget [
    qudt:value 500 ;
    qudt:unit qudt:Unit-Millisecond
  ] ;
  it:errorRateTarget [
    qudt:value 0.1 ;
    qudt:unit qudt:Percent
  ] .
```

---

## 6. Cloud Compute Types

```
VirtualMachine  → cloud:VirtualMachine     (EC2, Compute Engine, VMs)
Container       → cloud:Container          (ECS, GKE, AKS)
Serverless      → cloud:Serverless         (Lambda, Cloud Functions, Functions)
```

### Example: Compute Service Definition
```turtle
myapp:WebTier a cloud:Container ;
  cloud:cpu [
    qudt:value 2 ;
    qudt:unit qudt:Unit-CPUCore
  ] ;
  cloud:memory [
    qudt:value 4 ;
    qudt:unit qudt:Unit-Gigabyte
  ] ;
  cloud:containerOrchestrator cloud:Kubernetes .
```

---

## 7. Cloud Storage Types

```
ObjectStorage  → cloud:ObjectStorage       (S3, Cloud Storage, Blob)
BlockStorage   → cloud:BlockStorage        (EBS, Persistent Disk, Managed Disks)
FileStorage    → cloud:FileStorage         (EFS, Filestore, File Share)
Database       → cloud:Database            (RDS, Cloud SQL, SQL Database)
```

---

## 8. Cloud Network Components

```
VPC            → cloud:VPC                 (Virtual Private Cloud)
LoadBalancer   → cloud:LoadBalancer        (ALB, Cloud Load Balancing, LB)
CDN            → cloud:CDN                 (CloudFront, Cloud CDN, Front Door)
DNS            → cloud:DNS                 (Route53, Cloud DNS, DNS)
```

---

## 9. Authentication Methods

```
MFA          → security:MFA                (Multi-Factor Auth)
OAuth2       → security:OAuth2
SAML         → security:SAML
Kerberos     → security:Kerberos
```

---

## 10. Encryption Algorithms

```
AES256       → security:AES256             (Advanced Encryption Standard)
RSA4096      → security:RSA4096
ChaCha20     → security:ChaCha20
```

---

## 11. Cloud Providers

```
AWS          → cloud:AWS
GCP          → cloud:GCP
Azure        → cloud:Azure
Alibaba      → cloud:Alibaba
```

### Example: AWS Binding
```turtle
myapp:Deployment a chatman:AWSBinding ;
  chatman:mapsCloudComputeToAWS chatman:ECS ;
  chatman:mapsCloudStorageToAWS chatman:RDS ;
  chatman:mapsSecurityToAWS chatman:IAM ;
  chatman:mapsComplianceToAWS chatman:CloudTrail .
```

---

## 12. Common SPARQL Patterns

### Find all HIPAA obligations
```sparql
SELECT ?obligation ?label WHERE {
  ?obligation a legal:ComplianceRequirement ;
    legal:jurisdiction legal:HIPAA ;
    rdfs:label ?label .
}
```

### Find high-severity vulnerabilities
```sparql
SELECT ?vuln ?cvss WHERE {
  ?vuln a security:Vulnerability ;
    security:cvssScore ?score ;
    rdfs:label ?vuln .
  FILTER (?score >= 7.0)
}
ORDER BY DESC(?score)
```

### Get all compute services with CPU/memory
```sparql
SELECT ?compute ?cpu ?memory WHERE {
  ?compute a cloud:ComputeService ;
    cloud:cpu [qudt:value ?cpu] ;
    cloud:memory [qudt:value ?memory] .
}
```

### Find topologies for HIPAA with encryption
```sparql
SELECT ?topology ?storage ?encryption WHERE {
  ?topology a cloud:Topology ;
    legal:jurisdiction legal:HIPAA ;
    cloud:hasStorage ?storage .
  ?storage cloud:encrypted true ;
    cloud:encryptionAlgorithm ?encryption .
}
```

### Map compliance to technical controls
```sparql
SELECT ?compliance ?control WHERE {
  ?compliance a legal:ComplianceRequirement ;
    legal:technicalControl ?control .
  ?control a security:AccessControl .
}
```

---

## 13. Rust Integration Examples

### Load ontology in Rust
```rust
use ggen_core::rdf::parser::RdfParser;
use oxigraph::store::Store;

fn load_ontologies() -> Result<Store, Error> {
    let store = Store::new()?;

    // Load canonical ontologies
    store.load_graph(
        &Graph::load_from_file("./.specify/UNIFIED-ONTOLOGY-REGISTRY.ttl")?,
        GraphName::DefaultGraph,
    )?;

    // Load LKIF, STIX, TOSCA, QUDT...
    // (individual loading per ontology)

    Ok(store)
}
```

### Query for compliance requirements
```rust
fn get_hipaa_requirements(store: &Store) -> Result<Vec<String>, Error> {
    let query = r#"
        SELECT ?requirement WHERE {
          ?requirement a ?ComplianceRequirement ;
            legal:jurisdiction legal:HIPAA .
        }
    "#;

    let results = store.query(query)?;
    Ok(results.into_iter().map(|r| r.to_string()).collect())
}
```

### Create security finding
```rust
fn create_vulnerability(cve_id: &str, cvss: f32) -> String {
    format!(r#"
        [] a security:Vulnerability ;
          security:cveId "{}" ;
          security:cvssScore [
            qudt:value {} ;
            qudt:unit qudt:Unit-CVSSScore
          ] .
    "#, cve_id, cvss)
}
```

---

## 14. Validation Commands

```bash
# Validate ontology consistency
cargo make validate-rdf \
  --ontology ./.specify/UNIFIED-ONTOLOGY-REGISTRY.ttl \
  --report validation-report.json

# Run SPARQL query tests
cargo make test \
  --test sparql_integration_tests \
  -- --nocapture

# Check for OWL DL violations
cargo run --bin ggen-validate -- \
  --check-owl-dl \
  --ontology ./.specify/UNIFIED-ONTOLOGY-REGISTRY.ttl
```

---

## 15. File Locations

```
.specify/UNIFIED-ONTOLOGY-REGISTRY.ttl      Main registry (ChatmanGPT)
crates/ggen-core/src/rdf/                   RDF parsing & handling
crates/ggen-ai/tests/fixtures/              Test ontologies
docs/ONTOLOGY-CANONICAL-ANALYSIS.md         Full analysis (this document's parent)
docs/agent/rdf-sparql-guide.md              RDF/SPARQL tutorials
```

---

## 16. Quick Troubleshooting

| Problem | Cause | Solution |
|---------|-------|----------|
| Namespace not found | Missing @prefix | Add prefix declaration |
| owl:equivalentClass error | Circular dependency | Use rdfs:subClassOf instead |
| SPARQL empty results | Wrong property name | Check SPARQL pattern examples |
| CVSS score range error | Value outside 0.0-10.0 | Validate score in Range |
| Unit mismatch | Percentage vs decimal | Use qudt:Percent consistently |
| Query timeout | Large dataset | Add FILTER to reduce results |

---

## 17. Integration Checklist (Per Feature)

- [ ] Identify relevant ontology (LKIF/STIX/TOSCA/QUDT)
- [ ] Add namespace prefix to file header
- [ ] Use owl:equivalentClass for mappings
- [ ] All metrics use qudt:unit
- [ ] SPARQL patterns written & tested
- [ ] Documentation updated
- [ ] Unit tests cover new ontology usage
- [ ] Validation report clean

---

## 18. Performance Notes

| Operation | Typical Time | Notes |
|-----------|--------------|-------|
| Load registry | <5s | First load; cached after |
| SPARQL query (1K triples) | <50ms | Simple patterns |
| SPARQL join (cross-domain) | <200ms | More complex patterns |
| OWL reasoning | <10s | For 1K instances |
| Add instance | <10ms | Per new entity |

---

## 19. Version Compatibility

```
Ontology       Version    Released  Status        ChatmanGPT Support
────────────────────────────────────────────────────────────────────
LKIF           1.0        2009      Stable        Always
STIX           2.1        2021-06   Production    2.1+ (2.0 via adapter)
TOSCA          1.3        2020      Stable        1.3+ (1.2 via adapter)
QUDT           2.1+       Ongoing   Stable        Latest
OWL            2 DL/RL    2012      W3C Std       2 DL required
XSD            1.1        2012      W3C Std       Full support
RDF            1.1        2014      W3C Std       Full support
```

---

## 20. Key Contacts & Resources

**ChatmanGPT Documentation**: `/home/user/ggen/docs/`
**Canonical Ontology Analysis**: `/home/user/ggen/docs/ONTOLOGY-CANONICAL-ANALYSIS.md`
**LKIF Tutorial**: https://www.estrellaproject.org/lkif-core/
**STIX Documentation**: https://oasis-open.github.io/cti-documentation/stix/
**TOSCA Specification**: https://www.oasis-open.org/committees/tosca/
**QUDT Browser**: http://qudt.org/

---

**Last Updated**: 2026-01-19 | **Next Review**: 2026-04-19 (quarterly)
