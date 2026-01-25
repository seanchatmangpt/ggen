# TAI RDF Ontology and Specification System

Documentation for the RDF-based specification system that drives TAI architecture.

## .specify/ Directory Structure

```
.specify/
├── specs/                          # Feature specifications (source of truth)
│   ├── 001-governance/
│   │   ├── feature.ttl             # User stories in RDF (EDIT THIS)
│   │   ├── entities.ttl            # Domain entities (EDIT THIS)
│   │   ├── plan.ttl                # Architecture/design plan (EDIT THIS)
│   │   ├── tasks.ttl               # Task breakdown (EDIT THIS)
│   │   ├── spec.md                 # Generated markdown (DO NOT EDIT)
│   │   └── evidence/               # Test results, proof of completion
│   │
│   ├── 002-signals/
│   ├── 003-coordination/
│   └── ...
│
├── templates/                      # Tera templates for spec generation
│   ├── spec.tera                   # TTL → Markdown transformation
│   └── rdf-helpers/                # TTL templates
│       ├── user-story.ttl
│       ├── acceptance-scenario.ttl
│       └── task.ttl
│
├── memory/                         # Project memory (shared context)
│   ├── constitution.ttl            # Project principles
│   ├── decisions.ttl               # Architecture decisions
│   └── constraints.ttl             # System constraints
│
└── *.ttl                          # Ontology files (semantic layer)
    ├── cli-schema.ttl             # CLI noun-verb model
    ├── naming-conventions.ttl     # Canonical naming
    ├── folk-calculus-definitions.ttl
    └── ... (60+ ontologies)
```

## RDF Concepts

### Triples (Subject-Predicate-Object)

All TAI knowledge represented as RDF triples:

```turtle
@prefix ggen: <http://ggen.ai/tai/> .
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .

# Example: Governor service has responsibility
ggen:GovernorService rdf:type ggen:Microservice ;
  rdfs:label "Governor Service" ;
  ggen:responsibility ggen:PolicyManagement ;
  ggen:technology ggen:Rust ;
  ggen:replicas "3-50" ;
  ggen:port 50051 .
```

### Namespace Prefixes

```turtle
@prefix ggen: <http://ggen.ai/tai/> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
@prefix owl: <http://www.w3.org/2002/07/owl#> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .
@prefix sh: <http://www.w3.org/ns/shacl#> .

# Custom TAI properties
@prefix tai-gov: <http://tai.example.com/governance/> .
@prefix tai-arch: <http://tai.example.com/architecture/> .
@prefix tai-ops: <http://tai.example.com/operations/> .
```

## Ontology Files

### 1. Architecture Ontologies

**cli-schema.ttl**
- Defines noun-verb CLI model
- Command structure (action targets)
- Example: `ggen policy propose --type security --name "TLS 1.3"`

**naming-conventions-canonical.ttl**
- Canonical naming rules
- Resource ID formats (UUID, kebab-case)
- Field naming (snake_case vs camelCase)

### 2. Domain Ontologies

**folk-calculus-definitions.ttl**
- Formal semantics of TAI concepts
- Policy, Signal, Action, Coordination definitions
- Constraint logic for validity

**market-phase-diagram.ttl**
- Business/lifecycle phases
- State transitions
- Compliance checkpoints

### 3. Process Ontologies

**governance-process.ttl**
- Policy lifecycle (proposed → enforced → revoked)
- Approval workflows
- Audit requirements

**incident-response-process.ttl**
- Incident classification
- Escalation paths
- Recovery procedures

## SPARQL Query Examples

### Find All Policies by Type

```sparql
PREFIX ggen: <http://ggen.ai/tai/>

SELECT ?policy ?type ?name
WHERE {
  ?policy rdf:type ggen:Policy ;
          ggen:policyType ?type ;
          ggen:policyName ?name .
  FILTER (?type = "security")
}
```

### Find Services with High Replication

```sparql
PREFIX ggen: <http://ggen.ai/tai/>

SELECT ?service ?minReplicas ?maxReplicas
WHERE {
  ?service rdf:type ggen:Microservice ;
           ggen:minReplicas ?minReplicas ;
           ggen:maxReplicas ?maxReplicas .
  FILTER (?maxReplicas > 50)
}
ORDER BY DESC(?maxReplicas)
```

### Find Resources with Encryption Requirement

```sparql
PREFIX ggen: <http://ggen.ai/tai/>

SELECT ?resource ?encryption
WHERE {
  ?resource ggen:requiresEncryption ?encryption ;
            ggen:classification "sensitive" .
}
```

### Transitivity: Find All Services in Architecture

```sparql
PREFIX ggen: <http://ggen.ai/tai/>

SELECT ?service
WHERE {
  ggen:TaiArchitecture ggen:contains* ?component .
  ?component rdf:type ggen:Microservice .
  BIND (?component AS ?service)
}
```

## SHACL Validation

### Shape Definition (Example: Policy)

```turtle
@prefix ggen: <http://ggen.ai/tai/> .
@prefix sh: <http://www.w3.org/ns/shacl#> .

ggen:PolicyShape
  a sh:NodeShape ;
  sh:targetClass ggen:Policy ;
  sh:property [
    sh:path ggen:policyType ;
    sh:in ( "finance" "security" "operational" ) ;
    sh:minCount 1 ;
  ] ;
  sh:property [
    sh:path ggen:policyName ;
    sh:datatype xsd:string ;
    sh:minLength 1 ;
    sh:maxLength 256 ;
    sh:minCount 1 ;
  ] ;
  sh:property [
    sh:path ggen:rules ;
    sh:datatype rdf:JSON ;
    sh:minCount 1 ;
  ] ;
  sh:property [
    sh:path ggen:createdAt ;
    sh:datatype xsd:dateTime ;
    sh:minCount 1 ;
  ] .
```

### Validation Command

```bash
# Validate instance against shapes
ggen validate .specify/specs/001-governance/entities.ttl \
  --shape-file .specify/*.ttl

# Output: Conform | Non-conform with violations
```

## Feature Specification Template

### feature.ttl (User Stories)

```turtle
@prefix ggen: <http://ggen.ai/tai/> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .

ggen:Spec001-Governance
  a ggen:FeatureSpecification ;
  rdfs:label "Specification 001: Governance System" ;
  ggen:description "Policy proposal, enforcement, and revocation" ;
  ggen:priority "P1" ;

  ggen:hasUserStory [
    a ggen:UserStory ;
    rdfs:label "Admin proposes security policy" ;
    ggen:as "Administrator" ;
    ggen:want "propose a security policy requiring TLS 1.3" ;
    ggen:so "the system enforces modern encryption standards" ;

    ggen:hasAcceptanceScenario [
      a ggen:Scenario ;
      ggen:given "admin is authenticated with admin role" ;
      ggen:when "admin calls ProposePolicy with policy JSON" ;
      ggen:then "system stores policy and returns receipt with ID" ;
    ] ;
  ] .
```

### entities.ttl (Domain Entities)

```turtle
@prefix ggen: <http://ggen.ai/tai/> .

ggen:Policy
  a owl:Class ;
  rdfs:label "Policy" ;
  owl:hasProperty ggen:id, ggen:policyType, ggen:rules, ggen:enabled ;

  ggen:id a xsd:UUID ;
  ggen:policyType a xsd:String ; owl:oneOf ( "finance" "security" "operational" ) ;
  ggen:rules a rdf:JSON ;
  ggen:enabled a xsd:Boolean ;
  ggen:createdAt a xsd:DateTime ;
  ggen:createdBy a xsd:String .
```

### plan.ttl (Architecture)

```turtle
@prefix ggen: <http://ggen.ai/tai/> .

ggen:Spec001-Plan
  a ggen:ArchitecturePlan ;
  ggen:component ggen:GovernorService ;
  ggen:dataStore ggen:Firestore ;
  ggen:eventStream ggen:CloudPubSub ;

  ggen:designDecision [
    ggen:question "Where to store policies?" ;
    ggen:answer "Google Firestore (multi-region)" ;
    ggen:rationale "ACID transactions, real-time updates, auto-replication" ;
  ] .
```

### tasks.ttl (Implementation)

```turtle
@prefix ggen: <http://ggen.ai/tai/> .

ggen:Spec001-Tasks
  a ggen:TaskBreakdown ;

  ggen:hasTask [
    a ggen:Task ;
    rdfs:label "Implement Governor.ProposePolicy RPC" ;
    ggen:assignedTo "backend-team" ;
    ggen:estimatePoints 13 ;
    ggen:blocks ggen:Task002 ;
  ] ;

  ggen:hasTask [
    a ggen:Task ;
    rdfs:label "Write integration tests for policy storage" ;
    ggen:dependsOn ggen:Task001 ;
    ggen:estimatePoints 8 ;
  ] .
```

## Code Generation from RDF

The `ggen sync` command transforms RDF specifications into code:

```bash
# Generate all from .specify/
ggen sync

# What happens (5-stage pipeline):
# μ₁ (Normalize)     → Load TTL, validate SHACL
# μ₂ (Extract)       → Execute SPARQL queries
# μ₃ (Emit)          → Render Tera templates
# μ₄ (Canonicalize)  → Format output
# μ₅ (Receipt)       → Generate audit trail
```

### Example: Generate API Documentation

```bash
# Tera template: templates/api-docs.tera
{% for service in services %}
## {{ service.name }} Service

{% for method in service.methods %}
### {{ method.name }}
**Request:** {{ method.request }}
**Response:** {{ method.response }}
{% endfor %}
{% endfor %}

# Generated by ggen with SPARQL:
SELECT ?service ?method ?request ?response
WHERE {
  ?service rdf:type ggen:Microservice ;
           ggen:hasMethod ?method .
  ?method ggen:requestType ?request ;
          ggen:responseType ?response .
}
```

## Best Practices

### 1. Single Source of Truth
- Edit .ttl files (specifications)
- Never edit generated .md files
- Generated outputs are projections of RDF ontology

### 2. Semantic Clarity
- Use appropriate classes (Policy, Signal, Service)
- Use consistent predicates (ggen:requires, ggen:provides)
- Avoid ambiguous naming

### 3. Validation First
```bash
# Always validate before generating
ggen validate .specify/specs/*.ttl --strict

# This prevents defects before generation
```

### 4. Complete Specifications
```bash
# Check 100% coverage
ggen sync --validate_only true

# Exit code 0 = complete, 1 = incomplete
```

## Tools for RDF Development

### Query and Explore
```bash
# SPARQL query against all TTL
ggen query "SELECT ?s ?p ?o WHERE { ?s ?p ?o } LIMIT 100"

# List all defined classes
ggen query "SELECT ?class WHERE { ?class rdf:type owl:Class }"

# Find instances of Policy
ggen query "SELECT ?policy WHERE { ?policy rdf:type ggen:Policy }"
```

### Validate
```bash
# Check SHACL compliance
ggen validate .specify/specs/001-governance/feature.ttl

# Generate validation report
ggen validate .specify/ --format json > validation-report.json
```

### Generate Documentation
```bash
# Render all markdown from TTL
ggen sync --dry_run true

# Preview changes without writing files
```

## References

- **Turtle Format:** https://www.w3.org/TR/turtle/
- **RDF Concepts:** https://www.w3.org/TR/rdf-concepts/
- **SPARQL 1.1:** https://www.w3.org/TR/sparql11-query/
- **SHACL:** https://www.w3.org/TR/shacl/
- **OWL 2:** https://www.w3.org/TR/owl2-overview/
- **ggen RDF Guide:** See `/docs/releases/v0.2.0/INDEX.md`

## Document Maintenance

- **Last Updated:** 2026-01-25
- **Versions:** RDF Ontology v0.2.0 (production-ready)
- **Audience:** Architects, RDF specialists, specification authors
