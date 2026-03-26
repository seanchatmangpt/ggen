# ggen-codegen and ggen-yawl Architecture

Comprehensive system architecture documentation for the ggen specification-driven code generation framework.

## Five-Stage Transformation Pipeline

The core ggen system implements `A = μ(O)` where code precipitates from RDF ontologies through five deterministic stages:

```
μ₁ (Normalize)      → RDF validation and graph loading
μ₂ (Extract)        → SPARQL CONSTRUCT rule execution
μ₃ (Emit)           → Tera template rendering
μ₄ (Canonicalize)   → Deterministic formatting and hashing
μ₅ (Receipt)        → Validation and audit trail generation
```

## System Architecture

### Core Components

**ggen-core**: Generic code generation framework
- `GenerationPipeline`: Orchestrates five-stage transformation
- `Queryable` trait: Abstract RDF query interface
- `Renderable` trait: Abstract template rendering interface
- `Rule<Q, T>` pattern: Generic rule with composable traits

**ggen-yawl**: YAWL workflow generation (domain implementation)
- 10 SPARQL CONSTRUCT rules for workflow transformation
- Two-phase execution (agents → workflows)
- Tera template rendering for YAWL XML and Erlang
- Full integration with ggen-core pipeline

### Data Flow

```
Input (RDF)
    ↓ μ₁ (Normalize)
OntologyGraph
    ↓ μ₂ (Extract)
CodeGraph + Bindings
    ↓ μ₃ (Emit)
Generated Files
    ↓ μ₄ (Canonicalize)
Canonical Output + Hashes
    ↓ μ₅ (Receipt)
Verified Files + Audit Trail
```

## ggen-yawl Rule Execution

### Phase 1: Agent-Centric (Parallel)
- Rule 1: Discover Agents
- Rule 2: Discover Capabilities
- Rule 6: Extract Tasks (independent)

### Phase 2: Skill Matching (Serial with Deps)
- Rule 3: Match Skills (depends on 1, 2, 6)
- Rule 4: Construct Messages (depends on 3)
- Rule 5: Filter Protocols (depends on 1-4)

### Phase 3: Workflow Construction (Mixed)
- Rule 7: Extract Flows (depends on 6, parallel)
- Rule 8: Cardinality to Split/Join (depends on 7)
- Rule 9: Rules to Conditions (independent)
- Rule 10: Multiple Instance / Composite (depends on 6, 9)

## Performance Characteristics

| Operation | Target | Typical |
|-----------|--------|---------|
| RDF loading | <5s per 1k triples | ~2s |
| Rule execution (10 rules) | <2s | ~800ms |
| Template rendering | <1s | ~100ms |
| Canonicalization | <500ms | ~100ms |
| **Total pipeline** | **<15s** | **~10s** |
| Incremental (cached) | <2s | ~400ms |

## Module Organization

```
ggen-core/
├── codegen/          - Pipeline orchestration
├── graph/            - RDF graph storage
├── rdf/              - SPARQL abstractions
├── templates/        - Template rendering
├── validation/       - Quality gates
└── lifecycle/        - Execution state

ggen-yawl/
├── transform/        - SPARQL transformation
├── ontology/         - RDF loading
├── template/         - YAWL templating
└── codegen/          - XML serialization
```

## Error Handling

- **μ₁ Errors** (Validation): User-fixable, fail-fast
- **μ₂ Errors** (Query): Developer-fixable (SPARQL syntax)
- **μ₃ Errors** (Template): Developer-fixable (template syntax)
- **μ₄ Errors** (Canonicalization): Internal consistency (rare)
- **μ₅ Errors** (Verification): Validation failures, can be warnings

## Documentation

- [Framework Guide](./ggen-codegen/FRAMEWORK.md) - Detailed architecture and implementation guides
- [YAWL Rules Reference](./ggen-yawl/YAWL_RULES.md) - All 10 transformation rules with examples
- [Spring Boot Integration](./ggen-yawl/INTEGRATION.md) - Production deployment guide
