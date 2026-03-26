# YAWL v6 Architecture Rationale

**Version:** 6.0.0
**Date:** 2026-03-26

This document answers the question: **Why was YAWL v6 designed this way?**

---

## Business Goals

### 1. Reduce Time-to-Code for Domain Experts

**Goal:** Non-programmers should generate code without learning Rust/Python/Java

**How YAWL Achieves This:**
- Ontology engineers design specifications in RDF (their native format)
- SPARQL queries extract facts (ontology engineers know SPARQL)
- Tera templates render results (non-technical designers can write Tera)
- Result: Developers + ontologists = code (no programmer bottleneck)

**Measurement:** Time from specification to working code < 1 hour

### 2. Ensure Consistency Across Generated Artifacts

**Goal:** All generated code follows same patterns, naming conventions, structure

**How YAWL Achieves This:**
- Tera templates enforce code style (indentation, naming, structure)
- SPARQL ensures consistent fact extraction (same query = same facts)
- Canonicalization ensures deterministic formatting
- Result: Every generated file looks like it came from same codebase

**Measurement:** Code review friction < 10% of hand-written code

### 3. Enable Rapid Evolution of Generation Rules

**Goal:** Update code generation strategy without recompiling, redeploying

**How YAWL Achieves This:**
- Manifest-driven rules (change ggen.toml, re-run `ggen sync`)
- Rules are composable (add new rule without touching existing ones)
- Version pinning ensures old manifests still work
- Result: A/B test generation strategies, quick rollback

**Measurement:** New rule deployment < 5 minutes

### 4. Achieve Reproducible Builds

**Goal:** Given same inputs, always produce same outputs (audit trail + proof)

**How YAWL Achieves This:**
- μ-calculus pipeline enforces stage boundaries
- Content hashing proves reproducibility
- Deterministic rule ordering (no races)
- Audit trail shows exactly what produced what
- Result: Legal/compliance audit = simple (hash verification)

**Measurement:** Diff-zero rebuilds with same manifest/ontology

### 5. Support Multi-Domain Code Generation

**Goal:** One framework for YAWL, Elixir, Kubernetes, Terraform, etc.

**How YAWL Achieves This:**
- Generic μ-calculus pipeline (ggen-core)
- Domain-specific specialization (ggen-yawl, ggen-craftplan, etc.)
- Manifest-driven (no code changes needed for new domain)
- SPARQL is domain-agnostic (works for any ontology)
- Result: ~80% code reuse across domains

**Measurement:** New domain generation framework < 20% new code

---

## Technical Goals

### 1. Zero-Cost Abstractions

**Goal:** Pipeline stages should not add overhead vs direct code generation

**How YAWL Achieves This:**
- Stages are simple transformations (not indirection)
- Tera is compiled (no interpretation overhead)
- Oxigraph queries are indexed (SPARQL execution is O(log n) or O(1))
- Canonicalization is single-pass (O(n) in code size)
- Result: Typical generation < 15s (acceptable for development loop)

**Measurement:** Time breakdown:
```
μ₁ Normalize      ~500ms (RDF parsing)
μ₂ Extract        ~1000ms (SPARQL inference + queries)
μ₃ Emit           ~5000ms (template rendering)
μ₄ Canonicalize   ~100ms (formatting)
μ₅ Receipt        ~50ms (hashing + JSON)
────────────────────────────
Total             ~6650ms first build
```

### 2. Reusability via Composition

**Goal:** Rules, templates, queries should be reusable across projects

**How YAWL Achieves This:**
- SPARQL queries are standalone (testable in any SPARQL client)
- Tera templates are parameterized (work with different query results)
- Rules are declarative (no hidden dependencies)
- Manifests are composable (can include other manifests)
- Result: Template library, query library, rule templates

**Measurement:** Average rule reuse rate > 3 projects

### 3. Cognitive Load Minimization

**Goal:** Developers should understand generation without reading 5000 lines of code

**How YAWL Achieves This:**
- Five stages are conceptually simple (input → output at each stage)
- Audit trail explains everything (which rule produced which output)
- Manifest is readable (single TOML file expresses entire generation)
- Error messages are specific (include line numbers, code snippets)
- Result: New developer can understand architecture in < 2 hours

**Measurement:** Documentation completeness (100% of rules documented)

### 4. Maintenance Burden

**Goal:** Minimize breakage when Rust/Tera/Oxigraph upgrade

**How YAWL Achieves This:**
- Interfaces are stable (RDF/SPARQL are W3C standards)
- Dependencies are minimal (3 core: oxigraph, tera, serde)
- Test coverage is high (80%+ to catch regressions)
- Determinism guarantee (can verify regressions with audit trail)
- Result: Upgrade burden is bounded (can verify with hash comparison)

**Measurement:** Dependency upgrade cost < 1 person-day

### 5. Security Posture

**Goal:** Generated code should not contain vulnerabilities from generation process

**How YAWL Achieves This:**
- No code execution in templates (Tera renders, no eval())
- SPARQL injection protected (parameterized queries)
- No external service calls (self-contained oxigraph)
- File overwrite protected (poka-yoke, requires --force)
- Audit trail (all operations recorded)
- Result: Attack surface is small (manifest parsing + template rendering)

**Measurement:** Security audit completed (0 critical findings)

---

## Architectural Principles

### Principle 1: Single Source of Truth

**RDF/Turtle is the specification.** Code is derived from it.

**Benefit:** Specification-code divergence is impossible
- If code doesn't match spec, spec was changed
- All changes go through git (RDF files + manifest)
- Audit trail shows who changed what

**How We Enforce This:**
- ggen.toml.source = path to RDF file (immutable reference)
- SHACL shapes validate RDF (cannot generate if shapes violated)
- Audit.json includes RDF hash (detect spec changes)

### Principle 2: Determinism as Highest Priority

**Same inputs → Same outputs. Always.**

**Benefit:**
- Reproducible builds (critical for compliance)
- Bug reproducibility (same seed → same bug)
- CI/CD efficiency (cache based on input hashes)

**How We Enforce This:**
- μ₂a: Inference rules have explicit `order` field
- μ₂b: Generation rules execute in manifest order
- μ₃: SPARQL results are sorted
- μ₄: Output is canonicalized before hashing
- μ₅: Hash verification detects non-determinism

### Principle 3: Radical Transparency

**All decisions are recorded. Replicability is verifiable.**

**Benefit:**
- Audit trail explains everything
- Can reproduce old generation (version pinning)
- Compliance/legal proof is simple (show audit.json)

**How We Enforce This:**
- audit.json records every executed rule
- Hash of code proves deterministic generation
- SPARQL query hashes track what data was extracted
- Timestamps + versions enable versioning strategy

### Principle 4: Separation of Concerns

**Each role has a tool, each tool has one job.**

**Benefit:**
- Ontologists write RDF (not Rust)
- Developers write Tera templates (not SPARQL)
- Operations run ggen sync (not deploy code)

**How We Enforce This:**
- ggen-core: Generic pipeline (for developers)
- ggen-yawl: Domain specialization (for ontologists)
- ggen CLI: Operations tool (for DevOps)

### Principle 5: Pragmatism Over Perfection

**Good enough solutions that work in practice.**

**Benefit:**
- Ship faster (don't optimize prematurely)
- Accept trade-offs explicitly (documented)
- Provide workarounds for limitations

**How We Enforce This:**
- Limitations document workarounds
- Design decisions table shows trade-offs
- Performance SLOs are realistic (~15s typical)

---

## Trade-off Analysis

### Speed vs Reproducibility

**Trade-off:** Reproducibility requires deterministic ordering (slower)

**YAWL's Choice:** Reproducibility > Speed
- Reasoning: Code generation is done infrequently (< 10x per day)
- Speed benefit of parallelism: 2-3x speedup
- Reproducibility value: Priceless (enables audit trail)
- Verdict: Worth the cost

### Expressiveness vs Simplicity

**Trade-off:** Complex generation requires complex rules (harder to maintain)

**YAWL's Choice:** Simplicity > Expressiveness
- Reasoning: Most code generation is 80/20 (simple cases)
- Complex cases: Split into multiple rules
- Verdict: Acceptable (workaround for 20% of cases)

### Performance vs Genericity

**Trade-off:** Generic pipeline slower than specialized code

**YAWL's Choice:** Genericity > Performance
- Reasoning: Code generation is not performance-critical
- Generic pipeline enables reuse (2-3x developer productivity)
- Verdict: Worth the cost (~30% performance overhead acceptable)

### In-Memory vs Streaming

**Trade-off:** In-memory RDF store limited to ~100MB (streaming would scale)

**YAWL's Choice:** In-Memory > Streaming
- Reasoning: Code generation doesn't need big data
- Typical ontologies: 1-10MB (well within limit)
- Verdict: Acceptable (99% of use cases fit)

### Full SPARQL vs Simplified DSL

**Trade-off:** Full SPARQL steeper learning curve than simplified DSL

**YAWL's Choice:** Full SPARQL > Simplified DSL
- Reasoning: Developers already use SPARQL
- DSL would fragment ecosystem (non-standard)
- Verdict: Worth the cost (standard > custom)

---

## Why These Goals Matter

### Business Impact

1. **Faster Time-to-Market**
   - Ontologies → Code in hours (vs weeks of programming)
   - Reduces development cycle time
   - Enables rapid feature velocity

2. **Lower Development Cost**
   - Specification engineers (cheaper than programmers)
   - High code reuse (3+ domains share 80% of code)
   - Maintenance burden split across domains

3. **Compliance & Audit**
   - Reproducible builds (legal requirement for financial software)
   - Audit trail (regulatory compliance)
   - Content hashing (secure supply chain)

4. **Quality Improvement**
   - Consistency enforcement (no code review surprises)
   - Automated pattern application (no manual copy-paste)
   - Fewer human errors (rules are tested, not developers)

### Technical Impact

1. **Reduced Cognitive Load**
   - Architecture is understandable (μ-calculus is simple)
   - Determinism is verifiable (audit trail proves correctness)
   - Error messages are helpful (specific, actionable)

2. **Risk Mitigation**
   - Reproducibility (can rebuild from old manifests)
   - Auditability (compliance requirements met)
   - Composability (can test rules independently)

3. **Long-Term Maintainability**
   - Specification-code sync is automatic
   - Evolution is controlled (version pinning)
   - Debugging is straightforward (audit trail)

---

## Constraints & Assumptions

### Constraints We Accept

1. **Ontologies < 100MB**
   - In-memory RDF store limitation
   - Acceptable: 99% of ontologies are < 10MB
   - Workaround: Partition large ontologies

2. **Generation Rules Execute Sequentially**
   - No parallelism
   - Acceptable: Typical generation < 15s
   - Workaround: Split into multiple pipelines if needed

3. **Tera Templates Cannot Recurse**
   - Limited macro power
   - Acceptable: 95% of code generation is non-recursive
   - Workaround: Use loops instead of recursion

4. **SPARQL Federation Not Supported**
   - Cannot query across multiple endpoints
   - Acceptable: Merge ontologies offline
   - Workaround: Pre-merge, then run pipeline

### Assumptions We Make

1. **Developers are Familiar with SPARQL**
   - Assumption: Target audience knows semantic web
   - Supported by: Ontology engineering background

2. **Reproducibility is More Important than Speed**
   - Assumption: Compliance > Development convenience
   - Supported by: Financial/healthcare domains requiring audit

3. **Code Generation is Batch Process, Not Real-Time**
   - Assumption: Generate when manifest changes (< 10x/day)
   - Supported by: Typical development workflow

4. **Manifest Size is Small (<1MB)**
   - Assumption: Rules are declarative, not code
   - Supported by: Typical manifest is 100-500 lines TOML

---

## Future Evolution

### Short-term (v6.1-v6.5)

1. **Plugin Architecture**
   - Custom SPARQL filters (Rust functions in templates)
   - Custom Tera filters
   - Custom validation rules

2. **Incremental Caching**
   - Cache inference graph hashes
   - Skip unchanged rules
   - Enable fast iteration

3. **Manifest Composition**
   - `include "other.toml"` support
   - Enables rule libraries
   - Version-specific rule sets

### Medium-term (v7.x)

1. **Distributed Execution**
   - Parallel rule execution (with explicit DAG dependencies)
   - Distributed RDF stores (if needed)
   - CI/CD integration

2. **Template Compilation**
   - Tera → Rust code (compile templates, avoid runtime)
   - Better error messages (compile-time validation)
   - Performance improvement (~2x)

3. **Multi-Language Output**
   - Generate multiple languages from single ontology
   - Consistency across polyglot systems
   - Language-specific semantic differences

### Long-term (v8.x)

1. **AI-Assisted Generation**
   - LLM-powered rule generation (suggest SPARQL patterns)
   - Test case generation
   - Documentation generation

2. **Real-Time Streaming**
   - Continuous generation (manifest watch mode)
   - Incremental deployment
   - Live specification feedback

3. **Compliance Automation**
   - Automated audit trail validation
   - Traceability across supply chain
   - Provenance tracking

---

## Conclusion

YAWL v6 is designed to solve a specific problem: **Generate code from specifications reliably, consistently, and auditably.**

Every architectural decision—μ-calculus pipeline, manifest rules, SPARQL queries, Tera templates, content hashing—serves this goal.

The system accepts trade-offs (no parallelism, bounded ontology size, learning curve) because the benefits (reproducibility, auditability, domain reuse) far outweigh the costs in the target domain (regulated industries, semantic web integration, code consistency).

---

## Related Documentation

- **[YAWL_ARCHITECTURE.md](./YAWL_ARCHITECTURE.md)** - How it works
- **[YAWL_DESIGN_DECISIONS.md](./YAWL_DESIGN_DECISIONS.md)** - Why specific patterns
- **[docs/10-architecture/c4-*.md](../10-architecture/)** - Component diagrams
