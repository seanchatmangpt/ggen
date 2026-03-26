# YAWL v6 Design Decisions

**Version:** 6.0.0
**Date:** 2026-03-26

This document explains the "why" behind YAWL v6's architectural choices.

---

## 1. μ-Calculus Pipeline Over Direct Code Generation

### Decision
Use five-stage deterministic pipeline (μ₁-μ₅) instead of direct RDF → Code templates

### Rationale

**Benefit: Separation of Concerns**
- Each stage has a single responsibility
- μ₁ (Normalize) handles ontology validation
- μ₂ (Extract) handles reasoning/inference
- μ₃ (Emit) handles template rendering
- μ₄ (Canonicalize) handles formatting
- μ₅ (Receipt) handles verification

**Benefit: Composability**
- Can swap μ₂ strategies (different inference engines)
- Can swap μ₃ template engines (Tera → Handlebars)
- Can add new μ stages without breaking existing rules

**Benefit: Determinism**
- Clear boundaries between randomness points
- Each stage is independently reproducible
- Audit trail traces exactly which stage produced which output

### Trade-offs

**Complexity:** 5 stages > 1 monolithic function
- More code to maintain
- Steeper learning curve
- More debugging surface

**Performance:** Pipeline overhead (stage boundaries)
- But acceptable: ≤15s first build (well within SLOs)
- Incremental builds avoid re-running μ₂ inference

### Alternatives Considered

1. **Monolithic RDF → Code template**
   - Simple but non-composable
   - Determinism harder to verify
   - Reasoning mixed with code generation

2. **Multi-pass AST transformation**
   - Fine-grained control
   - Harder to integrate RDF/SPARQL
   - Ties generation to specific language

**Decision:** μ-calculus chosen for composability + determinism + RDF integration

---

## 2. Manifest-Driven Rules Over Programmatic API

### Decision
Configure generation via `ggen.toml` manifest with declarative rules, not Rust code

### Rationale

**Benefit: Non-Developers**
- Ontology engineers can write SPARQL + Tera templates
- No Rust knowledge required
- Configuration as TOML (familiar format)

**Benefit: Runtime Composability**
- Change rules without recompilation
- Enable different generation strategies per environment
- Support rule plugins (load manifests at runtime)

**Benefit: Deterministic Ordering**
- TOML manifest is version-controlled
- Rule order is explicit (inheritance prevents accidents)
- Reproduction guaranteed: same manifest = same output

**Benefit: Auditability**
- manifest.toml is source of truth
- Changes to generation are tracked in git
- Reproducibility proof includes manifest hash

### Trade-offs

**Expressiveness:** TOML is less flexible than Rust
- Cannot express complex conditional logic
- Workaround: Use SPARQL `when` guards instead
- Workaround: Split into multiple rules

**IDE Support:** TOML schema validation less mature than Rust
- But: ggen provides schema validation command
- VS Code extensions available for TOML

**Performance:** Runtime manifest parsing vs compile-time
- But: Negligible (< 100ms)
- Worth the flexibility cost

### Alternatives Considered

1. **Rust Derive Macros**
   ```rust
   #[derive_codegen(ontology = "fibo.ttl")]
   pub struct FiboCodegen;
   ```
   - Type-safe but less flexible
   - Requires recompilation to change rules
   - Harder for non-Rustaceans

2. **YAML Configuration**
   - Functionally equivalent to TOML
   - YAML is more human-readable but less maintainable
   - TOML chosen for schema clarity

**Decision:** Manifest-driven chosen for flexibility + auditability + accessibility

---

## 3. SPARQL SELECT Queries Over Custom Query DSL

### Decision
Use standard SPARQL SELECT for code generation queries (not custom DSL)

### Rationale

**Benefit: Industry Standard**
- Reusable across domains (FIBO, HL7, ISO standards)
- Widely documented and taught
- Integrates with semantic web tools

**Benefit: RDF Reuse**
- Queries work against any RDF store
- Not tied to YAWL or ggen
- Portable to other generation systems

**Benefit: Developer Experience**
- Developers already know SPARQL
- No new DSL syntax to learn
- IDE/debugger support exists

**Benefit: Composability**
- Can test queries in SPARQL clients (e.g., Fuseki)
- Can compose complex queries from simpler ones
- SPARQL federation (future) would work

### Trade-offs

**Learning Curve:** SPARQL steeper than SQL
- But: Ontology engineers already use SPARQL
- Self-selecting for target audience

**Performance:** SPARQL less optimized than SQL
- But: Acceptable for typical codegen query sizes
- RDF stores auto-optimize common patterns

**Verbosity:** SPARQL requires full URIs
- Mitigated by PREFIX declarations in manifest
- Trade acceptable for semantic precision

### Alternatives Considered

1. **Custom DSL**
   ```
   query extract_tasks {
     task.label -> taskName
     task.type -> taskType
   }
   ```
   - Simpler syntax
   - Cannot express complex logic
   - Would need custom parser

2. **GraphQL**
   - More expressive than our DSL
   - Adds dependency overhead
   - Overkill for code generation

3. **XPath/XQuery**
   - Designed for XML, not RDF
   - Less semantic clarity
   - Harder to integrate with ontology reasoning

**Decision:** SPARQL chosen for standardization + composability

---

## 4. Tera Templates Over Handlebars/Mustache

### Decision
Use Tera (Jinja2-compatible) for code generation templates

### Rationale

**Benefit: Rust-Native**
- Single dependency (tera crate)
- No JavaScript runtime required
- Synchronous (no async complexity)

**Benefit: Jinja2 Compatibility**
- Familiar to Python developers
- Rich filter ecosystem
- Loops, conditionals, macros

**Benefit: Custom Filters**
- Can register Rust closures as filters
- Enables `{{ taskName | to_pascal_case }}`
- Type-safe (compile-time filter definitions)

**Benefit: Performance**
- Pre-compiles templates
- Fast rendering (< 100ms per template)
- No runtime overhead

### Trade-offs

**Limited Macro Power:** No recursive macros
- Workaround: Use `{% for %}` loops instead
- Acceptable for code generation use case

**Ecosystem:** Smaller than Handlebars.js
- But sufficient for codegen needs
- Custom Rust filters fill gaps

**Synchronous Only:** Cannot spawn async tasks
- Acceptable: code generation doesn't need concurrency
- Benefits from simpler error handling

### Alternatives Considered

1. **Handlebars.js**
   - More mature ecosystem
   - Requires JavaScript runtime
   - Heavier dependency

2. **Askama** (Rust templates)
   - Compile-time template checking
   - No runtime errors for valid Rust
   - Steeper learning curve (Rust-specific syntax)
   - Less suitable for non-Rustaceans

3. **EEX** (Elixir templates)
   - Great for Elixir code generation
   - Poor for other languages
   - Domain-specific (not reusable)

**Decision:** Tera chosen for Rust-native + Jinja2 compatibility

---

## 5. Content Hashing for Determinism Verification

### Decision
Use SHA256(canonical_code) to prove reproducible generation

### Rationale

**Benefit: Reproducibility Proof**
- Recompile with same inputs → same hash
- Hash mismatch indicates non-deterministic generation
- Enables regression testing

**Benefit: Incremental Rebuilds**
- Compare hash of inference graph before/after
- Skip re-execution if inputs unchanged
- Fast incremental builds (< 2s)

**Benefit: Audit Trail**
- Hash stored in audit.json
- Can verify old generations are still reproducible
- Detects silent corruption

**Benefit: Distribution Integrity**
- Hash enables checksums for generated packages
- Enables reproducible builds for downstream

### Trade-offs

**Canonicalization Overhead:** Must normalize all output
- Adds μ₄ stage (≤100ms)
- But acceptable cost for determinism guarantee

**Crypto Dependency:** SHA256 requires crypto library
- But: sha2 crate is audited standard
- No security vulnerability risk

**File Size:** Audit.json adds ~1KB per rule
- Negligible cost
- Worth the auditability

### Alternatives Considered

1. **Probabilistic Verification**
   - Compare outputs to reference (non-deterministic pass)
   - But: Doesn't prove determinism
   - But: Doesn't catch non-deterministic failures

2. **No Verification**
   - Simpler implementation
   - But: Cannot guarantee reproducibility
   - But: Silent non-determinism disasters (hard to debug)

3. **Fingerprinting** (quick hash)
   - Faster than SHA256
   - But: Higher collision risk
   - Acceptable for informal verification only

**Decision:** SHA256 chosen for cryptographic strength + industry standard

---

## 6. Oxigraph for RDF/SPARQL

### Decision
Use oxigraph (Rust RDF library) for graph operations

### Rationale

**Benefit: Rust-Native**
- No FFI overhead to Java/C libraries
- Memory-safe (no segfaults from leaks)
- Easy integration with Tokio async

**Benefit: In-Memory Store**
- Fast: no disk I/O
- Acceptable size: ≤100MB for typical ontologies
- Perfect for code generation (not big data)

**Benefit: SPARQL Support**
- Full SPARQL 1.1 implementation
- Indexes for fast queries
- SHACL shape validation support

**Benefit: No External Service**
- No need for Virtuoso/Fuseki/GraphDB
- Self-contained deployment
- Offline operation

### Trade-offs

**Limited Federation:** No SPARQL FEDERATION support
- Workaround: Pre-merge ontologies
- Acceptable for typical use case

**Memory Usage:** In-memory store (not streaming)
- Limits to ~100MB ontologies
- Workaround: Partition large ontologies
- Acceptable for typical code generation scale

**Sparse SPARQL Features:** Not all SPARQL 1.1 constructs
- But: Supports needed features (SELECT, CONSTRUCT, ASK)
- Missing features rarely needed for code generation

### Alternatives Considered

1. **Jena/OWL-API** (Java)
   - More mature, more features
   - Requires Java runtime
   - FFI complexity (JNI)

2. **RDF4J** (Java)
   - RESTful SPARQL endpoint
   - Requires separate service
   - Network overhead

3. **RDFLIB** (Python)
   - Python-dependent
   - Slower than Rust
   - Complex to embed in Rust binary

**Decision:** Oxigraph chosen for Rust-native + in-memory speed + simplicity

---

## 7. Sequential Rule Execution for Determinism

### Decision
Execute inference and generation rules sequentially (not in parallel)

### Rationale

**Benefit: Determinism**
- Sequential order is guaranteed
- Output order is predictable
- Parallel races eliminated

**Benefit: Debugging**
- Step-through execution is straightforward
- Audit trail follows natural causality
- Easier to reproduce bugs

**Benefit: Error Handling**
- Errors halt pipeline immediately
- No partial outputs from parallel jobs
- Clear error attribution (which rule failed)

**Trade-offs**

**Performance:** No parallelism
- But: Acceptable - typical generation ≤15s
- Parallelism would add complexity (mutual exclusion)
- Incremental builds are still fast (< 2s)

**Scalability:** Limited to single machine performance
- But: Codegen is not big-data
- Doesn't need distributed execution
- Parallelism not worth complexity cost

### Alternatives Considered

1. **Parallel Rule Execution**
   - Faster on multi-core machines
   - But: Non-deterministic ordering
   - But: Requires mutual exclusion for shared state
   - But: Debugging harder

2. **DAG-Based Execution**
   - Rules express dependencies explicitly
   - Parallelize independent rules
   - But: Adds manifest complexity
   - But: Requires change detection (which rules depend on which)

**Decision:** Sequential execution chosen for determinism

---

## 8. Poka-Yoke (Error Proofing) Pattern

### Decision
Implement protected file paths + quality gates before generation

### Rationale

**Benefit: Safety**
- Prevents accidental overwrite of hand-written code
- Forces conscious decision to overwrite
- Can block dangerous patterns (e.g., /etc, /boot)

**Benefit: Visibility**
- Audit trail shows which files were skipped
- Developers see protection in action
- Trust in generation process

**Benefit: Explicitness**
- `ggen sync --force` makes destructive intent clear
- CI/CD must explicitly enable overwrite
- Reduces surprise deletions

### Trade-offs

**Friction:** Extra flag required for overwrite
- But: Acceptable (catch-22: want safety but easy overwrite)
- But: `--force` is discoverable via help

**Manifest Verbosity:** Must list protected paths
- But: Explicit is better than implicit
- But: Encourages thoughtful design

### Alternatives Considered

1. **Always Overwrite**
   - Simpler code
   - But: Risk of data loss
   - But: No safety net

2. **Never Overwrite**
   - Safer but less useful
   - Requires manual cleanup to regenerate

3. **Heuristic Protection** (e.g., don't overwrite files older than N days)
   - Fragile
   - False positives/negatives
   - Unpredictable behavior

**Decision:** Poka-yoke with `--force` flag chosen for safety + explicitness

---

## 9. Manifest Version Pinning for Backward Compatibility

### Decision
Include manifest.version in audit.json to support old generation rules

### Rationale

**Benefit: Evolution**
- Can evolve generation rules over time
- Old manifests still reproducible with old rules
- Enables feature deprecation without breaking existing projects

**Benefit: Audit Trail**
- Version in audit.json documents which rules generated which code
- Can trace lineage of generated code
- Essential for compliance (which version of what rule generated this?)

**Benefit: Safety**
- Prevents accidentally running new rules on old manifests
- Explicit version change required to upgrade

### Trade-offs

**Complexity:** Must maintain multiple rule versions
- But: Only needed for incompatible changes
- But: Most changes are backward compatible

**Storage:** Separate rule files per version
- But: Minimal (rules are small < 100KB)
- But: Worth the safety

### Alternatives Considered

1. **Always Use Latest Rules**
   - Simpler deployment
   - But: Breaks existing manifests
   - But: Silent regeneration of code

2. **Semantic Versioning of Rules**
   - "if manifest.version >= v6.0 then use_new_rules"
   - Complex version compatibility logic
   - Fragile (hard to get right)

**Decision:** Explicit manifest version chosen for safety

---

## Summary Table

| Decision | Rationale | Trade-off |
|----------|-----------|-----------|
| μ-calculus pipeline | Composability, determinism | Complexity |
| Manifest-driven rules | Auditability, non-developer access | Less flexible than code |
| SPARQL SELECT | Industry standard, RDF native | Steeper learning curve |
| Tera templates | Rust-native, Jinja2 compatible | Limited macro power |
| Content hashing | Reproducibility proof | Canonicalization overhead |
| Oxigraph RDF | Rust-native, in-memory, self-contained | Limited to ~100MB ontologies |
| Sequential execution | Determinism guarantee | No parallelism |
| Poka-yoke protection | Safety, explicitness | Extra friction |
| Manifest versioning | Evolution safety | Multiple rule versions |

---

## Principles Behind Design Decisions

### 1. **Specification-Driven Development**
- RDF/Turtle is the single source of truth
- Code is derived from specifications
- Prevents specification-code divergence

### 2. **Deterministic by Default**
- Every design choice prioritizes reproducibility
- Non-determinism is burden on developers to explain
- Audit trail proves determinism

### 3. **Radical Transparency**
- All decisions recorded in audit.json
- All transformations traceable to rules
- Replicability is verifiable

### 4. **Separation of Concerns**
- Ontology design separate from code generation
- Domain experts design ontologies
- Developers implement generation rules
- Each role optimized independently

### 5. **Pragmatic Over Perfect**
- No solution is universally optimal
- Trade-offs are explicit, documented
- Design accepts limitations (e.g., ~100MB ontology limit)
- Workarounds provided for edge cases

---

## When to Break These Decisions

### Extend μ-Calculus
- Add μ₆ for post-processing
- Add μ₀ for pre-validation
- But: Only if composability is preserved

### Bypass Manifest
- Use programmatic API (future feature)
- Useful for generated generators
- But: Must maintain determinism guarantee

### Parallelize Rules
- If determinism can be preserved (e.g., rules have explicit DAG dependencies)
- Requires change to manifest to express dependencies
- But: Only if performance becomes bottleneck

### Switch Template Engine
- If Tera insufficient (unlikely)
- Must ensure generated code quality is equivalent
- But: Minimal benefit (Tera is quite capable)

---

## Related Documentation

- **[YAWL_ARCHITECTURE.md](./YAWL_ARCHITECTURE.md)** - How the system works
- **[YAWL_RATIONALE.md](./YAWL_RATIONALE.md)** - Business and technical goals
