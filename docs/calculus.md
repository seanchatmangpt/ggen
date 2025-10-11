<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [ggen calculus (v1)](#ggen-calculus-v1)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# ggen calculus (v1)

The ggen calculus provides a formal mathematical foundation for understanding the ggen projection system. It defines the state space, transformation pipeline, and fundamental laws that ensure deterministic, reproducible code generation.

## State Definition

The system state Σ is defined as:

**Σ = ⟨T,G,S,C,B,A,M,σ⟩**

Where:
- **T**: Templates (transformation rules)
- **G**: RDF Graphs (semantic data)
- **S**: SHACL Shapes (validation constraints)
- **C**: Configuration (CLI, env, file sources)
- **B**: Bindings (variable assignments)
- **A**: Artifacts (generated code files)
- **M**: Matrix (projection specifications)
- **σ**: Seed (determinism control)

## Projection Pipeline

The core projection algorithm follows this functional composition:

```
project = write ∘ render* ∘ matrix? ∘ bind? ∘ shape ∘ load
```

### Pipeline Stages

1. **load**: Load RDF graphs and templates from sources
   - Parse Turtle, N-Triples, JSON-LD formats
   - Load template files with frontmatter metadata
   - Validate graph integrity and template syntax

2. **shape**: Apply SHACL validation constraints
   - Validate RDF graphs against shape definitions
   - Ensure data quality and structural integrity
   - Report validation errors with detailed context

3. **bind?**: Optional variable binding resolution
   - Extract variables from SPARQL queries in frontmatter
   - Bind query results to template variables
   - Handle missing or malformed bindings gracefully

4. **matrix?**: Optional matrix projection
   - Execute matrix SPARQL queries with ORDER BY clauses
   - Generate projection matrices for batch processing
   - Ensure deterministic ordering for reproducible results

5. **render***: Template rendering (iterative)
   - Apply templates to bound data
   - Generate intermediate representations
   - Handle template composition and inheritance

6. **write**: Artifact generation
   - Write generated code to filesystem
   - Ensure atomic writes and proper file permissions
   - Maintain deterministic output across runs

## Fundamental Laws

### 1. Determinism
**Law**: Given identical inputs (Σ, seed), the projection must produce identical outputs.

**Formal**: ∀Σ₁, Σ₂, σ: (Σ₁ = Σ₂ ∧ σ₁ = σ₂) ⇒ project(Σ₁, σ₁) = project(Σ₂, σ₂)

**Implementation**: All random operations are seeded, floating-point operations use deterministic algorithms, and file I/O is atomic.

### 2. Idempotent Write
**Law**: Writing the same projection multiple times produces no changes after the first write.

**Formal**: write(project(Σ, σ)) = write(project(Σ, σ)) ∘ write(project(Σ, σ))

**Implementation**: File modification times are preserved, and content comparison prevents unnecessary writes.

### 3. Precedence Hierarchy
**Law**: Configuration sources follow a strict precedence order.

**Hierarchy** (highest to lowest priority):
1. **CLI arguments** (runtime overrides)
2. **Environment variables** (system configuration)
3. **Project files** (.ggen.toml, ggen.toml)
4. **User configuration** (~/.config/ggen/)
5. **System defaults** (/etc/ggen/)
6. **Built-in defaults** (compiled constants)

### 4. Matrix Ordering Requirement
**Law**: All matrix SPARQL queries must include an ORDER BY clause.

**Rationale**: Ensures deterministic projection order and reproducible artifact generation.

**Enforcement**: The system validates all matrix queries at load time and rejects unordered queries.

## Mathematical Properties

### Commutativity
Some stages commute under specific conditions:
- `shape ∘ load = load ∘ shape` (when shapes don't depend on loaded data)
- `bind ∘ matrix = matrix ∘ bind` (when bindings don't affect matrix queries)

### Associativity
The pipeline is left-associative:
- `(render* ∘ matrix?) ∘ bind? = render* ∘ (matrix? ∘ bind?)`

### Identity Elements
- Identity template: renders input unchanged
- Identity matrix: projects all data without filtering
- Identity binding: passes through all variables

## Applications

The calculus enables:
- **Formal verification** of projection correctness
- **Optimization** of pipeline execution
- **Debugging** through state inspection
- **Testing** via property-based verification
- **Documentation** of system behavior

## Verification

The calculus is verified through:
- **Unit tests** for each pipeline stage
- **Property tests** for mathematical laws
- **Integration tests** for end-to-end projections
- **Performance benchmarks** for optimization validation
