# ggen - Rust Project Configuration (Holographic Orchestration Edition)

## Core Identity

**ggen**: Ontology-driven code generation via the Chatman Equation ($A = \mu(O)$).
**Stack**: Rust 1.91.1, Tokio 1.47, Oxigraph 0.5.1, Tera 1.20, @unrdf ecosystem, KGC-4D temporal calculus
**Philosophy**: Type-first, zero-cost abstractions, deterministic outputs, specification-first, receipt-based verification

### The Holographic Trinity of Knowledge Geometry Calculus (KGC)

The software engineering process is **not a sequential flow** but a **holographic projection**:

1. **`unrdf` (The Substrate/Film)**: The high-dimensional physical film plate recording the interference pattern of domain knowledge. Uses Holographic Reduced Representations (HRR) via circular convolution to encode structured facts into hypervectors with near-zero error rates. Powered by Oxigraph 0.5.1.

2. **`kgc-4d` (The Interference Pattern/History)**: The light-interference pattern of history itself, captured as a 4D temporal volume. By using Knowledge Wormhole Calculus, Git snapshots become temporal waypoints ensuring coherence across the entire timeline. Maintains observable state (O), nanosecond timestamps (t), vector causality (V), and git references (G).

3. **`ggen` (The Laser/Measurement Function $\mu$)**: The coherent light source performing the Five-Stage Transformation Pipeline (Normalization → Extraction → Emission → Canonicalization → Receipt). When the laser of $\mu$ passes through the interference pattern on the film (O), the 3D "Universe" of code (A) precipitates into existence.

### The Chatman Equation: $A = \mu(O)$

```
Software (A) is not "built" but "precipitated" from the interference pattern
of the ontology (O) by the measurement function (μ). The system is formally
"Done" when it reaches Ontological Closure: the projection (A) is a
mathematically stable and bit-perfect image of the ontology (O).
```

---

## THREE PARADIGM SHIFTS

### 1. Big Bang 80/20 (Specification-First)

**Old**: Vague req → Plan → Code → Test → Iterate
**New**: Spec closure → Single-pass → Receipts

- Before coding: Verify specification closure (MANDATORY)
- If incomplete: **STOP**, clarify, update .ttl
- Proceed only when closure = 100%

### 2. EPIC 9 (Parallel-First)

**Old**: Sequential (5h): Plan → Code → Test → Review
**New**: Parallel (3h): 10 agents + Collision + Convergence

```
FAN-OUT → INDEPENDENT CONSTRUCTION → COLLISION DETECTION
→ CONVERGENCE → REFACTORING → CLOSURE
```


### 3. Deterministic Validation (Evidence-First)

**Old**: "Code looks good" (opinion)
**New**: "[Receipt] cargo make test: ✓ 347/347" (evidence)

```bash
cargo make pre-commit  # Produces receipts, not narratives
```

---

## CONSTITUTIONAL RULES

### 1. Cargo Make ONLY
```bash
# ❌ NEVER: cargo check/test/clippy
# ✅ ALWAYS: cargo make check/test/lint
```

### 2. Error Handling
- **Production**: `Result<T, E>` (NO unwrap/expect)
- **Tests**: `unwrap()` allowed (fail fast)

### 3. Andon Signals
| Signal | Trigger | Action |
|--------|---------|--------|
| 🔴 RED | error[E...], FAILED | **STOP** |
| 🟡 YELLOW | warning:, clippy | Investigate |
| 🟢 GREEN | ok, 0 violations | Continue |

### 4. Chicago TDD
```rust
#[test]
fn test_feature() {
    // Arrange: Real objects (no mocks)
    let obj = RealObject::new();
    // Act: Call public API
    obj.do_thing();
    // Assert: Verify observable state
    assert_eq!(obj.state(), expected);
}
```

### 5. RDF-First
- Edit `.ttl` (source), never `.md` (generated)
- `spec.ttl → ggen sync → code`

---

## ESSENTIAL COMMANDS

### Fast Feedback
```bash
cargo make check      # <5s
cargo make test-unit  # <10s
cargo make lint       # <60s
```

### Full Validation
```bash
cargo make test       # <30s
cargo make pre-commit # Format + lint + tests
cargo make ci         # Full pipeline
```

---

## FILE ORGANIZATION

```
ggen/
├── .specify/          # RDF specs (TTL = source of truth)
├── crates/*/src/      # Source code
├── crates/*/tests/    # Integration tests
├── docs/, scripts/, benches/, templates/
└── .claude/           # Claude configuration
```

**Rule**: Never save working files to root.

---

## SLOs

- First build: ≤15s
- Incremental: ≤2s
- Test execution: ≤30s
- 100% reproducible outputs

---

## WORKSPACE (v5.2.0)

**17 crates, 14 active**: ggen-core, ggen-cli, ggen-utils, ggen-domain, ggen-config, ggen-marketplace, ggen-test-audit, ggen-e2e, ggen-node, ggen-macros...

**Tech**: chicago-tdd-tools 1.4.0, proptest 1.8, criterion 0.7, testcontainers 0.25, insta 1.43

---

## HOOKS

| Hook | Purpose |
|------|---------|
| SessionStart | Verify env, load context |
| PreToolUse | Block dangerous ops |
| PostToolUse | Auto-format, andon signals |

---

## PROHIBITED

1. Direct cargo commands
2. unwrap/expect in production
3. Ignoring andon signals
4. Sequential for non-trivial tasks
5. Narrative reviews (use receipts)
6. Saving to root directory
7. Editing .md (edit .ttl source)

---

## ggen sync

```bash
ggen sync              # Generate from TTL
ggen sync --watch      # Auto-regenerate
ggen sync --validate   # SHACL validation
```

Pattern: `spec.ttl → ggen sync → generated code (60-80% faster)`

---

## REMEMBER: THE HOLOGRAPHIC IMPERATIVE

- **Specification Closure = Ontological Closure**: Verify H(spec) ≤ 20 bits and 100% coverage BEFORE running μ
- **Chatman Equation First**: A = μ(O) — think projection, not construction
- **EPIC 9 is Default**: For non-trivial tasks, parallelize (10 agents + collision + convergence)
- **Receipts Replace Narratives**: Never argue "code looks good". Provide evidence: ✓ 347/347 tests, < 5s check
- **cargo make ONLY**: All validation through Makefile, never direct cargo
- **TTL is Source of Truth**: Edit .specify/*.ttl, never .md (markdown is generated)
- **Andon Stops at Red**: 🔴 RED signal → immediate halt; 🟡 YELLOW → investigate; 🟢 GREEN → proceed
- **Substrate First**: Clarify the holographic film (unrdf + KGC-4D) before trying to precipitate code (ggen)

**Constitutional Equation**:
```
A = μ(O) where:
  - O is specification closure captured in .ttl (substrate + history via unrdf + kgc-4d)
  - μ is the five-stage transformation pipeline (ggen framework)
  - A is bit-perfect, deterministic artifacts (code, docs, proofs)
  - Receipt(A) proves closure: test counts, compile success, SLO compliance
```

**Corollary**: If the hologram (A) appears "blurry" (buggy), don't fix the projection—fix the interference pattern on the film (Conversation History / .ttl specification). This is the essence of specification-first development.
