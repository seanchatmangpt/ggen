# ggen - Rust Project Configuration (80/20 Edition)

## Core Identity

**ggen**: Ontology-driven code generation. RDF → reproducible code.
**Stack**: Rust 1.91.1, Tokio 1.47, Oxigraph 0.5.1, Tera 1.20
**Philosophy**: Type-first, zero-cost abstractions, deterministic outputs

---

## THREE PARADIGM SHIFTS

### 1. Big Bang 80/20 (Specification-First)

**Old**: Vague req → Plan → Code → Test → Iterate
**New**: Spec closure → Single-pass → Receipts

- Before coding: `/speckit-verify` (MANDATORY)
- If incomplete: **STOP**, clarify, update .ttl
- Proceed only when closure = 100%

### 2. EPIC 9 (Parallel-First)

**Old**: Sequential (5h): Plan → Code → Test → Review
**New**: Parallel (3h): 10 agents + Collision + Convergence

```
FAN-OUT → INDEPENDENT CONSTRUCTION → COLLISION DETECTION
→ CONVERGENCE → REFACTORING → CLOSURE
```

Commands: `/speckit-verify` → `/bb80-parallel` → `/collision-detect` → `/convergence`

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

### EPIC 9
```bash
/speckit-verify [feature]  # Verify closure
/bb80-parallel "[spec]"    # 10 agents parallel
/collision-detect          # Analyze overlaps
/convergence               # Synthesize result
```

### Quality
```bash
/test-audit      # Mutation testing
/review-errors   # Error handling audit
/optimize        # Performance
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

## REMEMBER

- **Specification closure before EPIC 9**
- **EPIC 9 is default for non-trivial**
- **Receipts replace review**
- **cargo make ONLY**
- **TTL before code**

**Constitutional Equation**: `spec.md = μ(feature.ttl) | EPIC 9 is default | Receipts replace review`
