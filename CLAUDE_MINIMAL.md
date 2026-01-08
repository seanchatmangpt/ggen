# ggen (Essential Configuration)

**ggen precipitates code from RDF ontologies via A = μ(O). Big Bang 80/20: verify spec closure first, single-pass generation. EPIC 9: 10 parallel agents for non-trivial work. Receipts (test counts, SLOs) prove closure.**

---

## Quick Rules

| Rule | Details |
|------|---------|
| **Spec First** | `.specify/*.ttl` is source of truth. Code derives from RDF. 100% closure before generation. |
| **Cargo Make** | `cargo make [target]` always (never raw `cargo`). Enforces SLOs, gates, timeouts. |
| **Result<T,E>** | Production code: `Result<T,E>` throughout. Tests: `unwrap()` OK. |
| **Andon Signals** | 🔴 RED = stop. 🟡 YELLOW = investigate. 🟢 GREEN = proceed. |
| **Receipts** | Evidence replaces narrative: `[Receipt] cargo make test: ✓ 347/347, <30s` |
| **No Unwrap** | Production = zero `unwrap/expect`. Errors handled via `Result`. |
| **RDF is Truth** | Edit `.ttl` (source). Never edit `.md` (generated). |

---

## Essential Commands

```bash
cargo make check       # <5s    Compile
cargo make test        # <30s   All tests
cargo make pre-commit  # <2min  Full validation
ggen sync              # RDF spec → generated code
```

---

## Three Paradigms

1. **Big Bang 80/20**: Spec closure (100%) → single-pass generation → receipts. No iteration.
2. **EPIC 9**: 10 parallel agents (fan-out) → collision detection → convergence → optimal synthesis.
3. **Deterministic Receipts**: Evidence over opinion. SLO compliance + test counts = closure proof.

---

## File Structure

```
ggen/
├── .specify/           # RDF specs (Turtle format)
├── crates/*/src/       # Rust source
├── crates/*/tests/     # Tests
└── docs/, templates/, benches/
```

---

## Holographic Factory (Philosophy)

**O** = RDF ontology (substrate, interference pattern)
**μ** = ggen transformation pipeline (laser, measurement)
**A** = generated code (projection, 3D universe)

**Receipt(A)** = proof that A matches O (test counts, SLOs, hashes)

**Corollary**: Bug in code? Fix the RDF spec (interference pattern), not the code (projection).

---

## Stack

Rust 1.91.1 | Tokio 1.47 | Oxigraph 0.5.1 | Tera 1.20 | chicago-tdd-tools 1.4.0 | proptest, criterion

---

## Automatic (No Reminders Needed)

I implement by default:
- Result<T,E> for all fallible operations
- Zero unwrap/expect in production
- Chicago TDD (AAA: Arrange/Act/Assert, real objects)
- Type-safe design (constraints in types)
- Error context mapping
- Idiomatic Rust (clippy compliance)
- Performance analysis (SLO targets)

---

## Detailed Reference

See `CLAUDE_SPR.md` for full specification, `.claude/AGENTS_SKILLS_REGISTRY.md` for agent/skill reference.

