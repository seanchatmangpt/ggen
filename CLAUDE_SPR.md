# ggen Configuration (SPR Edition)

**ggen precipitates code from ontologies via A = μ(O): O is RDF specification (substrate), μ is transformation pipeline (laser), A is generated artifacts (projection). Big Bang 80/20: spec closure first (mandatory), single-pass generation, receipts prove closure. EPIC 9: 10 parallel agents for non-trivial work, collision detection reconciles outputs, convergence synthesizes optimal solution.**

---

## Three Core Paradigms (Minimal)

1. **Big Bang 80/20**: Verify specification closure (100%) before any code generation. Single-pass construction, no iteration. Receipts (test counts, SLO compliance) prove ontological closure.

2. **EPIC 9**: Non-trivial tasks = 10 parallel agents (fan-out) → collision detection (overlap analysis) → convergence (synthesis) → optimal solution. Not democratic voting; reconciliation via selection pressure.

3. **Deterministic Receipts**: Evidence replaces narrative. `[Receipt] cargo make test: ✓ 347/347, <30s` is proof. No "code looks good" claims.

---

## Rules (Non-Negotiable)

- `cargo make [target]` always; never raw `cargo` (timeouts, gates bypass)
- Production: `Result<T,E>` throughout; tests: `unwrap()` OK
- 🔴 RED = stop immediately; 🟡 YELLOW = investigate; 🟢 GREEN = proceed
- Edit `.ttl` source (RDF specs); `.md` files are generated, never edited
- No files to root directory; `.specify/` = source of truth

---

## Essential Commands (SLO Targets)

```bash
cargo make check      # <5s    (compile)
cargo make test-unit  # <16s   (fast feedback)
cargo make test       # <30s   (full suite)
cargo make lint       # <60s   (clippy, fmt)
cargo make pre-commit # <2min  (all three)

ggen sync             # RDF spec → generated code (60-80% faster than hand-coding)
```

---

## Project Structure

```
ggen/
├── .specify/          # RDF specs (.ttl = source of truth)
├── crates/*/src/      # Rust source code
├── crates/*/tests/    # Integration tests
├── docs/, templates/, benches/
└── (no .claude in critical path; kept minimal for reference)
```

---

## Type-First Design Pattern

Express constraints in types, not runtime checks. Leverage compiler for invariant verification. Use NewType patterns for domain concepts. Generic abstractions with zero-cost (compiler erases at codegen).

---

## RDF/Ontology First Principle

`.specify/*.ttl` is source of truth. All generated artifacts (code, docs, configs) derive from RDF. Use SPARQL queries to extract and validate closure. Holographic Factory: TTL defines the interference pattern, ggen is the laser, code is the projection.

---

## Holographic Factory Metaphor (Core Philosophy)

**Substrate** (unrdf/Oxigraph): RDF ontology as high-dimensional film plate capturing domain knowledge. **History** (KGC-4D): Git snapshots as temporal waypoints ensuring coherence. **Transformation** (ggen): Five-stage pipeline (Normalize → Extract → Emit → Canonicalize → Receipt) precipitates code from ontology. **Corollary**: If generated code is buggy, fix the RDF spec (interference pattern), not the output (projection).

---

## File Organization Rules

- `.specify/` directory: RDF specifications (Turtle format)
- Crates follow pattern: `crates/{name}/src/`, `crates/{name}/tests/`
- Never create working files in root directory
- Templates in `templates/`; generated code in output targets

---

## Quality Gates (Pre-Commit)

1. `cargo make check` passes (zero errors)
2. `cargo make lint` passes (zero warnings, `-D warnings`)
3. `cargo make test` passes (all tests green)
4. No `unwrap/expect` in production code
5. All APIs return `Result<T, E>`
6. Deterministic outputs (same input → same output byte-for-byte)

---

## When to Use EPIC 9 (Parallel Agents)

- Non-trivial tasks (affects multiple files or systems)
- Multiple valid approaches with trade-offs
- Large architectural decisions
- Unclear requirements needing exploration

**Workflow**: Specification closure check → 10 parallel agents (fan-out) → collision detection → convergence orchestration → receipts.

---

## What Automatic (Don't Repeat Instructions For)

I implement automatically:
- `Result<T,E>` for all fallible operations (no need to state)
- Zero `unwrap/expect` in production (standard practice)
- Chicago TDD pattern (AAA: Arrange/Act/Assert, real objects, no mocks)
- Type-safe design (constraints in types, compiler verification)
- Performance awareness (SLO targets, complexity analysis)
- Error handling context (map_err, custom error types)
- Idiomatic Rust (clippy compliance, naming conventions)

---

## Stack

**Rust**: 1.91.1 | **Async**: Tokio 1.47 | **RDF**: Oxigraph 0.5.1 | **Templating**: Tera 1.20 | **Testing**: chicago-tdd-tools 1.4.0, proptest, criterion | **Crates**: 17 (ggen-core, ggen-cli, ggen-domain, ggen-infer, ggen-marketplace, ...)

---

## Remember

**Specification Closure**: Verify 100% coverage before code generation (non-negotiable).
**Receipts Over Narratives**: Always produce evidence (test counts, compile times, SLO metrics).
**RDF is Reality**: Edit `.ttl`; everything else (code, docs, configs) is projection.
**Andon Stops Work**: Red signals halt execution immediately; investigate yellows; green proceeds.
**Cargo Make is Law**: All validation through Makefile, never raw cargo (prevents bypassing gates).

