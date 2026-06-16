<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [Documentation Definition of Done (ggen v26.5.28)](#documentation-definition-of-done-ggen-v26528)
  - [Table of Contents](#table-of-contents)
  - [Overview](#overview)
  - [8 Documentation Surfaces](#8-documentation-surfaces)
    - [1. Architecture Documentation](#1-architecture-documentation)
    - [2. Rules & Policy Documentation](#2-rules--policy-documentation)
    - [3. API Documentation (Generated via `cargo doc`)](#3-api-documentation-generated-via-cargo-doc)
    - [4. Examples & Validation (Real Executable Code)](#4-examples--validation-real-executable-code)
    - [5. Specification Documentation (RDF-Driven Source)](#5-specification-documentation-rdf-driven-source)
    - [6. Process & Workflow Documentation](#6-process--workflow-documentation)
    - [7. Evidence-First Principle (MANDATORY)](#7-evidence-first-principle-mandatory)
    - [8. Searchability & Navigation (Cross-Linked Index)](#8-searchability--navigation-cross-linked-index)
  - [Master Validation Checklist](#master-validation-checklist)
  - [Implementation Example](#implementation-example)
    - [Scenario: Adding a New Crate to ggen](#scenario-adding-a-new-crate-to-ggen)
  - [Governance](#governance)
  - [See Also](#see-also)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Documentation Definition of Done (ggen v26.5.28)

> **Core Principle:** All documentation must reference REAL code paths, REAL OTEL output, and ACTUAL execution evidence. No fabricated examples, no generic placeholders, no theoretical claims without proof.

**Last Updated:** 2026-06-14  
**Version:** 1.0.0  
**Owner:** Project Architecture (verified by LSP survey and live tests)

---

## Table of Contents

1. [Overview](#overview)
2. [8 Documentation Surfaces](#8-documentation-surfaces)
3. [Evidence-First Principle](#evidence-first-principle)
4. [Master Validation Checklist](#master-validation-checklist)
5. [Implementation Example](#implementation-example)

---

## Overview

Documentation is **COMPLETE** only when:

- ✅ **Architecture docs** are verified by LSP and match Cargo.toml
- ✅ **Rules documentation** is modular, auto-loaded, and enforced by hooks
- ✅ **API documentation** is auto-generated and all doctests pass
- ✅ **Examples** are real Rust code that compiles and runs
- ✅ **Specifications** are RDF/Turtle (source), markdown is auto-generated
- ✅ **Process documentation** is traceable to live hooks and gates
- ✅ **All evidence is real** — no fabricated output, no generic placeholders
- ✅ **Documentation is navigable** — single entry point, all links valid

**Any missing surface = Definition of Done NOT MET.**

---

## 8 Documentation Surfaces

### 1. Architecture Documentation

**What it is:** High-level system design, container diagrams, dependency maps, and cross-cutting patterns.

**Location:**
- Primary: `docs/architecture/COMPRESSED_REFERENCE.md`
- Reference: `CLAUDE.md` (Architecture Reference section)

**Source of Truth:**
- Live codebase (`Cargo.toml` members, actual trait definitions)
- LSP workspace survey (verifies real dependencies)

**Validation:**
```bash
# Verify crates match Cargo.toml
grep -A 20 '\[workspace\]' Cargo.toml | grep 'members ='

# Verify no stale paths in COMPRESSED_REFERENCE.md
LSP workspaceSymbol <ggen-core>

# Verify C4 diagram is actual (not generic)
# Should show real dependencies: GCORE→GMKT, CLI→GCORE→GRECEIPT
```

**Forbidden:**
- Generic C4 templates
- Crate descriptions without Cargo.toml verification
- Dependency diagrams not verified by LSP

**Required:**
- Real crate names verified against `Cargo.toml`
- Actual trait/function signatures from `lib.rs` files
- Genuine dependencies with real call sites
- Last verified date (must be within 30 days)
- LSP survey methodology documented

**Update Trigger:**
- Cargo.toml members change (add/remove crate)
- New public trait/type added to any `lib.rs`
- Error enum refactored
- More than 3 months since last verification

---

### 2. Rules & Policy Documentation

**What it is:** Modular, auto-loaded development policy enforced via hooks and validation gates.

**Location:**
- Entry point: `.claude/rules/README.md`
- Auto-loaded: `.claude/rules/_core/`
- Policy rules: `.claude/rules/rust/`, `.claude/rules/andon/`

**Source of Truth:**
- Live rules applied daily by developers and agents (not code artifacts)

**Content Must Include:**
- Absolute Rules (6 non-negotiable)
- Workflow (4 steps: spec→test→ontology→commit)
- Rust practices (LSP-first, Chicago TDD, performance)
- Quality gates (Andon signals, OTEL validation)
- Failure modes (5 mistake classes, 6-question patch contract)

**Validation:**
```bash
# Verify README is complete index
find .claude/rules -name '*.md' | wc -l
# All files should be listed in .claude/rules/README.md

# Verify absolute rules exist
cat .claude/rules/_core/absolute.md | grep '^| Rule'
# Should show 6+ rules

# Verify testing rules reference Chicago TDD
grep -c 'mockall\|#\[automock\]' .claude/rules/rust/testing-forbidden.md
# Should show >5 forbidden patterns
```

**Forbidden:**
- Generic policy statements without enforcement
- Rules that contradict each other
- Examples without actual command output
- Testing rules without forbidden/allowed code blocks
- Absolute rules that are not enforced by hooks
- Stale rule citations (referencing deleted files)

**Required:**
- Each rule file has version/last_updated date
- README.md is complete index with descriptions
- Every rule has concrete examples (not generic)
- Testing rules show forbidden patterns with Rust code blocks
- Andon signals have clear failure patterns and exit codes
- OTEL validation has required spans table with attributes
- Coding-agent mistakes have detection commands and forbidden examples

**Update Trigger:**
- New development policy introduced
- Absolute rule violated in production
- Agent failure mode discovered
- Tooling changes (LSP update, new clippy rule)
- More than 2 months since last review

---

### 3. API Documentation (Generated via `cargo doc`)

**What it is:** Rust doc comments on public types, traits, functions. Auto-generated HTML and searchable symbol index.

**Location:**
- Source: `crates/*/src/` (all .rs files with `///` comments)
- Generated: `target/doc/ggen_*/index.html` (run `cargo doc --no-deps --open`)

**Source of Truth:**
- Rust source code (`///` comments are authoritative)

**Validation:**
```bash
# Check for missing doc comments
cargo doc 2>&1 | grep -c 'warning: missing doc comment for'
# Should be 0 warnings for public items

# Verify doctests compile and run
cargo test --doc --all --exclude ggen-core
# All examples blocks must pass doctest compilation
```

**Forbidden:**
- Placeholder text in doc comments (e.g., `TODO: document this`)
- Example blocks that don't compile
- Generic descriptions without concrete types
- Doc comments for private items
- Broken links in doc comments

**Required:**
- All `pub struct/enum/fn/trait` have `///` doc comments
- Examples in doc comments are valid Rust code
- Examples demonstrate the most common use case
- Trait bounds and lifetimes are documented
- Links use proper syntax: `[`Type`](path::to::Type)`
- Errors returned by functions are documented

**Update Trigger:**
- Public API signature changes
- New error variant added
- Trait bound changed
- `cargo doc` produces new warnings
- Example code is invalidated by refactoring

---

### 4. Examples & Validation (Real Executable Code)

**What it is:** Working examples in doc comments and standalone files validated by CI.

**Location:**
- Doc examples: `crates/*/src/` (in `///` blocks)
- Standalone: `examples/` (executable Rust binaries)
- Guides: `docs/agent/` (workflow quick-starts)

**Source of Truth:**
- Live Rust code compiled by `cargo test --doc` and `cargo build --examples`

**Validation:**
```bash
# All examples must compile
cargo test --doc --all --exclude ggen-core

# Standalone examples must build
cargo build --examples

# Run an example and verify output is real
cargo run --example <name> 2>&1 | head -20
```

**Forbidden:**
- Examples that use mock objects or test doubles
- Pseudocode in example comments
- Examples that don't compile
- Fabricated example output
- Examples that panic without handling Errors
- References to nonexistent functions or types

**Required:**
- Every public function has ≥1 `///` Examples block
- Examples demonstrate the most common use case
- Examples use real dependencies, not mocks
- Examples handle Errors with `Result<T, E>`, not `unwrap()`
- Example output is captured from actual program run
- Standalone examples in `examples/` are built by CI

**Update Trigger:**
- Public API signature changes
- New feature added
- Example code breaks due to refactoring
- `cargo test --doc` produces new failures

---

### 5. Specification Documentation (RDF-Driven Source)

**What it is:** Machine-readable RDF/Turtle ontologies are the source of truth. Markdown files are GENERATED and must NOT be hand-edited.

**Location:**
- **SOURCE (edit these):** `.specify/specs/NNN-*/*.ttl` (feature.ttl, plan.ttl, tasks.ttl)
- **GENERATED (don't edit):** `.specify/specs/NNN-*/*.md` (auto-generated by `ggen sync`)

**Source of Truth:**
- RDF/Turtle files (`.specify/specs/*/feature.ttl`) — EDIT THESE, NOT THE .md

**Validation:**
```bash
# Feature TTL must validate against SHACL
ggen validate .specify/specs/NNN-feature/feature.ttl
# Expected: ✅ All SHACL constraints pass

# Generated markdown should reflect TTL
ggen sync --dry_run true

# Verify all required properties present
rdfquery -f turtle .specify/specs/NNN-feature/feature.ttl 'SELECT ?p ?o WHERE { ?s a spec:Feature . ?s ?p ?o }'
```

**Forbidden:**
- Hand-editing `.specify/specs/*/*.md` (these are generated, not source)
- RDF without SHACL validation
- Specs without `feature.ttl`
- Circular ontology references
- Undefined namespace URIs
- Orphan specs (no corresponding code implementation)

**Required:**
- Every spec has `feature.ttl` (RDF source, SHACL-valid)
- RDF includes: label, comment, specification, maturity, philosophy
- All namespaces declared at top of `.ttl` file
- Markdown generated from TTL via `ggen sync` (never hand-edited)
- Cross-references to other specs use valid URIs
- User stories and acceptance criteria defined in RDF
- Receipts generated for each `ggen sync` run

**Update Trigger:**
- Feature requirements change (edit `.ttl`, run `ggen sync`)
- New spec created
- Ontology schema changes
- Design decision recorded (add to RDF)
- `ggen validate` produces new errors

---

### 6. Process & Workflow Documentation

**What it is:** How ggen is developed, tested, released, and maintained. Includes gates, hooks, and CI/CD discipline.

**Location:**
- Primary: `CLAUDE.md` (root project brief)
- Workflow: `.claude/rules/_core/workflow.md` (4-step dev cycle)
- Gates: `.claude/rules/andon/signals.md` (stop-the-line protocol)
- Enforcement: `.git/hooks/` (pre-commit, pre-push scripts)

**Source of Truth:**
- Live hooks and CI enforcement

**Validation:**
```bash
# Verify CLAUDE.md reflects current architecture
grep -c 'ggen-' CLAUDE.md

# Verify 4-step workflow is documented
cat .claude/rules/_core/workflow.md | grep -E '^## [1-4]\.'

# Verify Definition of Done gates work
just check && just lint && just test && just slo-check

# Verify Andon signals are enforced
cat .git/hooks/pre-commit | grep -i 'error\|andon\|stop'
```

**Forbidden:**
- Workflow steps not enforced by hooks
- Gates that can be bypassed (no `--no-verify`)
- Stale documentation (references deleted files)
- Definition of Done without all required checks
- Andon signals without actionable failure messages
- Release gates without evidence collection

**Required:**
- CLAUDE.md is current (version, architecture section up-to-date)
- 4-step workflow documented in `.claude/rules/_core/workflow.md`
- Definition of Done checklist in `.claude/rules/README.md`
- All dev commands in `justfile` (not bare `cargo`)
- Pre-commit hook enforces checks
- Pre-push hook runs tests
- Release gate validates before shipping
- Evidence collected at each stage

**Update Trigger:**
- New policy or gate introduced
- Workflow step changes
- Tool changes
- Gate becomes unreliable
- Release process needs validation update

---

### 7. Evidence-First Principle (MANDATORY)

**What it is:** Constitutional rule that ALL documentation must be corroborated by real code or captured execution. No fabricated examples, no generic placeholders, no theoretical claims without proof.

**Applies to:** ALL 8 documentation surfaces

**Validation:**
```bash
# No generic examples
grep -rn 'example\|Example' docs/ | grep -i 'TODO\|stub\|placeholder\|generic'
# Should return 0 matches

# File paths are absolute and real
grep -rn '/path/to/\|example\.rs' docs/
# Should return 0 matches

# Code quotes match source
grep -rn 'crate.*Error' docs/ | head -3
# All quoted code must exist in actual source files

# OTEL examples are from real runs (not synthetic)
grep -rn 'llm\.complete\|llm\.model' docs/ | grep -v 'required\|example'
# All OTEL spans must be from RUST_LOG=trace execution

# Claims have proof
grep -rn 'works\|passes\|valid\|complete' docs/ | grep -v 'test\|verify'
# Claims must be followed by evidence citation
```

**Forbidden:**
- Fabricating JSON schemas, API responses, or OTEL traces
- Generic 5 Whys without real failure data
- Template-filled documentation
- Claiming features work without OTEL evidence
- Pseudocode presented as real code
- Example output not from actual execution
- File paths with placeholders (e.g., `/path/to/file`)
- Theoretical claims without empirical corroboration

**Required:**
- Every code quote must be from actual source
- Every OTEL example must be from `RUST_LOG=trace` execution
- Every file path must be absolute and real (verified to exist)
- Every claim must be corroborated by reading code or capturing output
- Every example must be executable as-is
- Every error message must be from actual error output
- Every design decision must reference the RDF spec that drove it
- Every architecture claim must be verified by LSP workspace survey

**How to Apply:**
1. **Read the code** (actual files)
2. **Run the feature** (capture real output)
3. **Verify claims** (OTEL spans, test passes)
4. **Document with evidence** (quote code, cite file paths, embed real output)
5. **Cross-check** (run commands to verify all paths and quotes exist)

**Rule:** If you didn't read it or capture it, don't write it.

---

### 8. Searchability & Navigation (Cross-Linked Index)

**What it is:** All documentation is indexed, discoverable, and cross-linked. Single entry point navigates to all surfaces.

**Location:**
- Entry point: `CLAUDE.md`
- Index: `docs/INDEX.md`
- Rules entry: `.claude/rules/README.md`
- Architecture entry: `docs/architecture/COMPRESSED_REFERENCE.md`

**Source of Truth:**
- Live file structure and links (must point to actual files)

**Validation:**
```bash
# All links in CLAUDE.md are valid
grep -o '\[.*\](.*\.md)' CLAUDE.md | sed 's/.*(//' | sed 's/)//' | while read f; do test -f "$f" || echo "BROKEN: $f"; done

# docs/INDEX.md lists all main docs
ls -la docs/ | grep '\.md$' | wc -l
# Compare to number of links in docs/INDEX.md

# No broken internal links anywhere
find docs .claude -name '*.md' -exec grep -l '\[.*\](.*\.md)' {} \; | \
  xargs -I {} bash -c 'grep -o "\[.*\](.*\.md)" {} | sed "s/.*(/$(dirname {})\//; s/).*/\1/" | \
  while read f; do test -f "$f" || echo "BROKEN in {}: $f"; done'
```

**Forbidden:**
- Broken internal links
- Circular links without escape
- Links to placeholder files
- Duplicated content without links
- Missing entry point
- Orphaned documents (not reachable from CLAUDE.md)

**Required:**
- `CLAUDE.md` is the single entry point
- All links in `CLAUDE.md` point to existing files (no broken links)
- `docs/INDEX.md` lists all major documentation files
- `.claude/rules/README.md` is a complete index of rule files
- Each doc has a clear purpose statement
- Cross-references use relative paths (e.g., `../rules/testing.md`)
- No circular dependencies in link graph
- Every `.md` file is reachable from `CLAUDE.md` within 3 clicks

**Update Trigger:**
- New documentation file created
- Documentation file renamed or moved
- Documentation file deleted
- New rule added to `.claude/rules/`
- Architecture documentation updated

---

## Master Validation Checklist

**Documentation is COMPLETE only when ALL of these pass:**

```bash
# 1. Code compiles and all tests pass
just check && just lint && just test && just slo-check ✅

# 2. Specifications validate
ggen validate .specify/specs/*/feature.ttl ✅

# 3. API docs compile, no warnings
cargo doc --no-deps 2>&1 | grep -c 'warning:' | xargs test 0 -eq ✅

# 4. All examples are real and working
cargo test --doc --all --exclude ggen-core ✅

# 5. No fabricated docs or placeholders
grep -rn 'TODO.*doc\|placeholder\|/path/to/' docs/ crates/*/src/ | wc -l | xargs test 0 -eq ✅

# 6. All internal links are valid
grep -o '\[.*\](.*\.md)' CLAUDE.md | sed 's/.*(//' | sed 's/)//' | while read f; do test -f "$f" || exit 1; done ✅

# 7. Architecture is verified by LSP
LSP workspaceSymbol <ggen-core> ✅

# 8. COMPRESSED_REFERENCE.md is current
grep 'Verified' docs/architecture/COMPRESSED_REFERENCE.md | grep -q "2026-0[5-6]" ✅
```

**If ANY check fails:**
1. Identify which documentation surface failed
2. Read the failure message carefully
3. Fix the root cause
4. Re-run the check
5. **Do NOT proceed until ALL checks pass**

---

## Implementation Example

### Scenario: Adding a New Crate to ggen

| Step | Action | Documentation Impact |
|------|--------|----------------------|
| 1 | Update `Cargo.toml` members list | CLAUDE.md crate count increases |
| 2 | Create `crates/new-crate/src/lib.rs` with `///` doc comments | `cargo doc` auto-generates API reference |
| 3 | Run LSP workspaceSymbol to discover types/traits | `docs/architecture/COMPRESSED_REFERENCE.md` lists new crate |
| 4 | Create `.specify/specs/NNN-new-feature/feature.ttl` | `ggen sync` generates markdown |
| 5 | Add integration tests in `crates/new-crate/tests/` | Real examples for doc comments, OTEL spans captured |
| 6 | Update `docs/architecture/COMPRESSED_REFERENCE.md` with new crate | COMPRESSED_REFERENCE verified date updates |
| 7 | Update `CLAUDE.md` crate table | Entry point reflects new architecture |
| 8 | Verify: `just check && cargo doc && ggen validate && all links work` | All documentation surfaces are consistent |

---

## Governance

| Item | Details |
|------|---------|
| **Owner** | Sean Chatman (sean@chatmangpt.com) |
| **Review Frequency** | Monthly (last Friday of month) |
| **Update Authority** | Architecture decisions require CLAUDE.md update |
| **Version Scheme** | SemVer (CLAUDE.md version + COMPRESSED_REFERENCE verified date) |
| **Enforcement** | Pre-commit hook verifies: no fabricated docs, no broken links, all CLAUDE.md references valid |

---

## See Also

- `CLAUDE.md` — Root project brief and architecture reference
- `docs/architecture/COMPRESSED_REFERENCE.md` — Verified C4, real sync flow, error map
- `.claude/rules/README.md` — Rules documentation index
- `.claude/rules/vision-2030-mandate.md` — Non-negotiable standards
- `docs/DOCUMENTATION_DEFINITION_OF_DONE.json` — Structured JSON specification

---

**Last Updated:** 2026-06-14  
**Version:** 1.0.0  
**Status:** APPROVED — All 8 surfaces defined and validated
