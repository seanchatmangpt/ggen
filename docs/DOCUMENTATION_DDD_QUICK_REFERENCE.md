<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [Documentation Definition of Done — Quick Reference](#documentation-definition-of-done--quick-reference)
  - [The 8 Surfaces Checklist](#the-8-surfaces-checklist)
  - [Before You Write/Edit Docs](#before-you-writeedit-docs)
  - [Forbidden Patterns](#forbidden-patterns)
  - [Master Validation (All Must Pass)](#master-validation-all-must-pass)
  - [When Adding a New Documentation File](#when-adding-a-new-documentation-file)
  - [Common Mistakes (Avoid These)](#common-mistakes-avoid-these)
  - [One-Minute Health Check](#one-minute-health-check)
  - [Reference Files](#reference-files)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Documentation Definition of Done — Quick Reference

**One-pager for documentation validation.** Reference when adding/updating docs.

---

## The 8 Surfaces Checklist

| # | Surface | Source | Validation Command | Red Flag |
|---|---------|--------|-------------------|----------|
| 1 | Architecture | `Cargo.toml` + LSP | `LSP workspaceSymbol <ggen-core>` | Crate list doesn't match Cargo.toml |
| 2 | Rules & Policy | Live hooks + code | `find .claude/rules -name '*.md' \| wc -l` | Absolute rules not enforced |
| 3 | API Docs | `///` comments in .rs | `cargo doc && cargo test --doc` | Missing doc comments (warnings) |
| 4 | Examples | Real executable code | `cargo test --doc && cargo build --examples` | Example doesn't compile |
| 5 | Specifications | `.specify/specs/*.ttl` | `ggen validate .specify/specs/*/feature.ttl` | SHACL validation fails |
| 6 | Process | `.git/hooks/` + justfile | `just check && just test` | Gate can be bypassed |
| 7 | Evidence-First | ALL surfaces | `grep -r 'TODO\|placeholder' docs/` | Fabricated examples or paths |
| 8 | Navigation | Links in CLAUDE.md | `grep '\[.*\]' CLAUDE.md \| while read f...` | Broken links |

---

## Before You Write/Edit Docs

1. **Read the code** — Actual file paths, real function signatures
2. **Run the code** — Capture real OTEL spans, real test output
3. **Quote exactly** — Never paraphrase code or errors
4. **Link to files** — Use absolute paths: `/home/user/ggen/crates/ggen-core/src/lib.rs`
5. **Verify all links** — Run link checker before committing
6. **Date everything** — When was this verified? Last reviewed?

---

## Forbidden Patterns

| Pattern | Instead Do |
|---------|-----------|
| `Example output: { "key": "value" }` (made up) | Run the actual program and paste real output |
| `/path/to/file` (placeholder) | Use actual absolute path: `/home/user/ggen/crates/...` |
| `// TODO: document this` | Delete or implement fully |
| `// Example: some_function()` (pseudocode) | Paste real, compilable Rust code block |
| Linking to deleted files | Update all links or delete the reference |
| Generic C4 diagram from template | Draw actual dependencies verified by LSP |
| OTEL span fabrication | Run `RUST_LOG=trace` and capture real output |

---

## Master Validation (All Must Pass)

```bash
# Run this before committing documentation changes
just check && \
just lint && \
just test && \
just slo-check && \
cargo doc 2>&1 | grep 'warning:' | wc -l | xargs test 0 -eq && \
cargo test --doc && \
ggen validate .specify/specs/*/feature.ttl && \
grep -rn 'TODO.*doc\|placeholder\|/path/to/' docs/ | wc -l | xargs test 0 -eq && \
grep -o '\[.*\](.*\.md)' CLAUDE.md | sed 's/.*(//' | sed 's/)//' | while read f; do test -f "$f" || exit 1; done

echo "✅ All documentation gates pass"
```

---

## When Adding a New Documentation File

1. Create file in appropriate location (`docs/`, `.claude/rules/`, `crates/*/src/`, etc.)
2. Add entry to parent index (CLAUDE.md, docs/INDEX.md, .claude/rules/README.md)
3. Update breadcrumb/navigation links
4. Verify all links point to actual files
5. Run master validation above
6. Commit with reference to Definition of Done version

---

## Common Mistakes (Avoid These)

| Mistake | Why It's Wrong | Fix |
|---------|---------------|-----|
| Updating `.md` spec files instead of `.ttl` | Markdown is generated; you're editing the wrong source | Edit `.specify/specs/NNN-*/*.ttl` and run `ggen sync` |
| Linking to `/path/to/file` | Not a real path; no one can find it | Use actual: `/home/user/ggen/crates/ggen-core/src/lib.rs` |
| Copying doc comment from template | Templates are generic examples | Read actual function in source and describe what it really does |
| Claiming feature works without OTEL | Not proven | Run `RUST_LOG=trace cargo test <feature>` and capture spans |
| Broken doc links | Users can't navigate | Run link checker: `grep -o '\[.*\](.*\.md)' file.md \| while read f...` |
| Stale crate list in CLAUDE.md | Architecture info is wrong | Run `LSP workspaceSymbol` to verify all 15 crates |
| Fabricated JSON example | Not reproducible | Run actual code: `cargo run --example <name>` and paste real output |

---

## One-Minute Health Check

```bash
# Is documentation healthy? Run these 4 commands:

# 1. Do all paths exist?
grep -o '/home/user/ggen/[^ )]*' docs/*.md | cut -d: -f2 | sort -u | while read p; do test -e "$p" || echo "BROKEN: $p"; done

# 2. Do all links work?
grep -o '\[.*\](.*\.md)' docs/*.md | sed 's/.*(//' | sed 's/)//' | while read f; do test -f "$f" || echo "BROKEN: $f"; done

# 3. Are tests passing?
just test 2>&1 | tail -1

# 4. Is cargo doc clean?
cargo doc --no-deps 2>&1 | grep 'warning:' | wc -l
```

All should return 0 errors.

---

## Reference Files

| For | Read | Update |
|-----|------|--------|
| Architecture decisions | `docs/architecture/COMPRESSED_REFERENCE.md` | When Cargo.toml changes |
| Development policy | `.claude/rules/README.md` | When new rule added |
| API reference | Run `cargo doc --no-deps --open` | When public API changes |
| Examples | `crates/*/src/lib.rs` (look for `///` blocks) | When code changes |
| Specifications | `.specify/specs/NNN-*/feature.ttl` | When feature changes |
| Workflow | `CLAUDE.md` + `.claude/rules/_core/workflow.md` | When process changes |

---

**Last Updated:** 2026-06-14 | **Version:** 1.0.0
