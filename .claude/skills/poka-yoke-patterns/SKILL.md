---
name: poka-yoke-patterns
description: "Master Poka-Yoke error-proofing from Toyota Production System. Design systems that prevent mistakes through structure, not vigilance."
allowed_tools: "Read, Write, Edit, Bash(cargo make:*)"
---

# Poka-Yoke Patterns

## What is Poka-Yoke?

**Poka-Yoke** (ポカ・ヨケ): Japanese for "mistake-proofing"

Design systems so mistakes become impossible or immediately obvious.

## Three Types

### 1. Prevent (Make mistakes impossible)
- Type system constraints (NewType patterns)
- Compiler verification (Result<T,E>)
- Zero unwrap/expect in production code

### 2. Detect (Catch mistakes immediately)
- Andon signals (RED/YELLOW/GREEN)
- Quality gates (pre-commit hooks)
- SLO timeouts (prevent hanging)

### 3. Recover (Minimize damage)
- Graceful degradation
- Automatic rollback
- Clear error messages

## In ggen

- **Cargo Make Only**: Prevents bypassing quality gates
- **Result<T,E>**: Compiler enforces error handling
- **Clippy `-D warnings`**: Zero tolerance for warnings
- **SHACL Validation**: Prevents invalid specifications

## Reference
See CLAUDE.md sections:
- Constitutional Rules (Poka-Yoke)
- Holographic Factory Metaphor (Quality Gates)
