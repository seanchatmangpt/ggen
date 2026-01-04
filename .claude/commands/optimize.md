---
description: "Optimize Rust code for performance. Analyzes hot paths, suggests improvements, and verifies SLOs. Use when improving performance, reducing allocations, or optimizing algorithms."
allowed_tools: "Read, Grep, Glob, Bash(cargo make bench:*), Bash(cargo make slo-check:*)"
argument_hint: "[crate-name] [focus-area]"
model: "claude-haiku-4-5-20251001"
---

# Code Optimization Command

Analyze and optimize the specified crate or entire workspace for performance.

## Task Breakdown

1. **Identify Hot Paths**
   - Read relevant source files in $1 (or entire workspace if empty)
   - Use grep to find performance-critical sections
   - Look for allocations, clones, and unnecessary work

2. **Benchmark Current State**
   - Run relevant benchmarks: `cargo make bench`
   - Capture baseline metrics

3. **Suggest Optimizations**
   - Zero-cost abstractions (prefer generics over trait objects)
   - Reference usage patterns
   - Iterator chains vs explicit loops
   - Stack vs heap allocations

4. **Verify SLOs**
   - Check target metrics: `cargo make slo-check`
   - Ensure improvements don't break invariants

5. **Implement & Measure**
   - Apply changes following CLAUDE.md conventions
   - Re-run benchmarks to verify improvements
   - Run `cargo make pre-commit` to validate

## Focus Areas (if specified in $2)

- **memory**: Reduce allocations, stack usage
- **async**: Optimize tokio spawn/await patterns
- **collections**: Better data structures (Vec, HashMap, BTreeMap)
- **compilation**: Reduce codegen_units, enable LTO

## Key Principles

- Type-first: Express constraints in types, not runtime checks
- Measure first: Don't optimize without baseline numbers
- Safety first: Never compromise memory safety for speed
- Determinism: Ensure reproducible outputs

## Success Criteria

✓ Benchmarks show measurable improvement
✓ SLOs are met or exceeded
✓ Code still passes all tests
✓ Clippy lints are clean
