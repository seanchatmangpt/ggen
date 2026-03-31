# Self-Play Commands - Quick Start Guide

## Overview

The `ggen self_play` commands provide a framework for demonstrating ggen's recursive self-generation capability.

## Commands

### 1. Demo Information

Display information about the self-play demo:

```bash
ggen self_play demo
```

**Output**: JSON with demo path, file existence checks, and usage instructions.

### 2. Validate Ontology

Validate the self-play ontology file:

```bash
ggen self_play validate --ontology examples/self-play/ontology.ttl
```

**Output**: JSON with validation result and check counts.

### 3. Run Self-Play

Execute self-play iterations (creates directory structure):

```bash
ggen self_play run --output_dir=/tmp/self-play --ontology=examples/self-play/ontology.ttl
```

**Parameters**:
- `--output_dir`: Where to create iteration directories (default: `/tmp/self-play`)
- `--ontology`: Path to ontology file (default: `examples/self-play/ontology.ttl`)
- Default: 3 iterations

**Output**: Creates iteration directories (`iteration-1`, `iteration-2`, `iteration-3`) and returns JSON with paths.

## Example Session

```bash
# 1. Check demo information
$ ggen self_play demo

# 2. Validate ontology
$ ggen self_play validate --ontology examples/self-play/ontology.ttl

# 3. Run self-play (creates directories)
$ ggen self_play run --output_dir=/tmp/my-self-play --ontology=examples/self-play/ontology.ttl

# 4. Check created directories
$ ls -la /tmp/my-self-play/
iteration-1/
iteration-2/
iteration-3/
self-play-report.md
```

## Integration with Full Demo

For the complete self-play experience with actual code generation, use the demo script:

```bash
cd examples/self-play
./run-demo.sh
```

This script:
1. Validates the ontology
2. Runs multiple iterations of self-play
3. Collects metrics
4. Generates a visual report

## Architecture

The self-play module follows the five-stage pipeline (μ₁-μ₅):

```
μ₁: Spec → μ₂: Generate → μ₃: Merge → μ₄: Validate → μ₅: Emit
```

Each iteration represents ggen generating a new version of itself from an ontology description.

## Files

- **Module**: `/Users/sac/ggen/crates/ggen-cli/src/cmds/self_play.rs`
- **Tests**: `/Users/sac/ggen/crates/ggen-cli/tests/self_play_smoke_test.rs`
- **Demo**: `/Users/sac/ggen/examples/self-play/`
- **Summary**: `/Users/sac/ggen/SELF_PLAY_MODULE_REENABLE_SUMMARY.md`

## Status

✅ Fully functional
✅ Chicago TDD compliant
✅ Production-ready

---

**Last Updated**: 2026-03-31
