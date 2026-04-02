<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [11. TIMEOUT PROTECTION **](#11-timeout-protection-)
  - [Context](#context)
  - [Connections](#connections)
  - [Implementation](#implementation)
    - [Configuration Layers](#configuration-layers)
    - [Timeout Scopes](#timeout-scopes)
    - [Execution with Timeout](#execution-with-timeout)
    - [Error Output](#error-output)
    - [Exit Code](#exit-code)
  - [Choosing Timeout Values](#choosing-timeout-values)
    - [For Development](#for-development)
    - [For CI/CD](#for-cicd)
    - [For Production](#for-production)
  - [Diagnosing Timeout Issues](#diagnosing-timeout-issues)
  - [SLO Alignment](#slo-alignment)
  - [The Deeper Pattern](#the-deeper-pattern)
  - [When This Pattern Breaks](#when-this-pattern-breaks)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# 11. TIMEOUT PROTECTION **

*No task should run forever. Every process deserves an ending.*

---

## Context

The generation pipeline executes SPARQL queries against RDF graphs. Some queries are simple—they complete in milliseconds. Others are complex—they may take seconds. And some are accidentally infinite—they never complete.

A malformed query, an unexpectedly large graph, or a pathological data pattern can cause the pipeline to hang. The user waits. The CI/CD job waits. Resources are consumed. Nothing is produced.

---

❖ ❖ ❖

**Unbounded operations are a reliability risk. Time limits transform risks into failures—and failures can be handled.**

The forces:
- Queries must be allowed to complete (they do useful work)
- Queries must not run forever (infinite loops exist)
- The timeout should be configurable (different contexts need different limits)
- Timeout violations should be clear (not silent hangs)

Without timeouts:
- Bad queries hang the pipeline indefinitely
- CI/CD jobs exhaust their limits
- Users kill the process manually, uncertain of the cause
- The system feels unreliable

**Therefore:**

**Impose time limits on all pipeline operations. Provide configurable timeouts at both the manifest level and the command line. When a timeout is exceeded, fail with a clear error indicating which operation exceeded its limit.**

Timeout protection should:
- Apply to inference rules (CONSTRUCT execution)
- Apply to generation rules (SELECT execution)
- Apply to the overall pipeline (total execution time)
- Be configurable per-context
- Produce clear error messages on violation

---

❖ ❖ ❖

## Connections

This pattern protects **[INFERENCE ENRICHMENT](05-inference-enrichment.md)** and **[GENERATION RULES](06-generation-rules.md)**.

- **[ERROR SIGNALS](12-error-signals.md)** reports timeout failures (exit code 6)
- **[MANIFEST AS TRUTH](02-manifest-as-truth.md)** configures default timeouts
- **[THE SINGLE COMMAND](01-single-command.md)** accepts a timeout flag

---

## Implementation

### Configuration Layers

**Manifest level** (default timeouts):

```toml
[inference]
max_reasoning_timeout_ms = 5000  # 5 seconds for all inference

[generation]
max_sparql_timeout_ms = 5000     # 5 seconds per SPARQL query
```

**Command line level** (override):

```bash
ggen sync --timeout 30000  # 30 seconds total
```

### Timeout Scopes

| Scope | Configuration | Default |
|-------|---------------|---------|
| Inference (total) | `inference.max_reasoning_timeout_ms` | 5000ms |
| SPARQL query | `generation.max_sparql_timeout_ms` | 5000ms |
| Pipeline (total) | `--timeout` flag | 30000ms |

### Execution with Timeout

```rust
use tokio::time::{timeout, Duration};

pub async fn execute_with_timeout<T, F>(
    operation: F,
    timeout_ms: u64,
    operation_name: &str,
) -> Result<T>
where
    F: std::future::Future<Output = Result<T>>,
{
    let duration = Duration::from_millis(timeout_ms);

    match timeout(duration, operation).await {
        Ok(result) => result,
        Err(_) => Err(Error::new(&format!(
            "Operation '{}' exceeded timeout of {}ms",
            operation_name, timeout_ms
        ))),
    }
}
```

### Error Output

When a timeout occurs:

```
$ ggen sync

error[E0006]: Timeout exceeded

  Inference rule 'complex-derivation' exceeded timeout of 5000ms

  This may indicate:
    - A very complex query that needs optimization
    - A pathological data pattern causing exponential behavior
    - An incorrectly written query with infinite loops

  Solutions:
    - Increase timeout: ggen sync --timeout 60000
    - Simplify the query
    - Check for Cartesian products in the SPARQL
    - Profile the query with --verbose

Exit code: 6
```

### Exit Code

Timeout failures produce exit code 6:

| Exit Code | Meaning |
|-----------|---------|
| 0 | Success |
| 1 | Manifest validation error |
| 2 | Ontology load error |
| 3 | SPARQL query error |
| 4 | Template rendering error |
| 5 | File I/O error |
| **6** | **Timeout exceeded** |

---

## Choosing Timeout Values

### For Development

Generous timeouts during development:

```bash
ggen sync --timeout 60000  # 1 minute
```

### For CI/CD

Strict timeouts in CI/CD:

```toml
[inference]
max_reasoning_timeout_ms = 5000

[generation]
max_sparql_timeout_ms = 2000
```

Plus a job-level timeout in the workflow:

```yaml
- name: Generate code
  run: ggen sync --timeout 30000
  timeout-minutes: 1
```

### For Production

Balanced timeouts with monitoring:

```toml
[inference]
max_reasoning_timeout_ms = 10000

[generation]
max_sparql_timeout_ms = 5000
```

---

## Diagnosing Timeout Issues

When timeouts occur, investigate:

**1. Profile the query:**

```bash
ggen sync --verbose
# Shows timing for each operation
```

**2. Check query complexity:**

```sparql
# Potential Cartesian product:
SELECT ?a ?b ?c
WHERE {
  ?a a :Entity .
  ?b a :Entity .
  ?c a :Entity .
}
# This is O(n³) where n = number of entities!
```

**3. Check data size:**

```bash
# Count triples in ontology
grep -c "^\s*\S" ontology/*.ttl
```

**4. Simplify incrementally:**

```bash
# Run only one rule to isolate the problem
ggen sync --rule structs --verbose
```

---

## SLO Alignment

Timeout protection supports Service Level Objectives:

| SLO | Timeout Implication |
|-----|---------------------|
| First build ≤ 15s | Pipeline timeout ≤ 15000ms |
| Incremental ≤ 2s | SPARQL timeout ≤ 1000ms |
| RDF processing ≤ 5s for 1k+ triples | Inference timeout ≤ 5000ms |

Configure timeouts to match SLOs:

```toml
[inference]
max_reasoning_timeout_ms = 5000  # Matches SLO

[generation]
max_sparql_timeout_ms = 1000     # Supports incremental SLO
```

---

## The Deeper Pattern

TIMEOUT PROTECTION is about **bounded execution**.

Every operation should have a known upper bound on its execution time. This bound may be generous, but it must exist. Without it:

- Failures cannot be distinguished from slow operations
- Resources cannot be managed predictably
- Users cannot know whether to wait or intervene

The timeout transforms **unknown-duration** operations into **bounded** operations. When the bound is exceeded, the operation fails. Failure is a known state; it can be handled.

---

## When This Pattern Breaks

TIMEOUT PROTECTION struggles when:

- Legitimate operations exceed timeouts (need higher limits)
- Timeouts are too high (hangs still happen, just bounded)
- Timeout granularity is wrong (rule-level vs operation-level)

ggen manages this through:

- Configurable timeouts at multiple levels
- Clear error messages indicating the problem
- Verbose mode for profiling slow operations

For very slow legitimate operations, consider:
- Optimizing queries
- Pre-computing expensive derivations
- Running generation in a dedicated job with higher limits

The pattern remains: every operation completes or times out. There is no third state.
