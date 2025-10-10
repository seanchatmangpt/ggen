<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [Pattern 002: DETERMINISTIC ENGINE ***](#pattern-002-deterministic-engine-)
  - [Context](#context)
  - [Problem](#problem)
  - [Forces](#forces)
  - [Solution](#solution)
  - [Template Example](#template-example)
  - [CLI Invocation](#cli-invocation)
  - [Expected Output](#expected-output)
  - [Verification](#verification)
  - [Anti-Patterns](#anti-patterns)
  - [Deterministic Patterns Checklist](#deterministic-patterns-checklist)
  - [Related Patterns](#related-patterns)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Pattern 002: DETERMINISTIC ENGINE ***

## Context

Building on **Pattern 001: KNOWLEDGE-FIRST PROJECTION**, we have semantic graphs as our source of truth. However, code generators often produce different outputs from the same inputs due to timestamp injection, random IDs, or environmental dependencies. For AI-assisted development and reproducible builds, we need **absolute determinism**: identical inputs must produce byte-identical outputs.

## Problem

**How do you guarantee that the same semantic graph and template will always produce exactly the same generated code, regardless of when or where generation occurs?**

Non-deterministic generation causes:
- Failed reproducible builds (different outputs break content-addressable caching)
- Difficult debugging (can't reproduce exact conditions)
- Git diff noise (spurious changes on regeneration)
- Broken snapshot testing (timestamps, UUIDs change each run)
- AI confusion (LLMs can't predict non-deterministic outputs)

## Forces

- Templates often need timestamps, IDs, or version numbers
- Default Handlebars helpers (e.g., `{{now}}`) inject runtime values
- Environment variables leak into generated code
- Random number generators create non-reproducible state
- File system iteration order varies across platforms
- Developers expect generation to be idempotent

## Solution

**Therefore, eliminate all sources of non-determinism from the generation pipeline.**

Enforce these invariants:

1. **Fixed Seed**: All pseudo-random operations use a deterministic seed
2. **No System Time**: Remove `{{now}}`, `{{timestamp}}`, and date helpers
3. **Ordered Iteration**: Sort all graph query results and template loops
4. **No Environment Leaks**: Never read `ENV vars` or system state during generation
5. **Content-Addressed IDs**: Generate IDs from content hashes, not UUIDs
6. **Hermetic Execution**: Same graph + same template = byte-identical output

```
┌──────────────────────────────────────────────────────────────┐
│                   DETERMINISTIC PIPELINE                      │
└──────────────────────────────────────────────────────────────┘

    Input Graph (TTL)         Template (.tmpl)         Output
    ════════════════          ════════════════         ══════
         │                           │                    │
         │  Canonical Sort           │  No {{now}}        │
         │  (alphabetical)           │  No {{uuid}}       │
         │                           │  No $ENV           │
         ├──────────┬────────────────┤                    │
         │          │                │                    │
         │    SPARQL Query           │                    │
         │    (ORDER BY)             │                    │
         │          │                │                    │
         │    JSON Projection        │                    │
         │    (deterministic)        │                    │
         │          │                │                    │
         │          └────────────────┤                    │
         │                           │  Render            │
         │                           ├───────────────────>│
         │                           │                    │
    ┌────┴────┐               ┌──────┴──────┐      ┌─────┴─────┐
    │ user.ttl│               │ struct.tmpl │      │ user.rs   │
    │ (hash:  │  ========>    │ (hash:      │ ───> │ (hash:    │
    │  abc123)│               │  def456)    │      │  xyz789)  │
    └─────────┘               └─────────────┘      └───────────┘
         │                                               │
         └───────────────────────────────────────────────┘
                    SAME INPUTS = SAME HASH

## Result

You achieve:

- **Reproducible Builds**: Same input graph always produces identical output
- **Content-Addressable Caching**: Hash outputs to detect changes
- **Snapshot Testing**: Generated code can be committed and diffed
- **Time-Travel Debugging**: Reproduce any historical generation exactly
- **AI-Friendly**: LLMs can predict outputs deterministically
- **Parallel Safety**: Multiple workers generate identical results

This pattern enables → **Pattern 006: LOCKFILE VERSIONING** (content hashes for dependencies)
This pattern enables → **Pattern 007: SNAPSHOT TESTING** (commit generated code)
This pattern enables → **GOLDEN_PATTERN: DELTA-DRIVEN PROJECTION** (detect actual changes)

## Graph Example

**File**: `docs/examples/service.ttl`

```turtle
@prefix : <http://example.org/> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .

# Note: Alphabetically ordered to ensure determinism
:apiVersion :value "v1.0.0" .
:serviceName :value "UserService" .

:endpoint1 a :Endpoint ;
    :path "/users" ;
    :method "GET" ;
    :order 1 .

:endpoint2 a :Endpoint ;
    :path "/users/:id" ;
    :method "GET" ;
    :order 2 .
```

## Template Example

**File**: `templates/service_deterministic.tmpl`

```handlebars
---
query: |
  PREFIX : <http://example.org/>

  SELECT ?serviceName ?version ?path ?method
  WHERE {
    ?service :serviceName/:value ?serviceName .
    ?service :apiVersion/:value ?version .
    ?endpoint a :Endpoint ;
              :path ?path ;
              :method ?method ;
              :order ?order .
  }
  ORDER BY ?order ASC
---
// Generated service (deterministic)
// Version: {{version}}
// Service: {{serviceName}}

{{#each results}}
// Endpoint {{@index}}: {{method}} {{path}}
fn handle_{{method}}_{{path}}() {
    // Implementation
}
{{/each}}

// Content hash: {{content_hash}}
```

**Key Deterministic Features**:
- ✅ `ORDER BY ?order ASC` ensures consistent query results
- ✅ `{{@index}}` uses loop position, not random IDs
- ✅ `{{content_hash}}` computed from template content, not timestamp
- ✅ No `{{now}}`, `{{uuid}}`, or `{{random}}` helpers

## CLI Invocation

```bash
# Generate once
ggen template apply templates/service_deterministic.tmpl > output1.rs

# Generate again (no changes to graph or template)
ggen template apply templates/service_deterministic.tmpl > output2.rs

# Verify byte-identical outputs
diff output1.rs output2.rs
# (Should return no differences)

# Verify with content hashing
sha256sum output1.rs output2.rs
# Both files should have IDENTICAL hashes
```

## Expected Output

**File**: `output1.rs` and `output2.rs` (identical)

```rust
// Generated service (deterministic)
// Version: v1.0.0
// Service: UserService

// Endpoint 0: GET /users
fn handle_GET_users() {
    // Implementation
}
// Endpoint 1: GET /users/:id
fn handle_GET_users_id() {
    // Implementation
}

// Content hash: a3f5b8c9d2e1f4a7b6c5d8e9f1a2b3c4
```

## Verification

```bash
# Test 1: Multiple generations produce identical output
for i in {1..10}; do
  ggen template apply templates/service_deterministic.tmpl > output_$i.rs
done

sha256sum output_*.rs | awk '{print $1}' | sort -u | wc -l
# Expected: 1 (all files have the same hash)

# Test 2: Verify SPARQL query ordering
ggen graph query "SELECT ?path WHERE { ?e :path ?path } ORDER BY ?path"
# Expected: Results in alphabetical order every time

# Test 3: Detect non-deterministic helpers
ggen template lint templates/*.tmpl --check-determinism
# Expected: No warnings about {{now}}, {{uuid}}, etc.

# Test 4: Content-addressed storage
hash=$(sha256sum output_1.rs | awk '{print $1}')
echo "Output hash: $hash"
# This hash should never change unless graph or template changes
```

## Anti-Patterns

❌ **Timestamp Injection**: Using `{{now}}` or `DateTime::now()` in templates
❌ **Random IDs**: Generating UUIDs or random numbers during rendering
❌ **Environment Leaks**: Reading `$USER`, `$HOSTNAME`, or other system vars
❌ **Unordered Queries**: SPARQL without `ORDER BY` (undefined iteration order)
❌ **Filesystem Ordering**: Relying on `readdir()` order (non-deterministic)
❌ **Concurrent Writes**: Race conditions in multi-threaded generation

## Deterministic Patterns Checklist

✅ **Graph Loading**:
- Parse TTL with canonical ordering
- Sort triples alphabetically by subject, predicate, object

✅ **SPARQL Queries**:
- Always include `ORDER BY` clause
- Use stable ordering keys (URIs, not BLANKs)

✅ **Template Rendering**:
- Remove all `{{now}}`, `{{timestamp}}` helpers
- Replace `{{uuid}}` with `{{content_hash}}`
- Ban `Math.random()` and PRNG usage

✅ **Output Generation**:
- Write to temporary file, then atomic rename
- Use UTF-8 encoding consistently
- Normalize line endings (LF only)

## Related Patterns

- **Pattern 001: KNOWLEDGE-FIRST PROJECTION** - Provides the input graph
- **Pattern 006: LOCKFILE VERSIONING** - Uses content hashes for dependency tracking
- **Pattern 007: SNAPSHOT TESTING** - Commits generated code for regression testing
- **GOLDEN_PATTERN: DELTA-DRIVEN PROJECTION** - Detects changes via deterministic diffs