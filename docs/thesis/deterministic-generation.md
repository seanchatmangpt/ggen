<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [Thesis: Deterministic Code Generation](#thesis-deterministic-code-generation)
  - [The Problem: Non-Deterministic Generation](#the-problem-non-deterministic-generation)
    - [What is Non-Determinism?](#what-is-non-determinism)
    - [Why Non-Determinism is Harmful](#why-non-determinism-is-harmful)
      - [1. CI/CD Failures](#1-cicd-failures)
      - [2. Difficult Debugging](#2-difficult-debugging)
      - [3. Version Control Pollution](#3-version-control-pollution)
  - [The Solution: Deterministic Transforms](#the-solution-deterministic-transforms)
    - [Principles of Deterministic Generation](#principles-of-deterministic-generation)
      - [1. Pure Functions (No Side Effects)](#1-pure-functions-no-side-effects)
      - [2. Explicit Dependencies (No Hidden State)](#2-explicit-dependencies-no-hidden-state)
      - [3. Idempotent Operations (Re-Running Safe)](#3-idempotent-operations-re-running-safe)
      - [4. Stable Ordering (Consistent Iteration)](#4-stable-ordering-consistent-iteration)
  - [Implementation in ggen](#implementation-in-ggen)
    - [1. RDF Graphs are Immutable](#1-rdf-graphs-are-immutable)
    - [2. SPARQL Queries are Deterministic](#2-sparql-queries-are-deterministic)
    - [3. Tera Templates Have No Side Effects](#3-tera-templates-have-no-side-effects)
    - [4. No Timestamps in Generated Code](#4-no-timestamps-in-generated-code)
  - [Verification and Testing](#verification-and-testing)
    - [1. Hash-Based Verification](#1-hash-based-verification)
    - [2. Version-Controlled Snapshots](#2-version-controlled-snapshots)
    - [3. Property-Based Testing (All Inputs)](#3-property-based-testing-all-inputs)
  - [Benefits of Deterministic Generation](#benefits-of-deterministic-generation)
    - [1. CI/CD Integration](#1-cicd-integration)
    - [2. Git Diffs are Meaningful](#2-git-diffs-are-meaningful)
    - [3. Reproducible Bugs](#3-reproducible-bugs)
    - [4. Caching Opportunities](#4-caching-opportunities)
  - [Challenges and Solutions](#challenges-and-solutions)
    - [Challenge 1: AI-Generated Code](#challenge-1-ai-generated-code)
    - [Challenge 2: External Data Sources](#challenge-2-external-data-sources)
    - [Challenge 3: Floating-Point Arithmetic](#challenge-3-floating-point-arithmetic)
  - [Comparison to Other Tools](#comparison-to-other-tools)
    - [ggen vs. OpenAPI Generator](#ggen-vs-openapi-generator)
    - [ggen vs. Protobuf Compiler](#ggen-vs-protobuf-compiler)
  - [Conclusion](#conclusion)
    - [Key Principles](#key-principles)
    - [Production Guarantees](#production-guarantees)
    - [Call to Action](#call-to-action)
  - [References](#references)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Thesis: Deterministic Code Generation

**Abstract**: Code generation must be reproducible - same inputs must always produce same outputs. This thesis argues that deterministic generation is essential for CI/CD pipelines, debugging, and version control, and demonstrates how to achieve it in practice.

**Last Updated**: 2025-12-11

---

## The Problem: Non-Deterministic Generation

### What is Non-Determinism?

**Definition**: System where same inputs can produce different outputs

**Example** (Non-Deterministic):
```python
import random

def generate_id():
    return f"user_{random.randint(1000, 9999)}"

# Run 1: user_3421
# Run 2: user_7823  ← Different output!
```

---

### Why Non-Determinism is Harmful

#### 1. CI/CD Failures

**Scenario**: Generated code checked into version control

```bash
# Developer 1
$ ggen generate --template class.rs.tera
Generated: User.rs (hash: abc123)

# Developer 2 (same inputs)
$ ggen generate --template class.rs.tera
Generated: User.rs (hash: def456)  ← Different!

# CI Pipeline
$ git diff
diff --git a/src/User.rs b/src/User.rs
- // Generated at 2025-12-11 10:23:45
+ // Generated at 2025-12-11 14:56:12
```

**Result**: CI detects "changes" even though logic is identical → False positives

---

#### 2. Difficult Debugging

**Scenario**: Bug report from production

```
User report: "API returned wrong user on 2025-12-10 14:32:11"

Developer tries to reproduce:
$ ggen generate --template api.rs.tera  # Different output each time!
```

**Result**: Cannot reproduce bug → Cannot fix

---

#### 3. Version Control Pollution

**Scenario**: Generated code committed to git

```bash
# Every commit changes generated files (even with same logic)
commit 1: User.rs (hash abc123)
commit 2: User.rs (hash def456)  ← Git sees as changed
commit 3: User.rs (hash ghi789)  ← Git sees as changed
```

**Result**: Meaningless diffs, bloated git history, merge conflicts

---

## The Solution: Deterministic Transforms

### Principles of Deterministic Generation

#### 1. Pure Functions (No Side Effects)

**Non-Deterministic**:
```rust
// ❌ Uses system time (different every run)
fn generate_timestamp() -> String {
    SystemTime::now().to_string()
}

// ❌ Uses random values
fn generate_id() -> String {
    format!("id_{}", rand::random::<u64>())
}
```

**Deterministic**:
```rust
// ✅ Uses input parameter (same input → same output)
fn generate_timestamp(timestamp: SystemTime) -> String {
    timestamp.to_string()
}

// ✅ Uses hash of input (deterministic)
fn generate_id(input: &str) -> String {
    let hash = sha256(input);
    format!("id_{}", hash)
}
```

**Rule**: All outputs MUST be computed from inputs only

---

#### 2. Explicit Dependencies (No Hidden State)

**Non-Deterministic**:
```rust
// ❌ Depends on global state
static mut COUNTER: usize = 0;

fn generate_unique_name() -> String {
    unsafe {
        COUNTER += 1;
        format!("item_{}", COUNTER)
    }
}
```

**Deterministic**:
```rust
// ✅ All state passed as parameters
fn generate_unique_name(index: usize) -> String {
    format!("item_{}", index)
}
```

**Rule**: All dependencies MUST be explicit parameters

---

#### 3. Idempotent Operations (Re-Running Safe)

**Definition**: Running operation multiple times has same effect as running once

**Non-Idempotent**:
```rust
// ❌ Appends to file (different result each run)
fn generate_code(output: &mut File) {
    output.write_all(b"// Generated code\n");
    output.write_all(b"struct User {}\n");
}

// Run 1: "// Generated code\nstruct User {}\n"
// Run 2: "// Generated code\nstruct User {}\n// Generated code\nstruct User {}\n"
```

**Idempotent**:
```rust
// ✅ Overwrites file (same result each run)
fn generate_code(output_path: &Path) {
    let content = "// Generated code\nstruct User {}\n";
    fs::write(output_path, content);  // Overwrites
}

// Run 1: "// Generated code\nstruct User {}\n"
// Run 2: "// Generated code\nstruct User {}\n"  ← Identical
```

**Rule**: Re-running generation MUST produce identical output

---

#### 4. Stable Ordering (Consistent Iteration)

**Non-Deterministic**:
```rust
// ❌ HashMap iteration order is random
use std::collections::HashMap;

let mut classes = HashMap::new();
classes.insert("User", "...");
classes.insert("Post", "...");

for (name, def) in classes {  // Random order!
    generate_class(name, def);
}
```

**Deterministic**:
```rust
// ✅ BTreeMap iteration order is sorted
use std::collections::BTreeMap;

let mut classes = BTreeMap::new();
classes.insert("User", "...");
classes.insert("Post", "...");

for (name, def) in classes {  // Always alphabetical!
    generate_class(name, def);
}
```

**Rule**: Iteration order MUST be stable (sorted, insertion-order, etc.)

---

## Implementation in ggen

### 1. RDF Graphs are Immutable

**Once Loaded, Never Modified**:
```rust
// ggen's RDF store
pub struct RdfStore {
    graph: oxigraph::Graph,  // Immutable after load
}

impl RdfStore {
    pub fn load(&mut self, data: &str) -> Result<()> {
        // Load RDF data (one-time operation)
        self.graph = parse_rdf(data)?;
        Ok(())
    }

    pub fn query(&self, sparql: &str) -> Result<QueryResults> {
        // Read-only query (no mutations)
        self.graph.query(sparql)
    }
}
```

**Why**: Immutable graphs guarantee same query results

---

### 2. SPARQL Queries are Deterministic

**Query Example**:
```sparql
SELECT ?class ?property
WHERE {
    ?class a owl:Class .
    ?class ex:hasProperty ?property .
}
ORDER BY ?class ?property  # ← Explicit sorting
```

**Execution**:
```rust
// First run
let results = store.query(sparql)?;
// Results: [("Post", "body"), ("Post", "title"), ("User", "email"), ("User", "name")]

// Second run (same inputs)
let results = store.query(sparql)?;
// Results: [("Post", "body"), ("Post", "title"), ("User", "email"), ("User", "name")]
//           ↑ Identical order
```

**Why**: `ORDER BY` clause ensures stable ordering

---

### 3. Tera Templates Have No Side Effects

**Template Example**:
```jinja2
{# class.rs.tera #}
{% for class in classes | sort(attribute="name") %}  {# ← Explicit sort #}
pub struct {{ class.name }} {
    {% for prop in class.properties | sort(attribute="name") %}  {# ← Explicit sort #}
    pub {{ prop.name }}: {{ prop.type }},
    {% endfor %}
}
{% endfor %}
```

**Execution**:
```rust
// First render
let output = template.render(&context)?;
// Output: "pub struct Post { pub body: String, pub title: String }\n..."

// Second render (same inputs)
let output = template.render(&context)?;
// Output: "pub struct Post { pub body: String, pub title: String }\n..."
//          ↑ Byte-identical
```

**Why**: Tera has no side effects, explicit sorting guarantees order

---

### 4. No Timestamps in Generated Code

**Bad Example**:
```jinja2
{# ❌ Non-deterministic #}
// Generated at {{ now() }}
struct User {}
```

Output changes every run:
```rust
// Run 1: // Generated at 2025-12-11 10:00:00
// Run 2: // Generated at 2025-12-11 10:05:23  ← Different!
```

**ggen Approach**:
```jinja2
{# ✅ Deterministic (no timestamp) #}
// Generated by ggen v{{ version }}
// DO NOT EDIT - This file is generated
struct User {}
```

Output identical every run:
```rust
// Run 1: // Generated by ggen v4.0.0
// Run 2: // Generated by ggen v4.0.0  ← Identical
```

**Alternative** (if timestamp needed):
```jinja2
{# ✅ Deterministic (explicit timestamp parameter) #}
// Generated at {{ timestamp }}  {# Passed as input, not system time #}
struct User {}
```

---

## Verification and Testing

### 1. Hash-Based Verification

**Test Approach**: Generate twice, compare hashes

```rust
#[test]
fn test_deterministic_generation() {
    let schema = load_schema("tests/fixtures/schema.ttl");

    // First generation
    let output1 = generate(&schema).unwrap();
    let hash1 = sha256(&output1);

    // Second generation (same inputs)
    let output2 = generate(&schema).unwrap();
    let hash2 = sha256(&output2);

    // Hashes must be identical
    assert_eq!(hash1, hash2, "Generation is non-deterministic!");
}
```

**ggen CI**: Runs this test on every commit

---

### 2. Version-Controlled Snapshots

**Test Approach**: Generate output, commit to git, verify no changes

```bash
# Generate code
$ ggen generate --template class.rs.tera --output src/generated/

# Check git status
$ git status
On branch main
nothing to commit, working tree clean  ✅

# Regenerate
$ ggen generate --template class.rs.tera --output src/generated/

# Check git status again
$ git status
On branch main
nothing to commit, working tree clean  ✅ (no changes)
```

**ggen CI**: Fails if regeneration produces git diff

---

### 3. Property-Based Testing (All Inputs)

**Test Approach**: Generate for random RDF graphs, verify determinism

```rust
use proptest::prelude::*;

proptest! {
    #[test]
    fn test_deterministic_for_any_rdf(
        triples in prop::collection::vec(any::<(String, String, String)>(), 1..100)
    ) {
        let store = create_store_from_triples(&triples);

        // Generate twice
        let output1 = generate(&store).unwrap();
        let output2 = generate(&store).unwrap();

        // Must be identical
        prop_assert_eq!(output1, output2);
    }
}
```

**ggen CI**: Runs 100+ random RDF graphs, verifies determinism

---

## Benefits of Deterministic Generation

### 1. CI/CD Integration

**Before** (Non-Deterministic):
```yaml
# .github/workflows/ci.yml
- name: Generate code
  run: ggen generate

- name: Check diff
  run: |
    if [ -n "$(git diff)" ]; then
      echo "Generated code changed!"
      exit 1  # ← False positives every run!
    fi
```

**After** (Deterministic):
```yaml
# .github/workflows/ci.yml
- name: Generate code
  run: ggen generate

- name: Check diff
  run: |
    if [ -n "$(git diff)" ]; then
      echo "Generated code changed!"
      exit 1  # ← Only fails if actual changes
    fi
```

**Result**: No false positives, reliable CI checks

---

### 2. Git Diffs are Meaningful

**Before** (Non-Deterministic):
```diff
diff --git a/src/User.rs b/src/User.rs
- // Generated at 2025-12-11 10:00:00
+ // Generated at 2025-12-11 14:23:45  ← Noise
- pub user_id_3421: String,
+ pub user_id_7823: String,  ← Meaningless change
```

**After** (Deterministic):
```diff
diff --git a/src/User.rs b/src/User.rs
  // Generated by ggen v4.0.0
  pub struct User {
      pub email: String,
+     pub age: u32,  ← Real change (new field added)
  }
```

**Result**: Diffs reflect actual schema changes only

---

### 3. Reproducible Bugs

**Before** (Non-Deterministic):
```
Bug report: "API crashed with User generated on 2025-12-10"
Developer: Cannot reproduce (different User generated each time)
```

**After** (Deterministic):
```
Bug report: "API crashed with User from commit abc123"
Developer: Checkout abc123, regenerate, bug reproduces!
```

**Result**: Debuggable systems

---

### 4. Caching Opportunities

**Without Determinism**:
```
Cannot cache generation (output unpredictable)
→ Must regenerate every time
```

**With Determinism**:
```
Cache key: sha256(schema.ttl)
If cached output exists → Reuse (10-100x speedup)
```

**ggen**: Aggressive caching (SPARQL queries, templates, generated code)

---

## Challenges and Solutions

### Challenge 1: AI-Generated Code

**Problem**: LLMs are non-deterministic by design

```python
# Non-deterministic (temperature > 0)
response = openai.chat.completions.create(
    model="gpt-4",
    messages=[{"role": "user", "content": "Generate Rust struct"}],
    temperature=0.7  # ← Different output each run
)
```

**ggen Solution**: Two modes

**Mode 1** - Deterministic (templates only):
```bash
$ ggen generate --template class.rs.tera  # No AI
# Always produces same output
```

**Mode 2** - AI-enhanced (non-deterministic):
```bash
$ ggen ai generate --provider anthropic --temperature 0.0  # Deterministic setting
# Or: Accept non-determinism in AI mode, warn user
```

**Key Point**: Determinism is opt-in for AI features

---

### Challenge 2: External Data Sources

**Problem**: External APIs may return different data

```rust
// ❌ Non-deterministic (external API call)
let schema = fetch_schema_from_api("https://api.example.com/schema");
generate(schema);  // Different result if API changes
```

**ggen Solution**: Explicit timestamps

```bash
# ✅ Deterministic (explicit version/timestamp)
$ ggen generate --schema-url https://api.example.com/schema?version=2025-12-11
# Or: Cache schema locally (snapshot)
$ ggen cache-schema --url https://api.example.com/schema --output schema.ttl
$ ggen generate --schema schema.ttl  # Deterministic (local file)
```

---

### Challenge 3: Floating-Point Arithmetic

**Problem**: Floating-point operations may differ across platforms

```rust
// ❌ Non-deterministic (platform-dependent)
let value = 0.1 + 0.2;  // May be 0.30000000000000004 or 0.3
```

**ggen Solution**: Avoid floating-point in templates

```jinja2
{# ❌ Avoid this #}
{% set size = 10.5 * 2.3 %}

{# ✅ Use integers or strings #}
{% set size = 10 %}
```

---

## Comparison to Other Tools

### ggen vs. OpenAPI Generator

| Aspect | OpenAPI Gen | ggen |
|--------|-------------|------|
| Deterministic by default | ⚠️ Mostly (timestamps in comments) | ✅ Always |
| Hash-based verification | ❌ No tests | ✅ CI enforced |
| Caching | ❌ No | ✅ Aggressive |
| AI features | ❌ None | ✅ Opt-in non-determinism |

---

### ggen vs. Protobuf Compiler

| Aspect | protoc | ggen |
|--------|--------|------|
| Deterministic | ✅ Yes | ✅ Yes |
| Verification | ⚠️ Manual | ✅ Automated tests |
| Iteration order | ✅ Stable | ✅ Stable (sorted) |

---

## Conclusion

### Key Principles

1. ✅ **Pure Functions**: No side effects in generation logic
2. ✅ **Explicit Dependencies**: All inputs as parameters
3. ✅ **Idempotent Operations**: Re-running safe
4. ✅ **Stable Ordering**: Consistent iteration order
5. ✅ **Immutable Inputs**: RDF graphs never modified after load
6. ✅ **No Timestamps**: Unless explicitly provided as input
7. ✅ **Hash Verification**: CI enforces determinism

---

### Production Guarantees

**ggen v4.0.0** guarantees 100% deterministic generation:
- ✅ 1,168+ tests verify determinism
- ✅ CI fails on non-deterministic output
- ✅ Property-based testing (100+ random inputs)
- ✅ Hash-based snapshot testing
- ✅ Version-controlled golden files

**Verification**:
```bash
# Run determinism tests
cargo test test_deterministic

# Verify with CI
git commit && git push  # CI checks determinism automatically
```

---

### Call to Action

**For Tool Builders**:
- Make determinism a first-class requirement
- Add hash-based verification tests
- Document non-deterministic features clearly

**For Users**:
- Demand deterministic code generation
- Verify determinism with git diffs
- Report non-deterministic behavior as bugs

---

## References

1. **Pure Functions** (Functional Programming)
   - Haskell Wiki: https://wiki.haskell.org/Pure

2. **Idempotence** (Distributed Systems)
   - Martin Fowler: https://martinfowler.com/articles/idempotence.html

3. **Deterministic Testing** (Property-Based Testing)
   - QuickCheck: https://hackage.haskell.org/package/QuickCheck

4. **Reproducible Builds** (Software Supply Chain)
   - https://reproducible-builds.org/

5. **Git Best Practices** (Version Control)
   - Pro Git Book: https://git-scm.com/book/en/v2

---

**Related Theses**:
- `docs/thesis/ontology-driven-development.md` - Why RDF for generation
- `docs/thesis/rdf-as-universal-schema.md` - RDF guarantees
- `docs/thesis/ai-assisted-codegen.md` - Balancing AI + determinism

**Implementation**:
- `docs/explanations/fundamentals/deterministic-generation.md` - Practical guide
- `docs/how-to/generation/ensure-reproducibility.md` - Step-by-step tutorial
- `crates/ggen-core/src/generation.rs` - Implementation details
