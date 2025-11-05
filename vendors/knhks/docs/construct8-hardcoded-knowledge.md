# Hardcoded Knowledge Construction: 80/20 Enterprise CONSTRUCT Use Cases

## Overview
Analysis of what knowledge can be **hardcoded/precomputed** at Rust warm/cold path time vs what must be computed at C hot path runtime. Focus on 80/20 enterprise patterns that provide maximum value.

## Key Insight: Hardcoded Knowledge = Zero Hot Path Cost

**Principle**: If Rust can precompute it at warm/cold path time, it costs **0 ticks** in the hot path.

## 1. Hardcoded Knowledge Patterns (100% Precomputable)

### 1.1. Fixed Template Constants
**Current Runtime Cost:** Broadcast `p_const` and `o_const` (~1 tick)

**Hardcoded Knowledge:**
- **Authorization**: `(?u, ex:hasAccess, ?r)` template
  - `p_const = ex:hasAccess` (fixed predicate)
  - `o_const = ex:Allowed` or `ex:Denied` (constant based on policy)
  - **Rust can precompute**: Broadcast vectors, store as constants

- **Compliance Classification**: `(?s, rdf:type, :Compliant)` template
  - `p_const = rdf:type` (fixed predicate)
  - `o_const = :Compliant` (constant classification)
  - **Rust can precompute**: All constants

- **Risk Flags**: `(?asset, ex:riskLevel, ?level)` template
  - `p_const = ex:riskLevel` (fixed predicate)
  - `o_const = ex:High|Medium|Low` (precomputed from policy)
  - **Rust can precompute**: Classification constants

**Savings:** ~1 tick (eliminates runtime broadcasts)

### 1.2. Length-Based Specialization
**Current Runtime Cost:** `len_mask_bits = ((1ULL << len) - 1) & 0xFFULL` (~0.25 ticks)

**Hardcoded Knowledge:**
- Most enterprise runs have predictable `len` values:
  - Authorization: typically `len = 1-4` (user has 1-4 permissions)
  - Compliance: typically `len = 1-8` (resource has 1-8 compliance flags)
  - Provenance: typically `len = 8` (full batch)

**Rust can generate:**
- Specialized functions for each `len` value: `knhk_construct8_emit_8_len1()` through `knhk_construct8_emit_8_len8()`
- Each function has `len_mask_bits` as compile-time constant: `0x01`, `0x03`, `0x07`, `0x0F`, `0x1F`, `0x3F`, `0x7F`, `0xFF`

**Savings:** ~0.25 ticks per call

### 1.3. Pattern-Based Precomputation
**Current Runtime Cost:** Mask generation (~4 ticks)

**Hardcoded Knowledge:**
- **All-NonZero Pattern**: Enterprise data often has no zeros
  - Authorization: Users always have at least one permission
  - Compliance: Resources always have compliance state
  - **Rust can detect**: Precompute mask = `0xFF` (all non-zero)
  - **Hot path**: Skip mask generation entirely

- **All-Zero Pattern**: Empty runs (rare but detectable)
  - **Rust can detect**: Early return (0 ticks)
  - **Hot path**: Skip everything

- **Sparse Pattern**: Known zero positions
  - **Rust can detect**: Precompute zero-position hint bitmask
  - **Hot path**: Use hint to skip unnecessary comparisons

**Savings:** ~4 ticks for all-nonzero pattern

### 1.4. Provenance Templates
**Current Runtime Cost:** Receipt generation (moved out of hot path)

**Hardcoded Knowledge:**
- Provenance receipt structure is fixed template:
  - `(?action, ex:hasReceipt, ?receipt)`
  - `(?receipt, ex:spanId, ?spanId)`
  - `(?receipt, ex:aHash, ?hash)`
  - **Rust can precompute**: Receipt template structure
  - **Hot path**: Just fill in values (no structure computation)

**Savings:** Receipt generation already moved out of hot path

## 2. 80/20 Enterprise CONSTRUCT Use Cases

Based on enterprise test data and CONVO.txt analysis, here are the most common patterns:

### 2.1. Authorization Reflex (30% of Runtime) - TOP PRIORITY

**Pattern:**
```sparql
CONSTRUCT { ?u ex:hasAccess ?r } WHERE { ?u ex:role ?x . ?x ex:grants ?r }
```

**Hardcoded Knowledge:**
- Template: `(?u, ex:hasAccess, ?r)`
- `p_const = ex:hasAccess` (fixed)
- `o_const = ?r` (from role grants - can be precomputed at warm path)
- Length: Typically 1-4 permissions per user

**Rust AOT Preparation:**
1. Precompute role→permission mappings at warm path
2. Generate specialized function: `knhk_construct8_emit_8_auth()`
3. Precompute broadcast vectors for common permission sets
4. Store authorization template in IR structure

**Hot Path Cost:** ~6 ticks (with optimizations)

**Use Cases:**
- RBAC permission checks
- Entitlement generation
- Access control decisions

### 2.2. Compliance Classification (20% of Runtime)

**Pattern:**
```sparql
CONSTRUCT { ?s rdf:type :Compliant } WHERE { ?s ex:passesPolicy true }
```

**Hardcoded Knowledge:**
- Template: `(?s, rdf:type, :Compliant)`
- `p_const = rdf:type` (fixed)
- `o_const = :Compliant` (constant)
- Pattern: All subjects that pass policy → all non-zero

**Rust AOT Preparation:**
1. Precompute compliance state from policy evaluation
2. Generate specialized function: `knhk_construct8_emit_8_compliant()`
3. Use all-nonzero pattern specialization (skip mask generation)
4. Precompute broadcast vectors

**Hot Path Cost:** ~4 ticks (with all-nonzero optimization)

**Use Cases:**
- Regulatory compliance flags
- Policy compliance states
- Audit trail generation

### 2.3. Risk Flag Generation (15% of Runtime)

**Pattern:**
```sparql
CONSTRUCT { ?asset ex:riskLevel ?level } WHERE { ?asset ex:riskScore ?score . FILTER(?score > threshold) }
```

**Hardcoded Knowledge:**
- Template: `(?asset, ex:riskLevel, ?level)`
- `p_const = ex:riskLevel` (fixed)
- `o_const = ex:High|Medium|Low` (precomputed from thresholds)
- Pattern: Risk levels computed from scores (can be precomputed)

**Rust AOT Preparation:**
1. Precompute risk levels from scores at warm path
2. Generate specialized function: `knhk_construct8_emit_8_risk()`
3. Precompute risk level constants
4. Store risk classification in IR

**Hot Path Cost:** ~6 ticks

**Use Cases:**
- Financial risk assessment
- Security risk flags
- Operational risk indicators

### 2.4. Provenance Assertions (10% of Runtime)

**Pattern:**
```sparql
CONSTRUCT { ?action ex:hasReceipt ?receipt } WHERE { ?action ex:executed true }
```

**Hardcoded Knowledge:**
- Template: `(?action, ex:hasReceipt, ?receipt)`
- `p_const = ex:hasReceipt` (fixed)
- `o_const = ?receipt` (receipt ID - can be precomputed)
- Pattern: Provenance structure is fixed

**Rust AOT Preparation:**
1. Precompute receipt IDs from span IDs
2. Generate specialized function: `knhk_construct8_emit_8_provenance()`
3. Precompute receipt template structure
4. Store provenance template in IR

**Hot Path Cost:** ~6 ticks

**Use Cases:**
- Audit trail generation
- Provenance tracking
- Receipt generation

### 2.5. Type Classification (10% of Runtime)

**Pattern:**
```sparql
CONSTRUCT { ?resource rdf:type ?type } WHERE { ?resource ex:hasSchema ?schema . ?schema ex:definesType ?type }
```

**Hardcoded Knowledge:**
- Template: `(?resource, rdf:type, ?type)`
- `p_const = rdf:type` (fixed)
- `o_const = ?type` (from schema - can be precomputed)
- Pattern: Schema→type mappings are static

**Rust AOT Preparation:**
1. Precompute schema→type mappings at warm path
2. Generate specialized function: `knhk_construct8_emit_8_type()`
3. Precompute type constants
4. Store type template in IR

**Hot Path Cost:** ~6 ticks

**Use Cases:**
- Schema validation
- Type inference
- RDF type materialization

### 2.6. Entitlement Sets (5% of Runtime)

**Pattern:**
```sparql
CONSTRUCT { ?user ex:entitled ?resource } WHERE { ?user ex:role ?role . ?role ex:canAccess ?resource }
```

**Hardcoded Knowledge:**
- Template: `(?user, ex:entitled, ?resource)`
- `p_const = ex:entitled` (fixed)
- `o_const = ?resource` (from role→resource mapping - can be precomputed)
- Pattern: Role→resource mappings are relatively static

**Rust AOT Preparation:**
1. Precompute role→resource mappings at warm path
2. Generate specialized function: `knhk_construct8_emit_8_entitlement()`
3. Precompute entitlement sets
4. Store entitlement template in IR

**Hot Path Cost:** ~6 ticks

**Use Cases:**
- Access control lists
- Resource entitlements
- Permission sets

### 2.7. Status Flags (5% of Runtime)

**Pattern:**
```sparql
CONSTRUCT { ?process ex:status ?status } WHERE { ?process ex:state ?state . ?state ex:mapsToStatus ?status }
```

**Hardcoded Knowledge:**
- Template: `(?process, ex:status, ?status)`
- `p_const = ex:status` (fixed)
- `o_const = ?status` (from state→status mapping - can be precomputed)
- Pattern: State→status mappings are deterministic

**Rust AOT Preparation:**
1. Precompute state→status mappings at warm path
2. Generate specialized function: `knhk_construct8_emit_8_status()`
3. Precompute status constants
4. Store status template in IR

**Hot Path Cost:** ~6 ticks

**Use Cases:**
- Process state materialization
- Status tracking
- Workflow state flags

### 2.8. Delta Assertions (5% of Runtime)

**Pattern:**
```sparql
CONSTRUCT { ?delta ex:added ?triple } WHERE { ?delta ex:adds ?triple }
```

**Hardcoded Knowledge:**
- Template: `(?delta, ex:added, ?triple)`
- `p_const = ex:added` (fixed)
- `o_const = ?triple` (from delta - computed at warm path)
- Pattern: Delta structure is fixed

**Rust AOT Preparation:**
1. Precompute delta assertions at warm path
2. Generate specialized function: `knhk_construct8_emit_8_delta()`
3. Precompute delta structure
4. Store delta template in IR

**Hot Path Cost:** ~6 ticks

**Use Cases:**
- Change tracking
- Delta generation
- Event materialization

## 3. Hardcoded Knowledge Summary

### What Can Be 100% Hardcoded (Zero Hot Path Cost):

1. **Template Predicates**: `ex:hasAccess`, `rdf:type`, `ex:riskLevel`, etc.
   - Cost: 0 ticks (compile-time constants)

2. **Template Objects**: `:Compliant`, `:Allowed`, `:Denied`, etc.
   - Cost: 0 ticks (compile-time constants)

3. **Length Masks**: For known `len` values (1-8)
   - Cost: 0 ticks (compile-time constants)

4. **Pattern Masks**: All-nonzero, all-zero, sparse patterns
   - Cost: 0 ticks (warm-path precomputation)

5. **Broadcast Vectors**: For common constants
   - Cost: 0 ticks (compile-time optimization)

6. **Function Selection**: Optimal specialized function variant
   - Cost: 0 ticks (warm-path analysis)

### What Must Be Computed at Runtime (Minimal Cost):

1. **Subject Values**: From input array `S_base[off..off+len]`
   - Cost: ~4 ticks (SIMD loads)

2. **Mask Generation**: For unknown patterns
   - Cost: ~4 ticks (SIMD comparisons) - but can be eliminated if pattern known

3. **Blend Operations**: Conditional selection based on masks
   - Cost: ~8 ticks (SIMD blends)

4. **Store Operations**: Writing results to output arrays
   - Cost: ~12 ticks (SIMD stores)

5. **Mask Extraction**: Building bitmask for output
   - Cost: ~4 ticks (lane extraction) - but can overlap with stores

## 4. 80/20 Optimization Strategy

### Priority 1: Hardcode Top 3 Use Cases (80% of Value)

1. **Authorization Reflex** (30% of runtime)
   - Hardcode: `ex:hasAccess` predicate, common permission sets
   - Generate: `knhk_construct8_emit_8_auth()` specialized function
   - Expected savings: ~2 ticks (constant optimization + pattern detection)

2. **Compliance Classification** (20% of runtime)
   - Hardcode: `rdf:type :Compliant` template, all-nonzero pattern
   - Generate: `knhk_construct8_emit_8_compliant()` specialized function
   - Expected savings: ~4 ticks (skip mask generation)

3. **Risk Flag Generation** (15% of runtime)
   - Hardcode: `ex:riskLevel` predicate, risk level constants
   - Generate: `knhk_construct8_emit_8_risk()` specialized function
   - Expected savings: ~1 tick (constant optimization)

**Total Coverage:** 65% of runtime use cases

### Priority 2: Generic Hardcoding (Remaining 35%)

4. **Length Specialization**: Generate functions for len ∈ {1, 2, 3, 4, 5, 6, 7, 8}
   - Coverage: All use cases
   - Expected savings: ~0.25 ticks per call

5. **Pattern Detection**: Analyze inputs for all-nonzero/all-zero patterns
   - Coverage: ~40% of use cases (authorization, compliance often all-nonzero)
   - Expected savings: ~4 ticks per call (when pattern detected)

6. **Constant Propagation**: Precompute common `p_const`/`o_const` pairs
   - Coverage: ~60% of use cases (enterprise templates are stable)
   - Expected savings: ~1 tick per call

## 5. Rust AOT Implementation Plan

### Phase 1: Template Registry (Build-Time)

```rust
// rust/knhk-aot/src/templates.rs
pub struct ConstructTemplate {
    pub name: &'static str,
    pub p_const: u64,        // Hardcoded predicate
    pub o_const: Option<u64>, // Hardcoded object (if constant)
    pub pattern_hint: PatternType, // Expected pattern
    pub len_range: Range<u8>, // Expected length range
}

pub const TEMPLATES: &[ConstructTemplate] = &[
    ConstructTemplate {
        name: "authorization",
        p_const: hash_iri("ex:hasAccess"),
        o_const: None,  // Variable (from role grants)
        pattern_hint: PatternType::AllNonZero,
        len_range: 1..5,
    },
    ConstructTemplate {
        name: "compliance",
        p_const: hash_iri("rdf:type"),
        o_const: Some(hash_iri(":Compliant")),
        pattern_hint: PatternType::AllNonZero,
        len_range: 1..8,
    },
    // ... more templates
];
```

### Phase 2: Code Generation (Build-Time)

```rust
// rust/knhk-aot/build.rs
fn generate_specialized_functions() {
    for template in TEMPLATES {
        for len in template.len_range {
            let code = generate_function(template, len);
            write_to_file(&format!("construct8_{}_len{}.c", template.name, len), code);
        }
    }
}
```

### Phase 3: Runtime Selection (Warm Path)

```rust
// rust/knhk-aot/src/dispatch.rs
pub fn select_optimal_function(
    template: &ConstructTemplate,
    len: u8,
    zero_hint: u8
) -> Construct8Fn {
    match (template.name, len, zero_hint) {
        ("authorization", 4, 0x00) => knhk_construct8_emit_8_auth_len4_all_nonzero,
        ("compliance", 8, 0x00) => knhk_construct8_emit_8_compliant_len8_all_nonzero,
        // ... more cases
        _ => knhk_construct8_emit_8_generic, // Fallback
    }
}
```

## 6. Expected Performance Impact

### Current Performance: ~42 ticks

### With Hardcoded Knowledge Optimizations:

**Best Case (All Optimizations Applied):**
- Authorization (30%): 42 → 36 ticks (hardcoded constants)
- Compliance (20%): 42 → 34 ticks (all-nonzero pattern)
- Risk (15%): 42 → 37 ticks (hardcoded constants)
- Remaining (35%): 42 → 38 ticks (length specialization)

**Weighted Average:** ~36 ticks (still exceeds 8-tick budget, but 14% improvement)

**Critical Insight:** Even with all hardcoding, we still need runtime optimizations (ILP overlap, store reduction) to hit 8 ticks.

## 7. Hardcoded Knowledge Checklist

### ✅ Can Be Hardcoded (Zero Hot Path Cost):
- [x] Template predicates (`p_const` values)
- [x] Template objects (`o_const` values when constant)
- [x] Length mask bits (for known `len` values)
- [x] Pattern masks (all-nonzero, all-zero)
- [x] Broadcast vectors (for common constants)
- [x] Function selection (optimal variant)
- [x] Zero-position hints (warm-path analysis)
- [x] Alignment guarantees (Rust allocation)

### ❌ Cannot Be Hardcoded (Runtime Required):
- [ ] Subject values (from input array)
- [ ] Mask generation (for unknown patterns)
- [ ] Blend operations (SIMD conditional selection)
- [ ] Store operations (memory writes)
- [ ] Dynamic mask extraction (when pattern unknown)

## 8. Conclusion

**Hardcoded Knowledge Benefits:**
- **Authorization**: ~2 ticks saved (30% of runtime)
- **Compliance**: ~4 ticks saved (20% of runtime)
- **Risk**: ~1 tick saved (15% of runtime)
- **Generic**: ~0.25 ticks saved (length specialization)

**Total Potential Savings:** ~2-4 ticks per call (depending on use case)

**Critical Finding:** Hardcoded knowledge alone is **not sufficient** to reach 8-tick budget. We still need:
1. ILP overlap optimizations
2. Store count reduction
3. Better instruction scheduling

**However**, hardcoded knowledge provides the **foundation** for specialized functions that can then be further optimized with runtime techniques.

**Recommendation:** Implement hardcoded knowledge optimizations **first** (foundation), then apply runtime optimizations (ILP, stores) to specialized functions.

