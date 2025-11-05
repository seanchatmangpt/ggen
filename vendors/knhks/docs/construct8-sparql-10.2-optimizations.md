# CONSTRUCT Query Optimization: What Can Be Hardcoded/AOT (SPARQL 10.2)

## Overview

Analysis of SPARQL CONSTRUCT queries (Section 10.2) to identify what can be **assumed**, **hardcoded**, or **AOT-optimized** for the 8-tick hot path and warm path execution.

## 1. Template Structure (100% Hardcodable)

### 1.1. Fixed Template Patterns

**From SPARQL Spec 10.2:**
```sparql
CONSTRUCT { <http://example.org/person#Alice> vcard:FN ?name }
WHERE { ?x foaf:name ?name }
```

**What Can Be Hardcoded:**
- **Subject**: If constant (e.g., `<http://example.org/person#Alice>`), hardcode as `uint64_t`
- **Predicate**: If constant (e.g., `vcard:FN`), hardcode as `uint64_t`
- **Object**: If variable (e.g., `?name`), known at runtime from WHERE clause

**AOT Optimization:**
```c
// Compile-time specialization
typedef struct {
  uint64_t s_const;      // Hardcoded subject (0 = variable)
  uint64_t p_const;      // Hardcoded predicate (0 = variable)
  uint64_t o_const;      // Hardcoded object (0 = variable)
  uint8_t s_is_var;      // 1 if subject is variable
  uint8_t p_is_var;      // 1 if predicate is variable
  uint8_t o_is_var;      // 1 if object is variable
} construct_template_t;

// AOT-generated specialized functions
static inline size_t knhk_construct8_emit_8_s_const_p_const_o_var(
  const uint64_t *S_base, uint64_t off, uint64_t len,
  uint64_t s_const, uint64_t p_const,
  const uint64_t *O_base,  // Object comes from WHERE clause
  uint64_t *out_S, uint64_t *out_P, uint64_t *out_O
) {
  // Hardcoded: s_const, p_const
  // Variable: O_base[off..off+len]
  // No need to broadcast s_const/p_const (compile-time constants)
  // ...
}
```

**Savings:** ~1 tick (eliminates runtime broadcasts for constants)

### 1.2. Ground Triples (100% Hardcodable)

**From SPARQL Spec 10.2:**
> "The graph template can contain triples with no variables (known as ground or explicit triples), and these also appear in the output RDF graph."

**Example:**
```sparql
CONSTRUCT {
  <http://example.org/person#Alice> vcard:FN "Alice" .
  <http://example.org/person#Alice> vcard:ORG "Example Corp" .
  ?x vcard:FN ?name .
}
WHERE { ?x foaf:name ?name }
```

**What Can Be Hardcoded:**
- Ground triples (no variables) are **completely constant**
- Can be emitted **once** at warm path time, not per solution
- No hot path execution needed for ground triples

**AOT Optimization:**
```rust
// Rust warm path: Precompute ground triples
pub struct ConstructTemplate {
    pub ground_triples: Vec<(u64, u64, u64)>,  // (s, p, o) constants
    pub template_triples: Vec<TemplateTriple>, // Variable triples
}

// Warm path: Emit ground triples once
fn emit_ground_triples(template: &ConstructTemplate, output: &mut Vec<Quad>) {
    for (s, p, o) in &template.ground_triples {
        output.push(Quad { s: *s, p: *p, o: *o, g: 0 });
    }
}

// Hot path: Only emit variable triples
fn construct8_variable_triples(...) {
    // Only processes variable triples (≤8)
}
```

**Savings:** Eliminates ground triple emission from hot path entirely

### 1.3. Variable Binding Patterns

**Analysis:**
- **Subject Variable** (`?x`): Bound from WHERE clause → Known at runtime
- **Predicate Variable** (`?p`): Rare in CONSTRUCT templates → Usually constant
- **Object Variable** (`?o`): Bound from WHERE clause → Known at runtime

**What Can Be Hardcoded:**
- **Variable positions**: Known at compile time (template analysis)
- **Variable sources**: WHERE clause binding → Can be precomputed at warm path

**AOT Optimization:**
```rust
// Rust AOT: Analyze template and WHERE clause
pub fn analyze_template(template: &str, where: &str) -> TemplateAnalysis {
    let mut analysis = TemplateAnalysis::new();
    
    // Identify variable positions
    if template.contains("?s") {
        analysis.s_is_var = true;
        analysis.s_source = find_binding_source("?s", where);
    }
    if template.contains("?p") {
        analysis.p_is_var = true;
        analysis.p_source = find_binding_source("?p", where);
    }
    if template.contains("?o") {
        analysis.o_is_var = true;
        analysis.o_source = find_binding_source("?o", where);
    }
    
    // Generate specialized function
    generate_specialized_function(&analysis);
    analysis
}
```

## 2. Blank Node Labels (AOT Optimizable)

### 2.1. Blank Node Scoping

**From SPARQL Spec 10.2.1:**
> "The blank node labels are scoped to the template for each solution. If the same label occurs twice in a template, then there will be one blank node created for each query solution."

**Example:**
```sparql
CONSTRUCT { ?x vcard:N _:v .
            _:v vcard:givenName ?gname .
            _:v vcard:familyName ?fname }
```

**What Can Be Hardcoded:**
- **Blank node count**: Known from template analysis
- **Blank node structure**: Known from template structure
- **Blank node allocation**: Can be pre-allocated (per solution)

**AOT Optimization:**
```rust
// Rust AOT: Analyze blank node structure
pub struct BlankNodeTemplate {
    pub bnode_count: u8,              // Max blank nodes per solution
    pub bnode_structure: Vec<BNodeTriple>, // Structure of blank node triples
}

// Warm path: Pre-allocate blank node IDs
fn allocate_blank_nodes(template: &BlankNodeTemplate, solution_count: usize) -> Vec<u64> {
    let mut bnode_ids = Vec::new();
    for _ in 0..(template.bnode_count * solution_count) {
        bnode_ids.push(generate_blank_node_id());
    }
    bnode_ids
}

// Hot path: Use pre-allocated blank node IDs
fn construct8_with_bnodes(
    bnode_ids: &[u64],
    bnode_offset: usize,
    template: &BlankNodeTemplate,
    // ... other args
) {
    // Use pre-allocated IDs (no allocation in hot path)
}
```

**Savings:** Eliminates blank node allocation from hot path

### 2.2. Blank Node Label Mapping

**What Can Be Hardcoded:**
- **Label→ID mapping**: Can be precomputed at warm path
- **ID generation**: Can use deterministic ID generation (hash-based)

**AOT Optimization:**
```rust
// Warm path: Generate deterministic blank node IDs
fn generate_bnode_id(solution_id: u64, bnode_label: &str) -> u64 {
    // Deterministic: hash(solution_id, bnode_label)
    // Ensures same solution → same blank node ID
    hash64(solution_id, bnode_label)
}
```

**Savings:** Deterministic IDs enable better caching/optimization

## 3. Graph Access (AOT Optimizable)

### 3.1. Named Graph Access

**From SPARQL Spec 10.2.2:**
```sparql
CONSTRUCT { ?s ?p ?o }
WHERE { GRAPH <http://example.org/aGraph> { ?s ?p ?o } }
```

**What Can Be Hardcoded:**
- **Graph IRI**: If constant (e.g., `<http://example.org/aGraph>`), hardcode as `uint64_t`
- **Graph selection**: Can be precomputed at warm path (graph lookup)

**AOT Optimization:**
```rust
// Rust AOT: Analyze GRAPH clause
pub fn analyze_graph_clause(where: &str) -> GraphAccess {
    if let Some(graph_iri) = extract_constant_graph(where) {
        GraphAccess::Constant(graph_iri)  // Hardcode graph IRI
    } else {
        GraphAccess::Variable  // Variable graph (rare)
    }
}

// Warm path: Pre-select graph
fn select_graph(ctx: &Context, graph_iri: u64) -> Option<GraphHandle> {
    ctx.graphs.get(&graph_iri)  // O(1) lookup
}

// Hot path: Use pre-selected graph handle
fn construct8_from_graph(
    graph: &GraphHandle,  // Pre-selected graph
    // ... other args
) {
    // Direct access to graph data (no lookup in hot path)
}
```

**Savings:** Eliminates graph lookup from hot path

### 3.2. Conditional Graph Access

**From SPARQL Spec 10.2.2:**
```sparql
CONSTRUCT { ?s ?p ?o }
WHERE {
  GRAPH ?g { ?s ?p ?o } .
  { ?g dc:publisher <http://www.w3.org/> } .
  FILTER ( app:customDate(?date) > "2005-02-28T00:00:00Z"^^xsd:dateTime ) .
}
```

**What Can Be Hardcoded:**
- **Graph filter conditions**: Can be precomputed at warm path
- **Graph selection**: Can be done at warm path, hot path only processes selected graphs

**AOT Optimization:**
```rust
// Warm path: Pre-filter graphs based on conditions
fn prefilter_graphs(ctx: &Context, conditions: &GraphConditions) -> Vec<GraphHandle> {
    ctx.graphs
        .iter()
        .filter(|(_, graph)| matches_conditions(graph, conditions))
        .map(|(_, graph)| graph.clone())
        .collect()
}

// Hot path: Process only pre-filtered graphs
fn construct8_from_filtered_graphs(
    graphs: &[GraphHandle],  // Pre-filtered graphs
    // ... other args
) {
    // Process graphs sequentially (no filtering in hot path)
}
```

**Savings:** Eliminates graph filtering from hot path

## 4. Solution Modifiers (AOT Optimizable)

### 4.1. ORDER BY

**From SPARQL Spec 10.2.3:**
```sparql
CONSTRUCT { [] foaf:name ?name }
WHERE { [] foaf:name ?name ; site:hits ?hits }
ORDER BY desc(?hits)
LIMIT 2
```

**What Can Be Hardcoded:**
- **Sort key**: Known from ORDER BY clause (e.g., `?hits`)
- **Sort direction**: Known from ORDER BY clause (e.g., `desc`)
- **Sort operation**: Can be precomputed at warm path (pre-sort solutions)

**AOT Optimization:**
```rust
// Rust AOT: Analyze ORDER BY clause
pub fn analyze_order_by(order_by: &str) -> OrderBy {
    if order_by.contains("desc") {
        OrderBy::Descending(extract_variable(order_by))
    } else {
        OrderBy::Ascending(extract_variable(order_by))
    }
}

// Warm path: Pre-sort solutions
fn pre_sort_solutions(solutions: &mut Vec<Solution>, order_by: &OrderBy) {
    match order_by {
        OrderBy::Ascending(var) => {
            solutions.sort_by_key(|s| s.get(var));
        }
        OrderBy::Descending(var) => {
            solutions.sort_by_key(|s| Reverse(s.get(var)));
        }
    }
}

// Hot path: Process pre-sorted solutions (no sorting)
fn construct8_from_sorted_solutions(
    solutions: &[Solution],  // Pre-sorted
    // ... other args
) {
    // Process solutions in order (already sorted)
}
```

**Savings:** Eliminates sorting from hot path

### 4.2. LIMIT

**What Can Be Hardcoded:**
- **Limit value**: Known from LIMIT clause (e.g., `LIMIT 2`)
- **Limit application**: Can be applied at warm path (truncate solutions)

**AOT Optimization:**
```rust
// Warm path: Apply LIMIT before hot path
fn apply_limit(solutions: &mut Vec<Solution>, limit: usize) {
    solutions.truncate(limit);
}

// Hot path: Process limited solutions (no limit checking)
fn construct8_from_limited_solutions(
    solutions: &[Solution],  // Already limited
    // ... other args
) {
    // Process all solutions (limit already applied)
}
```

**Savings:** Eliminates limit checking from hot path

### 4.3. OFFSET

**What Can Be Hardcoded:**
- **Offset value**: Known from OFFSET clause
- **Offset application**: Can be applied at warm path (skip solutions)

**AOT Optimization:**
```rust
// Warm path: Apply OFFSET before hot path
fn apply_offset(solutions: &mut Vec<Solution>, offset: usize) {
    solutions.drain(..offset);
}

// Hot path: Process offset solutions (no offset checking)
fn construct8_from_offset_solutions(
    solutions: &[Solution],  // Already offset
    // ... other args
) {
    // Process all solutions (offset already applied)
}
```

**Savings:** Eliminates offset checking from hot path

## 5. WHERE Clause Patterns (AOT Optimizable)

### 5.1. Triple Pattern Analysis

**What Can Be Hardcoded:**
- **Pattern structure**: Known from WHERE clause
- **Variable bindings**: Can be precomputed at warm path
- **Join patterns**: Can be optimized at warm path (pre-join)

**AOT Optimization:**
```rust
// Rust AOT: Analyze WHERE clause patterns
pub fn analyze_where_clause(where: &str) -> WhereAnalysis {
    let patterns = parse_triple_patterns(where);
    let variables = extract_variables(&patterns);
    let joins = identify_joins(&patterns);
    
    WhereAnalysis {
        patterns,
        variables,
        joins,
        // Generate optimized execution plan
        execution_plan: generate_execution_plan(&patterns, &joins),
    }
}

// Warm path: Pre-join data
fn pre_join_data(ctx: &Context, where_analysis: &WhereAnalysis) -> JoinedData {
    // Execute joins at warm path
    // Return pre-joined data for hot path
}

// Hot path: Process pre-joined data (no joins)
fn construct8_from_prejoined_data(
    joined_data: &JoinedData,  // Pre-joined
    // ... other args
) {
    // Process joined data directly (no joins in hot path)
}
```

**Savings:** Eliminates joins from hot path

### 5.2. FILTER Conditions

**What Can Be Hardcoded:**
- **Filter expressions**: Can be analyzed at compile time
- **Filter evaluation**: Can be done at warm path (pre-filter solutions)

**AOT Optimization:**
```rust
// Warm path: Pre-filter solutions
fn pre_filter_solutions(solutions: &mut Vec<Solution>, filter: &FilterExpr) {
    solutions.retain(|s| evaluate_filter(s, filter));
}

// Hot path: Process pre-filtered solutions (no filtering)
fn construct8_from_filtered_solutions(
    solutions: &[Solution],  // Pre-filtered
    // ... other args
) {
    // Process all solutions (filter already applied)
}
```

**Savings:** Eliminates filter evaluation from hot path

### 5.3. UNION Patterns

**What Can Be Hardcoded:**
- **UNION branches**: Known from WHERE clause
- **UNION selection**: Can be done at warm path (pre-select branch)

**AOT Optimization:**
```rust
// Warm path: Pre-select UNION branch
fn pre_select_union_branch(ctx: &Context, union: &UnionPattern) -> Vec<Solution> {
    // Evaluate each branch, select best branch
    // Return solutions from selected branch
}

// Hot path: Process pre-selected branch (no UNION evaluation)
fn construct8_from_union_solutions(
    solutions: &[Solution],  // Pre-selected from UNION
    // ... other args
) {
    // Process solutions directly (no UNION in hot path)
}
```

**Savings:** Eliminates UNION evaluation from hot path

## 6. CONSTRUCT8-Specific Optimizations

### 6.1. Fixed Template Assumptions

**For CONSTRUCT8 (≤8 triples):**

**What Can Be Hardcoded:**
- **Template size**: Known at compile time (≤8 triples)
- **Template structure**: Known at compile time (fixed template)
- **Variable bindings**: Precomputed at warm path

**AOT Optimization:**
```rust
// Rust AOT: Generate CONSTRUCT8 template
pub fn generate_construct8_template(template: &str) -> Construct8Template {
    let triples = parse_template_triples(template);
    
    assert!(triples.len() <= 8, "CONSTRUCT8 template must have ≤8 triples");
    
    Construct8Template {
        triples,
        // Precompute constant values
        constants: extract_constants(&triples),
        // Precompute variable positions
        variable_positions: extract_variable_positions(&triples),
    }
}

// Warm path: Bind variables, prepare hot path input
fn prepare_construct8_input(
    template: &Construct8Template,
    solutions: &[Solution],
) -> Construct8Input {
    // Bind variables from solutions
    // Prepare S, P, O arrays for hot path
    // Precompute constants
    Construct8Input {
        S: bind_subjects(template, solutions),
        P: bind_predicates(template, solutions),  // Usually constant
        O: bind_objects(template, solutions),
        len: solutions.len().min(8),
    }
}

// Hot path: Execute CONSTRUCT8 (≤8 ticks)
fn construct8_execute(input: &Construct8Input, output: &mut Construct8Output) {
    knhk_construct8_emit_8(
        input.S.as_ptr(),
        input.off,
        input.len,
        input.p_const,  // Usually constant
        input.o_const,  // May be constant
        output.out_S.as_mut_ptr(),
        output.out_P.as_mut_ptr(),
        output.out_O.as_mut_ptr(),
    );
}
```

### 6.2. Illegal RDF Construct Detection

**From SPARQL Spec 10.2:**
> "If any such instantiation produces a triple containing an unbound variable or an illegal RDF construct, such as a literal in subject or predicate position, then that triple is not included in the output RDF graph."

**What Can Be Hardcoded:**
- **Validation rules**: Known at compile time (RDF constraints)
- **Validation checks**: Can be done at warm path (pre-validate solutions)

**AOT Optimization:**
```rust
// Warm path: Pre-validate solutions
fn pre_validate_solutions(solutions: &mut Vec<Solution>, template: &Construct8Template) {
    solutions.retain(|s| {
        // Validate each solution against template
        validate_solution_against_template(s, template)
    });
}

// Hot path: Process pre-validated solutions (no validation)
fn construct8_from_validated_solutions(
    solutions: &[Solution],  // Pre-validated
    // ... other args
) {
    // All solutions are valid (no validation in hot path)
}
```

**Savings:** Eliminates RDF validation from hot path

## 7. Summary: What Can Be Hardcoded/AOT

### 7.1. 100% Hardcodable (Zero Hot Path Cost)

| Component | Hardcoding Strategy | Savings |
|-----------|---------------------|---------|
| **Ground Triples** | Emit at warm path once | Eliminates from hot path |
| **Constant Predicates** | Compile-time constants | ~1 tick (no broadcast) |
| **Constant Objects** | Compile-time constants | ~1 tick (no broadcast) |
| **Constant Subjects** | Compile-time constants | ~1 tick (no broadcast) |
| **Template Structure** | Fixed template analysis | Enables specialization |
| **Blank Node Count** | Known from template | Pre-allocation possible |
| **Graph IRI (constant)** | Hardcode graph lookup | Eliminates lookup |
| **LIMIT Value** | Apply at warm path | Eliminates limit check |
| **OFFSET Value** | Apply at warm path | Eliminates offset check |
| **ORDER BY Key** | Pre-sort at warm path | Eliminates sorting |

### 7.2. AOT Optimizable (Warm Path Precomputation)

| Component | AOT Strategy | Savings |
|-----------|--------------|---------|
| **Variable Bindings** | Pre-bind at warm path | Eliminates binding from hot path |
| **Blank Node IDs** | Pre-allocate at warm path | Eliminates allocation |
| **Graph Selection** | Pre-select at warm path | Eliminates graph lookup |
| **Solution Filtering** | Pre-filter at warm path | Eliminates filter evaluation |
| **Solution Sorting** | Pre-sort at warm path | Eliminates sorting |
| **Solution Limiting** | Pre-limit at warm path | Eliminates limit checking |
| **Join Operations** | Pre-join at warm path | Eliminates joins |
| **UNION Evaluation** | Pre-select at warm path | Eliminates UNION evaluation |
| **RDF Validation** | Pre-validate at warm path | Eliminates validation |

### 7.3. Runtime Required (Hot Path)

| Component | Runtime Requirement | Notes |
|-----------|---------------------|-------|
| **Subject Values** | From WHERE clause bindings | Variable, but pre-bound |
| **Object Values** | From WHERE clause bindings | Variable, but pre-bound |
| **Solution Iteration** | Process each solution | Required, but pre-processed |
| **Triple Emission** | Write to output arrays | Required, but optimized (SIMD) |

## 8. Implementation Strategy

### Phase 1: Template Analysis (Rust AOT)

```rust
// rust/knhk-aot/src/template_analyzer.rs
pub struct ConstructTemplateAnalyzer;

impl ConstructTemplateAnalyzer {
    pub fn analyze(query: &str) -> TemplateAnalysis {
        // 1. Parse CONSTRUCT template
        let template = parse_construct_template(query);
        
        // 2. Identify constants vs variables
        let constants = extract_constants(&template);
        let variables = extract_variables(&template);
        
        // 3. Identify ground triples
        let ground_triples = extract_ground_triples(&template);
        
        // 4. Analyze blank nodes
        let blank_nodes = analyze_blank_nodes(&template);
        
        // 5. Analyze WHERE clause
        let where_analysis = analyze_where_clause(query);
        
        // 6. Analyze solution modifiers
        let modifiers = analyze_solution_modifiers(query);
        
        TemplateAnalysis {
            template,
            constants,
            variables,
            ground_triples,
            blank_nodes,
            where_analysis,
            modifiers,
        }
    }
}
```

### Phase 2: Warm Path Preprocessing (Rust)

```rust
// rust/knhk-hot/src/construct8_preprocessor.rs
pub fn preprocess_construct8(
    analysis: &TemplateAnalysis,
    ctx: &Context,
    solutions: &[Solution],
) -> Construct8Input {
    // 1. Emit ground triples (once)
    let mut output = emit_ground_triples(&analysis.ground_triples);
    
    // 2. Apply solution modifiers
    let mut processed_solutions = apply_modifiers(solutions, &analysis.modifiers);
    
    // 3. Pre-validate solutions
    let validated_solutions = pre_validate(&processed_solutions, &analysis);
    
    // 4. Pre-bind variables
    let bound_data = pre_bind_variables(&validated_solutions, &analysis.variables);
    
    // 5. Pre-allocate blank nodes
    let bnode_ids = pre_allocate_blank_nodes(&analysis.blank_nodes, validated_solutions.len());
    
    // 6. Prepare hot path input
    Construct8Input {
        S: bound_data.subjects,
        P: bound_data.predicates,
        O: bound_data.objects,
        len: validated_solutions.len().min(8),
        p_const: analysis.constants.predicate,
        o_const: analysis.constants.object,
        bnode_ids,
    }
}
```

### Phase 3: Hot Path Execution (C)

```c
// src/simd/construct.h (already implemented)
// Hot path: Pure CONSTRUCT8 execution (≤8 ticks)
// All preprocessing done at warm path
```

## 9. Expected Performance Impact

### Current CONSTRUCT8 Performance: ~42 ticks

### With Hardcoding/AOT Optimizations:

**Ground Triples:** Eliminated from hot path (0 ticks saved per solution)

**Constant Optimization:** ~1-2 ticks saved (no broadcasts)

**Pre-validation:** ~2-4 ticks saved (no validation in hot path)

**Pre-binding:** ~1-2 ticks saved (no binding in hot path)

**Pre-modifiers:** ~2-4 ticks saved (no sorting/limiting in hot path)

**Total Potential Savings:** ~8-14 ticks

**Target Performance:** ~28-34 ticks (still exceeds 8-tick budget, but significant improvement)

**Critical Insight:** Hardcoding/AOT alone is **not sufficient** to reach 8-tick budget. We still need:
1. ILP overlap optimizations
2. Store count reduction
3. Better instruction scheduling

## 10. Conclusion

**Key Findings:**

1. **100% Hardcodable:**
   - Ground triples (emit at warm path)
   - Constant predicates/objects/subjects (compile-time)
   - Template structure (fixed template)
   - Solution modifiers (apply at warm path)

2. **AOT Optimizable:**
   - Variable bindings (pre-bind at warm path)
   - Blank node allocation (pre-allocate)
   - Graph selection (pre-select)
   - Solution filtering/sorting/limiting (pre-process)

3. **Runtime Required:**
   - Subject/object values (from pre-bound variables)
   - Triple emission (SIMD-optimized)
   - Solution iteration (pre-processed)

**Recommendation:** Implement hardcoding/AOT optimizations **first** (foundation), then apply runtime optimizations (ILP, stores) to reach 8-tick budget.

