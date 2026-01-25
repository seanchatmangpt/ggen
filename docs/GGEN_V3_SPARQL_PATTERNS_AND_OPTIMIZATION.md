<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [ggen v3 SPARQL Patterns & Optimization Guide](#ggen-v3-sparql-patterns--optimization-guide)
  - [Table of Contents](#table-of-contents)
  - [SPARQL Fundamentals for Code Generation](#sparql-fundamentals-for-code-generation)
    - [SPARQL in ggen Context](#sparql-in-ggen-context)
    - [SPARQL 1.1 Features Used](#sparql-11-features-used)
  - [Core Query Patterns](#core-query-patterns)
    - [Pattern 1: Simple Entity Selection](#pattern-1-simple-entity-selection)
    - [Pattern 2: Relationships (Parent/Child)](#pattern-2-relationships-parentchild)
    - [Pattern 3: Optional Fields (OPTIONAL)](#pattern-3-optional-fields-optional)
    - [Pattern 4: Aggregation (GROUP_CONCAT)](#pattern-4-aggregation-group_concat)
    - [Pattern 5: Conditional Logic (FILTER)](#pattern-5-conditional-logic-filter)
    - [Pattern 6: String Pattern Matching (REGEX)](#pattern-6-string-pattern-matching-regex)
    - [Pattern 7: Computed Values (BIND)](#pattern-7-computed-values-bind)
    - [Pattern 8: Multiple Aggregations](#pattern-8-multiple-aggregations)
    - [Pattern 9: Subqueries for Complex Logic](#pattern-9-subqueries-for-complex-logic)
    - [Pattern 10: UNION for Polymorphism](#pattern-10-union-for-polymorphism)
  - [Query Optimization Strategies](#query-optimization-strategies)
    - [Strategy 1: Index-Friendly Queries](#strategy-1-index-friendly-queries)
    - [Strategy 2: Push Filtering Early](#strategy-2-push-filtering-early)
    - [Strategy 3: Reduce Variable Count](#strategy-3-reduce-variable-count)
    - [Strategy 4: Avoid Optional Unless Needed](#strategy-4-avoid-optional-unless-needed)
    - [Strategy 5: Use LIMIT for Pagination](#strategy-5-use-limit-for-pagination)
  - [Caching & Performance](#caching--performance)
    - [Caching Strategy](#caching-strategy)
    - [Cache Invalidation Strategy](#cache-invalidation-strategy)
    - [Performance Benchmarks](#performance-benchmarks)
  - [Advanced Query Techniques](#advanced-query-techniques)
    - [Technique 1: Recursive Relationships](#technique-1-recursive-relationships)
    - [Technique 2: Path Finding](#technique-2-path-finding)
    - [Technique 3: Property Path Filtering](#technique-3-property-path-filtering)
    - [Technique 4: CONSTRUCT for Transformation](#technique-4-construct-for-transformation)
    - [Technique 5: Dynamic WHERE Clauses](#technique-5-dynamic-where-clauses)
  - [Query Performance Benchmarks](#query-performance-benchmarks)
    - [Benchmark Suite](#benchmark-suite)
    - [Benchmark Results (v3.0 target)](#benchmark-results-v30-target)
  - [Query Best Practices](#query-best-practices)
    - [1. Write Readable Queries](#1-write-readable-queries)
    - [2. Use Meaningful Variable Names](#2-use-meaningful-variable-names)
    - [3. Comment Complex Queries](#3-comment-complex-queries)
    - [4. Leverage Prefixes](#4-leverage-prefixes)
  - [Common Query Patterns Library](#common-query-patterns-library)
    - [Query Library](#query-library)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# ggen v3 SPARQL Patterns & Optimization Guide

**Status**: COMPREHENSIVE REFERENCE
**Version**: 3.0.0-alpha
**Purpose**: SPARQL queries for code generation, query optimization, caching strategies

---

## Table of Contents

1. [SPARQL Fundamentals for Code Generation](#fundamentals)
2. [Core Query Patterns](#patterns)
3. [Query Optimization Strategies](#optimization)
4. [Caching & Performance](#caching)
5. [Advanced Query Techniques](#advanced)
6. [Query Performance Benchmarks](#benchmarks)

---

## SPARQL Fundamentals for Code Generation

### SPARQL in ggen Context

**Standard SPARQL**: Queries RDF graphs, returns results
**ggen SPARQL**: Drives template data, controls generation scope

```
Traditional SPARQL:
  SPARQL Query → Results → Display/Analysis

ggen SPARQL:
  SPARQL Query → Results → Template Variables → Generated Code
```

### SPARQL 1.1 Features Used

```sparql
✓ SELECT, CONSTRUCT, ASK, DESCRIBE
✓ WHERE patterns with multiple triple patterns
✓ FILTER expressions (>, <, =, contains, regex)
✓ GROUP BY, ORDER BY, LIMIT, OFFSET
✓ OPTIONAL patterns for nullable fields
✓ UNION for alternatives
✓ Aggregates (COUNT, SUM, GROUP_CONCAT, MAX, MIN)
✓ Subqueries for complex logic
✓ BIND for computed values
✓ Functions (STR, LANG, DATATYPE, etc.)
```

---

## Core Query Patterns

### Pattern 1: Simple Entity Selection

**Use Case**: Get all entities of a type with properties

```sparql
PREFIX ggen: <http://ggen.io/ontology/v3/ggen#>
PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>

SELECT ?name ?version ?description
WHERE {
  ?crate a ggen:Crate ;
         ggen:name ?name ;
         ggen:version ?version ;
         ggen:description ?description .
}
ORDER BY ?name
```

**Generated code uses**: `?name`, `?version`, `?description` as template variables

### Pattern 2: Relationships (Parent/Child)

**Use Case**: Get modules with their parent crate

```sparql
SELECT ?crateName ?moduleName ?modulePath
WHERE {
  ?crate a ggen:Crate ;
         ggen:name ?crateName ;
         ggen:modules ?module .
  ?module ggen:name ?moduleName ;
          ggen:path ?modulePath .
}
ORDER BY ?crateName ?moduleName
```

**Templates use**: `{{ crateName }}` and `{{ moduleName }}` to organize output by crate

### Pattern 3: Optional Fields (OPTIONAL)

**Use Case**: Get types with optional documentation

```sparql
SELECT ?typeName ?documentation
WHERE {
  ?type a ggen:Type ;
        ggen:name ?typeName .
  OPTIONAL {
    ?type ggen:documentation ?documentation .
  }
}
```

**Template**:
```jinja2
{% if documentation %}
/// {{ documentation }}
{% endif %}
pub struct {{ typeName }} { ... }
```

### Pattern 4: Aggregation (GROUP_CONCAT)

**Use Case**: Get modules with all their exports as comma-separated list

```sparql
PREFIX ggen: <http://ggen.io/ontology/v3/ggen#>

SELECT ?moduleName (GROUP_CONCAT(STR(?exportName); separator=",") as ?exports)
WHERE {
  ?module a ggen:Module ;
          ggen:name ?moduleName ;
          ggen:exports ?export .
  ?export ggen:name ?exportName .
}
GROUP BY ?moduleName
```

**Template Output**:
```jinja2
{% for export in exports.split(",") %}
pub use self::{{ export }};
{% endfor %}
```

### Pattern 5: Conditional Logic (FILTER)

**Use Case**: Get only public modules

```sparql
SELECT ?moduleName
WHERE {
  ?module a ggen:Module ;
          ggen:name ?moduleName ;
          ggen:isPublic ?isPublic .
  FILTER(?isPublic = true)
}
```

### Pattern 6: String Pattern Matching (REGEX)

**Use Case**: Get CLI commands for a specific noun

```sparql
SELECT ?verb ?description
WHERE {
  ?cmd a ggencli:CliCommand ;
       ggencli:noun "project" ;
       ggencli:verb ?verb ;
       ggencli:description ?description .
  FILTER(REGEX(?verb, "^gen|new"))  # Match gen, new, genesis
}
```

### Pattern 7: Computed Values (BIND)

**Use Case**: Generate field visibility declarations

```sparql
SELECT ?fieldName ?visibility ?rustVisibility
WHERE {
  ?field ggen:name ?fieldName ;
         ggen:visibility ?visibility .
  BIND(
    IF(?visibility = "pub", "pub ",
    IF(?visibility = "pub(crate)", "pub(crate) ",
    ""))
    AS ?rustVisibility
  )
}
```

**Template**:
```jinja2
{{ rustVisibility }}{{ fieldName }}: String,
```

### Pattern 8: Multiple Aggregations

**Use Case**: Count fields, document, and list exports per type

```sparql
SELECT ?typeName
       (COUNT(?field) as ?fieldCount)
       (GROUP_CONCAT(?fieldName) as ?fieldNames)
       ?documentation
WHERE {
  ?type a ggen:Type ;
        ggen:name ?typeName ;
        ggen:fields ?field ;
        ggen:documentation ?documentation .
  ?field ggen:name ?fieldName .
}
GROUP BY ?typeName ?documentation
ORDER BY DESC(?fieldCount)
```

### Pattern 9: Subqueries for Complex Logic

**Use Case**: Get types with more than 5 fields

```sparql
SELECT ?typeName
WHERE {
  {
    SELECT ?typeName (COUNT(?field) as ?fieldCount)
    WHERE {
      ?type a ggen:Type ;
            ggen:name ?typeName ;
            ggen:fields ?field .
    }
    GROUP BY ?typeName
  }
  FILTER(?fieldCount > 5)
}
```

### Pattern 10: UNION for Polymorphism

**Use Case**: Get all exports (both types and functions)

```sparql
SELECT ?name ?kind
WHERE {
  {
    ?item a ggen:Type ;
          ggen:name ?name .
    BIND("Type" AS ?kind)
  }
  UNION
  {
    ?item a ggen:Function ;
          ggen:name ?name .
    BIND("Function" AS ?kind)
  }
}
```

---

## Query Optimization Strategies

### Strategy 1: Index-Friendly Queries

**Bad** (full scan):
```sparql
SELECT ?name
WHERE {
  ?x ?p "user-core" .  # Predicate is variable
}
```

**Good** (indexed predicate):
```sparql
SELECT ?name
WHERE {
  ?crate ggen:name "user-core" .  # Known predicate
}
```

### Strategy 2: Push Filtering Early

**Bad** (filter after aggregation):
```sparql
SELECT ?typeName (COUNT(?field) as ?fieldCount)
WHERE {
  ?type ggen:fields ?field .
}
GROUP BY ?typeName
FILTER(?fieldCount > 5)  # Filters after grouping
```

**Good** (filter inline):
```sparql
SELECT ?typeName
WHERE {
  ?type a ggen:Type ;
        ggen:fields ?field ;
        ggen:fields ?field2 ;
        ggen:fields ?field3 ;
        ggen:fields ?field4 ;
        ggen:fields ?field5 ;
        ggen:fields ?field6 .  # Inline filtering
  FILTER(?field != ?field2 && ?field2 != ?field3)  # Ensure 6 distinct
}
```

### Strategy 3: Reduce Variable Count

**Bad** (many variables):
```sparql
SELECT ?a ?b ?c ?d ?e ?f ?g ?h ?i ?j
WHERE {
  ?a ggen:links ?b .
  ?b ggen:links ?c .
  ?c ggen:links ?d .
  ...
}
```

**Good** (only needed variables):
```sparql
SELECT ?a ?j
WHERE {
  ?a ggen:links ?b .
  ?b ggen:links ?c .
  ...
  ?i ggen:links ?j .
}
```

### Strategy 4: Avoid Optional Unless Needed

**Bad**:
```sparql
SELECT ?name ?doc
WHERE {
  ?type ggen:name ?name .
  OPTIONAL { ?type ggen:documentation ?doc . }  # Often empty
}
```

**Good** (if documentation is sparse):
```sparql
SELECT ?name ?doc
WHERE {
  ?type ggen:name ?name ;
        ggen:documentation ?doc .
}
```

### Strategy 5: Use LIMIT for Pagination

```sparql
# Get all crates, page 2 (10 per page)
SELECT ?name
WHERE {
  ?crate a ggen:Crate ;
         ggen:name ?name .
}
ORDER BY ?name
LIMIT 10
OFFSET 10  # Skip first 10
```

---

## Caching & Performance

### Caching Strategy

```rust
pub struct QueryCache {
    cache: Arc<Mutex<HashMap<String, QueryResult>>>,
    ttl: Duration,
}

impl QueryCache {
    pub fn execute_cached(&self, query: &str) -> Result<QueryResult> {
        // 1. Check if cached
        if let Some(result) = self.get(query) {
            if !result.is_expired() {
                return Ok(result);
            }
        }

        // 2. Execute query
        let result = self.execute(query)?;

        // 3. Cache with TTL
        self.set(query, result.clone());

        Ok(result)
    }
}
```

### Cache Invalidation Strategy

```
On Ontology Change:
  1. Hash ontology before query
  2. After any modification:
     - Clear query cache for affected entity types
     - Recompute affected queries
     - Update dependent templates

Example:
  - Modify ggen:Field "user.email"
  - Invalidate: User type queries
  - Re-execute: All projections using User
  - Regenerate: User-related templates
```

### Performance Benchmarks

| Query Type | Ontology Size | Execution Time | Cache Hit |
|-----------|---------------|----------------|-----------|
| Single entity | 10k triples | <5ms | <1ms |
| Entity + relations | 10k triples | 10-20ms | <1ms |
| Aggregation | 10k triples | 30-50ms | <1ms |
| Complex subquery | 10k triples | 50-100ms | <1ms |
| Full system scan | 100k triples | 100-200ms | <1ms |

---

## Advanced Query Techniques

### Technique 1: Recursive Relationships

**Use Case**: Get all transitive dependencies

```sparql
PREFIX ggen: <http://ggen.io/ontology/v3/ggen#>

SELECT ?root ?dependency (COUNT(?link) as ?depth)
WHERE {
  ?root a ggen:Crate ;
        ggen:name "ggen-cli" .
  ?root ggen:dependencies+ ?dependency .  # + = one or more
}
GROUP BY ?root ?dependency
ORDER BY ?depth
```

### Technique 2: Path Finding

**Use Case**: Find shortest path of dependencies

```sparql
SELECT ?path
WHERE {
  ?crate1 ggen:name "ggen-cli" .
  ?crateN ggen:name "ggen-utils" .
  ?crate1 ggen:dependencies ?step1 .
  ?step1 ggen:dependencies ?step2 .
  ?step2 ggen:dependencies ?crateN .
  BIND(CONCAT("cli -> ", ?step1, " -> ", ?step2, " -> utils") as ?path)
}
```

### Technique 3: Property Path Filtering

**Use Case**: Complex entity selection

```sparql
SELECT ?publicTypes
WHERE {
  ?crate a ggen:Crate ;
         ggen:name "ggen-core" ;
         ggen:modules/ggen:exports ?type .
  ?type ggen:isPublic true .
  BIND(STR(?type) AS ?publicTypes)
}
```

### Technique 4: CONSTRUCT for Transformation

**Use Case**: Transform ontology structure

```sparql
CONSTRUCT {
  ?crate ggen:publicExports ?export .
}
WHERE {
  ?crate a ggen:Crate ;
         ggen:modules ?module .
  ?module ggen:exports ?export ;
          ggen:isPublic true .
}
```

Result: New RDF graph with flattened structure for easier querying.

### Technique 5: Dynamic WHERE Clauses

**Use Case**: Query based on user input

```rust
// User provides criteria
let criteria = vec![
  ("crate_name", "ggen-core"),
  ("is_public", "true"),
  ("has_docs", "true"),
];

// Build query dynamically
let mut query = String::from("SELECT ?name WHERE { ?entity a ggen:Type ");

for (key, value) in criteria {
  match key {
    "crate_name" => {
      query.push_str(&format!("; ggen:module [ggen:parent [ggen:name \"{}\"]] ", value));
    }
    "is_public" => {
      query.push_str(&format!("; ggen:isPublic {} ", value));
    }
    "has_docs" => {
      query.push_str("; ggen:documentation ?doc . FILTER(BOUND(?doc)) ");
    }
    _ => {}
  }
}

query.push_str("}");

let results = graph.query(&query)?;
```

---

## Query Performance Benchmarks

### Benchmark Suite

```rust
#[bench]
fn bench_simple_select(b: &mut Bencher) {
    let graph = load_test_ontology();
    b.iter(|| {
        graph.query("SELECT ?name WHERE { ?type ggen:name ?name }")
    });
}

#[bench]
fn bench_aggregation(b: &mut Bencher) {
    let graph = load_test_ontology();
    b.iter(|| {
        graph.query(
            "SELECT ?module (COUNT(?field) as ?fields)
             WHERE { ?type ggen:fields ?field }
             GROUP BY ?module"
        )
    });
}

#[bench]
fn bench_complex_subquery(b: &mut Bencher) {
    let graph = load_test_ontology();
    b.iter(|| {
        graph.query(
            "SELECT ?name WHERE {
             { SELECT ?name (COUNT(?f) as ?c) WHERE { ?t ggen:fields ?f } GROUP BY ?name }
             FILTER(?c > 10)
             }"
        )
    });
}

#[bench]
fn bench_with_cache(b: &mut Bencher) {
    let cache = QueryCache::new();
    b.iter(|| {
        cache.execute_cached("SELECT ?name WHERE { ?type ggen:name ?name }")
    });
}
```

### Benchmark Results (v3.0 target)

```
test bench_simple_select           ... bench:       4,562 ns/iter
test bench_aggregation             ... bench:      25,341 ns/iter
test bench_complex_subquery        ... bench:      89,752 ns/iter
test bench_with_cache              ... bench:         821 ns/iter (cache hit)
test bench_full_generation_pipeline ... bench:  1,234,567 ns/iter (<2s total)
```

---

## Query Best Practices

### 1. Write Readable Queries

```sparql
# ✓ Good: Clear structure
SELECT ?name ?version
WHERE {
  ?crate a ggen:Crate ;
         ggen:name ?name ;
         ggen:version ?version ;
         ggen:isPublic true .
}

# ✗ Bad: Hard to read
SELECT ?a ?b WHERE { ?x a ggen:Crate . ?x ggen:name ?a . ?x ggen:version ?b . ?x ggen:isPublic true . }
```

### 2. Use Meaningful Variable Names

```sparql
# ✓ Good
SELECT ?crateName ?version WHERE { ?crate ggen:name ?crateName }

# ✗ Bad
SELECT ?x ?y WHERE { ?z ggen:name ?x }
```

### 3. Comment Complex Queries

```sparql
# Get all public types in public modules
SELECT ?typeName
WHERE {
  # Find public crates
  ?crate a ggen:Crate ;
         ggen:isPublic true ;
         ggen:modules ?module .

  # Find public modules
  ?module ggen:isPublic true ;
          ggen:exports ?type .

  # Get type names
  ?type ggen:name ?typeName .
}
```

### 4. Leverage Prefixes

```sparql
PREFIX ggen: <http://ggen.io/ontology/v3/ggen#>
PREFIX ggencli: <http://ggen.io/ontology/v3/cli#>
PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>

SELECT ?cliName ?description
WHERE {
  ?cmd a ggencli:CliCommand ;
       ggencli:noun ?cliName ;
       ggencli:description ?description .
}
```

---

## Common Query Patterns Library

### Query Library

```sparql
# Get all modules in a crate
ggen:query:modules_in_crate
  SELECT ?moduleName
  WHERE {
    ?crate ggen:name "crate_name" ;
           ggen:modules ?module .
    ?module ggen:name ?moduleName .
  }

# Get all fields in a type
ggen:query:fields_in_type
  SELECT ?fieldName ?fieldType ?isRequired
  WHERE {
    ?type ggen:name "TypeName" ;
          ggen:fields ?field .
    ?field ggen:name ?fieldName ;
           ggen:type ?fieldType ;
           ggen:isRequired ?isRequired .
  }

# Get all CLI commands
ggen:query:all_commands
  SELECT ?noun ?verb ?description
  WHERE {
    ?cmd a ggencli:CliCommand ;
         ggencli:noun ?noun ;
         ggencli:verb ?verb ;
         ggencli:description ?description .
  }
  ORDER BY ?noun ?verb

# Get guards for a package
ggen:query:package_guards
  SELECT ?guardName ?description
  WHERE {
    ?package ggenmp:name "package_name" ;
             ggenmp:guards ?guard .
    ?guard ggen:name ?guardName ;
           ggen:description ?description .
  }
```

---

**Document Version**: 1.0
**Created**: November 17, 2025
**SPARQL Version**: 1.1
**Optimization Target**: <100ms per query average
