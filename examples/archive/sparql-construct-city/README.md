# SPARQL CONSTRUCT: 8 Bleeding Edge Best Practices

This example demonstrates **8 bleeding edge SPARQL CONSTRUCT patterns** for innovative feature engineering, graph enrichment, and data transformation. It uses **Citty CLI testing framework** to validate all patterns with clean, idiomatic tests.

## Overview

### What is SPARQL CONSTRUCT?

**CONSTRUCT** is the transformation layer of SPARQL. It reads from a source graph and projects it into a new derived graph.

```sparql
CONSTRUCT {
  # Define new graph pattern
  ?city a :EnrichedCity ;
    :density ?computedDensity ;
    :type ?calculatedType .
}
WHERE {
  # Match source patterns
  ?city :population ?pop ;
    :area ?area .

  # Compute and transform
  BIND(?pop / ?area AS ?computedDensity)
}
```

### The 8 Patterns

| # | Pattern | Innovation | Use Case |
|---|---------|-----------|----------|
| 1 | **OPTIONAL** | Safe property enrichment | Add attributes only when available |
| 2 | **BIND** | Computed values | Derive new attributes from existing data |
| 3 | **FILTER** | Conditional generation | Focus output on important entities |
| 4 | **UNION** | Polymorphic matching | Unify heterogeneous entity types |
| 5 | **GROUP_CONCAT** | Aggregation | Collect related values into summaries |
| 6 | **VALUES** | Parameterization | Process dynamic lists of entities |
| 7 | **EXISTS/NOT EXISTS** | Graph logic | Sophisticated pattern matching |
| 8 | **Property Paths** | Transitive navigation | Follow relationships at any depth |

## Quick Start

### 1. Install Dependencies

```bash
npm install
```

### 2. Run Tests

```bash
# All tests
npm test

# Watch mode
npm run test:watch

# With UI
npm run test:ui

# Coverage report
npm run test:coverage
```

### 3. Execute Individual Queries

```bash
# Pattern 1: OPTIONAL enrichment
npm run sparql:query1

# Pattern 2: BIND computed values
npm run sparql:query2

# ... Pattern 3-8
npm run sparql:query{3-8}

# All queries
npm run sparql:all
```

## Detailed Patterns

### Pattern 1: OPTIONAL - Safe Enrichment

**Problem**: How do you add optional properties without NULLs?

**Solution**: OPTIONAL + BIND to create boolean flags

```sparql
OPTIONAL {
  ?mayor city:mayorsOf ?city ;
    foaf:name ?mayorName ;
    FILTER NOT EXISTS { ?mayor city:endYear ?end }
}

BIND(BOUND(?mayorName) AS ?hasMayorInfo)
```

**Benefits**:
- No missing values break the query
- Explicit indication of data presence
- Enables conditional downstream processing
- Clean, predictable outputs

**Test**: `describe('1. CONSTRUCT with OPTIONAL')`

---

### Pattern 2: BIND - Computed Values

**Problem**: How do you derive new attributes from existing data?

**Solution**: BIND expressions for computation

```sparql
BIND(?pop / ?area AS ?density)
BIND(
  IF(?pop > 1000000, "Major",
    IF(?pop > 100000, "Large",
      IF(?pop > 50000, "Medium", "Small")))
  AS ?sizeCategory
)
```

**Benefits**:
- Type-safe computation
- Reduces downstream complexity
- Enables optimization (computed once, reused many times)
- Clean separation of logic and data

**Test**: `describe('2. BIND for Computed Values')`

---

### Pattern 3: FILTER - Selective Output

**Problem**: How do you focus the graph on important entities?

**Solution**: FILTER combined with aggregation

```sparql
WHERE {
  ?city a city:City ;
    city:isMajor true .

  FILTER (?pop > 500000)

  {
    SELECT ?city (COUNT(?landmark) AS ?landmarkCount)
    WHERE { ?landmark city:locatedIn ?city . }
    GROUP BY ?city
  }
}
```

**Benefits**:
- Smaller output graphs
- Better downstream performance
- Clear intent (what matters)
- Enables focused analytics

**Test**: `describe('3. FILTER Expressions')`

---

### Pattern 4: UNION - Polymorphic Matching

**Problem**: How do you unify heterogeneous entity types?

**Solution**: UNION patterns with normalization

```sparql
{
  ?poi a city:Landmark ;
    rdfs:label ?name ;
    city:locatedIn ?city ;
    city:type ?poiType .
}
UNION
{
  ?poi a city:Event ;
    rdfs:label ?name ;
    city:heldIn ?city .
  BIND("Event" AS ?poiType)
}
```

**Benefits**:
- Single query for multiple entity types
- Schema flexibility
- Enables federated reasoning
- Clean polymorphism

**Test**: `describe('4. UNION for Alternative Patterns')`

---

### Pattern 5: GROUP_CONCAT - Aggregation

**Problem**: How do you summarize without losing detail?

**Solution**: GROUP_CONCAT for structured aggregation

```sparql
{
  SELECT ?city (GROUP_CONCAT(DISTINCT ?neighborName; separator=" | ") AS ?neighborsList)
  WHERE {
    ?city city:hasNeighbor ?neighbor .
    ?neighbor rdfs:label ?neighborName .
  }
  GROUP BY ?city
}
```

**Benefits**:
- Summarized output without data loss
- Human-readable formats
- Reduces output graph size
- Enables reporting

**Test**: `describe('5. GROUP_CONCAT Aggregation')`

---

### Pattern 6: VALUES - Parameterization

**Problem**: How do you make queries reusable with dynamic inputs?

**Solution**: VALUES clause for parameter lists

```sparql
WHERE {
  VALUES ?city {
    city:sf
    city:oakland
    city:berkeley
  }

  ?city a city:City ;
    rdfs:label ?name ;
    city:population ?pop .
  # Rest of query...
}
```

**Benefits**:
- Parameterized query templates
- Clean separation of logic and data
- Reusable patterns
- Easy to compose from code

**Test**: `describe('6. VALUES for Dynamic Parameters')`

---

### Pattern 7: EXISTS / NOT EXISTS - Graph Logic

**Problem**: How do you encode sophisticated pattern matching?

**Solution**: EXISTS/NOT EXISTS for logical operators

```sparql
BIND(
  IF(EXISTS {?landmark city:locatedIn ?city}, true, false)
  AS ?hasLandmarks
)

BIND(
  IF(?hasLandmarks && ?hasMayors, "Historic",
    IF(?hasLandmarks, "Unmapped", "Unknown"))
  AS ?characterType
)
```

**Benefits**:
- Sophisticated logical patterns
- Avoids expensive JOINs
- Clear intent
- Enables decision trees

**Test**: `describe('7. FILTER EXISTS / NOT EXISTS')`

---

### Pattern 8: Property Paths - Transitive Navigation

**Problem**: How do you follow paths of unknown depth?

**Solution**: Property paths with operators

```sparql
WHERE {
  ?startCity a city:City ;
    rdfs:label ?startName ;
    city:hasNeighbor+ ?reachableCity .

  ?landmark city:locatedIn ?reachableCity ;
    rdfs:label ?landmarkName ;
    city:type ?landmarkType .
}
```

**Operators**:
- `?prop?` - Zero or one (optional)
- `?prop*` - Zero or more (zero-or-more)
- `?prop+` - One or more (one-or-more)
- `?prop1|?prop2` - Alternatives (union)

**Benefits**:
- Graph traversal without recursion
- Closure computation
- Path finding
- Transitive reasoning

**Test**: `describe('8. Property Paths')`

---

## Architecture

### File Structure

```
sparql-construct-city/
├── README.md                        # This file
├── CONSTRUCT-BEST-PRACTICES.md      # Detailed pattern explanations
├── data.ttl                         # City ontology (5 cities, 10 landmarks, 4 events)
├── queries.sparql                   # All 8 CONSTRUCT queries
├── sparql-construct.test.js         # Citty-based tests (70+ test cases)
├── package.json                     # Dependencies
├── vitest.config.js                 # Vitest + Citty config
└── scripts/
    ├── run-query.js                 # Execute single query
    └── run-all-queries.js           # Execute all queries
```

### Data Model

**Ontology Classes**:
- `city:City` - Cities (SF, Oakland, Berkeley, NYC, Newark)
- `city:Landmark` - Points of interest (Golden Gate Bridge, Statue of Liberty, etc.)
- `city:Event` - Events (Tech Summit, Marathon)
- `foaf:Person` - Mayors and notable people

**Key Properties**:
- `city:population` - Integer
- `city:area` - Float (square kilometers)
- `city:founded` - Integer (year)
- `city:hasNeighbor` - Relationships between cities
- `city:locatedIn` / `city:heldIn` - Location relationships

### Test Organization

Tests are organized by pattern:
1. **Unit Tests** - Individual pattern correctness
2. **Integration Tests** - Query compilation and data validation
3. **Performance Tests** - Computational behavior
4. **Snapshot Tests** - Output consistency

### Citty Integration

```javascript
// Citty provides:
// - runCitty([...args]) - Execute CLI commands
// - scenario(...) - Multi-step test scenarios
// - Fluent assertions - .expectSuccess(), .expectOutput()
// - Snapshot support - Compare against golden outputs

it('enriches cities with optional data', async () => {
  const result = await runCitty(['sparql', 'query', '1'])
    .expectSuccess()
    .expectOutput(/CityProfile/);

  expect(result.stdout).toContain('CONSTRUCT');
});
```

---

## Innovation Applications

### Knowledge Graph Enrichment
```sparql
# Compute embeddings
BIND(hash(CONCAT(?name, ?pop)) AS ?embedding)

# Add metadata
BIND(NOW() AS ?enrichedAt)
```

### Feature Engineering for ML
```sparql
CONSTRUCT {
  ?city :mlFeatures [
    :density ?density ;
    :age ?age ;
    :connectivity ?neighborCount ;
    :culturalValue ?landmarkCount
  ] .
}
```

### Data Quality Assessment
```sparql
CONSTRUCT {
  ?city :qualityMetric [
    :completeness ?complete ;
    :consistency ?consistent ;
    :confidence ?confidence
  ] .
}
WHERE {
  ?city a city:City .
  BIND(EXISTS {?city city:population ?p} AS ?complete)
  BIND(EXISTS {?city :area ?a} AS ?consistent)
}
```

### Multi-view Synthesis
```sparql
# Create different normalized views from single source
CONSTRUCT {
  ?city a :CityForReporting ;
    :name ?name ;
    :summary ?summary ;
    :metrics [...]
}
WHERE {
  # Transform based on purpose
  BIND("Purpose-optimized view" AS ?summary)
}
```

---

## Performance Characteristics

| Pattern | Time | Memory | Use When |
|---------|------|--------|----------|
| OPTIONAL | O(n) | O(n) | Many optionals |
| BIND | O(n) | O(1) | Pre-compute values |
| FILTER | O(n log n) | O(1) | Focus output |
| UNION | O(n + m) | O(n+m) | Combine patterns |
| GROUP_CONCAT | O(n log n) | O(m) | Aggregate |
| VALUES | O(1) | O(k) | k parameters |
| EXISTS | O(n) | O(1) | Check presence |
| Property Paths | O(V+E) | O(V) | Graph traversal |

---

## Real-World Use Cases

### 1. **Knowledge Graph Enrichment** (LinkedIn, Google)
```sparql
# Add computed attributes to profiles
CONSTRUCT {
  ?person :influenceScore ?score ;
    :networkSize ?size ;
    :expertise ?field .
}
```

### 2. **Data Integration** (FDA, Public Health)
```sparql
# Combine data from multiple sources
CONSTRUCT {
  ?patient :unifiedRecord [
    :providers ?providers ;  # GROUP_CONCAT
    :conditions ?conditions ;
    :riskScore ?score       # BIND
  ] .
}
```

### 3. **Recommendation Systems** (Netflix, Spotify)
```sparql
# Create recommendation features
CONSTRUCT {
  ?user :similarTo ?otherUser ;
    :recommendedItem ?item ;
    :relevanceScore ?score  # BIND
}
WHERE {
  # UNION to match different similarity patterns
}
```

### 4. **Compliance and Audit** (Regulatory)
```sparql
# Generate compliance evidence
CONSTRUCT {
  ?organization :auditTrail [
    :modifications ?changes ;  # GROUP_CONCAT
    :violations ?violations ;   # FILTER
    :status ?status            # BIND
  ] .
}
```

---

## Advanced Techniques

### Recursive Patterns with Property Paths
```sparql
# Find all descendants (recursive)
?ancestor (foaf:knows|foaf:knows/foaf:knows+)* ?descendant .
```

### Conditional CONSTRUCT
```sparql
CONSTRUCT {
  ?city a ?type ;
    ?predicate ?value .
}
WHERE {
  BIND(IF(?pop > 1M, city:Major, city:Minor) AS ?type)
  BIND(IF(?hasLandmarks, city:culturalValue, city:commercialValue) AS ?predicate)
}
```

### Materialized Views
```sparql
# Pre-compute and store results
INSERT {
  GRAPH <http://example.org/views/city-enriched> {
    [CONSTRUCT results]
  }
}
```

---

## Debugging Tips

### 1. Check BIND Expressions
```sparql
# Use explicit FILTER to debug BIND
BIND(?pop / ?area AS ?density)
FILTER(?density > 100)
```

### 2. Verify OPTIONAL Results
```sparql
# Make optional property visible
OPTIONAL { ?mayor foaf:name ?mayorName }
BIND(BOUND(?mayorName) AS ?hasMayorInfo)
# Then you can see if BOUND() works
```

### 3. Test FILTER Logic
```sparql
# Break complex FILTER into parts
FILTER(?hasMajor)
FILTER(?pop > 500000)
# vs
FILTER(?hasMajor && ?pop > 500000)
```

### 4. Validate UNION Results
```sparql
# Add type info to distinguish branches
BIND("Landmark" AS ?source)
UNION
BIND("Event" AS ?source)
```

---

## Further Reading

- [SPARQL 1.1 Specification](https://www.w3.org/TR/sparql11-query/)
- [Wikidata Query Examples](https://query.wikidata.org/)
- [DBpedia Tutorial](https://wiki.dbpedia.org/develop/datasets)
- [Citty Documentation](https://github.com/seanchatmangpt/citty-test-utils)

---

## Contributing

Add new patterns or examples:

1. Add query to `queries.sparql`
2. Add test cases to `sparql-construct.test.js`
3. Document in `CONSTRUCT-BEST-PRACTICES.md`
4. Run `npm test` to validate

---

## License

MIT - See LICENSE in parent ggen project
