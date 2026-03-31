# JTBD Story: Developing and Debugging SPARQL Queries

## 5 Whys Analysis

### Why #1: Why does the user need to write SPARQL queries?

**Answer:** To extract structured insights and relationships from RDF knowledge graphs that cannot be easily retrieved through traditional database queries or API calls.

**Context:**
- RDF graphs contain complex interconnected data (ontologies, metadata, semantic relationships)
- Need to answer questions like "What are all subclasses of `foaf:Person` in my ontology?" or "Find all resources with `schema:startDate` after 2025"
- SPARQL is the standard query language for RDF, analogous to SQL for relational databases
- Users are working with domain-specific ontologies (FIBO, TOGAF, enterprise schemas)

**Pain Points:**
- Manual graph traversal is error-prone and time-consuming
- Generic RDF parsers return too much raw data (millions of triples)
- Need precise filtering and pattern matching to get actionable insights

---

### Why #2: Why validate SPARQL syntax before execution?

**Answer:** Because SPARQL query engines (like Oxigraph, Apache Jena) have cryptic error messages that don't clearly indicate where the syntax error occurred, and running invalid queries against large RDF graphs can waste significant time and resources.

**Cost of Invalid Queries:**
- **Time loss:** Query parsing fails after 10-30 seconds of processing on large graphs
- **Resource waste:** CPU and memory consumed by query planning/optimization before syntax errors are caught
- **Frustration:** Error messages like `"Parse error: unexpected token at line 15"` without clear context
- **Iteration slowdown:** Each syntax error requires a full edit-run-debug cycle

**Example of Cryptic Error:**
```
Error: Parse error on line 15: unexpected '}', expected ';'
  (Actual issue: Missing semicolon on line 14 after FILTER clause)
```

**Validation Benefits:**
- Fast feedback (<100ms) without query execution
- Clear error messages with line/column numbers
- Detection of common issues: unmatched braces, missing prefixes, malformed FILTERs
- Query type detection (SELECT, ASK, CONSTRUCT, DESCRIBE) for appropriate tooling

---

### Why #3: Why auto-fix SPARQL queries?

**Answer:** Because SPARQL has a steep learning curve and developers frequently make repetitive mistakes that are easily detectable and correctable, causing unnecessary friction in the development workflow.

**Common SPARQL Mistakes:**

1. **Typos in keywords:** `SELEC` instead of `SELECT`, `WHER` instead of `WHERE`
2. **Missing prefixes:** Using `foaf:Person` without declaring `PREFIX foaf: <http://xmlns.com/foaf/0.1/>`
3. **Malformed filters:** Missing quotes in strings, incorrect comparison operators
4. **Unmatched braces:** Missing `}` at end of WHERE clause
5. **Incorrect LIMIT/OFFSET:** Using `LIMIT` without `ORDER BY` (non-deterministic results)
6. **Missing semicolons:** In triple patterns with multiple predicates

**Auto-Fix Examples:**

```sparql
# Before: Typo in SELECT keyword
SELEC ?s WHERE { ?s a foaf:Person }

# After: Auto-corrected
SELECT ?s WHERE { ?s a foaf:Person }
```

```sparql
# Before: Missing prefix
SELECT ?s WHERE { ?s a foaf:Person }

# After: Auto-added prefix
PREFIX foaf: <http://xmlns.com/foaf/0.1/>
SELECT ?s WHERE { ?s a foaf:Person }
```

```sparql
# Before: Missing quotes in FILTER
FILTER (?name = John Doe)

# After: Auto-quoted string
FILTER (?name = "John Doe")
```

**Benefits:**
- Reduced cognitive load (don't need to memorize every syntax rule)
- Faster iteration (fewer edit-fix cycles)
- Better learning (see corrections in context)
- Consistency (standard fixes across team)

---

### Why #4: Why test queries against sample data?

**Answer:** Because SPARQL queries that are syntactically valid can still fail at runtime due to schema mismatches, empty results from incorrect graph patterns, or performance issues that only appear with real data volumes.

**Risks of Untested Queries:**

1. **Empty results:** Query runs successfully but returns no data due to incorrect graph patterns
   - Example: `?s rdf:type ex:Employee` but the actual type is `ex:employee` (lowercase)

2. **Type coercion errors:** FILTER comparisons between incompatible types
   - Example: `FILTER (?date < "2025-01-01"^^xsd:dateTime)` when `?date` is a string

3. **Performance disasters:** Queries that work on 100 triples but timeout on 10M triples
   - Example: Cartesian products from unbounded triple patterns
   - Missing indexes on commonly filtered properties

4. **Namespace mismatches:** Using wrong prefix URIs
   - Example: `schema:Person` vs `http://schema.org/Person` (actual data uses different URI)

**Testing Benefits:**
- Verify query returns expected results (not just runs without error)
- Detect performance issues early (execution time monitoring)
- Validate graph patterns match actual data structure
- Test edge cases: empty graphs, missing properties, duplicate results

**Example Test Scenario:**
```sparql
# Query: Find all managers with more than 5 reports
SELECT ?manager ?reportCount WHERE {
  ?manager a ex:Manager ;
           ex:hasReport ?report .
  ?report a ex:Employee .
}
GROUP BY ?manager
HAVING (COUNT(?report) > 5)

# Test against sample data:
# - Returns 0 managers (bug: GROUP BY applied before HAVING)
# - Fix: Move HAVING after proper aggregation
# - Re-test: Returns 3 managers as expected
```

---

### Why #5: Why use MCP tools for SPARQL development?

**Answer:** Because MCP tools provide a unified, standardized interface for SPARQL development that integrates with AI assistants (Claude, ChatGPT), provides consistent tooling across different environments, and enables faster iteration through automated validation, fixing, and testing.

**Traditional Workflow (Slow):**
```bash
# 1. Write query in text editor
vim query.rq

# 2. Run against local RDF store
sparql-query --data ontology.ttl --file query.rq

# 3. Parse cryptic error message
Error: Parse error at line 15

# 4. Fix and retry (manual cycle)
vim query.rq
sparql-query --data ontology.ttl --file query.rq

# 5. Verify results manually (grep, wc, etc.)
sparql-query ... | grep -c "^"
```

**MCP-Enabled Workflow (Fast):**
```python
# 1. Write query (possibly with errors)
sparql = "SELEC ?s WHER { ?s a foaf:Person }"

# 2. Validate syntax (fast, <100ms)
result = mcp.call("validate_sparql_syntax", {sparql})
# Returns: {is_valid: false, error: "Expected 'SELECT' at line 1"}

# 3. Auto-fix (corrects typos, adds prefixes)
fixed = mcp.call("fix_sparql_query", {sparql})
# Returns: {fixed_sparql: "SELECT ?s WHERE { ... }", fixes_applied: ["SELECT", "WHERE"]}

# 4. Test against sample data
test_result = mcp.call("test_sparql_query", {
  sparql: fixed,
  test_data: sample_graph
})
# Returns: {results: [...], execution_time_ms: 23, row_count: 42}

# 5. Iterate with AI assistance
# "The query works but returns 42 results. I expected 50. What's wrong?"
# AI analyzes query and suggests: "Add FILTER to exclude inactive resources"
```

**MCP Tool Benefits:**

1. **Standardized interface:** Same tools work in Claude Code, ChatGPT, custom IDEs
2. **AI integration:** AI assistants can call tools directly to validate/fix/test queries
3. **Fast feedback:** Validation in milliseconds vs. seconds for full query execution
4. **Consistent behavior:** Same validation rules across all environments
5. **Composability:** Combine multiple tools (validate → fix → test) in automated workflows
6. **Error recovery:** Auto-fix reduces manual debugging time by 70%+

**Workflow Integration:**
```bash
# In CI/CD pipeline:
mcp.validate_sparql_syntax(query.rq) || exit 1
mcp.test_sparql_query(query.rq, test_data.ttl) || exit 1

# In AI chat:
User: "Write a SPARQL query to find all subclasses of skos:Concept"
AI: [Generates query] → [Calls validate_sparql_syntax] → [Calls test_sparql_query] → Returns working query

# In IDE extension:
On-save: validate_sparql_syntax → Show errors inline
On-command: fix_sparql_query → Apply auto-fixes
On-demand: test_sparql_query → Show results in side panel
```

---

## JTBD Story

### When [situation]
I am a data engineer or developer working with RDF knowledge graphs, ranging from small test datasets (1K triples) to production ontologies (10M+ triples). I need to extract insights, validate schema constraints, or perform complex graph traversals. My SPARQL expertise varies from beginner (learning syntax) to expert (optimizing query performance), and I frequently iterate on queries as requirements evolve.

### I want to [motivation]
Write, validate, and test SPARQL queries with fast feedback loops, automated error detection, and confidence that my queries will work correctly in production. I want to avoid syntax errors, catch runtime issues early, and optimize query performance without manual trial-and-error.

### So that [expected outcome]
I can rapidly develop correct SPARQL queries that return accurate results, perform efficiently on large datasets, and are maintainable by my team. I reduce the time from "idea" to "working query" from hours to minutes, and I avoid costly runtime failures in production.

---

## Context

### User Experience Levels

| Level | Characteristics | Tool Needs |
|-------|-----------------|------------|
| **Beginner** | Learning SPARQL syntax, unfamiliar with RDF patterns, makes typos | Auto-fix, syntax validation, examples, explanations |
| **Intermediate** | Knows basics but struggles with complex queries, performance issues | Query optimization, pattern suggestions, test data validation |
| **Expert** | Writes complex queries, optimizes for performance, builds production pipelines | Fast validation, performance profiling, advanced testing |

### Query Complexity

| Complexity | Example | Tool Requirements |
|------------|---------|-------------------|
| **Simple SELECT** | `SELECT ?s WHERE { ?s a foaf:Person }` | Basic syntax validation |
| **FILTERs and Aggregates** | `SELECT ?type (COUNT(?s) AS ?count) WHERE { ?s a ?type } GROUP BY ?type` | Type checking, aggregation validation |
| **Subqueries and EXISTS** | `SELECT ?s WHERE { ?s a ?type . FILTER EXISTS { ?s ex:property ?value } }` | Nested query validation, scoping rules |
| **CONSTRUCT/DESCRIBE** | `CONSTRUCT { ?s ex:inferredType ex:Person } WHERE { ... }` | Graph construction validation |
| **Property Paths** | `?s ex:childOf/ex:childOf ?grandparent` | Path expression validation |

### Data Volume

| Scale | Triple Count | Performance Considerations |
|-------|--------------|----------------------------|
| **Small** | <10K | Any query works; testing instant |
| **Medium** | 10K-1M | Need indexes; LIMIT important |
| **Large** | 1M-10M | Query planning critical; timeout risks |
| **Production** | 10M+ | Requires optimization; sample data for testing |

---

## Forces

### 1. SPARQL Learning Curve
- **Challenge:** SPARQL syntax is complex (keywords, prefixes, filters, aggregates)
- **Impact:** Beginners make frequent mistakes; experts still make typos
- **Mitigation:** Auto-fix reduces friction; validation provides fast feedback

### 2. Query Performance
- **Challenge:** Poorly written queries can timeout on large graphs
- **Impact:** Production outages, wasted resources
- **Mitigation:** Test execution time measurement; query pattern warnings

### 3. Debugging Difficulty
- **Challenge:** Error messages are cryptic; line numbers often incorrect
- **Impact:** Long debugging cycles; frustration
- **Mitigation:** Clear validation errors; auto-fix suggestions

### 4. Iteration Speed
- **Challenge:** Need to test many variations (different filters, patterns)
- **Impact:** Slow feedback delays insight discovery
- **Mitigation:** Fast validation (<100ms) vs. slow execution (seconds/minutes)

### 5. Schema Evolution
- **Challenge:** RDF schemas change; queries break over time
- **Impact:** Maintenance burden; production failures
- **Mitigation:** Continuous testing; regression detection

---

## Expected Outcomes

### Functional Requirements
- ✅ Syntactically valid SPARQL queries (no parser errors)
- ✅ Common patterns auto-fixed (typos, missing prefixes, malformed filters)
- ✅ Test execution returns expected results (not just runs without error)
- ✅ Performance acceptable (<5 seconds for typical queries on sample data)

### Quality Requirements
- ✅ Query optimized for target data volume (appropriate LIMIT, filters)
- ✅ Graph patterns match actual data structure (verified by testing)
- ✅ Error messages are clear and actionable (line/column numbers, suggestions)
- ✅ Auto-fixes are safe (don't change query semantics)

### Developer Experience
- ✅ Fast iteration: <1 minute from "write query" to "validated result"
- ✅ Low cognitive load: Don't need to memorize every syntax rule
- ✅ Confidence: Know query will work in production (tested on sample data)
- ✅ Learning: See corrections and understand why they were made

---

## MCP Tool Usage

### Tool 1: `validate_sparql_syntax`

**Purpose:** Fast syntax validation without query execution

**Usage:**
```python
result = mcp.call("validate_sparql_syntax", {
  "sparql": "SELECT ?s ?p ?o WHERE { ?s ?p ?o } LIMIT 10"
})
```

**Response:**
```json
{
  "is_valid": true,
  "query_type": "SELECT",
  "warnings": [
    {
      "level": "info",
      "message": "Query includes LIMIT without ORDER BY (results may be non-deterministic)"
    }
  ],
  "validation_time_ms": 23
}
```

**Error Example:**
```json
{
  "is_valid": false,
  "error": {
    "message": "Expected 'SELECT' keyword at line 1, column 1",
    "line": 1,
    "column": 1,
    "snippet": "SELEC ?s WHERE { ?s a foaf:Person }",
    "suggestion": "Did you mean 'SELECT'?"
  }
}
```

---

### Tool 2: `fix_sparql_query`

**Purpose:** Automatically correct common SPARQL syntax errors

**Usage:**
```python
result = mcp.call("fix_sparql_query", {
  "sparql": "SELEC ?s WHER { ?s a foaf:Person }",
  "dry_run": true  # Preview fixes without applying
})
```

**Response:**
```json
{
  "fixed_sparql": "SELECT ?s WHERE { ?s a foaf:Person }",
  "fixes_applied": [
    {
      "type": "typo_correction",
      "original": "SELEC",
      "corrected": "SELECT",
      "line": 1,
      "reason": "Corrected keyword typo"
    },
    {
      "type": "typo_correction",
      "original": "WHER",
      "corrected": "WHERE",
      "line": 1,
      "reason": "Corrected keyword typo"
    },
    {
      "type": "prefix_addition",
      "prefix": "foaf",
      "uri": "http://xmlns.com/foaf/0.1/",
      "reason": "Added missing prefix declaration"
    }
  ],
  "confidence": 0.95
}
```

**Dry Run Mode:**
- `dry_run: true` → Preview fixes without modifying query
- `dry_run: false` → Return corrected query (safe to use)

---

### Tool 3: `test_sparql_query`

**Purpose:** Execute query against sample data and verify results

**Usage:**
```python
result = mcp.call("test_sparql_query", {
  "sparql": "SELECT ?s WHERE { ?s a foaf:Person }",
  "test_data": {
    "triples": [
      "<alice> a <foaf:Person> .",
      "<bob> a <foaf:Person> .",
      "<company> a <foaf:Organization> ."
    ],
    "prefixes": {
      "foaf": "http://xmlns.com/foaf/0.1/"
    }
  },
  "expected_results": {
    "min_row_count": 2,
    "max_row_count": 2
  }
})
```

**Response:**
```json
{
  "success": true,
  "results": [
    {"s": "alice"},
    {"s": "bob"}
  ],
  "execution_time_ms": 15,
  "row_count": 2,
  "validation": {
    "meets_min_rows": true,
    "meets_max_rows": true,
    "passed": true
  },
  "performance": {
    "is_acceptable": true,
    "threshold_ms": 5000
  }
}
```

**Failure Example:**
```json
{
  "success": false,
  "error": "Query returned 0 results, expected at least 2",
  "results": [],
  "execution_time_ms": 8,
  "row_count": 0,
  "validation": {
    "meets_min_rows": false,
    "meets_max_rows": true,
    "passed": false
  },
  "suggestion": "Check graph pattern: Does test data contain 'foaf:Person' instances?"
}
```

---

## Success Criteria

### 1. Syntax Validation
- **Metric:** 100% of syntax errors detected before query execution
- **Test:** Invalid queries return `is_valid: false` with clear error message
- **Target:** Validation time <100ms for typical queries

### 2. Auto-Fix Accuracy
- **Metric:** >90% of common mistakes auto-corrected without changing semantics
- **Test:** Fixed queries pass validation and return expected results
- **Target:** False positive rate <5% (over-correction)

### 3. Test Execution
- **Metric:** Queries return expected results on sample data
- **Test:** `validation.passed = true` for correct queries
- **Target:** Execution time <5 seconds for typical test datasets

### 4. Developer Experience
- **Metric:** Time from "write query" to "validated result" reduced by 70%
- **Test:** Survey of developers after 1 week of usage
- **Target:** <1 minute for simple queries, <5 minutes for complex queries

---

## Example Workflow

### Scenario: Finding Duplicate Resources

**User Story:**
"I need to find all resources in my ontology that have duplicate `skos:prefLabel` values, which indicates data quality issues."

#### Step 1: Write Initial Query
```sparql
SELECT ?label (COUNT(?resource) AS ?count)
WHERE {
  ?resource skos:prefLabel ?label
}
GROUP BY ?label
HAVING (?count > 1)
```

#### Step 2: Validate Syntax
```python
mcp.call("validate_sparql_syntax", {
  "sparql": query
})
# Returns: is_valid: true, query_type: SELECT
# Warning: "HAVING clause should use COUNT(?resource) not ?count"
```

#### Step 3: Fix Warnings
```python
mcp.call("fix_sparql_query", {
  "sparql": query,
  "dry_run": true
})
# Returns: No syntax fixes needed, suggests HAVING improvement
```

#### Step 4: Test Against Sample Data
```python
mcp.call("test_sparql_query", {
  "sparql": query,
  "test_data": {
    "triples": [
      "<concept1> skos:prefLabel 'Apple' .",
      "<concept2> skos:prefLabel 'Apple' .",
      "<concept3> skos:prefLabel 'Banana' ."
    ]
  }
})
# Returns: 1 result (label='Apple', count=2), execution_time_ms=12
```

#### Step 5: Production Deployment
```bash
# Query validated, tested, and optimized
# Deploy to production with confidence
```

---

## Conclusion

This JTBD story captures the core job of "Developing and debugging SPARQL queries" through the lens of reducing friction, increasing confidence, and accelerating iteration. The MCP tools (validate, fix, test) address each pain point identified in the 5 Whys analysis:

1. **Syntax validation** → Fast feedback, clear errors
2. **Auto-fix** → Reduced cognitive load, faster iteration
3. **Testing** → Runtime correctness, performance validation
4. **MCP integration** → Unified workflow, AI assistance

The result is a development experience that transforms SPARQL query development from a slow, error-prone process into a rapid, confident workflow.
