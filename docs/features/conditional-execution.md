# Conditional Execution Feature

## Overview

Conditional execution allows generation rules to be **skipped dynamically** based on SPARQL ASK queries. Instead of always running every rule, ggen evaluates conditions against the ontology graph and only executes rules when conditions are met.

## Purpose

- **Selective Generation**: Generate files only when relevant data exists
- **Performance**: Skip expensive template rendering for empty result sets
- **Workflow Control**: Enable/disable rules via ontology metadata
- **Feature Flags**: Toggle code generation based on ontology predicates

## Usage

### Basic Conditional Rule

```toml
# ggen.toml
[[generation_rule]]
name = "api_endpoints"
query = "query/api_endpoints.rq"
template = "templates/rust_api.tera"
output_pattern = "src/api/{{ endpoint_name }}.rs"

# Only generate if API endpoints exist in ontology
condition = """
  ASK {
    ?endpoint a :ApiEndpoint .
  }
"""
```

### How It Works

```
1. Load ontology → RDF graph
2. Evaluate condition (SPARQL ASK) → true/false
3. If true: Execute SELECT query, render template, write files
4. If false: Skip rule entirely (no query, no template)
```

## Condition Query Syntax

Conditions must be valid SPARQL ASK queries:

```sparql
# Basic existence check
ASK { ?s a :User }

# Multiple conditions
ASK {
  ?s a :User .
  ?s :email ?email .
}

# Negation (generate if NOT exists)
ASK {
  FILTER NOT EXISTS { ?s a :LegacySystem }
}

# Cardinality check (generate if > 10 entities)
ASK {
  {
    SELECT (COUNT(?s) AS ?count) WHERE {
      ?s a :Product
    }
  }
  FILTER (?count > 10)
}

# Feature flag (generate if ontology metadata enables feature)
ASK {
  :ProjectConfig :enableFeature :ApiGeneration .
}
```

## Examples

### Example 1: Optional Features

```toml
# Only generate GraphQL API if schema exists
[[generation_rule]]
name = "graphql_schema"
condition = """
  ASK { ?type a :GraphQLType }
"""
query = "query/graphql_types.rq"
template = "templates/graphql_schema.tera"
output_pattern = "src/graphql/schema.graphql"

# Only generate REST API if endpoints exist
[[generation_rule]]
name = "rest_endpoints"
condition = """
  ASK { ?endpoint a :RestEndpoint }
"""
query = "query/rest_endpoints.rq"
template = "templates/rust_rest.tera"
output_pattern = "src/api/rest/{{ endpoint_name }}.rs"
```

**Workflow**:
```bash
# Ontology has GraphQL types, no REST endpoints
ggen sync
# Result:
#   - graphql_schema rule EXECUTED (condition = true)
#   - rest_endpoints rule SKIPPED (condition = false)
```

### Example 2: Environment-Based Generation

```turtle
# ontology/config.ttl
:ProjectConfig
  :environment "production" ;
  :enableDebugRoutes false ;
  :enableTestHarness false .
```

```toml
# ggen.toml

# Only in development
[[generation_rule]]
name = "debug_routes"
condition = """
  ASK {
    :ProjectConfig :environment "development" .
  }
"""
query = "query/debug_routes.rq"
template = "templates/debug.tera"

# Only in test
[[generation_rule]]
name = "test_harness"
condition = """
  ASK {
    :ProjectConfig :enableTestHarness true .
  }
"""
query = "query/test_fixtures.rq"
template = "templates/test_harness.tera"
```

### Example 3: Incremental Migration

```turtle
# ontology/domain.ttl
:User
  a :Entity ;
  :migrated true ;
  :fields (...) .

:Product
  a :Entity ;
  :migrated false ;  # Legacy entity
  :fields (...) .
```

```toml
# Only generate for migrated entities
[[generation_rule]]
name = "entities"
condition = """
  ASK {
    ?entity a :Entity ;
            :migrated true .
  }
"""
query = """
  SELECT ?entity ?name WHERE {
    ?entity a :Entity ;
            :migrated true ;
            :name ?name .
  }
"""
template = "templates/entity.tera"
output_pattern = "src/models/{{ name | snake_case }}.rs"
```

**Workflow**:
```bash
ggen sync
# Result:
#   - src/models/user.rs GENERATED (migrated = true)
#   - src/models/product.rs SKIPPED (migrated = false)

# Later: Migrate Product
# ontology/domain.ttl: :Product :migrated true .

ggen sync
# Result:
#   - src/models/user.rs UNCHANGED
#   - src/models/product.rs GENERATED (now migrated)
```

### Example 4: Dependency-Aware Generation

```toml
# Generate database migrations only if schema changed
[[generation_rule]]
name = "migrations"
condition = """
  ASK {
    ?entity a :Entity ;
            :schemaVersion ?version .
    FILTER (?version > "1.0.0"^^xsd:string)
  }
"""
query = "query/schema_changes.rq"
template = "templates/migration.tera"
output_pattern = "migrations/{{ timestamp }}_{{ entity_name }}.sql"
```

### Example 5: Cardinality Checks

```toml
# Skip generation if no data (avoid empty files)
[[generation_rule]]
name = "user_crud"
condition = """
  ASK {
    {
      SELECT (COUNT(?user) AS ?count) WHERE {
        ?user a :User .
      }
    }
    FILTER (?count > 0)
  }
"""
query = "query/users.rq"
template = "templates/crud.tera"
output_pattern = "src/crud/users.rs"
```

## Combining Conditions with Filters

### Template-Level Filtering (Anti-Pattern)

```jinja
{# templates/entities.tera - WRONG APPROACH #}
{% for entity in entities %}
  {% if entity.migrated %}  {# Filtering in template #}
    pub struct {{ entity.name }} { ... }
  {% endif %}
{% endfor %}
```

**Problem**: Template still executes even if no migrated entities exist.

### Condition-Level Filtering (Correct Pattern)

```toml
# CORRECT: Condition skips rule entirely if no migrated entities
[[generation_rule]]
name = "entities"
condition = """
  ASK {
    ?entity a :Entity ;
            :migrated true .
  }
"""
query = """
  SELECT ?entity WHERE {
    ?entity a :Entity ;
            :migrated true .  # Query also filters
  }
"""
```

**Benefit**: Rule skipped if no data → No template execution → Faster.

## Debugging Conditions

### Verbose Output

```bash
ggen sync --verbose

# Output:
# [14:30:45] Evaluating condition for rule 'api_endpoints'...
# [14:30:45]   ASK { ?endpoint a :ApiEndpoint . }
# [14:30:45]   Result: false
# [14:30:45]   SKIPPED (condition not met)
```

### Dry-Run with Conditions

```bash
ggen sync --dry-run --verbose --format json | jq '.rules[] | select(.skipped == true)'

# Output:
# {
#   "name": "api_endpoints",
#   "skipped": true,
#   "reason": "condition_failed",
#   "condition": "ASK { ?endpoint a :ApiEndpoint . }"
# }
```

### Manual Testing

```bash
# Test condition query directly with SPARQL tool
sparql --data ontology/domain.ttl --query - <<EOF
ASK { ?endpoint a :ApiEndpoint . }
EOF

# Output:
# yes   (or "no")
```

## Performance Impact

### Condition Evaluation Cost

- **ASK queries**: Typically <10ms (graph traversal only)
- **Complex ASK**: Up to 50ms (aggregations, filters)
- **Total overhead**: Negligible compared to template rendering (100-500ms)

### Optimization

```toml
# SLOW: Complex condition with aggregation
condition = """
  ASK {
    {
      SELECT (COUNT(DISTINCT ?field) AS ?count) WHERE {
        ?entity a :Entity ;
                :field ?field .
      }
    }
    FILTER (?count > 100)
  }
"""

# FASTER: Simple existence check (push complexity to SELECT query)
condition = """
  ASK {
    ?entity a :Entity ;
            :field ?field .
  }
"""
# Let SELECT query handle cardinality filtering
```

## Best Practices

1. **Keep conditions simple**
   - Prefer existence checks over aggregations
   - Push complex logic to SELECT queries

2. **Use feature flags for toggleable generation**
   ```turtle
   :ProjectConfig
     :enableGraphQL true ;
     :enableREST false ;
     :enableGRPC true .
   ```

3. **Document condition intent**
   ```toml
   [[generation_rule]]
   name = "api_endpoints"
   # Condition: Only generate API if at least one endpoint exists
   # This prevents empty api/ directory in minimal projects
   condition = "ASK { ?endpoint a :ApiEndpoint . }"
   ```

4. **Test conditions in isolation**
   ```bash
   # Create test script
   ./scripts/test_conditions.sh

   # Inside script:
   for rule in api_endpoints graphql_schema; do
     echo "Testing condition for $rule:"
     sparql --data ontology/domain.ttl --query "conditions/${rule}.rq"
   done
   ```

5. **Use dry-run to verify condition behavior**
   ```bash
   ggen sync --dry-run --verbose | grep "SKIPPED"
   ```

## Advanced Patterns

### Mutually Exclusive Rules

```toml
# Generate EITHER GraphQL OR REST (not both)
[[generation_rule]]
name = "graphql_api"
condition = """
  ASK {
    :ProjectConfig :apiStyle "graphql" .
  }
"""

[[generation_rule]]
name = "rest_api"
condition = """
  ASK {
    :ProjectConfig :apiStyle "rest" .
  }
"""
```

### Cascading Conditions

```toml
# Rule 1: Generate base entities
[[generation_rule]]
name = "entities"
condition = "ASK { ?e a :Entity . }"
query = "query/entities.rq"

# Rule 2: Generate relationships (requires entities)
[[generation_rule]]
name = "relationships"
condition = """
  ASK {
    # Check both entities AND relationships exist
    ?e a :Entity .
    ?rel a :Relationship .
  }
"""
query = "query/relationships.rq"
```

### Version-Based Generation

```turtle
# ontology/config.ttl
:ProjectConfig
  :apiVersion "v2" .
```

```toml
# Only generate v2 API
[[generation_rule]]
name = "api_v2"
condition = """
  ASK {
    :ProjectConfig :apiVersion "v2" .
  }
"""

# Only generate v1 API (legacy)
[[generation_rule]]
name = "api_v1"
condition = """
  ASK {
    :ProjectConfig :apiVersion "v1" .
  }
"""
```

## Troubleshooting

### Issue: Condition always false

**Cause**: Namespace prefix not defined in query

**Fix**: Add PREFIX declarations
```sparql
# WRONG (namespace undefined)
ASK { ?e a :Entity . }

# CORRECT
PREFIX : <http://example.com/ontology#>
ASK { ?e a :Entity . }
```

**OR**: Configure default namespace in `ggen.toml`
```toml
[ontology]
default_namespace = "http://example.com/ontology#"
```

### Issue: Rule skipped unexpectedly

**Cause**: Condition query has syntax error

**Debug**:
```bash
ggen sync --verbose 2>&1 | grep -A5 "ERROR"
# Look for SPARQL syntax errors in condition evaluation
```

### Issue: Condition too expensive

**Symptom**: `ggen sync` takes >1s per rule

**Fix**: Simplify condition or add index hints
```sparql
# SLOW: Full graph traversal
ASK {
  ?e a :Entity .
  ?e :field ?f .
  ?f :type ?t .
}

# FAST: Limit to indexed predicates
ASK {
  ?e a :Entity .  # Indexed by type
}
```

## Security Considerations

- **Conditions execute arbitrary SPARQL** (same risk as SELECT queries)
- **Malicious conditions can DoS** (e.g., infinite loops in CONSTRUCT-based ASK)
- **Validate ontology sources** before executing conditions

## Related Documentation

- [Validation](validation.md) - SHACL/SPARQL constraints (related to ASK queries)
- [Watch Mode](watch-mode.md) - Conditions re-evaluated on each watch trigger
- [Audit Trail](audit-trail.md) - Logs condition evaluation results
