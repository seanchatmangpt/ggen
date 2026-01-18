# Validation Feature

## Overview

The validation feature enforces **structural and semantic constraints** on ontologies using SHACL (Shapes Constraint Language) and SPARQL ASK queries. Validation runs **before code generation** to ensure the ontology is consistent, complete, and ready for transformation into code.

## Purpose

- **Data Quality**: Catch ontology errors before code generation
- **Schema Enforcement**: Ensure entities have required properties
- **Business Rules**: Validate semantic constraints (e.g., "User must have email")
- **Type Safety**: Verify type consistency across relationships
- **Migration Safety**: Validate schema versions and compatibility

## Validation Types

### 1. SHACL Validation (Structural Constraints)

SHACL defines "shapes" that describe how RDF data should look:

```turtle
# validation/user_shape.ttl
PREFIX sh: <http://www.w3.org/ns/shacl#>
PREFIX : <http://example.com/ontology#>

:UserShape
  a sh:NodeShape ;
  sh:targetClass :User ;
  sh:property [
    sh:path :email ;
    sh:minCount 1 ;           # Required
    sh:maxCount 1 ;           # Single-valued
    sh:datatype xsd:string ;  # Type constraint
    sh:pattern "^.+@.+$" ;    # Regex validation
  ] ;
  sh:property [
    sh:path :age ;
    sh:datatype xsd:integer ;
    sh:minInclusive 0 ;       # Range constraint
    sh:maxInclusive 150 ;
  ] .
```

### 2. SPARQL ASK Validation (Semantic Constraints)

SPARQL ASK queries check for logical consistency:

```sparql
# validation/business_rules.rq
PREFIX : <http://example.com/ontology#>

# Rule: Every User with admin role must have verified email
ASK {
  ?user a :User ;
        :role :Admin ;
        :emailVerified false .
}
# Returns true if violation exists (admin with unverified email)
```

## Usage

### Basic Validation

```bash
# Validate before generating
ggen sync --validate-only
```

### Combined with Generation

```bash
# Validate, then generate (fail fast on errors)
ggen sync  # Validation is automatic

# Skip validation (DANGEROUS)
ggen sync --skip-validation
```

### Validation Configuration

```toml
# ggen.toml
[validation]
shacl_shapes = [
  "validation/user_shape.ttl",
  "validation/product_shape.ttl",
  "validation/order_shape.ttl"
]

sparql_asks = [
  "validation/business_rules.rq",
  "validation/consistency_checks.rq"
]

# Validation strictness
fail_on_warning = true   # Warnings treated as errors
fail_on_info = false     # Info messages don't fail

# Performance
max_validation_time_ms = 5000  # Timeout after 5s
```

## SHACL Constraint Types

### Cardinality Constraints

```turtle
# Required property (minCount)
sh:property [
  sh:path :email ;
  sh:minCount 1 ;  # Must have at least 1
] .

# Optional property (maxCount)
sh:property [
  sh:path :middleName ;
  sh:maxCount 1 ;  # At most 1
] .

# Exact cardinality
sh:property [
  sh:path :id ;
  sh:minCount 1 ;
  sh:maxCount 1 ;  # Exactly 1
] .
```

### Datatype Constraints

```turtle
sh:property [
  sh:path :age ;
  sh:datatype xsd:integer ;
] .

sh:property [
  sh:path :createdAt ;
  sh:datatype xsd:dateTime ;
] .

sh:property [
  sh:path :price ;
  sh:datatype xsd:decimal ;
] .
```

### Value Constraints

```turtle
# Allowed values (enum)
sh:property [
  sh:path :status ;
  sh:in ( "active" "inactive" "pending" ) ;
] .

# Range constraints
sh:property [
  sh:path :rating ;
  sh:minInclusive 1 ;
  sh:maxInclusive 5 ;
] .

# Pattern matching
sh:property [
  sh:path :email ;
  sh:pattern "^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}$" ;
] .

# String length
sh:property [
  sh:path :username ;
  sh:minLength 3 ;
  sh:maxLength 20 ;
] .
```

### Relationship Constraints

```turtle
# Type constraint
sh:property [
  sh:path :author ;
  sh:class :User ;  # Must be a User
] .

# Existence constraint
sh:property [
  sh:path :belongsTo ;
  sh:minCount 1 ;
  sh:node :OrganizationShape ;  # Must reference valid Organization
] .
```

## SPARQL ASK Validation Patterns

### Existence Checks

```sparql
# Ensure all Users have unique emails
ASK {
  ?user1 a :User ; :email ?email .
  ?user2 a :User ; :email ?email .
  FILTER (?user1 != ?user2)
}
# Returns true if duplicate emails exist (violation)
```

### Consistency Checks

```sparql
# Ensure OrderItem quantity matches Order total
ASK {
  ?order a :Order ;
         :total ?orderTotal .
  {
    SELECT ?order (SUM(?itemTotal) AS ?calculatedTotal) WHERE {
      ?order :hasItem ?item .
      ?item :quantity ?qty ; :price ?price .
      BIND (?qty * ?price AS ?itemTotal)
    }
    GROUP BY ?order
  }
  FILTER (?orderTotal != ?calculatedTotal)
}
```

### Business Rule Checks

```sparql
# Ensure Admins cannot be under 18
ASK {
  ?user a :User ;
        :role :Admin ;
        :age ?age .
  FILTER (?age < 18)
}
```

### Cross-Entity Validation

```sparql
# Ensure all Order references exist as Products
ASK {
  ?order :hasItem ?item .
  ?item :productId ?prodId .
  FILTER NOT EXISTS {
    ?product a :Product ; :id ?prodId .
  }
}
```

## Validation Output

### Success (No Violations)

```bash
$ ggen sync --validate-only
✓ SHACL validation passed (3 shapes, 0 violations)
✓ SPARQL validation passed (5 queries, 0 failures)
✓ Ontology is valid
```

### SHACL Violations

```bash
$ ggen sync --validate-only
✗ SHACL validation failed (12 violations)

Violation 1:
  Severity: sh:Violation
  Focus Node: :User123
  Path: :email
  Message: Required property missing
  Shape: :UserShape

Violation 2:
  Severity: sh:Violation
  Focus Node: :User456
  Path: :age
  Value: -5
  Message: Value -5 is less than minimum 0
  Shape: :UserShape

Use --verbose for full validation report
```

### SPARQL ASK Failures

```bash
$ ggen sync --validate-only
✗ SPARQL validation failed (2 queries failed)

Query: validation/business_rules.rq
  Name: "Admin age check"
  Result: FAILED (ASK returned true)
  Message: Found admin users under age 18
  Affected entities: :User789

Query: validation/consistency_checks.rq
  Name: "Order total consistency"
  Result: FAILED
  Message: Order totals don't match item sums
  Affected entities: :Order101, :Order102
```

## Validation Workflow Examples

### Example 1: Pre-Commit Validation

```bash
# .git/hooks/pre-commit
#!/bin/bash
set -e

echo "Validating ontology before commit..."
ggen sync --validate-only

if [ $? -ne 0 ]; then
  echo "ERROR: Ontology validation failed. Fix violations before committing."
  exit 1
fi

echo "✓ Validation passed"
```

### Example 2: CI/CD Pipeline

```yaml
# .github/workflows/validate.yml
name: Validate Ontology

on:
  pull_request:
    paths:
      - 'ontology/**/*.ttl'
      - 'validation/**/*.ttl'

jobs:
  validate:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4

      - name: Install ggen
        run: cargo install ggen --version 5.1.0

      - name: Validate ontology
        run: |
          ggen sync --validate-only --format json > validation_report.json

      - name: Check for violations
        run: |
          violations=$(jq '.validation.shacl_violations + .validation.sparql_failures' validation_report.json)
          if [ "$violations" -gt 0 ]; then
            echo "ERROR: $violations validation violations detected"
            jq '.validation' validation_report.json
            exit 1
          fi

      - name: Upload validation report
        uses: actions/upload-artifact@v4
        with:
          name: validation-report
          path: validation_report.json
```

### Example 3: Development Workflow

```bash
# Terminal 1: Watch mode with validation
ggen sync --watch --validate-only

# Terminal 2: Edit ontology
vim ontology/domain.ttl
# Save → Validation runs → See violations → Fix → Repeat
```

### Example 4: Migration Validation

```turtle
# ontology/config.ttl
:ProjectConfig
  :schemaVersion "2.0.0" ;
  :minSupportedVersion "1.5.0" .
```

```sparql
# validation/version_check.rq
# Ensure no entities from unsupported schema versions
ASK {
  ?entity :schemaVersion ?version .
  FILTER (?version < "1.5.0"^^xsd:string)
}
```

```bash
# Before migration
ggen sync --validate-only
# ERROR: Found entities with schema version < 1.5.0
# → Migrate old entities first
```

## Validation Best Practices

1. **Validate early and often**
   - Run validation in watch mode during development
   - Enforce validation in pre-commit hooks
   - Block CI/CD on validation failures

2. **Separate structural and semantic validation**
   - SHACL for structure (required fields, types)
   - SPARQL ASK for business logic (age checks, consistency)

3. **Use descriptive violation messages**
   ```turtle
   sh:property [
     sh:path :email ;
     sh:minCount 1 ;
     sh:message "User must have an email address (required for authentication)" ;
   ] .
   ```

4. **Document validation rules**
   ```sparql
   # validation/business_rules.rq
   # Rule: BR-001 - Admin Age Requirement
   # Description: Administrators must be at least 18 years old
   # Severity: ERROR
   # Owner: Security Team
   ASK { ?user a :User ; :role :Admin ; :age ?age . FILTER (?age < 18) }
   ```

5. **Test validation rules**
   ```bash
   # Create test ontology with intentional violations
   # validation/test_cases/invalid_user.ttl
   :TestUser a :User .  # Missing required :email

   # Verify SHACL catches it
   ggen validate --data validation/test_cases/invalid_user.ttl
   # Expected: SHACL violation on :TestUser
   ```

## Integration with Other Features

### With `--force`

```bash
# Validation runs BEFORE --force overwrites
ggen sync --force
# 1. Validate ontology
# 2. If valid: Generate and overwrite files
# 3. If invalid: Fail without overwriting
```

### With `--watch`

```bash
# Continuous validation
ggen sync --watch --validate-only

# Edit ontology → Validation runs → See violations immediately
```

### With `--audit`

```bash
# Log validation results in audit trail
ggen sync --audit

# audit.json includes:
{
  "validation": {
    "shacl_constraints": 12,
    "shacl_violations": 2,
    "sparql_asks": 5,
    "ask_results": [false, false, true, false, false]
  }
}
```

## Performance Impact

- **SHACL validation**: ~50-200ms for 1000 triples
- **SPARQL ASK**: ~10-50ms per query
- **Total overhead**: <5% of generation time

### Optimization

```toml
# Skip validation in development (UNSAFE)
[validation]
skip_in_watch_mode = true  # Only validate on explicit sync

# Parallel validation
[validation]
parallel = true  # Run SHACL and SPARQL in parallel
max_workers = 4
```

## Troubleshooting

### Issue: False positives

**Cause**: SHACL shape too strict

**Fix**: Relax constraints
```turtle
# Before (too strict)
sh:minCount 1 ;  # Required

# After (optional)
sh:minCount 0 ;
```

### Issue: SPARQL ASK always fails

**Cause**: Query logic inverted

**Fix**: Negate query
```sparql
# WRONG: Returns true if valid (should return true if INVALID)
ASK { ?user a :User ; :email ?email . }

# CORRECT: Returns true if violation exists
ASK {
  ?user a :User .
  FILTER NOT EXISTS { ?user :email ?email . }
}
```

### Issue: Validation timeout

**Cause**: Complex SPARQL queries on large graphs

**Fix**: Increase timeout or simplify queries
```toml
[validation]
max_validation_time_ms = 30000  # 30 seconds
```

## Security Considerations

- **Validation rules execute arbitrary SPARQL** (same risk as generation queries)
- **Malicious SHACL shapes can DoS** (e.g., recursive shape references)
- **Validate validation rules themselves** (meta-validation)

## Related Documentation

- [Conditional Execution](conditional-execution.md) - SPARQL ASK for selective generation
- [Audit Trail](audit-trail.md) - Logging validation results
- [Force Flag](force-flag.md) - Validation before destructive operations
