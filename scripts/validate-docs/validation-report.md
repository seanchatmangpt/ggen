# ggen Documentation Validation Report

**Generated**: 2025-12-11 15:13:27
**ggen Version**: 
**Duration**: 58s

---

## Summary

| Metric | Count |
|--------|-------|
| **Test Suites Run** | 8 |
| **Suites Passed** | ✅ 3 |
| **Suites Failed** | ❌ 5 |
| **Individual Tests Run** | 0 |
| **Tests Passed** | ✅ 0 |
| **Tests Failed** | ❌ 0 |

**Overall Status**: ❌ **FAIL**

---

## Test Suites

### 1. Quick Start Tutorial (10 minutes)

**Document**: `docs/getting-started/quick-start.md`
**Status**: ✅ PASS

Tests that users can:
- Install ggen
- Create an RDF ontology
- Load data into the graph
- Query with SPARQL
- Extract schema to JSON
- Generate JavaScript + Zod code

### 2. SPARQL Query How-to Guide

**Document**: `docs/how-to/generation/query-rdf-sparql.md`
**Status**: ✅ PASS

Tests all SPARQL query examples:
- Basic SELECT queries
- Filtering with FILTER
- Aggregation with COUNT and GROUP BY
- LIMIT and DISTINCT
- Common patterns

### 3. Complete CLI Command Reference

**Document**: `docs/reference/commands/complete-cli-reference.md`
**Status**: ✅ PASS

Tests all documented CLI commands:
- Template commands (list, show, lint, etc.)
- Graph commands (load, query, export, visualize)
- Ontology commands (extract, validate, generate)
- Project commands (init, gen, watch, etc.)
- Global options (--help, --version)

---

## Recommendations

⚠️  **Some documentation validation tests failed**

**Action Required**:
1. Review failed test output above
2. Fix failing commands or update documentation
3. Re-run validation: `scripts/validate-docs/validate-all.sh`
4. Do not merge documentation changes until all tests pass

**Common Issues**:
- Commands may have changed since documentation was written
- Examples may reference non-existent templates
- SPARQL queries may not match current data model
- CLI flags may have been renamed or removed

---

## Running Validation

**Run all tests**:
```bash
scripts/validate-docs/validate-all.sh
```

**Run specific test**:
```bash
scripts/validate-docs/validate-quick-start.sh
scripts/validate-docs/validate-sparql-guide.sh
scripts/validate-docs/validate-cli-reference.sh
```

**Environment Variables**:
- `GGEN_BIN`: Path to ggen binary (default: `ggen`)
- `REPORT_FILE`: Path to report output (default: `validation-report.md`)

---

**Last Updated**: 2025-12-11 15:13:28
