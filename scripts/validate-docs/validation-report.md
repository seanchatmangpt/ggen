# ggen Documentation Validation Report

**Generated**: 2025-12-11 15:27:49
**ggen Version**: 
**Duration**: 56s

---

## Summary

| Metric | Count |
|--------|-------|
| **Test Suites Run** | 8 |
| **Suites Passed** | ✅ 8 |
| **Suites Failed** | ❌ 0 |
| **Individual Tests Run** | 110 |
| **Tests Passed** | ✅ 110 |
| **Tests Failed** | ❌ 0 |

**Overall Status**: ✅ **PASS**

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

✅ **All documentation is validated and working!**

The documentation accurately reflects the current state of ggen. All tutorials,
guides, and examples have been tested and work as documented.

**Next Steps**:
- Documentation is ready for users
- Consider adding this validation to CI/CD pipeline
- Run this validation before each release

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

**Last Updated**: 2025-12-11 15:27:50
