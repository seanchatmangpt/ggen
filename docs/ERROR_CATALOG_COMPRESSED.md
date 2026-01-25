<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [Error Catalog (100% Compressed)](#error-catalog-100-compressed)
  - [EXIT CODES](#exit-codes)
  - [CONFIG ERRORS](#config-errors)
  - [SPARQL ERRORS](#sparql-errors)
  - [TEMPLATE ERRORS](#template-errors)
  - [RUNTIME ERRORS](#runtime-errors)
  - [POKA-YOKE ERRORS](#poka-yoke-errors)
  - [INFERENCE RULE ERRORS](#inference-rule-errors)
  - [VALIDATION (SHACL) ERRORS](#validation-shacl-errors)
  - [MARKETPLACE ERRORS](#marketplace-errors)
  - [COMMON COMBOS](#common-combos)
  - [DEBUG WORKFLOW](#debug-workflow)
  - [ONE-LINE FIXES](#one-line-fixes)
  - [ERROR PRIORITY](#error-priority)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Error Catalog (100% Compressed)

## EXIT CODES

| Code | Error | Cause | Fix |
|------|-------|-------|-----|
| 0 | Success | - | - |
| 1 | Generic | Uncategorized | Check `-vvv` logs |
| 2 | Parse error | Bad TTL/TOML syntax | Validate files: `rapper -c file.ttl` |
| 3 | Generation failed | SPARQL/template error | Check query, template syntax |
| 4 | Validation failed | Ontology constraint violation | Check SHACL, `--validate-only` |
| 5 | Timeout | Execution >--timeout ms | Increase `--timeout`, simplify queries |

---

## CONFIG ERRORS

| Error | Cause | Fix |
|-------|-------|-----|
| `Config not found` | ggen.toml missing | Create it or use `--config` |
| `Invalid TOML` | Syntax error | Check brackets, quotes, types |
| `ontology_dir not found` | Dir doesn't exist | Create directory or fix path |
| `template not found` | .tera file missing | Check `templates_dir`, filename |
| `Invalid rule name` | Rule not in `[[inference.rules]]` | Check `ggen.toml` rule_order |
| `Duplicate rule` | Rule defined twice | Remove duplicate |

---

## SPARQL ERRORS

| Error | Cause | Fix |
|-------|-------|-----|
| `SPARQL syntax error` | Bad query | Validate: `SELECT * WHERE {}` structure |
| `Unknown variable` | `?undefinedVar` in CONSTRUCT | Define in WHERE clause |
| `Unbound variable` | Optional var not bound | Use `BIND()` or move to CONSTRUCT |
| `Invalid IRI` | Bad namespace | Use full IRI: `<http://example.com/>` |
| `Type mismatch` | String vs number | Use `STR()`, `xsd:integer` |
| `No results` | Query matches nothing | Check WHERE conditions, data |
| `Query timeout` | SPARQL too complex | Add LIMIT, DISTINCT, simplify WHERE |

---

## TEMPLATE ERRORS

| Error | Cause | Fix |
|-------|-------|-----|
| `Undefined variable` | `{{ undefined }}` | Check SPARQL SELECT columns |
| `Syntax error` | `{% for %}` unmatched | Count `{%` and `%}` pairs |
| `Type mismatch` | Filter on wrong type | Use `| string_filter` |
| `No output` | All conditionals false | Check `{% if %}` logic |
| `Encoding error` | Unicode characters | Use `UTF-8` encoding |

---

## RUNTIME ERRORS

| Error | Cause | Fix |
|-------|-------|-----|
| `Cannot write file` | Permission denied | Check dir permissions: `chmod 755` |
| `Protected path` | Overwrite `protected_paths` | Use `--force` or reconfig |
| `Path already exists` | Output file exists | Use `--force` or change output file |
| `Invalid UTF-8` | Non-UTF8 in ontology | Re-save as UTF-8 |
| `Out of memory` | Ontology too large | Split into multiple files, use `LIMIT` |

---

## POKA-YOKE ERRORS

| Error | Cause | Fix |
|-------|-------|-----|
| `Cannot overwrite protected` | File in `protected_paths` | Edit `protected_paths` or use `--force` |
| `Missing DO NOT EDIT header` | `warning_headers=false` | Set `warning_headers=true` |
| `Not in .gitignore` | Generated file not ignored | Add to `.gitignore` or set `gitignore_generated=true` |

---

## INFERENCE RULE ERRORS

| Error | Cause | Fix |
|-------|-------|-----|
| `Rule not found` | Typo in `rule_order` | Check spelling, ensure rule defined |
| `Rule failed` | SPARQL error in rule | Test rule independently |
| `No facts materialized` | Query matches nothing | Verify data exists, check WHERE |
| `Circular dependency` | Rules depend on each other | Reorder in `rule_order` |
| `Infinite materialization` | Recursive rule | Add `MINUS` to prevent re-processing |

---

## VALIDATION (SHACL) ERRORS

| Error | Cause | Fix |
|-------|-------|-----|
| `Property missing` | Required property not present | Add property to ontology |
| `Type mismatch` | `rdfs:range` incorrect | Fix property type definition |
| `Cardinality` | Too many/few values | Check `sh:minCount`, `sh:maxCount` |
| `Pattern` | Doesn't match regex | Fix value or pattern |
| `Min/Max value` | Out of range | Check `sh:minInclusive`, `sh:maxInclusive` |

---

## MARKETPLACE ERRORS

| Error | Cause | Fix |
|-------|-------|-----|
| `Package not found` | Package not in registry | Check package name, version |
| `Dependency conflict` | Version mismatch | Update `ggen.toml` versions |
| `Signature verification failed` | Package tampered/untrusted | Update registry, check source |
| `FMEA threshold exceeded` | RPN > `critical_threshold` | Add mitigations or increase threshold |

---

## COMMON COMBOS

| Symptoms | Likely Cause |
|----------|--------------|
| No output + no errors | Ontology empty or SPARQL matches nothing |
| Timeout | Large ontology + complex SPARQL |
| Files not generated | Template has all-false conditions |
| Wrong output | SPARQL matches unexpected data |
| Permission denied | Files owned by other user |
| Encoding errors | Mixed UTF-8/ASCII files |

---

## DEBUG WORKFLOW

```bash
# 1. Validate config
ggen sync --validate-only

# 2. Check SPARQL
ggen sync --rule rule-name --dry-run -vvv

# 3. Check template output
ggen sync --dry-run > output.txt

# 4. See exactly what happened
ggen sync --audit audit.json

# 5. Full verbosity
ggen sync -vvv --audit debug.json --dry-run

# 6. Test files directly
rapper -c ontology/file.ttl  # TTL validation
toml-lint ggen.toml           # TOML validation
```

---

## ONE-LINE FIXES

| Problem | Command |
|---------|---------|
| Bad TTL syntax | `rapper -c file.ttl` |
| Bad TOML syntax | `toml-lint ggen.toml` |
| Ontology empty | `cat ontology/*.ttl \| wc -l` |
| Template bug | `ggen sync --dry-run \| head -50` |
| Slow query | Add `LIMIT 1000` to SPARQL |
| Too verbose | Use `--quiet` flag |
| See all options | `ggen sync --help` |

---

## ERROR PRIORITY

| P0 (Stop Work) | P1 (Fix Soon) | P2 (Acceptable) | P3 (Polish) |
|----------------|--------------|-----------------|-----------|
| Parse errors | Validation fail | Slow generation | Warnings |
| Syntax errors | Timeout | Missing features | Suggestions |
| Protected path | Permission deny | Type mismatch | Info messages |
| Config missing | File not found | Unused classes | Debug output |

