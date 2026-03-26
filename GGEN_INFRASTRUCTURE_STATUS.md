# ggen Infrastructure Status Report

**Date**: 2026-03-26
**Status**: INFRASTRUCTURE INCOMPLETE - Blockers Identified

## Summary

The Java 26 E2E/integration test regeneration via `ggen sync` cannot proceed. Three critical infrastructure gaps were systematically identified:

---

## Issue #1: ggen Binary Architecture Mismatch

### Problem
- `ggen.toml` defines 9 generation rules with SPARQL queries:
  - jpa-entities, repositories, hibernate-repositories, virtual-thread-daos
  - services, dto-records, enums, domain-interfaces, spring-boot-app
- `ggen sync` CLI doesn't read or use `ggen.toml`
- `ggen sync` only supports `--from`, `--to`, `--mode`, `--dry-run`, `--force`, `--verbose`

### Current Behavior
```bash
$ ggen sync --from .claude/ggen/ontology --to .claude/ggen/generated --mode full
```
1. Discovers 70 OWL classes from ontology
2. Discovers 10 Tera templates in `templates/ggen/`
3. Attempts to render EVERY template for EVERY class
4. Fails: No SPARQL queries executed, templates get empty `sparql_results`

### Expected Behavior (from ggen.toml)
```toml
[[generation.rules]]
name = "jpa-entities"
query = { file = "queries/jpa-entities.rq" }
template = { file = "templates/jpa-entity.java.tera" }
output_file = "java/{{ packagePath }}/jpa/{{ simpleClassName }}JpaEntity.java"
```

**Gap**: ggen binary doesn't implement rule execution engine needed for this configuration.

---

## Issue #2: Template Variable Binding Broken

### Evidence

**Template: enum.java.tera**
```tera
{% set enumName = sparql_results[0]["?enumName"] %}
{% set constants = sparql_results[0]["?constants"] %}
public enum {{ enumName }} {
```

**Query: supporting-enums.rq**
```sparql
SELECT ?enumName (GROUP_CONCAT(...) AS ?constants)
WHERE { ... }
```

**Error**:
```
Failed to render template: enum.java.tera
Variable `sparql_results[0]` not found in context
the evaluated version was `sparql_results.0`. Maybe the index is out of bounds?
```

**Root Cause**: `ggen sync` doesn't execute SPARQL queries. Templates expect query results, but `sparql_results` is empty array.

### Scope
ALL 10 templates in `templates/ggen/` have this issue:
- domain-interface.java.tera
- enum.java.tera (partially fixed variable extraction)
- jpa-entity.java.tera
- repository-interface.java.tera
- hibernate-repository.java.tera
- virtual-thread-dao.java.tera
- service.java.tera
- dto-record.java.tera
- wcp-enum.java.tera
- aggregator-pom.xml.tera

---

## Issue #3: Baseline Tests Failing

### ClassNotFoundException
5 integration test classes are missing or unreachable:
```
java.lang.ClassNotFoundException: org.yawlfoundation.yawl.engine.interfce.YHttpServletTest$IntegrationTest
java.lang.ClassNotFoundException: org.yawlfoundation.yawl.engine.interfce.AuthenticationConfigTest$IntegrationTest
java.lang.ClassNotFoundException: org.yawlfoundation.yawl.engine.time.YTimerVariableTest$IntegrationTest
java.lang.ClassNotFoundException: org.yawlfoundation.yawl.engine.time.YTimedObjectTest$IntegrationTest
java.lang.ClassNotFoundException: org.yawlfoundation.yawl.thread.StructuredConcurrencyHelperTest$IntegrationTest
```

This blocks Phase 1 of the plan (baseline test validation).

---

## Systematic Investigation Results

### What Was Fixed
1. ✅ Updated enum.java.tera to properly extract `enumName` from SPARQL results
2. ✅ Created `templates/ggen/` directory and copied all templates
3. ✅ Confirmed ontology contains Entity instances (e.g., YExternalClient, CostDriver)
4. ✅ Confirmed SPARQL queries are syntactically valid

### What Cannot Be Fixed Without Architecture Changes
1. ❌ ggen binary doesn't execute SPARQL queries (needs rule engine implementation)
2. ❌ ggen binary doesn't use ggen.toml configuration (needs config file parsing)
3. ❌ ggen binary doesn't support template filtering (needs `--rule` or similar)
4. ❌ No `ggen apply` command (referenced in planned workflow)

---

## Options to Proceed

### Option 1: Fix ggen Binary Architecture (HIGH EFFORT)
**Implement missing components in ggen Rust CLI**:
- Add SPARQL query execution engine
- Add ggen.toml configuration parsing
- Implement rule-based generation (execute queries → render templates)
- Add `ggen apply` command for selective rule execution
- Support filtering (e.g., `--rule jpa-entities`, `--filter "e2e|integration"`)

**Effort**: Large (estimated 4-8 hours)
**Benefit**: Full ggen.toml-based generation pipeline working
**Risk**: Changes to ggen binary might require full test suite rerun

---

### Option 2: Use Existing ggen Without ggen.toml (MEDIUM EFFORT)
**Adapt approach to current ggen capabilities**:
- Remove ggen.toml (it's not used by the binary)
- Rewrite templates to work with ggen's generic class extraction
- Have ggen generate domain-interfaces for all OWL classes
- Skip template-specific features (field mapping, SPARQL-driven filtering)
- Implement test generation separately (external tool, not via ggen)

**Effort**: Medium (estimated 2-3 hours)
**Benefit**: Get code generation working quickly without changing ggen binary
**Risk**: Generated code may be less rich (missing field mappings, relationships)

---

### Option 3: Focus on Baseline Tests First (LOW EFFORT IMMEDIATE WIN)
**Prioritize in this order**:
1. Fix ClassNotFoundException for 5 integration test classes
2. Get baseline YAWL tests passing
3. Manually write E2E/integration tests (not via ggen)
4. Test Java 26 virtual thread functionality with real tests

**Effort**: Low (estimated 1-2 hours)
**Benefit**: Demonstrates Java 26 works, tests pass
**Risk**: Doesn't prove code generation works (you mentioned this is key)

---

## Recommended Path Forward

Given that your explicit goal is "prove code generation works by showing generated code + generated tests both pass":

**Best approach**: Option 1 (Fix ggen binary architecture)

**Rationale**:
- The correct-by-construction philosophy requires the generation pipeline to work
- Current ggen binary is broken for this use case
- Fixing it once enables full pipeline validation
- Templates are already written and mostly correct

**Alternative** if time is limited: Option 2 (Adapt to current ggen)
- Quick workaround to show Java 26 + generation works
- Can refine ggen architecture later

---

## Status: Awaiting Direction

**Cannot proceed with `ggen sync` until either**:
1. ggen binary architecture is fixed to implement rule execution (Option 1), OR
2. Templates are rewritten to work with current ggen architecture (Option 2), OR
3. Focus shifts to baseline tests + manual test writing (Option 3)

**Which direction would you like to pursue?**
