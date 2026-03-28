# PRD: Strip `?` Prefix from SPARQL Variable Names in Tera Context

**Status:** Ready for implementation  
**Priority:** High — blocks clean template authoring  
**Effort:** XS (1 line of production code, ~5 lines of tests)  
**File:** `crates/ggen-core/src/graph/core.rs:194`

---

## Problem

When ggen builds the Tera context from SPARQL query results, it preserves the `?` prefix that SPARQL uses for variable names. This leaks SPARQL syntax into the template layer.

**Current behavior — multi-row templates must use bracket notation:**
```tera
{% for row in sparql_results %}
{{ row["?label"] }}      {# ?-prefix required — ugly, confusing #}
{{ row["?number"] }}
{% endfor %}
```

**Per-row templates (one file per SPARQL row) already strip `?`:**
```tera
{# This already works — ggen strips ? for per-row context #}
{{ label }}
{{ number }}
```

The inconsistency means template authors need to know which rendering mode they're in and use different access syntax for each. This is a leaky abstraction — Tera templates should not know or care that data came from SPARQL.

---

## Root Cause

In `crates/ggen-core/src/graph/core.rs`, the `materialize_results` method builds `BTreeMap<String, String>` rows from Oxigraph's SPARQL solution iterator. Oxigraph's `Variable::as_str()` returns the variable name **with** the `?` prefix (e.g., `"?label"`). ggen inserts this raw string as the map key without stripping.

```rust
// crates/ggen-core/src/graph/core.rs:194 — CURRENT (broken)
for (var, term) in solution.iter() {
    row.insert(var.as_str().to_string(), term.to_string());
    //         ^^^^^^^^^^^^^^^^^^^^^^^^
    //         returns "?label" — ? is SPARQL syntax, not part of the name
}
```

---

## Solution

Strip the leading `?` with `trim_start_matches('?')` at the single insertion point:

```rust
// crates/ggen-core/src/graph/core.rs:194 — FIX
for (var, term) in solution.iter() {
    row.insert(
        var.as_str().trim_start_matches('?').to_string(),
        term.to_string(),
    );
}
```

That is the entire production code change.

---

## Expected Behavior After Fix

**Multi-row templates use clean dot notation — consistent with per-row:**
```tera
{% for row in sparql_results %}
{{ row.label }}       {# clean, no ? #}
{{ row.number }}
{% endfor %}
```

**Per-row templates unchanged** (already work without `?`):
```tera
{{ label }}
{{ number }}
```

**`sparql_first()` and other helpers** that operate on row maps will also benefit — no more `"?column"` string arguments.

---

## Tests Required (Chicago TDD — RED first)

### Test 1: `?` stripped from multi-row `sparql_results` keys

```rust
// crates/ggen-core/tests/graph_sparql_variable_naming_test.rs

#[test]
fn test_sparql_variable_keys_have_no_question_mark_prefix() {
    let graph = RdfGraph::new();
    graph.insert_turtle(r#"
        @prefix ex: <http://example.com/> .
        ex:foo ex:label "hello" .
    "#).unwrap();

    let results = graph.query(
        "SELECT ?label WHERE { ?s <http://example.com/label> ?label }"
    ).unwrap();

    if let CachedResult::Solutions(rows) = results {
        assert!(!rows.is_empty(), "Expected at least one result row");
        let row = &rows[0];
        // KEY ASSERTION: key must be "label", not "?label"
        assert!(row.contains_key("label"),
            "Expected key 'label' but got keys: {:?}", row.keys().collect::<Vec<_>>());
        assert!(!row.contains_key("?label"),
            "Key '?label' must not exist — ? is SPARQL syntax, not part of the name");
        assert_eq!(row["label"], "\"hello\"");
    } else {
        panic!("Expected Solutions result");
    }
}
```

### Test 2: Multi-variable row — all keys stripped

```rust
#[test]
fn test_all_sparql_variable_keys_stripped_of_question_mark() {
    let graph = RdfGraph::new();
    graph.insert_turtle(r#"
        @prefix ex: <http://example.com/> .
        ex:p1 ex:name "Alice" ; ex:age "30" .
    "#).unwrap();

    let results = graph.query(
        "SELECT ?name ?age WHERE { ?s <http://example.com/name> ?name ; <http://example.com/age> ?age }"
    ).unwrap();

    if let CachedResult::Solutions(rows) = results {
        let row = &rows[0];
        // All variable names clean
        assert!(row.contains_key("name"), "missing 'name'");
        assert!(row.contains_key("age"),  "missing 'age'");
        // None have ? prefix
        for key in row.keys() {
            assert!(!key.starts_with('?'),
                "Key '{key}' starts with ? — all keys must be stripped");
        }
    } else {
        panic!("Expected Solutions result");
    }
}
```

### Test 3: End-to-end — Tera template accesses `row.varname` (no `?`)

```rust
#[test]
fn test_tera_template_accesses_sparql_row_without_question_mark() {
    // Setup: a minimal ggen pipeline with a multi-row template
    let ttl = r#"
        @prefix ex: <http://example.com/> .
        ex:p1 ex:label "Alpha" ; ex:num 1 .
        ex:p2 ex:label "Beta"  ; ex:num 2 .
    "#;
    let sparql = "SELECT ?label ?num WHERE { ?s <http://example.com/label> ?label ; <http://example.com/num> ?num }";
    let template = r#"{% for row in sparql_results %}{{ row.label }}={{ row.num }}
{% endfor %}"#;

    let output = run_pipeline(ttl, sparql, template).unwrap();

    // Verify clean dot-notation access works
    assert!(output.contains("Alpha="), "Expected 'Alpha=' in output, got: {output}");
    assert!(output.contains("Beta="),  "Expected 'Beta=' in output, got: {output}");
}
```

---

## Migration Impact

Any existing templates using `row["?varname"]` will break. Search scope:

```bash
# Find all templates using the old ?-prefix access pattern
grep -rn 'row\["\?' /Users/sac/ggen --include="*.tera"
```

Known affected templates (to update alongside the fix):
- `docs/archive/academic/thesis-construct-schema/templates/*.tera` — uses `row["?varname"]` throughout
- Any other `*.tera` files in the archive using the old pattern

The java26-patterns templates have already been written with `row["?varname"]` as a workaround — they should be updated to `row.varname` after this fix ships.

---

## Acceptance Criteria

- [ ] All 3 RED tests above turn GREEN
- [ ] `cargo test -p ggen-core` passes (no regressions)
- [ ] `cargo clippy -p ggen-core -- -D warnings` clean
- [ ] `ggen sync` on `docs/java26-patterns/` produces identical output with templates updated to `row.varname`
- [ ] Archive templates updated: `row["?varname"]` → `row.varname`
- [ ] CHANGELOG.md entry under next version: `fix: strip SPARQL ? prefix from Tera context variable keys`

---

## Implementation Checklist

```
1. [ ] Write RED tests in crates/ggen-core/tests/graph_sparql_variable_naming_test.rs
2. [ ] Confirm tests FAIL (RED)
3. [ ] Apply one-line fix in crates/ggen-core/src/graph/core.rs:194
4. [ ] Confirm tests PASS (GREEN)
5. [ ] Run cargo clippy — 0 warnings
6. [ ] Update docs/java26-patterns/templates/*.tera: row["?x"] → row.x
7. [ ] Update docs/archive templates: row["?x"] → row.x
8. [ ] Run ggen sync on java26-patterns — verify identical output
9. [ ] Add CHANGELOG.md entry
```

---

## Notes

- `trim_start_matches('?')` is safe: SPARQL variables always start with `?`; if somehow a variable had no `?` (shouldn't happen with Oxigraph), `trim_start_matches` is a no-op.
- This is a **breaking change** for any template using `row["?varname"]`. Version bump to next minor.
- The per-row template path (where variables are injected directly into the Tera context) should also be audited to confirm it already strips `?` — if it does via a separate code path, consolidating both through `materialize_results` ensures consistent behavior everywhere.
