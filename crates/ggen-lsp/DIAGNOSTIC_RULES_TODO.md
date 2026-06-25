# ggen-lsp Diagnostic Rules — Implementation TODO

## Summary

The ggen pack system validates TTL/SPARQL/Tera provision chains at author time via ggen-lsp. The following diagnostic rules were identified during real-world validation and should be implemented to improve the authoring experience.

**Current Status:** GGEN-TPL-001 (template variable mismatch) already exists and caught the primary issues. The diagnostics below are enhancements discovered through testing claude-code-config-lsp generation.

---

## New Diagnostic Rules

### GGEN-SRC-004: Generated module imports don't map to generation rules
**Severity:** ERROR  
**Applies to:** Generated Rust source files (lib.rs, backend.rs, etc.)  
**Detection:** Parse `pub mod` declarations and `use` statements; cross-reference against ggen.toml `output_file` values to verify each declared module has a corresponding generation rule.

**Example Violation:**
```rust
// Generated lib.rs
pub mod capabilities;  // ← ERROR GGEN-SRC-004: no rule generates src/capabilities.rs
```

**Root Cause:** A rule generates `lib.rs` which declares a module, but no other rule's `output_file` produces that module. Common when:
- A template iterates results to emit `pub mod` statements
- But the paired SPARQL query returns empty or incomplete results
- Or a rule was removed from ggen.toml without cleaning up the template

**Message Template:**
```
GGEN-SRC-004: Generated source declares module '{{ module_name }}' but no
generation rule produces 'src/{{ module_name }}.rs'. Either:
1. Add a new generation rule with output_file = "src/{{ module_name }}.rs"
2. Or remove the module declaration from the template that generates this file
```

**Why it matters:** Without this check, `ggen sync` succeeds but subsequent `cargo check` fails with "file not found for module" — catching the issue at generation time saves a compile cycle.

---

### GGEN-SPARQL-VAR-001: SPARQL query variable names don't match template access patterns
**Severity:** WARNING  
**Applies to:** SPARQL query files paired with templates  
**Detection:** Parse template `{{ row.fieldname }}` or `{{ field.prop }}` accesses; extract variable names; compare against SPARQL SELECT output columns.

**Example Violation:**
```sparql
-- declare_constraints.rq
SELECT ?constraint_type ?activity_a ?activity_b
WHERE { ... }
```

```tera
-- declare_model.rs.tera
{% for row in results %}
    {{ row.antecedent }}   <-- ERROR: SELECT returns activity_a, not antecedent
{% endfor %}
```

**Root Cause:** Refactoring — SPARQL query was renamed (`?activity_a` → `?antecedent`) but the template wasn't updated, or vice versa.

**Message Template:**
```
GGEN-SPARQL-VAR-001: Template accesses ?{{ missing_var }} which the paired
SPARQL SELECT does not produce. SELECT columns: {{ columns | join(", ") }}

Fix: Either rename in template to match query, or update query SELECT to include ?{{ missing_var }}
```

**Why it matters:** GGEN-TPL-001 already covers this (template consumes undefined variable). This is a more specific downstream advisory pointing directly at the SPARQL source.

---

### GGEN-TEMPLATE-CONTEXT-001: Template uses undefined Tera variables
**Severity:** ERROR  
**Applies to:** Tera templates  
**Detection:** Static analysis: extract all `{{ var }}` and `{{ var.field }}` references; compare against:
1. Known SPARQL result columns (from paired query)
2. Known Tera context variables (`results`, `loop`, Tera globals)
3. Variables set locally via `{% set var = ... %}`

**Example Violation:**
```tera
{% for row in results %}
    {{ row.law_status }}   <-- ERROR if query doesn't SELECT law_status
    {{ server_name }}      <-- OK if server_name is a known context var
{% endfor %}
```

**Root Cause:** Template author assumes a variable exists but the SPARQL query doesn't provide it, or a context variable was removed.

**Note:** GGEN-TPL-001 already covers this. This would be a formalization/clarification.

---

## Implementation Notes

### Where to Add

1. **GGEN-SRC-004:** `src/analyzers/source_law_analyzer.rs`  
   Add function: `pub fn module_imports_diagnostics(source: &str, manifest: &ggen_core::manifest::Manifest) -> Vec<Diagnostic>`

2. **GGEN-SPARQL-VAR-001:** `src/analyzers/sparql_analyzer.rs`  
   Extend existing analyzer to cross-check template variables (requires loading paired template from manifest).

3. **GGEN-TEMPLATE-CONTEXT-001:** Already covered by GGEN-TPL-001. Document in code comments.

### Data Flow

For GGEN-SRC-004, the detector needs:
1. **File path** (via LSP did_open/did_change) → determines which rule generated it via ggen.toml `output_file` matching
2. **Source content** → regex `pub mod (\w+)` to extract module declarations
3. **Manifest** (load ggen.toml) → scan all rules' `output_file` to build set of generated files
4. **Cross-check** → for each declared module, verify corresponding file is in generated set

For GGEN-SPARQL-VAR-001:
1. Parse template to extract `row.field` references
2. Locate paired SPARQL query (via ggen.toml rule lookup)
3. Parse SPARQL SELECT columns
4. Report mismatches

---

## Test Cases

### GGEN-SRC-004

```rust
#[test]
fn detects_ungenerated_module_import() {
    // Setup: ggen.toml rule "lib" generates src/lib.rs
    // Template contains: pub mod capabilities;
    // But no rule generates src/capabilities.rs
    
    let src = r#"
pub mod backend;
pub mod capabilities;  // <- ERROR: no rule generates this
pub use backend::*;
"#;
    
    let manifest = load_test_manifest();  // lib rule only
    let diags = module_imports_diagnostics(src, &manifest);
    
    assert_eq!(diags.len(), 1);
    assert_eq!(diags[0].code.as_deref(), Some(GGEN_SRC_004));
    assert!(diags[0].message.contains("capabilities"));
}

#[test]
fn clean_when_all_modules_generated() {
    let src = r#"
pub mod capabilities;
pub mod backend;
"#;
    let manifest = load_test_manifest_with_rules(vec![
        ("backend", "src/backend.rs"),
        ("capabilities", "src/capabilities.rs"),
    ]);
    
    let diags = module_imports_diagnostics(src, &manifest);
    assert!(diags.is_empty());
}
```

---

## Related Incidents

This diagnostic suite was identified during the **claude-code-config-lsp build validation** (2026-06-24):

1. **Template variable mismatch:** `declare_model.rs.tera` referenced `row.law_status` which the paired SPARQL query `declare_constraints.rq` didn't provide.  
   **Caught by:** GGEN-TPL-001 (existing)

2. **Missing SPARQL PREFIX:** `config_surfaces.rq` used `rdfs:label` but lacked `PREFIX rdfs:`.  
   **Would be caught by:** GGEN-SPARQL-VAR-001 (proposed)

3. **Ungenerated module import:** Generated `lib.rs` imports `pub mod capabilities;` but no rule generates `src/capabilities.rs`.  
   **Would be caught by:** GGEN-SRC-004 (proposed)

---

## Priority

- **GGEN-SRC-004:** HIGH — Catches compilation-time failures at generation time
- **GGEN-SPARQL-VAR-001:** MEDIUM — Advisory; GGEN-TPL-001 catches the same issue
- **GGEN-TEMPLATE-CONTEXT-001:** LOW — Formalize existing GGEN-TPL-001
