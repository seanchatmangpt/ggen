# Error Context Improvements

## Executive Summary

Analysis of the ggen codebase reveals that **most error messages already have good context**. However, 10 high-impact user-facing errors can be improved with more actionable guidance.

## Current State

The codebase uses `ggen_utils::Error` with:
- Error chaining support (source errors)
- Context attachment via `.context()` method
- Formatted error messages with `format!()`
- Helper methods like `Error::invalid_input()`, `Error::file_not_found()`

**Overall Grade: B+** - Most errors are adequate, but some lack actionable guidance.

## 10 High-Impact Improvements

### 1. Merge Marker Validation Error

**File:** `crates/ggen-core/src/codegen/merge.rs:104-110`

**Before:**
```rust
return Err(Error::new(
    "Invalid merge markers: ======= must come after <<<<<<< GENERATED",
));
```

**After:**
```rust
return Err(Error::new(&format!(
    "error[E0001]: Invalid merge marker order in '{}'\n  |\n  = help: Merge markers must appear in this order:\n  =   1. <<<<<<< GENERATED\n  =   2. =======\n  =   3. >>>>>>> MANUAL\n  = help: Found '=======' at line {} before GENERATED marker at line {}",
    path.display(),
    markers.manual_start,
    markers.generated_start
)));
```

**Impact:** Users editing generated files manually need to know the exact marker order.

---

### 2. Condition Query Type Error

**File:** `crates/ggen-core/src/codegen/pipeline.rs:456-458`

**Before:**
```rust
_ => Err(Error::new(
    "Condition query must be ASK query (got SELECT/CONSTRUCT)",
)),
```

**After:**
```rust
_ => Err(Error::new(&format!(
    "error[E0002]: Condition query must return boolean (ASK), not results\n  --> query used in: '{}'\n  |\n  = help: Change SPARQL query from SELECT/CONSTRUCT to ASK:\n  =   ASK {{ ... }}\n  = help: Conditions must return true/false, not result rows",
    rule_name
))),
```

**Impact:** Users writing SPARQL queries need to know condition queries must be ASK queries.

---

### 3. Generation Rule Query Type Error

**File:** `crates/ggen-core/src/codegen/pipeline.rs:541-544`

**Before:**
```rust
return Err(Error::new(&format!(
    "Generation rule '{}' query must be SELECT (got CONSTRUCT/ASK)",
    rule.name
)));
```

**After:**
```rust
return Err(Error::new(&format!(
    "error[E0003]: Generation rules require SELECT queries (not CONSTRUCT/ASK)\n  --> rule: '{}'\n  |\n  = help: Change SPARQL query to SELECT to return result rows for template rendering\n  = help: Example: SELECT ?var WHERE {{ ... }}",
    rule.name
)));
```

**Impact:** Users need to know generation rules use SELECT for template data.

---

### 4. Empty Generated Content Validation

**File:** `crates/ggen-core/src/codegen/pipeline.rs:883-887`

**Before:**
```rust
return Err(Error::new(&format!(
    "Validation failed for rule '{}': Generated content is empty for path '{}'",
    rule_id,
    path.display()
)));
```

**After:**
```rust
return Err(Error::new(&format!(
    "error[E0004]: Generated content is empty\n  --> rule: '{}', output: '{}'\n  |\n  = help: Check if:\n  =   1. SPARQL query returned results (test in separate SPARQL tool)\n  =   2. Template has content (not empty file)\n  =   3. Template variables match query result columns\n  = help: Use 'ggen validate --dry-run' to see query results",
    rule_id,
    path.display()
)));
```

**Impact:** Users debugging empty generation need a troubleshooting checklist.

---

### 5. File Size Validation Error

**File:** `crates/ggen-core/src/codegen/pipeline.rs:894-899`

**Before:**
```rust
return Err(Error::new(&format!(
    "Validation failed for rule '{}': Generated file size ({} bytes) exceeds 10MB limit for path '{}'",
    rule_id,
    size_bytes,
    path.display()
)));
```

**After:**
```rust
return Err(Error::new(&format!(
    "error[E0005]: Generated file too large ({} bytes, limit: 10MB)\n  --> rule: '{}', output: '{}'\n  |\n  = help: Consider splitting into multiple smaller files\n  = help: Or adjust template to reduce output size\n  = help: Check for unexpected data duplication in SPARQL results",
    size_bytes,
    rule_id,
    path.display()
)));
```

**Impact:** Users hitting size limits need actionable solutions.

---

### 6. Directory Traversal Validation Error

**File:** `crates/ggen-core/src/codegen/pipeline.rs:905-909`

**Before:**
```rust
return Err(Error::new(&format!(
    "Validation failed for rule '{}': Path contains directory traversal pattern (..) in '{}'",
    rule_id,
    path.display()
)));
```

**After:**
```rust
return Err(Error::new(&format!(
    "error[E0006]: Directory traversal pattern detected in output path\n  --> rule: '{}', path: '{}'\n  |\n  = help: Remove '../' or '..\\' from template output path\n  = help: Use relative paths from base directory without '..'\n  = security: Directory traversal is blocked for security reasons",
    rule_id,
    path.display()
)));
```

**Impact:** Security errors should explain why they exist and how to fix.

---

### 7. Watch Channel Disconnected Error

**File:** `crates/ggen-core/src/codegen/watch.rs:214`

**Before:**
```rust
Err(Error::new("Watch channel disconnected"))
```

**After:**
```rust
Err(Error::new(&format!(
    "error[E0007]: Watch system stopped unexpectedly\n  |\n  = help: This usually indicates a crash in the watch thread\n  = help: Check logs above for panic or error messages\n  = help: Try restarting: ggen sync --watch\n  = help: If issue persists, run without --watch to debug",
)))
```

**Impact:** Users experiencing watch failures need recovery steps.

---

### 8. Dependency Check Failed Error

**File:** `crates/ggen-core/src/codegen/executor.rs:349-352`

**Before:**
```rust
return Err(Error::new(&format!(
    "error[E0002]: {} dependency checks failed\n  |\n  = help: Fix missing files or imports before syncing",
    dep_validator.failed_checks
)));
```

**After:**
```rust
return Err(Error::new(&format!(
    "error[E0002]: {} dependency validation checks failed\n  |\n  = help: Common issues:\n  =   1. Query file not found: Check ontology.source and ontology.imports paths\n  =   2. Template file not found: Check generation.rules[].template paths\n  =   3. Import cycle: Check if imported files reference each other\n  = help: Run 'ggen validate' for detailed dependency analysis",
    dep_validator.failed_checks
)));
```

**Impact:** Users need specific things to check when dependencies fail.

---

### 9. Template Read Error

**File:** `crates/ggen-core/src/codegen/pipeline.rs:558-562`

**Before:**
```rust
let content = std::fs::read_to_string(&template_path).map_err(|e| {
    Error::new(&format!(
        "Failed to read template file '{}': {}",
        template_path.display(),
        e
    ))
})?;
```

**After:**
```rust
let content = std::fs::read_to_string(&template_path).map_err(|e| {
    Error::new(&format!(
        "error[E0008]: Failed to read template file\n  --> path: '{}'\n  |\n  = error: {}\n  = help: Check if file exists and is readable\n  = help: Verify template path in ggen.toml is relative to project root",
        template_path.display(),
        e
    ))
})?;
```

**Impact:** Template read errors should guide users to check paths.

---

### 10. Watch Path Not Found Error

**File:** `crates/ggen-core/src/codegen/watch.rs:106-109`

**Before:**
```rust
return Err(Error::new(&format!(
    "Watch path does not exist: {}",
    path.display()
)));
```

**After:**
```rust
return Err(Error::new(&format!(
    "error[E0009]: Watch path does not exist\n  --> path: '{}'\n  |\n  = help: Create the directory or update ggen.toml to remove from watch list\n  = help: Watch paths are collected from: ontology.source, ontology.imports, and generation.rules[].query",
    path.display()
)));
```

**Impact:** Users need to know where watch paths come from and how to fix missing paths.

---

## Implementation Priority

1. **High Priority** (Users see frequently):
   - #3: Generation rule query type error
   - #4: Empty generated content
   - #8: Dependency check failed
   - #9: Template read error

2. **Medium Priority** (Less frequent but important):
   - #1: Merge marker validation
   - #2: Condition query type
   - #5: File size validation
   - #10: Watch path not found

3. **Low Priority** (Edge cases):
   - #6: Directory traversal (security feature, rarely triggered)
   - #7: Watch channel disconnect (system issue, rare)

## Testing Strategy

For each improved error message:

1. **Unit test**: Verify error message contains expected substrings
2. **Integration test**: Trigger the error and capture output
3. **User acceptance**: Show error to users and ask "What would you do?"

Example test:
```rust
#[test]
fn test_empty_generation_error_message() {
    let err = validate_generated_output("", Path::new("test.txt"), "rule_1")
        .unwrap_err();

    let msg = err.to_string();
    assert!(msg.contains("error[E0004]"));
    assert!(msg.contains("rule: 'rule_1'"));
    assert!(msg.contains("SPARQL query returned results"));
    assert!(msg.contains("ggen validate --dry-run"));
}
```

## Metrics

Track before/after:
- **Time to resolution**: How long users take to fix the error
- **Support tickets**: Number of issues related to each error
- **User satisfaction**: Survey feedback on error helpfulness

## Conclusion

The ggen codebase already has **better-than-average error context**. These 10 improvements focus on adding **actionable guidance** to help users fix issues without consulting documentation or support.

**Estimated effort**: 2-3 hours to implement all 10 improvements.
**Estimated impact**: 30-40% reduction in support requests for these errors.
