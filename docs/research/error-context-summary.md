# Error Context Improvements - Summary

## Task Completed

Improved error messages in the ggen codebase to provide **actionable context** for users encountering errors.

## Analysis Findings

**Current State:** The codebase already has **good error context** (Grade: B+). Most errors include:
- What operation failed
- What input caused the failure
- File paths and line numbers
- Error codes (E0001, E0002, etc.)

**Improvement Focus:** Add **how to fix** guidance to the most common user-facing errors.

## Implemented Improvements (4 High-Priority)

### 1. Generation Rule Query Type Error (`pipeline.rs:541`)
**Before:**
```
Generation rule 'my_rule' query must be SELECT (got CONSTRUCT/ASK)
```

**After:**
```
error[E0003]: Generation rules require SELECT queries (not CONSTRUCT/ASK)
  --> rule: 'my_rule'
  |
  = help: Change SPARQL query to SELECT to return result rows for template rendering
  = help: Example: SELECT ?var WHERE { ... }
```

**Impact:** Users now know exactly what to change and see an example.

---

### 2. Empty Generated Content Error (`pipeline.rs:883`)
**Before:**
```
Validation failed for rule 'my_rule': Generated content is empty for path 'output.txt'
```

**After:**
```
error[E0004]: Generated content is empty
  --> rule: 'my_rule', output: 'output.txt'
  |
  = help: Check if:
  =   1. SPARQL query returned results (test in separate SPARQL tool)
  =   2. Template has content (not empty file)
  =   3. Template variables match query result columns
  = help: Use 'ggen validate --dry-run' to see query results
```

**Impact:** Users get a troubleshooting checklist instead of a generic error.

---

### 3. Dependency Validation Failed Error (`executor.rs:349`)
**Before:**
```
error[E0002]: 3 dependency checks failed
  |
  = help: Fix missing files or imports before syncing
```

**After:**
```
error[E0002]: 3 dependency validation checks failed
  |
  = help: Common issues:
  =   1. Query file not found: Check ontology.source and ontology.imports paths
  =   2. Template file not found: Check generation.rules[].template paths
  =   3. Import cycle: Check if imported files reference each other
  = help: Run 'ggen validate' for detailed dependency analysis
```

**Impact:** Users see specific things to check instead of vague guidance.

---

### 4. Template Read Error (`pipeline.rs:558`)
**Before:**
```
Failed to read template file '/path/to/template.txt': No such file or directory
```

**After:**
```
error[E0008]: Failed to read template file
  --> path: '/path/to/template.txt'
  |
  = error: No such file or directory
  = help: Check if file exists and is readable
  = help: Verify template path in ggen.toml is relative to project root
```

**Impact:** Users learn that paths should be relative to project root.

---

## Files Modified

1. `crates/ggen-core/src/codegen/pipeline.rs` (3 errors improved)
2. `crates/ggen-core/src/codegen/executor.rs` (1 error improved)

## Additional 6 Improvements Documented

The following improvements are documented in `/Users/sac/ggen/docs/research/error-context-improvements.md` but not yet implemented:

5. Merge marker validation error (`merge.rs:104`)
6. Condition query type error (`pipeline.rs:456`)
7. File size validation error (`pipeline.rs:894`)
8. Directory traversal error (`pipeline.rs:905`)
9. Watch channel disconnected error (`watch.rs:214`)
10. Watch path not found error (`watch.rs:106`)

**Estimated effort:** 1 hour to implement remaining 6 improvements.

## Error Message Pattern

All improved errors follow this structure:

```
error[EXXXX]: Brief description
  --> location: 'file/path'
  |
  = error: Technical details (optional)
  = help: Actionable guidance
  = help: More guidance (optional)
  = security: Security rationale (for security errors)
```

This pattern matches Rust compiler error output, making it familiar to Rust developers.

## Testing Strategy

For each error improvement, add a unit test:

```rust
#[test]
fn test_error_message_contains_helpful_guidance() {
    let err = trigger_error().unwrap_err();
    let msg = err.to_string();

    // Check error code
    assert!(msg.contains("error[EXXXX]"));

    // Check location info
    assert!(msg.contains("rule:") || msg.contains("path:"));

    // Check help text
    assert!(msg.contains("= help:"));
}
```

## Metrics to Track

- **Time to resolution**: How long users take to fix each error type
- **Support tickets**: Number of issues related to each error
- **User satisfaction**: Survey feedback on error helpfulness

## Conclusion

**Implemented:** 4 high-priority error message improvements
**Documented:** 10 total improvements with priority ranking
**Impact:** Users will spend less time debugging and more time being productive

The codebase already had good error context. These improvements add **actionable guidance** that helps users fix issues without consulting documentation or support.

## References

- Full analysis: `/Users/sac/ggen/docs/research/error-context-improvements.md`
- Error infrastructure: `/Users/sac/ggen/crates/ggen-utils/src/error.rs`
- HDOC framework: Error context is part of the ggen HDOC (High-DevOps-Copilot) quality framework
