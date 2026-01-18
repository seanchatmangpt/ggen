# ggen UX Visual Guide

**Version**: 5.1.0
**Date**: 2026-01-18

## Overview

This guide showcases the user experience improvements in ggen CLI with real output examples.

---

## 1. Quality Gates Visualization

Quality gates run before code generation to ensure all validations pass:

```
[Quality Gate: Manifest Schema] ✓
[Quality Gate: Ontology Dependencies] ✓
[Quality Gate: SPARQL Validation] ✓
[Quality Gate: Template Validation] ✓
[Quality Gate: File Permissions] ✓
[Quality Gate: Rule Validation] ✓

All Gates: ✅ PASSED → Proceeding to generation phase
```

**Features**:
- Green checkmarks (✓) for passed gates
- Emoji indicator (✅) for overall status
- Clear status line with arrow (→) showing next phase

---

## 2. Dry-Run Preview

Preview what will be generated without writing files:

```bash
$ ggen sync --dry_run true
```

**Output**:
```
[Quality Gate: Manifest Schema] ✓
[Quality Gate: Ontology Dependencies] ✓
[Quality Gate: SPARQL Validation] ✓
[Quality Gate: Template Validation] ✓
[Quality Gate: File Permissions] ✓
[Quality Gate: Rule Validation] ✓

All Gates: ✅ PASSED → Proceeding to generation phase

[DRY RUN] Would sync 1 files:
  ontology-summary.txt (would create)

Inference rules: []
Generation rules: ["example-rule -> ontology-summary.txt"]
```

**Features**:
- `[DRY RUN]` prefix for clarity
- "Would create" action indicators
- Lists affected files and rules
- No actual modifications

---

## 3. Validation-Only Mode

Run validations without generation:

```bash
$ ggen sync --validate_only true
```

**Output**:
```
[Quality Gate: Manifest Schema] ✓
[Quality Gate: Ontology Dependencies] ✓
[Quality Gate: SPARQL Validation] ✓
[Quality Gate: Template Validation] ✓
[Quality Gate: File Permissions] ✓
[Quality Gate: Rule Validation] ✓

All Gates: ✅ PASSED → Proceeding to generation phase

Manifest schema:     PASS ()
Dependencies:     PASS (3/3 checks passed)
Ontology syntax:     PASS (schema/domain.ttl)
SPARQL queries:     PASS (1 queries validated)
Templates:     PASS (1 templates validated)

All validations passed.
```

**Features**:
- Detailed validation results for each check
- Pass counts and file references
- Summary message at the end

---

## 4. Compiler-Style Error Messages

Errors follow Rust compiler conventions with error codes and help text:

### Missing Manifest
```
ERROR: CLI execution failed: Command execution failed: error[E0001]: Manifest not found
  --> /tmp/ggen-error-test/nonexistent.toml
  |
  = help: Create a ggen.toml manifest file or specify path with --manifest
```

### TOML Parse Error
```
ERROR: CLI execution failed: Command execution failed: error[E0001]: Manifest parse error
  --> ggen.toml
  |
  = error: TOML parse error: TOML parse error at line 1, column 9
  |
1 | invalid toml content [[[
  |         ^
key with no value, expected `=`

  = help: Check ggen.toml syntax and required fields
```

### Invalid Format
```
error[E0005]: Invalid output format 'yaml'
  |
  = help: Valid formats: text, json, yaml
  = note: defaulting to 'text'
```

**Error Code Catalog**:
- `E0001` - Manifest errors (parse, validation, not found)
- `E0002` - Dependency errors (circular dependencies, missing imports)
- `E0003` - Pipeline execution errors (SPARQL, template rendering)
- `E0005` - Output format errors
- `E0020` - Pre-flight validation errors
- `E0028` - Filesystem errors

---

## 5. Verbose Mode Diagnostics

Get detailed execution information:

```bash
$ ggen sync --verbose true
```

**Output Sample**:
```
⚠ Pre-flight warning: error[E0020]: Pre-flight validation failed
  |
  = 1 check(s) failed:
    - Disk space: error[E0028]: Cannot get filesystem stats
  -->
  |
  = Error: ENOENT: No such file or directory
  = help: Check if path exists and is accessible

[Quality Gate: Manifest Schema] ✓
[Quality Gate: Ontology Dependencies] ✓
...

Pre-flight checks: 1 validations, 1 high-risk items detected
⚠ Warning: 0 critical failures, 1 warnings in packages
ℹ Manifest: ggen.toml
ℹ Using incremental cache
```

**Symbols**:
- ⚠ (Yellow) - Warnings
- ℹ (Blue) - Information
- ✓ (Green) - Success
- ✗ (Red) - Error

---

## 6. Init Command Output

Initialize a new project:

```bash
$ ggen init
```

**Output** (success):
```json
{
  "status": "success",
  "project_dir": ".",
  "files_created": [
    "ggen.toml",
    "schema/domain.ttl",
    "Makefile",
    "templates/example.txt.tera",
    "scripts/startup.sh",
    ".gitignore",
    "README.md"
  ],
  "directories_created": [
    "schema",
    "templates",
    "src/generated",
    "scripts"
  ],
  "transaction": {
    "total_files": 7,
    "backups_created": 0,
    "committed": true
  },
  "next_steps": [
    "Run 'make setup' to initialize your project",
    "Edit schema/domain.ttl to define your domain model",
    "Create Tera templates in templates/ for your target languages",
    "Run 'make build' to generate code from your ontology"
  ]
}
```

**Output** (already initialized):
```json
{
  "status": "error",
  "error": "ggen project already initialized here. Use --force to reinitialize.",
  "next_steps": ["Run 'make build' to regenerate code"]
}
```

**Output** (with --force):
```json
{
  "status": "success",
  "files_overwritten": [
    "ggen.toml",
    "schema/domain.ttl",
    "Makefile",
    "templates/example.txt.tera",
    "scripts/startup.sh"
  ],
  "files_preserved": [".gitignore", "README.md"],
  "transaction": {
    "total_files": 5,
    "backups_created": 5,
    "committed": true
  },
  "warning": "Overwrote 5 file(s); Preserved 2 user file(s)."
}
```

---

## 7. Help Text

Comprehensive help documentation:

```bash
$ ggen init --help
```

**Key Sections**:
- Command description
- What gets created
- Usage examples
- Flag documentation
- Next steps after initialization

```bash
$ ggen sync --help
```

**Key Sections**:
- Pipeline flow diagram
- All flags with descriptions
- Flag combination examples
- Safety notes with warnings
- Common workflow examples
- Links to detailed documentation

---

## 8. Progress Indicators (Code)

While not visible in the output samples above (due to JSON output mode), the code implements:

```rust
// From executor.rs
let mut progress = ProgressIndicator::new(show_progress);

progress.start_spinner("Loading manifest and cache...");
// ... loading logic ...
progress.finish_with_message("Loaded manifest: my-project");

progress.start_spinner("Loading ontology and running inference...");
// ... pipeline execution ...
progress.finish_with_message("Loaded 42 triples, ran 3 inference rules");
```

**Features**:
- Animated spinner: `⠁⠂⠄⡀⢀⠠⠐⠈`
- Success finish: `✓ Message`
- Error finish: `✗ Message`
- Automatic cleanup

---

## Color Scheme Reference

| Status | Symbol | Color | Usage |
|--------|--------|-------|-------|
| Success | ✓ | Green | Completed operations, passed checks |
| Error | ✗ | Red | Failed operations, critical errors |
| Warning | ⚠ | Yellow | Non-critical warnings, advisories |
| Info | ℹ | Blue | Informational messages, metadata |
| Gate Pass | ✅ | Green | All quality gates passed |

---

## JSON vs Text Output

### JSON Format
```bash
$ ggen sync --format json --dry_run true
```
**Output**: Structured JSON (after stderr messages)
```json
{"duration_ms":3,"files":[{"action":"would create","path":"ontology-summary.txt","size_bytes":0}],
 "files_synced":0,"generation_rules_executed":0,"inference_rules_executed":0,"status":"success"}
```

**Use Cases**:
- CI/CD pipelines
- Programmatic parsing
- Log aggregation
- API responses

### Text Format (default)
```bash
$ ggen sync --dry_run true
```
**Output**: Human-readable text
```
[DRY RUN] Would sync 1 files:
  ontology-summary.txt (would create)

Inference rules: []
Generation rules: ["example-rule -> ontology-summary.txt"]
```

**Use Cases**:
- Interactive terminal sessions
- Developer workflows
- Manual inspection
- Documentation examples

---

## Best Practices

### Safe Workflows

1. **Preview before generation**:
   ```bash
   ggen sync --dry_run true
   ```

2. **Validate before syncing**:
   ```bash
   ggen sync --validate_only true
   ```

3. **Use verbose for diagnostics**:
   ```bash
   ggen sync --verbose true
   ```

4. **Force with audit trail**:
   ```bash
   ggen sync --force true --audit true
   ```

### Error Recovery

1. **Check error code** (E0001, E0002, etc.)
2. **Read help text** (= help: section)
3. **Follow suggested fixes**
4. **Verify with --dry_run**

### CI/CD Integration

1. **Use JSON output**:
   ```bash
   ggen sync --format json > result.json
   ```

2. **Validate in CI**:
   ```bash
   ggen sync --validate_only true || exit 1
   ```

3. **Check exit codes**:
   - 0 = Success
   - 1 = Manifest validation error
   - 2 = Ontology load error
   - 3 = SPARQL query error
   - 4 = Template rendering error
   - 5 = File I/O error
   - 6 = Timeout exceeded

---

## Conclusion

The ggen CLI provides a professional user experience with:

- ✅ Clear, actionable error messages
- ✅ Visual progress indicators
- ✅ Colored status output
- ✅ Comprehensive help text
- ✅ Safe preview modes
- ✅ Multiple output formats
- ✅ Consistent UX patterns

All features follow industry best practices and provide an intuitive, productive developer experience.

---

**Document Version**: 1.0
**Last Updated**: 2026-01-18
**Related**: UX_END_TO_END_VERIFICATION_RECEIPT.md
