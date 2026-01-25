<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [12. ERROR SIGNALS **](#12-error-signals-)
  - [Context](#context)
  - [Connections](#connections)
  - [Implementation](#implementation)
    - [Exit Codes](#exit-codes)
    - [Error Format (Text)](#error-format-text)
    - [Error Format (JSON)](#error-format-json)
  - [Error Categories](#error-categories)
    - [E0001: Manifest Errors](#e0001-manifest-errors)
    - [E0002: Ontology Errors](#e0002-ontology-errors)
    - [E0003: SPARQL Errors](#e0003-sparql-errors)
    - [E0004: Template Errors](#e0004-template-errors)
    - [E0005: File I/O Errors](#e0005-file-io-errors)
    - [E0006: Timeout Errors](#e0006-timeout-errors)
  - [Error Handling in Code](#error-handling-in-code)
  - [CI/CD Integration](#cicd-integration)
  - [The Deeper Pattern](#the-deeper-pattern)
  - [The Andon Connection](#the-andon-connection)
  - [When This Pattern Breaks](#when-this-pattern-breaks)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# 12. ERROR SIGNALS **

*A tool that fails silently is worse than one that fails loudly.*

---

## Context

The generation pipeline has many stages. Each stage can fail in different ways:

- The manifest might be invalid
- An ontology file might not exist
- A SPARQL query might have syntax errors
- A template might reference missing variables
- A file write might fail due to permissions

When something goes wrong, the user needs to know:
- **That** something went wrong
- **What** went wrong
- **Where** it went wrong
- **Why** it might have gone wrong
- **How** to fix it

---

❖ ❖ ❖

**Error messages are the user interface for failures. Good error messages are actionable; bad ones are bewildering.**

The forces:
- Different errors require different information
- Errors must be machine-parseable (for CI/CD)
- Errors must be human-readable (for debugging)
- The system must exit with appropriate codes

Without clear error signals:
- Users don't know what failed
- CI/CD can't categorize failures
- Debugging becomes guesswork
- Trust in the tool erodes

**Therefore:**

**Define a clear error taxonomy with distinct exit codes for each failure category. Format errors with context, explanation, and suggestions. Make errors parseable in both text and JSON formats.**

Error signals should:
- Use distinct exit codes for failure categories
- Include error codes for machine parsing
- Provide human-readable explanations
- Suggest potential fixes
- Point to the source of the problem

---

❖ ❖ ❖

## Connections

This pattern is produced by all other patterns when they fail.

- **[THE SINGLE COMMAND](01-single-command.md)** returns exit codes
- **[VALIDATION GATE](09-validation-gate.md)** produces validation errors
- **[TIMEOUT PROTECTION](11-timeout-protection.md)** produces timeout errors

---

## Implementation

### Exit Codes

| Exit Code | Meaning | Failure Category |
|-----------|---------|------------------|
| 0 | Success | — |
| 1 | Manifest validation error | Configuration |
| 2 | Ontology load error | Data |
| 3 | SPARQL query error | Query |
| 4 | Template rendering error | Template |
| 5 | File I/O error | System |
| 6 | Timeout exceeded | Resource |

### Error Format (Text)

```
error[E0003]: SPARQL query error

  Generation rule 'structs' failed: Parse error at line 5

    5 │ SELCT ?name ?type
      │ ^^^^^ Unknown keyword 'SELCT'

  Help: Did you mean 'SELECT'?

  --> queries/structs.sparql:5:1

Exit code: 3
```

Components:
- **Error level**: `error[E0003]`
- **Category**: `SPARQL query error`
- **Context**: `Generation rule 'structs' failed`
- **Detail**: `Parse error at line 5`
- **Code snippet**: The problematic line
- **Pointer**: `^^^^^` indicating the issue
- **Help**: Suggested fix
- **Location**: `queries/structs.sparql:5:1`
- **Exit code**: Machine-readable status

### Error Format (JSON)

```bash
ggen sync --format json
```

```json
{
  "status": "error",
  "error": {
    "code": "E0003",
    "category": "sparql_query_error",
    "message": "Generation rule 'structs' failed: Parse error at line 5",
    "detail": "Unknown keyword 'SELCT'",
    "location": {
      "file": "queries/structs.sparql",
      "line": 5,
      "column": 1
    },
    "help": "Did you mean 'SELECT'?"
  },
  "exit_code": 3
}
```

---

## Error Categories

### E0001: Manifest Errors

```
error[E0001]: Manifest validation error

  Required field 'project.name' is missing

  --> ggen.toml

  Help: Add 'name = "your-project"' to the [project] section
```

Causes:
- Missing required fields
- Invalid TOML syntax
- Type mismatches

### E0002: Ontology Errors

```
error[E0002]: Ontology load error

  Failed to read ontology: ontology/domian.ttl
  No such file or directory

  --> ggen.toml:6 (ontology.source)

  Help: Check the path. Did you mean 'ontology/domain.ttl'?
```

Causes:
- File not found
- Invalid Turtle syntax
- UTF-8 encoding issues

### E0003: SPARQL Errors

```
error[E0003]: SPARQL query error

  Inference rule 'infer-required' failed: Parse error

    3 │ CONSTRUCT { ?x :isRequired true }
    4 │ WERE {
      │ ^^^^ Unknown keyword 'WERE'

  Help: Did you mean 'WHERE'?
```

Causes:
- Syntax errors
- Unknown prefixes
- Invalid query structure

### E0004: Template Errors

```
error[E0004]: Template rendering error

  Generation rule 'structs' failed: Variable not found

  Template references 'naem' but query provides: [name, type, fields]

  --> templates/struct.tera:7

  Help: Check for typos. Did you mean 'name'?
```

Causes:
- Missing variables
- Invalid filter usage
- Syntax errors

### E0005: File I/O Errors

```
error[E0005]: File I/O error

  Failed to write: src/generated/user.rs
  Permission denied

  Help: Check file permissions on the output directory.
```

Causes:
- Permission denied
- Disk full
- Path too long

### E0006: Timeout Errors

```
error[E0006]: Timeout exceeded

  Inference rule 'complex-join' exceeded timeout of 5000ms

  Help: Increase timeout or optimize the query.
```

Causes:
- Complex queries
- Large datasets
- Pathological patterns

---

## Error Handling in Code

```rust
// Errors carry context through the pipeline
fn execute_generation_rule(&self, rule: &GenerationRule) -> Result<()> {
    let query = self.load_query(&rule.query)
        .map_err(|e| Error::new(&format!(
            "Generation rule '{}' failed: {}",
            rule.name, e
        )))?;

    let results = self.graph.query(&query)
        .map_err(|e| Error::new(&format!(
            "Generation rule '{}' query failed: {}",
            rule.name, e
        )))?;

    // ...
}
```

---

## CI/CD Integration

Exit codes enable workflow logic:

```yaml
- name: Generate code
  run: ggen sync
  id: generate

- name: Handle errors
  if: failure()
  run: |
    case ${{ steps.generate.outputs.exit-code }} in
      1) echo "Configuration error - check ggen.toml" ;;
      2) echo "Ontology error - check TTL files" ;;
      3) echo "Query error - check SPARQL files" ;;
      4) echo "Template error - check Tera files" ;;
      5) echo "I/O error - check permissions" ;;
      6) echo "Timeout - optimize or increase limit" ;;
    esac
```

---

## The Deeper Pattern

ERROR SIGNALS is about **actionable failure**.

An error is only useful if it leads to resolution. Every error message should answer:

1. **What happened?** — The error category and code
2. **Where did it happen?** — File, line, column
3. **Why might it have happened?** — Common causes
4. **How can it be fixed?** — Suggested actions

This is the difference between:

```
Error: parse error
```

And:

```
error[E0003]: SPARQL query error
  Parse error at queries/structs.sparql:5:1
  Unknown keyword 'SELCT'
  Help: Did you mean 'SELECT'?
```

The first tells you something broke. The second tells you how to fix it.

---

## The Andon Connection

Error signals align with the Andon principle from lean manufacturing:

| Signal | Meaning | Action |
|--------|---------|--------|
| 🔴 RED | Error occurred | STOP - fix immediately |
| 🟡 YELLOW | Warning | Investigate before proceeding |
| 🟢 GREEN | Success | Continue |

ggen's exit codes are Andon signals:
- Exit 0 = GREEN (proceed)
- Exit 1-6 = RED (stop and fix)

CI/CD pipelines should treat non-zero exit codes as "stop the line" signals.

---

## When This Pattern Breaks

ERROR SIGNALS struggles when:

- Errors cascade (one error causes many)
- Root cause is obscured by symptoms
- Error messages become too long

ggen manages this through:

- Failing fast (first error stops)
- Contextual wrapping (errors carry chain)
- Structured format (key info prominent)

For complex debugging, combine errors with `--verbose` mode for full traces.

The pattern remains: every failure produces a clear, actionable signal.
