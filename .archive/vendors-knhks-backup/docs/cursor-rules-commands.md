# Cursor IDE Rules and Commands Guide

## Overview

Cursor IDE supports two main customization features:
1. **Rules** - System-level instructions that guide AI behavior
2. **Commands** - Reusable workflows triggered with `/` prefix

## Cursor Rules

### Types of Rules

#### 1. Project Rules (Recommended)
**Location**: `.cursor/rules/` directory
**Format**: `.mdc` files (Markdown with metadata)

**Structure**:
```mdc
---
description: Brief description of the rule
globs:
  - "**/*.rs"           # Applies to all Rust files
  - "src/**/*.c"        # Applies to C files in src/
alwaysApply: false      # Whether to always include this rule
---

# Rule content in Markdown
- Use Rust best practices
- Follow 80/20 principle
- No placeholders or TODOs
```

**Metadata Fields**:
- `description`: What the rule does
- `globs`: File patterns where rule applies (optional)
- `alwaysApply`: Always include rule (default: false)

**Creating Rules**:
1. Open Command Palette: `Cmd+Shift+P` (Mac) or `Ctrl+Shift+P` (Windows/Linux)
2. Search: "New Cursor Rule"
3. File created in `.cursor/rules/` with template

**Invoking Rules**:
- Automatic: If `alwaysApply: true` or globs match current file
- Manual: Type `@rule-name` in chat (without `.mdc` extension)

#### 2. User Rules (Global)
**Location**: Cursor Settings > Rules
**Format**: Plain text
**Scope**: All projects

**Example**:
```
Always provide concise responses.
Use production-ready code, no placeholders.
Follow 80/20 principle - focus on critical path.
```

#### 3. Legacy `.cursorrules` (Deprecated)
**Location**: `.cursorrules` file at project root
**Status**: Still supported but deprecated
**Recommendation**: Migrate to Project Rules

### Best Practices for Rules

- **Keep Rules Focused**: One rule per concern
- **Use Descriptive Names**: Clear purpose from filename
- **Provide Examples**: Include code examples in rules
- **Use Glob Patterns**: Target specific file types/directories
- **Version Control**: Commit `.cursor/rules/` directory
- **Modularize**: Break complex rules into smaller ones

### Example Rule Files

**`rust-best-practices.mdc`**:
```mdc
---
description: Rust coding standards
globs:
  - "rust/**/*.rs"
alwaysApply: false
---

- Use `Result<T, E>` for all fallible operations
- Never use `unwrap()` or `expect()` in production code
- Use feature gates: `#[cfg(feature = "...")]`
- Follow Rust naming conventions (snake_case for functions)
- Zero-copy when possible (references over clones)
```

**`c-hot-path.mdc`**:
```mdc
---
description: C hot path optimization guidelines
globs:
  - "src/**/*.c"
  - "include/**/*.h"
alwaysApply: false
---

- Hot path operations must complete in ≤8 ticks
- Use branchless SIMD operations
- 64-byte alignment for SoA arrays
- No bounds checks on hot path (validate at build time)
- Use `static inline` for hot path functions
```

## Cursor Commands

### Creating Commands

**Location**: `.cursor/commands/` directory
**Format**: `.md` files (plain Markdown)

**Structure**:
```markdown
# Command Name

Description of what this command does.

Steps:
1. First step
2. Second step
3. Third step
```

**Usage**: Type `/` in chat to see available commands

### Example Commands

**`/.cursor/commands/run-tests.md`**:
```markdown
Run all test suites and report results.

1. Run C test suites: `make test-chicago-v04`
2. Run Rust tests: `cargo test --workspace`
3. Run performance tests: `make test-performance-v04`
4. Report any failures
```

**`/.cursor/commands/code-review.md`**:
```markdown
Perform code review checklist:

- [ ] No `unwrap()` or `expect()` in production code
- [ ] All errors properly handled with `Result<T, E>`
- [ ] Guard constraints enforced (max_run_len ≤ 8)
- [ ] Performance within 8-tick budget
- [ ] No placeholders or TODOs
- [ ] Tests cover critical paths
```

**`/.cursor/commands/validate-release.md`**:
```markdown
Validate v0.4.0 release readiness:

1. Run validation script: `./scripts/validate_v0.4.0.sh`
2. Check all tests pass
3. Verify documentation complete
4. Confirm performance benchmarks met
5. Review integration tests
```

### Command Best Practices

- **Descriptive Names**: Use clear, action-oriented names
- **Simple Markdown**: Plain text, no complex formatting needed
- **Actionable Steps**: Provide clear, sequential steps
- **Reusable**: Make commands work across different contexts
- **Version Control**: Commit `.cursor/commands/` directory

## Directory Structure

```
project-root/
├── .cursor/
│   ├── rules/
│   │   ├── rust-best-practices.mdc
│   │   ├── c-hot-path.mdc
│   │   └── testing-standards.mdc
│   └── commands/
│       ├── run-tests.md
│       ├── code-review.md
│       └── validate-release.md
└── .cursorrules  (legacy - deprecated)
```

## Keyboard Shortcuts

- **Command Palette**: `Cmd+Shift+P` (Mac) / `Ctrl+Shift+P` (Windows/Linux)
- **New Rule**: Command Palette → "New Cursor Rule"
- **Invoke Rule**: Type `@rule-name` in chat
- **Invoke Command**: Type `/command-name` in chat

## Integration with KNHK Project

### Recommended Rules for KNHK

1. **`80-20-principle.mdc`** - Core team best practices
2. **`no-placeholders.mdc`** - Production-ready code requirements
3. **`hot-path-performance.mdc`** - 8-tick budget enforcement
4. **`error-handling.mdc`** - Proper error handling patterns
5. **`testing-standards.mdc`** - Chicago TDD methodology

### Recommended Commands for KNHK

1. **`validate-v0.4.0.md`** - Run release validation
2. **`run-all-tests.md`** - Execute all test suites
3. **`check-performance.md`** - Verify 8-tick compliance
4. **`code-review-checklist.md`** - Code review checklist
5. **`release-checklist.md`** - Release readiness checklist

## References

- [Cursor Rules Documentation](https://docs.cursor.com/en/context/rules)
- [Cursor Commands Documentation](https://docs.cursor.com/en/agent/chat/commands)
- [Learn Cursor - Rules](https://learncursor.dev/features/rules)

## Migration from `.cursorrules`

If you have a legacy `.cursorrules` file:

1. Create `.cursor/rules/` directory
2. Split `.cursorrules` into focused `.mdc` files
3. Add metadata (description, globs, alwaysApply)
4. Test rules work correctly
5. Remove `.cursorrules` file

## Tips

- **Start Simple**: Begin with a few key rules
- **Iterate**: Refine rules based on AI behavior
- **Test**: Verify rules produce expected results
- **Document**: Add comments explaining why rules exist
- **Share**: Commit rules to version control for team consistency

