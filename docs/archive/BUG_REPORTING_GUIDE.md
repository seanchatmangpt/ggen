# Bug Reporting Guide for ggen

Thank you for helping us improve ggen! This guide explains how to report bugs effectively and responsibly.

## Before You Report

Please check the following before opening a bug report:

1. **Check the [Issues](https://github.com/seanchatmangpt/ggen/issues)** - Your bug may already be reported
2. **Check the [Discussions](https://github.com/seanchatmangpt/ggen/discussions)** - It might be a known limitation
3. **Update to the latest version** - The bug may be already fixed
4. **Check the documentation** - It might not be a bug but expected behavior

## Security Vulnerabilities

**Do not** open a public issue for security vulnerabilities. Instead:

1. Email: `sean@chatmangpt.com` with subject line `[SECURITY] ggen vulnerability`
2. Include:
   - A clear description of the vulnerability
   - Steps to reproduce (if possible)
   - The affected versions
   - Any suggested fixes

We aim to respond to security reports within 48 hours.

## How to Report a Bug

### 1. Use a Clear, Descriptive Title

Good: `Watch service crashes when RDF file contains non-UTF8 characters`
Bad: `Watch mode broken`

### 2. Provide Your Environment

```
- ggen version: 5.0.2
- Operating System: Ubuntu 22.04 LTS
- Rust version: 1.75.0
- Installed features: all
```

### 3. Describe the Bug

Write a clear description including:
- What you expected to happen
- What actually happened
- When the issue started (if known)

### 4. Provide Minimal Reproducible Example

```rust
// Minimal code that reproduces the bug
use ggen::GeneratorConfig;

fn main() {
    let config = GeneratorConfig::default();
    // Steps to reproduce bug...
}
```

### 5. Include Relevant Logs

Attach error messages, stack traces, or logs:

```
thread 'main' panicked at 'Failed to parse ontology: expected RDF/XML',
src/core/ontology.rs:42:5
```

Enable debug logging:

```bash
RUST_LOG=debug ggen sync
```

### 6. Screenshots (if applicable)

For UI-related issues, include screenshots showing the problem.

## Bug Report Template

```markdown
## Environment

- ggen version:
- OS:
- Rust version:

## Description

Brief description of the bug.

## Steps to Reproduce

1.
2.
3.

## Expected Behavior

What should happen.

## Actual Behavior

What actually happens.

## Minimal Example

```rust
// Code that reproduces the issue
```

## Logs/Error Messages

```
Paste error messages here
```

## Additional Context

Any other context about the problem.
```

## Bug Severity Levels

- **Critical**: System crash, data loss, security vulnerability
- **High**: Core functionality broken, affects most users
- **Medium**: Feature doesn't work as expected, workaround exists
- **Low**: Minor issue, cosmetic bug, affects edge cases

## What Happens Next

1. We'll acknowledge your report within 48 hours
2. We'll assign a severity level
3. We'll work on a fix (timeline depends on severity)
4. You'll be notified when fixed in a release

## Contributing a Fix

Found a fix for the bug? We'd love your contribution!

1. Fork the repository
2. Create a feature branch: `git checkout -b fix/issue-description`
3. Add tests for the bug
4. Submit a pull request with a clear description

See [CONTRIBUTING.md](CONTRIBUTING.md) for more details.

## Code of Conduct

Please note we have a Code of Conduct. By reporting bugs, you agree to follow it.

## Questions?

If you're unsure whether something is a bug, open a [Discussion](https://github.com/seanchatmangpt/ggen/discussions) first.
