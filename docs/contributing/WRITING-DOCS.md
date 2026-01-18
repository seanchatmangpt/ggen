<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [Writing Documentation for ggen](#writing-documentation-for-ggen)
  - [Quick Start for Documentation Authors](#quick-start-for-documentation-authors)
    - [1. Set Up Live Validation](#1-set-up-live-validation)
    - [2. Write Documentation](#2-write-documentation)
    - [3. Save and Watch](#3-save-and-watch)
  - [Documentation Structure (Diataxis)](#documentation-structure-diataxis)
    - [1. **Tutorials** (`docs/tutorials/`)](#1-tutorials-docstutorials)
    - [2. **How-to Guides** (`docs/how-to/`)](#2-how-to-guides-docshow-to)
    - [3. **Explanations** (`docs/explanations/`)](#3-explanations-docsexplanations)
    - [4. **Reference** (`docs/reference/`)](#4-reference-docsreference)
  - [Writing Guidelines](#writing-guidelines)
    - [Commands Must Work](#commands-must-work)
    - [Use Real Examples](#use-real-examples)
    - [Show Expected Output](#show-expected-output)
    - [JavaScript, Zod, JSDoc (NOT TypeScript)](#javascript-zod-jsdoc-not-typescript)
  - [Adding Validation for Your Documentation](#adding-validation-for-your-documentation)
    - [If You Write a Tutorial](#if-you-write-a-tutorial)
    - [If You Write a How-to Guide](#if-you-write-a-how-to-guide)
    - [If You Write an Explanation](#if-you-write-an-explanation)
  - [Testing Your Documentation Locally](#testing-your-documentation-locally)
    - [Before Committing](#before-committing)
    - [While Writing](#while-writing)
    - [Individual Test](#individual-test)
  - [CI/CD Integration](#cicd-integration)
  - [Common Mistakes](#common-mistakes)
    - [1. Outdated Commands](#1-outdated-commands)
    - [2. Broken Links](#2-broken-links)
    - [3. Missing Prerequisites](#3-missing-prerequisites)
  - [Documentation Checklist](#documentation-checklist)
  - [Questions?](#questions)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Writing Documentation for ggen

**TL;DR**: Write docs with live validation feedback. Every example must work.

## Quick Start for Documentation Authors

### 1. Set Up Live Validation

Open two terminal windows:

**Terminal 1**: Watch mode (auto-validates on save)
```bash
./scripts/validate-docs/watch-docs-live.sh
```

**Terminal 2**: Your editor
```bash
vim docs/getting-started/my-new-guide.md
# or
code docs/getting-started/my-new-guide.md
```

### 2. Write Documentation

**Every code example MUST be testable**. If you write:

````markdown
## Example: Load RDF Data

```bash
ggen graph load --file ontology.ttl
```
````

Then `scripts/validate-docs/` MUST have a test that runs this command and verifies it works.

### 3. Save and Watch

As you save your file, Terminal 1 shows validation results:

```
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
🔄 Running validation... (14:23:45)
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

✓ Quick Start Tutorial: ALL TESTS PASSED
✓ SPARQL Query Guide: ALL TESTS PASSED
✓ CLI Reference: ALL TESTS PASSED

━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
✓ VALIDATION PASSED
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

👀 Watching for changes...
```

## Documentation Structure (Diataxis)

### 1. **Tutorials** (`docs/tutorials/`)

**Goal**: Learning-oriented, step-by-step lessons

**Format**:
- Start with "By the end of this tutorial, you will..."
- Numbered steps (Step 1, Step 2, etc.)
- Complete working examples
- End with "What you learned"

**Example**: `docs/getting-started/quick-start.md`

**When to write**: For onboarding new users or teaching a complete workflow

### 2. **How-to Guides** (`docs/how-to/`)

**Goal**: Task-oriented, problem-solving recipes

**Format**:
- Start with "Problem: [user's issue]"
- "Solution: [how to solve it]"
- Step-by-step instructions
- Troubleshooting section
- Related guides

**Example**: `docs/how-to/generation/generate-javascript-zod.md`

**When to write**: For specific tasks users need to accomplish

### 3. **Explanations** (`docs/explanations/`)

**Goal**: Understanding-oriented, conceptual discussions

**Format**:
- "What is [concept]?"
- "Why it matters"
- Real-world examples
- Comparisons to alternatives
- Trade-offs

**Example**: `docs/explanations/fundamentals/ontology-driven-development.md`

**When to write**: For core concepts users need to understand

### 4. **Reference** (`docs/reference/`)

**Goal**: Information-oriented, accurate descriptions

**Format**:
- Complete API/CLI documentation
- All commands with all options
- Examples for each command
- Tables, lists, precise definitions

**Example**: `docs/reference/commands/complete-cli-reference.md`

**When to write**: For comprehensive command/API documentation

## Writing Guidelines

### Commands Must Work

❌ **BAD**:
```bash
# Generate code (this might not work)
ggen magic generate --foo bar
```

✅ **GOOD**:
```bash
# Generate JavaScript from schema
ggen ontology generate schema.json \
  --language javascript \
  --output src/models/
```

**Why**: Every command in docs is validated. If it doesn't work in validation scripts, it fails CI.

### Use Real Examples

❌ **BAD**:
```bash
ggen graph load --file YOUR_FILE.ttl
```

✅ **GOOD**:
```bash
# Create example ontology
cat > product.ttl << 'EOF'
@prefix ex: <http://example.org/> .
ex:Product a ex:Class .
EOF

# Load it
ggen graph load --file product.ttl
```

**Why**: Validation scripts run actual commands. Placeholders fail.

### Show Expected Output

❌ **BAD**:
```bash
ggen template list
# Shows list of templates
```

✅ **GOOD**:
```bash
ggen template list

# Output:
# {
#   "templates": [
#     {"name": "hello.tmpl", "version": "1.0.0"},
#     ...
#   ]
# }
```

**Why**: Users know what to expect. Validation scripts can verify output.

### JavaScript, Zod, JSDoc (NOT TypeScript)

❌ **BAD**:
```typescript
interface Product {
  name: string;
  price: number;
}
```

✅ **GOOD**:
```javascript
import { z } from 'zod';

/**
 * @typedef {Object} Product
 * @property {string} name - Product name
 * @property {number} price - Price in USD
 */

const ProductSchema = z.object({
  name: z.string(),
  price: z.number(),
});
```

**Why**: ggen targets JavaScript + Zod + JSDoc (per user requirements).

## Adding Validation for Your Documentation

### If You Write a Tutorial

Create a validation script:

```bash
cat > scripts/validate-docs/validate-my-tutorial.sh << 'EOF'
#!/usr/bin/env bash
# Validate My Tutorial
# Tests: docs/tutorials/my-tutorial.md

set -e

# ... run every command from the tutorial ...
# ... verify outputs ...
# ... fail if anything doesn't work ...

EOF

chmod +x scripts/validate-docs/validate-my-tutorial.sh
```

Add it to `validate-all.sh`:

```bash
run_validation "validate-my-tutorial.sh" "My Tutorial"
```

### If You Write a How-to Guide

Add tests to existing validation script or create new one:

```bash
# If it's about SPARQL, add to validate-sparql-guide.sh
# If it's about CLI commands, add to validate-cli-reference.sh
# If it's a new category, create new script
```

### If You Write an Explanation

Ensure any code examples are tested elsewhere:

- Explanations can reference examples from tutorials/how-tos
- If you introduce new examples, add validation
- Conceptual docs still need working code examples

## Testing Your Documentation Locally

### Before Committing

```bash
# Run all validations
./scripts/validate-docs/validate-all.sh

# Check report
cat scripts/validate-docs/validation-report.md
```

### While Writing

```bash
# Watch mode (auto-validates on save)
./scripts/validate-docs/watch-docs-live.sh
```

### Individual Test

```bash
# Test just your changes
./scripts/validate-docs/validate-quick-start.sh
./scripts/validate-docs/validate-sparql-guide.sh
```

## CI/CD Integration

**Your documentation WILL be validated in CI**:

- ✅ Every PR that touches `docs/`
- ✅ Pushes to main/master
- ✅ Nightly builds

**If validation fails**:
- PR build fails
- Cannot merge
- Must fix documentation or commands

**See**: `.github/workflows/validate-docs.yml`

## Common Mistakes

### 1. Outdated Commands

```bash
# ❌ Command changed but docs didn't
ggen template show my-template  # Old syntax

# ✅ Updated to match current CLI
ggen template show --template my-template.tmpl
```

**Solution**: Run validation scripts. They catch this.

### 2. Broken Links

```markdown
❌ See [Guide](../broken-link.md)
✅ See [Guide](../how-to/generation/query-rdf-sparql.md)
```

**Solution**: Check all internal links work.

### 3. Missing Prerequisites

```markdown
❌ Run: ggen graph query ...
   (Forgot to mention you need to load data first!)

✅ First, load data:
   ggen graph load --file data.ttl

   Then query:
   ggen graph query ...
```

**Solution**: Follow your own tutorial start-to-finish.

## Documentation Checklist

Before submitting PR:

- [ ] All code examples tested
- [ ] Expected outputs shown
- [ ] Prerequisites documented
- [ ] Validation script updated
- [ ] `./scripts/validate-docs/validate-all.sh` passes
- [ ] Links checked
- [ ] JavaScript + Zod + JSDoc (not TypeScript)
- [ ] Real examples (not placeholders)

## Questions?

- **GitHub Discussions**: https://github.com/seanchatmangpt/ggen/discussions
- **Documentation Issues**: https://github.com/seanchatmangpt/ggen/issues

---

**Remember**: Documentation that doesn't work is worse than no documentation.

Every example must be **tested**, **verified**, and **working**.
