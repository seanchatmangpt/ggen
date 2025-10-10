<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [GGen Cookbook 2nd Edition - Quick Start Guide](#ggen-cookbook-2nd-edition---quick-start-guide)
  - [🚀 For Readers](#-for-readers)
    - [View the Book Locally](#view-the-book-locally)
    - [Build Static HTML](#build-static-html)
  - [✍️ For Content Authors](#-for-content-authors)
    - [File Locations](#file-locations)
    - [Content Priority Order](#content-priority-order)
    - [Writing Guidelines](#writing-guidelines)
    - [Pattern Template](#pattern-template)
  - [🔧 Development Workflow](#-development-workflow)
    - [Before Writing](#before-writing)
    - [While Writing](#while-writing)
    - [After Writing](#after-writing)
  - [📚 Markdown Tips](#-markdown-tips)
    - [Code Blocks](#code-blocks)
    - [Callouts](#callouts)
    - [Cross-References](#cross-references)
    - [Diagrams](#diagrams)
  - [🎯 Quality Checklist](#-quality-checklist)
  - [📖 Style Guide](#-style-guide)
    - [Voice and Tone](#voice-and-tone)
    - [Formatting](#formatting)
    - [Code Examples](#code-examples)
  - [🔗 Resources](#-resources)
    - [mdBook Documentation](#mdbook-documentation)
    - [GGen Resources](#ggen-resources)
    - [Pattern Language](#pattern-language)
  - [🆘 Getting Help](#-getting-help)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# GGen Cookbook 2nd Edition - Quick Start Guide

## 🚀 For Readers

### View the Book Locally

```bash
cd /Users/sac/ggen/docs/ggen-cookbook-2nd
mdbook serve
# Open http://localhost:3000
```

### Build Static HTML

```bash
mdbook build
open book/html/index.html
```

## ✍️ For Content Authors

### File Locations

| Content Type | Location | Files |
|--------------|----------|-------|
| Configuration | `book.toml` | 1 |
| TOC | `src/SUMMARY.md` | 1 |
| Preface/Intro | `src/` | 2 |
| Part I | `src/part-1/` | 10 |
| Part II | `src/part-2/` | 10 |
| Part III | `src/part-3/` | 32 |
| Part IV | `src/part-4/` | 15 |
| Part V | `src/part-5/` | 10 |
| Part VI | `src/part-6/` | 10 |
| Appendices | `src/appendices/` | 12 |

### Content Priority Order

1. **Phase 1: Foundation** (Estimated: 2-3 weeks)
   - [ ] Part I: Chapters 1-2 (philosophy and vision)
   - [ ] Introduction refinement with diagrams

2. **Phase 2: Core Documentation** (Estimated: 3-4 weeks)
   - [ ] Part II: Chapters 3-4 (engine and CLI)
   - [ ] Create C4 architecture diagrams
   - [ ] CLI reference in Appendix B

3. **Phase 3: Pattern Language** (Estimated: 4-6 weeks)
   - [ ] Part III: Chapters 5-10 (templates and patterns)
   - [ ] Develop all 10 core patterns with examples
   - [ ] Create pattern catalog

4. **Phase 4: Autonomic Features** (Estimated: 2-3 weeks)
   - [ ] Part IV: Chapters 11-13 (self-* properties)
   - [ ] Add performance benchmarks

5. **Phase 5: Ecosystem** (Estimated: 2-3 weeks)
   - [ ] Part V: Chapters 14-15 (marketplace and tooling)
   - [ ] Create integration examples

6. **Phase 6: Advanced Topics** (Estimated: 2-3 weeks)
   - [ ] Part VI: Chapters 16-17 (enterprise and extensibility)
   - [ ] Plugin development guide

7. **Phase 7: References** (Estimated: 1-2 weeks)
   - [ ] Appendix A: Template syntax reference
   - [ ] Appendix B: Complete CLI reference
   - [ ] Appendix C: Resources and links

### Writing Guidelines

Each chapter should include:

```markdown
# Chapter Title

## Overview
Brief introduction to the chapter's topic.

## Key Concepts
- Concept 1: Definition
- Concept 2: Definition

## Detailed Content
Main content with subsections...

## Example
\`\`\`rust
// Complete, runnable example
\`\`\`

## Best Practices
- Practice 1
- Practice 2

## Common Pitfalls
⚠️ Warning about anti-patterns

## Summary
Key takeaways

## Next Steps
Link to next chapter
```

### Pattern Template

For patterns in Part III:

```markdown
# Pattern N: Pattern Name

## Intent
What this pattern accomplishes

## Context
When to use this pattern

## Problem
The challenge this pattern solves

## Solution
How to implement it

## Structure
Diagram or schema

## Implementation
\`\`\`yaml
---
name: "Pattern Template"
description: "..."
variables:
  - name: example
    type: string
---
Template body here
\`\`\`

## Example Usage
\`\`\`bash
ggen generate pattern.tmpl --set example=value
\`\`\`

## Variations
- Variation 1
- Variation 2

## Related Patterns
- Builds on: Pattern M
- See also: Pattern K

## Anti-Patterns
❌ Don't do this...
✅ Do this instead...

## References
Links to related documentation
```

## 🔧 Development Workflow

### Before Writing

```bash
# Pull latest changes
git pull origin master

# Create feature branch
git checkout -b docs/cookbook-chapter-N
```

### While Writing

```bash
# Start live preview
mdbook serve

# Edit files in src/
# Browser auto-refreshes
```

### After Writing

```bash
# Verify build
mdbook build

# Run verification
./scripts/verify-structure.sh

# Test all code examples
mdbook test

# Commit changes
git add src/
git commit -m "docs: add Chapter N content"
git push origin docs/cookbook-chapter-N
```

## 📚 Markdown Tips

### Code Blocks

\`\`\`rust
// Rust code with syntax highlighting
fn main() {
    println!("Hello, GGen!");
}
\`\`\`

### Callouts

> **Note**: Important information

> **Warning**: Caution required

> **Tip**: Helpful suggestion

### Cross-References

```markdown
See [Chapter 3](./part-2/chapter-3.md) for details.

Refer to [Pattern 5](./part-3/chapter-6-5.md).

Check the [CLI Reference](./appendices/appendix-b.md).
```

### Diagrams

Use ASCII art or reference external images:

```
┌─────────────┐
│  Component  │
└──────┬──────┘
       │
       ▼
┌─────────────┐
│   Output    │
└─────────────┘
```

Or:

```markdown
![Architecture Diagram](../images/architecture.svg)
```

## 🎯 Quality Checklist

Before marking a chapter complete:

- [ ] All sections filled with content (no placeholders)
- [ ] At least one complete, tested example
- [ ] Code blocks properly formatted with syntax highlighting
- [ ] Cross-references to related chapters
- [ ] Best practices and anti-patterns included
- [ ] Grammar and spelling checked
- [ ] mdBook builds without errors
- [ ] Links verified (no broken links)
- [ ] Images referenced correctly
- [ ] Consistent tone and style

## 📖 Style Guide

### Voice and Tone

- **Active voice**: "GGen generates code" not "Code is generated by GGen"
- **Second person**: "You create templates" not "One creates templates"
- **Present tense**: "GGen validates outputs" not "GGen will validate"
- **Clear and concise**: Avoid jargon, explain acronyms

### Formatting

- **Bold** for emphasis and first use of terms
- *Italic* for pattern names and concepts
- `Code font` for commands, files, variables
- > Blockquotes for important notes

### Code Examples

- Use realistic, meaningful examples
- Include comments explaining key points
- Show complete, runnable code
- Test all examples before publishing

## 🔗 Resources

### mdBook Documentation
- [mdBook Guide](https://rust-lang.github.io/mdBook/)
- [Markdown Syntax](https://www.markdownguide.org/)

### GGen Resources
- [GGen Repository](https://github.com/seanchatmangpt/ggen)
- [GGen Examples](https://github.com/seanchatmangpt/ggen/tree/master/examples)

### Pattern Language
- Christopher Alexander's *A Pattern Language*
- Gang of Four *Design Patterns*

## 🆘 Getting Help

- **Questions**: Open GitHub discussion
- **Issues**: File GitHub issue with `docs` label
- **PRs**: Submit for review with detailed description

---

**Status**: ✅ Structure Ready
**Next**: Begin Phase 1 content authoring
**Target**: Complete 2nd edition by Q2 2025
