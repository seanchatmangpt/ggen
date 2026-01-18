# Documentation Evolution Guide

How to maintain and evolve the Diataxis documentation structure as the ontology-packs system grows.

---

## Core Principles

1. **Preserve Diataxis structure** - Always maintain the 4 quadrants
2. **User needs first** - Add docs based on user questions and issues
3. **Keep it DRY** - Don't duplicate content across quadrants
4. **Version with code** - Docs evolve alongside features

---

## When to Add Documentation

### Add a Tutorial When...
- A new major feature is released
- User onboarding is taking > 30 minutes
- Support tickets show users struggling with initial setup
- A common workflow emerges that isn't documented

**Template for new tutorial:**
```markdown
# Tutorial: [Action-Oriented Title]

**Goal:** [What the user will achieve]

**What you'll learn:**
- [Skill 1]
- [Skill 2]
- [Skill 3]

**Prerequisites:**
- [Prerequisite 1]
- [Prerequisite 2]

**Time:** [Estimated completion time]

---

## Step 1: [First Step]
[Detailed instructions with commands and expected output]

---

## Step 2: [Second Step]
[Continue...]

---

## What You Learned
- ✅ [Learning 1]
- ✅ [Learning 2]

---

## Next Steps
- [Link to related tutorial]
- [Link to how-to guide]
```

---

### Add a How-To Guide When...
- Users ask "How do I [specific task]?"
- A common customization pattern emerges
- A feature has multiple approaches and users need guidance
- Support tickets show repeated questions about the same task

**Template for new how-to:**
```markdown
# How to: [Task Description]

**Problem:** [The specific problem this solves]

**Solution:** [Quick summary of the solution]

---

## Quick Start
[Minimal example showing the solution]

---

## [Section 1: Specific Approach]
[Detailed instructions]

---

## [Section 2: Alternative Approach]
[Another way to solve the problem]

---

## Tips
1. [Best practice 1]
2. [Best practice 2]

---

## Related Guides
- [Related how-to 1]
- [Related how-to 2]
```

---

### Add a Reference Document When...
- A new configuration option is added
- A new CLI command is introduced
- Template variables change
- API surface expands
- New metadata fields are added

**Template for new reference:**
```markdown
# [Topic] Reference

Complete reference for [topic].

---

## Overview
[Brief description]

---

## [Section 1]

### `option_name`

**Type:** [data type]

**Description:** [What it does]

**Default:** [default value]

**Example:**
[Code example]

---

## Related References
- [Related reference 1]
- [Related reference 2]
```

---

### Add an Explanation When...
- A design decision needs justification
- Users ask "Why does it work this way?"
- A new architectural pattern is introduced
- Complex concepts need clarification
- A major refactor or redesign occurs

**Template for new explanation:**
```markdown
# [Concept] Explanation

[Opening that sets context]

---

## [Main Concept 1]

[Explanation with examples, diagrams, analogies]

---

## [Main Concept 2]

[Continue...]

---

## Why This Matters

[Connect to user benefits and real-world impact]

---

## Related Explanations
- [Related explanation 1]
- [Related explanation 2]
```

---

## Document Lifecycle

### 1. Creation (New Feature/Pattern)

```
New Feature Released
        ↓
Write Reference (technical specs)
        ↓
Write How-To (common tasks)
        ↓
Write Tutorial (if major feature)
        ↓
Write Explanation (if design needs justification)
        ↓
Update Index and Cross-References
```

---

### 2. Maintenance (Existing Docs)

**Quarterly review:**
- Check for outdated screenshots/examples
- Verify all commands still work
- Update version numbers
- Check for broken links
- Review user feedback

**Signs a doc needs updating:**
- Command syntax has changed
- Example code no longer works
- Screenshots show old UI
- Referenced features have been deprecated
- User comments indicate confusion

---

### 3. Deprecation (Removing Docs)

**When to deprecate:**
- Feature has been removed
- Approach is no longer recommended
- Superseded by better documentation

**How to deprecate:**

1. Add warning banner:
```markdown
> ⚠️ **DEPRECATED:** This document describes a deprecated feature.
> See [New Approach](link-to-new-doc.md) instead.
```

2. Keep deprecated doc for 2 versions

3. After 2 versions, move to `docs/archive/`

4. Add redirect in index:
```markdown
- ~~Old Topic~~ → See [New Topic](new-topic.md)
```

---

## Documentation Versioning

### Version Alignment

Docs should be versioned alongside code:

```
ggen v1.0.0 → docs v1.0.0
ggen v1.1.0 → docs v1.1.0
```

### Version-Specific Content

Use conditional content for version differences:

```markdown
**ggen v1.0.0:**
\`\`\`bash
ggen ontology install schema.org
\`\`\`

**ggen v1.1.0+:**
\`\`\`bash
ggen ontology install schema.org@1.0.0  # Now supports version pinning
\`\`\`
```

---

## Quality Checklist

Before publishing new docs:

### All Documents
- [ ] Follows Diataxis principles for its quadrant
- [ ] Uses consistent formatting and style
- [ ] Includes cross-references to related docs
- [ ] Has been tested (commands run successfully)
- [ ] Passes spell check
- [ ] Uses inclusive language

### Tutorials
- [ ] Has clear goal and prerequisites
- [ ] Includes estimated time
- [ ] Every step has expected output
- [ ] Has "What You Learned" summary
- [ ] Has "Next Steps" section

### How-To Guides
- [ ] Starts with problem statement
- [ ] Has quick-start example
- [ ] Covers common variations
- [ ] Includes troubleshooting tips
- [ ] Links to related guides

### Reference
- [ ] Alphabetically organized (where appropriate)
- [ ] Every option/field documented
- [ ] Includes data types
- [ ] Has examples for each item
- [ ] Links to related references

### Explanations
- [ ] Explains "why" not "how"
- [ ] Uses analogies and examples
- [ ] Includes diagrams where helpful
- [ ] Connects to user benefits
- [ ] Links to practical docs (tutorials/how-tos)

---

## User Feedback Integration

### Collecting Feedback

Add feedback mechanism to each page:

```markdown
---

**Was this helpful?** Let us know:
- 👍 Yes / 👎 No
- [Submit feedback](https://github.com/your-org/ggen/issues/new?template=docs-feedback.md)
```

### Acting on Feedback

**Weekly:**
- Review feedback on new documents
- Fix obvious errors immediately
- Create issues for larger improvements

**Monthly:**
- Analyze patterns in feedback
- Identify most-requested topics
- Plan documentation additions

---

## Contribution Guidelines

### For Community Contributors

1. **Check existing docs** - Avoid duplication
2. **Choose right quadrant** - Use Diataxis guide
3. **Follow templates** - Use templates above
4. **Test examples** - All code must run
5. **Get review** - Submit PR for review

### For Maintainers

1. **Review for accuracy** - Verify technical correctness
2. **Check Diataxis fit** - Ensure document is in right quadrant
3. **Suggest improvements** - Be constructive
4. **Merge promptly** - Don't let PRs go stale

---

## Migration Strategy (Existing Docs → Diataxis)

If you have existing docs not in Diataxis format:

### 1. Audit Current Docs

Create inventory:
```
- Getting Started Guide → Tutorial
- API Documentation → Reference
- Configuration Guide → Split (How-To + Reference)
- Architecture Overview → Explanation
```

### 2. Prioritize

Order by:
1. Most-visited pages
2. Most-outdated content
3. Highest support ticket volume
4. Strategic importance

### 3. Migrate Incrementally

Don't rewrite everything at once:
- Week 1: Migrate top tutorial
- Week 2: Migrate critical how-tos
- Week 3: Migrate reference docs
- Week 4: Migrate explanations

### 4. Redirect Old URLs

```markdown
# Old: /docs/getting-started.md
→ Redirect to: /docs/ontology-packs/tutorials/01-getting-started.md

# Old: /docs/config-reference.md
→ Redirect to: /docs/ontology-packs/reference/pack-metadata.md
```

---

## Metrics and Success

### Track These Metrics

- **Page views** - Which docs are most used?
- **Time on page** - Are users reading or bouncing?
- **Feedback sentiment** - Positive/negative ratio
- **Support tickets** - Are docs reducing tickets?
- **External links** - Are others linking to your docs?

### Success Indicators

- ✅ Fewer "How do I..." support tickets
- ✅ Higher positive feedback ratio
- ✅ Users completing tutorials successfully
- ✅ Community contributing improvements
- ✅ Faster onboarding for new users

---

## Tools and Automation

### Recommended Tools

- **Spell check:** `mdspell`
- **Link checker:** `markdown-link-check`
- **Formatting:** `prettier`
- **Diagrams:** Mermaid.js (for architecture diagrams)
- **Versioning:** Keep docs in same repo as code

### CI/CD for Docs

```yaml
# .github/workflows/docs.yml
name: Documentation

on: [push, pull_request]

jobs:
  validate:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v2
      - name: Check links
        run: npx markdown-link-check docs/**/*.md
      - name: Check spelling
        run: npx mdspell --en-us --ignore-numbers --ignore-acronyms docs/**/*.md
      - name: Verify examples
        run: ./scripts/test-doc-examples.sh
```

---

## Emergency Updates

### When a critical bug affects docs:

1. **Immediate:** Add warning banner to affected docs
2. **Within 24h:** Update with correct information
3. **Within 48h:** Deploy updated docs
4. **Communicate:** Announce via changelog/social media

**Warning banner template:**
```markdown
> 🚨 **CRITICAL UPDATE (2025-11-18):**
> The approach described below has a critical bug in ggen v1.2.0.
> Please upgrade to v1.2.1 or use [this workaround](link).
```

---

## Long-Term Vision

As the system grows, consider:

### Interactive Documentation
- Live code editors (CodeSandbox, StackBlitz)
- Interactive tutorials with guided steps
- Video walkthroughs

### Localization
- Translate to other languages
- Maintain parallel versions
- Use translation management tools

### Advanced Features
- Search with AI assistance
- Personalized learning paths
- Difficulty ratings for tutorials
- Estimated time to complete

---

## Summary

**Remember:**
1. Documentation is never "done" - it evolves with code
2. User feedback is gold - act on it quickly
3. Diataxis keeps docs organized - stick to the quadrants
4. Quality over quantity - better to have 10 great docs than 50 mediocre ones
5. Make it easy to contribute - lower the barrier for community help

**Questions to ask regularly:**
- Are users finding what they need?
- Are we explaining the "why" not just the "how"?
- Are examples current and tested?
- Are we reducing support burden?
- Are we teaching users to fish, not just giving them fish?

---

## Related Documents

- [Cross-Reference Guide](CROSS_REFERENCES.md)
- [Index](index.md)
- [Diataxis Framework](https://diataxis.fr/)
