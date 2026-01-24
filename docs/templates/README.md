# Event Horizon Documentation Templates

## Overview

This directory contains production-ready templates for creating comprehensive "Crossing the Event Horizon" documentation. These templates help developers understand and document the paradigm shift from traditional code-first to RDF-first, ontology-driven development.

## Templates Included

### 1. EVENT_HORIZON_GUIDE_TEMPLATE.md

**Purpose**: Main conceptual guide explaining the RDF-first paradigm shift

**Sections**:
- Executive summary with A = μ(O) equation
- Before/after comparison (code-first vs RDF-first)
- Five-stage pipeline (μ₁ through μ₅) explanation
- Mental model shifts (files→ontologies, code→projections, bugs→interference patterns)
- Transformation journey (5 phases from recognition to mastery)
- Visual diagrams (ASCII art for event horizon, holographic projection, drift prevention)
- Decision framework (when to cross the event horizon)
- Success metrics and common pitfalls

**When to Use**:
- Creating introductory documentation for new developers
- Explaining RDF-first concepts to stakeholders
- Onboarding materials for team members

**Customization Points**:
- Replace `[bracketed placeholders]` with project-specific content
- Add project-specific case studies
- Include team-specific metrics and examples

---

### 2. EVENT_HORIZON_CASE_STUDY_TEMPLATE.md

**Purpose**: Document real-world transformation from code-first to RDF-first

**Sections**:
- Background and context
- Traditional approach (before) with code examples
- Pain points with specific incidents
- Transformation process (6 phases with timelines)
- RDF-first solution (after) with ontologies and generated artifacts
- Lessons learned (what went well, challenges, improvements)
- Metrics and evidence (velocity, quality, team metrics)
- Recommendations for others

**When to Use**:
- After completing a feature using RDF-first
- For team retrospectives
- Creating portfolio examples
- Training materials for new teams

**Customization Points**:
- Fill in all `[bracketed sections]` with actual data
- Include real ontology excerpts
- Add genuine generated code examples
- Include actual receipts and SPARQL queries
- Add team member testimonials

---

### 3. EVENT_HORIZON_EXERCISE_TEMPLATE.md

**Purpose**: Hands-on learning exercises for practicing RDF-first development

**Sections**:
- Learning objectives and prerequisites
- Step-by-step instructions (detailed, actionable)
- Expected outcomes (files, code, receipts)
- Verification steps (automated and manual)
- Troubleshooting common issues
- "Going Further" challenges and extensions
- Key takeaways and next steps

**When to Use**:
- Creating training exercises for workshops
- Self-paced learning materials
- Certification/assessment exercises
- Onboarding tasks for new developers

**Customization Points**:
- Define specific learning objectives
- Create step-by-step instructions
- Provide starter files or repository
- Add checkpoints and verification commands
- Include solution ontologies and expected outputs

---

### 4. EVENT_HORIZON_FAQ_TEMPLATE.md

**Purpose**: Answer frequently asked questions about RDF-first development

**Sections Organized by Category**:
- **Conceptual**: What is the event horizon? What is A = μ(O)?
- **Getting Started**: Where to start? Do I need RDF knowledge?
- **Technical Details**: RDF vs Turtle vs SPARQL? Five stages of μ?
- **Workflow**: How to edit generated code? Custom code? Existing projects?
- **Troubleshooting**: SHACL failing? Code not compiling? Ontology debugging?
- **Comparisons**: RDF-first vs GraphQL/Protobuf/OpenAPI?
- **Team/Organizational**: Convincing team? Adoption strategy?
- **Advanced**: Performance optimization? Multi-language generation?

**When to Use**:
- Creating support documentation
- Addressing common questions from team members
- Onboarding knowledge base
- Reducing support burden

**Customization Points**:
- Add project-specific questions
- Include real examples from your codebase
- Link to internal documentation
- Add troubleshooting for your specific setup

---

## How to Use These Templates

### Step 1: Choose Template

Select the template that matches your documentation need:
- **Guide**: Explaining concepts
- **Case Study**: Documenting real-world usage
- **Exercise**: Creating hands-on training
- **FAQ**: Answering common questions

### Step 2: Copy Template

```bash
# Copy template to your documentation directory
cp docs/templates/EVENT_HORIZON_GUIDE_TEMPLATE.md \
   docs/guides/crossing_the_event_horizon.md
```

### Step 3: Customize

1. Search for `[bracketed sections]` and replace with actual content
2. Delete template instruction blocks (marked with "Template Instructions")
3. Add project-specific examples, code snippets, and screenshots
4. Update placeholders like `[NNN]`, `[feature-name]`, etc.

### Step 4: Review

- [ ] All `[bracketed placeholders]` replaced
- [ ] Template instruction blocks removed
- [ ] Code examples compile and work
- [ ] Links point to valid resources
- [ ] Markdown renders correctly

### Step 5: Publish

```bash
# Add to git
git add docs/guides/crossing_the_event_horizon.md

# Commit with descriptive message
git commit -m "docs: Add Crossing the Event Horizon guide"

# Push
git push
```

---

## Template Conventions

### Markdown Structure

All templates follow consistent markdown structure:
- H1 (`#`) for document title
- H2 (`##`) for major sections
- H3 (`###`) for subsections
- H4 (`####`) for details within subsections

### Code Blocks

Code blocks always specify language:
```markdown
```rust
// Rust code example
```

```turtle
# Turtle/RDF ontology
```

```bash
# Shell commands
```
\`\`\`

### Placeholders

Templates use these placeholder conventions:
- `[Bracketed Text]`: Replace with actual content
- `[NNN]`: Replace with number (e.g., feature number)
- `[name]`: Replace with specific name
- `[X]`, `[Y]`, `[Z]`: Replace with metric values

### Internal Links

Templates use anchor links for navigation:
```markdown
[See Five-Stage Pipeline](#five-stage-pipeline)

## Five-Stage Pipeline {#five-stage-pipeline}
```

### Admonitions

Templates use consistent formatting for notes/warnings:
```markdown
**Note**: Important information

**Warning**: Critical warning

**Example**: Illustrative example
```

---

## Example Usage

### Example 1: Creating Team Onboarding Guide

```bash
# 1. Copy guide template
cp docs/templates/EVENT_HORIZON_GUIDE_TEMPLATE.md \
   docs/onboarding/rdf_first_guide.md

# 2. Customize for your team
vim docs/onboarding/rdf_first_guide.md
# - Add team-specific examples
# - Include internal project links
# - Add screenshots of your setup

# 3. Create accompanying exercise
cp docs/templates/EVENT_HORIZON_EXERCISE_TEMPLATE.md \
   docs/onboarding/first_ontology_exercise.md

# 4. Customize exercise
vim docs/onboarding/first_ontology_exercise.md
# - Define exercise using your project domain
# - Create starter repository
# - Add solution files

# 5. Create FAQ
cp docs/templates/EVENT_HORIZON_FAQ_TEMPLATE.md \
   docs/onboarding/rdf_first_faq.md

# 6. Add project-specific Q&A
vim docs/onboarding/rdf_first_faq.md
# - Add questions from team slack
# - Include troubleshooting for your CI/CD
```

### Example 2: Documenting Feature Migration

```bash
# 1. Copy case study template
cp docs/templates/EVENT_HORIZON_CASE_STUDY_TEMPLATE.md \
   docs/case-studies/auth_migration.md

# 2. Fill in project details
vim docs/case-studies/auth_migration.md
# - Describe authentication feature
# - Include before/after code examples
# - Add metrics (time saved, bugs reduced)
# - Include team testimonials

# 3. Attach artifacts
mkdir -p docs/case-studies/auth_migration/
cp .specify/specs/013-auth/feature.ttl \
   docs/case-studies/auth_migration/ontology.ttl
cp .ggen/receipts/20260124*.json \
   docs/case-studies/auth_migration/receipt.json
```

---

## Template Maintenance

### Version Control

Each template includes metadata footer:
```markdown
**Document Status**: [Template/Draft/Review/Published]
**Version**: [Version number]
**Last Updated**: [Date]
**Maintainer**: [Name/Team]
```

Update this when modifying templates.

### Contribution Guidelines

When improving templates:

1. **Preserve Structure**: Keep section hierarchy consistent
2. **Add Examples**: Prefer concrete examples over abstract explanations
3. **Test Instructions**: Verify all commands and code snippets work
4. **Update Cross-References**: Fix any broken internal links
5. **Document Changes**: Update version and changelog

### Feedback

Submit feedback via:
- GitHub Issues: [Link to issue tracker]
- Team Slack: #rdf-first channel
- Email: ggen-docs@example.com

---

## Integration with ggen Project

### Alignment with CLAUDE.md

These templates follow principles from `/home/user/ggen/CLAUDE.md`:
- RDF-first thinking (ontology as source of truth)
- Five-stage pipeline (μ₁ through μ₅)
- Deterministic receipts (SHA-256 verification)
- SPARC methodology alignment
- Chicago TDD principles
- Poka-Yoke (error prevention)

### Relationship to .specify/

Templates reference the `.specify/` directory structure:
- `.specify/specs/[NNN]-[name]/feature.ttl` (ontology source)
- `.specify/templates/` (Tera templates)
- `.specify/ontology/` (SHACL shapes)

### ggen Commands Used

Templates demonstrate these ggen commands:
- `ggen sync --audit true` (full pipeline)
- `ggen validate` (SHACL validation)
- `ggen verify-receipt` (cryptographic verification)
- `ggen query` (SPARQL debugging)

---

## Best Practices

### Documentation as Code

Treat documentation like code:
- Version control all docs
- Review documentation changes
- Test all code examples
- Automate validation (markdown linting, link checking)

### Living Documentation

Keep documentation synchronized with code:
- Update docs when features change
- Regenerate examples from ontologies
- Include receipts as evidence
- Link to actual source files

### Accessibility

Make documentation accessible:
- Use clear, simple language
- Provide multiple formats (text, video, diagrams)
- Include working examples
- Offer hands-on exercises

### Metrics-Driven

Include evidence in documentation:
- Before/after metrics
- Receipts (cryptographic proof)
- Team testimonials
- Real-world case studies

---

## Resources

### Related Documentation

- [Main Event Horizon Guide](../guides/crossing_the_event_horizon.md) (if published)
- [RDF-First Specification System](../../.specify/README.md)
- [ggen Development Workflow](../DEVELOPMENT_WORKFLOW.md)

### External References

- [W3C RDF Primer](https://www.w3.org/TR/rdf11-primer/)
- [W3C Turtle Specification](https://www.w3.org/TR/turtle/)
- [W3C SPARQL Query Language](https://www.w3.org/TR/sparql11-query/)
- [W3C SHACL Shapes](https://www.w3.org/TR/shacl/)

### Tools

- **Markdown Linters**: `markdownlint`, `vale`
- **Link Checkers**: `markdown-link-check`
- **Code Validators**: Language-specific linters
- **Diagram Tools**: Mermaid, PlantUML, ASCII art generators

---

## License

All templates in this directory are licensed under MIT License, consistent with the ggen project.

---

## Contact

**Maintainer**: ggen Core Team
**Documentation**: https://github.com/seanchatmangpt/ggen/tree/main/docs
**Issues**: https://github.com/seanchatmangpt/ggen/issues
**Discussions**: https://github.com/seanchatmangpt/ggen/discussions

---

**Last Updated**: 2026-01-24
**Template Set Version**: 1.0.0
