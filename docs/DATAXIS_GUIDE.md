# Diataxis Documentation Framework Guide

Complete guide to the Diataxis framework and how it's applied to ggen documentation.

## What is Diataxis?

**Diataxis** is a systematic approach to documentation that organizes content into four distinct types, each serving a different purpose and answering different questions.

The framework was developed by Daniele Procida and is based on the principle that documentation should be organized by **user need**, not by **technical structure**.

## The Four Documentation Types

Diataxis categorizes documentation into four types:

1. **Tutorials** - Learning-oriented, goal-based
2. **How-to Guides** - Problem-oriented, task-based
3. **Reference** - Information-oriented, description-based
4. **Explanations** - Understanding-oriented, explanation-based

Each type serves a different purpose and answers different questions.

## Documentation Type Matrix

| Type | Orientation | Purpose | Answers |
|------|-------------|---------|---------|
| **Tutorials** | Learning | Teach | "How do I...?" (learning) |
| **How-to Guides** | Problem-solving | Guide | "How do I...?" (doing) |
| **Reference** | Information | Describe | "What is...?" |
| **Explanations** | Understanding | Explain | "Why...?" |

## 1. Tutorials (Learning-Oriented)

**Purpose:** Teach users how to accomplish a goal through a series of steps.

**Characteristics:**
- Step-by-step instructions
- Assumes no prior knowledge
- Shows the "happy path"
- Builds understanding progressively
- Includes examples and exercises

**Questions answered:**
- "How do I get started?"
- "How do I learn to use this?"
- "What's the basic workflow?"

**Example from ggen:**
- [Getting Started Tutorial](tutorials/getting-started.md) - Teaches installation and first code generation
- [Ontology-to-Code Workflow](tutorials/ontology-to-code.md) - Teaches the complete workflow

**Writing guidelines:**
- Start with prerequisites
- Break into clear steps
- Show expected outcomes
- Include "What you'll learn" sections
- End with "Next steps"

**Structure:**
```markdown
# Tutorial Title

**Goal:** What the user will accomplish
**What you'll learn:** Key concepts

## Prerequisites
- Required knowledge
- Required tools

## Step 1: Title
Instructions...

## Step 2: Title
Instructions...

## What You've Learned
Summary of concepts

## Next Steps
Links to related content
```

## 2. How-to Guides (Problem-Oriented)

**Purpose:** Help users solve specific problems or accomplish specific tasks.

**Characteristics:**
- Task-focused
- Assumes some knowledge
- Shows multiple approaches when relevant
- Includes troubleshooting
- Practical and actionable

**Questions answered:**
- "How do I install this?"
- "How do I configure X?"
- "How do I solve problem Y?"

**Example from ggen:**
- [Installation Guide](how-to-guides/installation.md) - How to install ggen
- [Create Templates Guide](how-to-guides/create-templates.md) - How to create custom templates
- [Troubleshooting Guide](how-to-guides/troubleshoot.md) - How to solve common problems

**Writing guidelines:**
- Focus on the task, not the learning
- Provide multiple solutions when appropriate
- Include troubleshooting sections
- Show edge cases and alternatives
- Be concise and direct

**Structure:**
```markdown
# How to [Task Name]

Brief description of what this guide does.

## Method 1: [Approach]
Steps...

## Method 2: [Alternative Approach]
Steps...

## Troubleshooting
Common issues and solutions

## See Also
Links to related content
```

## 3. Reference (Information-Oriented)

**Purpose:** Provide complete, accurate information about the system.

**Characteristics:**
- Comprehensive and complete
- Accurate and precise
- Organized for quick lookup
- No narrative or explanation
- Just the facts

**Questions answered:**
- "What commands are available?"
- "What are the configuration options?"
- "What is the syntax for X?"

**Example from ggen:**
- [CLI Reference](reference/cli.md) - Complete command reference
- [Template Reference](reference/templates.md) - Template syntax
- [RDF/SPARQL Reference](reference/rdf-sparql.md) - RDF and SPARQL documentation

**Writing guidelines:**
- Be complete and accurate
- Organize for quick lookup
- Use tables and lists
- Include examples
- Avoid narrative

**Structure:**
```markdown
# [Feature] Reference

Complete reference for [feature].

## Overview
Brief description

## [Category 1]
### [Item 1]
**Syntax:** `command --option`
**Description:** What it does
**Options:**
- `--option`: Description

**Examples:**
```bash
command --option value
```

## [Category 2]
...

## See Also
Links to related content
```

## 4. Explanations (Understanding-Oriented)

**Purpose:** Provide background information and conceptual understanding.

**Characteristics:**
- Explains concepts and ideas
- Provides context and background
- Discusses design decisions
- Explains "why" not "how"
- Narrative and discursive

**Questions answered:**
- "Why does this work this way?"
- "What is the architecture?"
- "How does this concept work?"

**Example from ggen:**
- [Architecture Explanation](explanations/architecture.md) - System architecture
- [Ontology-Driven Explanation](explanations/ontology-driven.md) - Why ontology-driven development
- [Determinism Explanation](explanations/determinism.md) - How determinism works

**Writing guidelines:**
- Explain concepts, not procedures
- Provide context and background
- Discuss design decisions
- Use diagrams when helpful
- Be discursive and narrative

**Structure:**
```markdown
# [Concept] Explanation

Background and context for [concept].

## The Core Concept
What it is and why it matters

## How It Works
Underlying mechanisms

## Benefits
Why this approach

## Design Decisions
Why certain choices were made

## See Also
Links to related content
```

## Choosing the Right Type

Use this decision tree to choose the right documentation type:

```
Is the user learning something new?
├─ Yes → Tutorial
└─ No → Is the user trying to solve a problem?
    ├─ Yes → How-to Guide
    └─ No → Does the user need specific information?
        ├─ Yes → Reference
        └─ No → Explanation
```

## Common Patterns

### Tutorial → How-to → Reference

A typical learning path:

1. **Tutorial**: Learn the basics ([Getting Started](tutorials/getting-started.md))
2. **How-to Guide**: Solve specific problems ([Installation](how-to-guides/installation.md))
3. **Reference**: Look up details ([CLI Reference](reference/cli.md))

### How-to → Explanation

When users need to understand why:

1. **How-to Guide**: Do something ([Create Templates](how-to-guides/create-templates.md))
2. **Explanation**: Understand why ([Projections Explanation](explanations/projections.md))

### Reference → How-to

When users need to apply information:

1. **Reference**: Look up syntax ([Template Reference](reference/templates.md))
2. **How-to Guide**: Apply it ([Create Templates](how-to-guides/create-templates.md))

## Cross-Referencing

Documents should cross-reference related content:

**From Tutorials:**
- Link to relevant How-to Guides
- Link to Reference for details
- Link to Explanations for understanding

**From How-to Guides:**
- Link to Tutorials for learning
- Link to Reference for syntax
- Link to Explanations for context

**From Reference:**
- Link to How-to Guides for usage
- Link to Explanations for concepts

**From Explanations:**
- Link to Tutorials for learning
- Link to How-to Guides for application
- Link to Reference for details

## ggen Documentation Structure

```
docs/
├── README.md                    # Main navigation
├── tutorials/                   # Learning-oriented
│   ├── getting-started.md
│   ├── ontology-to-code.md
│   ├── ai-powered-generation.md
│   └── marketplace-workflow.md
├── how-to-guides/              # Problem-oriented
│   ├── installation.md
│   ├── create-templates.md
│   ├── use-rdf-ontologies.md
│   ├── configure-hooks.md
│   ├── deploy-production.md
│   └── troubleshoot.md
├── reference/                   # Information-oriented
│   ├── cli.md
│   ├── templates.md
│   ├── rdf-sparql.md
│   └── configuration.md
└── explanations/                # Understanding-oriented
    ├── architecture.md
    ├── ontology-driven.md
    ├── determinism.md
    ├── projections.md
    └── marketplace.md
```

## Writing Checklist

### For Tutorials:
- [ ] Clear goal stated at the beginning
- [ ] Prerequisites listed
- [ ] Steps are numbered and clear
- [ ] Expected outcomes shown
- [ ] "What you'll learn" section included
- [ ] "Next steps" provided

### For How-to Guides:
- [ ] Task is clearly stated
- [ ] Multiple approaches shown when relevant
- [ ] Troubleshooting included
- [ ] Edge cases covered
- [ ] Links to related content

### For Reference:
- [ ] Complete and accurate
- [ ] Organized for quick lookup
- [ ] Examples provided
- [ ] Syntax clearly shown
- [ ] No narrative or explanation

### For Explanations:
- [ ] Concept clearly explained
- [ ] Context and background provided
- [ ] Design decisions discussed
- [ ] Diagrams used when helpful
- [ ] Links to practical content

## Anti-Patterns to Avoid

### ❌ Mixing Types

**Bad:** A tutorial that explains why things work (should be an explanation)

**Good:** A tutorial that teaches how to do something, with links to explanations

### ❌ Wrong Orientation

**Bad:** A how-to guide that teaches concepts (should be a tutorial)

**Good:** A how-to guide that solves a specific problem

### ❌ Missing Cross-References

**Bad:** A reference that doesn't link to how-to guides

**Good:** A reference with "See Also" linking to related how-to guides

### ❌ Incomplete Reference

**Bad:** A reference that explains concepts (should be an explanation)

**Good:** A reference that provides complete, accurate information

## Examples from ggen

### Tutorial Example

[Getting Started Tutorial](tutorials/getting-started.md):
- **Goal:** Install ggen and generate first code
- **Steps:** Numbered, clear instructions
- **Learning:** Builds understanding progressively
- **Next steps:** Links to related content

### How-to Guide Example

[Installation Guide](how-to-guides/installation.md):
- **Task:** Install ggen
- **Multiple methods:** Homebrew, Cargo, from source
- **Troubleshooting:** Common problems and solutions
- **Practical:** Actionable steps

### Reference Example

[CLI Reference](reference/cli.md):
- **Complete:** All commands documented
- **Organized:** By command category
- **Accurate:** Syntax and options
- **Examples:** Command examples

### Explanation Example

[Architecture Explanation](explanations/architecture.md):
- **Concept:** System architecture
- **Context:** Design decisions
- **Understanding:** Why it's structured this way
- **Diagrams:** Visual representation

## Benefits of Diataxis

1. **Clear Organization:** Users know where to find information
2. **Reduced Duplication:** Each type serves a distinct purpose
3. **Better Navigation:** Clear paths between related content
4. **Improved Maintenance:** Easier to update and maintain
5. **Better User Experience:** Users find what they need faster

## Resources

- [Diataxis Framework](https://diataxis.fr/) - Official Diataxis website
- [Diataxis Book](https://diataxis.fr/book/) - Complete guide to Diataxis
- [Documentation System](https://diataxis.fr/system/) - The four types explained

## Contributing

When contributing to ggen documentation:

1. **Identify the type:** Choose the right documentation type
2. **Follow the structure:** Use the templates provided
3. **Add cross-references:** Link to related content
4. **Review the checklist:** Ensure all items are covered
5. **Test the links:** Verify all links work

## Summary

Diataxis organizes documentation by **user need**, not technical structure. The four types—Tutorials, How-to Guides, Reference, and Explanations—each serve distinct purposes and answer different questions. By following this framework, ggen documentation is organized, navigable, and maintainable.

For questions or suggestions about the documentation structure, see [Contributing](CONTRIBUTING.md).

