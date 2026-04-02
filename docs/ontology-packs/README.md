<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [Ontology Packs Documentation](#ontology-packs-documentation)
  - [Quick Start](#quick-start)
  - [Documentation Structure](#documentation-structure)
    - [📚 Tutorials - Learning-Oriented](#-tutorials---learning-oriented)
    - [🛠️ How-To Guides - Problem-Oriented](#-how-to-guides---problem-oriented)
    - [📖 Reference - Information-Oriented](#-reference---information-oriented)
    - [💡 Explanations - Understanding-Oriented](#-explanations---understanding-oriented)
  - [Navigation Guides](#navigation-guides)
  - [Documentation Highlights](#documentation-highlights)
    - [⭐ Must-Read: The Case Study](#-must-read-the-case-study)
  - [Finding What You Need](#finding-what-you-need)
    - [By Role](#by-role)
    - [By Goal](#by-goal)
  - [Documentation Philosophy](#documentation-philosophy)
  - [File Structure](#file-structure)
  - [Contributing to Documentation](#contributing-to-documentation)
  - [Documentation Versions](#documentation-versions)
  - [Feedback](#feedback)
  - [License](#license)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Ontology Packs Documentation

Complete documentation for the ggen ontology-as-packs system, organized using the [Diataxis](https://diataxis.fr/) framework.

---

## Quick Start

**New to ontology packs?** Start here:
1. Read [Why Ontologies Matter](explanations/why-ontologies.md) (5 min)
2. Follow [Getting Started Tutorial](tutorials/01-getting-started.md) (10 min)
3. Explore [How-To Guides](how-to/) for your specific needs

---

## Documentation Structure

This documentation follows the Diataxis framework with 4 quadrants:

```
                Learning-Oriented
                       ↑
                       |
    TUTORIALS          |          EXPLANATIONS
    (learning + action)|          (learning + knowledge)
                       |
    ←──────────────────┼──────────────────────→
    Practical          |          Theoretical
                       |
    HOW-TO GUIDES      |          REFERENCE
    (problem + action) |          (knowledge + information)
                       |
                       ↓
                Problem-Oriented
```

### 📚 [Tutorials](tutorials/) - Learning-Oriented
Step-by-step lessons for building practical skills:
- [Getting Started: Your First Ontology Pack](tutorials/01-getting-started.md)
- [Tutorial: Installing and Using SCHEMA.org](tutorials/02-using-schema-org.md)
- [Tutorial: Building Your Own Ontology Pack](tutorials/03-building-custom-pack.md)
- [Tutorial: Publishing to the Marketplace](tutorials/04-publishing-pack.md)

### 🛠️ [How-To Guides](how-to/) - Problem-Oriented
Practical solutions to specific tasks:
- [How to: Discover Available Ontologies](how-to/discover-ontologies.md)
- [How to: Generate TypeScript Types from FOAF](how-to/generate-typescript-types.md)
- [How to: Customize Code Generation](how-to/customize-generation.md)
- [How to: Compose Multiple Ontologies](how-to/compose-ontologies.md)
- [How to: Version and Update Packs](how-to/version-packs.md)
- [How to: Debug Pack Installation](how-to/debug-installation.md)

### 📖 [Reference](reference/) - Information-Oriented
Complete technical specifications:
- [Ontology Pack Metadata Format](reference/pack-metadata.md)
- [Template Variable Reference](reference/template-variables.md)
- [SPARQL Query Patterns](reference/sparql-patterns.md)
- [Supported Ontologies Registry](reference/ontologies-registry.md)
- [Code Generation Options](reference/generation-options.md)
- [CLI Command Reference](reference/cli-commands.md)
- [API Reference](reference/api.md)

### 💡 [Explanations](explanations/) - Understanding-Oriented
Background knowledge and design decisions:
- [Why Ontologies Matter (and what they are)](explanations/why-ontologies.md)
- [From Direct Generation to Template-Driven Design](explanations/case-study-evolution.md) ⭐
- [Understanding Pack Composition and Dependencies](explanations/pack-composition.md)
- [Architecture: Ontology Packs in the Marketplace](explanations/marketplace-architecture.md)
- [The Philosophy of Reusable Semantic Assets](explanations/philosophy.md)

---

## Navigation Guides

- **[Cross-Reference Guide](CROSS_REFERENCES.md)** - Navigate between related documents
- **[Documentation Evolution Guide](DOCUMENTATION_EVOLUTION.md)** - Maintain and grow these docs

---

## Documentation Highlights

### ⭐ Must-Read: The Case Study

[From Direct Generation to Template-Driven Design](explanations/case-study-evolution.md) tells the story of how the old thinking (direct CLI generation from URLs) failed in production and how the 2029 thinking (ontology packs + templates) solved it.

**Key lessons:**
- Why every user was reimplementing the same work
- How versioning prevents production breakage
- Why templates eliminate code forks
- The waste of duplicated effort (200+ person-days saved)
- Real metrics showing 90% reduction in support tickets

---

## Finding What You Need

### By Role

**I'm a first-time user:**
→ [Getting Started Tutorial](tutorials/01-getting-started.md)

**I'm an experienced developer:**
→ [How-To Guides](how-to/) for your specific task

**I'm integrating programmatically:**
→ [API Reference](reference/api.md)

**I want to understand the design:**
→ [Explanations](explanations/), especially the [Case Study](explanations/case-study-evolution.md)

---

### By Goal

**Install and use an ontology:**
1. [Getting Started](tutorials/01-getting-started.md)
2. [Using SCHEMA.org](tutorials/02-using-schema-org.md)

**Create my own ontology pack:**
1. [Building Custom Pack](tutorials/03-building-custom-pack.md)
2. [Pack Metadata Reference](reference/pack-metadata.md)

**Customize code generation:**
1. [Customize Generation](how-to/customize-generation.md)
2. [Template Variables Reference](reference/template-variables.md)

**Publish to marketplace:**
1. [Publishing Tutorial](tutorials/04-publishing-pack.md)
2. [Version Packs How-To](how-to/version-packs.md)

---

## Documentation Philosophy

This documentation follows these principles:

1. **Respect Diataxis boundaries** - Tutorials teach, How-Tos solve problems, Reference informs, Explanations clarify
2. **User needs first** - Content driven by real user questions and support tickets
3. **Keep it DRY** - No duplicate content across quadrants
4. **Show, don't tell** - Concrete examples over abstract explanations
5. **Evolve continuously** - Docs versioned alongside code

See [Documentation Evolution Guide](DOCUMENTATION_EVOLUTION.md) for maintenance principles.

---

## File Structure

```
docs/ontology-packs/
├── README.md                          # This file
├── index.md                           # Main navigation page
├── CROSS_REFERENCES.md                # Cross-reference guide
├── DOCUMENTATION_EVOLUTION.md         # Maintenance guide
│
├── tutorials/                         # 🎓 Learning-oriented
│   ├── 01-getting-started.md
│   ├── 02-using-schema-org.md
│   ├── 03-building-custom-pack.md
│   └── 04-publishing-pack.md
│
├── how-to/                            # 🛠️ Problem-oriented
│   ├── discover-ontologies.md
│   ├── generate-typescript-types.md
│   ├── customize-generation.md
│   ├── compose-ontologies.md
│   ├── publish-pack.md
│   ├── version-packs.md
│   └── debug-installation.md
│
├── reference/                         # 📚 Information-oriented
│   ├── pack-metadata.md
│   ├── template-variables.md
│   ├── sparql-patterns.md
│   ├── ontologies-registry.md
│   ├── generation-options.md
│   ├── cli-commands.md
│   └── api.md
│
└── explanations/                      # 💡 Understanding-oriented
    ├── why-ontologies.md
    ├── case-study-evolution.md       # ⭐ The story
    ├── pack-composition.md
    ├── marketplace-architecture.md
    └── philosophy.md
```

---

## Contributing to Documentation

We welcome documentation contributions! Please:

1. Read [Documentation Evolution Guide](DOCUMENTATION_EVOLUTION.md)
2. Follow Diataxis principles for your quadrant
3. Test all code examples
4. Submit PR with clear description

---

## Documentation Versions

These docs are versioned alongside ggen:
- **Current:** v1.0.0
- **Last Updated:** 2025-11-18
- **Changelog:** See individual documents for version-specific content

---

## Feedback

**Was this documentation helpful?**

- 👍 Yes → [Star the repo](https://github.com/your-org/ggen)
- 👎 No → [Submit feedback](https://github.com/your-org/ggen/issues/new?template=docs-feedback.md)
- 💡 Suggestion → [Open an issue](https://github.com/your-org/ggen/issues/new)

---

## License

This documentation is licensed under [CC BY-SA 4.0](https://creativecommons.org/licenses/by-sa/4.0/).

Code examples are licensed under [MIT](https://opensource.org/licenses/MIT).
