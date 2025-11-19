# Cross-Reference Guide

This document maps relationships between documentation sections to help you navigate the Diataxis structure.

---

## By Learning Path

### Path 1: Complete Beginner

```
1. [Explanation] Why Ontologies Matter
   ↓
2. [Tutorial] Getting Started
   ↓
3. [Tutorial] Installing and Using SCHEMA.org
   ↓
4. [How-to] Discover Ontologies
   ↓
5. [Reference] Ontologies Registry
```

---

### Path 2: Building Custom Packs

```
1. [Tutorial] Building Your Own Pack
   ↓
2. [Reference] Pack Metadata Format
   ↓
3. [Reference] Template Variables
   ↓
4. [Reference] SPARQL Patterns
   ↓
5. [Tutorial] Publishing to Marketplace
   ↓
6. [How-to] Version Packs
```

---

### Path 3: Advanced Customization

```
1. [How-to] Customize Code Generation
   ↓
2. [Reference] Template Variables (complete reference)
   ↓
3. [How-to] Compose Multiple Ontologies
   ↓
4. [Explanation] Pack Composition
   ↓
5. [Reference] API Reference (programmatic usage)
```

---

## By Task

### Installing and Using Packs

| Start Here | Then See |
|------------|----------|
| [Tutorial: Getting Started](tutorials/01-getting-started.md) | [How-to: Discover Ontologies](how-to/discover-ontologies.md) |
| [How-to: Discover](how-to/discover-ontologies.md) | [Reference: Ontologies Registry](reference/ontologies-registry.md) |
| [Tutorial: Using SCHEMA.org](tutorials/02-using-schema-org.md) | [How-to: Generate TypeScript](how-to/generate-typescript-types.md) |

---

### Creating Custom Packs

| Start Here | Then See |
|------------|----------|
| [Tutorial: Building Custom Pack](tutorials/03-building-custom-pack.md) | [Reference: Pack Metadata](reference/pack-metadata.md) |
| [Reference: Pack Metadata](reference/pack-metadata.md) | [Reference: Template Variables](reference/template-variables.md) |
| [Reference: Template Variables](reference/template-variables.md) | [Reference: SPARQL Patterns](reference/sparql-patterns.md) |

---

### Customizing Generation

| Start Here | Then See |
|------------|----------|
| [How-to: Customize Generation](how-to/customize-generation.md) | [Reference: Template Variables](reference/template-variables.md) |
| [How-to: Generate TypeScript](how-to/generate-typescript-types.md) | [Reference: Generation Options](reference/generation-options.md) |
| [Tutorial: Using SCHEMA.org](tutorials/02-using-schema-org.md) | [How-to: Customize Generation](how-to/customize-generation.md) |

---

### Publishing and Sharing

| Start Here | Then See |
|------------|----------|
| [Tutorial: Publishing](tutorials/04-publishing-pack.md) | [How-to: Version Packs](how-to/version-packs.md) |
| [Tutorial: Building Custom Pack](tutorials/03-building-custom-pack.md) | [Tutorial: Publishing](tutorials/04-publishing-pack.md) |
| [How-to: Version Packs](how-to/version-packs.md) | [Reference: Pack Metadata](reference/pack-metadata.md) |

---

### Troubleshooting

| Problem | Start Here | Then See |
|---------|------------|----------|
| Installation fails | [How-to: Debug Installation](how-to/debug-installation.md) | [Reference: CLI Commands](reference/cli-commands.md) |
| Generation fails | [How-to: Debug Installation](how-to/debug-installation.md) | [Reference: Generation Options](reference/generation-options.md) |
| Wrong types generated | [How-to: Customize Generation](how-to/customize-generation.md) | [Reference: Template Variables](reference/template-variables.md) |

---

## By Concept

### Ontologies

| Concept | Explanation | Tutorial | Reference |
|---------|-------------|----------|-----------|
| What are ontologies? | [Why Ontologies Matter](explanations/why-ontologies.md) | [Getting Started](tutorials/01-getting-started.md) | [Ontologies Registry](reference/ontologies-registry.md) |
| How to use them? | [Philosophy](explanations/philosophy.md) | [Using SCHEMA.org](tutorials/02-using-schema-org.md) | [CLI Commands](reference/cli-commands.md) |

---

### Packs

| Concept | Explanation | Tutorial | How-to | Reference |
|---------|-------------|----------|--------|-----------|
| What are packs? | [Case Study](explanations/case-study-evolution.md) | [Getting Started](tutorials/01-getting-started.md) | [Discover](how-to/discover-ontologies.md) | [Pack Metadata](reference/pack-metadata.md) |
| Creating packs | [Philosophy](explanations/philosophy.md) | [Building Custom Pack](tutorials/03-building-custom-pack.md) | - | [Pack Metadata](reference/pack-metadata.md) |
| Publishing packs | [Marketplace Architecture](explanations/marketplace-architecture.md) | [Publishing](tutorials/04-publishing-pack.md) | [Version Packs](how-to/version-packs.md) | [CLI Commands](reference/cli-commands.md) |

---

### Templates

| Concept | Explanation | Tutorial | How-to | Reference |
|---------|-------------|----------|--------|-----------|
| What are templates? | [Case Study](explanations/case-study-evolution.md) | [Building Custom Pack](tutorials/03-building-custom-pack.md) | [Customize Generation](how-to/customize-generation.md) | [Template Variables](reference/template-variables.md) |
| Using templates | - | [Using SCHEMA.org](tutorials/02-using-schema-org.md) | [Generate TypeScript](how-to/generate-typescript-types.md) | [Generation Options](reference/generation-options.md) |

---

### Composition

| Concept | Explanation | How-to | Reference |
|---------|-------------|--------|-----------|
| Why compose? | [Pack Composition](explanations/pack-composition.md) | [Compose Ontologies](how-to/compose-ontologies.md) | [CLI Commands](reference/cli-commands.md) |
| How to compose? | [Pack Composition](explanations/pack-composition.md) | [Compose Ontologies](how-to/compose-ontologies.md) | [Pack Metadata](reference/pack-metadata.md) |

---

## Understanding vs Doing

### Want to Understand?
Start in [Explanations](explanations/):
- [Why Ontologies Matter](explanations/why-ontologies.md) - The "why"
- [Case Study: Evolution](explanations/case-study-evolution.md) - The story
- [Pack Composition](explanations/pack-composition.md) - How it works
- [Marketplace Architecture](explanations/marketplace-architecture.md) - System design
- [Philosophy](explanations/philosophy.md) - Core principles

### Want to Do?
Start in [Tutorials](tutorials/) or [How-To Guides](how-to/):
- **First time:** [Getting Started](tutorials/01-getting-started.md)
- **Specific task:** [How-To Guides](how-to/)
- **Need details:** [Reference](reference/)

---

## Document Relationships Graph

```
                    [Index]
                       |
        ┌──────────────┼──────────────┐
        │              │              │
   [Tutorials]    [How-To]      [Explanations]
        │              │              │
        └──────────────┼──────────────┘
                       │
                  [Reference]
```

**Key:**
- **Tutorials** → Step-by-step learning (references How-To and Reference)
- **How-To** → Task solutions (references Reference)
- **Explanations** → Understanding (references Tutorials and How-To for examples)
- **Reference** → Complete specs (standalone, no outbound links)

---

## Navigation Tips

1. **New to ontologies?** Start with [Why Ontologies Matter](explanations/why-ontologies.md)
2. **Want quick results?** Jump to [Getting Started](tutorials/01-getting-started.md)
3. **Have specific problem?** Find it in [How-To Guides](how-to/)
4. **Need precise details?** Look in [Reference](reference/)
5. **Want deeper understanding?** Read [Explanations](explanations/)

---

## Maintenance

This cross-reference guide should be updated when:
- New documents are added
- Document relationships change
- New learning paths emerge
- User feedback suggests better navigation

See [DOCUMENTATION_EVOLUTION.md](DOCUMENTATION_EVOLUTION.md) for maintenance guidelines.
