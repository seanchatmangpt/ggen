<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [ggen Documentation](#ggen-documentation)
  - [Quick Start](#quick-start)
  - [Documentation Structure](#documentation-structure)
    - [📚 Tutorials (Learning-Oriented)](#-tutorials-learning-oriented)
    - [🔧 How-to Guides (Problem-Oriented)](#-how-to-guides-problem-oriented)
    - [📖 Reference (Information-Oriented)](#-reference-information-oriented)
    - [💡 Explanations (Understanding-Oriented)](#-explanations-understanding-oriented)
  - [Common Tasks](#common-tasks)
    - [I want to...](#i-want-to)
  - [Documentation Principles](#documentation-principles)
  - [Documentation Framework](#documentation-framework)
  - [Contributing](#contributing)
  - [Additional Documentation](#additional-documentation)
    - [Development Tools](#development-tools)
    - [Marketplace](#marketplace)
    - [Release Information](#release-information)
    - [Advanced How-to Guides](#advanced-how-to-guides)
  - [Documentation Tools](#documentation-tools)
  - [See Also](#see-also)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# ggen Documentation

Welcome to the ggen documentation! This documentation follows the [Diataxis framework](https://diataxis.fr/), organized into four categories: **Tutorials**, **How-to Guides**, **Reference**, and **Explanations**.

## Quick Start

**New to ggen?** Start here:

1. **[Getting Started Tutorial](tutorials/getting-started.md)** - Install ggen and generate your first code in 5 minutes
2. **[Installation Guide](how-to-guides/installation.md)** - Complete installation instructions
3. **[CLI Reference](reference/cli.md)** - All available commands

**Note**: This directory uses `_CURRENT` suffix for active documentation files. Older versions are removed to prevent waste (see [MUDA_INVENTORY.md](wip/MUDA_INVENTORY.md)). Work-in-progress and intermediate documentation is stored in the [`wip/`](wip/) directory.

## 🗺️ Choose Your Learning Path

Not sure where to start? Answer these questions to find the right documentation path for you:

### Question 1: Are you familiar with RDF/Semantic Web?

<details>
<summary><b>Yes, I know RDF</b> → Go to Path A (RDF-First Path)</summary>

You can jump straight to domain modeling:
- **[Ontology-to-Code Tutorial](tutorials/ontology-to-code.md)** - Master the complete workflow
- **[RDF/SPARQL Reference](reference/rdf-sparql.md)** - Complete RDF documentation
- **[Getting Started](tutorials/getting-started.md)** - Installation and first generation
</details>

<details>
<summary><b>No, new to RDF</b> → Start with RDF Primer</summary>

Learn RDF fundamentals first:
1. **[RDF for Beginners](explanations/rdf-for-beginners.md)** - Understand RDF concepts (no background needed)
2. Then follow **Path A: RDF-First Path** above

This takes 15 minutes and makes everything else click into place.
</details>

### Question 2: Do you want to use AI to generate ontologies?

<details>
<summary><b>Yes, use AI</b> → Configure LLM First</summary>

You'll need to set up a language model:
1. **[Configure LLM Guide](how-to-guides/configure-llm.md)** - Set up OpenAI, Anthropic, or local Ollama
2. Then: **[AI-Powered Generation Tutorial](tutorials/ai-powered-generation.md)**
3. **[LLM Reference](reference/configuration.md)** - All configuration options

Choose your path:
- **Free/Local**: Use Ollama (qwen3-coder:30b recommended)
- **Production**: Use OpenAI (GPT-4o recommended) or Anthropic (Claude)
</details>

<details>
<summary><b>No, manual RDF</b> → Skip to Marketplace</summary>

You'll write RDF manually or use existing templates:
- **[Marketplace Quick-Start](tutorials/marketplace-quick-start.md)** - Find and use templates
- **[Getting Started Tutorial](tutorials/getting-started.md)** - Learn the workflow
</details>

### Question 3: What's your primary use case?

<details>
<summary><b>Build REST/GraphQL APIs</b></summary>

Backend developer path:
1. **[REST API Quick-Start](tutorials/rest-api-quickstart.md)** - Generate APIs in 10 minutes
2. **[Marketplace Quick-Start](tutorials/marketplace-quick-start.md)** - Use REST API templates
3. **[How to Deploy](how-to-guides/deploy-production.md)** - Deploy to production
4. **[CI/CD Workflows](how-to-guides/cicd-workflows.md)** - Automate deployment
</details>

<details>
<summary><b>Generate Data Models (Python/TypeScript)</b></summary>

Data engineer path:
1. Configure LLM (see Question 2 above)
2. **[RDF for Beginners](explanations/rdf-for-beginners.md)** - Understand domain modeling
3. **[Ontology-to-Code Tutorial](tutorials/ontology-to-code.md)** - Full workflow
4. **[Type Mapping Reference](reference/type-mapping.md)** - Language-specific types
</details>

<details>
<summary><b>Build Reusable Templates for My Team</b></summary>

Platform team path:
1. **[Marketplace Quick-Start](tutorials/marketplace-quick-start.md)** - Understand templates
2. **[Create Templates Guide](how-to-guides/create-templates.md)** - Build custom templates
3. **[Marketplace Metadata Reference](reference/marketplace-metadata.md)** - Publishing spec
4. **[Marketplace Ecosystem Explanation](explanations/marketplace.md)** - Why this works
</details>

<details>
<summary><b>Automate with Git Hooks / CI-CD</b></summary>

DevOps path:
1. **[Installation Guide](how-to-guides/installation.md)** - Install ggen
2. **[Configure Hooks Guide](how-to-guides/configure-hooks.md)** - Set up pre-commit hooks
3. **[CI/CD Workflows Guide](how-to-guides/cicd-workflows.md)** - GitHub Actions integration
4. **[Deployment Guide](how-to-guides/deploy-production.md)** - Production deployment
</details>

### Recommended Learning Paths

**Path A: RDF-First (Fundamental Understanding)**
- Time: 1-2 hours
- Prerequisites: Basic programming
- Outcome: Understand how to model domains, generate code
- Flow: RDF Primer → Getting Started → Ontology-to-Code

**Path B: AI-Powered (Fastest to Code)**
- Time: 30 minutes
- Prerequisites: LLM API key (optional - Ollama is free)
- Outcome: Generate ontologies from English, get working code
- Flow: Configure LLM → AI-Powered Generation → Marketplace Templates

**Path C: Marketplace-First (Template-Driven)**
- Time: 15 minutes
- Prerequisites: None
- Outcome: Use pre-built templates, generate customized code
- Flow: Marketplace Quick-Start → REST API Quick-Start → Deploy

**Path D: Deep Dive (Complete Understanding)**
- Time: 4+ hours
- Prerequisites: Curiosity
- Outcome: Understand architecture, design patterns, advanced features
- Flow: Getting Started → Architecture → All explanations → Advanced guides

## Documentation Structure

### 📚 Tutorials (Learning-Oriented)

Step-by-step guides that teach you how to accomplish a goal:

- **[Getting Started](tutorials/getting-started.md)** - Install and generate your first code
- **[Ontology-to-Code Workflow](tutorials/ontology-to-code.md)** - Master the complete workflow
- **[AI-Powered Generation](tutorials/ai-powered-generation.md)** - Use AI to generate ontologies
- **[Marketplace Workflow](tutorials/marketplace-workflow.md)** - Discover and use marketplace templates

### 🔧 How-to Guides (Problem-Oriented)

Task-based guides that help you solve specific problems:

- **[Installation](how-to-guides/installation.md)** - How to install ggen
- **[Create Templates](how-to-guides/create-templates.md)** - How to create custom templates
- **[Use RDF Ontologies](how-to-guides/use-rdf-ontologies.md)** - How to work with RDF
- **[Configure Hooks](how-to-guides/configure-hooks.md)** - How to set up Git hooks
- **[Deploy Production](how-to-guides/deploy-production.md)** - How to deploy to production
- **[CI/CD Workflows](how-to-guides/cicd-workflows.md)** - How to use GitHub Actions workflows
- **[Troubleshoot](how-to-guides/troubleshoot.md)** - How to solve common problems
- **[Development Workflow](DEVELOPMENT_WORKFLOW.md)** - How to develop ggen (testing, error handling, quality assurance)

### 📖 Reference (Information-Oriented)

Complete, accurate information about ggen's features:

- **[CLI Reference](reference/cli.md)** - Complete command reference
- **[Template Reference](reference/templates.md)** - Template syntax and features
- **[RDF/SPARQL Reference](reference/rdf-sparql.md)** - RDF and SPARQL documentation
- **[Configuration Reference](reference/configuration.md)** - Configuration options

### 💡 Explanations (Understanding-Oriented)

Background information and conceptual explanations:

- **[Architecture](explanations/architecture.md)** - System architecture and design
- **[Ontology-Driven Development](explanations/ontology-driven.md)** - Why ontology-driven development
- **[Determinism](explanations/determinism.md)** - How determinism works
- **[Code Projections](explanations/projections.md)** - How code projections work
- **[Marketplace Ecosystem](explanations/marketplace.md)** - Marketplace ecosystem overview
- **[Quality & Reliability: FMEA Analysis](FMEA_ANALYSIS.md)** - How ggen uses systematic failure analysis to ensure quality
- **[Error Prevention: Poke-Yoke Design](explanations/poke-yoke.md)** - How single-source-of-truth ontology prevents errors

## Common Tasks

### I want to...

- **Install ggen** → [Installation Guide](how-to-guides/installation.md)
- **Generate code from an ontology** → [Getting Started Tutorial](tutorials/getting-started.md)
- **Create a custom template** → [Create Templates Guide](how-to-guides/create-templates.md)
- **Use AI to generate ontologies** → [AI-Powered Generation Tutorial](tutorials/ai-powered-generation.md)
- **Find and install templates** → [Marketplace Workflow Tutorial](tutorials/marketplace-workflow.md)
- **Understand how it works** → [Architecture Explanation](explanations/architecture.md)
- **Troubleshoot a problem** → [Troubleshooting Guide](how-to-guides/troubleshoot.md)
- **See all commands** → [CLI Reference](reference/cli.md)
- **Learn about quality & error prevention** → [FMEA Analysis](FMEA_ANALYSIS.md) & [Poke-Yoke Design](explanations/poke-yoke.md)

## Documentation Principles

This documentation follows the **Diataxis framework**:

- **Tutorials**: Goal-oriented, learning paths
- **How-to Guides**: Problem-oriented, task-based
- **Reference**: Information-oriented, complete and accurate
- **Explanations**: Understanding-oriented, background information

Each document type serves a different purpose and answers different questions.

## Documentation Framework

This documentation follows the [Diataxis framework](https://diataxis.fr/), which organizes content into four types:

- **Tutorials**: Learning-oriented, step-by-step guides
- **How-to Guides**: Problem-oriented, task-based guides
- **Reference**: Information-oriented, complete documentation
- **Explanations**: Understanding-oriented, conceptual background

For a complete guide to the Diataxis framework and how it's applied here, see the [Diataxis Guide](DATAXIS_GUIDE.md).

## Contributing

Found an error or want to improve the documentation? Contributions are welcome!

1. Check existing issues
2. Create a new issue or pull request
3. Follow the [Diataxis Guide](DATAXIS_GUIDE.md) for documentation structure

## Additional Documentation

### Development Tools
- **[Cargo Make Guide](MAKEFILE.md)** - Complete guide to `cargo make` tasks and workflows
- **[CI/CD Workflows](how-to-guides/cicd-workflows.md)** - GitHub Actions workflows for CI/CD and releases
- **[Claude Development Configuration](CLAUDE.md)** - Claude-specific development environment setup

### Marketplace
- **[Marketplace Production Guide](MARKETPLACE.md)** - Production deployment guide for the marketplace

### Release Information
- **[Release Documentation](releases/)** - Historical release checklists and commands
  - [v2.6.0 Release Checklist](releases/RELEASE_v2.6.0_CHECKLIST.md)
  - [v2.6.0 Release Status](releases/RELEASE_v2.6.0_STATUS.md)
  - [v2.5.1 Release Checklist](releases/RELEASE_v2.5.1_CHECKLIST.md)
  - [v2.0.0 Release Commands](releases/RELEASE_v2.0.0_COMMANDS.md)

### Advanced How-to Guides
- **[Dogfooding Quick Start](how-to-guides/DOGFOODING_QUICKSTART.md)** - Using ggen to fix ggen's own problems

## Documentation Tools

For information about tools to compile this documentation into a website:
- **[Documentation Tools Survey](DOCUMENTATION_TOOLS_SURVEY.md)** - Complete survey of all options
- **[Core Team Recommendation](CORE_TEAM_RECOMMENDATION.md)** - What the core team would choose (mdBook + rustdoc + GitHub Pages)

## See Also

- **[Main README](../README.md)** - Project overview
- **[CHANGELOG](../CHANGELOG.md)** - Version history
- **[Examples](../examples/)** - Example projects
