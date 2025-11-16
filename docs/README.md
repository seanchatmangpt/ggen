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
