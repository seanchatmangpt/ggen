<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [GGen Cookbook Examples (80/20 Edition)](#ggen-cookbook-examples-8020-edition)
  - [Overview](#overview)
  - [Organization](#organization)
  - [Running Examples (Marketplace-First)](#running-examples-marketplace-first)
    - [Prerequisites](#prerequisites)
    - [Basic Usage (80/20 Workflow)](#basic-usage-8020-workflow)
  - [Example Categories](#example-categories)
    - [Essential Examples](#essential-examples)
    - [Production Examples](#production-examples)
    - [AI Examples](#ai-examples)
    - [Recipe Examples](#recipe-examples)
  - [Getting Help](#getting-help)
  - [Contributing](#contributing)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# GGen Cookbook Examples (80/20 Edition)

This directory contains runnable examples that demonstrate GGen's **marketplace-first development workflow** and **80/20 approach** to eliminating software's dark matter.

## Overview

All examples follow the 80/20 rule - focusing on the 20% of features that provide 80% of development value:

- **Marketplace Discovery** - Search for proven patterns first
- **Production Readiness** - Validate before deployment
- **Essential Commands** - Workflow over configuration complexity

## Organization

Examples are organized to demonstrate the 80/20 workflow:

- **`chapter-*/`**: Examples showing marketplace-first patterns
- **`patterns/`**: Essential patterns that provide 80% of value
- **`recipes/`**: Complete recipes using marketplace packages

## Running Examples (Marketplace-First)

### Prerequisites

1. Install GGen:
   ```bash
   cargo install ggen
   ```

2. Verify installation:
   ```bash
   ggen --version && ggen market list
   ```

### Basic Usage (80/20 Workflow)

1. **Search marketplace for proven solutions**:
   ```bash
   ggen market search "rust web service"
   ggen market categories
   ```

2. **Install required packages**:
   ```bash
   ggen market add "rust-axum-service"
   ggen market add "postgresql-database"
   ```

3. **Initialize project structure**:
   ```bash
   ggen lifecycle run init
   ```

4. **Generate using marketplace templates**:
   ```bash
   ggen template generate rust-axum-service:user-service.tmpl
   ggen template generate postgresql-database:schema.tmpl
   ```

5. **Validate production readiness**:
   ```bash
   ggen lifecycle readiness
   ggen lifecycle validate --env development
   ```

6. **Deploy with confidence**:
   ```bash
   ggen lifecycle run deploy --env development
   ```

## Example Categories

### Essential Examples
- **Marketplace discovery** and package installation
- **Lifecycle management** and project initialization
- **Template generation** from marketplace patterns
- **Production readiness** validation

### Production Examples
- **Microservices architecture** using marketplace patterns
- **Multi-environment deployment** with validation
- **Production-ready services** with comprehensive testing

### AI Examples
- **AI-powered development** workflows
- **Template enhancement** with AI assistance
- **Natural language** to code generation

### Recipe Examples
- **Complete end-to-end** solutions
- **Domain-specific** generators
- **Integration patterns** for complex systems

## Getting Help

- **Main Documentation**: See `CLAUDE.md` for essential workflow
- **Command Reference**: See `docs/cli.md` for complete commands
- **Marketplace Guide**: See `docs/marketplace.md` for package management
- **Examples Directory**: Each example includes detailed README

## Contributing

Examples should demonstrate the 80/20 approach:

1. **Focus on marketplace-first** workflow
2. **Include production readiness** validation
3. **Use essential commands** (avoid complex configuration)
4. **Document the 20%** that provides 80% of value