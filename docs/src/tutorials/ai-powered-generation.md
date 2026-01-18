<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [AI-Powered Generation](#ai-powered-generation)
  - [Prerequisites](#prerequisites)
  - [Step 1: Generate Ontology with AI](#step-1-generate-ontology-with-ai)
  - [Step 2: Review and Refine](#step-2-review-and-refine)
  - [Step 3: Generate Code](#step-3-generate-code)
  - [Step 4: AI-Assisted Analysis](#step-4-ai-assisted-analysis)
  - [AI Features](#ai-features)
    - [Generate Ontology](#generate-ontology)
    - [Interactive Chat](#interactive-chat)
    - [Code Analysis](#code-analysis)
  - [Workflow: AI → Ontology → Code](#workflow-ai-%E2%86%92-ontology-%E2%86%92-code)
  - [Best Practices](#best-practices)
  - [Next Steps](#next-steps)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# AI-Powered Generation

**Goal:** Use AI to generate ontologies, refine them, and generate code automatically.

**What you'll learn:** How to leverage AI for rapid ontology creation and code generation.

## Prerequisites

- ggen installed (see [Getting Started](getting-started.md))
- AI API key (Anthropic, OpenAI, or Ollama configured)

## Step 1: Generate Ontology with AI

Describe your domain in natural language:

```bash
ggen ai generate-ontology \
  --prompt "E-commerce platform: Product (name, price, sku), Order (items, total, customer), Customer (name, email, address)" \
  --output domain.ttl
```

AI generates a complete RDF ontology from your description.

## Step 2: Review and Refine

Inspect the generated ontology:

```bash
cat domain.ttl
```

Use AI to refine:

```bash
ggen ai chat \
  --prompt "Add product categories and inventory tracking to domain.ttl" \
  --input domain.ttl \
  --output domain-refined.ttl
```

## Step 3: Generate Code

Generate Rust models:

```bash
ggen template generate-rdf \
  --ontology domain-refined.ttl \
  --template rust-models \
  --output src/models.rs
```

## Step 4: AI-Assisted Analysis

Analyze your codebase and suggest ontology improvements:

```bash
ggen ai analyze src/ \
  --focus domain-model \
  --suggest-ontology-improvements
```

AI suggests ontology changes based on your code patterns.

## AI Features

### Generate Ontology
```bash
ggen ai generate-ontology \
  --prompt "Your domain description" \
  --output domain.ttl
```

### Interactive Chat
```bash
ggen ai chat --interactive
```

### Code Analysis
```bash
ggen ai analyze <path> \
  --focus <aspect> \
  --suggest-improvements
```

## Workflow: AI → Ontology → Code

1. **AI generates ontology** from natural language
2. **Review and refine** ontology (AI-assisted)
3. **Generate code** from ontology
4. **Analyze code** to suggest ontology improvements
5. **Iterate** for continuous improvement

## Best Practices

- Start with AI-generated ontologies, then refine manually
- Use AI analysis to discover missing ontology concepts
- Combine AI generation with manual SPARQL queries for complex domains
- Validate AI-generated ontologies with SHACL before generating code

## Next Steps

- **Learn ontology creation:** [Use RDF Ontologies Guide](../how-to-guides/use-rdf-ontologies.md)
- **CLI reference:** [AI Commands Reference](../reference/cli.md#ai-commands)
- **Understand concepts:** [Ontology-Driven Explanation](../explanations/ontology-driven.md)

