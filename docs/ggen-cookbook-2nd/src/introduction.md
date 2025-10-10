# Introduction

## The Challenge of Code Generation

Every software developer has faced repetitive coding tasks:

- Scaffolding new projects with boilerplate files
- Writing CRUD operations for database models
- Generating API clients from OpenAPI specifications
- Creating test suites that mirror production code
- Translating data schemas across language boundaries

Traditional approaches to code generation fall into two camps:

1. **Static scaffolding tools** that generate code once, leaving you to maintain it manually
2. **Dynamic frameworks** that generate code at runtime, creating tight coupling and magic

Both approaches have significant drawbacks. Static tools create **maintenance burden** - generated code drifts from the template over time. Dynamic frameworks introduce **complexity** and **vendor lock-in**.

## The GGen Philosophy

GGen takes a different approach, rooted in three core principles:

### 1. Templates as First-Class Artifacts

In GGen, templates are not second-class citizens. They are **versioned**, **tested**, **documented**, and **shared** just like production code. A template encodes not just structure, but domain knowledge and best practices.

### 2. Autonomic Properties

Drawing from autonomic computing research, GGen exhibits **self-configuration**, **self-optimization**, and **self-healing** properties. The system adapts to context, learns from usage, and validates outputs automatically.

### 3. Pattern Language Approach

Following Christopher Alexander's architectural patterns, GGen provides a **vocabulary of composable patterns** rather than monolithic generators. You combine patterns to create solutions tailored to your exact needs.

## What is a Pattern Language?

Christopher Alexander, in *A Pattern Language*, described how:

> "Each pattern describes a problem which occurs over and over again in our environment, and then describes the core of the solution to that problem, in such a way that you can use this solution a million times over, without ever doing it the same way twice."

In software, **design patterns** (Gang of Four) borrowed this concept for object-oriented design. GGen extends it to **code generation**:

- A **pattern** is a reusable template that solves a specific generation problem
- Patterns have **names** that form a shared vocabulary
- Patterns specify their **context** - when to use them
- Patterns compose into **sequences** that solve larger problems
- The collection of patterns forms a **language** for describing generation solutions

## The Generate-Validate-Refine Loop

At the heart of GGen is a simple but powerful loop:

```
┌─────────────┐
│  GENERATE   │  Apply template with input data
└──────┬──────┘
       │
       ▼
┌─────────────┐
│  VALIDATE   │  Check outputs against rules
└──────┬──────┘
       │
       ▼
┌─────────────┐
│   REFINE    │  Adjust template or input
└──────┬──────┘
       │
       └──────────┐
                  │
           (iterate until satisfied)
```

This loop embodies **autonomic** behavior:

- **Self-configuration**: Templates auto-detect context and apply intelligent defaults
- **Self-optimization**: The system learns which templates work best for which scenarios
- **Self-healing**: Validation catches errors early; recovery strategies adapt templates
- **Self-protection**: Deterministic generation ensures reproducibility

## A Simple Example

Let's see GGen in action with a concrete example. Suppose we want to generate a REST API handler in Rust:

**1. Create a template** (`api-handler.tmpl`):

```yaml
---
name: "Rust REST API Handler"
description: "Generate a RESTful API handler with validation"
variables:
  - name: resource_name
    description: "Name of the resource (e.g., 'User', 'Post')"
  - name: fields
    description: "List of fields with types"
    type: array
---
use actix_web::{web, HttpResponse, Result};
use serde::{Deserialize, Serialize};

#[derive(Debug, Serialize, Deserialize)]
pub struct {{resource_name}} {
{{#each fields}}
    pub {{this.name}}: {{this.type}},
{{/each}}
}

pub async fn create_{{resource_name | snake_case}}(
    data: web::Json<{{resource_name}}>
) -> Result<HttpResponse> {
    // TODO: Validate input
    // TODO: Save to database
    Ok(HttpResponse::Ok().json(data.0))
}

pub async fn get_{{resource_name | snake_case}}(
    id: web::Path<i32>
) -> Result<HttpResponse> {
    // TODO: Fetch from database
    Ok(HttpResponse::NotFound().finish())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[actix_rt::test]
    async fn test_create_{{resource_name | snake_case}}() {
        // TODO: Implement test
    }
}
```

**2. Generate code** with data:

```bash
ggen generate api-handler.tmpl \
  --set resource_name=User \
  --set fields='[{"name":"id","type":"i32"},{"name":"email","type":"String"}]' \
  --output src/handlers/user.rs
```

**3. Validate output**:

GGen automatically:
- Checks syntax (valid Rust)
- Applies formatting (`rustfmt`)
- Runs linters (`clippy`)
- Verifies template determinism

**4. Refine as needed**:

Adjust the template based on validation feedback, project conventions, or new requirements.

This simple example demonstrates:

- **Declarative templates** with metadata and variables
- **Handlebars syntax** with helpers (like `snake_case`)
- **Multi-file generation** (handler + tests in one template)
- **Automatic validation** ensuring quality
- **Determinism** - same inputs always produce same outputs

## Key Concepts

Before diving into the details, let's establish a shared vocabulary:

**Template**: A `.tmpl` file with frontmatter metadata and a body using templating syntax (Handlebars). Templates are the building blocks of generation.

**Pattern**: A named, reusable template that solves a specific generation problem in a specific context. Patterns compose into larger patterns.

**Generator**: A template or collection of templates that produces complete artifacts (files, projects, etc.).

**Registry**: A collection of templates (local or remote). The marketplace is a global registry.

**Autonomic**: Self-managing properties - configuration, optimization, healing, protection - that reduce manual intervention.

**Deterministic**: Given the same inputs, generation always produces identical outputs. Essential for reproducibility and testing.

**Variable**: A named input to a template. Variables have types, defaults, and validation rules.

**Helper**: A function available in templates for transformations (e.g., `snake_case`, `pluralize`).

**Frontmatter**: YAML metadata at the start of a `.tmpl` file, specifying name, description, variables, hooks, etc.

**Validation**: Automated checks ensuring generated outputs meet quality criteria (syntax, linting, tests).

## The Journey Ahead

This book will guide you through:

**Part I** grounds us in the philosophy of pattern languages and autonomic computing.

**Part II** demystifies the GGen engine and CLI, showing how the system works under the hood.

**Part III** is the heart of the book - a comprehensive guide to authoring templates, organized as a pattern language. You'll learn core patterns, advanced techniques, and best practices.

**Part IV** explores GGen's autonomic properties - how it configures, optimizes, and heals itself.

**Part V** situates GGen in its ecosystem - the marketplace, tooling integrations, and community.

**Part VI** addresses advanced scenarios - enterprise patterns, custom processors, and contributing to the core.

**Appendices** provide quick references for syntax, commands, and resources.

## Prerequisites

To get the most from this book, you should have:

- Basic familiarity with programming (any language)
- Comfort with command-line tools
- Understanding of version control (Git)
- Experience with at least one templating language (Jinja, Handlebars, etc.) is helpful but not required

We'll introduce concepts progressively, building from fundamentals to advanced topics.

## Setup

Before proceeding, ensure you have GGen installed:

```bash
# Install via Cargo (Rust package manager)
cargo install ggen

# Verify installation
ggen --version
```

For detailed installation instructions, see [Chapter 4: Command-Line Interface](./part-2/chapter-4.md).

## A Note on Examples

All examples in this book are available in the GGen repository:

```bash
git clone https://github.com/seanchatmangpt/ggen.git
cd ggen/examples
```

Examples are organized by chapter and pattern. You can run them directly:

```bash
ggen generate examples/chapter-6/pattern-1-single-file.tmpl
```

We encourage you to experiment, modify, and extend these examples.

## Contributing to This Book

Found an error? Have a suggestion? Want to contribute a new pattern?

This book is open-source and lives alongside the GGen codebase. See [Appendix C: Resources](./appendices/appendix-c.md) for contribution guidelines.

---

**Let's begin by understanding the foundation: What is a pattern language, and why does it matter for code generation?**

[Continue to Chapter 1: The Pattern Language →](./part-1/chapter-1.md)
