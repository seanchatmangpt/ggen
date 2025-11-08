# AI-Powered Code Generation Guide

**ggen v2.5.0** | **AI Integration** | **Multi-Provider Support**

---

## Overview

ggen revolutionizes code generation by combining **AI-powered natural language processing** with **formal RDF ontologies**. This guide shows you how to leverage AI to:

1. Generate ontologies from natural language descriptions
2. Create code with AI assistance and context awareness
3. Analyze and improve existing codebases
4. Build interactive AI-assisted development workflows

---

## Table of Contents

- [Quick Start](#quick-start)
- [Multi-Provider Configuration](#multi-provider-configuration)
- [Command Reference](#command-reference)
- [Workflows and Examples](#workflows-and-examples)
- [Best Practices](#best-practices)
- [Troubleshooting](#troubleshooting)

---

## Quick Start

### Installation Verification

```bash
# Check if AI commands are available
ggen ai --help

# Verify system setup
ggen utils doctor
```

### Your First AI-Generated Ontology

```bash
# Generate an ontology from natural language
ggen ai generate-ontology \
  "Create a task management system with projects, tasks, and users" \
  --output tasks.ttl \
  --model gpt-4

# View the generated ontology
cat tasks.ttl

# Generate code from the ontology
ggen project gen task-manager --graph tasks.ttl
```

---

## Multi-Provider Configuration

ggen supports **three AI providers** for maximum flexibility:

### 1. OpenAI (GPT Models)

**Models:** `gpt-4`, `gpt-4-turbo`, `gpt-3.5-turbo`

**Setup:**
```bash
# Set provider and API key
export GGEN_AI_PROVIDER=openai
export OPENAI_API_KEY=sk-your-key-here

# Or via ggen utils
ggen utils env --set GGEN_AI_PROVIDER=openai
ggen utils env --set OPENAI_API_KEY=sk-your-key-here
```

**Usage:**
```bash
ggen ai generate "Create REST API" --model gpt-4
```

**Best For:**
- Production code generation
- Fast iteration cycles
- Cost-effective at scale

---

### 2. Anthropic (Claude Models)

**Models:** `claude-3-opus-20240229`, `claude-3-sonnet-20240229`, `claude-3-haiku-20240307`

**Setup:**
```bash
# Set provider and API key
export GGEN_AI_PROVIDER=anthropic
export ANTHROPIC_API_KEY=sk-ant-your-key-here

# Or via ggen utils
ggen utils env --set GGEN_AI_PROVIDER=anthropic
ggen utils env --set ANTHROPIC_API_KEY=sk-ant-your-key-here
```

**Usage:**
```bash
ggen ai generate "Create REST API" --model claude-3-opus-20240229
```

**Best For:**
- Complex reasoning tasks
- Large context windows (200k tokens)
- Detailed code analysis

---

### 3. Local Models (Ollama/LM Studio)

**Models:** `codellama`, `deepseek-coder`, `mistral`, custom models

**Setup:**
```bash
# Start Ollama server
ollama serve

# Pull a code model
ollama pull codellama

# Configure ggen
export GGEN_AI_PROVIDER=local
export GGEN_LOCAL_MODEL=codellama
export GGEN_LOCAL_ENDPOINT=http://localhost:11434
```

**Usage:**
```bash
ggen ai generate "Create REST API" --model codellama
```

**Best For:**
- Privacy-first development
- Offline coding
- No API costs
- Custom fine-tuned models

---

## Command Reference

### `ggen ai generate-ontology`

**Generate RDF ontologies from natural language descriptions**

#### Syntax

```bash
ggen ai generate-ontology <description> [OPTIONS]
```

#### Arguments

| Argument | Required | Description |
|----------|----------|-------------|
| `<description>` | ✅ | Natural language description of domain |

#### Options

| Option | Short | Type | Default | Description |
|--------|-------|------|---------|-------------|
| `--output` | `-o` | path | stdout | Output file path (.ttl) |
| `--model` | `-m` | string | `gpt-3.5-turbo` | AI model to use |
| `--api-key` | | string | env | API key (overrides env) |
| `--max-tokens` | | int | 4000 | Maximum tokens in response |
| `--temperature` | `-t` | float | 0.7 | Creativity (0.0-1.0) |
| `--format` | `-f` | enum | turtle | Output format (turtle/ntriples/rdfxml/jsonld) |

#### Examples

**Basic Usage:**
```bash
ggen ai generate-ontology "E-commerce system with products and orders" \
  --output ecommerce.ttl
```

**With Specific Model:**
```bash
ggen ai generate-ontology "Blog platform with posts, comments, tags" \
  --model gpt-4 \
  --output blog.ttl \
  --temperature 0.3
```

**Complex Domain:**
```bash
ggen ai generate-ontology \
  "Healthcare system with:
   - Patients (name, DOB, medical record number)
   - Doctors (name, specialization, license number)
   - Appointments (date, time, status)
   - Prescriptions (medication, dosage, duration)" \
  --output healthcare.ttl \
  --model claude-3-opus-20240229 \
  --max-tokens 8000
```

**Output Example:**
```turtle
@prefix ex: <http://example.org/ecommerce#> .
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

# Classes
ex:Product a rdfs:Class ;
    rdfs:label "Product" ;
    rdfs:comment "A product in the e-commerce system" .

ex:Order a rdfs:Class ;
    rdfs:label "Order" ;
    rdfs:comment "A customer order" .

ex:Customer a rdfs:Class ;
    rdfs:label "Customer" ;
    rdfs:comment "A customer account" .

# Properties
ex:productName a rdf:Property ;
    rdfs:domain ex:Product ;
    rdfs:range xsd:string ;
    rdfs:label "Product Name" .

ex:price a rdf:Property ;
    rdfs:domain ex:Product ;
    rdfs:range xsd:decimal ;
    rdfs:label "Price" .

ex:orderDate a rdf:Property ;
    rdfs:domain ex:Order ;
    rdfs:range xsd:dateTime ;
    rdfs:label "Order Date" .

ex:containsProduct a rdf:Property ;
    rdfs:domain ex:Order ;
    rdfs:range ex:Product ;
    rdfs:label "Contains Product" .
```

---

### `ggen ai generate`

**Generate code with AI assistance**

#### Syntax

```bash
ggen ai generate <prompt> [OPTIONS]
```

#### Arguments

| Argument | Required | Description |
|----------|----------|-------------|
| `<prompt>` | ✅ | Description of code to generate |

#### Options

| Option | Type | Default | Description |
|--------|------|---------|-------------|
| `--code` | string | - | Existing code for context |
| `--model` | string | `gpt-3.5-turbo` | AI model to use |
| `--api-key` | string | env | API key override |
| `--suggestions` | bool | false | Include improvement suggestions |
| `--language` | string | auto | Target language (rust/python/typescript) |
| `--max-tokens` | int | 2000 | Maximum response tokens |
| `--temperature` | float | 0.7 | Response creativity |

#### Examples

**Basic Code Generation:**
```bash
ggen ai generate "Create a Rust function that calculates Fibonacci numbers"
```

**Output:**
```json
{
  "generated_code": "fn fibonacci(n: u64) -> u64 {\n    match n {\n        0 => 0,\n        1 => 1,\n        _ => fibonacci(n - 1) + fibonacci(n - 2)\n    }\n}",
  "language": "rust",
  "model": "gpt-3.5-turbo",
  "tokens_used": 156
}
```

**With Existing Code Context:**
```bash
ggen ai generate "Add error handling to this function" \
  --code "fn divide(a: f64, b: f64) -> f64 { a / b }" \
  --language rust
```

**Output:**
```json
{
  "generated_code": "fn divide(a: f64, b: f64) -> Result<f64, String> {\n    if b == 0.0 {\n        Err(\"Division by zero\".to_string())\n    } else {\n        Ok(a / b)\n    }\n}",
  "language": "rust",
  "model": "gpt-3.5-turbo"
}
```

**With Suggestions:**
```bash
ggen ai generate "Create a REST API server" \
  --language rust \
  --suggestions \
  --model gpt-4
```

---

### `ggen ai chat`

**Interactive AI chat sessions for development assistance**

#### Syntax

```bash
ggen ai chat [message] [OPTIONS]
```

#### Arguments

| Argument | Required | Description |
|----------|----------|-------------|
| `[message]` | Optional | Single message (omit for interactive mode) |

#### Options

| Option | Type | Default | Description |
|--------|------|---------|-------------|
| `--model` | string | `gpt-3.5-turbo` | AI model to use |
| `--api-key` | string | env | API key override |
| `--interactive` | bool | false | Start interactive session |
| `--stream` | bool | false | Stream responses in real-time |
| `--max-tokens` | int | 2000 | Maximum tokens per response |
| `--temperature` | float | 0.7 | Response creativity |

#### Examples

**Single Question:**
```bash
ggen ai chat "Explain Rust ownership and borrowing"
```

**Interactive Mode:**
```bash
ggen ai chat --interactive --model claude-3-sonnet-20240229
```

**Interactive Session Example:**
```
🤖 AI Chat - Interactive Mode
Model: claude-3-sonnet-20240229
Type 'exit' or 'quit' to end session

> How do I implement async/await in Rust?

🤖: To implement async/await in Rust, you need:

1. Add the tokio runtime to Cargo.toml:
   [dependencies]
   tokio = { version = "1", features = ["full"] }

2. Mark your main function as async:
   #[tokio::main]
   async fn main() {
       // Your async code here
   }

3. Use .await on async functions:
   async fn fetch_data() -> Result<String, Error> {
       // async operations
   }

> Show me an example with reqwest

🤖: Here's a complete example using reqwest for HTTP requests:
   [code example follows...]
```

**Streaming Responses:**
```bash
ggen ai chat "Write a comprehensive Rust web server tutorial" --stream
```

---

### `ggen ai analyze`

**Analyze code with AI insights**

#### Syntax

```bash
ggen ai analyze [code|--file|--project] [OPTIONS]
```

#### Arguments

| Argument | Required | Description |
|----------|----------|-------------|
| `[code]` | One of | Code string to analyze |
| `--file` | | File path to analyze |
| `--project` | | Project directory to analyze |

#### Options

| Option | Type | Default | Description |
|--------|------|---------|-------------|
| `--model` | string | `gpt-3.5-turbo` | AI model to use |
| `--api-key` | string | env | API key override |
| `--complexity` | bool | false | Include complexity analysis |
| `--security` | bool | false | Security considerations |
| `--performance` | bool | false | Performance optimization |
| `--max-tokens` | int | 4000 | Maximum analysis tokens |

#### Examples

**Analyze Code String:**
```bash
ggen ai analyze "fn main() { let x = vec![1,2,3]; for i in x { println!(\"{}\", i); } }"
```

**Output:**
```json
{
  "insights": [
    "Uses Rust's ownership system correctly with move semantics",
    "Iterator pattern applied with for loop",
    "Vector initialization is concise and idiomatic"
  ],
  "suggestions": [
    "Consider using .iter() for borrowed iteration if x is needed later",
    "Use {:?} debug formatting for better output",
    "Add type annotations for clarity in larger projects"
  ],
  "model": "gpt-3.5-turbo"
}
```

**Analyze File with Security Focus:**
```bash
ggen ai analyze \
  --file src/api/auth.rs \
  --security \
  --model gpt-4
```

**Output:**
```json
{
  "file_path": "src/api/auth.rs",
  "insights": [
    "Password hashing implemented with bcrypt",
    "JWT tokens used for session management",
    "Input validation on all endpoints"
  ],
  "suggestions": [
    "Add rate limiting to prevent brute force attacks",
    "Implement password strength requirements",
    "Use secure random for token generation",
    "Add CSRF protection for state-changing operations"
  ],
  "model": "gpt-4"
}
```

**Analyze Project with Complexity:**
```bash
ggen ai analyze \
  --project . \
  --complexity \
  --performance \
  --model claude-3-opus-20240229
```

**Output:**
```json
{
  "file_path": ".",
  "insights": [
    "Well-structured Cargo workspace with 8 crates",
    "Clear separation of CLI, domain, and core layers",
    "Consistent async/await usage throughout"
  ],
  "suggestions": [
    "Consider extracting common types to shared crate",
    "Add connection pooling for database operations",
    "Implement caching for frequently accessed data"
  ],
  "complexity_score": 45.2,
  "model": "claude-3-opus-20240229"
}
```

---

## Workflows and Examples

### Workflow 1: Natural Language → RDF → Code

**Complete E2E workflow for building a domain model**

#### Step 1: Define Your Domain

Write a natural language description:

```
"Social media platform with:
 - Users (username, email, bio, avatar)
 - Posts (content, timestamp, likes, author)
 - Comments (text, author, post, timestamp)
 - Follows (follower, following, since)"
```

#### Step 2: Generate Ontology

```bash
ggen ai generate-ontology \
  "Social media platform with users, posts, comments, and follows" \
  --output social.ttl \
  --model gpt-4 \
  --temperature 0.3
```

#### Step 3: Validate Ontology

```bash
# Load and validate
ggen graph load social.ttl

# Visualize structure
ggen graph visualize social.ttl --output social.svg

# Lint for issues
ggen template lint --graph social.ttl
```

#### Step 4: Generate Code

```bash
# Generate Rust project
ggen project gen social-media \
  --graph social.ttl \
  --template rust-actix-api

# View generated structure
tree social-media/
```

#### Step 5: Iterate with AI

```bash
# Analyze generated code
ggen ai analyze --project social-media --performance

# Generate additional features
ggen ai generate "Add authentication middleware" \
  --code "$(cat social-media/src/main.rs)" \
  --language rust
```

---

### Workflow 2: AI-Assisted Code Refinement

**Use AI to improve existing codebases**

#### Step 1: Analyze Current Code

```bash
ggen ai analyze \
  --file src/main.rs \
  --complexity \
  --security \
  --performance
```

#### Step 2: Get Specific Improvements

```bash
ggen ai generate "Refactor this code for better performance" \
  --code "$(cat src/main.rs)" \
  --suggestions
```

#### Step 3: Interactive Refinement

```bash
ggen ai chat --interactive --model gpt-4

> I have a function that's too complex (complexity score 78). How should I refactor it?
[paste code]

> What are the most critical security issues?

> Generate unit tests for the refactored version
```

---

### Workflow 3: Domain Evolution

**Update your domain model and regenerate code**

#### Step 1: Current State

```bash
# Existing ontology
cat domain.ttl
```

#### Step 2: Describe Changes

```bash
ggen ai generate-ontology \
  "Add these features to the existing e-commerce system:
   - Product reviews with ratings
   - Wishlist functionality
   - Product recommendations based on purchase history" \
  --output domain-v2.ttl \
  --model claude-3-opus-20240229
```

#### Step 3: Merge Ontologies

```bash
# Manual merge or use SPARQL update
ggen graph load domain.ttl domain-v2.ttl --output merged.ttl
```

#### Step 4: Regenerate Code

```bash
ggen project gen . \
  --graph merged.ttl \
  --force \
  --backup
```

#### Step 5: Set Up Auto-Regeneration Hook

```bash
ggen hook create \
  --event on-ontology-change \
  --script ./scripts/regenerate.sh \
  --name "auto-regen-on-ontology-update"
```

---

## Best Practices

### 1. Model Selection

**Use GPT-4 for:**
- ✅ Production code generation
- ✅ Complex domain modeling
- ✅ Critical security analysis

**Use GPT-3.5 for:**
- ✅ Rapid prototyping
- ✅ Simple code generation
- ✅ Cost-sensitive operations

**Use Claude 3 Opus for:**
- ✅ Large context analysis (200k tokens)
- ✅ Detailed architectural reviews
- ✅ Complex reasoning tasks

**Use Local Models for:**
- ✅ Privacy-first development
- ✅ Offline coding
- ✅ High-frequency iterations

### 2. Prompt Engineering

**Be Specific:**
```bash
# ❌ Vague
ggen ai generate-ontology "A system"

# ✅ Specific
ggen ai generate-ontology "Inventory management system with:
 - Products (SKU, name, quantity, warehouse location)
 - Warehouses (ID, address, capacity)
 - Transfers (from_warehouse, to_warehouse, product, quantity, date)"
```

**Provide Context:**
```bash
# ❌ No context
ggen ai generate "Add logging"

# ✅ With context
ggen ai generate "Add structured logging with tracing crate" \
  --code "$(cat src/main.rs)" \
  --language rust
```

**Iterate Incrementally:**
```bash
# Start broad
ggen ai generate-ontology "Blog platform" --output blog-v1.ttl

# Refine with chat
ggen ai chat --interactive
> Expand the blog ontology with SEO metadata, social sharing, and analytics

# Generate final version
ggen ai generate-ontology "Blog platform with [refined requirements]" \
  --output blog-v2.ttl
```

### 3. Version Control

**Always commit ontologies:**
```bash
git add domain.ttl
git commit -m "feat: Add product review ontology"
```

**Tag ontology versions:**
```bash
git tag v1.0.0-ontology
git push --tags
```

**Use hooks for validation:**
```bash
ggen hook create \
  --event pre-commit \
  --script ./scripts/validate-ontology.sh
```

### 4. Testing AI-Generated Code

**Never trust blindly - always test:**

```bash
# Generate code
ggen ai generate "Create authentication system" > auth.rs

# Analyze for issues
ggen ai analyze --file auth.rs --security --performance

# Write tests
ggen ai generate "Generate unit tests for this code" \
  --code "$(cat auth.rs)"

# Run tests
cargo test
```

### 5. Cost Management

**Monitor token usage:**
```bash
# Check usage in output
ggen ai generate "..." --model gpt-4 | jq '.tokens_used'

# Use cheaper models for iteration
ggen ai generate "..." --model gpt-3.5-turbo

# Switch to local models for high-volume
export GGEN_AI_PROVIDER=local
```

---

## Troubleshooting

### API Key Issues

**Problem:** `API key not found`

**Solution:**
```bash
# Verify environment variable
echo $OPENAI_API_KEY

# Set via ggen utils
ggen utils env --set OPENAI_API_KEY=sk-...

# Or export directly
export OPENAI_API_KEY=sk-...
```

### Model Not Available

**Problem:** `Model 'gpt-4' not available`

**Solution:**
```bash
# Check your API plan
# Use alternative model
ggen ai generate "..." --model gpt-3.5-turbo

# Or use local model
ggen ai generate "..." --model codellama
```

### Rate Limits

**Problem:** `Rate limit exceeded`

**Solution:**
```bash
# Add delays between requests
sleep 1 && ggen ai generate "..."

# Use local model
export GGEN_AI_PROVIDER=local
ggen ai generate "..."

# Reduce max_tokens
ggen ai generate "..." --max-tokens 1000
```

### Invalid Ontology Output

**Problem:** Generated ontology has syntax errors

**Solution:**
```bash
# Validate with graph load
ggen graph load output.ttl

# If errors, regenerate with stricter parameters
ggen ai generate-ontology "..." \
  --temperature 0.1 \
  --model gpt-4

# Or use Claude for better structure
ggen ai generate-ontology "..." \
  --model claude-3-opus-20240229
```

### Large Project Analysis Timeout

**Problem:** `ggen ai analyze --project . ` times out

**Solution:**
```bash
# Analyze specific subdirectories
ggen ai analyze --project src/

# Increase max tokens
ggen ai analyze --project . --max-tokens 8000

# Use Claude 3 with larger context
ggen ai analyze --project . \
  --model claude-3-opus-20240229
```

---

## Advanced Topics

### Custom Prompts with Templates

Create reusable prompt templates:

**File: `templates/api-prompt.txt`**
```
Generate a REST API in Rust with:
- Framework: {framework}
- Database: {database}
- Authentication: {auth_method}
- Features: {features}

Include:
- Error handling with anyhow
- Async/await with tokio
- Database migrations
- OpenAPI documentation
```

**Usage:**
```bash
# Expand template
PROMPT=$(cat templates/api-prompt.txt | \
  sed 's/{framework}/actix-web/' | \
  sed 's/{database}/PostgreSQL/' | \
  sed 's/{auth_method}/JWT/' | \
  sed 's/{features}/CRUD operations, pagination/')

# Generate code
ggen ai generate "$PROMPT" --model gpt-4
```

### Chain Multiple AI Operations

**Script: `ai-workflow.sh`**
```bash
#!/bin/bash

# 1. Generate ontology
ggen ai generate-ontology "$1" --output temp.ttl

# 2. Analyze ontology
ggen ai analyze --file temp.ttl --complexity

# 3. Generate code
ggen project gen temp-project --graph temp.ttl

# 4. Analyze generated code
ggen ai analyze --project temp-project --security --performance

# 5. Generate tests
for file in temp-project/src/*.rs; do
  ggen ai generate "Generate unit tests" --code "$(cat $file)"
done
```

### Integration with CI/CD

**GitHub Actions Example:**

```yaml
name: AI-Powered Code Review

on: [pull_request]

jobs:
  ai-review:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3

      - name: Install ggen
        run: cargo install ggen

      - name: AI Analysis
        env:
          OPENAI_API_KEY: ${{ secrets.OPENAI_API_KEY }}
        run: |
          ggen ai analyze --project . --security --performance > ai-review.json

      - name: Post Results
        uses: actions/github-script@v6
        with:
          script: |
            const fs = require('fs');
            const analysis = JSON.parse(fs.readFileSync('ai-review.json'));
            github.rest.issues.createComment({
              issue_number: context.issue.number,
              owner: context.repo.owner,
              repo: context.repo.repo,
              body: `## AI Code Review\n\n${JSON.stringify(analysis, null, 2)}`
            });
```

---

## Next Steps

1. **Explore Examples:** See `docs/src/examples/` for complete projects
2. **Join Community:** Share your AI-generated ontologies
3. **Contribute Templates:** Submit prompt templates for common use cases
4. **Advanced Features:** Try neural code generation (v2.6.0+)

---

## References

- **Release Notes:** `docs/src/whats-new-2.5.0.md`
- **Hooks Guide:** `docs/src/guides/hooks.md`
- **Ontology Patterns:** `docs/src/guides/ontology-patterns.md` (coming soon)
- **Model Comparison:** `docs/src/guides/model-selection.md` (coming soon)
