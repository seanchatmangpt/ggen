# Claude Skills - Progressive Disclosure System

## Overview

This directory contains modular skill documentation with progressive loading strategy. Skills are loaded on-demand to minimize token usage while maintaining fast access to critical information.

## Loading Strategy

### 1. Metadata First (100 bytes)
```json
{
  "id": "cargo-make",
  "name": "Cargo Make Protocol",
  "tags": ["rust", "build"],
  "size": 2048,
  "requires": [],
  "autoload": false
}
```

### 2. Content On-Demand (2KB)
Content files are loaded only when needed based on context or explicit request.

### 3. References When Needed
Related skills are loaded based on `requires` field in metadata.

## Directory Structure

```
.claude/skills/
├── _metadata/          # Lightweight metadata only (100 bytes each)
│   ├── index.json      # All skills registry
│   ├── cargo-make.meta.json
│   ├── chicago-tdd.meta.json
│   ├── type-first.meta.json
│   ├── ontologies.meta.json
│   └── sparql.meta.json
├── rust/               # Rust-specific skills (2KB each)
│   ├── cargo-make.md
│   ├── chicago-tdd.md
│   └── type-first.md
├── rdf/                # RDF-specific skills (2KB each)
│   ├── ontologies.md
│   └── sparql.md
└── README.md           # This file
```

## Available Skills

### Rust Skills

#### cargo-make
**Tags**: rust, build, quality
**Description**: MANDATORY cargo make protocol - NEVER use direct cargo commands
**When to load**: Always before any build/test operations

#### chicago-tdd
**Tags**: rust, testing, tdd
**Description**: Chicago TDD methodology with AAA pattern
**When to load**: Before writing tests or implementing TDD

#### type-first
**Tags**: rust, design, types
**Description**: Elite Rust type-first design patterns
**When to load**: Before designing APIs or type systems

### RDF Skills

#### ontologies
**Tags**: rdf, ontology, sparql
**Description**: RDF ontology patterns and holographic factory pipeline
**When to load**: Before working with .specify/*.ttl files

#### sparql
**Tags**: rdf, sparql, query
**Description**: SPARQL query patterns for code generation
**When to load**: Before writing SPARQL queries or extraction logic
**Requires**: ontologies

## Usage Patterns

### Automatic Loading
1. Read `_metadata/index.json` (always loaded)
2. Identify relevant skills based on task
3. Load skill metadata from `_metadata/*.meta.json`
4. Load skill content only if needed

### Manual Loading
Reference a skill explicitly:
```
Load cargo-make skill
Load chicago-tdd skill
Load type-first skill
```

### Dependency Resolution
Skills with `requires` field automatically load dependencies:
```json
{
  "id": "sparql",
  "requires": ["ontologies"]
}
```
Loading `sparql` will also load `ontologies`.

## Benefits

1. **Token Efficiency**: Load only what's needed (100 bytes vs 2KB)
2. **Fast Access**: Metadata always available for quick lookup
3. **Clear Organization**: Skills grouped by domain
4. **Dependency Management**: Automatic loading of prerequisites
5. **Scalability**: Add new skills without increasing base context

## Adding New Skills

1. Create metadata file in `_metadata/`:
```json
{
  "id": "new-skill",
  "name": "New Skill Name",
  "tags": ["tag1", "tag2"],
  "size": 2048,
  "requires": [],
  "autoload": false
}
```

2. Add entry to `_metadata/index.json`:
```json
{
  "skills": [
    {
      "id": "new-skill",
      "name": "New Skill Name",
      "tags": ["tag1", "tag2"],
      "size": 2048,
      "requires": [],
      "autoload": false
    }
  ]
}
```

3. Create content file in appropriate subdirectory:
```markdown
# New Skill Name

## Overview
...

## Core Concepts
...

## Examples
...
```

## Cache Policy

- **Metadata**: Always loaded, cached in memory
- **Content**: Loaded on-demand, cached for session
- **Dependencies**: Loaded with parent, cached for session

## Performance Characteristics

- Metadata load: < 1ms (600 bytes total)
- Content load: < 10ms (2KB per skill)
- Dependency resolution: < 5ms (graph traversal)

## Integration with CLAUDE.md

Skills complement CLAUDE.md instructions:
- **CLAUDE.md**: Core rules, absolute requirements, project structure
- **Skills**: Detailed methodology, patterns, examples

Skills are referenced from CLAUDE.md but loaded separately to minimize base context.
