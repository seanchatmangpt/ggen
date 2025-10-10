<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [Chapter 8: Data-Driven Generation](#chapter-8-data-driven-generation)
  - [Overview](#overview)
  - [What You'll Learn](#what-youll-learn)
  - [Chapter Structure](#chapter-structure)
  - [Key Concepts](#key-concepts)
    - [**Data-Driven Generation**](#data-driven-generation)
    - [**Semantic Models**](#semantic-models)
    - [**Schema Mapping**](#schema-mapping)
    - [**API-First Generation**](#api-first-generation)
  - [Data Source Hierarchy](#data-source-hierarchy)
  - [Pattern 001: Knowledge-First Projection](#pattern-001-knowledge-first-projection)
  - [Data Processing Pipeline](#data-processing-pipeline)
  - [Example: Multi-Source Generation](#example-multi-source-generation)
    - [**1. Domain Model (RDF)**](#1-domain-model-rdf)
    - [**2. Database Schema**](#2-database-schema)
    - [**3. API Specification**](#3-api-specification)
    - [**4. Generated Code**](#4-generated-code)
  - [Benefits of Data-Driven Generation](#benefits-of-data-driven-generation)
    - [**1. Consistency**](#1-consistency)
    - [**2. Traceability**](#2-traceability)
    - [**3. Multi-Target Generation**](#3-multi-target-generation)
    - [**4. Evolution**](#4-evolution)
    - [**5. Validation**](#5-validation)
  - [Best Practices](#best-practices)
    - [**1. Choose the Right Data Source**](#1-choose-the-right-data-source)
    - [**2. Validate Early**](#2-validate-early)
    - [**3. Use Semantic Queries**](#3-use-semantic-queries)
    - [**4. Maintain Data Sources**](#4-maintain-data-sources)
  - [Next Steps](#next-steps)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Chapter 8: Data-Driven Generation

## Overview

This chapter explores how GGen generates code from various data sources. We'll examine JSON/YAML input, RDF semantic models, database schemas, and API specifications, showing how each data source enables different types of code generation.

## What You'll Learn

- How to use JSON and YAML as input sources for templates
- How RDF knowledge graphs serve as the semantic foundation for generation
- Techniques for generating code from database schemas
- How to create generators from API specifications

## Chapter Structure

- [8.1 JSON/YAML Input](./chapter-8-1.md) - Using structured data files
- [8.2 RDF & Semantic Models](./chapter-8-2.md) - Knowledge graphs as source of truth
- [8.3 Database Schema to Code](./chapter-8-3.md) - Generating from database schemas
- [8.4 API Specification Generators](./chapter-8-4.md) - Creating code from API specs

## Key Concepts

### **Data-Driven Generation**
The process of generating code from structured data sources rather than hardcoded templates.

### **Semantic Models**
RDF knowledge graphs that provide rich semantic information about domains and relationships.

### **Schema Mapping**
The process of transforming database schemas into code structures.

### **API-First Generation**
Generating code from API specifications to ensure consistency between API and implementation.

## Data Source Hierarchy

GGen supports multiple data sources, each with different capabilities:

```
┌─────────────────────────────────────────────────────────────┐
│                    Data Source Hierarchy                      │
└─────────────────────────────────────────────────────────────┘

    ┌─────────────┐
    │   RDF/TTL   │  ← Most semantic, most powerful
    │ Knowledge   │
    │   Graphs    │
    └─────────────┘
           │
           ▼
    ┌─────────────┐
    │  Database   │  ← Structured, relational
    │  Schemas    │
    └─────────────┘
           │
           ▼
    ┌─────────────┐
    │ API Specs   │  ← Contract-based, service-oriented
    │ (OpenAPI,   │
    │ GraphQL)    │
    └─────────────┘
           │
           ▼
    ┌─────────────┐
    │ JSON/YAML   │  ← Simple, flexible, human-readable
    │   Files     │
    └─────────────┘
```

## Pattern 001: Knowledge-First Projection

At the foundation of GGen's data-driven approach is **Pattern 001: KNOWLEDGE-FIRST PROJECTION**:

**Context**: You need to generate code from domain models, but templates could accept arbitrary data structures.

**Problem**: How do you ensure generated code is consistent, traceable, and semantically grounded?

**Solution**: Establish the knowledge graph as the single source of truth. Before any template execution, load semantic data into an RDF graph, query it using SPARQL, and project results into template-friendly structures.

**Consequences**: 
- ✅ Semantic consistency across all generated artifacts
- ✅ Traceability from generated code back to domain concepts
- ✅ Multi-language generation from the same semantic source
- ❌ Requires investment in knowledge graph creation and maintenance
- ❌ Adds complexity to the generation pipeline

## Data Processing Pipeline

All data sources flow through a consistent processing pipeline:

```
Data Source → Validation → Projection → Template → Generated Code
     │            │           │           │            │
     ▼            ▼           ▼           ▼            ▼
  RDF/TTL      Syntax      SPARQL      Handlebars    Rust
  JSON/YAML    Check       Queries     Rendering     Python
  DB Schema    Rules       Results     Engine        TypeScript
  API Spec     Validation  Mapping     Processing    Java
```

## Example: Multi-Source Generation

Consider generating a complete web application:

### **1. Domain Model (RDF)**
```turtle
@prefix : <http://example.org/> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .

:User a rdfs:Class ;
    rdfs:label "User" ;
    rdfs:comment "A system user with authentication" .

:name a rdf:Property ;
    rdfs:domain :User ;
    rdfs:range xsd:string ;
    rdfs:label "name" .
```

### **2. Database Schema**
```sql
CREATE TABLE users (
    id SERIAL PRIMARY KEY,
    name VARCHAR(255) NOT NULL,
    email VARCHAR(255) UNIQUE NOT NULL,
    created_at TIMESTAMP DEFAULT NOW()
);
```

### **3. API Specification**
```yaml
openapi: 3.0.0
info:
  title: User API
  version: 1.0.0
paths:
  /users:
    get:
      summary: List users
      responses:
        '200':
          description: List of users
          content:
            application/json:
              schema:
                type: array
                items:
                  $ref: '#/components/schemas/User'
```

### **4. Generated Code**
From these sources, GGen can generate:

- **Rust structs** from the RDF model
- **Database migrations** from the schema
- **API handlers** from the OpenAPI spec
- **Tests** that validate all components work together

## Benefits of Data-Driven Generation

### **1. Consistency**
All generated code reflects the same underlying data model, ensuring consistency across different parts of the system.

### **2. Traceability**
Generated code can be traced back to specific data elements, making it easier to understand and maintain.

### **3. Multi-Target Generation**
The same data source can generate code for multiple languages and frameworks.

### **4. Evolution**
When the data source changes, all generated code can be updated automatically.

### **5. Validation**
Data sources can be validated independently, ensuring generated code is based on correct information.

## Best Practices

### **1. Choose the Right Data Source**
- **RDF/TTL**: For complex domains with rich relationships
- **Database Schemas**: For data-centric applications
- **API Specs**: For service-oriented architectures
- **JSON/YAML**: For simple, human-readable configurations

### **2. Validate Early**
Validate data sources before using them for generation:
- Check syntax and structure
- Verify semantic correctness
- Ensure completeness and consistency

### **3. Use Semantic Queries**
When possible, use SPARQL queries to extract exactly what you need:
- More precise than simple data access
- Enables complex relationships and constraints
- Provides better traceability

### **4. Maintain Data Sources**
Keep data sources up to date and well-documented:
- Version control data sources
- Document changes and rationale
- Test data source changes

## Next Steps

Start with [8.1: JSON/YAML Input](./chapter-8-1.md) to understand the simplest data-driven approach, then explore more sophisticated sources.
