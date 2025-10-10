<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [Appendix A: Template Reference](#appendix-a-template-reference)
  - [Overview](#overview)
  - [Template Structure](#template-structure)
  - [Frontmatter Fields](#frontmatter-fields)
    - [**Required Fields**](#required-fields)
    - [**Optional Fields**](#optional-fields)
  - [Variable Definitions](#variable-definitions)
    - [**Variable Types**](#variable-types)
    - [**Validation Rules**](#validation-rules)
  - [Handlebars Syntax](#handlebars-syntax)
    - [**Basic Syntax**](#basic-syntax)
    - [**Built-in Helpers**](#built-in-helpers)
      - [**String Helpers**](#string-helpers)
      - [**Number Helpers**](#number-helpers)
      - [**Array Helpers**](#array-helpers)
      - [**Object Helpers**](#object-helpers)
      - [**Utility Helpers**](#utility-helpers)
  - [SPARQL Integration](#sparql-integration)
    - [**Query Results**](#query-results)
  - [Deterministic Generation](#deterministic-generation)
    - [**Deterministic Settings**](#deterministic-settings)
    - [**Best Practices**](#best-practices)
  - [Template Hooks](#template-hooks)
  - [Template Dependencies](#template-dependencies)
  - [Error Handling](#error-handling)
  - [Validation](#validation)
  - [Best Practices](#best-practices-1)
    - [**1. Clear Naming**](#1-clear-naming)
    - [**2. Comprehensive Documentation**](#2-comprehensive-documentation)
    - [**3. Validation Rules**](#3-validation-rules)
    - [**4. Error Handling**](#4-error-handling)
    - [**5. Testing**](#5-testing)
  - [Next Steps](#next-steps)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Appendix A: Template Reference

## Overview

This appendix provides a comprehensive reference for GGen templates, including syntax, built-in functions, helper libraries, and best practices. Use this as a quick reference when authoring templates.

## Template Structure

GGen templates use the `.tmpl` format with YAML frontmatter and Handlebars body:

```yaml
---
name: "Template Name"
description: "What this template generates"
variables:
  - name: variable_name
    description: "Description of the variable"
    type: string
    default: "default_value"
query: |
  PREFIX : <http://example.org/>
  SELECT ?subject ?predicate ?object
  WHERE {
    ?subject ?predicate ?object .
  }
---

{{! Template body using Handlebars syntax }}
{{#each results}}
// Generated from: {{subject}}
{{predicate}}: {{object}}
{{/each}}
```

## Frontmatter Fields

### **Required Fields**

- **`name`**: Human-readable name for the template
- **`description`**: Brief description of what the template generates

### **Optional Fields**

- **`variables`**: List of input variables with metadata
- **`query`**: SPARQL query for extracting data from knowledge graphs
- **`rdf`**: RDF/Turtle data to load into the graph
- **`sparql`**: Alternative name for `query` field
- **`determinism`**: Deterministic generation settings
- **`hooks`**: Pre/post processing hooks
- **`dependencies`**: Template dependencies
- **`tags`**: Tags for categorization
- **`version`**: Template version
- **`author`**: Template author information

## Variable Definitions

Variables are defined in the frontmatter with the following structure:

```yaml
variables:
  - name: user_name
    description: "Name of the user"
    type: string
    default: "Anonymous"
    required: true
    validation:
      pattern: "^[a-zA-Z0-9_]+$"
      min_length: 1
      max_length: 50
```

### **Variable Types**

- **`string`**: Text values
- **`number`**: Numeric values
- **`boolean`**: True/false values
- **`array`**: Lists of values
- **`object`**: Key-value pairs
- **`file`**: File paths or content

### **Validation Rules**

- **`pattern`**: Regular expression pattern
- **`min_length`**: Minimum string length
- **`max_length`**: Maximum string length
- **`min_value`**: Minimum numeric value
- **`max_value`**: Maximum numeric value
- **`required`**: Whether the variable is required
- **`choices`**: Allowed values for the variable

## Handlebars Syntax

GGen uses Handlebars templating with custom helpers:

### **Basic Syntax**

```handlebars
{{! Comments }}
{{variable}}                    {{! Variable interpolation }}
{{#if condition}}...{{/if}}     {{! Conditional blocks }}
{{#each items}}...{{/each}}     {{! Iteration }}
{{#with context}}...{{/with}}   {{! Context switching }}
```

### **Built-in Helpers**

#### **String Helpers**

- **`{{uppercase text}}`**: Convert to uppercase
- **`{{lowercase text}}`**: Convert to lowercase
- **`{{camelCase text}}`**: Convert to camelCase
- **`{{snake_case text}}`**: Convert to snake_case
- **`{{kebab-case text}}`**: Convert to kebab-case
- **`{{pascalCase text}}`**: Convert to PascalCase
- **`{{pluralize text}}`**: Pluralize the text
- **`{{singularize text}}`**: Singularize the text

#### **Number Helpers**

- **`{{add a b}}`**: Add two numbers
- **`{{subtract a b}}`**: Subtract b from a
- **`{{multiply a b}}`**: Multiply two numbers
- **`{{divide a b}}`**: Divide a by b
- **`{{modulo a b}}`**: Modulo operation

#### **Array Helpers**

- **`{{length array}}`**: Get array length
- **`{{first array}}`**: Get first element
- **`{{last array}}`**: Get last element
- **`{{join array separator}}`**: Join array elements
- **`{{sort array}}`**: Sort array elements
- **`{{unique array}}`**: Remove duplicates

#### **Object Helpers**

- **`{{keys object}}`**: Get object keys
- **`{{values object}}`**: Get object values
- **`{{has object key}}`**: Check if key exists
- **`{{get object key}}`**: Get value by key

#### **Utility Helpers**

- **`{{eq a b}}`**: Equality check
- **`{{ne a b}}`**: Inequality check
- **`{{gt a b}}`**: Greater than
- **`{{lt a b}}`**: Less than
- **`{{and a b}}`**: Logical AND
- **`{{or a b}}`**: Logical OR
- **`{{not value}}`**: Logical NOT

## SPARQL Integration

Templates can include SPARQL queries to extract data from knowledge graphs:

```yaml
query: |
  PREFIX : <http://example.org/>
  PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
  
  SELECT ?className ?propName ?propType
  WHERE {
    ?class a rdfs:Class ;
           rdfs:label ?className .
    ?prop rdfs:domain ?class ;
          rdfs:label ?propName ;
          rdfs:range ?propType .
  }
  ORDER BY ?className ?propName
```

### **Query Results**

SPARQL query results are available in the template as `results`:

```handlebars
{{#each results}}
// Class: {{className}}
// Property: {{propName}}
// Type: {{propType}}
{{/each}}
```

## Deterministic Generation

GGen ensures deterministic output through several mechanisms:

### **Deterministic Settings**

```yaml
determinism:
  seed: "fixed-seed-value"
  sort_results: true
  canonical_order: true
  no_timestamps: true
  no_random: true
```

### **Best Practices**

- Use `ORDER BY` clauses in SPARQL queries
- Avoid `{{now}}` and timestamp helpers
- Use content-based IDs instead of random ones
- Sort arrays and objects consistently

## Template Hooks

Templates can define pre and post-processing hooks:

```yaml
hooks:
  pre:
    - name: "validate-input"
      command: "ggen validate input"
    - name: "load-dependencies"
      command: "ggen deps install"
  post:
    - name: "format-code"
      command: "rustfmt"
    - name: "lint-code"
      command: "clippy"
    - name: "test-code"
      command: "cargo test"
```

## Template Dependencies

Templates can depend on other templates:

```yaml
dependencies:
  - name: "base-template"
    version: "1.0.0"
    source: "registry"
  - name: "utility-helpers"
    version: "2.1.0"
    source: "local"
```

## Error Handling

Templates should handle errors gracefully:

```handlebars
{{#if error}}
// Error: {{error.message}}
// {{error.details}}
{{else}}
// Generated code here
{{/if}}
```

## Validation

Templates are validated for:

- **Syntax**: Valid YAML frontmatter and Handlebars syntax
- **Semantics**: Correct variable definitions and query syntax
- **Security**: No dangerous operations or injections
- **Determinism**: No non-deterministic operations

## Best Practices

### **1. Clear Naming**
Use descriptive names for templates and variables:

```yaml
name: "Rust REST API Handler"
variables:
  - name: resource_name
    description: "Name of the resource (e.g., 'User', 'Post')"
```

### **2. Comprehensive Documentation**
Document all variables and their purpose:

```yaml
description: "Generate a RESTful API handler with validation, error handling, and tests"
variables:
  - name: resource_name
    description: "Name of the resource (e.g., 'User', 'Post')"
    type: string
    required: true
```

### **3. Validation Rules**
Define validation rules for all variables:

```yaml
variables:
  - name: email
    type: string
    validation:
      pattern: "^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}$"
      required: true
```

### **4. Error Handling**
Include error handling in templates:

```handlebars
{{#if error}}
// Error: {{error.message}}
// Please check your input and try again
{{else}}
// Generated code here
{{/if}}
```

### **5. Testing**
Test templates with various inputs:

```bash
# Test with valid input
ggen template apply my-template.tmpl --set resource_name="User"

# Test with invalid input
ggen template apply my-template.tmpl --set resource_name=""

# Test with edge cases
ggen template apply my-template.tmpl --set resource_name="VeryLongResourceNameThatExceedsNormalLength"
```

## Next Steps

- [A.1: Syntax Quick Reference](./appendix-a-1.md) - Quick syntax reference
- [A.2: Built-in Functions](./appendix-a-2.md) - Complete function reference
- [A.3: Helper Libraries](./appendix-a-3.md) - Additional helper libraries
