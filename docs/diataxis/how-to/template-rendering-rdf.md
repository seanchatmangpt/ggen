<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [How-To Guide: Writing Templates with RDF and SPARQL](#how-to-guide-writing-templates-with-rdf-and-sparql)
  - [Problem](#problem)
  - [Solution: The v2 RDF/SPARQL Engine](#solution-the-v2-rdfsparql-engine)
    - [Step 1: Provide RDF Context](#step-1-provide-rdf-context)
    - [Step 2: Define SPARQL Queries in Frontmatter](#step-2-define-sparql-queries-in-frontmatter)
    - [Step 3: Iterate Over Results in the Template Body](#step-3-iterate-over-results-in-the-template-body)
    - [Step 4: Multi-File Code Generation](#step-4-multi-file-code-generation)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# How-To Guide: Writing Templates with RDF and SPARQL

This guide explains how to leverage the `ggen` v2 template engine to integrate Open Ontologies directly into your generated code. The v2 engine allows you to define inline RDF contexts, execute SPARQL queries, and loop over semantic relationships directly within your Tera templates.

## Problem

You want to generate code or documentation based on semantic relationships defined in an ontology file (`.ttl`), without writing manual extraction logic in Rust or TypeScript.

## Solution: The v2 RDF/SPARQL Engine

### Step 1: Provide RDF Context

You can provide the RDF graph context to the template in two ways:
1. **CLI Flag:** Provide external TTL files when running the renderer.
   `ggen template render --rdf-file ontologies/data.ttl my-template.tmpl`
2. **Inline Frontmatter:** Define the context directly in the template frontmatter using `rdf_inline`.

### Step 2: Define SPARQL Queries in Frontmatter

Use the `sparql` block in the YAML frontmatter to define named queries that the template engine will execute prior to rendering.

```yaml
---
to: "output.txt"
prefixes: 
  ex: "http://example.org/"
rdf_inline:
  - "@prefix ex: <http://example.org/> . ex:Alice a ex:Person ."
  - "@prefix ex: <http://example.org/> . ex:Bob a ex:Person ."
sparql:
  people: "SELECT ?person WHERE { ?person a ex:Person }"
---
```

### Step 3: Iterate Over Results in the Template Body

The v2 engine binds the outputs of your queries to a `sparql_results` context variable. You can iterate over this object in Tera syntax:

```tera
Total people found: {{ sparql_results.people | length }}

List of people:
{% for row in sparql_results.people %}
- {{ row.person }}
{% endfor %}
```

### Step 4: Multi-File Code Generation

The v2 engine supports emitting multiple files from a single template pass using the `{# FILE: path #}` marker syntax. This is particularly useful when mapping an ontology of many classes to separate `.ts` or `.rs` files.

```tera
---
sparql:
  classes: "SELECT ?class_name WHERE { ?class a owl:Class . BIND(REPLACE(STR(?class), '^.*#', '') AS ?class_name) }"
---
{% for row in sparql_results.classes %}
{# FILE: src/models/{{ row.class_name }}.ts #}
export interface {{ row.class_name }} {
  id: string;
}
{% endfor %}
```

When `ggen` detects these file markers, it splits the output buffer and safely writes each chunk to the specified path, creating directories as needed.
