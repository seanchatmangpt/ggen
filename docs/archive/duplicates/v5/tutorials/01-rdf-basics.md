<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [Tutorial: RDF Basics for Code Generation](#tutorial-rdf-basics-for-code-generation)
  - [What is RDF?](#what-is-rdf)
    - [Example: Describing a Book](#example-describing-a-book)
  - [Why RDF for Code Generation?](#why-rdf-for-code-generation)
    - [The Problem with JSON/YAML](#the-problem-with-jsonyaml)
    - [RDF Solution](#rdf-solution)
  - [RDF Syntax: Turtle Format](#rdf-syntax-turtle-format)
    - [1. Prefixes (Shortcuts)](#1-prefixes-shortcuts)
    - [2. Simple Triples](#2-simple-triples)
    - [3. Shorthand (Same Subject)](#3-shorthand-same-subject)
    - [4. Types (Classes)](#4-types-classes)
    - [5. Blank Nodes (Nested Objects)](#5-blank-nodes-nested-objects)
    - [6. Lists](#6-lists)
  - [Hands-On: Model a Blog](#hands-on-model-a-blog)
    - [Define Classes](#define-classes)
    - [Define Properties](#define-properties)
    - [Add Data](#add-data)
    - [Complete Example](#complete-example)
  - [Querying with SPARQL](#querying-with-sparql)
    - [Example: Find all post titles](#example-find-all-post-titles)
    - [Example: Find posts with author names](#example-find-posts-with-author-names)
  - [RDF for Code Generation: Pattern](#rdf-for-code-generation-pattern)
    - [1. Define Schema (Vocabulary)](#1-define-schema-vocabulary)
    - [2. Define Code Generation Directives](#2-define-code-generation-directives)
    - [3. Query with SPARQL](#3-query-with-sparql)
    - [4. Template with Tera](#4-template-with-tera)
    - [5. Result](#5-result)
  - [Common RDF Patterns for Code Generation](#common-rdf-patterns-for-code-generation)
    - [Pattern 1: Hierarchical Data](#pattern-1-hierarchical-data)
    - [Pattern 2: Ordered Lists](#pattern-2-ordered-lists)
    - [Pattern 3: Attributes](#pattern-3-attributes)
    - [Pattern 4: Relationships](#pattern-4-relationships)
  - [RDF vs. JSON: When to Use Each](#rdf-vs-json-when-to-use-each)
  - [RDF Formats](#rdf-formats)
    - [Turtle (.ttl) - Recommended](#turtle-ttl---recommended)
    - [JSON-LD (.jsonld)](#json-ld-jsonld)
    - [RDF/XML (.rdf)](#rdfxml-rdf)
  - [Practice Exercise](#practice-exercise)
  - [Key Takeaways](#key-takeaways)
  - [Next Steps](#next-steps)
  - [Resources](#resources)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Tutorial: RDF Basics for Code Generation

**Time**: 15 minutes
**Difficulty**: Beginner
**Prerequisites**: None (start here if you're new to RDF)

## What is RDF?

**RDF (Resource Description Framework)** is a way to model information using **triples**:

```
Subject → Predicate → Object
```

Think of it like English sentences:
- **Subject**: "What we're talking about"
- **Predicate**: "Property or relationship"
- **Object**: "Value or related thing"

### Example: Describing a Book

English sentence:
> "The book has the title 'Introduction to RDF'"

RDF triple:
```turtle
:MyBook :hasTitle "Introduction to RDF" .
```

- Subject: `:MyBook`
- Predicate: `:hasTitle`
- Object: `"Introduction to RDF"`

---

## Why RDF for Code Generation?

### The Problem with JSON/YAML

Consider this JSON:

```json
{
  "user": {
    "email": "alice@example.com"
  }
}
```

**Questions:**
- Is `email` required?
- Is it validated?
- Is it unique?
- What format is expected?

**Answer**: 🤷 You have to read documentation (which might be outdated).

### RDF Solution

```turtle
:User a rdfs:Class ;
  :hasField [
    :name "email" ;
    :type xsd:string ;
    :required true ;
    :unique true ;
    :pattern "^[^@]+@[^@]+\\.[^@]+$"
  ] .
```

**Questions:**
- Is `email` required? **Yes** (`:required true`)
- Is it validated? **Yes** (`:pattern` regex)
- Is it unique? **Yes** (`:unique true`)
- What format? **String** (`:type xsd:string`)

**The difference**: RDF makes semantics **explicit and queryable**.

---

## RDF Syntax: Turtle Format

**Turtle** is the most human-friendly RDF format. Let's learn it through examples.

### 1. Prefixes (Shortcuts)

```turtle
@prefix : <http://example.com/myapp#> .
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .
```

**Why?** Instead of writing full URLs every time, use prefixes:
- `:User` expands to `<http://example.com/myapp#User>`
- `rdf:type` expands to `<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>`

### 2. Simple Triples

```turtle
:Alice :hasEmail "alice@example.com" .
:Alice :hasAge 30 .
:Alice :hasFriend :Bob .
```

Three triples, all with subject `:Alice`.

### 3. Shorthand (Same Subject)

```turtle
:Alice
  :hasEmail "alice@example.com" ;
  :hasAge 30 ;
  :hasFriend :Bob .
```

**Meaning**: Same as above, but more readable. `;` means "same subject, new predicate".

### 4. Types (Classes)

```turtle
:Alice a :User .
# Equivalent to:
:Alice rdf:type :User .
```

**Meaning**: "Alice is a User" (`a` is shorthand for `rdf:type`)

### 5. Blank Nodes (Nested Objects)

```turtle
:User :hasField [
  :name "email" ;
  :type xsd:string ;
  :required true
] .
```

**Meaning**: The object is an anonymous node (like a JSON object without a name).

### 6. Lists

```turtle
:User :fields (
  "id"
  "email"
  "name"
) .
```

**Meaning**: An ordered list of values.

---

## Hands-On: Model a Blog

Let's model a blog with posts and authors.

### Define Classes

```turtle
@prefix blog: <http://example.com/blog#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .

blog:Post a rdfs:Class ;
  rdfs:label "Blog Post" ;
  rdfs:comment "A blog post with title, content, author" .

blog:Author a rdfs:Class ;
  rdfs:label "Author" ;
  rdfs:comment "A blog author" .
```

### Define Properties

```turtle
blog:title a rdf:Property ;
  rdfs:label "title" ;
  rdfs:domain blog:Post ;
  rdfs:range xsd:string .

blog:content a rdf:Property ;
  rdfs:label "content" ;
  rdfs:domain blog:Post ;
  rdfs:range xsd:string .

blog:author a rdf:Property ;
  rdfs:label "author" ;
  rdfs:domain blog:Post ;
  rdfs:range blog:Author .

blog:name a rdf:Property ;
  rdfs:label "name" ;
  rdfs:domain blog:Author ;
  rdfs:range xsd:string .
```

**Key points:**
- `rdfs:domain`: What class this property belongs to
- `rdfs:range`: What type of value it has

### Add Data

```turtle
blog:Post1 a blog:Post ;
  blog:title "Introduction to RDF" ;
  blog:content "RDF is a powerful way to model knowledge..." ;
  blog:author blog:Alice .

blog:Alice a blog:Author ;
  blog:name "Alice Smith" .
```

### Complete Example

Put it all together:

```turtle
@prefix blog: <http://example.com/blog#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

# Classes
blog:Post a rdfs:Class ;
  rdfs:label "Blog Post" .

blog:Author a rdfs:Class ;
  rdfs:label "Author" .

# Properties
blog:title a rdf:Property ;
  rdfs:domain blog:Post ;
  rdfs:range xsd:string .

blog:author a rdf:Property ;
  rdfs:domain blog:Post ;
  rdfs:range blog:Author .

blog:name a rdf:Property ;
  rdfs:domain blog:Author ;
  rdfs:range xsd:string .

# Data
blog:Post1 a blog:Post ;
  blog:title "Introduction to RDF" ;
  blog:author blog:Alice .

blog:Alice a blog:Author ;
  blog:name "Alice Smith" .
```

---

## Querying with SPARQL

Now that we have RDF data, we can **query** it.

### Example: Find all post titles

```sparql
PREFIX blog: <http://example.com/blog#>

SELECT ?title
WHERE {
  ?post a blog:Post ;
        blog:title ?title .
}
```

**Result:**
```
title
---------------------
"Introduction to RDF"
```

### Example: Find posts with author names

```sparql
PREFIX blog: <http://example.com/blog#>

SELECT ?postTitle ?authorName
WHERE {
  ?post a blog:Post ;
        blog:title ?postTitle ;
        blog:author ?author .
  ?author blog:name ?authorName .
}
```

**Result:**
```
postTitle             authorName
----------------------------------------
"Introduction to RDF" "Alice Smith"
```

**Why this matters**: SPARQL lets ggen **extract structured data** from ontologies to pass to templates.

---

## RDF for Code Generation: Pattern

Here's how ggen uses RDF:

### 1. Define Schema (Vocabulary)

```turtle
:Model a rdfs:Class .
:hasField a rdf:Property .
```

### 2. Define Code Generation Directives

```turtle
:User a :Model ;
  :codegen-as "struct" ;
  :codegen-language "rust" ;
  :hasField [
    :name "id" ;
    :type "Uuid"
  ] .
```

**Key**: `:codegen-as` and `:codegen-language` are **your custom properties** that tell ggen what to generate.

### 3. Query with SPARQL

```sparql
SELECT ?className ?fieldName ?fieldType
WHERE {
  ?class :codegen-as "struct" ;
         :hasField ?field .
  ?field :name ?fieldName ;
         :type ?fieldType .
}
```

### 4. Template with Tera

```rust
pub struct {% raw %}{{ className }}{% endraw %} {
  {% raw %}{% for field in fields %}{% endraw %}
  pub {% raw %}{{ field.name }}{% endraw %}: {% raw %}{{ field.type }}{% endraw %},
  {% raw %}{% endfor %}{% endraw %}
}
```

### 5. Result

```rust
pub struct User {
  pub id: Uuid,
}
```

---

## Common RDF Patterns for Code Generation

### Pattern 1: Hierarchical Data

```turtle
:Chapter1 a :Chapter ;
  :hasSection :Section1, :Section2 .

:Section1 a :Section ;
  :title "Introduction" .
```

**Use case**: Generate nested structures (documents, state machines)

### Pattern 2: Ordered Lists

```turtle
:Book :chapters (
  :Chapter1
  :Chapter2
  :Chapter3
) .
```

**Use case**: Preserve order (chapter sequence, argument lists)

### Pattern 3: Attributes

```turtle
:Field a :StructField ;
  :name "email" ;
  :type "String" ;
  :required true ;
  :default "\"\"" .
```

**Use case**: Rich metadata for code generation (validation, defaults, constraints)

### Pattern 4: Relationships

```turtle
:User :hasFriend :OtherUser .
:Post :author :User .
```

**Use case**: Generate foreign keys, references, graph traversal

---

## RDF vs. JSON: When to Use Each

| Use Case | JSON | RDF |
|----------|------|-----|
| **Configuration files** | ✅ Simple, readable | ❌ Overkill |
| **API responses** | ✅ Standard format | ❌ Too verbose |
| **Data exchange** | ✅ Universal | ⚠️ If semantics matter |
| **Code generation** | ❌ Ambiguous | ✅ Precise |
| **Knowledge graphs** | ❌ No semantics | ✅ Built for it |
| **Validation** | ⚠️ JSON Schema | ✅ SHACL + reasoning |
| **Querying** | ❌ JSONPath limited | ✅ SPARQL powerful |

**Rule of thumb**: Use RDF when you need:
- Semantic precision
- Complex querying
- Relationships between entities
- Code generation from models

---

## RDF Formats

RDF has multiple serialization formats:

### Turtle (.ttl) - Recommended

```turtle
:User a rdfs:Class ;
  :hasField [ :name "email" ] .
```

**Pro**: Human-readable, concise
**Con**: Not valid JSON

### JSON-LD (.jsonld)

```json
{
  "@context": "http://example.com/context",
  "@type": "Class",
  "hasField": {
    "name": "email"
  }
}
```

**Pro**: Valid JSON (works with existing tools)
**Con**: More verbose

### RDF/XML (.rdf)

```xml
<rdf:RDF xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#">
  <rdfs:Class rdf:about="http://example.com/User">
    <hasField>
      <rdf:Description>
        <name>email</name>
      </rdf:Description>
    </hasField>
  </rdfs:Class>
</rdf:RDF>
```

**Pro**: XML tooling support
**Con**: Very verbose, hard to read

**ggen supports all formats.** Use Turtle for readability.

---

## Practice Exercise

Create an RDF ontology for a library system:

**Requirements:**
- Books have title, author, ISBN
- Authors have name, birth year
- Books can have multiple authors
- Authors can write multiple books

**Solution:**

```turtle
@prefix lib: <http://example.com/library#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

# Classes
lib:Book a rdfs:Class ;
  rdfs:label "Book" .

lib:Author a rdfs:Class ;
  rdfs:label "Author" .

# Properties
lib:title a rdf:Property ;
  rdfs:domain lib:Book ;
  rdfs:range xsd:string .

lib:isbn a rdf:Property ;
  rdfs:domain lib:Book ;
  rdfs:range xsd:string .

lib:author a rdf:Property ;
  rdfs:domain lib:Book ;
  rdfs:range lib:Author .

lib:name a rdf:Property ;
  rdfs:domain lib:Author ;
  rdfs:range xsd:string .

lib:birthYear a rdf:Property ;
  rdfs:domain lib:Author ;
  rdfs:range xsd:integer .

# Data
lib:Book1 a lib:Book ;
  lib:title "The Pragmatic Programmer" ;
  lib:isbn "978-0135957059" ;
  lib:author lib:Hunt, lib:Thomas .

lib:Hunt a lib:Author ;
  lib:name "Andrew Hunt" ;
  lib:birthYear 1964 .

lib:Thomas a lib:Author ;
  lib:name "David Thomas" ;
  lib:birthYear 1956 .
```

---

## Key Takeaways

✅ **RDF = Triples** (Subject → Predicate → Object)
✅ **Turtle syntax** is human-friendly
✅ **Classes and Properties** define vocabulary
✅ **SPARQL queries** extract structured data
✅ **Semantics are explicit** (no ambiguity)
✅ **Perfect for code generation** (precise, queryable)

---

## Next Steps

**Ready to generate code?**
→ [Getting Started](../getting-started/README.md) - Generate your first document (15 minutes)

**Want a real example?**
→ [Thesis Generation Tutorial](02-thesis-generation.md) - Generate a 43-page PhD thesis (30 minutes)

**Need to learn SPARQL?**
→ [SPARQL Patterns Reference](../reference/sparql-patterns.md) - Query patterns for code generation

---

## Resources

**Learn More About RDF:**
- [W3C RDF Primer](https://www.w3.org/TR/rdf11-primer/) - Official specification
- [Turtle Syntax Spec](https://www.w3.org/TR/turtle/) - Turtle format details
- [SPARQL Tutorial](https://www.w3.org/TR/sparql11-query/) - Query language

**Practice:**
- Try the exercise above
- Model your domain in RDF
- Write SPARQL queries against your ontology

---

**Time investment**: 15 minutes
**Skills gained**: RDF fundamentals, Turtle syntax, SPARQL basics
**Next**: [Getting Started with ggen](../getting-started/README.md) →
