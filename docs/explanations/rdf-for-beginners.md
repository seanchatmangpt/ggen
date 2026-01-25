<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [RDF for Beginners: Understanding Semantic Data](#rdf-for-beginners-understanding-semantic-data)
  - [You Don't Need to Know This, But...](#you-dont-need-to-know-this-but)
  - [Table of Contents](#table-of-contents)
  - [What is RDF?](#what-is-rdf)
  - [Why Does This Matter?](#why-does-this-matter)
    - [Example: A Person Named Alice](#example-a-person-named-alice)
    - [Why ggen Cares](#why-ggen-cares)
  - [Core Concepts](#core-concepts)
    - [1. Triple (Subject-Predicate-Object)](#1-triple-subject-predicate-object)
    - [2. Resources (Things)](#2-resources-things)
    - [3. Properties (Relationships)](#3-properties-relationships)
    - [4. Literals (Values)](#4-literals-values)
    - [5. Classes (Types)](#5-classes-types)
  - [Analogies](#analogies)
    - [Analogy 1: Spreadsheet Rows](#analogy-1-spreadsheet-rows)
    - [Analogy 2: Social Network Graph](#analogy-2-social-network-graph)
    - [Analogy 3: Dictionary Definitions](#analogy-3-dictionary-definitions)
  - [Your First RDF](#your-first-rdf)
    - [Format: Turtle (Easiest to Read)](#format-turtle-easiest-to-read)
    - [Even Simpler: Just the Essentials](#even-simpler-just-the-essentials)
  - [Common RDF Formats](#common-rdf-formats)
    - [Format 1: Turtle (RECOMMENDED - Most Readable)](#format-1-turtle-recommended---most-readable)
    - [Format 2: RDF/XML (Machine-Readable, Hard to Read)](#format-2-rdfxml-machine-readable-hard-to-read)
    - [Format 3: JSON-LD (JSON with RDF)](#format-3-json-ld-json-with-rdf)
    - [Format 4: N-Triples (Explicit Triples, Very Verbose)](#format-4-n-triples-explicit-triples-very-verbose)
  - [Using RDF with ggen](#using-rdf-with-ggen)
    - [Step 1: Write Your Domain in RDF](#step-1-write-your-domain-in-rdf)
    - [Step 2: Use with ggen](#step-2-use-with-ggen)
    - [Step 3: Regenerate Anytime](#step-3-regenerate-anytime)
  - [SPARQL Basics](#sparql-basics)
    - [What is SPARQL?](#what-is-sparql)
    - [Simple SPARQL Query](#simple-sparql-query)
    - [Common Query Patterns](#common-query-patterns)
    - [Do You Need to Write SPARQL?](#do-you-need-to-write-sparql)
  - [When NOT to Use RDF](#when-not-to-use-rdf)
  - [Summary](#summary)
    - [Key Takeaways](#key-takeaways)
    - [Mental Model](#mental-model)
  - [Next Steps](#next-steps)
  - [Learn More](#learn-more)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# RDF for Beginners: Understanding Semantic Data

A beginner-friendly guide to RDF (Resource Description Framework) and why ggen uses it.

## You Don't Need to Know This, But...

Many ggen tutorials mention "RDF ontologies." This document explains what that means without requiring you to understand semantic web theory.

**Time to read:** 15 minutes
**Background needed:** Basic programming knowledge (no semantic web experience required)

---

## Table of Contents

1. [What is RDF?](#what-is-rdf)
2. [Why Does This Matter?](#why-does-this-matter)
3. [Core Concepts](#core-concepts)
4. [Analogies](#analogies)
5. [Your First RDF](#your-first-rdf)
6. [Common RDF Formats](#common-rdf-formats)
7. [Using RDF with ggen](#using-rdf-with-ggen)
8. [SPARQL Basics](#sparql-basics)
9. [When NOT to Use RDF](#when-not-to-use-rdf)

---

## What is RDF?

**Short answer:** RDF is a way to describe data that's specifically designed for describing relationships.

**Medium answer:** RDF stands for "Resource Description Framework." It's a standardized way to represent information that:
- Works across different programming languages
- Makes relationships between things explicit
- Can be combined with other data sources
- Enables powerful queries

**Technical answer:** RDF represents data as triples (Subject-Predicate-Object statements) in a directed graph where nodes are resources and edges are properties.

---

## Why Does This Matter?

### Example: A Person Named Alice

**Traditional approach (JSON):**
```json
{
  "name": "Alice",
  "email": "alice@example.com",
  "age": 30,
  "company": "Acme Corp",
  "manager": "Bob"
}
```

**Problem:** How does the system know that "Acme Corp" is a company? Is "Bob" a person or a string? What does "manager" mean?

**RDF approach:**
```
Alice --is-a--> Person
Alice --name--> "Alice"
Alice --email--> "alice@example.com"
Alice --age--> 30
Alice --works-for--> [Company]
  [Company] --name--> "Acme Corp"
Alice --has-manager--> Bob
  Bob --is-a--> Person
```

**Advantage:** The system KNOWS that Alice is a Person, that Acme Corp is a Company, that "has-manager" connects to another Person.

### Why ggen Cares

ggen generates code in multiple languages from a single definition. RDF lets ggen:
- **Explicitly describe your domain** (what's a User? What's a Post?)
- **Generate type-safe code** (TypeScript interfaces, Rust structs, Python dataclasses)
- **Generate in many languages** (one definition → Rust + TypeScript + Python + Go)
- **Guarantee consistency** (no drift between versions)

---

## Core Concepts

### 1. Triple (Subject-Predicate-Object)

RDF data consists of **triples**: simple statements with three parts.

```
Alice --knows--> Bob
```

Breaking it down:
- **Subject:** Alice (the "thing" we're describing)
- **Predicate:** knows (the relationship)
- **Object:** Bob (what it relates to)

More examples:
```
Alice --name--> "Alice"              (object is a string)
Alice --age--> 30                     (object is a number)
Alice --works-for--> Acme             (object is another thing)
```

### 2. Resources (Things)

A **resource** is anything you can describe: people, companies, concepts, etc.

Resources are identified by URIs (like URLs):
```
http://example.com/people/alice
http://example.com/companies/acme
http://example.com/concepts/Manager
```

(Don't worry - you don't have to use the full URL every time. You can use shortcuts like `alice:` or `ex:Alice`)

### 3. Properties (Relationships)

A **property** is a named relationship:
```
name
email
age
works-for
has-manager
knows
```

Again, properties are identified by URIs, but you use shortcuts:
```
foaf:name              (from Friend-of-a-Friend vocabulary)
foaf:email
foaf:knows
ex:worksFor           (custom property)
```

### 4. Literals (Values)

**Literals** are actual values (strings, numbers, dates):
```
"Alice"               (string)
"alice@example.com"   (email)
30                    (integer)
"2024-11-17"          (date)
true                  (boolean)
```

### 5. Classes (Types)

A **class** groups similar resources:
```
Person              (class)
Company             (class)
Post                (class)
User                (class)
```

Resources **belong to** classes:
```
alice --is-a--> Person
acme --is-a--> Company
```

---

## Analogies

### Analogy 1: Spreadsheet Rows

Think of triples like spreadsheet rows:

| Subject | Predicate | Object |
|---------|-----------|--------|
| Alice | name | Alice |
| Alice | email | alice@example.com |
| Alice | works-for | Acme |
| Bob | name | Bob |
| Bob | works-for | Acme |
| Acme | name | Acme Corp |

But unlike a spreadsheet:
- You can add columns (properties) dynamically
- Resources can appear in multiple rows
- You can query across "sheets" (different data sources)

### Analogy 2: Social Network Graph

Think of RDF like a social network:

```
          ┌─── knows ───┐
          ▼             ▼
[Alice] ◄─ works-for ──► [Acme] ──manages── [Bob]
   │
   name: "Alice"
   email: "alice@example.com"
   │
   has-manager ──► [Bob]
```

Resources are nodes, properties are edges.

### Analogy 3: Dictionary Definitions

Traditional database (fixed schema):
```sql
CREATE TABLE users (
  id INTEGER,
  name VARCHAR(255),
  email VARCHAR(255),
  age INTEGER
);
```

RDF approach (flexible, extensible):
```
A User is something that:
- has a name (string)
- has an email (string)
- has an age (number)
- CAN ALSO have: phone, company, manager, location, etc.

All these facts are separate statements about the User.
```

---

## Your First RDF

### Format: Turtle (Easiest to Read)

Turtle is the most readable RDF format. Here's a simple example:

```turtle
@prefix ex: <http://example.com/> .
@prefix foaf: <http://xmlns.com/foaf/0.1/> .

# Resources (things we're describing)
ex:alice a foaf:Person ;
  foaf:name "Alice" ;
  foaf:email "alice@example.com" ;
  foaf:age 30 ;
  ex:worksFor ex:acme ;
  ex:hasManager ex:bob .

ex:bob a foaf:Person ;
  foaf:name "Bob" ;
  foaf:email "bob@example.com" .

ex:acme a ex:Company ;
  foaf:name "Acme Corp" ;
  ex:hasEmployee ex:alice ;
  ex:hasEmployee ex:bob .
```

**Breaking it down:**

```turtle
@prefix ex: <http://example.com/> .
```
Shortcut definition: `ex:` means `http://example.com/`

```turtle
ex:alice a foaf:Person ;
```
Alice is-a (`a`) Person

```turtle
  foaf:name "Alice" ;
  foaf:email "alice@example.com" ;
```
Properties of Alice (semicolon `;` means "same subject, different property")

```turtle
  ex:worksFor ex:acme ;
```
Alice works-for Acme (the object is another resource)

```turtle
  ex:hasManager ex:bob .
```
Alice has-manager Bob (period `.` ends this resource's description)

### Even Simpler: Just the Essentials

You don't need the full Turtle syntax. Here's the minimum:

```turtle
@prefix ex: <http://example.com/> .
@prefix foaf: <http://xmlns.com/foaf/0.1/> .

ex:Alice
  a foaf:Person ;
  foaf:name "Alice" ;
  foaf:email "alice@example.com" .

ex:Bob
  a foaf:Person ;
  foaf:name "Bob" .
```

That's it. You now have:
- ✅ Two people (Alice and Bob)
- ✅ Their names and emails
- ✅ The system knows they're People

---

## Common RDF Formats

RDF can be written in multiple formats. All mean the same thing:

### Format 1: Turtle (RECOMMENDED - Most Readable)

```turtle
@prefix ex: <http://example.com/> .

ex:Alice a foaf:Person ;
  foaf:name "Alice" .
```

### Format 2: RDF/XML (Machine-Readable, Hard to Read)

```xml
<?xml version="1.0"?>
<rdf:RDF xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#"
         xmlns:ex="http://example.com/">
  <rdf:Description rdf:about="http://example.com/Alice">
    <rdf:type rdf:resource="http://xmlns.com/foaf/0.1/Person"/>
    <foaf:name>Alice</foaf:name>
  </rdf:Description>
</rdf:RDF>
```

### Format 3: JSON-LD (JSON with RDF)

```json
{
  "@context": {
    "ex": "http://example.com/",
    "foaf": "http://xmlns.com/foaf/0.1/"
  },
  "@id": "ex:Alice",
  "@type": "foaf:Person",
  "foaf:name": "Alice"
}
```

### Format 4: N-Triples (Explicit Triples, Very Verbose)

```
<http://example.com/Alice> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://xmlns.com/foaf/0.1/Person> .
<http://example.com/Alice> <http://xmlns.com/foaf/0.1/name> "Alice" .
```

**For ggen:** Use **Turtle** format. It's readable, standard, and easy to write by hand.

---

## Using RDF with ggen

### Step 1: Write Your Domain in RDF

Create `domain.ttl`:

```turtle
@prefix ex: <http://example.com/> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .

# Define the User class
ex:User
  rdfs:label "User" ;
  rdfs:comment "A person who uses the system" ;
  ex:hasProperty [
    rdfs:label "id" ;
    rdfs:comment "Unique identifier" ;
    ex:type xsd:integer
  ] ;
  ex:hasProperty [
    rdfs:label "email" ;
    rdfs:comment "Contact email" ;
    ex:type xsd:string
  ] ;
  ex:hasProperty [
    rdfs:label "name" ;
    rdfs:comment "Full name" ;
    ex:type xsd:string
  ] .

# Define the Post class
ex:Post
  rdfs:label "Post" ;
  ex:hasProperty [
    rdfs:label "id" ;
    ex:type xsd:integer
  ] ;
  ex:hasProperty [
    rdfs:label "title" ;
    ex:type xsd:string
  ] ;
  ex:hasProperty [
    rdfs:label "content" ;
    ex:type xsd:string
  ] ;
  ex:hasProperty [
    rdfs:label "author_id" ;
    ex:type xsd:integer
  ] .
```

### Step 2: Use with ggen

```bash
ggen generate \
  --template rest-api-template \
  --domain domain.ttl \
  --language rust \
  --output ./src
```

ggen will:
1. **Parse** your RDF
2. **Extract** the User and Post entities
3. **Generate** Rust code:
   ```rust
   pub struct User {
       pub id: i32,
       pub email: String,
       pub name: String,
   }

   pub struct Post {
       pub id: i32,
       pub title: String,
       pub content: String,
       pub author_id: i32,
   }
   ```

### Step 3: Regenerate Anytime

If you change `domain.ttl`:

```turtle
# Added a new field
ex:User
  ex:hasProperty [
    rdfs:label "phone" ;
    ex:type xsd:string
  ] .
```

Regenerate:
```bash
ggen generate --template rest-api-template --domain domain.ttl --language rust --output ./src
```

Your generated code now includes the `phone` field everywhere it should. **Zero drift.**

---

## SPARQL Basics

**SPARQL** is a query language for RDF data. You use it to ask questions about your domain.

### What is SPARQL?

SPARQL = "SPARQL Protocol and RDF Query Language" (the name is recursive 😄)

It's basically "SQL for RDF" - you write queries to extract information.

### Simple SPARQL Query

```sparql
PREFIX ex: <http://example.com/>
PREFIX foaf: <http://xmlns.com/foaf/0.1/>

SELECT ?name ?email
WHERE {
  ?person a foaf:Person ;
          foaf:name ?name ;
          foaf:email ?email .
}
```

**Translation:**
- `SELECT ?name ?email` - Show me names and emails
- `WHERE { ... }` - Find subjects where...
- `?person a foaf:Person` - The subject is a Person
- `foaf:name ?name` - It has a name (store in `?name`)
- `foaf:email ?email` - It has an email (store in `?email`)

**Result:**
```
name     email
"Alice"  "alice@example.com"
"Bob"    "bob@example.com"
```

### Common Query Patterns

**Pattern 1: Find all resources of a type**
```sparql
PREFIX foaf: <http://xmlns.com/foaf/0.1/>

SELECT ?person
WHERE {
  ?person a foaf:Person .
}
```

**Pattern 2: Find resources with specific property**
```sparql
PREFIX ex: <http://example.com/>

SELECT ?company ?name
WHERE {
  ?company a ex:Company ;
           foaf:name ?name .
}
```

**Pattern 3: Filter results**
```sparql
PREFIX ex: <http://example.com/>

SELECT ?name
WHERE {
  ?person a foaf:Person ;
          foaf:name ?name ;
          ex:age ?age .
  FILTER (?age > 25)
}
```

**Pattern 4: Count and aggregate**
```sparql
PREFIX foaf: <http://xmlns.com/foaf/0.1/>

SELECT (COUNT(?person) as ?count)
WHERE {
  ?person a foaf:Person .
}
```

### Do You Need to Write SPARQL?

**No.** For basic ggen usage:
- Write your domain in Turtle
- ggen handles the SPARQL internally
- You only use SPARQL if you need advanced queries

**Yes.** If you want to:
- Query your ontology to generate custom code
- Validate constraints
- Generate different code paths based on domain structure

For 80% of users, you never write SPARQL directly.

---

## When NOT to Use RDF

RDF is powerful, but not always necessary. Use RDF if:

✅ **USE RDF if you:**
- Want to generate code in multiple languages
- Need to explicitly describe relationships
- Want powerful semantic queries
- Need semantic validation
- Want to reuse your definition elsewhere
- Care about Linked Data compatibility

❌ **Don't use RDF if you:**
- Just want a quick script generator
- Have a simple CRUD app with 2-3 tables
- Only generate in one language
- Need to get started immediately without learning anything

**Simpler alternatives:**
- **JSON Schema** - For type definitions
- **YAML** - For simple configuration
- **SQL** - For database schemas

But if you're using ggen, RDF gives you 10x more power for slightly more learning.

---

## Summary

### Key Takeaways

1. **RDF is just statements (triples):** Subject-Predicate-Object
2. **Turtle is the readable format:** Use it for your ontologies
3. **ggen reads RDF and generates code:** One definition, many languages
4. **SPARQL is the query language:** You don't need it for basic usage
5. **The goal is consistency:** Define once, generate everywhere

### Mental Model

Think of RDF like this:

```
Your Domain (Business Concepts)
        ↓
RDF Ontology (Machine-Readable Description)
        ↓
ggen (Code Generator)
        ↓
Rust Code + TypeScript Code + Python Code + Go Code
```

---

## Next Steps

Now you understand RDF basics. You can:

1. **Read an RDF example:** Look at the `domain.ttl` in any ggen project
2. **Write your first ontology:** Define your domain in Turtle
3. **Generate code:** Use ggen to create type-safe implementations
4. **Learn SPARQL:** If you need custom queries (optional)

---

## Learn More

- **W3C RDF Primer:** https://www.w3.org/TR/rdf-primer/
- **Turtle Format Spec:** https://www.w3.org/TR/turtle/
- **SPARQL Tutorial:** https://www.wikidata.org/wiki/Wikidata:SPARQL_tutorial
- **ggen RDF Documentation:** [../reference/rdf-sparql.md](../reference/rdf-sparql.md)

---

**Questions?** This is a beginner's guide. If something isn't clear, [file an issue](https://github.com/seanchatmangpt/ggen/issues) and we'll improve the explanation.

---

**Ready to use RDF with ggen?** Next, try:
- **[Getting Started Tutorial](../tutorials/getting-started.md)** - Install and first generation
- **[Ontology to Code Tutorial](../tutorials/ontology-to-code.md)** - Full workflow with RDF
- **[RDF/SPARQL Reference](../reference/rdf-sparql.md)** - Complete technical details
