<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [SPARQL Design Philosophy: Graph Queries for Ontologies](#sparql-design-philosophy-graph-queries-for-ontologies)
  - [Traditional Query Thinking vs Graph Thinking](#traditional-query-thinking-vs-graph-thinking)
    - [Traditional (SQL) Thinking](#traditional-sql-thinking)
    - [Graph Thinking (SPARQL)](#graph-thinking-sparql)
  - [The Graph Mental Model](#the-graph-mental-model)
    - [Visualizing SPARQL Queries](#visualizing-sparql-queries)
  - [Query Patterns](#query-patterns)
    - [Pattern 1: Simple Path Following](#pattern-1-simple-path-following)
    - [Pattern 2: Multi-Hop Paths](#pattern-2-multi-hop-paths)
    - [Pattern 3: Finding Relationships](#pattern-3-finding-relationships)
    - [Pattern 4: Optional Relationships](#pattern-4-optional-relationships)
    - [Pattern 5: Counting and Aggregates](#pattern-5-counting-and-aggregates)
  - [Performance Thinking](#performance-thinking)
    - [✅ Queries are Well-Constrained](#-queries-are-well-constrained)
    - [✅ Queries Follow Natural Paths](#-queries-follow-natural-paths)
    - [✅ Queries Use Indices Effectively](#-queries-use-indices-effectively)
  - [Design Philosophy Principles](#design-philosophy-principles)
    - [Principle 1: Declarative, Not Imperative](#principle-1-declarative-not-imperative)
    - [Principle 2: Graph Patterns Are Your Vocabulary](#principle-2-graph-patterns-are-your-vocabulary)
    - [Principle 3: Filters Refine, Don't Define](#principle-3-filters-refine-dont-define)
    - [Principle 4: Common Relationships Are Powerful](#principle-4-common-relationships-are-powerful)
  - [Common Query Challenges](#common-query-challenges)
    - [Challenge 1: Backwards Relationships](#challenge-1-backwards-relationships)
    - [Challenge 2: Multiple Paths](#challenge-2-multiple-paths)
    - [Challenge 3: Recursive Relationships](#challenge-3-recursive-relationships)
  - [Design Best Practices](#design-best-practices)
    - [1. Use Semantic Naming](#1-use-semantic-naming)
    - [2. Model Entities Explicitly](#2-model-entities-explicitly)
    - [3. Index for Query Performance](#3-index-for-query-performance)
    - [4. Normalize Your Graph](#4-normalize-your-graph)
  - [Summary](#summary)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# SPARQL Design Philosophy: Graph Queries for Ontologies

Understanding how to think about queries using SPARQL.

## Traditional Query Thinking vs Graph Thinking

### Traditional (SQL) Thinking

```sql
SELECT u.name, p.price
FROM users u
JOIN purchases pu ON u.id = pu.user_id
JOIN products p ON pu.product_id = p.id
WHERE p.price > 100
```

**Mental Model**: Tables with joins
- Fixed schema
- Each table is a relation
- Join tables to combine data

### Graph Thinking (SPARQL)

```sparql
SELECT ?name ?price WHERE {
  ?user name ?name .
  ?user purchases ?product .
  ?product price ?price .
  FILTER (?price > 100)
}
```

**Mental Model**: Knowledge graph
- Nodes (entities) and edges (relationships)
- Any entity can relate to any other
- Follow paths through the graph

## The Graph Mental Model

### Visualizing SPARQL Queries

**Your RDF data** (knowledge graph):
```
Alice --name--> "Alice"
Alice --purchases--> iPad
iPad --price--> 1200
iPad --brand--> Apple

Bob --name--> "Bob"
Bob --purchases--> Book
Book --price--> 25
Book --author--> "Author Name"
```

**SPARQL Query**:
```sparql
SELECT ?name ?price WHERE {
  ?user name ?name .
  ?user purchases ?product .
  ?product price ?price .
  FILTER (?price > 100)
}
```

**Execution Trace**:
1. Find all `?user` who have a `name` relationship
2. Check which of those `?user` have `purchases` relationships
3. Get the `?product` they purchased
4. Get the `?price` of those products
5. Filter for prices > 100

**Results**:
```
?name    | ?price
---------|-------
Alice    | 1200
```

## Query Patterns

### Pattern 1: Simple Path Following

"Find all User names"

```sparql
SELECT ?name WHERE {
  ?user rdf:type User .
  ?user name ?name .
}
```

**Graph traversal**:
```
[User] --name--> ?name
```

### Pattern 2: Multi-Hop Paths

"Find brands of products purchased by users named Alice"

```sparql
SELECT ?brand WHERE {
  ?user name "Alice" .
  ?user purchases ?product .
  ?product brand ?brand .
}
```

**Graph traversal**:
```
[name="Alice"] --purchases--> [Product] --brand--> ?brand
```

### Pattern 3: Finding Relationships

"Find all users who purchased products over $100"

```sparql
SELECT ?user WHERE {
  ?user rdf:type User .
  ?user purchases ?product .
  ?product price ?price .
  FILTER (?price > 100)
}
```

**Graph traversal**:
```
[User] --purchases--> [Product price > 100]
```

### Pattern 4: Optional Relationships

"Find users and their phone numbers (if they have one)"

```sparql
SELECT ?user ?phone WHERE {
  ?user rdf:type User .
  OPTIONAL { ?user phone ?phone }
}
```

**Graph traversal**:
```
[User] --phone--> ?phone (optional edge)
```

### Pattern 5: Counting and Aggregates

"Count products per user"

```sparql
SELECT ?user (COUNT(?product) as ?count) WHERE {
  ?user purchases ?product .
}
GROUP BY ?user
```

**Graph traversal**:
```
[User] --purchases--> [Product] (count them)
```

## Performance Thinking

SPARQL is efficient when:

### ✅ Queries are Well-Constrained

```sparql
# ✅ Fast: Specific starting point
SELECT ?product WHERE {
  ?user name "Alice" .      # Start: find Alice
  ?user purchases ?product .  # Then: get her purchases
}

# ❌ Slow: Broad starting point
SELECT ?product WHERE {
  ?user purchases ?product .  # Start: all purchases
  FILTER (/* complex logic */)
}
```

### ✅ Queries Follow Natural Paths

```sparql
# ✅ Natural path through graph
SELECT ?brand WHERE {
  ?user rdf:type User .
  ?user purchases ?product .
  ?product brand ?brand .
}

# ❌ Awkward jumping around
SELECT ?brand WHERE {
  ?product brand ?brand .
  ?product price ?price .
  ?user purchases ?product .
  ?user name ?name .
  FILTER (/* lots of conditions */)
}
```

### ✅ Queries Use Indices Effectively

```sparql
# ✅ Indexed properties
SELECT ?user WHERE {
  ?user rdf:type User .        # Type index
  ?user email "alice@example.com" .  # Email index
}

# ❌ Inefficient property scan
SELECT ?user WHERE {
  ?user description ?desc .    # Unindexed text search
  FILTER (CONTAINS(?desc, "admin"))
}
```

## Design Philosophy Principles

### Principle 1: Declarative, Not Imperative

**DON'T**: Write procedures
```
WRONG: "First find all users, then for each user..."
```

**DO**: Describe what you're looking for
```sparql
# Right way: Describe the pattern
SELECT ?user ?product WHERE {
  ?user purchases ?product .
  ?product price ?price .
  FILTER (?price > 1000)
}
```

### Principle 2: Graph Patterns Are Your Vocabulary

Think in terms of **graph shapes**:

```
Single pattern:    ?user --name--> ?name
Double hop:        ?user --purchases--> ?product --price--> ?price
Fork:              ?user --name--> ?name
                   ?user --email--> ?email
Loop (recursive):  ?org --subsidiary--> ?child
                   ?child --subsidiary--> ?grandchild
```

### Principle 3: Filters Refine, Don't Define

**DON'T**: Use FILTER for primary selection
```sparql
# ❌ Inefficient
SELECT ?user WHERE {
  ?user rdf:type ?type .
  FILTER (?type = User)
}
```

**DO**: Use graph patterns for selection
```sparql
# ✅ Efficient
SELECT ?user WHERE {
  ?user rdf:type User .
}
```

**Filters are for refinement**:
```sparql
# ✅ Good filter usage
SELECT ?user WHERE {
  ?user rdf:type User .
  ?user age ?age .
  FILTER (?age > 18)  # Refine results
}
```

### Principle 4: Common Relationships Are Powerful

Instead of encoding in types, use relationships:

**DON'T**: Subclass everything
```turtle
# ❌ Proliferates types
app:PremiumUser rdfs:subClassOf app:User .
app:AdminUser rdfs:subClassOf app:User .
app:ModeratorUser rdfs:subClassOf app:User .
```

**DO**: Use relationships and roles
```turtle
# ✅ Flexible
app:user membership app:Premium .
app:user role app:Admin .
app:user permission app:Moderate .
```

Query any combination:
```sparql
# Users with Premium membership AND Admin role
SELECT ?user WHERE {
  ?user membership app:Premium .
  ?user role app:Admin .
}
```

## Common Query Challenges

### Challenge 1: Backwards Relationships

**Problem**: Need to follow edges in reverse

```sparql
# Find products purchased by Alice
SELECT ?product WHERE {
  ?user name "Alice" .
  ?user purchases ?product .
}

# Find users who purchased a specific product (reverse)
SELECT ?user WHERE {
  ?product name "iPad" .
  ?user purchases ?product .
  # SPARQL naturally handles both directions!
}
```

### Challenge 2: Multiple Paths

**Problem**: Need to match multiple relationship types

```sparql
# Find entities Alice owns or manages
SELECT ?entity WHERE {
  ?user name "Alice" .
  (
    ?user owns ?entity | ?user manages ?entity
  ) .
}
```

### Challenge 3: Recursive Relationships

**Problem**: Follow relationships of unknown depth

```sparql
# Find all transitive children of an organization
SELECT ?child WHERE {
  ?org name "Acme Inc" .
  ?org subsidiary* ?child .  # * = one or more
}
```

## Design Best Practices

### 1. Use Semantic Naming

```turtle
# ✅ Clear semantic meaning
app:user email ?email .
app:product category ?category .

# ❌ Opaque
app:p1 r1 ?v1 .
app:p2 r2 ?v2 .
```

### 2. Model Entities Explicitly

```turtle
# ✅ Clear entity types
?user rdf:type User .
?product rdf:type Product .

# ❌ Implicit
?x name "Alice" .  # Is this a person?
```

### 3. Index for Query Performance

```turtle
# ✅ Index frequently queried properties
app:user rdfs:comment "indexed for lookups" ;
  rdf:type User .
app:email rdf:type IndexedProperty .

# ❌ Unindexed text search on large graph
app:description ?text .
```

### 4. Normalize Your Graph

```turtle
# ✅ Normalize: User once, referenced by email
app:user1 rdf:type User ;
  email "alice@example.com" .
app:user2 rdf:type User ;
  email "bob@example.com" .

# ❌ Denormalize: Duplicate user data
app:emailRecord1 userName "Alice" ;
  email "alice@example.com" .
```

## Summary

SPARQL design thinking:
- ✅ Think in **graph patterns**, not table joins
- ✅ Follow **natural paths** through relationships
- ✅ Use **graph shape** as your query language
- ✅ Let **filters refine**, not define
- ✅ Embrace **semantic relationships**
- ✅ Design for **query patterns** upfront

Your SPARQL queries will be cleaner, faster, and more maintainable!
