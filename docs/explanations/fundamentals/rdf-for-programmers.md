# RDF for Programmers

**Concept**: RDF is a graph database format for representing structured data. Think of it as JSON for linked data.

**Why it matters**: Understanding RDF basics lets you define domain models that work with ggen, Schema.org, and the Semantic Web.

**No prior knowledge required**: This guide assumes you know JavaScript/Python/Rust but have never seen RDF.

---

## RDF in 60 Seconds

**RDF** = Resource Description Framework

**Core idea**: Everything is a graph of **triples**

**Triple** = `(subject, predicate, object)`

Think of it as: **"subject has predicate with value object"**

**Example**:
- `(John, knows, Mary)` - "John knows Mary"
- `(Book123, title, "RDF Guide")` - "Book123 has title 'RDF Guide'"
- `(Product, price, 29.99)` - "Product has price 29.99"

---

## From JSON to RDF

### JSON Object (Familiar)

```json
{
  "id": "product-001",
  "name": "Laptop",
  "price": 999.99,
  "inStock": true
}
```

**Interpretation**: This object has 4 properties

### Same Data in RDF Triples

```
(product-001, id, "product-001")
(product-001, name, "Laptop")
(product-001, price, 999.99)
(product-001, inStock, true)
```

**Interpretation**: 4 triples, all with same subject

### Key Difference

**JSON**: Hierarchical (trees)
**RDF**: Graph (nodes connected by labeled edges)

**JSON strength**: Nested structures
**RDF strength**: Linked data, relationships between entities

---

## Turtle Syntax (RDF for Humans)

**Turtle** is the most readable RDF syntax. It's what you'll write in `.ttl` files.

### Basic Turtle

```turtle
@prefix ex: <http://example.org/> .

ex:product-001 ex:name "Laptop" .
ex:product-001 ex:price 999.99 .
ex:product-001 ex:inStock true .
```

**Reads as**:
- `ex:product-001` is the subject (a product)
- `ex:name` is the predicate (property)
- `"Laptop"` is the object (value)
- `.` ends the triple

### Shorthand: Same Subject

```turtle
@prefix ex: <http://example.org/> .

ex:product-001
    ex:name "Laptop" ;
    ex:price 999.99 ;
    ex:inStock true .
```

**Shorthand rules**:
- `;` means "same subject, new predicate"
- `.` means "end of this subject"

### Shorthand: Multiple Values

```turtle
@prefix ex: <http://example.org/> .

ex:john
    ex:knows ex:mary, ex:bob, ex:alice .
```

**Means**:
```
John knows Mary
John knows Bob
John knows Alice
```

**`,` means "same subject, same predicate, new object"**

---

## Data Types

### Strings

```turtle
ex:product ex:name "Laptop" .
```

### Numbers

```turtle
ex:product ex:price 999.99 .
ex:product ex:quantity 42 .
```

### Booleans

```turtle
ex:product ex:inStock true .
ex:product ex:discontinued false .
```

### Typed Literals (Explicit Types)

```turtle
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

ex:product
    ex:price "999.99"^^xsd:decimal ;
    ex:launchDate "2024-01-15"^^xsd:date ;
    ex:weight "2.5"^^xsd:float .
```

**`^^xsd:decimal` means "this string is a decimal number"**

### Common XSD Types

| XSD Type | Example | JavaScript Equivalent |
|----------|---------|----------------------|
| `xsd:string` | `"hello"` | `string` |
| `xsd:integer` | `42` | `number` (integer) |
| `xsd:decimal` | `99.99` | `number` (decimal) |
| `xsd:float` | `3.14` | `number` |
| `xsd:boolean` | `true` | `boolean` |
| `xsd:date` | `"2024-01-15"` | `Date` |
| `xsd:dateTime` | `"2024-01-15T10:30:00"` | `Date` |

---

## Classes and Properties (Schema Definition)

### Defining a Class (Type)

```turtle
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
@prefix ex: <http://example.org/> .

ex:Product a rdfs:Class ;
    rdfs:label "Product" ;
    rdfs:comment "A product in the e-commerce catalog" .
```

**`a` is shorthand for `rdf:type`**

**Reads as**: "Product is a Class with label 'Product' and comment '...'"

### Defining Properties (Fields)

```turtle
ex:name a rdf:Property ;
    rdfs:domain ex:Product ;
    rdfs:range xsd:string ;
    rdfs:label "name" ;
    rdfs:comment "The product name" .

ex:price a rdf:Property ;
    rdfs:domain ex:Product ;
    rdfs:range xsd:decimal ;
    rdfs:label "price" .
```

**Key concepts**:
- `rdfs:domain` = "this property applies to Product instances"
- `rdfs:range` = "this property's value is a string/decimal/etc."

### Creating Instances

```turtle
ex:product-001 a ex:Product ;
    ex:name "Laptop" ;
    ex:price "999.99"^^xsd:decimal ;
    ex:inStock true .
```

**`a ex:Product` means "this is an instance of the Product class"**

---

## Relationships Between Entities

### Simple Relationship

```turtle
ex:product-001 ex:manufacturer ex:company-123 .

ex:company-123 a ex:Organization ;
    ex:name "TechCorp" .
```

**Graph visualization**:
```
product-001 --manufacturer--> company-123
                                |
                              name: "TechCorp"
```

### Nested Relationships

```turtle
ex:product-001 a ex:Product ;
    ex:name "Laptop" ;
    ex:category ex:electronics .

ex:electronics a ex:Category ;
    ex:name "Electronics" ;
    ex:parent ex:technology .

ex:technology a ex:Category ;
    ex:name "Technology" .
```

**Traversal**: Product → Electronics → Technology

---

## Namespaces and Prefixes

### Why Namespaces?

**Problem**: Avoid name collisions

Your `name` property vs. Schema.org's `name` property

### Prefixes

```turtle
@prefix ex: <http://example.org/> .
@prefix schema: <https://schema.org/> .
@prefix foaf: <http://xmlns.com/foaf/0.1/> .

ex:product-001
    schema:name "Laptop" ;
    foaf:name "Laptop (FOAF)" ;
    ex:name "Laptop (Custom)" .
```

**All three are different properties!**

### Standard Prefixes

| Prefix | Namespace | Purpose |
|--------|-----------|---------|
| `rdf:` | `http://www.w3.org/1999/02/22-rdf-syntax-ns#` | RDF core |
| `rdfs:` | `http://www.w3.org/2000/01/rdf-schema#` | RDF Schema |
| `xsd:` | `http://www.w3.org/2001/XMLSchema#` | Data types |
| `schema:` | `https://schema.org/` | Schema.org vocabulary |
| `foaf:` | `http://xmlns.com/foaf/0.1/` | Friend of a Friend |
| `dc:` | `http://purl.org/dc/elements/1.1/` | Dublin Core |

---

## Complete Example: E-commerce Product

```turtle
@prefix ex: <http://example.org/> .
@prefix schema: <https://schema.org/> .
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

# Define Product class
ex:Product a rdfs:Class ;
    rdfs:label "Product" ;
    rdfs:comment "A product in the catalog" .

# Define properties
ex:sku a rdf:Property ;
    rdfs:domain ex:Product ;
    rdfs:range xsd:string ;
    rdfs:label "sku" .

ex:name a rdf:Property ;
    rdfs:domain ex:Product ;
    rdfs:range xsd:string ;
    rdfs:label "name" .

ex:price a rdf:Property ;
    rdfs:domain ex:Product ;
    rdfs:range xsd:decimal ;
    rdfs:label "price" .

ex:category a rdf:Property ;
    rdfs:domain ex:Product ;
    rdfs:range ex:Category ;
    rdfs:label "category" .

# Define Category class
ex:Category a rdfs:Class ;
    rdfs:label "Category" .

# Create category instance
ex:electronics a ex:Category ;
    ex:name "Electronics" .

# Create product instance
ex:product-001 a ex:Product ;
    ex:sku "LAP-2024-001" ;
    ex:name "Wireless Laptop" ;
    ex:price "999.99"^^xsd:decimal ;
    ex:category ex:electronics ;
    ex:inStock true .
```

---

## RDF vs. Programming Constructs

### RDF Class ≈ JavaScript Class / Python Class / Rust Struct

**RDF**:
```turtle
ex:Product a rdfs:Class .
```

**JavaScript**:
```javascript
class Product { }
```

**Python**:
```python
class Product:
    pass
```

**Rust**:
```rust
struct Product { }
```

### RDF Property ≈ Class Field / Attribute

**RDF**:
```turtle
ex:name a rdf:Property ;
    rdfs:domain ex:Product ;
    rdfs:range xsd:string .
```

**JavaScript**:
```javascript
class Product {
  name: string;
}
```

**Python**:
```python
class Product:
    name: str
```

**Rust**:
```rust
struct Product {
    name: String,
}
```

### RDF Instance ≈ Object Instance

**RDF**:
```turtle
ex:product-001 a ex:Product ;
    ex:name "Laptop" .
```

**JavaScript**:
```javascript
const product001 = new Product();
product001.name = "Laptop";
```

**Python**:
```python
product_001 = Product()
product_001.name = "Laptop"
```

**Rust**:
```rust
let product_001 = Product {
    name: "Laptop".to_string(),
};
```

---

## Common Patterns

### Pattern 1: Enumeration

```turtle
ex:ProductStatus a rdfs:Class ;
    rdfs:label "Product Status" .

ex:InStock a ex:ProductStatus ;
    rdfs:label "In Stock" .

ex:OutOfStock a ex:ProductStatus ;
    rdfs:label "Out of Stock" .

ex:Discontinued a ex:ProductStatus ;
    rdfs:label "Discontinued" .

# Use in product
ex:product-001 ex:status ex:InStock .
```

**Generates to**:
```javascript
const ProductStatus = z.enum(["InStock", "OutOfStock", "Discontinued"]);
```

### Pattern 2: Optional Properties

```turtle
ex:description a rdf:Property ;
    rdfs:domain ex:Product ;
    rdfs:range xsd:string ;
    rdfs:label "description" .
    # No "required" constraint means optional
```

**Generates to**:
```javascript
const ProductSchema = z.object({
  description: z.string().optional(),
});
```

### Pattern 3: Lists/Arrays

```turtle
ex:product-001
    ex:tags "electronics", "laptop", "portable" .
```

**Generates to**:
```javascript
tags: z.array(z.string())
```

---

## Tools for Working with RDF

### View RDF in ggen

```bash
# Load RDF file
ggen graph load --file product.ttl

# Query all triples
ggen graph query --sparql_query "SELECT ?s ?p ?o WHERE { ?s ?p ?o }"

# Export to different format
ggen graph export --output product.jsonld --format json-ld
```

### Validate RDF

```bash
# Validate ontology
ggen ontology validate --schema product.ttl --strict
```

### Visualize RDF

```bash
# Generate visual graph
ggen graph visualize --output graph.svg
```

---

## Common Mistakes

### Mistake 1: Forgetting Prefixes

❌ **Wrong**:
```turtle
Product a Class .
```

✅ **Correct**:
```turtle
@prefix ex: <http://example.org/> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .

ex:Product a rdfs:Class .
```

### Mistake 2: Forgetting Trailing Dot

❌ **Wrong**:
```turtle
ex:product ex:name "Laptop"
ex:product ex:price 999.99
```

✅ **Correct**:
```turtle
ex:product ex:name "Laptop" .
ex:product ex:price 999.99 .
```

### Mistake 3: Mixing `;` and `.`

❌ **Wrong**:
```turtle
ex:product
    ex:name "Laptop" .
    ex:price 999.99 ;
```

✅ **Correct**:
```turtle
ex:product
    ex:name "Laptop" ;
    ex:price 999.99 .
```

---

## Next Steps

**Practice**:
1. [Quick Start Tutorial](../../getting-started/quick-start.md) - Create your first ontology
2. [Query RDF with SPARQL](../../how-to/generation/query-rdf-sparql.md) - Extract data from RDF

**Learn More**:
- [W3C RDF Primer](https://www.w3.org/TR/rdf11-primer/)
- [Turtle Syntax Spec](https://www.w3.org/TR/turtle/)
- [RDF Validator](http://www.w3.org/RDF/Validator/)

**Real Ontologies**:
- [Schema.org](https://schema.org) - E-commerce, events, organizations
- [FOAF](http://xmlns.com/foaf/spec/) - People and social networks
- [Dublin Core](https://www.dublincore.org/specifications/dublin-core/dcmi-terms/) - Metadata

---

## Summary

**Key Concepts**:
- ✅ RDF = graph of triples (subject, predicate, object)
- ✅ Turtle = human-readable RDF syntax
- ✅ Classes = types (like `class Product`)
- ✅ Properties = fields (like `name: string`)
- ✅ Instances = objects (like `const product = new Product()`)
- ✅ Prefixes = namespaces (avoid collisions)
- ✅ XSD types = data types (string, decimal, boolean, etc.)

**Turtle Shortcuts**:
- `;` = same subject, new predicate
- `,` = same subject, same predicate, new object
- `a` = shorthand for `rdf:type`
- `.` = end of statement

**Bottom Line**: If you understand JSON objects and classes, you understand 80% of RDF!
