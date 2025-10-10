# Chapter 3: Code as a Projection of Knowledge

## The Fundamental Insight

Here's a statement that might sound radical:

**Code is not the thing you're building. Code is a projection of the thing you're building.**

Let me explain with an analogy.

## The Architect's Blueprints

When an architect designs a building, they create multiple projections of the same underlying design:

- **Floor plans** (2D top-down view)
- **Elevations** (2D side views)
- **Sections** (2D cut-throughs)
- **3D renderings** (visual representation)
- **Structural drawings** (for engineers)
- **Electrical plans** (for electricians)
- **Plumbing diagrams** (for plumbers)

Each projection serves a different purpose, but they all derive from the **same design knowledge**.

When the architect changes the design—say, moving a wall—they update the design model, and all projections update accordingly. Nobody manually redraws seven different documents.

In modern architecture, this is done with **Building Information Modeling (BIM)** software like Revit or ArchiCAD. The architect models the building once, and the software projects it into all necessary views.

## Software's False Primary Artifact

Now consider software development.

We have a design (often implicit in our heads or scattered in docs), and we need multiple representations:

- **Database schema** (data structure)
- **Backend types** (ORM models)
- **API specification** (contract)
- **Frontend types** (client models)
- **Validation logic** (constraints)
- **Documentation** (human-readable)
- **Tests** (behavioral specification)

These are all **projections** of the same underlying knowledge about our domain.

But unlike architects, we treat one projection—the **code**—as the primary artifact. We write it by hand, and we write it multiple times in different forms.

It's as if architects drew floor plans by hand, then drew elevations by hand, then drew sections by hand, manually keeping them in sync. Absurd in architecture, normal in software.

## What is Knowledge?

Before we go further, let's define what we mean by "knowledge" in software.

Knowledge in a software system includes:

**1. Domain Concepts**
- "A User has a unique email address"
- "Products belong to Categories"
- "Orders have a status that follows a state machine"

**2. Constraints**
- "Ratings must be between 1 and 5"
- "Passwords must be at least 12 characters"
- "Inventory cannot go negative"

**3. Relationships**
- "A Review belongs to one Product and one User"
- "An Order contains many OrderItems"
- "Categories can have subcategories (hierarchical)"

**4. Behavior**
- "When an Order is placed, decrement inventory"
- "When a User signs up, send a welcome email"
- "If payment fails, cancel the Order"

**5. Derivations**
- "Order total = sum of OrderItem subtotals + shipping - discounts"
- "User reputation = average of their review ratings"
- "Product in stock = inventory > 0"

All of this knowledge currently lives **implicitly in code**, scattered across files, layers, and languages.

## The Current Approach: Knowledge Imprisonment

Let's trace how knowledge flows in a traditional codebase.

**Step 1: Knowledge in the developer's head**
"Products have reviews with ratings 1-5"

**Step 2: Encode in database schema**
```sql
CREATE TABLE reviews (
  id UUID PRIMARY KEY,
  product_id UUID REFERENCES products(id),
  rating INTEGER CHECK (rating BETWEEN 1 AND 5)
);
```

**Step 3: Re-encode in ORM model**
```python
class Review(Base):
    __tablename__ = 'reviews'
    id = Column(UUID, primary_key=True)
    product_id = Column(UUID, ForeignKey('products.id'))
    rating = Column(Integer, CheckConstraint('rating >= 1 AND rating <= 5'))
```

**Step 4: Re-encode in API schema**
```yaml
Review:
  type: object
  properties:
    id:
      type: string
      format: uuid
    productId:
      type: string
      format: uuid
    rating:
      type: integer
      minimum: 1
      maximum: 5
```

**Step 5: Re-encode in frontend types**
```typescript
interface Review {
  id: string;
  productId: string;
  rating: number; // 1-5
}
```

**Step 6: Re-encode in validation**
```typescript
const reviewSchema = z.object({
  productId: z.string().uuid(),
  rating: z.number().min(1).max(5),
});
```

**Step 7: Re-encode in tests**
```typescript
const validReview = { rating: 3 }; // OK
const invalidReview = { rating: 6 }; // Should fail
```

**Step 8: Re-encode in documentation**
```markdown
## Create Review
- `rating` (required): Integer between 1 and 5
```

The knowledge "ratings are 1-5 integers" was encoded **eight times** in **six different languages**.

Now imagine changing it to "ratings are 1-10 integers". Eight places to update. Miss one? Runtime error or data corruption.

## The Knowledge-First Approach

What if we stored the knowledge once in a **universal, semantic format**?

```turtle
:Review a owl:Class ;
  rdfs:label "Product Review" ;
  :hasProperty [
    :name "rating" ;
    :type xsd:integer ;
    :minValue 1 ;
    :maxValue 5 ;
    :required true ;
    rdfs:comment "Customer rating on a scale of 1 (worst) to 5 (best)"
  ] ;
  :hasRelationship [
    :name "product" ;
    :target :Product ;
    :cardinality "1" ;
    :inverse "reviews"
  ] .
```

This is **RDF** (Resource Description Framework), a W3C standard for expressing knowledge. It's semantic, machine-readable, and language-agnostic.

From this single source, we can **project** into all needed forms:

**PostgreSQL (via template):**
```sql
CREATE TABLE reviews (
  id UUID PRIMARY KEY,
  product_id UUID REFERENCES products(id),
  rating INTEGER CHECK (rating >= 1 AND rating <= 5)
);
COMMENT ON COLUMN reviews.rating IS 'Customer rating on a scale of 1 (worst) to 5 (best)';
```

**TypeScript types (via template):**
```typescript
/**
 * Customer rating on a scale of 1 (worst) to 5 (best)
 */
interface Review {
  id: string;
  productId: string;
  /** @minimum 1 @maximum 5 */
  rating: number;
}
```

**Validation (via template):**
```typescript
const reviewSchema = z.object({
  rating: z.number().min(1).max(5),
  productId: z.string().uuid(),
});
```

**OpenAPI docs (via template):**
```yaml
Review:
  properties:
    rating:
      type: integer
      minimum: 1
      maximum: 5
      description: Customer rating on a scale of 1 (worst) to 5 (best)
```

Change the knowledge? Regenerate. The projections stay in sync **automatically**.

## The Projection Layer

Templates are the key to projection. A template takes knowledge (RDF) and transforms it into code, schemas, or documentation.

**Example template (simplified):**
```handlebars
interface {{className}} {
  id: string;
{{#each properties}}
  {{#if comment}}/** {{comment}} */{{/if}}
  {{name}}{{#unless required}}?{{/unless}}: {{typeScriptType type}};
{{/each}}
}
```

**Input knowledge:**
```turtle
:Review :hasProperty [
  :name "rating" ;
  :type xsd:integer ;
  :required true ;
  :comment "Customer rating 1-5"
] .
```

**Output TypeScript:**
```typescript
interface Review {
  id: string;
  /** Customer rating 1-5 */
  rating: number;
}
```

Templates are reusable. Write a template once for "TypeScript interface from OWL class," and it works for all classes in your domain.

## The Knowledge Graph

As your domain grows, knowledge becomes interconnected—a **knowledge graph**.

```turtle
:User a owl:Class .
:Product a owl:Class .
:Review a owl:Class ;
  :relatesTo :Product ;
  :relatesTo :User .

:Order a owl:Class ;
  :relatesTo :User ;
  :contains :OrderItem .

:OrderItem a owl:Class ;
  :relatesTo :Product ;
  :partOf :Order .
```

This graph captures:
- Entities (User, Product, Review, Order, OrderItem)
- Relationships (User → Review → Product)
- Structure (Order contains OrderItems)

From this graph, you can query:
- "What entities relate to Product?" → Review, OrderItem
- "What's the path from User to Product?" → User → Order → OrderItem → Product
- "What aggregates contain Product?" → OrderItem (via Order)

These queries enable **smart generation**:
- Generate CRUD APIs for all entities
- Generate relationship endpoints (GET /products/:id/reviews)
- Generate join queries for related entities
- Generate GraphQL resolvers for the entire graph

## Real-World Example: The E-commerce Knowledge Graph

Let's build a small e-commerce domain from scratch, knowledge-first.

**Step 1: Define entities**
```turtle
@prefix : <http://example.com/ecommerce#> .
@prefix owl: <http://www.w3.org/2002/07/owl#> .

:Product a owl:Class ;
  rdfs:label "Product" ;
  :hasProperty [
    :name "sku" ;
    :type xsd:string ;
    :unique true ;
    :required true
  ] ;
  :hasProperty [
    :name "price" ;
    :type :Money ;
    :required true
  ] .

:User a owl:Class ;
  rdfs:label "User" ;
  :hasProperty [
    :name "email" ;
    :type :Email ;
    :unique true ;
    :required true
  ] .

:Review a owl:Class ;
  rdfs:label "Review" ;
  :hasProperty [
    :name "rating" ;
    :type xsd:integer ;
    :min 1 ;
    :max 5
  ] ;
  :belongsTo :Product ;
  :createdBy :User .
```

**Step 2: Generate schema**
```bash
ggen project generate db-schema
```

**Output:**
```sql
CREATE TABLE products (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  sku VARCHAR(255) UNIQUE NOT NULL,
  price DECIMAL(10,2) NOT NULL,
  created_at TIMESTAMP DEFAULT NOW(),
  updated_at TIMESTAMP DEFAULT NOW()
);

CREATE TABLE users (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  email VARCHAR(255) UNIQUE NOT NULL,
  created_at TIMESTAMP DEFAULT NOW(),
  updated_at TIMESTAMP DEFAULT NOW()
);

CREATE TABLE reviews (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  rating INTEGER CHECK (rating >= 1 AND rating <= 5),
  product_id UUID REFERENCES products(id) ON DELETE CASCADE,
  user_id UUID REFERENCES users(id) ON DELETE CASCADE,
  created_at TIMESTAMP DEFAULT NOW(),
  updated_at TIMESTAMP DEFAULT NOW()
);

CREATE INDEX idx_reviews_product ON reviews(product_id);
CREATE INDEX idx_reviews_user ON reviews(user_id);
```

**Step 3: Generate API types**
```bash
ggen project generate api-types
```

**Output (TypeScript):**
```typescript
export interface Product {
  id: string;
  sku: string;
  price: number;
  createdAt: string;
  updatedAt: string;
}

export interface User {
  id: string;
  email: string;
  createdAt: string;
  updatedAt: string;
}

export interface Review {
  id: string;
  rating: number;
  productId: string;
  userId: string;
  createdAt: string;
  updatedAt: string;
}
```

**Step 4: Generate CRUD operations**
```bash
ggen project generate crud-api
```

**Output (pseudocode):**
```typescript
// ProductService.ts
class ProductService {
  async findAll(query: QueryParams): Promise<Product[]>
  async findById(id: string): Promise<Product>
  async create(data: CreateProductDTO): Promise<Product>
  async update(id: string, data: UpdateProductDTO): Promise<Product>
  async delete(id: string): Promise<void>
  async findReviews(id: string): Promise<Review[]> // from relationship
}
```

**Total manual work:** Defining the knowledge graph.

**Generated:** Schema, types, CRUD, validations, tests, docs.

## Benefits of Code as Projection

**1. Single Source of Truth**

Change the knowledge, regenerate projections. No synchronization needed.

**2. Consistency by Default**

All projections derive from the same source, so they can't drift.

**3. Knowledge Reuse**

Patterns (CRUD, authentication, pagination) are templates. Reuse them across entities.

**4. Tooling**

Knowledge graphs enable intelligent tools:
- Auto-complete based on domain concepts
- Validation at the knowledge level
- Impact analysis ("what depends on this entity?")
- Semantic search ("find all entities with email properties")

**5. Evolution**

Add a property? Regenerate everything. Change a relationship? Regenerate affected code. The system maintains itself.

## The Shift in Mindset

Working knowledge-first requires a mental shift:

| Old mindset | New mindset |
|-------------|-------------|
| "I need to write a User model" | "I need to define what a User is" |
| "How do I implement this in Python?" | "What knowledge does this represent?" |
| "Let me copy this code and modify it" | "Let me reuse this pattern template" |
| "I need to update the types to match" | "I'll update the knowledge and regenerate" |
| "Is the documentation up to date?" | "Documentation is always generated fresh" |

**Code is output, not input.**

## Limitations and Trade-offs

Projection isn't a silver bullet. There are trade-offs:

**1. Template Development**

Someone has to write and maintain templates. This is upfront effort that pays off over time.

**2. Customization**

Generated code is standard. Highly custom logic still requires hand-coding (but can reference generated structures).

**3. Learning Curve**

RDF and SPARQL aren't mainstream. Developers need to learn semantic technologies.

**4. Tooling Maturity**

Knowledge-first workflows are less common than code-first. Fewer tools, smaller community.

**5. Hybrid Reality**

You'll likely work in a hybrid model: knowledge-first for structure, hand-coded for custom logic.

## The Path Forward

The next chapter explores **KGC (Knowledge-Graph-Code)**: the school of thought that formalizes this approach.

We'll see how GGen brings together:
- **RDF/OWL** for knowledge representation
- **SPARQL** for querying knowledge
- **Templates** for code projection
- **CLI workflows** for daily usage

You'll learn to work at the knowledge level, treating code as a byproduct rather than the main event.

The goal isn't to eliminate programming—it's to **elevate it**. Spend your time on problems that require human creativity, and let the machine handle the projections.
