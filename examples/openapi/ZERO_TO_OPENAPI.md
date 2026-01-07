# 🚀 From Zero to OpenAPI: Complete Beginner's Guide

## Welcome!

This guide will take you from zero knowledge to generating a complete REST API with OpenAPI specs, validation schemas, and type definitions—all from a single RDF specification.

**Time needed**: 30 minutes for full understanding + examples
**Prerequisites**: Basic command line familiarity, text editor, Node.js 18+
**End result**: Full API contract (OpenAPI + Zod schemas + JSDoc types)

---

## 📚 What You're About to Learn

By the end of this guide, you'll understand:

1. **What is ggen?** - An ontology-driven code generation system
2. **What is RDF?** - A way to describe data that computers can understand
3. **What is SPARQL?** - A query language to extract data from RDF
4. **What are templates?** - How to turn extracted data into code
5. **How it all works together** - The complete pipeline from spec to code

---

## Part 1: Understanding ggen (5 minutes)

### What is ggen?

`ggen` is a **specification-first code generation** tool. Instead of writing code manually, you:

1. **Write a specification** (RDF ontology) describing your API domain
2. **Run ggen sync** to generate code from that specification
3. **Get three synchronized outputs** (all in perfect agreement):
   - OpenAPI 3.0 spec (REST API documentation)
   - Zod validation schemas (runtime validation)
   - JSDoc type definitions (IDE support)

### Why This Matters

- **Single source of truth**: Change the spec, regenerate everything
- **Zero divergence**: Spec and code are always in sync
- **Type-safe**: Types, validation, and docs from one source
- **Deterministic**: Same spec → identical output every time

### Real-World Analogy

Think of it like a **recipe that generates restaurants**:

```
Recipe (ontology)
    ↓
    ├─→ Kitchen specifications (OpenAPI)
    ├─→ Ingredient validators (Zod)
    └─→ Equipment guides (JSDoc types)
```

Change one recipe → all three update automatically.

---

## Part 2: Understanding RDF (10 minutes)

### What is RDF?

RDF stands for **Resource Description Framework**. It's a way to describe data as **triples**.

A triple is: **Subject - Predicate - Object**

**Example 1**:
```
User  hasProperty  email
```
(The entity "User" has a property named "email")

**Example 2**:
```
email  hasType  string
```
(The property "email" has the type "string")

### RDF in Turtle Syntax

RDF is written in **Turtle** format (`.ttl` files):

```turtle
# Define a User entity
blog:User a api:Entity ;
    api:name "User" ;
    api:hasProperty blog:User_id ;
    api:hasProperty blog:User_email .

# Define properties
blog:User_id a api:Property ;
    api:name "id" ;
    api:type "string" ;
    api:required "true" .

blog:User_email a api:Property ;
    api:name "email" ;
    api:type "string" ;
    api:required "true" .
```

**Breaking it down:**

- `blog:User` - A reference (namespace:name)
- `a api:Entity` - This is an Entity type
- `api:name "User"` - Its display name is "User"
- `api:hasProperty blog:User_id` - It has a property called User_id
- `;` - Continue defining this resource
- `.` - End the definition

### The Blog API Example

The `examples/openapi/ontology/blog-api.ttl` file defines:

- **Entities**: User, Post, Comment, Tag
- **Properties**: Each entity has id, name, email, etc.
- **Endpoints**: CRUD operations for each entity
- **Request types**: CreateUserRequest, UpdateUserRequest, etc.

### Key Concepts

| Concept | Meaning | Example |
|---------|---------|---------|
| Entity | A data type in your domain | User, Post, Comment |
| Property | A field of an entity | id, username, email |
| Type | The data type of a property | string, number, boolean |
| Required | Whether a property must be present | true, false |

---

## Part 3: Understanding SPARQL (5 minutes)

### What is SPARQL?

SPARQL is like **SQL for RDF graphs**. It queries RDF data and extracts results.

### A Simple SPARQL Query

```sparql
PREFIX api: <https://ggen.io/ontology/api#>

SELECT ?entityName ?propertyName ?propertyType ?required
WHERE {
  ?entity a api:Entity ;
          api:name ?entityName ;
          api:hasProperty ?property .
  ?property api:name ?propertyName ;
            api:type ?propertyType .
  OPTIONAL { ?property api:required ?required }
}
ORDER BY ?entityName ?propertyName
```

**What this does:**

1. **SELECT** - Choose what to return
   - `?entityName` - Store entity name in a variable
   - `?propertyName` - Store property name in a variable
   - `?propertyType` - Store property type in a variable
   - `?required` - Store required flag in a variable

2. **WHERE** - Define the pattern to match
   - Find all entities (`?entity a api:Entity`)
   - Get their name (`api:name ?entityName`)
   - Get their properties (`api:hasProperty ?property`)
   - Get property details (name, type)

3. **OPTIONAL** - Include this if it exists (may be missing)
   - `required` flag is optional

4. **ORDER BY** - Sort results

### Query Results

This query returns a table of results:

```
entityName | propertyName | propertyType | required
-----------|--------------|--------------|----------
User       | id           | string       | true
User       | username     | string       | true
User       | email        | string       | true
Post       | id           | string       | true
Post       | title        | string       | true
...
```

**This is what gets passed to templates!**

---

## Part 4: Understanding Templates (5 minutes)

### What are Templates?

Templates are files that transform SPARQL results into code. They use **Tera** (a templating language, like Handlebars or Jinja2).

### Template Structure

Every template has **two parts**:

#### Part 1: YAML Frontmatter (metadata)

```yaml
---
to: lib/schemas/entities.mjs
description: Generates Zod validation schemas for entities
---
```

This tells ggen:
- Where to write the output (`to: lib/schemas/entities.mjs`)
- What this template does (`description`)

#### Part 2: Template Body (code generation)

```tera
// Zod schemas for validation
{% for row in sparql_results %}
export const {{ row["?entityName"] | lower }}Schema = z.object({
  {% for prop_row in sparql_results %}
    {% if prop_row["?entityName"] == row["?entityName"] %}
    {{ prop_row["?propertyName"] }}: z.string(),
    {% endif %}
  {% endfor %}
});
{% endfor %}
```

### How Templates Work: Step-by-Step

**Input**: SPARQL results (the table from above)

**Template Logic**:
1. Loop through each entity
2. For each entity, create a Zod schema
3. For each property of that entity, add a field
4. Substitute variable values

**Output**: Generated code

```javascript
// Generated output
export const userSchema = z.object({
  id: z.string(),
  username: z.string(),
  email: z.string(),
});

export const postSchema = z.object({
  id: z.string(),
  title: z.string(),
});
```

### Template Variables

In a template, you can access:
- `sparql_results` - The table of results
- `row["?variableName"]` - Access a specific variable
- Filters: `| lower`, `| upper`, `| capitalize` - Transform values

---

## Part 5: The Complete Flow (5 minutes)

### Step-by-Step Process

```
1. You run: ggen sync
                ↓
2. ggen reads: ontology/blog-api.ttl (the RDF spec)
                ↓
3. For each rule in ggen.toml:
    a) Run SPARQL query → get results
    b) Pass results to template
    c) Render template with results
    d) Write to output file
                ↓
4. Output: lib/ directory with generated files
```

### Concrete Example: Generating a Zod Schema

**Step 1: Input (Ontology)**
```turtle
blog:User a api:Entity ;
    api:name "User" ;
    api:hasProperty blog:User_email .
blog:User_email a api:Property ;
    api:name "email" ;
    api:type "string" .
```

**Step 2: SPARQL Query** (extract the data)
```sparql
SELECT ?entityName ?propertyName ?propertyType
WHERE {
  ?entity a api:Entity ;
          api:name ?entityName ;
          api:hasProperty ?property .
  ?property api:name ?propertyName ;
            api:type ?propertyType .
}
```

**Step 3: Query Results** (what the template gets)
```
entityName | propertyName | propertyType
-----------|--------------|---------------
User       | email        | string
```

**Step 4: Template** (generates code from results)
```tera
{% for row in sparql_results %}
export const {{ row["?entityName"] | lower }}Schema = z.object({
  {{ row["?propertyName"] }}: z.{{ row["?propertyType"] | lower }}(),
});
{% endfor %}
```

**Step 5: Generated Output**
```javascript
export const userSchema = z.object({
  email: z.string(),
});
```

**Step 6: Write to File**
```
lib/schemas/entities.mjs
```

---

## Part 6: The 13 Generation Rules

The `ggen.toml` file has 13 rules that generate different outputs:

### Core Rules (1-4): OpenAPI Specification
| Rule | Output | Purpose |
|------|--------|---------|
| 1 | `lib/openapi/api-info.yaml` | OpenAPI spec header (title, version, servers) |
| 2 | `lib/openapi/schemas.yaml` | OpenAPI component schemas (entities) |
| 3 | `lib/openapi/paths.yaml` | OpenAPI paths/endpoints (CRUD operations) |
| 4 | `lib/openapi/openapi.yaml` | Combined OpenAPI spec (complete, mergeable) |

### JavaScript Validation Rules (5-9): Type Safety
| Rule | Output | Purpose |
|------|--------|---------|
| 5 | `lib/types/entities.mjs` | JSDoc type definitions for entities |
| 6 | `lib/types/requests.mjs` | JSDoc type definitions for requests |
| 7 | `lib/schemas/entities.mjs` | Zod validation schemas for entities |
| 8 | `lib/schemas/requests.mjs` | Zod validation schemas for requests |
| 9 | `lib/guards/entities.mjs` | Runtime type guard functions |

### Index/Barrel Export Rules (10-13): Clean Imports
| Rule | Output | Purpose |
|------|--------|---------|
| 10 | `lib/index.mjs` | Main barrel export for all generated modules |
| 11 | `lib/schemas/index.mjs` | Barrel export for schemas subdirectory |
| 12 | `lib/types/index.mjs` | Barrel export for types subdirectory |
| 13 | `lib/guards/index.mjs` | Barrel export for guards subdirectory |

Each rule:
- Has a unique SPARQL query to extract relevant data from the ontology
- Uses a specific Tera template to transform the data
- Produces one output file with deterministic formatting
- Runs independently but in coordinated sequence by ggen sync

---

## Part 7: Getting Started (15 minutes)

### Prerequisites

```bash
# Check you have Node.js
node --version  # Should be 18+

# Install/check ggen (from your ggen repo)
# You'll run ggen from the examples/openapi directory
```

### Setup

```bash
# Navigate to the example
cd examples/openapi

# List files
ls -la
# You should see:
# - ggen.toml (configuration with 10 rules)
# - ontology/blog-api.ttl (the RDF spec)
# - templates/ (template files for code generation)
# - golden/ (expected outputs for testing)
```

### Generate Code

```bash
# Generate all outputs from specification
ggen sync

# Check output was created
ls -la lib/
# You should see: schemas/, types/, guards/, openapi/, index.mjs
```

### Verify Output

```bash
# Check generated files
cat lib/schemas/entities.mjs
# You should see Zod schemas

cat lib/openapi/openapi.yaml
# You should see OpenAPI spec

cat lib/types/entities.mjs
# You should see JSDoc types
```

### Test Output

```bash
# Compare with golden files (expected output)
node validate.mjs
# Should output: ✅ All files match golden files
```

---

## Part 8: Modifying the Example

### Add a New Property

1. **Edit** `ontology/blog-api.ttl`

```turtle
# Add a new property to User entity
blog:User_avatar a api:Property ;
    api:name "avatar" ;
    api:type "string" ;
    api:format "url" .

# Link it to User
blog:User api:hasProperty blog:User_avatar .
```

2. **Regenerate**

```bash
ggen sync
```

3. **Verify**

```bash
cat lib/schemas/entities.mjs
# Should now include avatar field in userSchema
```

### Add a New Entity

1. **Edit** `ontology/blog-api.ttl`

```turtle
# Define new entity
blog:Category a api:Entity ;
    api:name "Category" ;
    rdfs:comment "Post category" ;
    api:hasProperty blog:Category_id ;
    api:hasProperty blog:Category_name .

# Define properties
blog:Category_id a api:Property ;
    api:name "id" ;
    api:type "string" ;
    api:required "true" .

blog:Category_name a api:Property ;
    api:name "name" ;
    api:type "string" ;
    api:required "true" .
```

2. **Regenerate**

```bash
ggen sync
```

3. **Verify**

```bash
cat lib/schemas/entities.mjs
# Should include categorySchema
```

---

## Part 9: Understanding the Output

### Generated Files

**`lib/schemas/entities.mjs`** - Zod validation
```javascript
import { z } from 'zod';

export const userSchema = z.object({
  id: z.string(),
  username: z.string(),
  email: z.string(),
});

// Use in code:
const result = userSchema.safeParse(data);
if (result.success) {
  // data is valid
  console.log(result.data);
} else {
  // Show validation errors
  console.error(result.error.flatten());
}
```

**`lib/types/entities.mjs`** - JSDoc types
```javascript
/**
 * @typedef {Object} User
 * @property {string} id
 * @property {string} username
 * @property {string} email
 * @property {string} bio
 * @property {string[]} posts
 */
```

**`lib/guards/entities.mjs`** - Runtime type guards
```javascript
// Type guard function generated from the ontology
export function isUser(data) {
  return (
    typeof data === 'object' &&
    data !== null &&
    typeof data.id === 'string' &&
    typeof data.username === 'string' &&
    typeof data.email === 'string'
  );
}
```

**`lib/openapi/openapi.yaml`** - Complete API documentation
```yaml
openapi: 3.0.0
info:
  title: Blog API
  version: 1.0.0
paths:
  /users:
    get:
      summary: List all users
      responses:
        '200':
          description: List of users
          content:
            application/json:
              schema:
                type: array
                items:
                  $ref: '#/components/schemas/User'
  /users/{id}:
    get:
      summary: Get a user by ID
      parameters:
        - name: id
          in: path
          required: true
          schema:
            type: string
components:
  schemas:
    User:
      type: object
      properties:
        id: { type: string }
        username: { type: string }
        email: { type: string }
```

### Using in Next.js

```javascript
// app/api/users/route.mjs
import { createUserSchema } from '@/lib/schemas/requests.mjs';
import { isUser } from '@/lib/guards/entities.mjs';
import type { User } from '@/lib/types/entities.mjs';

export async function POST(request) {
  const body = await request.json();

  // Validate using generated schema
  const result = createUserSchema.safeParse(body);
  if (!result.success) {
    return Response.json(
      { error: 'Invalid user data', issues: result.error.flatten() },
      { status: 400 }
    );
  }

  // Create user (data is now type-safe)
  const user = await db.users.create(result.data);

  // Verify output with guard
  if (!isUser(user)) {
    console.error('Database returned invalid user data');
  }

  return Response.json(user);
}

export async function GET(request) {
  const { searchParams } = new URL(request.url);
  const id = searchParams.get('id');

  if (!id) {
    const users = await db.users.list();
    return Response.json(users);
  }

  const user = await db.users.findById(id);
  if (!user) {
    return Response.json({ error: 'User not found' }, { status: 404 });
  }

  return Response.json(user);
}
```

### Using in Express.js

```javascript
// routes/users.js
import { Router } from 'express';
import { userSchema, createUserSchema } from '../lib/schemas/index.mjs';

const router = Router();

// Middleware: Validate request body
function validateBody(schema) {
  return (req, res, next) => {
    const result = schema.safeParse(req.body);
    if (!result.success) {
      return res.status(400).json({
        error: 'Validation failed',
        issues: result.error.flatten()
      });
    }
    req.validatedData = result.data;
    next();
  };
}

// GET /users
router.get('/', async (req, res) => {
  const users = await db.users.list();
  // All users are guaranteed to match userSchema
  res.json(users);
});

// POST /users
router.post(
  '/',
  validateBody(createUserSchema),
  async (req, res) => {
    // req.validatedData already validated by schema
    const user = await db.users.create(req.validatedData);
    res.status(201).json(user);
  }
);

export default router;
```

### Using the OpenAPI Spec

Generated `lib/openapi/openapi.yaml` can be used with:
- **Swagger UI**: Display interactive API documentation
- **ReDoc**: Pretty-print API docs
- **Insomnia/Postman**: Import specs for testing
- **API Gateway**: Use for request validation
- **Client generation**: Generate SDK from spec

```bash
# Serve OpenAPI spec with Swagger UI
npm install swagger-ui-express
```

```javascript
import swaggerUi from 'swagger-ui-express';
import YAML from 'yaml';
import fs from 'fs';

const openapi = YAML.parse(fs.readFileSync('./lib/openapi/openapi.yaml', 'utf8'));
app.use('/api/docs', swaggerUi.serve, swaggerUi.setup(openapi));
```

---

## Part 10: Key Concepts Summary

| Concept | What It Is | Why It Matters |
|---------|-----------|----------------|
| **RDF** | Graph data format | Describes domain semantically |
| **Ontology** | RDF file describing domain | Single source of truth |
| **SPARQL** | Query language for RDF | Extracts specific data patterns |
| **Template** | Code generation template | Transforms data to code |
| **Rule** | Query + Template + Output | One artifact generated |
| **ggen sync** | Runs all 10 rules | Creates all outputs at once |

---

## Troubleshooting

### Common Issues

**Q: "ggen command not found"**
- A: Make sure ggen is installed and in your PATH

**Q: Generated files are empty**
- A: Check SPARQL query syntax in ggen.toml
- A: Verify ontology file has matching data

**Q: Template syntax errors**
- A: Check Tera template documentation
- A: Verify variable names match SPARQL results

**Q: Output doesn't match golden files**
- A: Run `node validate.mjs` to see differences
- A: Check ontology definitions are complete

---

## Learning Resources

- **RDF/Turtle**: https://www.w3.org/TR/rdf11-primer/
- **SPARQL**: https://www.w3.org/2009/Talks/0615-qbe/
- **Tera Templates**: https://keats.github.io/tera/
- **Zod**: https://zod.dev/
- **JSDoc**: https://jsdoc.app/
- **OpenAPI**: https://swagger.io/specification/

---

## Next Steps

1. ✅ **Understand the concepts** (you've done this!)
2. 🔧 **Run `ggen sync`** in `examples/openapi/`
3. 📝 **Modify the ontology** - add a property or entity
4. 🎯 **Regenerate** and see how all outputs update
5. 🚀 **Use in your project** - copy the generated code patterns

---

## Quick Reference

### Files You'll Edit

```
examples/openapi/
├── ontology/blog-api.ttl      ← Edit here to change domain
├── ggen.toml                   ← 10 rules (read-only usually)
└── templates/*.tera            ← Templates (advanced)
```

### Commands You'll Run

```bash
# Generate everything
ggen sync

# Verify output
node validate.mjs

# Check a generated file
cat lib/schemas/entities.mjs
```

### The Pipeline

```
Edit ontology/
  → ggen sync
    → lib/ updated
      → Use in project
```

---

**You're ready to generate APIs from specifications! 🚀**

Questions? Check the README.md and CONFIGURATION_EXPLAINED.md files in the example directory.
