# Tutorial: Code Generation Working Loop

**Learn how RDF ontologies transform into reproducible code through SPARQL queries and templates**

---

## Learning Objectives

By the end of this tutorial, you will:
- Understand the RDF → SPARQL → Templates → Code generation flow
- Be able to read and modify SPARQL queries to extract different data
- Know how to write Tera templates that transform data into code
- Be able to add new entities and properties to an ontology
- Understand why this approach eliminates consistency bugs

**Estimated Time:** 35 minutes
**Difficulty:** Intermediate
**Prerequisites:** Basic Turtle (RDF) syntax, understanding of what SPARQL does

---

## What You'll Learn

This tutorial walks through the complete ggen code generation flow using the real `examples/openapi` project. We'll:

1. Start with an RDF ontology (source of truth)
2. Extract data with SPARQL queries
3. Transform data with Tera templates
4. Generate synchronized code artifacts
5. Verify consistency with golden files

---

## Part 1: Understanding the Source (RDF Ontology)

### What is RDF?

RDF (Resource Description Framework) represents information as **triples**: subject-predicate-object.

Example:
```
User hasProperty username    ← "User has a property called username"
```

### The Ontology is Your Source of Truth

Open `examples/openapi/ontology/blog-api.ttl`. At the top, you'll see the vocabulary definition:

```turtle
@prefix api: <https://ggen.io/ontology/api#> .
@prefix blog: <https://ggen.io/examples/openapi/> .

blog:User a api:Entity ;
    api:name "User" ;
    rdfs:comment "Blog user account" ;
    api:hasProperty blog:User_id ;
    api:hasProperty blog:User_email .
```

**Breaking it down:**
- `blog:User` - The entity (what we're describing)
- `a api:Entity` - Its type (class)
- `api:name "User"` - Its display name
- `api:hasProperty blog:User_id` - It has a property called User_id

### Properties Define Structure

Each property is also defined:

```turtle
blog:User_id a api:Property ;
    api:name "id" ;
    api:type "string" ;
    api:required "true" .
```

**This means:**
- Property name: `id`
- Type: `string`
- Required: `true` (must always be present)

### Try It Yourself

1. Open `examples/openapi/ontology/blog-api.ttl`
2. Find the `blog:Post` entity
3. List all its properties
4. What types do they have?

---

## Part 2: Extracting Data (SPARQL Queries)

### What is SPARQL?

SPARQL is a query language for RDF. It's like SQL, but for graph data:
- **SELECT** - Choose what to return
- **WHERE** - Define the pattern to match
- **OPTIONAL** - Include data if it exists
- **ORDER BY** - Sort results

### Your First SPARQL Query

Look at `examples/openapi/ggen.toml`. Find Rule 5 (JavaScript Type Definitions). It has a SPARQL query:

```sparql
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

**What this does step-by-step:**

1. **Find all entities:** `?entity a api:Entity`
2. **Get entity names:** `api:name ?entityName`
3. **Find their properties:** `api:hasProperty ?property`
4. **Get property info:** `?property api:name ?propertyName` and `api:type ?propertyType`
5. **Include required flag if present:** `OPTIONAL { ?property api:required ?required }`
6. **Sort for consistent output:** `ORDER BY ?entityName ?propertyName`

### What Results Look Like

When you run this query against the blog ontology, you get rows:

| entityName | propertyName | propertyType | required |
|-----------|--------------|--------------|----------|
| User      | id           | string       | true     |
| User      | email        | string       | true     |
| User      | username     | string       | (empty)  |
| Post      | id           | string       | true     |
| Post      | title        | string       | true     |

### Try It Yourself

1. Open `ggen.toml`
2. Find Rule 7 (Zod Entity Schemas)
3. Read its SPARQL query
4. What additional data does it extract compared to Rule 5?
5. Why might it need `minLength`, `maxLength`, `pattern`?

**Answer:** Because it needs to generate validation schemas with constraints!

---

## Part 3: Transforming Data (Tera Templates)

### What are Templates?

Templates define **how to turn query results into code**. They use Tera syntax (similar to Jinja2).

### Template Structure

Every template has two parts:

```yaml
---
to: lib/schemas/entities.mjs
description: Generates Zod validation schemas
---

{# Template body comes here #}
export const {{ entityName | lower }}Schema = z.object({
  ...
});
```

**The YAML frontmatter** (between `---`):
- `to:` - Where to write the output file
- `description:` - What this template does

**The template body**:
- Uses Tera syntax to access query results
- Loops through rows
- Outputs code

### A Real Example

Look at `examples/openapi/templates/zod-schemas.tera`:

```tera
---
to: lib/schemas/entities.mjs
description: Generate Zod validation schemas for entities
---
import { z } from 'zod';

{%- for row in sparql_results -%}
{%- if row["?entityName"] != prev_entity -%}

export const {{ row["?entityName"] | lower }}Schema = z.object({
{%- set prev_entity = row["?entityName"] -%}
{%- endif -%}
  {{ row["?propertyName"] }}: z.string(){% if row["?required"] == "true" %}.min(1){% else %}.optional(){% endif %},
{%- endfor -%}
});
```

**Breaking it down:**
- `{%- for row in sparql_results -%}` - Loop through query results
- `row["?propertyName"]` - Access the property name
- `row["?required"] == "true"` - Check if required
- `| lower` - Filter to convert to lowercase
- Output a Zod schema for each entity

### Template Variables

Templates have access to:
- `sparql_results` - Array of query result rows
- Each row has keys like `?entityName`, `?propertyName`, etc.
- Built-in Tera filters like `lower`, `upper`, `snake_case`

### Try It Yourself

1. Open `examples/openapi/templates/typescript-interfaces.tera`
2. Find where it uses `row["?propertyName"]`
3. Does it add `.optional()` for non-required properties?
4. What does the output look like?

---

## Part 4: The Complete Flow

### Step-by-Step Process

When you run `ggen sync`:

```
1. Load ontology/blog-api.ttl
   ↓
2. For each rule in ggen.toml:
   ├─ Execute SPARQL query
   ├─ Get results as sparql_results
   ├─ Load template file
   ├─ Render template with results
   └─ Write to output file
   ↓
3. Result: All files in lib/ synchronized
```

### Concrete Example: User Entity

**Input (Ontology):**
```turtle
blog:User a api:Entity ;
    api:name "User" ;
    api:hasProperty blog:User_email .
blog:User_email a api:Property ;
    api:name "email" ;
    api:type "string" .
```

**Step 1 - SPARQL Query Results:**
```
entityName: "User"
propertyName: "email"
propertyType: "string"
```

**Step 2 - Template Rendering:**
```tera
export const {{ row["?entityName"] | lower }}Schema = z.object({
  {{ row["?propertyName"] }}: z.string(),
});
```

**Step 3 - Generated Code:**
```javascript
export const userSchema = z.object({
  email: z.string(),
});
```

**Step 4 - Output File:**
```
lib/schemas/entities.mjs
```

### Why This Matters

**Single source of truth:** You define the User once in the ontology.

**Synchronized outputs:** OpenAPI spec, TypeScript types, Zod schemas, and JSDoc all match perfectly because they all come from the same data.

**Deterministic:** Same ontology + templates = identical output every time.

---

## Part 5: Modifying the Example

### Adding a New Property

Let's add an `avatar` field to User:

**Step 1:** Edit `ontology/blog-api.ttl`:

```turtle
blog:User_avatar a api:Property ;
    api:name "avatar" ;
    api:type "string" ;
    api:format "url" .

blog:User api:hasProperty blog:User_avatar .
```

**Step 2:** Run generation:
```bash
cd examples/openapi
ggen sync
```

**Step 3:** Check results:
```bash
cat lib/schemas/entities.mjs | grep -A 2 "avatar"
# Should show: avatar: z.string().optional(),

cat lib/types/entities.mjs | grep -A 2 "avatar"
# Should show: @property {string} avatar - avatar

cat lib/openapi/schemas.yaml | grep -A 2 "avatar"
# Should show: avatar in component schemas
```

**Result:** All three files automatically updated!

### Adding a New Entity

Let's add a `Category` entity:

**Step 1:** Edit `ontology/blog-api.ttl`:

```turtle
blog:Category a api:Entity ;
    api:name "Category" ;
    rdfs:comment "Post category" ;
    api:hasProperty blog:Category_id ;
    api:hasProperty blog:Category_name .

blog:Category_id a api:Property ;
    api:name "id" ;
    api:type "string" ;
    api:required "true" .

blog:Category_name a api:Property ;
    api:name "name" ;
    api:type "string" ;
    api:required "true" .
```

**Step 2:** Run generation:
```bash
ggen sync
```

**Step 3:** Verify:
```bash
grep -r "Category" lib/
# Should find: schemas, types, guards, and openapi
```

**Result:** All 10 generation rules now include Category!

---

## Part 6: Verification & Golden Files

### Why Golden Files?

Without verification, generated code could silently diverge. Golden files catch this.

### The Process

1. **Generate code:**
   ```bash
   ggen sync
   ```

2. **Compare with golden files:**
   ```bash
   ./verify.sh
   # or
   node validate.mjs
   ```

3. **Result:**
   ```
   ✅ All files match golden files
   ```

### If They Don't Match

The validator tells you which files differ:

```
❌ lib/schemas/entities.mjs differs from golden/lib/schemas/entities.mjs
  Run: diff lib/schemas/entities.mjs golden/lib/schemas/entities.mjs
```

Then:
1. Review the diff
2. If correct, update golden files: `cp -r lib/* golden/lib/`
3. If wrong, fix the ontology or templates

---

## Part 7: Understanding Configuration

### The 10 Rules

Each rule generates one artifact. In `ggen.toml`:

```toml
[[generation.rules]]
name = "zod-schemas"
query = "SELECT ?entityName ... WHERE { ... }"
template = "templates/zod-schemas.tera"
output_file = "lib/schemas/entities.mjs"
```

**The 10 rules generate:**
1. OpenAPI info section
2. OpenAPI component schemas
3. OpenAPI paths (endpoints)
4. Complete OpenAPI spec
5. JSDoc type definitions (entities)
6. JSDoc type definitions (requests)
7. Zod schemas (entities)
8. Zod schemas (requests)
9. Type guard functions
10. Barrel exports (index.mjs)

### Why Separate Rules?

Each rule is independent:
- Different query → different data
- Different template → different code structure
- Different output file → organized directory structure

You can:
- Add a new rule without changing others
- Modify a rule's query or template
- Remove rules you don't need

---

## Common Patterns

### SPARQL Pattern: Get All Entities

```sparql
?entity a api:Entity ;
        api:name ?entityName .
```

### SPARQL Pattern: Get Entity Properties

```sparql
?entity api:hasProperty ?property .
?property api:name ?propertyName ;
          api:type ?propertyType .
```

### SPARQL Pattern: Optional Data

```sparql
OPTIONAL { ?property api:required ?required }
OPTIONAL { ?property api:format ?format }
```

Use OPTIONAL when data might be missing for some entities.

### Template Pattern: Loop Through Results

```tera
{%- for row in sparql_results -%}
  {{ row["?entityName"] }}
{%- endfor %}
```

### Template Pattern: Conditional Output

```tera
{%- if row["?required"] == "true" -%}
  required
{%- endif %}
```

### Template Pattern: Filters

```tera
{{ row["?entityName"] | lower }}    {# lowercase #}
{{ row["?entityName"] | upper }}    {# uppercase #}
{{ row["?entityName"] | snake_case }} {# snake_case #}
```

---

## Troubleshooting

### Issue: SPARQL query returns no results

**Symptoms:** Generated files are empty or missing sections

**Diagnosis:**
1. Check ontology file exists and is valid Turtle
2. Verify entity/property names match query patterns
3. Test the query: `ggen query --file ontology/blog-api.ttl --query "..."`

**Solution:**
1. Ensure all RDF prefixes are defined
2. Check property predicates (api:hasProperty vs api:property)
3. Validate Turtle syntax with a SPARQL endpoint

### Issue: Template rendering failed

**Symptoms:** Error message with template line number

**Diagnosis:**
1. Check Tera syntax (loops, conditionals)
2. Verify variable names match SPARQL results
3. Check for undefined variables

**Solution:**
1. Add `| default("")` filters for optional variables
2. Check template documentation
3. Test template with sample data

### Issue: Generated files have wrong structure

**Symptoms:** Files exist but content is wrong

**Diagnosis:**
1. Check output paths in ggen.toml
2. Verify frontmatter YAML syntax
3. Compare with golden files

**Solution:**
1. Fix `to:` paths in templates
2. Validate YAML syntax
3. Review template logic

---

## Next Steps

1. ✅ **You've learned:** RDF → SPARQL → Templates → Code
2. 📖 **Read next:** [Configuration Explained](../../examples/openapi/CONFIGURATION_EXPLAINED.md) for details on each rule
3. 🔧 **Try it:** Modify the example (add a new property, add a new entity)
4. 📋 **Reference:** [SPARQL Query Patterns](../reference/sparql-query-patterns.md) for more patterns

---

## Summary

The ggen working loop:

```
Ontology (RDF)
    ↓
SPARQL Queries (extract what you need)
    ↓
Tera Templates (transform to code)
    ↓
Generated Artifacts (OpenAPI, Zod, JSDoc)
    ↓
Golden Files (verify consistency)
```

**Key insight:** By storing everything in RDF and generating from SPARQL queries, you ensure all outputs are synchronized. Change the ontology once, and all generated code updates automatically.

**Why this matters:** No more manual OpenAPI/TypeScript/Zod synchronization. No more copy-paste consistency bugs. One source of truth.
