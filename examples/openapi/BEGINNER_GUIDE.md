# Beginner's Guide to the OpenAPI Example

This guide walks you through the OpenAPI example step-by-step, explaining each concept as we go.

## What You'll Learn

By the end of this guide, you'll understand:
- What RDF ontologies are and how they represent data
- How SPARQL queries extract data from ontologies
- How templates transform data into code
- How all the pieces fit together

## Step 1: Understanding RDF Ontologies

### What is RDF?

RDF (Resource Description Framework) represents data as **triples**: subject-predicate-object.

Example:
```
User hasProperty username
```

This means: "User has a property called username"

### Our Ontology Structure

Open `ontology/blog-api.ttl` and look at a simple entity:

```turtle
blog:User a api:Entity ;
    api:name "User" ;
    rdfs:comment "Blog user account" ;
    api:hasProperty blog:User_id .
```

**Breaking it down:**
- `blog:User` - The entity (subject)
- `a api:Entity` - It's an Entity type
- `api:name "User"` - Its name is "User"
- `api:hasProperty blog:User_id` - It has a property called User_id

### Try It Yourself

Look at the `blog:User_id` property definition:

```turtle
blog:User_id a api:Property ;
    api:name "id" ;
    api:type "string" ;
    api:required "true" .
```

**What does this mean?**
- Property name: `id`
- Type: `string`
- Required: `true` (must be present)

## Step 2: Understanding SPARQL Queries

### What is SPARQL?

SPARQL is a query language for RDF. It's like SQL for graphs.

### A Simple Query

Look at Rule 5 in `ggen.toml`:

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
```

**What this does:**
1. **SELECT** - Choose what data to return (`?entityName`, `?propertyName`, etc.)
2. **WHERE** - Define the pattern to match
3. **Pattern**: Find entities, get their names and properties
4. **OPTIONAL** - Include `required` if it exists (might be missing)

### Query Results

This query returns rows like:
```
entityName: "User"
propertyName: "id"
propertyType: "string"
required: "true"
```

### Try It Yourself

1. Open `ggen.toml`
2. Find Rule 7 (zod-schemas)
3. Read the SPARQL query
4. Can you see what data it extracts?

## Step 3: Understanding Templates

### What are Templates?

Templates are files that define how to transform SPARQL results into code.

### Template Structure

Every template has two parts:

1. **YAML Frontmatter** (between `---` markers):
   ```yaml
   ---
   to: lib/schemas/entities.mjs
   description: Generates Zod validation schemas
   ---
   ```

2. **Template Body** (Tera syntax):
   ```tera
   export const {{ entityName | lower }}Schema = z.object({
     {{ propertyName }}: z.string(),
   });
   ```

### How Templates Work

1. SPARQL query runs → produces `sparql_results`
2. Template loops through results:
   ```tera
   {%- for row in sparql_results -%}
     {{ row["?entityName"] }}
   {%- endfor %}
   ```
3. Variables are substituted → generates code

### Try It Yourself

1. Open `templates/zod-schemas.tera`
2. Find the loop: `{%- for row in sparql_results -%}`
3. See how it uses `row["?entityName"]` and `row["?propertyName"]`
4. Can you trace how it generates the Zod schema?

## Step 4: The Complete Flow

### Step-by-Step Process

1. **You run**: `ggen sync`

2. **ggen loads**: `ontology/blog-api.ttl`

3. **For each rule in `ggen.toml`**:
   - Run the SPARQL query
   - Get results
   - Pass to template
   - Render template
   - Write to output file

4. **Result**: Generated files in `lib/`

### Example: Generating a Zod Schema

**Input (Ontology)**:
```turtle
blog:User a api:Entity ;
    api:name "User" ;
    api:hasProperty blog:User_email .
blog:User_email a api:Property ;
    api:name "email" ;
    api:type "string" .
```

**SPARQL Query** extracts:
```
entityName: "User"
propertyName: "email"
propertyType: "string"
```

**Template** generates:
```javascript
export const userSchema = z.object({
  email: z.string(),
});
```

**Output**: `lib/schemas/entities.mjs`

## Step 5: Running the Example

### First Time Setup

```bash
# Navigate to example directory
cd examples/openapi

# Generate all outputs
ggen sync
```

### What Happens

1. ggen reads `ggen.toml`
2. For each of 10 rules:
   - Loads ontology
   - Runs SPARQL query
   - Renders template
   - Writes output file
3. Creates `lib/` directory with all generated files

### Verify It Worked

```bash
# Check output directory exists
ls -la lib/

# Should see:
# - schemas/
# - types/
# - guards/
# - openapi/
# - index.mjs
```

## Step 6: Understanding the Output

### Directory Structure

```
lib/
├── schemas/          # Validation schemas (Zod)
│   ├── entities.mjs # Entity schemas
│   └── requests.mjs # Request schemas
├── types/            # Type definitions (JSDoc)
│   ├── entities.mjs # Entity types
│   └── requests.mjs # Request types
├── guards/           # Type guards
│   └── entities.mjs # Guard functions
└── openapi/          # OpenAPI specs
    ├── openapi.yaml # Complete spec
    └── ...
```

### What Each File Does

**`lib/schemas/entities.mjs`**:
- Zod schemas for validation
- Use in API routes to validate requests/responses

**`lib/types/entities.mjs`**:
- JSDoc type definitions
- Provides IDE autocomplete and type checking

**`lib/guards/entities.mjs`**:
- Runtime type checking functions
- Use to verify data structure at runtime

**`lib/openapi/openapi.yaml`**:
- Complete OpenAPI 3.0 specification
- Use for API documentation and client generation

## Step 7: Modifying the Example

### Add a New Property

1. **Edit** `ontology/blog-api.ttl`
2. **Add** a new property to User:
   ```turtle
   blog:User_avatar a api:Property ;
       api:name "avatar" ;
       api:type "string" ;
       api:format "url" .
   ```
3. **Add** to User entity:
   ```turtle
   blog:User api:hasProperty blog:User_avatar .
   ```
4. **Run** `ggen sync`
5. **Check** `lib/schemas/entities.mjs` - should include `avatar` field

### Add a New Entity

1. **Edit** `ontology/blog-api.ttl`
2. **Add** entity definition:
   ```turtle
   blog:Category a api:Entity ;
       api:name "Category" ;
       rdfs:comment "Post category" ;
       api:hasProperty blog:Category_id ;
       api:hasProperty blog:Category_name .
   ```
3. **Add** properties:
   ```turtle
   blog:Category_id a api:Property ;
       api:name "id" ;
       api:type "string" .
   blog:Category_name a api:Property ;
       api:name "name" ;
       api:type "string" .
   ```
4. **Run** `ggen sync`
5. **Verify** all outputs include Category

## Step 8: Understanding the Configuration

### The 10 Rules

Each rule in `ggen.toml` generates one type of output:

1. **Rule 1**: OpenAPI info section
2. **Rule 2**: OpenAPI component schemas
3. **Rule 3**: OpenAPI paths (endpoints)
4. **Rule 4**: Combined OpenAPI spec
5. **Rule 5**: JSDoc type definitions (entities)
6. **Rule 6**: JSDoc type definitions (requests)
7. **Rule 7**: Zod validation schemas (entities)
8. **Rule 8**: Zod validation schemas (requests)
9. **Rule 9**: Type guard functions
10. **Rule 10**: Barrel export (index.mjs)

### Rule Structure

Each rule has:
- **name**: Identifier for the rule
- **query**: SPARQL query to extract data
- **template**: Template file to render
- **output_file**: Where to write the result

See `CONFIGURATION_EXPLAINED.md` for detailed explanations.

## Next Steps

1. **Read** `CONFIGURATION_EXPLAINED.md` for rule details
2. **Experiment** with modifying the ontology
3. **Try** adding a new entity or property
4. **Use** the generated code in a real project

## Common Questions

**Q: Why use RDF instead of JSON/YAML?**
A: RDF provides semantic meaning and can be queried with SPARQL. It's a single source of truth that can generate multiple outputs.

**Q: Do I need to know SPARQL?**
A: Basic understanding helps, but you can copy/modify existing queries. See SPARQL tutorials for more.

**Q: Can I use TypeScript instead of JSDoc?**
A: This example uses JSDoc for ES modules (.mjs). You could modify templates to generate .ts files if needed.

**Q: How do I debug template errors?**
A: Check the template syntax, verify SPARQL results match expectations, and inspect generated output for clues.

## Resources

- [RDF Primer](https://www.w3.org/TR/rdf11-primer/)
- [SPARQL Tutorial](https://www.w3.org/2009/Talks/0615-qbe/)
- [Tera Template Documentation](https://keats.github.io/tera/)
- [Zod Documentation](https://zod.dev/)
- [JSDoc Documentation](https://jsdoc.app/)

