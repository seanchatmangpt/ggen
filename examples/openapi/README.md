# OpenAPI/JavaScript/Zod Code Generation Example

Generate synchronized API contracts from RDF ontology using `ggen sync`.

## Overview

This example demonstrates generating three synchronized artifacts from a single RDF source:

1. **OpenAPI 3.0 specification** - REST API documentation
2. **Zod validation schemas** - Runtime validation with JSDoc types
3. **JavaScript type definitions (JSDoc)** - Type checking via JSDoc (ES modules)

All outputs are 100% consistent because they derive from the same ontology.

## Prerequisites

### Required Knowledge

Before starting, you should be familiar with:

- **Basic RDF/Turtle syntax** - Understanding how RDF represents data as triples
  - [RDF Primer](https://www.w3.org/TR/rdf11-primer/)
  - [Turtle Syntax](https://www.w3.org/TR/turtle/)
- **SPARQL basics** - How to query RDF data (SELECT queries)
  - [SPARQL 1.1 Query Language](https://www.w3.org/TR/sparql11-query/)
- **JavaScript ES modules** - Modern JavaScript with `import`/`export`
  - [MDN: ES Modules](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Modules)
- **Zod validation** - Runtime schema validation library
  - [Zod Documentation](https://zod.dev/)
- **OpenAPI 3.0** - REST API specification format (optional, but helpful)
  - [OpenAPI Specification](https://swagger.io/specification/)

### Required Tools

- `ggen` CLI installed and in PATH
- Node.js 18+ (for running generated JavaScript)
- Basic command-line familiarity

### Quick Check

```bash
# Verify ggen is installed
ggen --version

# Should output: ggen 5.0.0 (or later)
```

## Quick Start

```bash
# Generate all outputs
cd examples/openapi
ggen sync

# Output files in ./lib:
# - openapi/openapi.yaml     (OpenAPI 3.0 spec)
# - schemas/entities.mjs     (Zod validation + JSDoc types)
# - types/entities.mjs       (JSDoc type definitions)
# - guards/entities.mjs      (Type guard functions)
```

**Note**: This example uses the `ggen.toml` format with `generation.rules`. The current `ggen sync` CLI may use a different interface (`--from`/`--to`). The example structure is ready for when full `ggen.toml` support is available. You can test the templates and structure manually, or use the ggen core library directly.

## Project Structure

```
examples/openapi/
├── ggen.toml                 # Generation manifest (10 rules)
├── ontology/
│   ├── api-schema.ttl        # API vocabulary schema
│   └── blog-api.ttl          # Blog API instance data
├── templates/
│   ├── openapi-*.tera        # OpenAPI templates (with YAML frontmatter)
│   ├── zod-*.tera            # Zod schema templates (with YAML frontmatter)
│   └── typescript-*.tera     # Type definition templates (with YAML frontmatter)
├── golden/                   # Expected outputs (for testing)
│   └── lib/                  # Mirrors generated lib/ structure
│       ├── schemas/
│       ├── types/
│       ├── guards/
│       └── openapi/
└── lib/                      # Generated outputs (created by ggen sync)
    ├── schemas/
    │   ├── entities.mjs      # Entity validation schemas
    │   ├── requests.mjs      # Request validation schemas
    │   └── index.mjs         # Barrel export
    ├── types/
    │   ├── entities.mjs      # Entity type definitions (JSDoc)
    │   ├── requests.mjs      # Request type definitions (JSDoc)
    │   └── index.mjs         # Barrel export
    ├── guards/
    │   ├── entities.mjs      # Type guard functions
    │   └── index.mjs         # Barrel export
    ├── openapi/
    │   ├── openapi.yaml      # Complete OpenAPI spec
    │   ├── schemas.yaml      # Component schemas
    │   ├── paths.yaml        # API paths
    │   └── api-info.yaml     # API info section
    └── index.mjs             # Main barrel export
```

## Understanding the Example

### How It Works

1. **RDF Ontology** (`ontology/blog-api.ttl`) defines your domain:
   - Entities (User, Post, Comment, Tag)
   - Properties (id, username, email, etc.)
   - Endpoints (GET /users, POST /users, etc.)

2. **SPARQL Queries** (in `ggen.toml`) extract data:
   - Each rule has a SPARQL query that selects specific data
   - Results are passed to templates as `sparql_results`

3. **Templates** (`.tera` files) transform data to code:
   - Tera template syntax with YAML frontmatter
   - Frontmatter defines output path and metadata
   - Template body generates the actual code

4. **Generated Output** (`lib/`) contains:
   - OpenAPI specs for API documentation
   - Zod schemas for runtime validation
   - JSDoc types for IDE support
   - Type guards for runtime checking

### Key Concepts

- **Single Source of Truth**: The RDF ontology is the only place you define your domain
- **Deterministic**: Same ontology + templates = identical output
- **Type-Safe**: JSDoc types provide IDE autocomplete and type checking
- **Validated**: Zod schemas ensure runtime data matches the ontology

## Integration with Next.js BFF

The generated schemas are designed to integrate with Next.js BFF patterns.

### Pattern: Ontology → ggen sync → Next.js BFF

```
┌─────────────────┐      ┌──────────────┐      ┌─────────────────┐
│  RDF Ontology   │ ──── │  ggen sync   │ ──── │  Next.js BFF    │
│  (blog-api.ttl) │      │              │      │                 │
└─────────────────┘      │ OpenAPI.yaml │      │ API Routes      │
                         │ schemas.mjs  │ ──── │ Validation      │
                         │ types.mjs    │      │ TanStack DB     │
                         └──────────────┘      └─────────────────┘
```

### Using Generated Schemas in Next.js

**1. API Route Handler (validation)**

```javascript
// app/api/users/route.mjs
import { createUserRequestSchema } from '@/lib/schemas/requests.mjs';

export async function POST(request) {
  const body = await request.json();

  // Validate request using generated Zod schema
  const result = createUserRequestSchema.safeParse(body);
  if (!result.success) {
    return Response.json(
      { errors: result.error.flatten().fieldErrors },
      { status: 400 }
    );
  }

  // Create user...
  const user = await db.users.create(result.data);

  // Return validated data
  return Response.json(user);
}
```

**2. TanStack DB Collection (with schema)**

```javascript
// collections/users.mjs
import { createCollection } from '@tanstack/electric-db-collection';
import { userSchema } from '@/lib/schemas/entities.mjs';

export const usersCollection = createCollection({
  name: 'users',
  schema: userSchema,
  primaryKey: 'id',
});
```

**3. React Component (with types)**

```jsx
// components/UserCard.jsx
import { userSchema } from '@/lib/schemas/entities.mjs';

/**
 * @param {{ user: import('@/lib/types/entities.mjs').User }} props
 */
export function UserCard({ user }) {
  // Validation at boundary
  const validated = userSchema.parse(user);

  return (
    <div>
      <h2>{validated.username}</h2>
      <p>{validated.email}</p>
    </div>
  );
}
```

## Ontology Design

The `blog-api.ttl` ontology defines:

- **Entities**: User, Post, Comment, Tag
- **Properties**: id, username, email, bio, title, content, etc.
- **Endpoints**: CRUD operations for each entity
- **Request Types**: CreateUserRequest, UpdateUserRequest, etc.

### Adding New Entities

1. Add entity definition to `ontology/blog-api.ttl`:

```turtle
blog:NewEntity a api:Entity ;
    api:name "NewEntity" ;
    rdfs:comment "Description" ;
    api:hasProperty blog:NewEntity_id ;
    api:hasEndpoint blog:NewEntity_list .
```

2. Run `ggen sync` to regenerate all outputs

3. All outputs (OpenAPI, Zod, JSDoc types) are automatically updated

## SPARQL Queries

The `ggen.toml` manifest uses SPARQL queries to extract data from the ontology. Each rule has a query that selects specific data:

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

See `CONFIGURATION_EXPLAINED.md` for detailed explanations of each query.

## Testing

Compare generated output against golden files:

```bash
# Generate outputs
ggen sync

# Verify output matches expected (using verification script)
./verify.sh

# Or manually compare
diff -r lib/ golden/lib/
```

## Verification

Use the included validation script to compare generated output with golden files:

```bash
# Run validation (JavaScript/Node.js)
node validate.mjs
# or
./validate.mjs

# Should output: ✅ All files match golden files
```

**Alternative**: Use the shell script:
```bash
./verify.sh
```

The validation script compares:
- All `.mjs` files in `lib/` with `golden/lib/`
- All `.yaml` files in `lib/openapi/` with `golden/lib/openapi/`
- Reports any differences, missing files, and extra files
- Provides diff commands for mismatches

## Configuration Options

Key `ggen.toml` settings:

```toml
[ontology]
source = "ontology/blog-api.ttl"    # Input ontology
base_iri = "https://ggen.io/..."    # Namespace prefix

[generation]
output_dir = "lib"                  # Output directory (Node.js best practice)
require_audit_trail = true          # Track provenance
```

See `CONFIGURATION_EXPLAINED.md` for detailed explanations of all 10 generation rules.

## Troubleshooting

### Common Errors

**Error: Templates directory not found**
- **Cause**: Running `ggen sync` from wrong directory
- **Solution**: Ensure you're in `examples/openapi/` directory

**Error: SPARQL query returned no results**
- **Cause**: Ontology doesn't match query expectations
- **Solution**: 
  1. Check ontology file exists and is valid Turtle
  2. Verify entity names match query patterns
  3. Check SPARQL query syntax in `ggen.toml`

**Error: Template rendering failed**
- **Cause**: Template syntax error or missing variable
- **Solution**:
  1. Check template file for syntax errors
  2. Verify all required variables are provided
  3. Check Tera template documentation

**Generated files have wrong structure**
- **Cause**: Output paths in `ggen.toml` don't match directory structure
- **Solution**: Verify `output_file` paths match desired structure

### Debugging SPARQL Queries

1. Test queries manually:
   ```bash
   # Use a SPARQL query tool or ggen's query feature
   ggen query --file ontology/blog-api.ttl --query "YOUR_QUERY_HERE"
   ```

2. Check query results:
   - Verify variables are bound correctly
   - Ensure OPTIONAL clauses work as expected
   - Check ORDER BY doesn't affect results

### Debugging Template Rendering

1. Check template syntax:
   - Verify YAML frontmatter is valid
   - Check Tera syntax (loops, conditionals)
   - Ensure variable names match SPARQL results

2. Inspect generated output:
   - Look for empty files (template didn't render)
   - Check for syntax errors in generated code
   - Verify imports/exports are correct

## Learning Path

1. **Start Here**: Read `BEGINNER_GUIDE.md` for step-by-step walkthrough
2. **Understand Configuration**: Read `CONFIGURATION_EXPLAINED.md` for rule details
3. **Modify Example**: Try adding a new entity to the ontology
4. **Create Your Own**: Use this as a template for your own API

## Related Examples

- `examples/basic-template-generation/` - Learn template fundamentals
- `examples/thesis-gen/` - LaTeX generation from thesis ontology
- `examples/microservices/` - Multi-service code generation
