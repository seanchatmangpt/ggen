# OpenAPI/TypeScript/Zod Code Generation Example

Generate synchronized API contracts from RDF ontology using `ggen sync`.

## Overview

This example demonstrates generating three synchronized artifacts from a single RDF source:

1. **OpenAPI 3.0 specification** - REST API documentation
2. **Zod validation schemas** - Runtime validation with JSDoc types
3. **TypeScript type definitions** - Static type checking

All outputs are 100% consistent because they derive from the same ontology.

## Quick Start

```bash
# Generate all outputs
cd examples/openapi
ggen sync

# Output files in ./output:
# - openapi.yaml     (OpenAPI 3.0 spec)
# - schemas.js       (Zod validation + JSDoc types)
# - types.d.ts       (TypeScript definitions)
```

## Project Structure

```
examples/openapi/
├── ggen.toml                 # Generation manifest (10 rules)
├── ontology/
│   ├── api-schema.ttl        # API vocabulary schema
│   └── blog-api.ttl          # Blog API instance data
├── templates/
│   ├── openapi-*.tera        # OpenAPI templates
│   ├── zod-*.tera            # Zod schema templates
│   └── typescript-*.tera     # TypeScript templates
├── golden/                   # Expected outputs (for testing)
│   ├── openapi.yaml
│   ├── schemas.js
│   └── types.d.ts
└── output/                   # Generated outputs
```

## Integration with Next.js BFF

The generated schemas are designed to integrate with Next.js BFF patterns like [@astro/tanstack-db](https://github.com/example/astro).

### Pattern: Ontology → ggen sync → Next.js BFF

```
┌─────────────────┐      ┌──────────────┐      ┌─────────────────┐
│  RDF Ontology   │ ──── │  ggen sync   │ ──── │  Next.js BFF    │
│  (blog-api.ttl) │      │              │      │                 │
└─────────────────┘      │ OpenAPI.yaml │      │ API Routes      │
                         │ schemas.js   │ ──── │ Validation      │
                         │ types.d.ts   │      │ TanStack DB     │
                         └──────────────┘      └─────────────────┘
```

### Using Generated Schemas in Next.js

**1. API Route Handler (validation)**

```javascript
// app/api/users/route.js
import { createUserRequestSchema, parseUser } from '@/generated/schemas';

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

  // Validate response using generated schema
  return Response.json(parseUser(user).data);
}
```

**2. TanStack DB Collection (with schema)**

```javascript
// collections/users.js
import { createCollection } from '@tanstack/electric-db-collection';
import { userSchema } from '@/generated/schemas';

export const usersCollection = createCollection({
  name: 'users',
  schema: userSchema,
  primaryKey: 'id',
});
```

**3. React Component (with types)**

```jsx
// components/UserCard.jsx
import { userSchema } from '@/generated/schemas';

/**
 * @param {{ user: import('@/generated/types').User }} props
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

3. All three outputs (OpenAPI, Zod, TypeScript) are automatically updated

## SPARQL Queries

The `ggen.toml` manifest uses SPARQL queries to extract data from the ontology:

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

## Testing

Compare generated output against golden files:

```bash
# Generate outputs
ggen sync

# Compare with golden files
diff output/openapi.yaml golden/openapi.yaml
diff output/schemas.js golden/schemas.js
diff output/types.d.ts golden/types.d.ts
```

## Configuration Options

Key `ggen.toml` settings:

```toml
[ontology]
source = "ontology/blog-api.ttl"    # Input ontology
base_iri = "https://ggen.io/..."    # Namespace prefix

[generation]
output_dir = "output"               # Output directory
require_audit_trail = true          # Track provenance
```

## Related Examples

- `examples/thesis-gen/` - LaTeX generation from thesis ontology
- `examples/microservices/` - Multi-service code generation
