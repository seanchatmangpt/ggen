# Next.js + OpenAPI + SQLite + shadcn/ui + Vitest Example

Generate a complete full-stack Task Management application from RDF ontology.

## Overview

This example demonstrates ggen generating:

- **OpenAPI 3.0 spec** - REST API documentation
- **Drizzle ORM schema** - SQLite database with type safety
- **Next.js API routes** - Type-safe handlers with Zod validation
- **TypeScript types** - Interfaces for all entities
- **Zod schemas** - Runtime validation
- **shadcn/ui components** - Cards, Lists, Forms
- **Vitest tests** - Unit and integration tests

All outputs are **100% consistent** because they derive from the same ontology.

## Quick Start

```bash
cd examples/nextjs-openapi-sqlite-shadcn-vitest
ggen sync

# Output in lib/:
# - openapi/openapi.yaml
# - db/schema.ts
# - api/routes.ts
# - types/api.ts
# - schemas/index.ts
# - components/generated.tsx
# - tests/unit.test.ts
# - tests/integration.test.ts
```

## Project Structure

```
nextjs-openapi-sqlite-shadcn-vitest/
├── ggen.toml                    # Generation manifest (10 rules)
├── ontology/
│   ├── api-schema.ttl          # Vocabulary definitions
│   └── taskapp-api.ttl         # Task app instance data
├── templates/
│   ├── openapi-spec.tera       # OpenAPI 3.0 template
│   ├── drizzle-schema.tera     # Drizzle ORM template
│   ├── api-routes.tera         # Next.js API handlers
│   ├── api-types.tera          # TypeScript interfaces
│   ├── zod-schemas.tera        # Validation schemas
│   ├── shadcn-components.tera  # React components
│   ├── page-components.tera    # Next.js pages
│   ├── vitest-unit.tera        # Unit tests
│   ├── vitest-integration.tera # Integration tests
│   └── index-exports.tera      # Barrel exports
├── lib/                         # Generated output (gitignored)
├── golden/lib/                  # Expected output for validation
├── verify.sh                    # Shell validation script
├── validate.mjs                 # Node.js validation script
└── README.md
```

## Entities Defined

| Entity | Fields | Endpoints |
|--------|--------|-----------|
| **User** | id, name, email, avatar, createdAt | 5 (CRUD) |
| **Task** | id, title, description, status, priority, dueDate, userId, projectId, createdAt | 5 (CRUD) |
| **Project** | id, name, description, color, userId, createdAt | 5 (CRUD) |
| **Tag** | id, name, color | 5 (CRUD) |

## Generated Files

### 1. OpenAPI Spec (`lib/openapi/openapi.yaml`)
Complete REST API documentation with:
- All CRUD endpoints
- Request/response schemas
- Parameters and status codes

### 2. Database Schema (`lib/db/schema.ts`)
Drizzle ORM tables with:
- Type-safe column definitions
- Primary keys, foreign keys
- TypeScript inference

### 3. API Routes (`lib/api/routes.ts`)
Next.js handlers with:
- Zod validation
- Error handling
- CRUD operations

### 4. Zod Schemas (`lib/schemas/index.ts`)
Validation schemas with:
- Field constraints
- Create/Update variants
- Type inference

### 5. Components (`lib/components/generated.tsx`)
shadcn/ui components:
- TaskCard, TaskList, TaskForm
- ProjectCard, ProjectList
- Header, Sidebar

## Verification

```bash
# Compare generated output with golden files
./verify.sh

# Or with Node.js
node validate.mjs
```

## Customization

### Add New Entity

1. Edit `ontology/taskapp-api.ttl`:
```turtle
task:Comment a napi:Entity ;
    napi:name "Comment" ;
    napi:hasProperty task:Comment_id, task:Comment_content .
```

2. Regenerate:
```bash
ggen sync
```

### Modify Validation

Edit `templates/zod-schemas.tera` to customize validation rules.

## Technology Stack

- **Next.js 14** - React framework
- **TypeScript** - Type safety
- **SQLite** - Database
- **Drizzle ORM** - Type-safe ORM
- **Zod** - Validation
- **shadcn/ui** - Components
- **Vitest** - Testing

## See Also

- [EXAMPLE_CREATION_GUIDE.md](/docs/EXAMPLE_CREATION_GUIDE.md) - Dos and don'ts
- [examples/openapi/](../openapi/) - Reference implementation
