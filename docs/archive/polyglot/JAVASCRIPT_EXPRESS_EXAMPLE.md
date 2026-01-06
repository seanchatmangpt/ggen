# JavaScript Express Backend Generator Example

> Generate production-grade Express.js APIs from RDF ontologies

## Overview

This example demonstrates generating a complete Express.js backend from an RDF ontology, including:

- TypeScript type definitions with Zod validation
- Express route handlers with proper error handling
- Middleware configuration
- Input validation
- API documentation

## Prerequisites

- Node.js 18+
- Express.js 4.x
- ggen CLI installed and configured
- RDF ontology defining your API domain

## Step 1: Define Your Domain Ontology

Create `ontology.ttl` defining your domain entities:

```turtle
@prefix ex: <http://example.org/api/>
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#>
@prefix xsd: <http://www.w3.org/2001/XMLSchema#>

# Define User entity
ex:User a rdfs:Class ;
  rdfs:label "User" ;
  rdfs:comment "System user account" .

ex:userId a rdf:Property ;
  rdfs:domain ex:User ;
  rdfs:range xsd:string ;
  rdfs:label "User ID" .

ex:userName a rdf:Property ;
  rdfs:domain ex:User ;
  rdfs:range xsd:string ;
  rdfs:label "Username" .

ex:userEmail a rdf:Property ;
  rdfs:domain ex:User ;
  rdfs:range xsd:string ;
  rdfs:label "Email" .

# Define endpoints
ex:GetUsersEndpoint a rdfs:Class ;
  rdfs:label "Get Users" ;
  rdfs:comment "List all users" ;
  ex:httpMethod "GET" ;
  ex:path "/users" ;
  ex:returnType "User" ;
  ex:returnCollection true .

ex:CreateUserEndpoint a rdfs:Class ;
  rdfs:label "Create User" ;
  rdfs:comment "Create new user" ;
  ex:httpMethod "POST" ;
  ex:path "/users" ;
  ex:bodyType "CreateUserInput" ;
  ex:returnType "User" .

ex:GetUserByIdEndpoint a rdfs:Class ;
  rdfs:label "Get User by ID" ;
  ex:httpMethod "GET" ;
  ex:path "/users/{id}" ;
  ex:paramType "id" "string" ;
  ex:returnType "User" .
```

## Step 2: Configure ggen Generation

Create `ggen.toml`:

```toml
[project]
name = "user-api-generator"
version = "1.0.0"

[ontology]
source = "ontology.ttl"

[generation]
output_dir = "generated/"

# Generate TypeScript types
[[generation.rules]]
name = "typescript-types"
description = "Generate TypeScript interfaces from RDF entities"
query = { inline = """
PREFIX ex: <http://example.org/api/>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
SELECT ?className ?propertyName ?propertyRange ?label
WHERE {
  ?class a rdfs:Class ;
    rdfs:label ?className .
  ?prop rdfs:domain ?class ;
    rdfs:label ?propertyName ;
    rdfs:range ?propertyRange .
}
ORDER BY ?className
""" }
template = { file = "templates/typescript-interface.tera" }
output_file = "types.ts"

# Generate Express routes
[[generation.rules]]
name = "express-routes"
description = "Generate Express route handlers"
query = { inline = """
PREFIX ex: <http://example.org/api/>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
SELECT ?endpointName ?method ?path ?bodyType ?returnType
WHERE {
  ?endpoint a rdfs:Class ;
    rdfs:label ?endpointName ;
    ex:httpMethod ?method ;
    ex:path ?path ;
    ex:returnType ?returnType .
  OPTIONAL { ?endpoint ex:bodyType ?bodyType }
}
""" }
template = { file = "templates/express-route.tera" }
output_file = "routes.ts"

# Generate request validators
[[generation.rules]]
name = "validators"
description = "Generate Zod validation schemas"
query = { inline = """
PREFIX ex: <http://example.org/api/>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
SELECT ?inputName ?fieldName ?fieldType
WHERE {
  ?input a rdfs:Class ;
    rdfs:label ?inputName .
  ?field rdfs:domain ?input ;
    rdfs:label ?fieldName ;
    rdfs:range ?fieldType .
}
""" }
template = { file = "templates/zod-validator.tera" }
output_file = "validators.ts"
```

## Step 3: Create Tera Templates

### Template: TypeScript Interfaces (templates/typescript-interface.tera)

```tera
// Generated TypeScript interfaces
// Auto-generated from ontology. Do NOT edit manually.

{% set prev_class = "" %}
{% for row in results %}
  {% if row.className != prev_class %}
    {% if prev_class != "" %}
}
{% endif %}
export interface {{ row.className }} {
  {% set prev_class = row.className %}
  {% endif %}
  {{ row.propertyName | camelCase }}: {{ row.propertyRange | mapType }};
{% endfor %}
}
```

### Template: Express Routes (templates/express-route.tera)

```tera
// Generated Express routes
// Auto-generated from ontology. Do NOT edit manually.

import express, { Request, Response } from 'express';
import { {% for row in results %}{{ row.returnType }}{{ if not @last }}, {% endif %}{% endfor %} } from './types';
import { {% for row in results %}validate{{ row.bodyType }}{{ if not @last }}, {% endif %}{% endfor %} } from './validators';

const router = express.Router();

{% for row in results %}
/**
 * @route {{ row.method }} {{ row.path }}
 * @returns {# row.returnType #}
 */
router.{{ row.method | lower }}('{{ row.path }}', async (req: Request, res: Response) => {
  try {
    {% if row.bodyType %}
    // Validate request body
    const validated = validate{{ row.bodyType }}.parse(req.body);

    {% endif %}
    // TODO: Implement handler logic
    // - Access validated inputs
    // - Perform business logic
    // - Return response

    res.json({ success: true });
  } catch (error) {
    console.error('Error in {{ row.endpointName }}:', error);
    res.status(500).json({ error: 'Internal server error' });
  }
});

{% endfor %}
export default router;
```

### Template: Zod Validators (templates/zod-validator.tera)

```tera
// Generated Zod validation schemas
// Auto-generated from ontology. Do NOT edit manually.

import { z } from 'zod';

{% for row in results %}
export const validate{{ row.inputName }} = z.object({
  {{ row.fieldName | camelCase }}: {{ row.fieldType | mapZodType }},
});

export type {{ row.inputName }} = z.infer<typeof validate{{ row.inputName }}>;

{% endfor %}
```

## Step 4: Run Generation

```bash
# Generate all files
ggen-cli render

# Check output
ls -la generated/
# types.ts
# routes.ts
# validators.ts
```

## Step 5: Create Express Application

Create `src/app.ts`:

```typescript
import express from 'express';
import { z } from 'zod';
import routes from '../generated/routes';
import * as validators from '../generated/validators';
import * as types from '../generated/types';

const app = express();

// Middleware
app.use(express.json());

// Error handling middleware
app.use((err: any, req: express.Request, res: express.Response, next: express.NextFunction) => {
  if (err instanceof z.ZodError) {
    res.status(400).json({
      error: 'Validation error',
      details: err.errors,
    });
    return;
  }

  console.error('Unexpected error:', err);
  res.status(500).json({ error: 'Internal server error' });
});

// Routes
app.use('/api', routes);

// Health check
app.get('/health', (req, res) => {
  res.json({ status: 'ok' });
});

export default app;
```

## Step 6: Example Implementation

Update `generated/routes.ts` with actual logic:

```typescript
// Example: Implement GetUsers endpoint
router.get('/users', async (req: Request, res: Response) => {
  try {
    // In production, fetch from database
    const users: User[] = [
      {
        userId: '1',
        userName: 'alice',
        userEmail: 'alice@example.com',
      },
      {
        userId: '2',
        userName: 'bob',
        userEmail: 'bob@example.com',
      },
    ];

    res.json({ data: users });
  } catch (error) {
    console.error('Error fetching users:', error);
    res.status(500).json({ error: 'Failed to fetch users' });
  }
});

// Example: Implement CreateUser endpoint
router.post('/users', async (req: Request, res: Response) => {
  try {
    // Validate input using generated validator
    const validated = validateCreateUserInput.parse(req.body);

    // Create user (example: in-memory storage)
    const newUser: User = {
      userId: Date.now().toString(),
      userName: validated.userName,
      userEmail: validated.userEmail,
    };

    // In production, save to database
    res.status(201).json({ data: newUser });
  } catch (error) {
    console.error('Error creating user:', error);
    res.status(500).json({ error: 'Failed to create user' });
  }
});
```

## Step 7: Testing

Create `tests/api.test.ts`:

```typescript
import request from 'supertest';
import app from '../src/app';

describe('User API', () => {
  describe('GET /users', () => {
    it('should return list of users', async () => {
      const response = await request(app).get('/api/users');

      expect(response.status).toBe(200);
      expect(Array.isArray(response.body.data)).toBe(true);
    });
  });

  describe('POST /users', () => {
    it('should create new user', async () => {
      const response = await request(app)
        .post('/api/users')
        .send({
          userName: 'charlie',
          userEmail: 'charlie@example.com',
        });

      expect(response.status).toBe(201);
      expect(response.body.data.userId).toBeDefined();
    });

    it('should validate required fields', async () => {
      const response = await request(app)
        .post('/api/users')
        .send({ userName: 'incomplete' }); // Missing email

      expect(response.status).toBe(400);
      expect(response.body.error).toBe('Validation error');
    });
  });
});
```

## Best Practices

### 1. Keep Ontology Current

Always update `ontology.ttl` when API changes:

```turtle
# Add BEFORE generating new routes
ex:UpdateUserEndpoint a rdfs:Class ;
  rdfs:label "Update User" ;
  ex:httpMethod "PUT" ;
  ex:path "/users/{id}" .

# Then regenerate
ggen-cli render
```

### 2. Layer Generated and Manual Code

```typescript
// Manual: Business logic
src/
  ├── handlers/       # Business logic (manual)
  ├── db/            # Database access (manual)
  └── middleware/    # Custom middleware (manual)

generated/
  ├── routes.ts      # Endpoint structure (generated)
  ├── types.ts       # Interfaces (generated)
  └── validators.ts  # Zod schemas (generated)
```

### 3. Version Your Ontology

```bash
git add ontology.ttl
git commit -m "feat(api): Add UpdateUser endpoint to ontology"

ggen-cli render
git add generated/
git commit -m "chore(codegen): Regenerate routes from updated ontology"
```

### 4. Test Generated Code

```bash
npm test

# Regenerate and test again to verify consistency
ggen-cli render
npm test
```

## Common Patterns

### Pattern 1: Resource CRUD

```turtle
ex:TodoItem a rdfs:Class .
ex:GetTodos a rdfs:Class ;
  ex:httpMethod "GET" ;
  ex:path "/todos" .
ex:CreateTodo a rdfs:Class ;
  ex:httpMethod "POST" ;
  ex:path "/todos" .
ex:GetTodo a rdfs:Class ;
  ex:httpMethod "GET" ;
  ex:path "/todos/{id}" .
ex:UpdateTodo a rdfs:Class ;
  ex:httpMethod "PUT" ;
  ex:path "/todos/{id}" .
ex:DeleteTodo a rdfs:Class ;
  ex:httpMethod "DELETE" ;
  ex:path "/todos/{id}" .
```

### Pattern 2: Authentication

```turtle
ex:AuthenticateEndpoint a rdfs:Class ;
  ex:httpMethod "POST" ;
  ex:path "/auth/login" ;
  ex:requires ex:NoAuth .

ex:ProtectedEndpoint a rdfs:Class ;
  ex:httpMethod "GET" ;
  ex:path "/protected" ;
  ex:requires ex:BearerToken .
```

### Pattern 3: Pagination

```turtle
ex:ListUsersEndpoint a rdfs:Class ;
  ex:httpMethod "GET" ;
  ex:path "/users" ;
  ex:queryParam "skip" "integer" ;
  ex:queryParam "take" "integer" ;
  ex:returnCollection true .
```

## Troubleshooting

### Problem: Generated types don't match API

**Solution**: Ensure ontology defines all properties

```turtle
# ❌ Missing properties
ex:User a rdfs:Class .
ex:userName a rdf:Property ; rdfs:domain ex:User .

# ✅ Complete definition
ex:User a rdfs:Class .
ex:userName a rdf:Property ; rdfs:domain ex:User ; rdfs:range xsd:string .
ex:userEmail a rdf:Property ; rdfs:domain ex:User ; rdfs:range xsd:string .
```

### Problem: Routes not generating

**Solution**: Check SPARQL query returns results

```bash
# Test query directly
ggen-cli query "SELECT ?class WHERE { ?class a rdfs:Class }"
```

### Problem: Validation always fails

**Solution**: Ensure Zod schema matches request format

```typescript
// Debug: Print validated object
console.log('Received:', req.body);
const validated = validateInput.parse(req.body);
console.log('Validated:', validated);
```

## See Also

- [GraphQL Deep Dive](./GRAPHQL_DEEP_DIVE.md) - GraphQL generation patterns
- [PostgreSQL Integration](./POSTGRESQL_INTEGRATION_EXAMPLE.md) - Database integration
- [ggen.toml Reference](../GGEN_TOML_REFERENCE.md) - Configuration options
