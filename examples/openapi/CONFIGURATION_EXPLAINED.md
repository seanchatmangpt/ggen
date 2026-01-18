# Configuration Explained

This document explains each of the 10 generation rules in `ggen.toml`.

## Rule Overview

The configuration uses 10 rules to generate:
- 4 OpenAPI files (info, schemas, paths, combined)
- 2 JavaScript type definition files (entities, requests)
- 2 Zod schema files (entities, requests)
- 1 Type guard file
- 1 Index file (barrel exports)

## Rule 1: OpenAPI Info Section

**Purpose**: Generates the OpenAPI specification header with API metadata

**Output**: `lib/openapi/api-info.yaml`

**SPARQL Query**:
```sparql
SELECT ?title ?description ?version ?serverUrl ?serverDescription
WHERE {
  ?spec a api:Specification ;
        api:title ?title ;
        api:version ?version .
  OPTIONAL { ?spec api:description ?description }
  OPTIONAL {
    ?spec api:server ?server .
    ?server api:url ?serverUrl .
    OPTIONAL { ?server api:description ?serverDescription }
  }
}
LIMIT 1
```

**What it extracts**:
- API title, description, version
- Server URL and description

**Template**: `templates/openapi-info.tera`

**Example Output**:
```yaml
openapi: "3.0.0"
info:
  title: Blog API
  description: A complete blog API
  version: 1.0.0
servers:
  - url: http://localhost:3000
    description: Local development server
```

## Rule 2: OpenAPI Component Schemas

**Purpose**: Generates OpenAPI component schemas for all entities

**Output**: `lib/openapi/schemas.yaml`

**SPARQL Query**:
```sparql
SELECT ?entityName ?entityDescription ?propertyName ?propertyType
       ?required ?format ?minLength ?maxLength ?pattern ?example ?refEntity
WHERE {
  ?entity a api:Entity ;
          api:name ?entityName .
  OPTIONAL { ?entity rdfs:comment ?entityDescription }
  ?entity api:hasProperty ?property .
  ?property api:name ?propertyName ;
            api:type ?propertyType .
  OPTIONAL { ?property api:required ?required }
  OPTIONAL { ?property api:format ?format }
  OPTIONAL { ?property api:minLength ?minLength }
  OPTIONAL { ?property api:maxLength ?maxLength }
  OPTIONAL { ?property api:pattern ?pattern }
  OPTIONAL { ?property api:example ?example }
  OPTIONAL { ?property api:refEntity ?refEntity }
}
ORDER BY ?entityName ?propertyName
```

**What it extracts**:
- All entities and their properties
- Property types, constraints, and validation rules
- References to other entities

**Template**: `templates/openapi-schemas.tera`

**Example Output**:
```yaml
User:
  type: object
  description: Blog user account
  properties:
    id:
      type: string
      minLength: 1
    email:
      type: string
      format: email
```

## Rule 3: OpenAPI Paths

**Purpose**: Generates REST API endpoint definitions

**Output**: `lib/openapi/paths.yaml`

**SPARQL Query**:
```sparql
SELECT ?entityName ?path ?method ?operationId ?summary ?description
       ?requestSchema ?responseSchema ?statusCode
WHERE {
  ?entity a api:Entity ;
          api:name ?entityName ;
          api:hasEndpoint ?endpoint .
  ?endpoint api:path ?path ;
            api:method ?method ;
            api:operationId ?operationId .
  OPTIONAL { ?endpoint api:summary ?summary }
  OPTIONAL { ?endpoint api:description ?description }
  OPTIONAL { ?endpoint api:requestSchema ?requestSchema }
  OPTIONAL { ?endpoint api:responseSchema ?responseSchema }
  OPTIONAL { ?endpoint api:statusCode ?statusCode }
}
ORDER BY ?entityName ?path ?method
```

**What it extracts**:
- API endpoints (paths and HTTP methods)
- Operation IDs, summaries, descriptions
- Request/response schemas
- Status codes

**Template**: `templates/openapi-paths.tera`

**Example Output**:
```yaml
/users:
  get:
    operationId: listUsers
    summary: List all users
    responses:
      '200':
        description: Success
        content:
          application/json:
            schema:
              type: array
              items:
                $ref: '#/components/schemas/User'
```

## Rule 4: Combined OpenAPI Specification

**Purpose**: Merges all OpenAPI components into a complete specification

**Output**: `lib/openapi/openapi.yaml`

**SPARQL Query**:
```sparql
SELECT ?title ?version
WHERE {
  ?spec a api:Specification ;
        api:title ?title ;
        api:version ?version .
}
LIMIT 1
```

**What it extracts**:
- Basic spec metadata (title, version)

**Template**: `templates/openapi-combined.tera`

**Note**: This is a stub template. In a full implementation, it would include the schemas and paths from Rules 2 and 3.

## Rule 5: JavaScript Type Definitions (Entities)

**Purpose**: Generates JSDoc type definitions for entities

**Output**: `lib/types/entities.mjs`

**SPARQL Query**:
```sparql
SELECT ?entityName ?entityDescription ?propertyName ?propertyType
       ?required ?isArray ?refEntity
WHERE {
  ?entity a api:Entity ;
          api:name ?entityName .
  OPTIONAL { ?entity rdfs:comment ?entityDescription }
  ?entity api:hasProperty ?property .
  ?property api:name ?propertyName ;
            api:type ?propertyType .
  OPTIONAL { ?property api:required ?required }
  OPTIONAL { ?property api:isArray ?isArray }
  OPTIONAL { ?property api:refEntity ?refEntity }
}
ORDER BY ?entityName ?propertyName
```

**What it extracts**:
- Entity names and descriptions
- Property names, types, and optionality
- Array and reference information

**Template**: `templates/typescript-interfaces.tera`

**Example Output**:
```javascript
/**
 * User - Blog user account
 * @typedef {Object} User
 * @property {string} id - id
 * @property {string} email - email
 * @property {string} ?username - username
 */
```

## Rule 6: JavaScript Type Definitions (Requests)

**Purpose**: Generates JSDoc type definitions for API request/response types

**Output**: `lib/types/requests.mjs`

**SPARQL Query**:
```sparql
SELECT ?entityName ?operationType ?propertyName ?propertyType ?required
WHERE {
  ?entity a api:Entity ;
          api:name ?entityName ;
          api:hasRequestType ?reqType .
  ?reqType api:operationType ?operationType ;
           api:hasProperty ?property .
  ?property api:name ?propertyName ;
            api:type ?propertyType .
  OPTIONAL { ?property api:required ?required }
}
ORDER BY ?entityName ?operationType ?propertyName
```

**What it extracts**:
- Request types for create/update operations
- Properties specific to requests (may differ from entity properties)

**Template**: `templates/typescript-api-types.tera`

**Example Output**:
```javascript
/**
 * CreateUserRequest
 * @typedef {Object} CreateUserRequest
 * @property {string} email - email
 * @property {string} username - username
 * @property {string} ?bio - bio
 */
```

## Rule 7: Zod Validation Schemas (Entities)

**Purpose**: Generates Zod runtime validation schemas for entities

**Output**: `lib/schemas/entities.mjs`

**SPARQL Query**:
```sparql
SELECT ?entityName ?entityDescription ?propertyName ?propertyType
       ?required ?minLength ?maxLength ?pattern ?min ?max ?format ?refEntity
WHERE {
  ?entity a api:Entity ;
          api:name ?entityName .
  OPTIONAL { ?entity rdfs:comment ?entityDescription }
  ?entity api:hasProperty ?property .
  ?property api:name ?propertyName ;
            api:type ?propertyType .
  OPTIONAL { ?property api:required ?required }
  OPTIONAL { ?property api:minLength ?minLength }
  OPTIONAL { ?property api:maxLength ?maxLength }
  OPTIONAL { ?property api:pattern ?pattern }
  OPTIONAL { ?property api:min ?min }
  OPTIONAL { ?property api:max ?max }
  OPTIONAL { ?property api:format ?format }
  OPTIONAL { ?property api:refEntity ?refEntity }
}
ORDER BY ?entityName ?propertyName
```

**What it extracts**:
- All entity properties with validation constraints
- Min/max length, patterns, formats
- Required/optional flags

**Template**: `templates/zod-schemas.tera`

**Example Output**:
```javascript
export const userSchema = z.object({
  id: z.string().min(1),
  email: z.string().email("Must be a valid email address"),
  username: z.string().min(1).max(255),
  bio: z.string().max(500).optional().nullable(),
});
```

## Rule 8: Zod Request Validation Schemas

**Purpose**: Generates Zod validation schemas for API request bodies

**Output**: `lib/schemas/requests.mjs`

**SPARQL Query**:
```sparql
SELECT ?entityName ?operationType ?propertyName ?propertyType
       ?required ?minLength ?maxLength ?pattern
WHERE {
  ?entity a api:Entity ;
          api:name ?entityName ;
          api:hasRequestType ?reqType .
  ?reqType api:operationType ?operationType ;
           api:hasProperty ?property .
  ?property api:name ?propertyName ;
            api:type ?propertyType .
  OPTIONAL { ?property api:required ?required }
  OPTIONAL { ?property api:minLength ?minLength }
  OPTIONAL { ?property api:maxLength ?maxLength }
  OPTIONAL { ?property api:pattern ?pattern }
}
ORDER BY ?entityName ?operationType ?propertyName
```

**What it extracts**:
- Request-specific properties (may exclude read-only fields like `id`)
- Validation rules for request data

**Template**: `templates/zod-request-schemas.tera`

**Example Output**:
```javascript
export const createUserRequestSchema = z.object({
  email: z.string(),
  username: z.string().min(1).max(255),
  bio: z.string().max(500).optional(),
});
```

## Rule 9: Type Guards

**Purpose**: Generates JavaScript type guard functions for runtime type checking

**Output**: `lib/guards/entities.mjs`

**SPARQL Query**:
```sparql
SELECT ?entityName ?propertyName ?propertyType ?required ?isArray
WHERE {
  ?entity a api:Entity ;
          api:name ?entityName ;
          api:hasProperty ?property .
  ?property api:name ?propertyName ;
            api:type ?propertyType .
  OPTIONAL { ?property api:required ?required }
  OPTIONAL { ?property api:isArray ?isArray }
}
ORDER BY ?entityName ?propertyName
```

**What it extracts**:
- Entity names and required properties
- Used to generate runtime checks

**Template**: `templates/type-guards.tera`

**Example Output**:
```javascript
export function isUser(value) {
  return (
    typeof value === 'object' &&
    value !== null &&
    'id' in value &&
    'email' in value &&
    'username' in value &&
    true
  );
}
```

## Rule 10: Index File (Barrel Export)

**Purpose**: Generates main index file with barrel exports

**Output**: `lib/index.mjs`

**SPARQL Query**:
```sparql
SELECT ?entityName
WHERE {
  ?entity a api:Entity ;
          api:name ?entityName .
}
ORDER BY ?entityName
```

**What it extracts**:
- List of all entity names (for documentation)

**Template**: `templates/index.tera`

**Example Output**:
```javascript
// Entity schemas
export * from './schemas/entities.mjs';

// Request validation schemas
export * from './schemas/requests.mjs';

// Type definitions
export * from './types/entities.mjs';
export * from './types/requests.mjs';

// Type guards
export * from './guards/entities.mjs';
```

## Understanding the Flow

1. **All rules run in sequence**
2. **Each rule**:
   - Loads the ontology
   - Executes its SPARQL query
   - Gets results as `sparql_results`
   - Renders template with results
   - Writes to output file
3. **Results**: Complete set of generated files in `lib/`

## Modifying Rules

### Adding a New Rule

1. Add a new `[[generation.rules]]` section
2. Define:
   - `name`: Unique identifier
   - `query`: SPARQL query
   - `template`: Template file
   - `output_file`: Output path
3. Create the template file
4. Run `ggen sync`

### Modifying an Existing Rule

1. Edit the SPARQL query to extract different data
2. Update the template to use new data
3. Run `ggen sync` to regenerate

## Query Patterns

### Common Patterns

**Get all entities**:
```sparql
?entity a api:Entity ;
        api:name ?entityName .
```

**Get entity properties**:
```sparql
?entity api:hasProperty ?property .
?property api:name ?propertyName ;
          api:type ?propertyType .
```

**Get optional data**:
```sparql
OPTIONAL { ?property api:format ?format }
```

**Order results**:
```sparql
ORDER BY ?entityName ?propertyName
```

## Template Patterns

### Looping Through Results

```tera
{%- for row in sparql_results -%}
  {{ row["?entityName"] }}
{%- endfor %}
```

### Conditional Rendering

```tera
{%- if row["?required"] == "true" -%}
  required
{%- endif %}
```

### Variable Filters

```tera
{{ entityName | lower }}  {# Convert to lowercase #}
{{ propertyName | snake_case }}  {# Convert to snake_case #}
```

## Best Practices

1. **Keep queries focused**: Each query should extract one type of data
2. **Use OPTIONAL wisely**: Only for data that might not exist
3. **Order results**: Use ORDER BY for consistent output
4. **Document templates**: Add comments explaining complex logic
5. **Test incrementally**: Modify one rule at a time and verify output

