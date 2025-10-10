<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [Chapter 8: The Generation Workflow](#chapter-8-the-generation-workflow)
  - [What is Code Generation?](#what-is-code-generation)
  - [The Generation Pipeline](#the-generation-pipeline)
  - [Step 1: Load Knowledge](#step-1-load-knowledge)
  - [Step 2: Validate with SHACL](#step-2-validate-with-shacl)
  - [Step 3: Query with SPARQL](#step-3-query-with-sparql)
  - [Step 4: Apply Templates](#step-4-apply-templates)
  - [Step 5: Write Output Files](#step-5-write-output-files)
  - [Step 6: Run Hooks](#step-6-run-hooks)
  - [Generation Modes](#generation-modes)
    - [Mode 1: Full Generation](#mode-1-full-generation)
    - [Mode 2: Incremental Generation](#mode-2-incremental-generation)
    - [Mode 3: Selective Generation](#mode-3-selective-generation)
    - [Mode 4: Watch Mode](#mode-4-watch-mode)
  - [Multi-Target Generation](#multi-target-generation)
  - [Template Configuration](#template-configuration)
  - [Custom Templates](#custom-templates)
  - [Template Helpers](#template-helpers)
  - [Generation Workflow Examples](#generation-workflow-examples)
    - [Example 1: Daily Feature Development](#example-1-daily-feature-development)
    - [Example 2: Schema Migration](#example-2-schema-migration)
    - [Example 3: Multi-Language Generation](#example-3-multi-language-generation)
  - [Troubleshooting Generation](#troubleshooting-generation)
    - [Problem: Template errors](#problem-template-errors)
    - [Problem: Generated code doesn't compile](#problem-generated-code-doesnt-compile)
    - [Problem: Generation is slow](#problem-generation-is-slow)
  - [Best Practices](#best-practices)
  - [Summary](#summary)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Chapter 8: The Generation Workflow

## What is Code Generation?

Code generation in GGen is the process of **transforming knowledge graphs into working code** using templates.

Think of it as:
- **Input:** Domain knowledge (RDF/OWL)
- **Transform:** Template engine (Handlebars, Jinja, etc.)
- **Output:** Code, schemas, documentation

The beauty is that this transformation is **repeatable**, **consistent**, and **automated**.

## The Generation Pipeline

When you run `ggen project generate`, here's what happens:

```
1. Load Knowledge
   ↓
2. Validate with SHACL
   ↓
3. Query with SPARQL
   ↓
4. Apply Templates
   ↓
5. Write Output Files
   ↓
6. Run Hooks (format, lint, test)
```

Let's explore each step.

## Step 1: Load Knowledge

GGen loads all knowledge sources defined in `ggen.config.yaml`:

```yaml
knowledge:
  sources:
    - knowledge/domain.ttl
    - knowledge/patterns.ttl
    - market/auth-user.ttl
```

These are parsed into an **RDF graph**—a queryable data structure.

**Example graph:**
```turtle
:User a owl:Class ;
  :hasProperty :userEmail ;
  :hasProperty :userPassword .

:userEmail a owl:DatatypeProperty ;
  rdfs:range :Email ;
  :required true ;
  :unique true .
```

This becomes queryable:
```sparql
SELECT ?class ?property WHERE {
  ?class a owl:Class .
  ?class :hasProperty ?property .
}
# Returns: (:User, :userEmail), (:User, :userPassword)
```

## Step 2: Validate with SHACL

Before generation, GGen validates the graph against **SHACL shapes**:

```turtle
# shapes/user-shape.ttl
:UserShape a sh:NodeShape ;
  sh:targetClass :User ;
  sh:property [
    sh:path :email ;
    sh:minCount 1 ;
    sh:maxCount 1 ;
    sh:pattern "^[a-z0-9._%+-]+@[a-z0-9.-]+\\.[a-z]{2,}$"
  ] .
```

If validation fails, generation stops with a detailed error:

```
Validation Error:
  Entity: :User
  Property: :email
  Constraint: sh:pattern
  Issue: Value "invalid-email" does not match pattern
```

This catches errors **before** they become code bugs.

## Step 3: Query with SPARQL

For each generation target, GGen runs **SPARQL queries** to extract relevant knowledge.

**Example: Find all CRUD entities**

```sparql
PREFIX : <http://myapp.com/ontology#>
PREFIX owl: <http://www.w3.org/2002/07/owl#>

SELECT ?entity ?label WHERE {
  ?entity a owl:Class .
  ?entity :supportsCRUD true .
  ?entity rdfs:label ?label .
}
```

**Result:**
```
entity       label
-----------  ---------
:User        "User"
:Product     "Product"
:Order       "Order"
```

This data is passed to templates.

## Step 4: Apply Templates

Templates are **Handlebars** files (or Jinja, Liquid, etc.) that transform query results into code.

**Example template: TypeScript interface**

```handlebars
{{!-- templates/typescript-interface.hbs --}}
/**
 * {{rdfs:label}}
 * @generated
 */
export interface {{className}} {
  id: string;

{{#each properties}}
  {{#if :required}}
  {{name}}: {{toTypeScriptType rdfs:range}};
  {{else}}
  {{name}}?: {{toTypeScriptType rdfs:range}};
  {{/if}}
{{/each}}

  createdAt: Date;
  updatedAt: Date;
}
```

**Input data (from SPARQL):**
```json
{
  "className": "User",
  "rdfs:label": "User",
  "properties": [
    { "name": "email", "rdfs:range": "Email", ":required": true },
    { "name": "password", "rdfs:range": "HashedString", ":required": true }
  ]
}
```

**Output:**
```typescript
/**
 * User
 * @generated
 */
export interface User {
  id: string;

  email: string;
  password: string;

  createdAt: Date;
  updatedAt: Date;
}
```

## Step 5: Write Output Files

Generated code is written to the configured output directory:

```yaml
generation:
  targets:
    - name: ts-types
      template: "@ggen/typescript-types"
      output: src/types/
```

**Output structure:**
```
src/types/
├── user.ts           # Generated
├── product.ts        # Generated
├── order.ts          # Generated
└── index.ts          # Generated (exports all)
```

Each file includes a **generation marker**:

```typescript
/**
 * @generated by GGen
 * DO NOT EDIT MANUALLY
 *
 * Source: knowledge/domain.ttl
 * Template: @ggen/typescript-types
 * Generated: 2024-01-15T10:30:00Z
 */
```

This warns developers not to edit generated files directly.

## Step 6: Run Hooks

After generation, GGen runs configured hooks:

```yaml
hooks:
  postGenerate:
    - "npm run format"          # Format with Prettier
    - "npm run lint:fix"        # Fix linting issues
    - "npm run test:types"      # Run type tests
```

These ensure generated code is clean and valid.

## Generation Modes

### Mode 1: Full Generation

Generate everything from scratch.

```bash
ggen project generate
```

**Use when:**
- First time generating
- Major knowledge changes
- Clean slate needed

**Output:** All configured targets regenerated.

### Mode 2: Incremental Generation

Only regenerate changed entities.

```bash
ggen project generate --incremental
```

**Use when:**
- Daily development
- Small knowledge updates
- Fast iteration

**Output:** Only files for changed entities.

**How it works:**

GGen tracks the **last generation timestamp** and compares it with knowledge file timestamps:

```
knowledge/domain.ttl         modified: 2024-01-15 10:00
knowledge/patterns.ttl       modified: 2024-01-10 09:00
last generation              timestamp: 2024-01-14 15:00

Changes detected in: domain.ttl
Entities affected: User, Order
Regenerating: user.ts, order.ts
```

### Mode 3: Selective Generation

Generate specific targets or entities.

```bash
# Generate specific target
ggen project generate --target db-schema

# Generate specific entity
ggen project generate --entity User

# Generate multiple targets
ggen project generate --target ts-types,crud-api

# Combine filters
ggen project generate --entity User,Product --target ts-types
```

**Use when:**
- Debugging a specific target
- Testing a template change
- Avoiding full regeneration

### Mode 4: Watch Mode

Auto-regenerate on knowledge changes.

```bash
ggen project generate --watch
```

**Use when:**
- Active development
- Iterating on knowledge model
- Instant feedback needed

**How it works:**

GGen watches all knowledge files and regenerates when changes are detected:

```
Watching knowledge/ for changes...

[10:05:23] knowledge/domain.ttl changed
[10:05:23] Regenerating ts-types...
[10:05:24] ✓ Done (1.2s)

[10:07:15] knowledge/domain.ttl changed
[10:07:15] Regenerating ts-types...
[10:07:16] ✓ Done (1.1s)
```

## Multi-Target Generation

Most projects generate multiple targets:

```yaml
generation:
  targets:
    - name: db-schema
      template: "@ggen/postgresql-schema"
      output: migrations/

    - name: ts-types
      template: "@ggen/typescript-types"
      output: src/types/

    - name: crud-api
      template: "@ggen/nestjs-crud"
      output: src/api/

    - name: graphql-schema
      template: "@ggen/graphql-schema"
      output: src/schema.graphql

    - name: validation
      template: "@ggen/zod-schemas"
      output: src/validation/

    - name: tests
      template: "@ggen/jest-tests"
      output: tests/

    - name: api-docs
      template: "@ggen/openapi-docs"
      output: docs/api.yaml
```

Running `ggen project generate` generates **all enabled targets** in parallel.

**Parallel execution:**
```
Generating 7 targets...

⠋ db-schema       (0.5s)
⠋ ts-types        (0.3s)
⠋ crud-api        (1.2s)
⠋ graphql-schema  (0.4s)
⠋ validation      (0.6s)
⠋ tests           (0.9s)
⠋ api-docs        (0.7s)

✓ All targets generated (1.2s)
```

## Template Configuration

Templates can be configured per target:

```yaml
generation:
  targets:
    - name: ts-types
      template: "@ggen/typescript-types"
      output: src/types/
      config:
        # Template-specific options
        exportStyle: named          # vs default
        includeComments: true       # Add JSDoc
        strictNullChecks: true      # Optional types
        dateFormat: iso8601         # vs timestamp
        enumStyle: union            # vs enum
```

Each template defines its own config schema.

## Custom Templates

You can create project-specific templates:

**Step 1: Create template file**

```handlebars
{{!-- templates/my-custom-api.hbs --}}
import { Controller, Get } from '@nestjs/common';

@Controller('{{apiPath}}')
export class {{className}}Controller {
  @Get()
  async findAll() {
    // TODO: Implement
    return [];
  }
}
```

**Step 2: Configure generation**

```yaml
generation:
  targets:
    - name: custom-api
      template: templates/my-custom-api.hbs
      output: src/api/
```

**Step 3: Generate**

```bash
ggen project generate --target custom-api
```

## Template Helpers

Templates have access to helper functions:

**Built-in helpers:**

```handlebars
{{!-- String formatting --}}
{{pascalCase name}}        → UserProfile
{{camelCase name}}         → userProfile
{{snakeCase name}}         → user_profile
{{kebabCase name}}         → user-profile

{{!-- Type conversion --}}
{{toTypeScriptType type}}  → string, number, boolean, etc.
{{toSQLType type}}         → VARCHAR, INTEGER, etc.
{{toPythonType type}}      → str, int, bool, etc.

{{!-- Pluralization --}}
{{pluralize "user"}}       → users
{{singularize "users"}}    → user

{{!-- Conditionals --}}
{{#if :required}}required{{else}}optional{{/if}}

{{!-- Iteration --}}
{{#each properties}}
  {{name}}: {{type}}
{{/each}}
```

**Custom helpers:**

Define helpers in `templates/helpers.js`:

```javascript
// templates/helpers.js
module.exports = {
  // Custom type formatter
  formatType(type) {
    const mapping = {
      'Email': 'string',
      'URL': 'string',
      'Money': 'number',
      'HashedString': 'string'
    };
    return mapping[type] || 'any';
  },

  // Custom validator generator
  generateValidator(property) {
    if (property.type === 'Email') {
      return `z.string().email()`;
    }
    if (property.min && property.max) {
      return `z.number().min(${property.min}).max(${property.max})`;
    }
    return `z.${property.type}()`;
  }
};
```

Use in templates:

```handlebars
{{formatType rdfs:range}}
{{generateValidator this}}
```

## Generation Workflow Examples

### Example 1: Daily Feature Development

**Task:** Add a "Review" entity to an e-commerce app.

**Step 1: Define knowledge**

```turtle
# knowledge/domain.ttl
:Review a owl:Class ;
  rdfs:label "Product Review" ;
  :supportsCRUD true ;
  :hasProperty :reviewRating ;
  :hasProperty :reviewComment ;
  :hasRelationship :reviewProduct ;
  :hasRelationship :reviewAuthor .

:reviewRating a owl:DatatypeProperty ;
  rdfs:range xsd:integer ;
  :min 1 ;
  :max 5 ;
  :required true .

:reviewComment a owl:DatatypeProperty ;
  rdfs:range xsd:string ;
  :maxLength 1000 .

:reviewProduct a owl:ObjectProperty ;
  rdfs:range :Product ;
  :cardinality "1" ;
  :required true .

:reviewAuthor a owl:ObjectProperty ;
  rdfs:range :User ;
  :cardinality "1" ;
  :required true .
```

**Step 2: Validate**

```bash
ggen project validate
```

**Step 3: Generate**

```bash
ggen project generate --watch
```

Watch mode regenerates automatically as you edit `domain.ttl`.

**Step 4: Review output**

```bash
# Check generated files
ls src/types/review.ts
ls src/api/review.controller.ts
ls migrations/003_create_reviews.sql

# Inspect
cat src/types/review.ts
```

**Step 5: Add custom logic**

```typescript
// src/services/review.service.ts (hand-written)
import { ReviewService as GeneratedReviewService } from '../generated/review.service';

export class ReviewService extends GeneratedReviewService {
  async calculateProductRating(productId: string): Promise<number> {
    const reviews = await this.findByProduct(productId);
    const sum = reviews.reduce((acc, r) => acc + r.rating, 0);
    return sum / reviews.length;
  }
}
```

**Step 6: Test**

```bash
npm test
```

**Step 7: Commit**

```bash
git add knowledge/ src/
git commit -m "Add Review entity with rating calculation"
```

### Example 2: Schema Migration

**Task:** Add "verified purchase" flag to reviews.

**Step 1: Update knowledge**

```turtle
:Review :hasProperty :reviewVerifiedPurchase .

:reviewVerifiedPurchase a owl:DatatypeProperty ;
  rdfs:label "verifiedPurchase" ;
  rdfs:range xsd:boolean ;
  :default false ;
  :required false .
```

**Step 2: Generate migration**

```bash
ggen project generate --target db-schema
```

**Output:**

```sql
-- migrations/004_add_verified_purchase_to_reviews.sql
ALTER TABLE reviews
  ADD COLUMN verified_purchase BOOLEAN DEFAULT FALSE;

CREATE INDEX idx_reviews_verified
  ON reviews(verified_purchase)
  WHERE verified_purchase = TRUE;
```

**Step 3: Apply migration**

```bash
npm run migrate
```

**Step 4: Regenerate code**

```bash
ggen project generate
```

All types, APIs, and validation update automatically.

### Example 3: Multi-Language Generation

**Task:** Generate TypeScript client and Python SDK from the same knowledge.

**Configuration:**

```yaml
generation:
  targets:
    # TypeScript client
    - name: ts-client
      template: "@ggen/typescript-client"
      output: clients/typescript/
      config:
        apiBase: "https://api.myapp.com"

    # Python SDK
    - name: python-sdk
      template: "@ggen/python-sdk"
      output: clients/python/
      config:
        packageName: "myapp_sdk"
        apiBase: "https://api.myapp.com"
```

**Generate:**

```bash
ggen project generate
```

**Output:**

```
clients/
├── typescript/
│   ├── src/
│   │   ├── user.ts
│   │   ├── product.ts
│   │   └── review.ts
│   └── package.json
│
└── python/
    ├── myapp_sdk/
    │   ├── user.py
    │   ├── product.py
    │   └── review.py
    └── setup.py
```

Both clients stay in sync because they're generated from the same knowledge.

## Troubleshooting Generation

### Problem: Template errors

**Symptom:**
```
Error: Template compilation failed
  templates/my-template.hbs:12:3
  {{unknownHelper}}
```

**Solution:**

1. Check helper is defined:
   ```bash
   ls templates/helpers.js
   ```

2. Validate template syntax:
   ```bash
   ggen template test templates/my-template.hbs
   ```

3. Add missing helper:
   ```javascript
   // templates/helpers.js
   module.exports.unknownHelper = (value) => {
     return value.toUpperCase();
   };
   ```

### Problem: Generated code doesn't compile

**Symptom:**
```
src/types/user.ts:5:3 - error TS2322: Type 'string' is not assignable to type 'Email'.
```

**Solution:**

1. Check type mappings in template:
   ```handlebars
   {{toTypeScriptType rdfs:range}}
   ```

2. Add custom type mapping:
   ```javascript
   // templates/helpers.js
   module.exports.toTypeScriptType = (type) => {
     if (type === 'Email') return 'string';
     // ...
   };
   ```

3. Regenerate:
   ```bash
   ggen project generate --force
   ```

### Problem: Generation is slow

**Symptom:**
```
Generating... (15.3s)
```

**Solutions:**

1. Use incremental generation:
   ```bash
   ggen project generate --incremental
   ```

2. Generate specific targets:
   ```bash
   ggen project generate --target ts-types
   ```

3. Disable unnecessary targets:
   ```yaml
   generation:
     targets:
       - name: api-docs
         enabled: false  # Disable slow targets
   ```

4. Cache template compilation:
   ```yaml
   generation:
     cacheTemplates: true
   ```

## Best Practices

**1. Always validate before generating**

```bash
ggen project validate && ggen project generate
```

**2. Use watch mode during development**

```bash
ggen project generate --watch
```

**3. Commit knowledge, ignore generated code**

`.gitignore`:
```
generated/
src/types/*.generated.ts
migrations/*.sql
```

Only commit knowledge graphs; regenerate on pull.

**4. Version templates**

Lock template versions in config:

```yaml
generation:
  targets:
    - template: "@ggen/typescript-types@1.2.3"
```

**5. Test generated code**

Add tests for generated code:

```typescript
describe('Generated Types', () => {
  it('should have User type', () => {
    const user: User = {
      id: '123',
      email: 'test@example.com',
      createdAt: new Date(),
      updatedAt: new Date()
    };
    expect(user).toBeDefined();
  });
});
```

## Summary

The generation workflow is the heart of GGen:

1. **Define knowledge** in RDF/OWL
2. **Validate** with SHACL
3. **Generate** code with templates
4. **Extend** with custom logic
5. **Iterate** with watch mode

Key commands:
- `ggen project generate` - Generate all targets
- `ggen project generate --watch` - Auto-regenerate
- `ggen project generate --incremental` - Fast iteration
- `ggen project validate` - Check before generating

This workflow shifts effort from **writing code** to **defining knowledge**—the 20% that matters.

Next: **Part III - Patterns** where we explore reusable knowledge patterns and advanced template techniques.
