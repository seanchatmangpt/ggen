# How to: Customize Code Generation

**Problem:** Default templates don't match your project's needs.

**Solution:** Customize templates, create overrides, or write your own template.

---

## Method 1: Override Template Variables

Pass custom configuration to existing templates:

```bash
ggen ontology generate schema.org \
  --template typescript \
  --output ./src/schema \
  --config '{
    "strict_null_checks": true,
    "readonly_properties": true,
    "include_validators": true,
    "validation_library": "zod",
    "naming_convention": "camelCase",
    "include_json_ld_context": true,
    "generate_type_guards": true
  }'
```

See [Template Variable Reference](../reference/template-variables.md) for all options.

---

## Method 2: Use Template Override File

Create `./template-config.yaml`:

```yaml
# TypeScript generation config
typescript:
  strict_null_checks: true
  readonly_properties: false
  module_system: "esm"

  # Naming conventions
  interface_naming: "PascalCase"
  property_naming: "camelCase"
  enum_naming: "UPPER_SNAKE_CASE"

  # Features
  include_validators: true
  validation_library: "zod"
  include_type_guards: true
  include_json_ld_context: true

  # Documentation
  include_descriptions: true
  include_examples: true
  include_see_also: true

  # Type mappings
  custom_type_mappings:
    "http://www.w3.org/2001/XMLSchema#dateTime": "Date"
    "http://www.w3.org/2001/XMLSchema#duration": "number"  # milliseconds

  # Property mappings (rename properties)
  property_name_overrides:
    "givenName": "firstName"
    "familyName": "lastName"
    "mbox": "email"
```

Use the config:

```bash
ggen ontology generate schema.org \
  --template typescript \
  --output ./src/schema \
  --config-file ./template-config.yaml
```

---

## Method 3: Create Custom Template Variant

### Step 1: Copy Existing Template

```bash
ggen ontology template extract schema.org typescript ./my-templates/
```

**Output:**
```
Extracted template to ./my-templates/typescript/
  template.hbs
  config.yaml
  helpers.js
```

### Step 2: Modify Template

Edit `./my-templates/typescript/template.hbs`:

```handlebars
/**
 * Generated from {{ontology.name}} ontology
 * @generated DO NOT EDIT
 */

import { z } from 'zod';

{{#each classes}}
// Schema for {{name}}
export const {{name}}Schema = z.object({
  "@type": z.literal("{{name}}"),
  {{#each properties}}
  {{propertyName}}: {{zodType}}{{#if optional}}.optional(){{/if}},
  {{/each}}
});

// Inferred TypeScript type
export type {{name}} = z.infer<typeof {{name}}Schema>;

// Validator function
export function validate{{name}}(data: unknown): {{name}} {
  return {{name}}Schema.parse(data);
}

// Type guard
export function is{{name}}(obj: any): obj is {{name}} {
  return {{name}}Schema.safeParse(obj).success;
}

{{/each}}
```

### Step 3: Add Custom Helpers

Edit `./my-templates/typescript/helpers.js`:

```javascript
module.exports = {
  // Convert RDF type to Zod schema
  zodType(rdfType, isArray) {
    const mapping = {
      'xsd:string': 'z.string()',
      'xsd:integer': 'z.number().int()',
      'xsd:boolean': 'z.boolean()',
      'xsd:dateTime': 'z.string().datetime()',
      'xsd:float': 'z.number()',
    };

    let baseType = mapping[rdfType] || 'z.unknown()';
    return isArray ? `z.array(${baseType})` : baseType;
  },

  // Convert to camelCase
  camelCase(str) {
    return str.charAt(0).toLowerCase() + str.slice(1);
  }
};
```

### Step 4: Use Custom Template

```bash
ggen ontology generate schema.org \
  --template ./my-templates/typescript \
  --output ./src/schema
```

---

## Method 4: Filter Generated Types

Generate only specific types:

```bash
# Only Person and Organization
ggen ontology generate schema.org \
  --template typescript \
  --output ./src/schema \
  --filter "Person,Organization"
```

Filter by pattern:

```bash
# All types containing "Event"
ggen ontology generate schema.org \
  --template typescript \
  --output ./src/schema \
  --filter-pattern "*Event*"
```

Exclude types:

```bash
# Everything except deprecated types
ggen ontology generate schema.org \
  --template typescript \
  --output ./src/schema \
  --exclude "Thing,Intangible"
```

---

## Method 5: Custom SPARQL Queries

Override the SPARQL queries that extract data:

Create `./custom-queries.yaml`:

```yaml
# Custom SPARQL queries for extraction
classes: |
  PREFIX owl: <http://www.w3.org/2002/07/owl#>
  PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
  PREFIX schema: <http://schema.org/>

  SELECT ?class ?label ?comment ?deprecated WHERE {
    ?class a owl:Class .
    OPTIONAL { ?class rdfs:label ?label }
    OPTIONAL { ?class rdfs:comment ?comment }
    OPTIONAL { ?class schema:supersededBy ?deprecated }

    # Only include non-deprecated classes
    FILTER(!BOUND(?deprecated))
  }

properties: |
  PREFIX owl: <http://www.w3.org/2002/07/owl#>
  PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
  PREFIX schema: <http://schema.org/>

  SELECT ?property ?domain ?range ?label ?comment ?required WHERE {
    {?property a owl:DatatypeProperty} UNION {?property a owl:ObjectProperty}
    OPTIONAL { ?property rdfs:domain ?domain }
    OPTIONAL { ?property rdfs:range ?range }
    OPTIONAL { ?property rdfs:label ?label }
    OPTIONAL { ?property rdfs:comment ?comment }

    # Check if property is required (cardinality)
    OPTIONAL {
      ?domain rdfs:subClassOf [
        a owl:Restriction ;
        owl:onProperty ?property ;
        owl:minCardinality ?min
      ]
      BIND(?min > 0 AS ?required)
    }
  }
```

Use custom queries:

```bash
ggen ontology generate schema.org \
  --template typescript \
  --output ./src/schema \
  --queries-file ./custom-queries.yaml
```

---

## Method 6: Post-Processing Hook

Run custom script after generation:

```bash
ggen ontology generate schema.org \
  --template typescript \
  --output ./src/schema \
  --post-process ./scripts/post-process.sh
```

Create `./scripts/post-process.sh`:

```bash
#!/bin/bash
# Post-process generated TypeScript files

OUTPUT_DIR=$1

# Format with prettier
npx prettier --write "$OUTPUT_DIR/**/*.ts"

# Add custom header
for file in "$OUTPUT_DIR"/*.ts; do
  echo "// Copyright 2025 MyCompany" | cat - "$file" > temp && mv temp "$file"
done

# Generate index.ts
echo "// Auto-generated index" > "$OUTPUT_DIR/index.ts"
for file in "$OUTPUT_DIR"/types/*.ts; do
  basename=$(basename "$file" .ts)
  echo "export * from './types/$basename';" >> "$OUTPUT_DIR/index.ts"
done

echo "Post-processing complete"
```

---

## Method 7: Incremental Generation

Generate only changed types:

```bash
ggen ontology generate schema.org \
  --template typescript \
  --output ./src/schema \
  --incremental \
  --cache ./.ggen-cache
```

**How it works:**
1. Hashes each generated file
2. Compares with previous generation
3. Only regenerates changed types
4. Preserves manual edits in non-generated files

---

## Method 8: Multi-Language Generation

Generate multiple languages in one command:

```bash
ggen ontology generate schema.org \
  --templates typescript,python,rust \
  --output ./generated \
  --split-by-language
```

**Output structure:**
```
generated/
├── typescript/
│   └── types/
├── python/
│   └── models/
└── rust/
    └── src/
```

---

## Method 9: Template Composition

Combine multiple template features:

```bash
ggen ontology generate schema.org \
  --template typescript \
  --output ./src/schema \
  --compose-with graphql,zod,react
```

**What happens:**
- Base TypeScript types
- + GraphQL decorators
- + Zod validators
- + React hooks

**Generated:**

```typescript
import { ObjectType, Field } from 'type-graphql';
import { z } from 'zod';
import { useState } from 'react';

@ObjectType()
export class Person {
  @Field()
  "@type": "Person";

  @Field({ nullable: true })
  name?: string;
}

export const PersonSchema = z.object({
  "@type": z.literal("Person"),
  name: z.string().optional()
});

export function usePerson(initial?: Partial<Person>) {
  const [person, setPerson] = useState<Person>({
    "@type": "Person",
    ...initial
  });
  return { person, setPerson };
}
```

---

## Tips for Customization

1. **Start with overrides:** Try config options before custom templates
2. **Test incrementally:** Generate small subsets first
3. **Use post-processing:** For project-specific formatting
4. **Version templates:** Store custom templates in version control
5. **Share templates:** Publish custom templates as packs

---

## Related Guides

- [How to: Generate TypeScript Types](generate-typescript-types.md)
- [Reference: Template Variables](../reference/template-variables.md)
- [Reference: SPARQL Patterns](../reference/sparql-patterns.md)
