# Handlebars Templates for RDF-to-TypeScript Code Generation

This directory contains Handlebars templates for generating production-ready TypeScript code from RDF ontologies.

## Templates Overview

### 1. `types.ts.hbs`
Generates TypeScript interfaces and Zod validation schemas from `rdfs:Class` definitions.

**Features:**
- Maps XSD types to TypeScript types
- Generates Zod schemas from SHACL constraints
- Creates type guards and utility types
- Handles enumerations and union types
- Includes JSDoc comments from `rdfs:comment`

**Input Data Structure:**
```javascript
{
  generatedDate: "2025-01-13",
  ontologyUri: "https://example.org/ontology#",
  classes: [
    {
      name: "Task",
      comment: "A task in the system",
      superClass: "Entity",
      isStrict: true,
      hasEnumerations: true,
      properties: [
        {
          propertyName: "title",
          comment: "Task title",
          tsType: "string",
          zodType: "z.string().min(1).max(200)",
          isRequired: true,
          isArray: false,
          minValue: null,
          maxValue: null,
          pattern: null
        }
      ],
      enumerations: [
        {
          enumName: "TaskStatus",
          values: ["pending", "active", "completed"]
        }
      ]
    }
  ]
}
```

### 2. `api-routes.ts.hbs`
Generates Next.js 14 App Router API routes with full CRUD operations.

**Features:**
- GET endpoint with pagination and filtering
- POST endpoint with Zod validation
- PUT endpoint for updates
- DELETE endpoint
- Error handling and status codes
- Optional authentication middleware

**Input Data Structure:**
```javascript
{
  generatedDate: "2025-01-13",
  className: "Task",
  pluralName: "tasks",
  routePath: "tasks",
  lowerCaseName: "task",
  hasDatabase: true,
  hasAuth: true,
  filterableProperties: [
    { propertyName: "status", zodType: "string", isStringType: true }
  ],
  searchableProperties: [
    { propertyName: "title" },
    { propertyName: "description" }
  ]
}
```

### 3. `crud-table.tsx.hbs`
Generates shadcn/ui DataTable components with full CRUD functionality.

**Features:**
- Column sorting and filtering
- Column visibility toggle
- Search functionality
- Create/Edit/Delete dialogs
- Form validation with error display
- Loading states
- Pagination

**Input Data Structure:**
```javascript
{
  generatedDate: "2025-01-13",
  className: "Task",
  pluralName: "tasks",
  routePath: "tasks",
  searchColumn: "title",
  displayColumns: [
    {
      propertyName: "title",
      displayName: "Title",
      isDateType: false,
      isBooleanType: false,
      isEnumType: false,
      isNumberType: false,
      isTruncated: false
    },
    {
      propertyName: "status",
      displayName: "Status",
      isEnumType: true
    }
  ],
  formFields: [
    {
      propertyName: "title",
      displayName: "Title",
      isRequired: true,
      isEnumType: false,
      isBooleanType: false,
      isDateType: false,
      isNumberType: false,
      pattern: null,
      minValue: null,
      maxValue: null
    },
    {
      propertyName: "status",
      displayName: "Status",
      isRequired: true,
      isEnumType: true,
      enumValues: ["pending", "active", "completed"]
    }
  ]
}
```

### 4. `partials/type-mappings.hbs`
Helper partials for type conversion and formatting.

**Available Helpers:**
- `xsdToTs` - XSD type to TypeScript type
- `xsdToZod` - XSD type to Zod schema
- `isDateType` - Check if XSD type is a date
- `isNumberType` - Check if XSD type is numeric
- `isBooleanType` - Check if XSD type is boolean
- `isStringType` - Check if XSD type is string
- `formatPropertyName` - Convert to camelCase
- `formatClassName` - Convert to PascalCase

## Type Mappings

| XSD Type | TypeScript | Zod Schema |
|----------|-----------|------------|
| `xsd:string` | `string` | `z.string()` |
| `xsd:integer` | `number` | `z.number().int()` |
| `xsd:decimal` | `number` | `z.number()` |
| `xsd:boolean` | `boolean` | `z.boolean()` |
| `xsd:dateTime` | `Date` | `z.date()` |
| `xsd:anyURI` | `string` | `z.string().url()` |

## Usage Example

```typescript
// Example: Generating types from RDF ontology
import Handlebars from 'handlebars';
import fs from 'fs';

// Load template
const template = Handlebars.compile(
  fs.readFileSync('types.ts.hbs', 'utf-8')
);

// Prepare data from RDF ontology
const data = {
  generatedDate: new Date().toISOString().split('T')[0],
  ontologyUri: 'https://example.org/task-ontology#',
  classes: [
    {
      name: 'Task',
      comment: 'A task in the project management system',
      properties: [
        {
          propertyName: 'title',
          comment: 'The title of the task',
          tsType: 'string',
          zodType: 'z.string().min(1).max(200)',
          isRequired: true
        },
        {
          propertyName: 'status',
          comment: 'Current status of the task',
          tsType: 'TaskStatus',
          zodType: 'TaskStatusSchema',
          isRequired: true
        }
      ],
      hasEnumerations: true,
      enumerations: [
        {
          enumName: 'TaskStatus',
          values: ['pending', 'active', 'completed', 'cancelled']
        }
      ]
    }
  ]
};

// Generate code
const output = template(data);
fs.writeFileSync('generated/types.ts', output);
```

## Dependencies

The generated code requires these dependencies:

```json
{
  "dependencies": {
    "zod": "^3.22.4",
    "next": "^14.0.0",
    "@tanstack/react-table": "^8.10.0",
    "lucide-react": "^0.294.0",
    "@radix-ui/react-dialog": "^1.0.5",
    "@radix-ui/react-dropdown-menu": "^2.0.6",
    "@radix-ui/react-select": "^2.0.0",
    "@radix-ui/react-alert-dialog": "^1.0.5"
  }
}
```

## Best Practices

1. **Type Safety**: All generated code uses TypeScript strict mode
2. **Validation**: Zod schemas validate all inputs at runtime
3. **Error Handling**: Comprehensive error handling with user-friendly messages
4. **Accessibility**: shadcn/ui components are accessible by default
5. **Performance**: Efficient React patterns (memoization, lazy loading)
6. **Security**: Input sanitization and CSRF protection
7. **Testing**: Generated code is testable with clear separation of concerns

## Customization

Templates can be customized by:
1. Modifying the `.hbs` files directly
2. Creating custom helpers in `partials/`
3. Extending the input data structure
4. Adding custom validation rules

## Next Steps

1. Integrate with RDF parser (e.g., `rdflib`, `n3`)
2. Create CLI tool for batch generation
3. Add support for GraphQL schema generation
4. Create Storybook stories for generated components
5. Add E2E tests for generated API routes
