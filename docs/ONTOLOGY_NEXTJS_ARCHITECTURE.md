# Ontology-Driven Next.js Architecture

## Executive Summary

This architecture establishes RDF ontology as the single source of truth for a Next.js application, ensuring zero drift between semantic models and implementation through automated code generation, type safety, and validation.

## System Architecture Diagram

```
┌─────────────────────────────────────────────────────────────────────────┐
│                         RDF ONTOLOGY (Source of Truth)                  │
│  ┌────────────────────────────────────────────────────────────────┐    │
│  │  • OWL/RDFS Classes & Properties                               │    │
│  │  • SHACL Validation Shapes                                     │    │
│  │  • Domain Model Definitions                                    │    │
│  └────────────────────────────────────────────────────────────────┘    │
└───────────────────────────┬─────────────────────────────────────────────┘
                            │
                            │ Git Hook Trigger
                            ▼
┌─────────────────────────────────────────────────────────────────────────┐
│                      CODE GENERATION PIPELINE                            │
│  ┌─────────────┐   ┌──────────────┐   ┌───────────────┐              │
│  │ RDF Parser  │──▶│ Type Gen     │──▶│ Component Gen │              │
│  │ (Oxigraph)  │   │ (TS Types)   │   │ (React/UI)    │              │
│  └─────────────┘   └──────────────┘   └───────────────┘              │
│         │                  │                    │                       │
│         ▼                  ▼                    ▼                       │
│  ┌─────────────┐   ┌──────────────┐   ┌───────────────┐              │
│  │ Validation  │   │ API Routes   │   │ tRPC Routers  │              │
│  │ (Zod)       │   │ (Next.js)    │   │ (Type-safe)   │              │
│  └─────────────┘   └──────────────┘   └───────────────┘              │
└───────────────────────────┬─────────────────────────────────────────────┘
                            │
                            ▼
┌─────────────────────────────────────────────────────────────────────────┐
│                      NEXT.JS APPLICATION                                 │
│  ┌────────────────────────────────────────────────────────────────┐    │
│  │                     Frontend Layer                              │    │
│  │  ┌──────────────┐  ┌──────────────┐  ┌──────────────┐         │    │
│  │  │ CRUD Tables  │  │ Forms        │  │ Detail Views │         │    │
│  │  │ (shadcn/ui)  │  │ (shadcn/ui)  │  │ (shadcn/ui)  │         │    │
│  │  └──────────────┘  └──────────────┘  └──────────────┘         │    │
│  └────────────────────────────────────────────────────────────────┘    │
│  ┌────────────────────────────────────────────────────────────────┐    │
│  │                     API Layer                                   │    │
│  │  ┌──────────────┐  ┌──────────────┐  ┌──────────────┐         │    │
│  │  │ tRPC Routes  │  │ Validation   │  │ SHACL Engine │         │    │
│  │  │ (Type-safe)  │  │ (Zod Schema) │  │ (Runtime)    │         │    │
│  │  └──────────────┘  └──────────────┘  └──────────────┘         │    │
│  └────────────────────────────────────────────────────────────────┘    │
│  ┌────────────────────────────────────────────────────────────────┐    │
│  │                     Data Layer                                  │    │
│  │  ┌──────────────┐  ┌──────────────┐  ┌──────────────┐         │    │
│  │  │ Prisma ORM   │  │ Oxigraph     │  │ Cache Layer  │         │    │
│  │  │ (SQL)        │  │ (RDF Store)  │  │ (Redis)      │         │    │
│  │  └──────────────┘  └──────────────┘  └──────────────┘         │    │
│  └────────────────────────────────────────────────────────────────┘    │
└─────────────────────────────────────────────────────────────────────────┘
```

## File Structure

```
ontology-nextjs-app/
├── ontology/                          # Source of Truth
│   ├── schema/
│   │   ├── core.ttl                   # Core domain ontology
│   │   ├── entities.ttl               # Entity definitions
│   │   └── properties.ttl             # Property definitions
│   ├── shapes/
│   │   ├── validation.ttl             # SHACL validation shapes
│   │   └── ui-hints.ttl               # UI generation hints
│   └── config/
│       ├── generation.yaml            # Code generation config
│       └── mapping.yaml               # Ontology → TS mapping rules
│
├── generated/                         # Auto-generated (gitignored)
│   ├── types/
│   │   ├── entities.ts                # TypeScript interfaces
│   │   ├── schemas.ts                 # Zod validation schemas
│   │   └── index.ts                   # Barrel exports
│   ├── components/
│   │   ├── tables/                    # CRUD tables per entity
│   │   ├── forms/                     # Forms per entity
│   │   └── views/                     # Detail views per entity
│   ├── api/
│   │   ├── trpc/                      # tRPC routers
│   │   └── validators/                # API validators
│   └── prisma/
│       └── schema.prisma              # Prisma schema from ontology
│
├── src/
│   ├── app/                           # Next.js app directory
│   │   ├── (dashboard)/               # Dashboard routes
│   │   │   └── [entity]/              # Dynamic entity routes
│   │   │       ├── page.tsx           # List view
│   │   │       ├── [id]/page.tsx      # Detail view
│   │   │       └── new/page.tsx       # Create view
│   │   ├── api/
│   │   │   └── trpc/[trpc]/route.ts   # tRPC handler
│   │   └── layout.tsx
│   │
│   ├── lib/
│   │   ├── trpc/
│   │   │   ├── client.ts              # tRPC client
│   │   │   └── server.ts              # tRPC server
│   │   ├── validation/
│   │   │   ├── shacl-runtime.ts       # Runtime SHACL validator
│   │   │   └── error-formatter.ts     # Validation error formatter
│   │   ├── ontology/
│   │   │   ├── query.ts               # SPARQL query helpers
│   │   │   └── store.ts               # Oxigraph wrapper
│   │   └── utils/
│   │       └── cn.ts                  # Utility functions
│   │
│   ├── components/
│   │   ├── ui/                        # shadcn/ui components
│   │   ├── layout/                    # Layout components
│   │   └── shared/                    # Shared business components
│   │
│   └── hooks/
│       ├── use-entity-query.ts        # Type-safe data hooks
│       └── use-entity-mutation.ts     # Type-safe mutation hooks
│
├── codegen/                           # Code generation tools
│   ├── src/
│   │   ├── parsers/
│   │   │   ├── rdf-parser.ts          # Parse RDF ontology
│   │   │   ├── shacl-parser.ts        # Parse SHACL shapes
│   │   │   └── ui-hints-parser.ts     # Parse UI metadata
│   │   ├── generators/
│   │   │   ├── typescript-gen.ts      # Generate TS types
│   │   │   ├── zod-gen.ts             # Generate Zod schemas
│   │   │   ├── component-gen.ts       # Generate React components
│   │   │   ├── trpc-gen.ts            # Generate tRPC routers
│   │   │   └── prisma-gen.ts          # Generate Prisma schema
│   │   ├── templates/
│   │   │   ├── table.tsx.hbs          # Table component template
│   │   │   ├── form.tsx.hbs           # Form component template
│   │   │   ├── detail.tsx.hbs         # Detail view template
│   │   │   └── router.ts.hbs          # tRPC router template
│   │   └── cli.ts                     # CLI entry point
│   ├── tests/                         # Generator tests
│   └── package.json
│
├── .git/
│   └── hooks/
│       ├── pre-commit                 # Run codegen + type check
│       └── post-merge                 # Run codegen after pulls
│
├── scripts/
│   ├── generate.sh                    # Main generation script
│   ├── validate-ontology.sh           # Ontology validation
│   └── setup-hooks.sh                 # Install git hooks
│
├── docs/
│   ├── ONTOLOGY_GUIDE.md              # Ontology development guide
│   ├── GENERATION_GUIDE.md            # Code generation guide
│   └── ARCHITECTURE.md                # This document
│
├── package.json
├── tsconfig.json
└── next.config.js
```

## Code Generation Pipeline

### Phase 1: Ontology Parsing

```typescript
// codegen/src/parsers/rdf-parser.ts
import { Store } from 'oxigraph';

export interface OntologyClass {
  uri: string;
  label: string;
  comment?: string;
  properties: OntologyProperty[];
  constraints: SHACLConstraint[];
}

export interface OntologyProperty {
  uri: string;
  label: string;
  datatype: string;
  range?: string;
  cardinality: Cardinality;
  required: boolean;
}

export class OntologyParser {
  private store: Store;

  async parse(ontologyPath: string): Promise<OntologyModel> {
    // Load RDF files into Oxigraph store
    const rdfContent = await fs.readFile(ontologyPath);
    this.store.load(rdfContent, 'text/turtle');

    // Extract classes
    const classes = await this.extractClasses();

    // Extract properties for each class
    for (const cls of classes) {
      cls.properties = await this.extractProperties(cls.uri);
      cls.constraints = await this.extractConstraints(cls.uri);
    }

    return { classes, metadata: this.extractMetadata() };
  }

  private async extractClasses(): Promise<OntologyClass[]> {
    const query = `
      PREFIX owl: <http://www.w3.org/2002/07/owl#>
      PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>

      SELECT ?class ?label ?comment WHERE {
        ?class a owl:Class .
        ?class rdfs:label ?label .
        OPTIONAL { ?class rdfs:comment ?comment }
      }
    `;

    return this.store.query(query);
  }

  private async extractConstraints(classUri: string): Promise<SHACLConstraint[]> {
    const query = `
      PREFIX sh: <http://www.w3.org/ns/shacl#>

      SELECT ?property ?minCount ?maxCount ?pattern ?datatype WHERE {
        ?shape sh:targetClass <${classUri}> .
        ?shape sh:property ?propShape .
        ?propShape sh:path ?property .
        OPTIONAL { ?propShape sh:minCount ?minCount }
        OPTIONAL { ?propShape sh:maxCount ?maxCount }
        OPTIONAL { ?propShape sh:pattern ?pattern }
        OPTIONAL { ?propShape sh:datatype ?datatype }
      }
    `;

    return this.store.query(query);
  }
}
```

### Phase 2: TypeScript Type Generation

```typescript
// codegen/src/generators/typescript-gen.ts
export class TypeScriptGenerator {
  generate(model: OntologyModel): string {
    const imports = this.generateImports();
    const interfaces = model.classes.map(cls =>
      this.generateInterface(cls)
    ).join('\n\n');

    return `${imports}\n\n${interfaces}`;
  }

  private generateInterface(cls: OntologyClass): string {
    const name = this.toPascalCase(cls.label);
    const properties = cls.properties.map(prop =>
      this.generateProperty(prop)
    ).join('\n  ');

    return `
/**
 * ${cls.comment || cls.label}
 * @ontology ${cls.uri}
 */
export interface ${name} {
  ${properties}
}

export type ${name}Create = Omit<${name}, 'id' | 'createdAt' | 'updatedAt'>;
export type ${name}Update = Partial<${name}Create>;
    `.trim();
  }

  private generateProperty(prop: OntologyProperty): string {
    const name = this.toCamelCase(prop.label);
    const type = this.mapRDFTypeToTS(prop.datatype, prop.range);
    const optional = !prop.required ? '?' : '';
    const array = prop.cardinality.max > 1 ? '[]' : '';

    return `${name}${optional}: ${type}${array};`;
  }

  private mapRDFTypeToTS(datatype: string, range?: string): string {
    const typeMap: Record<string, string> = {
      'xsd:string': 'string',
      'xsd:integer': 'number',
      'xsd:decimal': 'number',
      'xsd:boolean': 'boolean',
      'xsd:dateTime': 'Date',
      'xsd:date': 'Date',
    };

    if (range) {
      // Object reference - use the interface name
      return this.toPascalCase(this.extractLabel(range));
    }

    return typeMap[datatype] || 'unknown';
  }
}
```

### Phase 3: Zod Schema Generation

```typescript
// codegen/src/generators/zod-gen.ts
export class ZodSchemaGenerator {
  generate(model: OntologyModel): string {
    const imports = `import { z } from 'zod';`;
    const schemas = model.classes.map(cls =>
      this.generateSchema(cls)
    ).join('\n\n');

    return `${imports}\n\n${schemas}`;
  }

  private generateSchema(cls: OntologyClass): string {
    const name = this.toPascalCase(cls.label);
    const properties = cls.properties.map(prop =>
      this.generatePropertySchema(prop, cls.constraints)
    ).join(',\n  ');

    return `
export const ${name}Schema = z.object({
  ${properties}
});

export const ${name}CreateSchema = ${name}Schema.omit({
  id: true,
  createdAt: true,
  updatedAt: true
});

export const ${name}UpdateSchema = ${name}CreateSchema.partial();
    `.trim();
  }

  private generatePropertySchema(
    prop: OntologyProperty,
    constraints: SHACLConstraint[]
  ): string {
    const name = this.toCamelCase(prop.label);
    let schema = this.mapRDFTypeToZod(prop.datatype, prop.range);

    // Apply SHACL constraints
    const propConstraints = constraints.filter(c => c.property === prop.uri);

    for (const constraint of propConstraints) {
      if (constraint.pattern) {
        schema += `.regex(/${constraint.pattern}/)`;
      }
      if (constraint.minLength) {
        schema += `.min(${constraint.minLength})`;
      }
      if (constraint.maxLength) {
        schema += `.max(${constraint.maxLength})`;
      }
      if (constraint.minInclusive) {
        schema += `.gte(${constraint.minInclusive})`;
      }
      if (constraint.maxInclusive) {
        schema += `.lte(${constraint.maxInclusive})`;
      }
    }

    // Handle cardinality
    if (prop.cardinality.max > 1) {
      schema = `z.array(${schema})`;
      if (prop.cardinality.min > 0) {
        schema += `.min(${prop.cardinality.min})`;
      }
      if (prop.cardinality.max !== Infinity) {
        schema += `.max(${prop.cardinality.max})`;
      }
    }

    // Handle optionality
    if (!prop.required) {
      schema += '.optional()';
    }

    return `${name}: ${schema}`;
  }

  private mapRDFTypeToZod(datatype: string, range?: string): string {
    const typeMap: Record<string, string> = {
      'xsd:string': 'z.string()',
      'xsd:integer': 'z.number().int()',
      'xsd:decimal': 'z.number()',
      'xsd:boolean': 'z.boolean()',
      'xsd:dateTime': 'z.date()',
      'xsd:date': 'z.date()',
    };

    if (range) {
      // Object reference - recursive schema
      const rangeName = this.toPascalCase(this.extractLabel(range));
      return `z.lazy(() => ${rangeName}Schema)`;
    }

    return typeMap[datatype] || 'z.unknown()';
  }
}
```

### Phase 4: Component Generation

```typescript
// codegen/src/generators/component-gen.ts
import Handlebars from 'handlebars';

export class ComponentGenerator {
  private templates: Map<string, HandlebarsTemplateDelegate>;

  constructor() {
    // Load Handlebars templates
    this.templates = new Map([
      ['table', Handlebars.compile(fs.readFileSync('./templates/table.tsx.hbs', 'utf-8'))],
      ['form', Handlebars.compile(fs.readFileSync('./templates/form.tsx.hbs', 'utf-8'))],
      ['detail', Handlebars.compile(fs.readFileSync('./templates/detail.tsx.hbs', 'utf-8'))],
    ]);
  }

  generateTable(cls: OntologyClass, uiHints: UIHints): string {
    const name = this.toPascalCase(cls.label);

    const context = {
      entityName: name,
      entityNameLower: this.toCamelCase(cls.label),
      columns: cls.properties.filter(p => uiHints.showInTable(p)).map(p => ({
        key: this.toCamelCase(p.label),
        label: p.label,
        type: this.mapToUIType(p.datatype, p.range),
        sortable: uiHints.isSortable(p),
        filterable: uiHints.isFilterable(p),
      })),
      actions: uiHints.getActions(cls),
    };

    return this.templates.get('table')!(context);
  }

  generateForm(cls: OntologyClass, uiHints: UIHints): string {
    const name = this.toPascalCase(cls.label);

    const context = {
      entityName: name,
      entityNameLower: this.toCamelCase(cls.label),
      fields: cls.properties.filter(p => uiHints.showInForm(p)).map(p => ({
        name: this.toCamelCase(p.label),
        label: p.label,
        type: this.mapToFormType(p.datatype, p.range),
        required: p.required,
        placeholder: uiHints.getPlaceholder(p),
        helpText: p.comment,
        validation: this.getValidationRules(p, cls.constraints),
      })),
    };

    return this.templates.get('form')!(context);
  }

  private mapToFormType(datatype: string, range?: string): string {
    if (range) return 'select'; // Reference to another entity

    const typeMap: Record<string, string> = {
      'xsd:string': 'text',
      'xsd:integer': 'number',
      'xsd:decimal': 'number',
      'xsd:boolean': 'checkbox',
      'xsd:dateTime': 'datetime',
      'xsd:date': 'date',
    };

    return typeMap[datatype] || 'text';
  }
}
```

### Phase 5: tRPC Router Generation

```typescript
// codegen/src/generators/trpc-gen.ts
export class TRPCGenerator {
  generate(model: OntologyModel): string {
    const imports = this.generateImports(model);
    const routers = model.classes.map(cls =>
      this.generateRouter(cls)
    ).join('\n\n');

    const appRouter = this.generateAppRouter(model.classes);

    return `${imports}\n\n${routers}\n\n${appRouter}`;
  }

  private generateRouter(cls: OntologyClass): string {
    const name = this.toPascalCase(cls.label);
    const nameLower = this.toCamelCase(cls.label);

    return `
export const ${nameLower}Router = router({
  list: publicProcedure
    .input(z.object({
      page: z.number().default(1),
      pageSize: z.number().default(10),
      filter: z.record(z.any()).optional(),
      sort: z.object({ field: z.string(), order: z.enum(['asc', 'desc']) }).optional(),
    }))
    .query(async ({ input, ctx }) => {
      const { page, pageSize, filter, sort } = input;

      const items = await ctx.db.${nameLower}.findMany({
        where: filter,
        orderBy: sort ? { [sort.field]: sort.order } : undefined,
        skip: (page - 1) * pageSize,
        take: pageSize,
      });

      const total = await ctx.db.${nameLower}.count({ where: filter });

      return {
        items,
        pagination: {
          page,
          pageSize,
          total,
          pages: Math.ceil(total / pageSize),
        },
      };
    }),

  getById: publicProcedure
    .input(z.object({ id: z.string() }))
    .query(async ({ input, ctx }) => {
      return ctx.db.${nameLower}.findUnique({
        where: { id: input.id },
      });
    }),

  create: publicProcedure
    .input(${name}CreateSchema)
    .mutation(async ({ input, ctx }) => {
      // Runtime SHACL validation
      await validateWithSHACL(input, '${cls.uri}');

      return ctx.db.${nameLower}.create({
        data: input,
      });
    }),

  update: publicProcedure
    .input(z.object({
      id: z.string(),
      data: ${name}UpdateSchema,
    }))
    .mutation(async ({ input, ctx }) => {
      // Runtime SHACL validation
      await validateWithSHACL(input.data, '${cls.uri}');

      return ctx.db.${nameLower}.update({
        where: { id: input.id },
        data: input.data,
      });
    }),

  delete: publicProcedure
    .input(z.object({ id: z.string() }))
    .mutation(async ({ input, ctx }) => {
      return ctx.db.${nameLower}.delete({
        where: { id: input.id },
      });
    }),
});
    `.trim();
  }

  private generateAppRouter(classes: OntologyClass[]): string {
    const routers = classes.map(cls => {
      const nameLower = this.toCamelCase(cls.label);
      return `  ${nameLower}: ${nameLower}Router,`;
    }).join('\n');

    return `
export const appRouter = router({
${routers}
});

export type AppRouter = typeof appRouter;
    `.trim();
  }
}
```

## Handlebars Templates

### Table Component Template

```handlebars
{{!-- codegen/src/templates/table.tsx.hbs --}}
"use client";

import { useState } from "react";
import { useRouter } from "next/navigation";
import { trpc } from "@/lib/trpc/client";
import {
  Table,
  TableBody,
  TableCell,
  TableHead,
  TableHeader,
  TableRow,
} from "@/components/ui/table";
import { Button } from "@/components/ui/button";
import { Input } from "@/components/ui/input";
import { Pencil, Trash2, Plus } from "lucide-react";

export function {{entityName}}Table() {
  const router = useRouter();
  const [page, setPage] = useState(1);
  const [filter, setFilter] = useState({});

  const { data, isLoading } = trpc.{{entityNameLower}}.list.useQuery({
    page,
    pageSize: 10,
    filter,
  });

  const deleteMutation = trpc.{{entityNameLower}}.delete.useMutation({
    onSuccess: () => {
      // Refresh the list
      utils.{{entityNameLower}}.list.invalidate();
    },
  });

  const handleDelete = async (id: string) => {
    if (confirm("Are you sure you want to delete this item?")) {
      await deleteMutation.mutateAsync({ id });
    }
  };

  if (isLoading) return <div>Loading...</div>;

  return (
    <div className="space-y-4">
      <div className="flex justify-between items-center">
        <h2 className="text-2xl font-bold">{{entityName}}s</h2>
        <Button onClick={() => router.push("/{{entityNameLower}}/new")}>
          <Plus className="mr-2 h-4 w-4" /> New {{entityName}}
        </Button>
      </div>

      <div className="rounded-md border">
        <Table>
          <TableHeader>
            <TableRow>
              {{#each columns}}
              <TableHead>{{label}}</TableHead>
              {{/each}}
              <TableHead className="text-right">Actions</TableHead>
            </TableRow>
          </TableHeader>
          <TableBody>
            {data?.items.map((item) => (
              <TableRow key={item.id}>
                {{#each columns}}
                <TableCell>
                  {{#if (eq type 'date')}}
                  {new Date(item.{{key}}).toLocaleDateString()}
                  {{else if (eq type 'boolean')}}
                  {item.{{key}} ? "Yes" : "No"}
                  {{else}}
                  {item.{{key}}}
                  {{/if}}
                </TableCell>
                {{/each}}
                <TableCell className="text-right">
                  <div className="flex gap-2 justify-end">
                    <Button
                      variant="ghost"
                      size="sm"
                      onClick={() => router.push(`/{{entityNameLower}}/${item.id}`)}
                    >
                      <Pencil className="h-4 w-4" />
                    </Button>
                    <Button
                      variant="ghost"
                      size="sm"
                      onClick={() => handleDelete(item.id)}
                    >
                      <Trash2 className="h-4 w-4" />
                    </Button>
                  </div>
                </TableCell>
              </TableRow>
            ))}
          </TableBody>
        </Table>
      </div>

      {/* Pagination */}
      <div className="flex justify-between items-center">
        <div className="text-sm text-muted-foreground">
          Showing {data?.items.length} of {data?.pagination.total} results
        </div>
        <div className="flex gap-2">
          <Button
            variant="outline"
            size="sm"
            disabled={page === 1}
            onClick={() => setPage(page - 1)}
          >
            Previous
          </Button>
          <Button
            variant="outline"
            size="sm"
            disabled={page === data?.pagination.pages}
            onClick={() => setPage(page + 1)}
          >
            Next
          </Button>
        </div>
      </div>
    </div>
  );
}
```

### Form Component Template

```handlebars
{{!-- codegen/src/templates/form.tsx.hbs --}}
"use client";

import { useRouter } from "next/navigation";
import { useForm } from "react-hook-form";
import { zodResolver } from "@hookform/resolvers/zod";
import { trpc } from "@/lib/trpc/client";
import { {{entityName}}CreateSchema } from "@/generated/types/schemas";
import {
  Form,
  FormControl,
  FormDescription,
  FormField,
  FormItem,
  FormLabel,
  FormMessage,
} from "@/components/ui/form";
import { Input } from "@/components/ui/input";
import { Checkbox } from "@/components/ui/checkbox";
import { Button } from "@/components/ui/button";

interface {{entityName}}FormProps {
  initialData?: any;
  id?: string;
}

export function {{entityName}}Form({ initialData, id }: {{entityName}}FormProps) {
  const router = useRouter();
  const form = useForm({
    resolver: zodResolver({{entityName}}CreateSchema),
    defaultValues: initialData || {},
  });

  const createMutation = trpc.{{entityNameLower}}.create.useMutation({
    onSuccess: () => {
      router.push("/{{entityNameLower}}");
    },
  });

  const updateMutation = trpc.{{entityNameLower}}.update.useMutation({
    onSuccess: () => {
      router.push("/{{entityNameLower}}");
    },
  });

  const onSubmit = async (data: any) => {
    if (id) {
      await updateMutation.mutateAsync({ id, data });
    } else {
      await createMutation.mutateAsync(data);
    }
  };

  return (
    <Form {...form}>
      <form onSubmit={form.handleSubmit(onSubmit)} className="space-y-6">
        {{#each fields}}
        <FormField
          control={form.control}
          name="{{name}}"
          render={({ field }) => (
            <FormItem>
              <FormLabel>{{label}} {{#if required}}*{{/if}}</FormLabel>
              <FormControl>
                {{#if (eq type 'text')}}
                <Input placeholder="{{placeholder}}" {...field} />
                {{else if (eq type 'number')}}
                <Input type="number" placeholder="{{placeholder}}" {...field} />
                {{else if (eq type 'checkbox')}}
                <Checkbox checked={field.value} onCheckedChange={field.onChange} />
                {{else if (eq type 'date')}}
                <Input type="date" {...field} />
                {{else if (eq type 'datetime')}}
                <Input type="datetime-local" {...field} />
                {{/if}}
              </FormControl>
              {{#if helpText}}
              <FormDescription>{{helpText}}</FormDescription>
              {{/if}}
              <FormMessage />
            </FormItem>
          )}
        />
        {{/each}}

        <div className="flex gap-2">
          <Button type="submit">
            {id ? "Update" : "Create"} {{entityName}}
          </Button>
          <Button
            type="button"
            variant="outline"
            onClick={() => router.back()}
          >
            Cancel
          </Button>
        </div>
      </form>
    </Form>
  );
}
```

## Git Hook Workflow

### Pre-Commit Hook

```bash
#!/bin/bash
# .git/hooks/pre-commit

set -e

echo "🔍 Checking for ontology changes..."

# Get list of changed ontology files
ONTOLOGY_CHANGED=$(git diff --cached --name-only | grep "^ontology/" || true)

if [ -n "$ONTOLOGY_CHANGED" ]; then
  echo "📝 Ontology files changed, regenerating code..."

  # Run validation first
  npm run validate:ontology

  # Run code generation
  npm run generate

  # Add generated files to commit
  git add generated/

  echo "✅ Code regenerated successfully"
fi

# Run type checking
echo "🔍 Type checking..."
npm run typecheck

# Run linting
echo "🔍 Linting..."
npm run lint

echo "✅ Pre-commit checks passed"
```

### Post-Merge Hook

```bash
#!/bin/bash
# .git/hooks/post-merge

set -e

echo "🔍 Checking for ontology changes after merge..."

# Check if ontology files were changed in the merge
ONTOLOGY_CHANGED=$(git diff --name-only HEAD@{1} HEAD | grep "^ontology/" || true)

if [ -n "$ONTOLOGY_CHANGED" ]; then
  echo "📝 Ontology files changed in merge, regenerating code..."

  # Run validation
  npm run validate:ontology

  # Run code generation
  npm run generate

  echo "⚠️  Code has been regenerated. Please review changes."
  echo "    Run 'git status' to see updated files."
fi
```

### Setup Script

```bash
#!/bin/bash
# scripts/setup-hooks.sh

set -e

echo "🔧 Setting up git hooks..."

# Make hooks executable
chmod +x .git/hooks/pre-commit
chmod +x .git/hooks/post-merge

# Copy hooks to .git/hooks/
cp scripts/git-hooks/pre-commit .git/hooks/pre-commit
cp scripts/git-hooks/post-merge .git/hooks/post-merge

echo "✅ Git hooks installed successfully"
```

## Data Flow: Ontology Change → Type-Safe CRUD

```
1. Developer edits ontology/schema/entities.ttl
   ↓
2. Git pre-commit hook triggers
   ↓
3. scripts/validate-ontology.sh runs
   • Validates RDF syntax
   • Validates SHACL shapes
   • Checks for breaking changes
   ↓
4. scripts/generate.sh runs
   • Parse ontology with Oxigraph
   • Generate TypeScript types
   • Generate Zod schemas
   • Generate React components
   • Generate tRPC routers
   • Generate Prisma schema
   ↓
5. Generated files written to generated/
   ↓
6. TypeScript compiler validates all generated code
   ↓
7. Generated files added to git commit
   ↓
8. Developer sees type-safe CRUD tables in UI
   • All types match ontology
   • All validations from SHACL
   • Zero drift between schema and UI
```

## Runtime SHACL Validation

```typescript
// src/lib/validation/shacl-runtime.ts
import { Store } from 'oxigraph';
import { SHACLValidator } from '@zazuko/shacl';

export class RuntimeSHACLValidator {
  private store: Store;
  private validator: SHACLValidator;
  private shapesGraph: string;

  constructor() {
    this.store = new Store();
    // Load SHACL shapes at startup
    this.loadShapes();
  }

  private async loadShapes() {
    const shapesContent = await fs.readFile('./ontology/shapes/validation.ttl');
    this.shapesGraph = shapesContent.toString();
    this.validator = new SHACLValidator(this.shapesGraph);
  }

  async validate(data: any, classUri: string): Promise<ValidationResult> {
    // Convert JSON data to RDF
    const dataGraph = this.jsonToRDF(data, classUri);

    // Run SHACL validation
    const report = await this.validator.validate(dataGraph);

    if (!report.conforms) {
      const errors = this.formatErrors(report.results);
      throw new ValidationError('SHACL validation failed', errors);
    }

    return { valid: true };
  }

  private jsonToRDF(data: any, classUri: string): string {
    // Convert JSON to RDF using JSON-LD context
    const jsonLd = {
      '@context': this.getContext(),
      '@type': classUri,
      ...data,
    };

    return JSON.stringify(jsonLd);
  }

  private formatErrors(results: any[]): ValidationError[] {
    return results.map(result => ({
      path: result.path,
      message: result.message,
      value: result.value,
      constraint: result.sourceConstraintComponent,
    }));
  }
}

// Export singleton instance
export const shaclValidator = new RuntimeSHACLValidator();

// Helper function for use in tRPC routers
export async function validateWithSHACL(data: any, classUri: string) {
  return shaclValidator.validate(data, classUri);
}
```

## Integration Patterns

### tRPC with Type Safety

```typescript
// src/lib/trpc/server.ts
import { initTRPC } from '@trpc/server';
import { type Context } from './context';

const t = initTRPC.context<Context>().create();

export const router = t.router;
export const publicProcedure = t.procedure;

// src/lib/trpc/client.ts
import { createTRPCReact } from '@trpc/react-query';
import { type AppRouter } from '@/generated/api/trpc/app-router';

export const trpc = createTRPCReact<AppRouter>();

// Usage in components is fully type-safe
const { data } = trpc.person.list.useQuery({ page: 1 });
//    ^? data: { items: Person[], pagination: {...} }
```

### Next.js API Routes Integration

```typescript
// src/app/api/trpc/[trpc]/route.ts
import { fetchRequestHandler } from '@trpc/server/adapters/fetch';
import { appRouter } from '@/generated/api/trpc/app-router';
import { createContext } from '@/lib/trpc/context';

const handler = (req: Request) =>
  fetchRequestHandler({
    endpoint: '/api/trpc',
    req,
    router: appRouter,
    createContext,
  });

export { handler as GET, handler as POST };
```

### Dynamic Entity Routes

```typescript
// src/app/(dashboard)/[entity]/page.tsx
import { notFound } from 'next/navigation';
import { entityRegistry } from '@/generated/registry';

interface EntityListPageProps {
  params: { entity: string };
}

export default function EntityListPage({ params }: EntityListPageProps) {
  const EntityTable = entityRegistry.getTable(params.entity);

  if (!EntityTable) {
    notFound();
  }

  return (
    <div className="container mx-auto py-10">
      <EntityTable />
    </div>
  );
}

// Generate static params for all entities
export function generateStaticParams() {
  return entityRegistry.getAllEntityNames().map((entity) => ({
    entity,
  }));
}
```

## Developer Experience

### Hot Reload During Development

```typescript
// next.config.js
const { watchOntology } = require('./codegen/watch');

module.exports = {
  webpack: (config, { dev, isServer }) => {
    if (dev && isServer) {
      // Watch ontology files and regenerate on change
      watchOntology({
        onRegenerate: () => {
          console.log('♻️  Ontology changed, code regenerated');
        },
      });
    }
    return config;
  },
};
```

### Type Checking Integration

```json
// package.json scripts
{
  "scripts": {
    "dev": "next dev",
    "build": "npm run generate && next build",
    "generate": "tsx codegen/src/cli.ts generate",
    "validate:ontology": "tsx codegen/src/cli.ts validate",
    "typecheck": "tsc --noEmit",
    "lint": "next lint",
    "test": "vitest"
  }
}
```

### VSCode Integration

```json
// .vscode/settings.json
{
  "files.associations": {
    "*.ttl": "turtle",
    "*.shacl": "turtle"
  },
  "turtle.format.enable": true,
  "typescript.tsdk": "node_modules/typescript/lib",
  "typescript.enablePromptUseWorkspaceTsdk": true
}
```

## Production Readiness

### Error Handling Strategy

```typescript
// src/lib/validation/error-formatter.ts
export class ValidationErrorFormatter {
  static formatSHACLError(error: SHACLValidationError): UserFriendlyError {
    // Map SHACL constraint violations to user-friendly messages
    const messageMap: Record<string, string> = {
      'sh:MinCountConstraintComponent': 'This field is required',
      'sh:MaxCountConstraintComponent': 'Too many values provided',
      'sh:PatternConstraintComponent': 'Invalid format',
      'sh:MinLengthConstraintComponent': 'Too short',
      'sh:MaxLengthConstraintComponent': 'Too long',
    };

    return {
      field: this.extractFieldName(error.path),
      message: messageMap[error.constraint] || error.message,
      value: error.value,
    };
  }

  static formatForForm(errors: ValidationError[]): Record<string, string> {
    return errors.reduce((acc, error) => {
      const formatted = this.formatSHACLError(error);
      acc[formatted.field] = formatted.message;
      return acc;
    }, {} as Record<string, string>);
  }
}
```

### Caching Strategy

```typescript
// src/lib/cache/ontology-cache.ts
import { Redis } from 'ioredis';

export class OntologyCache {
  private redis: Redis;
  private ttl = 3600; // 1 hour

  async getValidationResult(
    classUri: string,
    dataHash: string
  ): Promise<ValidationResult | null> {
    const key = `shacl:${classUri}:${dataHash}`;
    const cached = await this.redis.get(key);
    return cached ? JSON.parse(cached) : null;
  }

  async setValidationResult(
    classUri: string,
    dataHash: string,
    result: ValidationResult
  ): Promise<void> {
    const key = `shacl:${classUri}:${dataHash}`;
    await this.redis.setex(key, this.ttl, JSON.stringify(result));
  }
}
```

### Monitoring & Observability

```typescript
// src/lib/telemetry/ontology-metrics.ts
import { Counter, Histogram } from 'prom-client';

export const ontologyMetrics = {
  validationDuration: new Histogram({
    name: 'ontology_validation_duration_seconds',
    help: 'Duration of SHACL validation operations',
    labelNames: ['class_uri', 'result'],
  }),

  validationErrors: new Counter({
    name: 'ontology_validation_errors_total',
    help: 'Total number of validation errors',
    labelNames: ['class_uri', 'constraint_type'],
  }),

  generationDuration: new Histogram({
    name: 'ontology_code_generation_duration_seconds',
    help: 'Duration of code generation from ontology',
    labelNames: ['generator_type'],
  }),
};
```

## Migration Strategy

### Breaking Change Detection

```typescript
// codegen/src/validators/breaking-changes.ts
export class BreakingChangeDetector {
  async detect(
    oldOntology: OntologyModel,
    newOntology: OntologyModel
  ): Promise<BreakingChange[]> {
    const changes: BreakingChange[] = [];

    // Detect removed classes
    for (const oldClass of oldOntology.classes) {
      const newClass = newOntology.classes.find(c => c.uri === oldClass.uri);
      if (!newClass) {
        changes.push({
          type: 'class_removed',
          severity: 'breaking',
          uri: oldClass.uri,
          message: `Class ${oldClass.label} has been removed`,
        });
      }
    }

    // Detect removed properties
    for (const oldClass of oldOntology.classes) {
      const newClass = newOntology.classes.find(c => c.uri === oldClass.uri);
      if (newClass) {
        for (const oldProp of oldClass.properties) {
          const newProp = newClass.properties.find(p => p.uri === oldProp.uri);
          if (!newProp) {
            changes.push({
              type: 'property_removed',
              severity: 'breaking',
              uri: oldProp.uri,
              message: `Property ${oldProp.label} removed from ${oldClass.label}`,
            });
          }
        }
      }
    }

    // Detect cardinality changes
    // Detect type changes
    // ...more checks

    return changes;
  }
}
```

### Versioning Strategy

```turtle
# ontology/schema/core.ttl
@prefix schema: <https://example.com/schema/> .
@prefix owl: <http://www.w3.org/2002/07/owl#> .

schema:
  a owl:Ontology ;
  owl:versionIRI <https://example.com/schema/1.2.0> ;
  owl:versionInfo "1.2.0" ;
  owl:priorVersion <https://example.com/schema/1.1.0> .
```

## Security Considerations

### Input Sanitization

```typescript
// src/lib/security/sanitize.ts
export function sanitizeForSPARQL(input: string): string {
  // Prevent SPARQL injection
  return input
    .replace(/[<>]/g, '') // Remove angle brackets
    .replace(/['"]/g, '') // Remove quotes
    .trim();
}

export function sanitizeForRDF(input: any): any {
  // Recursively sanitize object for RDF conversion
  if (typeof input === 'string') {
    return sanitizeForSPARQL(input);
  }
  if (Array.isArray(input)) {
    return input.map(sanitizeForRDF);
  }
  if (typeof input === 'object' && input !== null) {
    return Object.fromEntries(
      Object.entries(input).map(([k, v]) => [k, sanitizeForRDF(v)])
    );
  }
  return input;
}
```

### Access Control Integration

```typescript
// src/lib/security/access-control.ts
export class OntologyAccessControl {
  async checkAccess(
    user: User,
    classUri: string,
    operation: 'read' | 'create' | 'update' | 'delete'
  ): Promise<boolean> {
    // Query ontology for access control rules
    const query = `
      PREFIX acl: <http://www.w3.org/ns/auth/acl#>

      ASK {
        <${user.uri}> acl:${operation} <${classUri}> .
      }
    `;

    return this.store.query(query);
  }
}
```

## Performance Optimization

### Code Generation Caching

```typescript
// codegen/src/cache/generation-cache.ts
export class GenerationCache {
  private cacheDir = '.cache/codegen';

  async getCachedOutput(
    ontologyHash: string,
    generatorName: string
  ): Promise<string | null> {
    const cachePath = path.join(
      this.cacheDir,
      `${ontologyHash}-${generatorName}.ts`
    );

    if (await fs.pathExists(cachePath)) {
      return fs.readFile(cachePath, 'utf-8');
    }

    return null;
  }

  async setCachedOutput(
    ontologyHash: string,
    generatorName: string,
    output: string
  ): Promise<void> {
    const cachePath = path.join(
      this.cacheDir,
      `${ontologyHash}-${generatorName}.ts`
    );

    await fs.ensureDir(this.cacheDir);
    await fs.writeFile(cachePath, output);
  }

  async invalidateCache(): Promise<void> {
    await fs.remove(this.cacheDir);
  }
}
```

### Incremental Generation

```typescript
// codegen/src/incremental.ts
export class IncrementalGenerator {
  async generateIncremental(
    changes: OntologyChange[]
  ): Promise<GeneratedFiles> {
    const affectedClasses = this.getAffectedClasses(changes);
    const generatedFiles: GeneratedFiles = {};

    // Only regenerate files for affected classes
    for (const cls of affectedClasses) {
      generatedFiles[`types/${cls.name}.ts`] = await this.generateType(cls);
      generatedFiles[`components/${cls.name}Table.tsx`] = await this.generateTable(cls);
      generatedFiles[`api/${cls.name}Router.ts`] = await this.generateRouter(cls);
    }

    return generatedFiles;
  }

  private getAffectedClasses(changes: OntologyChange[]): OntologyClass[] {
    // Determine which classes are affected by the changes
    const affected = new Set<string>();

    for (const change of changes) {
      affected.add(change.classUri);

      // If a property references another class, that class is affected too
      if (change.type === 'property_changed' && change.range) {
        affected.add(change.range);
      }
    }

    return Array.from(affected).map(uri => this.ontology.getClass(uri));
  }
}
```

## Testing Strategy

### Ontology Validation Tests

```typescript
// codegen/tests/ontology-validation.test.ts
import { describe, it, expect } from 'vitest';
import { OntologyValidator } from '../src/validators/ontology-validator';

describe('Ontology Validation', () => {
  it('should validate well-formed ontology', async () => {
    const validator = new OntologyValidator();
    const result = await validator.validate('./ontology/schema/core.ttl');

    expect(result.valid).toBe(true);
    expect(result.errors).toHaveLength(0);
  });

  it('should detect missing rdfs:label', async () => {
    const validator = new OntologyValidator();
    const result = await validator.validate('./test-fixtures/invalid-no-label.ttl');

    expect(result.valid).toBe(false);
    expect(result.errors).toContainEqual(
      expect.objectContaining({
        type: 'missing_label',
        severity: 'warning',
      })
    );
  });

  it('should validate SHACL shapes', async () => {
    const validator = new OntologyValidator();
    const result = await validator.validate('./ontology/shapes/validation.ttl');

    expect(result.valid).toBe(true);
  });
});
```

### Code Generation Tests

```typescript
// codegen/tests/typescript-generator.test.ts
import { describe, it, expect } from 'vitest';
import { TypeScriptGenerator } from '../src/generators/typescript-gen';
import { OntologyParser } from '../src/parsers/rdf-parser';

describe('TypeScript Generator', () => {
  it('should generate correct interface for simple class', async () => {
    const parser = new OntologyParser();
    const ontology = await parser.parse('./test-fixtures/person.ttl');

    const generator = new TypeScriptGenerator();
    const output = generator.generate(ontology);

    expect(output).toContain('export interface Person {');
    expect(output).toContain('name: string;');
    expect(output).toContain('email: string;');
  });

  it('should handle optional properties', async () => {
    const parser = new OntologyParser();
    const ontology = await parser.parse('./test-fixtures/person-optional.ttl');

    const generator = new TypeScriptGenerator();
    const output = generator.generate(ontology);

    expect(output).toContain('middleName?: string;');
  });

  it('should generate array types for multi-valued properties', async () => {
    const parser = new OntologyParser();
    const ontology = await parser.parse('./test-fixtures/person-multi.ttl');

    const generator = new TypeScriptGenerator();
    const output = generator.generate(ontology);

    expect(output).toContain('phoneNumbers: string[];');
  });
});
```

### Integration Tests

```typescript
// tests/integration/ontology-to-ui.test.ts
import { describe, it, expect } from 'vitest';
import { render, screen } from '@testing-library/react';
import { runCodeGeneration } from '../codegen/src/cli';
import { PersonTable } from '../generated/components/PersonTable';

describe('Ontology to UI Integration', () => {
  it('should generate working CRUD table from ontology', async () => {
    // Generate code from test ontology
    await runCodeGeneration({
      ontologyPath: './test-fixtures/person.ttl',
      outputDir: './test-output',
    });

    // Import generated component
    const { PersonTable } = await import('./test-output/components/PersonTable');

    // Render component
    render(<PersonTable />);

    // Verify table renders with columns from ontology
    expect(screen.getByText('Name')).toBeInTheDocument();
    expect(screen.getByText('Email')).toBeInTheDocument();
  });
});
```

## Example Ontology

```turtle
# ontology/schema/entities.ttl
@prefix : <https://example.com/schema/> .
@prefix owl: <http://www.w3.org/2002/07/owl#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

:Person a owl:Class ;
  rdfs:label "Person" ;
  rdfs:comment "A human being" .

:name a owl:DatatypeProperty ;
  rdfs:label "Name" ;
  rdfs:domain :Person ;
  rdfs:range xsd:string .

:email a owl:DatatypeProperty ;
  rdfs:label "Email" ;
  rdfs:domain :Person ;
  rdfs:range xsd:string .

:dateOfBirth a owl:DatatypeProperty ;
  rdfs:label "Date of Birth" ;
  rdfs:domain :Person ;
  rdfs:range xsd:date .

:Organization a owl:Class ;
  rdfs:label "Organization" ;
  rdfs:comment "A structured group of people" .

:worksFor a owl:ObjectProperty ;
  rdfs:label "Works For" ;
  rdfs:domain :Person ;
  rdfs:range :Organization .
```

```turtle
# ontology/shapes/validation.ttl
@prefix sh: <http://www.w3.org/ns/shacl#> .
@prefix : <https://example.com/schema/> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

:PersonShape a sh:NodeShape ;
  sh:targetClass :Person ;
  sh:property [
    sh:path :name ;
    sh:datatype xsd:string ;
    sh:minCount 1 ;
    sh:maxCount 1 ;
    sh:minLength 1 ;
    sh:maxLength 100 ;
  ] ;
  sh:property [
    sh:path :email ;
    sh:datatype xsd:string ;
    sh:minCount 1 ;
    sh:maxCount 1 ;
    sh:pattern "^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}$" ;
  ] ;
  sh:property [
    sh:path :dateOfBirth ;
    sh:datatype xsd:date ;
    sh:maxCount 1 ;
  ] ;
  sh:property [
    sh:path :worksFor ;
    sh:class :Organization ;
    sh:maxCount 1 ;
  ] .
```

## Architecture Decision Records

### ADR-001: RDF as Single Source of Truth

**Status**: Accepted

**Context**: Need to maintain consistency between data models, types, validation, and UI.

**Decision**: Use RDF ontology as the single source of truth, with all code generated from it.

**Consequences**:
- ✅ Zero drift between schema and implementation
- ✅ Semantic richness for complex domains
- ✅ Standard validation with SHACL
- ⚠️ Learning curve for RDF/OWL
- ⚠️ Build-time code generation required

### ADR-002: TypeScript Generation over Runtime Reflection

**Status**: Accepted

**Context**: Need type safety while maintaining ontology as source of truth.

**Decision**: Generate TypeScript types at build time rather than using runtime reflection.

**Consequences**:
- ✅ Full TypeScript type safety
- ✅ IDE autocomplete and IntelliSense
- ✅ Compile-time error detection
- ⚠️ Requires regeneration on ontology changes
- ⚠️ Generated code must be in version control (or regenerated in CI)

### ADR-003: Git Hooks for Auto-Regeneration

**Status**: Accepted

**Context**: Developers may forget to regenerate code after ontology changes.

**Decision**: Use git hooks to automatically regenerate code when ontology files change.

**Consequences**:
- ✅ Automatic synchronization
- ✅ Prevents commits with stale generated code
- ✅ Clear developer workflow
- ⚠️ Longer commit times when ontology changes
- ⚠️ Requires one-time hook setup

### ADR-004: tRPC for Type-Safe API

**Status**: Accepted

**Context**: Need end-to-end type safety from frontend to backend.

**Decision**: Use tRPC with generated routers from ontology.

**Consequences**:
- ✅ Full type safety across API boundary
- ✅ No manual API contract maintenance
- ✅ Automatic client generation
- ⚠️ Tied to TypeScript ecosystem
- ⚠️ Learning curve for tRPC

### ADR-005: Runtime SHACL Validation

**Status**: Accepted

**Context**: Zod schemas may not capture all SHACL constraints.

**Decision**: Run SHACL validation at runtime in addition to Zod validation.

**Consequences**:
- ✅ Complete validation coverage
- ✅ Semantic validation rules
- ✅ Consistent with ontology
- ⚠️ Additional runtime overhead
- ⚠️ Requires Oxigraph in production

## Next Steps

1. **Prototype Implementation**
   - Implement RDF parser with Oxigraph
   - Create TypeScript generator
   - Build basic table template
   - Test end-to-end flow

2. **Expand Generators**
   - Add Zod schema generator
   - Add tRPC router generator
   - Add form component generator
   - Add Prisma schema generator

3. **Validation Layer**
   - Implement runtime SHACL validator
   - Add validation error formatting
   - Create validation middleware

4. **Developer Experience**
   - Add hot reload support
   - Create VSCode extension for ontology editing
   - Add CLI for common operations
   - Create starter templates

5. **Production Hardening**
   - Add comprehensive error handling
   - Implement caching strategy
   - Add monitoring and metrics
   - Create migration tools

---

**Document Status**: Draft v1.0
**Last Updated**: 2025-11-13
**Author**: System Architecture Designer
**Review Status**: Pending technical review
