# Ontology-Driven Next.js Development - Quick Start Guide

## 🎯 What You Get

A **complete Next.js application** where your RDF ontology is the single source of truth. When you modify the ontology, your TypeScript types, API routes, validation schemas, and UI components **automatically regenerate** with zero drift.

## ⚡ 60-Second Demo

```bash
# Install via marketplace (recommended)
ggen marketplace install io.ggen.nextjs.ontology-crud
cd ~/.ggen/marketplace/io.ggen.nextjs.ontology-crud

# Or use the example directly
cd docs/examples/nextjs-ontology-app

# Install dependencies
npm install

# Generate code from ontology
npm run regenerate

# Start development server
npm run dev
# → Visit http://localhost:3000

# See Tasks and Projects pages with working CRUD operations
```

## 🎓 The Workflow

### 1. Ontology is Source of Truth

Your RDF ontology defines everything:

```turtle
# ontology/task-management.ttl
ex:Task a owl:Class ;
    rdfs:label "Task" ;
    rdfs:comment "A task in the system" .

ex:title a owl:DatatypeProperty ;
    rdfs:domain ex:Task ;
    rdfs:range xsd:string ;
    sh:minLength 1 ;
    sh:maxLength 200 .

ex:priority a owl:DatatypeProperty ;
    rdfs:domain ex:Task ;
    rdfs:range xsd:integer ;
    sh:minInclusive 1 ;
    sh:maxInclusive 5 .
```

### 2. Git Hooks Auto-Regenerate

When you commit ontology changes:

```bash
# Edit ontology
vim ontology/task-management.ttl

# Commit
git commit -m "feat: add estimatedHours to Task"

# Git hook automatically:
# ✅ Validates ontology
# ✅ Generates TypeScript types
# ✅ Generates Zod schemas
# ✅ Generates API routes
# ✅ Generates CRUD components
# ✅ Runs type checking
# ✅ Stages generated files
```

### 3. UI Stays in Perfect Sync

**Before:**
```typescript
interface Task {
  id: string;
  title: string;
  priority: number;
}
```

**Add to ontology:**
```turtle
ex:estimatedHours a owl:DatatypeProperty ;
    rdfs:domain ex:Task ;
    rdfs:range xsd:integer .
```

**After regeneration:**
```typescript
interface Task {
  id: string;
  title: string;
  priority: number;
  estimatedHours: number;  // ✨ Added automatically
}
```

The CRUD table, form, and API routes all update automatically!

## 📁 What Was Generated

### From Your Ontology

**Input:** `ontology/task-management.ttl` (RDF/SHACL)

**Output:**
1. **TypeScript Types** (`lib/types.ts`)
   ```typescript
   export interface Task {
     id: string;
     title: string;
     description?: string;
     status: "todo" | "in-progress" | "done";
     priority: number;
   }
   ```

2. **Zod Validation** (`lib/validation.ts`)
   ```typescript
   export const TaskSchema = z.object({
     title: z.string().min(1).max(200),
     priority: z.number().int().min(1).max(5),
     status: z.enum(["todo", "in-progress", "done"]),
   });
   ```

3. **API Routes** (`app/api/tasks/route.ts`)
   ```typescript
   export async function POST(request: Request) {
     const body = await request.json();
     const task = TaskSchema.parse(body);
     // Create task...
   }
   ```

4. **CRUD Components** (`components/generated/TaskCRUDTable.tsx`)
   ```typescript
   // Full DataTable with:
   // - Columns from ontology properties
   // - Sorting, filtering, pagination
   // - Create/Edit/Delete dialogs
   // - Zod-validated forms
   ```

## 🎨 shadcn/ui Components

All UI components use shadcn/ui:
- **Table** - DataTable with sorting/filtering
- **Dialog** - Create/Edit modals
- **Form** - Validated forms with react-hook-form
- **Button** - Actions (Edit, Delete)
- **Badge** - Status/Priority indicators
- **Input** - Form fields
- **Select** - Dropdowns for enums

## 🔧 Key Files

| File | Purpose |
|------|---------|
| `ontology/task-management.ttl` | **Source of truth** (RDF/SHACL) |
| `scripts/regenerate-from-ontology.sh` | Code generation script |
| `.git/hooks/pre-commit` | Auto-regenerate on commit |
| `docs/examples/templates/*.hbs` | Handlebars templates |
| `lib/types.ts` | **Generated** TypeScript types |
| `lib/validation.ts` | **Generated** Zod schemas |
| `app/api/*/route.ts` | **Generated** API routes |
| `components/generated/*` | **Generated** CRUD components |

## 🚀 Try It Yourself

### Example 1: Add a New Property

```bash
# 1. Add to ontology
cat >> ontology/task-management.ttl << 'EOT'
ex:estimatedHours a owl:DatatypeProperty ;
    rdfs:label "estimatedHours" ;
    rdfs:domain ex:Task ;
    rdfs:range xsd:integer ;
    sh:minInclusive 0 .
EOT

# 2. Regenerate
npm run regenerate

# 3. See the magic!
# - TypeScript interface has estimatedHours: number
# - Zod schema has .int().min(0)
# - CRUD table has "Estimated Hours" column
# - Form has number input with validation
```

### Example 2: Add Validation

```bash
# Add max length to title
# In ontology, change:
sh:maxLength 200  # to
sh:maxLength 100

# Regenerate
npm run regenerate

# Now form validation enforces 100 char limit!
```

### Example 3: Add New Entity

```bash
# Add Tag entity to ontology
cat >> ontology/task-management.ttl << 'EOT'
ex:Tag a owl:Class ;
    rdfs:label "Tag" .

ex:tagName a owl:DatatypeProperty ;
    rdfs:domain ex:Tag ;
    rdfs:range xsd:string .

ex:hasTags a owl:ObjectProperty ;
    rdfs:domain ex:Task ;
    rdfs:range ex:Tag .
EOT

# Regenerate
npm run regenerate

# You get:
# - Tag TypeScript interface
# - Tag API routes
# - Tag CRUD table
# - Tag relationship in Task form
```

## 📊 Architecture Flow

```
┌─────────────────────────┐
│  RDF Ontology (Source)  │
│  task-management.ttl    │
└───────────┬─────────────┘
            │
            ▼
┌─────────────────────────┐
│  SPARQL Queries         │
│  Extract structure      │
└───────────┬─────────────┘
            │
            ▼
┌─────────────────────────┐
│  Handlebars Templates   │
│  types/api/components   │
└───────────┬─────────────┘
            │
            ▼
┌─────────────────────────┐
│  Generated Code         │
│  TypeScript + React     │
└───────────┬─────────────┘
            │
            ▼
┌─────────────────────────┐
│  Next.js Application    │
│  Type-safe CRUD         │
└─────────────────────────┘
```

## 🎯 Benefits

### Zero Drift
- Ontology change → Automatic code update
- No manual synchronization
- No type mismatches

### Type Safety
- TypeScript interfaces from RDF classes
- Zod validation from SHACL constraints
- Runtime and compile-time checks

### Rapid Development
- Add entity → Get full CRUD automatically
- Add property → UI updates automatically
- Change validation → Forms update automatically

### Production Ready
- SHACL validation rules → Zod schemas
- Error handling
- Loading states
- Accessible UI (shadcn/ui)

## 📚 Documentation

- **[Full Architecture](ONTOLOGY_NEXTJS_ARCHITECTURE.md)** - System design
- **[Hooks Guide](examples/hooks/HOOKS_GUIDE.md)** - Git automation
- **[Template Reference](examples/templates/README.md)** - Code generation
- **[Integration Test](../tests/integration/nextjs_ontology_sync.rs)** - Validation

## 🧪 Run the Integration Test

```bash
# Validates complete workflow:
# 1. Base ontology → Generate code
# 2. Modify ontology → Regenerate
# 3. Verify types, schemas, components
# 4. Check idempotency (no drift)

cargo test --test nextjs_ontology_sync -- --nocapture
```

## 🎉 Next Steps

1. **Explore** the example app (`docs/examples/nextjs-ontology-app`)
2. **Modify** the ontology to add your entities
3. **Run** `npm run regenerate` to see changes
4. **Setup** git hooks with `npm run setup-hooks`
5. **Build** your production app!

## 💡 Real-World Use Cases

### E-Commerce
```turtle
ex:Product, ex:Order, ex:Customer, ex:Review
→ Full shopping platform with CRUD
```

### Healthcare
```turtle
ex:Patient, ex:Appointment, ex:Prescription
→ FHIR-compliant medical records system
```

### Project Management
```turtle
ex:Project, ex:Task, ex:User, ex:Comment
→ Jira-like task tracking
```

## 🔗 Resources

- **Next.js Docs**: https://nextjs.org/docs
- **shadcn/ui**: https://ui.shadcn.com
- **RDF Primer**: https://www.w3.org/TR/rdf11-primer/
- **SHACL**: https://www.w3.org/TR/shacl/

---

**Built with ggen's ontology-driven development** 🚀
