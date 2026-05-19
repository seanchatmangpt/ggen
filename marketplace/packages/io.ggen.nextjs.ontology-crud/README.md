# Next.js Ontology-Driven CRUD Application

**Full-stack Next.js + shadcn/ui application where your RDF ontology is the single source of truth. CRUD tables automatically sync with ontology changes via git hooks.**

## 🚀 Quick Start

```bash
# Install via mcpp marketplace
mcpp marketplace install io.mcpp.nextjs.ontology-crud

# Navigate to installed package
cd ~/.mcpp/marketplace/io.mcpp.nextjs.ontology-crud

# Install dependencies
npm install

# Generate code from ontology
npm run regenerate

# Start development server
npm run dev
# → Visit http://localhost:3000
```

## ✨ Features

- **Next.js 14** with App Router
- **shadcn/ui** components (DataTable, Dialog, Form)
- **TypeScript** strict mode with auto-generated types
- **Zod** validation from SHACL constraints
- **Git hooks** for automatic regeneration
- **Zero drift** between ontology and UI
- **Production-ready** validation and error handling

## 🎯 The Workflow

### 1. Ontology Defines Everything

```turtle
# ontology/task-management.ttl
ex:Task a owl:Class ;
    rdfs:label "Task" .

ex:priority a owl:DatatypeProperty ;
    rdfs:domain ex:Task ;
    rdfs:range xsd:integer ;
    sh:minInclusive 1 ;
    sh:maxInclusive 5 .
```

### 2. Auto-Generate Code

```bash
npm run regenerate
```

Generates:
- ✅ TypeScript types (`lib/types.ts`)
- ✅ Zod schemas (`lib/validation.ts`)
- ✅ API routes (`app/api/*/route.ts`)
- ✅ CRUD tables (`components/generated/`)

### 3. Git Hooks Keep It Synced

```bash
# Edit ontology
vim ontology/task-management.ttl

# Commit
git commit -m "feat: add estimatedHours"

# Hook automatically regenerates code!
```

## 📁 What You Get

```
io.mcpp.nextjs.ontology-crud/
├── app/                    # Next.js app
│   ├── api/               # Generated API routes
│   ├── tasks/             # Generated CRUD pages
│   └── projects/          # Generated CRUD pages
├── components/
│   ├── ui/                # shadcn components
│   └── generated/         # Generated CRUD tables
├── lib/
│   ├── types.ts           # Generated TypeScript types
│   └── validation.ts      # Generated Zod schemas
├── ontology/
│   └── task-management.ttl # Source of truth
├── templates/             # Handlebars templates
│   ├── types.ts.hbs
│   ├── api-routes.ts.hbs
│   └── crud-table.tsx.hbs
├── scripts/
│   └── regenerate-from-ontology.sh
└── hooks/
    ├── pre-commit
    └── post-merge
```

## 🎨 Example: Add a New Property

```bash
# 1. Add to ontology
cat >> ontology/task-management.ttl << 'EOT'
ex:estimatedHours a owl:DatatypeProperty ;
    rdfs:domain ex:Task ;
    rdfs:range xsd:integer ;
    sh:minInclusive 0 .
EOT

# 2. Regenerate
npm run regenerate

# 3. Magic! ✨
# - TypeScript: estimatedHours: number
# - Zod: .int().min(0)
# - CRUD table: "Estimated Hours" column
# - Form: number input with validation
```

## 📦 Installation via Marketplace

### Install the Package

```bash
mcpp marketplace install io.mcpp.nextjs.ontology-crud
```

### What Gets Installed

- Complete Next.js application
- Sample task management ontology
- Code generation templates
- Git hooks for automation
- All dependencies configured

### Customize for Your Domain

```bash
# Edit the ontology for your use case
vim ontology/task-management.ttl

# Add your entities (Product, Order, Patient, etc.)
# Add your properties
# Add SHACL validation

# Regenerate
npm run regenerate

# You now have a custom CRUD app!
```

## 🔧 npm Scripts

| Script | Description |
|--------|-------------|
| `npm run dev` | Start development server |
| `npm run build` | Build for production |
| `npm run start` | Start production server |
| `npm run regenerate` | Generate code from ontology |
| `npm run setup-hooks` | Install git hooks |
| `npm run validate-ontology` | Validate RDF/SHACL |

## 🎓 Use Cases

### E-Commerce Platform
```bash
# Create Product, Order, Customer ontology
# Get full shopping platform with CRUD
```

### Healthcare System
```bash
# Create Patient, Appointment, Prescription ontology
# Get FHIR-compliant medical records
```

### Project Management
```bash
# Create Project, Task, User ontology (already included!)
# Get Jira-like task tracking
```

## 📊 Architecture

```
Ontology (RDF/SHACL)
    ↓
SPARQL Queries
    ↓
Handlebars Templates
    ↓
Generated Code (TypeScript + React)
    ↓
Next.js Application (Type-safe CRUD)
```

## 🎯 Benefits

✅ **Zero Drift** - Ontology → Code (always in sync)  
✅ **Type Safety** - TypeScript + Zod validation  
✅ **Rapid Development** - Add entity → Get CRUD automatically  
✅ **Production Ready** - Error handling, loading states  
✅ **Maintainable** - Single source of truth  

## 📚 Documentation

- [Full Architecture](../../docs/ONTOLOGY_NEXTJS_ARCHITECTURE.md)
- [Quick Start Guide](../../docs/ONTOLOGY_NEXTJS_QUICKSTART.md)
- [Template Reference](templates/README.md)
- [Hooks Guide](hooks/HOOKS_GUIDE.md)

## 🧪 Testing

```bash
# Run integration tests (from mcpp root)
cargo test --test nextjs_ontology_sync -- --nocapture
```

## 📝 License

MIT

---

**Built with mcpp's ontology-driven development** 🚀

Install now: `mcpp marketplace install io.mcpp.nextjs.ontology-crud`
