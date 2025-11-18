# YAWL Editor - Next.js + shadcn/ui + SPARQL

A modern web-based YAWL (Yet Another Workflow Language) editor built with Next.js 14, shadcn/ui components, and SPARQL for flexible data querying. This application provides a complete workflow management system with ontology-driven architecture.

## 🎯 Features

- **Case Management**: Create and manage workflow case instances
- **Workitem Tracking**: Monitor tasks assigned to resources
- **Process Definition**: Define and configure workflow processes
- **Resource Management**: Manage users, roles, and allocations
- **SPARQL Integration**: Query and update workflow data via SPARQL endpoints
- **RDF/OWL Ontology**: Complete semantic workflow model
- **Type-Safe**: Full TypeScript support with Zod validation
- **shadcn/ui Components**: Beautiful, accessible UI components
- **Responsive Design**: Works on desktop, tablet, and mobile

## 🚀 Quick Start

```bash
# Install dependencies
npm install

# Configure SPARQL endpoint
cp .env.example .env.local
# Edit .env.local to point to your SPARQL endpoint

# Start development server
npm run dev

# Open browser
open http://localhost:3000
```

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
io.ggen.nextjs.ontology-crud/
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
ggen marketplace install io.ggen.nextjs.ontology-crud
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
# Run integration tests (from ggen root)
cargo test --test nextjs_ontology_sync -- --nocapture
```

## 📝 License

MIT

---

**Built with ggen's ontology-driven development** 🚀

Install now: `ggen marketplace install io.ggen.nextjs.ontology-crud`
