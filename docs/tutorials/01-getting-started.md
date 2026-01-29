# Tutorial: Getting Started with ggen

**Learn the basics of ggen in 15 minutes.**

This tutorial teaches you the fundamental concepts and your first generation.

## What You'll Learn

- ✅ Core concepts: Ontologies, SPARQL, Templates
- ✅ Initialize your first ggen project
- ✅ Create a simple RDF ontology
- ✅ Generate code from your specification
- ✅ Iterate and improve your results

## Prerequisites

- Rust 1.75+ (or use pre-built binary)
- 5-10 minutes of time
- A text editor
- Basic familiarity with command line

## Step 1: Install ggen (2 minutes)

```bash
# Install via cargo
cargo install ggen

# Verify installation
ggen --version
# Output: ggen 3.2.0
```

**Note**: If you're in Claude Code Web, this is all you need. The Rust toolchain is pre-installed.

## Step 2: Create Your Project Directory (1 minute)

```bash
# Create a new directory for your project
mkdir ggen-hello-world
cd ggen-hello-world

# List current directory
ls -la
```

## Step 3: Initialize a ggen Project (1 minute)

```bash
# Initialize ggen in your project
ggen init

# See what was created
tree -a
# Or: find . -type f
```

**What was created:**

```
ggen-hello-world/
├── .ggen/                    # ggen working directory
│   ├── cache/               # Caching layer
│   ├── receipts/            # Generation receipts (proofs)
│   └── audit/               # Audit trails
├── .specify/                # RDF specifications (source of truth)
│   ├── specs/               # Feature specifications
│   │   └── 001-hello/
│   │       └── hello.ttl    # Example ontology
│   └── templates/           # Tera templates
├── ggen.toml                # Project manifest
└── README.md                # Your project README
```

## Step 4: Understand the Core Ontology (2 minutes)

Look at the example ontology:

```bash
# Read the example ontology
cat .specify/specs/001-hello/hello.ttl
```

**Expected output** (RDF/Turtle format):

```turtle
@prefix : <https://example.org/hello/> .
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .

# Define a simple greeting
:Greeting a :GreetingType ;
  :message "Hello, World!" ;
  :language "en" .
```

**What this means:**
- `@prefix`: Namespace declarations (like imports in code)
- `: <https://example.org/hello/>`: Base namespace for this ontology
- `:Greeting a :GreetingType`: Declares a "Greeting" of type "GreetingType"
- `:message "Hello, World!"`: Sets a property to a string value

**RDF = Resource Description Framework** - A way to describe things using triples (Subject-Predicate-Object):
- Subject: `:Greeting`
- Predicate: `:message`
- Object: `"Hello, World!"`

## Step 5: Preview Generation (2 minutes)

Preview what code will be generated without writing files:

```bash
# Dry-run: see what would be generated
ggen sync --dry_run true

# Output preview:
# File: src/greeting.rs
# [ Generated Rust code here ]
#
# File: README.md
# [ Generated documentation ]
```

**What happens in dry-run:**
1. ✅ Loads and validates `.specify/specs/*.ttl` files
2. ✅ Executes SPARQL queries to extract data
3. ✅ Renders Tera templates with SPARQL results
4. ✅ Shows output WITHOUT writing files
5. ✅ Generates audit trail (no timestamp yet)

## Step 6: Generate Your First Code (2 minutes)

Now generate the actual code:

```bash
# Generate code from ontology
ggen sync

# List generated files
find . -name "*.rs" -o -name "*.md" | head -20
```

**What was created:**

- `src/greeting.rs` - Generated Rust module
- `README.md` - Generated documentation
- `.ggen/receipts/latest.json` - Deterministic proof

## Step 7: Inspect Generated Code (2 minutes)

```bash
# View the generated Rust code
cat src/greeting.rs

# View the generated documentation
cat README.md

# View the generation receipt (proof)
cat .ggen/receipts/latest.json | jq '.'
```

**Receipt shows:**
- ✅ Execution ID (unique identifier)
- ✅ Manifest hash (SHA-256 of your config)
- ✅ Ontology hash (SHA-256 of your .ttl files)
- ✅ Files generated with content hashes
- ✅ Timing information
- ✅ Full audit trail

**Key insight**: The receipt proves what was generated, when, and why. Deterministic = same input always produces identical output.

## Step 8: Edit and Regenerate (3 minutes)

Now let's modify the ontology and regenerate:

```bash
# Edit the ontology
cat > .specify/specs/001-hello/hello.ttl <<'EOF'
@prefix : <https://example.org/hello/> .
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .

# Updated greeting
:Greeting a :GreetingType ;
  :message "Welcome to ggen!" ;
  :language "en" ;
  :version "1.0.0" .

# Add a second greeting
:GreetingES a :GreetingType ;
  :message "¡Bienvenido a ggen!" ;
  :language "es" .
EOF

# Preview changes
ggen sync --dry_run true

# Apply changes
ggen sync

# View updated code
cat src/greeting.rs
```

**What changed:**
- ✅ Updated message
- ✅ Added version
- ✅ Added Spanish greeting
- ✅ Generated code reflects all changes
- ✅ New receipt created with updated hash

## Step 9: Understand the Five-Stage Pipeline (2 minutes)

ggen runs through a deterministic pipeline:

```
Input                Pipeline Stages              Output
------               ----------------             ------

.specify/            μ₁ Normalize     ───→  Validated RDF graph
 *.ttl                  • SHACL validation
                        • Schema checking
                        • Dependency resolution

(RDF triples)        μ₂ Extract       ───→  Structured data
                        • SPARQL queries
                        • OWL inference
                        • Template context

                     μ₃ Emit          ───→  Raw artifacts
                        • Tera rendering
                        • Multi-file generation
                        • Code generation

                     μ₄ Canonicalize  ───→  Formatted code
                        • rustfmt/prettier
                        • Syntax validation
                        • Content hashing

                     μ₅ Receipt       ───→  Proof + audit trail
                        • SHA-256 hashing
                        • Execution ID
                        • Audit log (JSON)
```

Each stage is **deterministic** = same input → identical output

## Step 10: Clean Up and Summary (1 minute)

```bash
# View your complete project
tree -a

# Clean generated files (optional)
rm -rf src .ggen/receipts/*

# Or keep for reference
```

**What you've learned:**

| Concept | What It Is | Example |
|---------|-----------|---------|
| **Ontology** | RDF specification (source of truth) | `.specify/specs/hello.ttl` |
| **Triple** | RDF unit (Subject-Predicate-Object) | `:Greeting :message "Hello"` |
| **SPARQL** | Query language for RDF | `SELECT ?msg WHERE { ?s :message ?msg }` |
| **Tera** | Template engine | `{{ greeting.message }}` |
| **Pipeline** | 5-stage generation (μ₁-μ₅) | Normalize → Extract → Emit → Canonicalize → Receipt |
| **Receipt** | Deterministic proof | `latest.json` with SHA-256 hashes |
| **Determinism** | Same input = same output | Always reproducible |

## Next: Build Something Real

Ready to build a real project? Continue with:

- 📖 **[Tutorial 2: Your First REST API](02-first-project.md)** - Generate an actual REST service
- 🔧 **[How-To: Common Tasks](../how-to/01-common-tasks.md)** - REST APIs, CLI tools, microservices
- 📚 **[Command Reference](../reference/01-commands.md)** - All ggen commands explained
- 💡 **[Concepts Guide](../explanation/01-concepts.md)** - Deep dive into RDF, SPARQL, templates

## Troubleshooting

### Issue: "ontology not found"

```bash
# Ensure .specify/specs directory exists
ls -la .specify/specs/

# If empty, run: ggen init
```

### Issue: "SPARQL query failed"

```bash
# Enable debug logging
GGEN_LOG_LEVEL=debug ggen sync

# Check .ttl syntax
# RDF is strict about prefixes and syntax
```

### Issue: "generation timed out"

```bash
# Reduce ontology complexity
# Split large .ttl files
# Use --validate_only for quick checks
ggen sync --validate_only true
```

---

## Key Takeaways

✅ **Ontologies are source of truth** - Edit `.ttl` files, not generated code

✅ **Deterministic** - Same spec = identical output every time

✅ **Receipts prove generation** - Cryptographic hashes verify reproducibility

✅ **Five-stage pipeline** - Normalize → Extract → Emit → Canonicalize → Receipt

✅ **RDF is powerful** - Ontologies encode domain knowledge that generates code

---

**You're now ready to generate code! Continue with Tutorial 2: Your First REST API.** 🚀
