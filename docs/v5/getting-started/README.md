# Getting Started with ggen

Welcome! This guide will get you from zero to generating code in **15 minutes**.

## What You'll Learn

By the end of this guide, you'll:
- ✅ Install ggen
- ✅ Understand what RDF ontologies are (briefly)
- ✅ Create your first domain model
- ✅ Generate working code from RDF
- ✅ Understand the ggen.toml project manifest
- ✅ See real examples from production systems

## Prerequisites

- **Rust 1.75+** installed ([rustup.rs](https://rustup.rs))
- **Basic command-line knowledge**
- **No RDF knowledge required** (we'll teach you the basics)

## Path Overview

```
Installation (2 min)
    ↓
Understand RDF (5 min)
    ↓
Your First Generation (8 min)
    ↓
Explore Real Examples
```

---

## Step 1: Installation (2 minutes)

### Option A: Install via Cargo (Recommended)

```bash
cargo install ggen
```

Verify installation:
```bash
ggen --version
# Expected output: ggen 5.x.x
```

### Option B: Build from Source

```bash
git clone https://github.com/seanchatmangpt/ggen.git
cd ggen
cargo build --release
sudo cp target/release/ggen /usr/local/bin/
```

### Verify Installation

```bash
ggen --help
```

Expected output:
```
ggen
Ontology-driven code generation

USAGE:
    ggen <COMMAND>

COMMANDS:
    sync        Generate code from ontology
    validate    Validate ontology structure
    query       Execute SPARQL query
    help        Print this message
```

✅ **Installation complete!**

---

## Step 2: Understand RDF (5 minutes)

### What is RDF?

**RDF (Resource Description Framework)** is a way to describe structured data using triples:

```
Subject → Predicate → Object
```

Think of it like:
- **Subject**: "What we're talking about" (e.g., `User`)
- **Predicate**: "Property or relationship" (e.g., `hasEmail`)
- **Object**: "Value or related thing" (e.g., `"alice@example.com"`)

### Why RDF for Code Generation?

| Feature | JSON Schema | RDF Ontology |
|---------|-------------|--------------|
| **Semantic precision** | ❌ Loose types | ✅ Rich semantic types |
| **Reasoning** | ❌ None | ✅ Inference, validation |
| **Reusability** | ❌ Copy-paste | ✅ Import & extend |
| **Validation** | ⚠️ Basic | ✅ SHACL constraints |
| **Determinism** | ⚠️ Depends | ✅ Guaranteed |

### Example: User Model

**JSON Schema (ambiguous):**
```json
{
  "type": "object",
  "properties": {
    "email": { "type": "string" }
  }
}
```
**Question:** Is `email` required? Validated? Unique?

**RDF Ontology (precise):**
```turtle
:User a rdfs:Class ;
  :fields (
    [ :name "email" ; :type "String" ; :required true ; :unique true ; :validation "email_format" ]
  ) .
```
**Answer:** Yes, yes, yes, yes!

### RDF Formats

ggen v5 supports all standard formats:
- **Turtle** (`.ttl`) - Human-friendly, recommended
- **N3** (`.n3`) - Turtle + rules
- **RDF/XML** (`.rdf`) - XML-based
- **JSON-LD** (`.jsonld`) - JSON-based
- **TriG** (`.trig`) - Named graphs

**We'll use Turtle** in this guide (most readable).

✅ **RDF concepts understood!**

---

## Step 3: Your First Generation (8 minutes)

Let's generate a simple document from an RDF ontology.

### Create Project Directory

```bash
mkdir my-first-ggen
cd my-first-ggen
mkdir ontology templates output
```

### Create Ontology Schema (ontology/book-schema.ttl)

First, define your domain vocabulary:

```turtle
@prefix : <http://example.com/book#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .

# Define a Book class
:Book a rdfs:Class ;
    rdfs:label "Book" ;
    rdfs:comment "A book with chapters" .

:Chapter a rdfs:Class ;
    rdfs:label "Chapter" ;
    rdfs:comment "A chapter within a book" .

# Properties
:title a rdf:Property ;
    rdfs:label "title" ;
    rdfs:range rdfs:Literal .

:author a rdf:Property ;
    rdfs:label "author" ;
    rdfs:range rdfs:Literal .

:hasChapter a rdf:Property ;
    rdfs:label "has chapter" ;
    rdfs:domain :Book ;
    rdfs:range :Chapter .

:chapterNumber a rdf:Property ;
    rdfs:label "chapter number" ;
    rdfs:domain :Chapter ;
    rdfs:range rdfs:Literal .

:content a rdf:Property ;
    rdfs:label "content" ;
    rdfs:range rdfs:Literal .
```

### Create Content (ontology/my-book.ttl)

Now add your actual content:

```turtle
@prefix : <http://example.com/book#> .

:MyBook a :Book ;
    :title "Introduction to ggen" ;
    :author "Your Name" ;
    :hasChapter :Chapter1, :Chapter2 .

:Chapter1 a :Chapter ;
    :chapterNumber 1 ;
    :title "Getting Started" ;
    :content "ggen is a deterministic code generator..." .

:Chapter2 a :Chapter ;
    :chapterNumber 2 ;
    :title "RDF Basics" ;
    :content "RDF uses triples to model knowledge..." .
```

### Create Template (templates/book.md.tera)

Create a Tera template to render Markdown:

```markdown
# {% raw %}{{ title }}{% endraw %}

**Author:** {% raw %}{{ author }}{% endraw %}

{% raw %}{% for chapter in chapters %}{% endraw %}
## Chapter {% raw %}{{ chapter.number }}{% endraw %}: {% raw %}{{ chapter.title }}{% endraw %}

{% raw %}{{ chapter.content }}{% endraw %}

{% raw %}{% endfor %}{% endraw %}
```

### Create ggen.toml (Project Manifest)

Create `ggen.toml`:

```toml
[project]
name = "my-first-book"
version = "1.0.0"

[rdf]
ontology_file = "ontology/my-book.ttl"
schema_file = "ontology/book-schema.ttl"
base_uri = "http://example.com/book#"

[templates]
source_dir = "templates"
output_dir = "output"

[sparql]
timeout = 30
```

**What this says:**
- "My ontology content is in `my-book.ttl`"
- "My schema is in `book-schema.ttl`"
- "Templates are in `templates/`, output to `output/`"

### Generate Your Document

```bash
ggen sync
```

Expected output:
```
✓ Loaded ontology: 15 triples
✓ Rendered template: book.md.tera
✓ Generated: output/book.md
Complete in 124ms
```

### Inspect Generated Output

Check `output/book.md`:

```markdown
# Introduction to ggen

**Author:** Your Name

## Chapter 1: Getting Started

ggen is a deterministic code generator...

## Chapter 2: RDF Basics

RDF uses triples to model knowledge...
```

✅ **Document generated successfully!**

### Verify Determinism

Run generation again:

```bash
ggen sync
```

Check the output—it should be **bit-for-bit identical**:

```bash
md5sum output/book.md
# Same hash every time = deterministic!
```

---

## Next Steps

🎉 **Congratulations!** You've generated your first document with ggen!

### Explore Real Examples

Now that you understand the basics, see what ggen can really do:

**📚 [Thesis Generation](../examples/thesis-generation/)**
- **What**: 43-page PhD dissertation with chapters, theorems, equations, bibliography
- **Generated**: LaTeX → PDF
- **Ontology**: 142+ triples defining document structure
- **Result**: Production-ready academic publication
- **Meta**: This thesis was generated using ggen itself!

**🔄 [ASTRO State Machine](../examples/astro-state-machine/)**
- **What**: Distributed order processing system
- **Generated**: Rust state machine code (47 states, 128 transitions)
- **Ontology**: Workflow states, transitions, guards, actions
- **Result**: 73% defect reduction in production

**🌐 [TanStack Web App](../examples/tanstack-webapp/)**
- **What**: Full-stack web application
- **Generated**: TypeScript routes, queries, schemas, SQL migrations
- **Ontology**: Application domain model
- **Result**: Type-safe full-stack application

### Learn More

**I want to learn RDF properly:**
→ [RDF Basics Tutorial](../tutorials/01-rdf-basics.md) (15 minutes, no prerequisites)

**I want to generate a complete document:**
→ [Thesis Generation Tutorial](../tutorials/02-thesis-generation.md) (30 minutes)

**I want to generate code, not documents:**
→ [ASTRO State Machines Tutorial](../tutorials/03-astro-state-machines.md)

**I want to customize templates:**
→ [Custom Templates How-To](../how-to-guides/customize-templates.md)

**I want to understand the theory:**
→ [Zero-Drift Theorem](../explanations/zero-drift-theorem.md) - Mathematical foundations

### Resources

- **[Reference Documentation](../reference/)** - Complete CLI, ggen.toml, SPARQL reference
- **[Examples](../examples/)** - Working code examples from production systems
- **[GitHub Issues](https://github.com/seanchatmangpt/ggen/issues)** - Report bugs
- **[GitHub Discussions](https://github.com/seanchatmangpt/ggen/discussions)** - Ask questions

### Key Takeaways

✅ **RDF ontologies** = Single source of truth
✅ **SPARQL queries** = Extract structured data
✅ **Tera templates** = Render any format
✅ **Determinism** = Same input → identical output (always)
✅ **Production-proven** = Real systems with measurable results

---

**Time to first generation: ~15 minutes** ✅

**Next:** Try generating the [thesis example](../examples/thesis-generation/) to see the full power of ggen!
