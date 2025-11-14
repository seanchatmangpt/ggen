# Getting Started with ggen

**Goal:** Install ggen and generate your first code from an RDF ontology in under 5 minutes.

**What you'll learn:** The core ggen workflow: ontology → SPARQL queries → code generation.

## Prerequisites

- Rust 1.70+ (for Cargo installation) or Homebrew (macOS/Linux)
- Internet connection (for marketplace access)
- Basic command-line familiarity

## Step 1: Install ggen (1 minute)

Choose your installation method:

**Homebrew (Recommended):**
```bash
brew tap seanchatmangpt/tap
brew install ggen
```

**Cargo:**
```bash
cargo install ggen
```

**From Source:**
```bash
git clone https://github.com/seanchatmangpt/ggen
cd ggen
cargo install --path crates/ggen-cli --bin ggen --force
```

Verify installation:
```bash
ggen --version
# Should output: ggen 2.7.0
```

## Step 2: Generate Your First Ontology (1 minute)

Use AI to create an RDF ontology:

```bash
ggen ai generate-ontology \
  --prompt "Blog system: User (name, email), Post (title, content, author), Comment (text, author, post)" \
  --output blog.ttl
```

This creates `blog.ttl` with your domain model in RDF format.

## Step 3: Generate Rust Code (1 minute)

Generate Rust structs from the ontology:

```bash
ggen template generate-rdf \
  --ontology blog.ttl \
  --template rust-models \
  --output src/models.rs
```

**Generated code:**
```rust
use serde::{Deserialize, Serialize};
use uuid::Uuid;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct User {
    pub id: Uuid,
    pub name: String,
    pub email: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Post {
    pub id: Uuid,
    pub title: String,
    pub content: String,
    pub author_id: Uuid,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Comment {
    pub id: Uuid,
    pub text: String,
    pub author_id: Uuid,
    pub post_id: Uuid,
}
```

## Step 4: Modify Ontology and Regenerate (1 minute)

Edit `blog.ttl` to add a new field:

```turtle
ex:Post a rdfs:Class ;
    rdfs:label "Post" ;
    rdfs:comment "Blog post" .

ex:postPublishedAt a rdf:Property ;
    rdfs:domain ex:Post ;
    rdfs:range xsd:dateTime ;
    rdfs:label "published_at" .
```

Regenerate:
```bash
ggen template generate-rdf \
  --ontology blog.ttl \
  --template rust-models \
  --output src/models.rs
```

**Result:** The `Post` struct now includes `pub published_at: DateTime<Utc>` automatically!

## What You've Learned

1. **Ontology as single source of truth:** RDF defines your domain
2. **Automatic code generation:** Templates project ontology into code
3. **Change propagation:** Modify ontology → regenerate → code updates
4. **Multi-language support:** Same ontology can generate Rust, TypeScript, Python

## Next Steps

- **Learn the workflow:** [Ontology-to-Code Tutorial](ontology-to-code.md)
- **Use AI features:** [AI-Powered Generation Tutorial](ai-powered-generation.md)
- **Explore marketplace:** [Marketplace Workflow Tutorial](marketplace-workflow.md)
- **Installation details:** [Installation Guide](../how-to-guides/installation.md)

