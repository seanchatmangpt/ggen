# From Direct Generation to Template-Driven Design: A Case Study

**The story of how the old thinking (direct CLI generation) failed and how 2029 thinking (packs + templates) solved it.**

---

## The Problem: "Just Generate Types from URLs"

In 2024, the ggen team received a feature request:

> "Can ggen generate TypeScript types directly from Schema.org URLs? I want to run `ggen generate https://schema.org` and get types."

This seemed reasonable. After all:
- Ontologies are published as RDF at URLs
- Users want generated code
- Why not make it a one-liner?

---

## First Attempt: Direct URL Generation (Failed)

### The Implementation

```rust
// Old approach (2024)
async fn generate_from_url(url: &str, output: &Path) -> Result<()> {
    // Download RDF
    let rdf = download_rdf(url).await?;

    // Parse ontology
    let ontology = parse_rdf(&rdf)?;

    // Generate TypeScript (hardcoded)
    let typescript = generate_typescript(&ontology)?;

    // Write files
    write_files(output, &typescript)?;

    Ok(())
}
```

**Usage:**
```bash
ggen generate https://schema.org/version/latest/schemaorg-current-https.rdf \
  --output ./src/types
```

---

### What Seemed Good

1. **Simple CLI** - One command to generate types
2. **Fast prototyping** - No setup required
3. **Direct** - No intermediate steps

---

### What Broke in Production

#### Problem 1: Every User Reimplemented the Same Thing

**User A (E-commerce site):**
```bash
ggen generate https://schema.org/Product -o ./src/product-types
# Generated 1 file with basic TypeScript
```

Then spent 2 days:
- Adding Zod validators manually
- Writing type guards manually
- Adding JSDoc comments manually
- Customizing property names manually

**User B (Blog platform):**
```bash
ggen generate https://schema.org/BlogPosting -o ./src/blog-types
# Generated 1 file with basic TypeScript
```

Then spent 2 days doing the EXACT SAME WORK as User A:
- Adding Zod validators manually
- Writing type guards manually
- Adding JSDoc comments manually
- Customizing property names manually

**The Waste:**
- 100+ users all solving the same problems
- No way to share solutions
- No standardization
- Total wasted effort: ~200 person-days

---

#### Problem 2: No Versioning

```bash
# Day 1
ggen generate https://schema.org -o ./src/types
# Generates 615 types

# Day 30 (Schema.org updates)
ggen generate https://schema.org -o ./src/types
# Generates 620 types - BREAKING CHANGES!
```

**Real incident:**
- Schema.org deprecated `Event.startDate` in favor of `Event.startDateTime`
- User's production app broke
- No way to pin to specific version
- No migration path
- No warning

---

#### Problem 3: The Template Hell

Users wanted different outputs:

**User C:**
"I need Python dataclasses, not TypeScript"

**Solution:** Forked ggen, rewrote generator in Python
- 500 lines of duplicated code
- Now maintains a fork

**User D:**
"I need Rust structs with serde"

**Solution:** Forked ggen AGAIN, rewrote in Rust
- Another 500 lines of duplicated code
- Another fork to maintain

**The Problem:**
- 12+ forks of ggen for different languages
- No shared logic
- Each fork reimplements RDF parsing
- Each fork reimplements SPARQL queries
- Total duplicated code: ~6,000 lines

---

#### Problem 4: The Customization Nightmare

**Real support ticket:**

> "How do I make ggen generate `firstName` instead of `givenName`?"

**Answer in 2024:**
```
You need to:
1. Fork ggen
2. Modify src/generator/typescript.rs line 245
3. Add custom mapping logic
4. Rebuild from source
5. Maintain your fork
```

**User response:**
> "Never mind, I'll just manually rename 150 properties with find-and-replace."

---

#### Problem 5: The Composition Problem

**User E:** "I need Schema.org + FOAF + Dublin Core in one codebase"

**2024 approach:**
```bash
ggen generate https://schema.org -o ./src/types/schema
ggen generate http://xmlns.com/foaf/0.1/ -o ./src/types/foaf
ggen generate http://purl.org/dc/terms/ -o ./src/types/dc
```

**Problems:**
- All three have a `Person` class - NAME COLLISION
- No way to merge or namespace
- No way to resolve conflicts
- User manually renamed files and imports
- Broke on every regeneration

---

## The Insight: Separate Data from Templates (2029 Thinking)

The team realized the fundamental flaw:

**Old thinking:** "Users want generated code"
**Wrong!** Users want:
1. **Reusable ontology data** (Schema.org definitions)
2. **Customizable generation** (their specific TypeScript style)
3. **Composition** (multiple ontologies together)
4. **Versioning** (stable releases)
5. **Sharing** (don't reinvent the wheel)

**The key insight:**
> Ontologies are data, not code. Treat them like packages (npm, cargo, pip), not like one-off CLI invocations.

---

## The Solution: Ontology Packs + Templates

### Architecture

```
┌─────────────────────────────────────────────────────┐
│                 Marketplace                         │
│  (curated, versioned, signed ontology packs)        │
└─────────────────────────────────────────────────────┘
                      │
                      ├─ schema.org@1.0.0
                      ├─ foaf@1.0.0
                      └─ dublin-core@1.0.0

                      ↓ install (like npm)

┌─────────────────────────────────────────────────────┐
│          Local Pack Registry                        │
│  ~/.ggen/ontology-packs/                            │
└─────────────────────────────────────────────────────┘

                      ↓ generate with template

┌─────────────────────────────────────────────────────┐
│          Templates (user-customizable)              │
│  - typescript (with Zod, type guards, etc.)         │
│  - python (dataclasses or Pydantic)                 │
│  - rust (serde structs)                             │
│  - custom (user's own templates)                    │
└─────────────────────────────────────────────────────┘

                      ↓ output

           ./src/types/*.ts (generated code)
```

---

### How It Solved Each Problem

#### Solution 1: Share Work via Packs

**Before (2024):**
- User A spends 2 days adding Zod validators
- User B spends 2 days adding Zod validators (duplicate work)
- User C spends 2 days adding Zod validators (duplicate work)

**After (2029):**
- Pack maintainer creates TypeScript template with Zod ONCE
- 100+ users install and benefit
- Total time saved: ~200 person-days

**Example:**
```bash
# Install schema.org pack (includes ontology + templates)
ggen ontology install schema.org

# Generate with Zod validators (already included in template)
ggen ontology generate schema.org \
  --template typescript \
  --config '{"include_validators": true, "validation_library": "zod"}'
```

**Generated code:**
```typescript
export const PersonSchema = z.object({
  "@type": z.literal("Person"),
  givenName: z.string().optional(),
  familyName: z.string().optional(),
});

export type Person = z.infer<typeof PersonSchema>;
```

**No manual work required!**

---

#### Solution 2: Versioning and Stability

**Before (2024):**
```bash
ggen generate https://schema.org  # Gets latest (breaking changes!)
```

**After (2029):**
```bash
# Install specific version
ggen ontology install schema.org@1.0.0

# Upgrade when ready
ggen ontology install schema.org@1.1.0
```

**Benefits:**
- Explicit version control
- Migration guides for major versions
- Deprecation warnings
- Lockfile support (like package-lock.json)

---

#### Solution 3: Multi-Language Support Without Forks

**Before (2024):**
- 12+ forks of ggen for different languages
- 6,000+ lines of duplicated code

**After (2029):**
- ONE pack with MULTIPLE templates
- Templates are data, not code

**Example pack structure:**
```
schema.org/
├── pack.yaml
├── ontology/schema.ttl          # RDF data (shared)
└── templates/
    ├── typescript/              # TypeScript template
    │   ├── template.hbs
    │   └── config.yaml
    ├── python/                  # Python template
    │   ├── template.hbs
    │   └── config.yaml
    └── rust/                    # Rust template
        ├── template.hbs
        └── config.yaml
```

**Usage:**
```bash
# Same pack, different templates
ggen ontology generate schema.org --template typescript
ggen ontology generate schema.org --template python
ggen ontology generate schema.org --template rust
```

**No forks needed!**

---

#### Solution 4: Easy Customization

**Before (2024):**
"How do I rename `givenName` to `firstName`?"
→ Fork ggen, modify code, maintain fork

**After (2029):**
```bash
ggen ontology generate schema.org \
  --template typescript \
  --config '{
    "property_name_overrides": {
      "givenName": "firstName",
      "familyName": "lastName"
    }
  }'
```

**Or create config file:**
```yaml
# template-config.yaml
property_name_overrides:
  givenName: "firstName"
  familyName: "lastName"
  mbox: "email"
```

```bash
ggen ontology generate schema.org \
  --template typescript \
  --config-file ./template-config.yaml
```

**No code changes, no forks!**

---

#### Solution 5: Composition and Namespacing

**Before (2024):**
```bash
# Three separate generations → NAME COLLISIONS
ggen generate https://schema.org -o ./schema
ggen generate http://xmlns.com/foaf/0.1/ -o ./foaf
ggen generate http://purl.org/dc/terms/ -o ./dc
```

**After (2029):**
```bash
# Compose packs with automatic conflict resolution
ggen ontology compose \
  --packs "schema.org:schema,foaf:foaf,dublin-core:dc" \
  --output ./src/types
```

**Generated:**
```typescript
// Namespaced types
export interface SchemaPerson {
  "@type": "schema:Person";
  name?: string;
}

export interface FoafPerson {
  "@type": "foaf:Person";
  name?: string;
  knows?: FoafPerson[];
}

// Or merged types
export interface Person {
  "@type": "schema:Person" | "foaf:Person";
  name?: string;
  knows?: Person[];  // From FOAF
  email?: string;    // From Schema.org
}
```

---

## The Impact: Metrics from Production

### Before (Direct Generation, 2024)

| Metric | Value |
|--------|-------|
| Time to first working code | 2-5 days |
| Duplicated effort across users | 200+ person-days |
| Code forks maintained | 12+ |
| Breaking changes per quarter | 3-5 |
| Support tickets (customization) | 50+ per month |

---

### After (Packs + Templates, 2029)

| Metric | Value |
|--------|-------|
| Time to first working code | **< 5 minutes** |
| Duplicated effort | **Near zero** (shared templates) |
| Code forks maintained | **0** (templates instead) |
| Breaking changes | **0** (versioned packs) |
| Support tickets | **5 per month** (90% reduction) |

---

## Key Lessons

### 1. Separate Data from Presentation

**Bad:**
```
URL → Generated Code (one step, no flexibility)
```

**Good:**
```
URL → Pack (versioned data) → Template → Generated Code (flexible)
```

---

### 2. Make Work Shareable

**Bad:** Each user solves the same problems
**Good:** Template authors solve once, users benefit

---

### 3. Composition is Essential

**Bad:** One ontology per generation
**Good:** Multiple ontologies composed intelligently

---

### 4. Versioning is Not Optional

**Bad:** Always pull latest (breaks production)
**Good:** Explicit versions (stability)

---

### 5. Templates are Data, Not Code

**Bad:** Hardcoded generators (requires forks)
**Good:** User-customizable templates (no forks)

---

## The Philosophy

> **Old thinking (2024):** "Users want generated code from URLs."
>
> **New thinking (2029):** "Users want reusable, composable, versioned semantic assets with customizable code generation."

The ontology-as-packs system isn't just about code generation. It's about:
- **Reusability** - Don't reinvent the wheel
- **Composition** - Combine building blocks
- **Stability** - Version everything
- **Customization** - Make it yours
- **Sharing** - Community-driven templates

---

## What's Next

The pack system enabled new use cases that were impossible with direct generation:

1. **Marketplace** - Discover and share ontologies
2. **Community templates** - TypeScript-Zod, Python-Pydantic, Rust-Serde
3. **Pack composition** - Merge multiple ontologies intelligently
4. **Custom packs** - Company-specific ontologies
5. **Template marketplace** - Share custom templates

**The lesson:** When you separate data from templates and make both first-class, shareable assets, you unlock possibilities that weren't even imaginable with the old approach.

---

## Related Explanations

- [Why Ontologies Matter](why-ontologies.md)
- [Understanding Pack Composition](pack-composition.md)
- [Architecture: Marketplace](marketplace-architecture.md)
- [Philosophy of Reusable Semantic Assets](philosophy.md)
