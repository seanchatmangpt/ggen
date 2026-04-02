<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [Getting Started: Your First Ontology Pack](#getting-started-your-first-ontology-pack)
  - [Step 1: Search for Available Packs](#step-1-search-for-available-packs)
  - [Step 2: Install SCHEMA.org Pack](#step-2-install-schemaorg-pack)
  - [Step 3: List Installed Packs](#step-3-list-installed-packs)
  - [Step 4: Generate Code from the Ontology](#step-4-generate-code-from-the-ontology)
  - [Step 5: Use Generated Types](#step-5-use-generated-types)
  - [What You Learned](#what-you-learned)
  - [Next Steps](#next-steps)
  - [Troubleshooting](#troubleshooting)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Getting Started: Your First Ontology Pack

**Goal:** Install and use your first ontology pack in under 10 minutes.

**What you'll learn:**
- How to search for ontology packs
- How to install a pack
- How to generate code from an ontology
- How to use generated code in your project

**Prerequisites:**
- ggen CLI installed (`cargo install ggen-cli`)
- Basic familiarity with command line

---

## Step 1: Search for Available Packs

Let's see what ontology packs are available:

```bash
ggen ontology search
```

**Expected output:**
```
Available Ontology Packs:
  schema.org (v1.0.0) - Schema.org vocabulary for structured data
  foaf (v1.0.0) - Friend of a Friend social network ontology
  dublin-core (v1.0.0) - Dublin Core metadata terms
  ...
```

You should see a list of official ontology packs. Each pack represents a well-known semantic vocabulary.

---

## Step 2: Install SCHEMA.org Pack

Let's install the Schema.org ontology pack:

```bash
ggen ontology install schema.org
```

**What happens:**
1. ggen downloads the pack metadata
2. Verifies the pack signature
3. Installs to `~/.ggen/ontology-packs/schema.org/`
4. Registers the pack in your local registry

**Expected output:**
```
Installing ontology pack: schema.org v1.0.0
✓ Downloaded pack metadata
✓ Verified signature
✓ Installed to ~/.ggen/ontology-packs/schema.org/
✓ Pack ready to use
```

---

## Step 3: List Installed Packs

Verify the installation:

```bash
ggen ontology list
```

**Expected output:**
```
Installed Ontology Packs:
  schema.org (v1.0.0)
    Location: ~/.ggen/ontology-packs/schema.org/
    Templates: 3 (typescript, rust, python)
```

---

## Step 4: Generate Code from the Ontology

Now let's generate TypeScript types from Schema.org:

```bash
ggen ontology generate schema.org \
  --template typescript \
  --output ./src/schema
```

**What happens:**
1. ggen loads the Schema.org ontology from the pack
2. Applies the TypeScript template
3. Generates `.ts` files with type definitions
4. Outputs to `./src/schema/`

**Expected output:**
```
Generating code from schema.org...
✓ Loaded ontology (615 classes, 1453 properties)
✓ Applied template: typescript
✓ Generated 615 type definitions
✓ Output: ./src/schema/

Generated files:
  ./src/schema/index.ts
  ./src/schema/types/Person.ts
  ./src/schema/types/Organization.ts
  ./src/schema/types/Event.ts
  ...
```

---

## Step 5: Use Generated Types

Open `./src/schema/types/Person.ts`:

```typescript
/**
 * A person (alive, dead, undead, or fictional).
 * @see https://schema.org/Person
 */
export interface Person extends Thing {
  "@type": "Person";

  /** Given name. In the U.S., the first name of a Person. */
  givenName?: string;

  /** Family name. In the U.S., the last name of a Person. */
  familyName?: string;

  /** Email address. */
  email?: string;

  /** The job title of the person. */
  jobTitle?: string;

  // ... more properties
}
```

Create a new file `./src/example.ts`:

```typescript
import { Person } from './schema/types/Person';

const author: Person = {
  "@type": "Person",
  givenName: "Jane",
  familyName: "Doe",
  email: "jane@example.com",
  jobTitle: "Software Engineer"
};

console.log(author);
```

Run it:

```bash
npx ts-node src/example.ts
```

**Expected output:**
```json
{
  "@type": "Person",
  "givenName": "Jane",
  "familyName": "Doe",
  "email": "jane@example.com",
  "jobTitle": "Software Engineer"
}
```

---

## What You Learned

- ✅ How to search for ontology packs
- ✅ How to install a pack (`ggen ontology install`)
- ✅ How to list installed packs (`ggen ontology list`)
- ✅ How to generate code (`ggen ontology generate`)
- ✅ How to use generated types in your code

---

## Next Steps

- **Learn more about Schema.org:** [Tutorial: Using SCHEMA.org](02-using-schema-org.md)
- **Create your own pack:** [Tutorial: Building Your Own Pack](03-building-custom-pack.md)
- **Customize generation:** [How to: Customize Code Generation](../how-to/customize-generation.md)

---

## Troubleshooting

**Pack not found?**
```bash
ggen ontology search --refresh  # Refresh marketplace index
```

**Generation failed?**
- Check that the pack is installed: `ggen ontology list`
- Verify output directory exists: `mkdir -p ./src/schema`
- Try with verbose logging: `ggen ontology generate schema.org --verbose`

**Need help?**
- See [How to: Debug Pack Installation](../how-to/debug-installation.md)
- Check [CLI Command Reference](../reference/cli-commands.md)
