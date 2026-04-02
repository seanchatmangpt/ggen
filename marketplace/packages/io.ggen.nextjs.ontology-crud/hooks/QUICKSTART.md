# Quick Start: 5-Minute Setup

Get automatic code regeneration from RDF ontologies in 5 minutes.

## Prerequisites

- Git repository initialized
- `ggen` installed (`cargo install --path .`)
- Basic understanding of RDF/Turtle syntax

## Step 1: Install Hooks (30 seconds)

```bash
cd /path/to/your/project
./docs/examples/scripts/install-hooks.sh
```

**Output:**
```
✓ Installed pre-commit hook
✓ Installed post-merge hook
✓ Made regeneration script executable
```

## Step 2: Verify Installation (15 seconds)

```bash
./docs/examples/scripts/test-hooks.sh
```

**Output:**
```
✓ pre-commit hook installed
✓ post-merge hook installed
✓ Regeneration script ready
✓ ggen available
✓ All critical tests passed
```

## Step 3: Create Ontology (2 minutes)

```bash
mkdir -p docs/examples/ontology
vim docs/examples/ontology/blog.ttl
```

**Content:**
```turtle
@prefix ex: <http://example.org/blog#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .

# Blog Post entity
ex:BlogPost a rdfs:Class ;
    rdfs:label "Blog Post" ;
    rdfs:comment "A blog post article" .

# Properties
ex:title a rdf:Property ;
    rdfs:domain ex:BlogPost ;
    rdfs:range rdfs:Literal ;
    rdfs:label "title" .

ex:content a rdf:Property ;
    rdfs:domain ex:BlogPost ;
    rdfs:range rdfs:Literal ;
    rdfs:label "content" .

ex:publishedAt a rdf:Property ;
    rdfs:domain ex:BlogPost ;
    rdfs:range rdfs:Literal ;
    rdfs:label "published at" .

ex:author a rdf:Property ;
    rdfs:domain ex:BlogPost ;
    rdfs:range ex:Author ;
    rdfs:label "author" .

# Author entity
ex:Author a rdfs:Class ;
    rdfs:label "Author" ;
    rdfs:comment "Blog post author" .

ex:name a rdf:Property ;
    rdfs:domain ex:Author ;
    rdfs:range rdfs:Literal ;
    rdfs:label "name" .

ex:email a rdf:Property ;
    rdfs:domain ex:Author ;
    rdfs:range rdfs:Literal ;
    rdfs:label "email" .
```

## Step 4: Create Templates (2 minutes)

```bash
mkdir -p docs/examples/templates
```

### Create `types.ts.hbs`:

```bash
cat > docs/examples/templates/types.ts.hbs << 'EOF'
// Generated from ontology: {{ontologyFile}}
// DO NOT EDIT - This file is auto-generated

{{#each classes}}
/**
 * {{comment}}
 */
export interface {{name}} {
  id: string;
{{#each properties}}
  /** {{label}} */
  {{name}}: {{#if isReference}}{{range}}{{else}}string{{/if}};
{{/each}}
}

{{/each}}

// Type guards
{{#each classes}}
export function is{{name}}(obj: any): obj is {{name}} {
  return obj && typeof obj.id === 'string';
}

{{/each}}
EOF
```

### Create `api-routes.ts.hbs`:

```bash
cat > docs/examples/templates/api-routes.ts.hbs << 'EOF'
// Generated from ontology: {{ontologyFile}}
// DO NOT EDIT - This file is auto-generated

import { Router, Request, Response } from 'express';
import type { {{#each classes}}{{name}}{{#unless @last}}, {{/unless}}{{/each}} } from '../types/{{basename}}.types';

const router = Router();

{{#each classes}}
// {{name}} routes
router.get('/{{pluralName}}', async (req: Request, res: Response) => {
  // TODO: Implement get all {{pluralName}}
  res.json({ data: [] });
});

router.get('/{{pluralName}}/:id', async (req: Request, res: Response) => {
  // TODO: Implement get {{name}} by id
  res.json({ data: null });
});

router.post('/{{pluralName}}', async (req: Request, res: Response) => {
  // TODO: Implement create {{name}}
  res.status(201).json({ data: null });
});

router.put('/{{pluralName}}/:id', async (req: Request, res: Response) => {
  // TODO: Implement update {{name}}
  res.json({ data: null });
});

router.delete('/{{pluralName}}/:id', async (req: Request, res: Response) => {
  // TODO: Implement delete {{name}}
  res.status(204).send();
});

{{/each}}

export default router;
EOF
```

## Step 5: Test Manual Regeneration (30 seconds)

```bash
./docs/examples/scripts/regenerate-from-ontology.sh --verbose
```

**Output:**
```
[regenerate] Starting code regeneration from ontology...
[regenerate]   Validating ontology...
[regenerate]   ✓ blog.ttl is valid
[regenerate]   Loading ontology into graph...
[regenerate]   Generating TypeScript types...
[regenerate]   ✓ Generated types: generated/types/blog.types.ts
[regenerate]   Generating API routes...
[regenerate]   ✓ Generated API routes: generated/api/blog.routes.ts
[regenerate] ✓ Completed blog
[regenerate] Regeneration complete in 2s
[regenerate] Changed files:
  - generated/types/blog.types.ts
  - generated/api/blog.routes.ts
```

## Step 6: Test Git Hook (1 minute)

```bash
# Stage ontology
git add docs/examples/ontology/blog.ttl

# Commit (hook runs automatically)
git commit -m "feat: add blog ontology"
```

**Output:**
```
[pre-commit] Checking for ontology changes...
[pre-commit] Ontology changes detected:
  - docs/examples/ontology/blog.ttl
[pre-commit] Validating ontologies...
[pre-commit]   ✓ blog.ttl is valid
[pre-commit] Running code regeneration...
[pre-commit] Generated/modified files:
  - generated/types/blog.types.ts
  - generated/api/blog.routes.ts
[pre-commit] ✓ Code regeneration completed in 2s
[pre-commit] Generated files have been added to commit
[master abc1234] feat: add blog ontology
 3 files changed, 150 insertions(+)
 create mode 100644 docs/examples/ontology/blog.ttl
 create mode 100644 generated/types/blog.types.ts
 create mode 100644 generated/api/blog.routes.ts
```

## Step 7: Verify Generated Code (30 seconds)

```bash
# Check generated types
cat generated/types/blog.types.ts

# Check generated API routes
cat generated/api/blog.routes.ts
```

**Example `blog.types.ts`:**
```typescript
// Generated from ontology: blog.ttl
// DO NOT EDIT - This file is auto-generated

/**
 * A blog post article
 */
export interface BlogPost {
  id: string;
  /** title */
  title: string;
  /** content */
  content: string;
  /** published at */
  publishedAt: string;
  /** author */
  author: Author;
}

/**
 * Blog post author
 */
export interface Author {
  id: string;
  /** name */
  name: string;
  /** email */
  email: string;
}

// Type guards
export function isBlogPost(obj: any): obj is BlogPost {
  return obj && typeof obj.id === 'string';
}

export function isAuthor(obj: any): obj is Author {
  return obj && typeof obj.id === 'string';
}
```

## What Just Happened?

1. ✅ Created RDF ontology defining `BlogPost` and `Author` entities
2. ✅ Created Handlebars templates for TypeScript types and API routes
3. ✅ Installed git hooks for automatic regeneration
4. ✅ Committed ontology → Hook validated, regenerated code, added to commit
5. ✅ Generated TypeScript interfaces and Express routes

## Next Steps

### Add More Entities

```bash
vim docs/examples/ontology/blog.ttl

# Add Comment entity:
ex:Comment a rdfs:Class ;
    rdfs:label "Comment" ;
    rdfs:comment "Comment on a blog post" .

ex:text a rdf:Property ;
    rdfs:domain ex:Comment ;
    rdfs:range rdfs:Literal .

ex:postedOn a rdf:Property ;
    rdfs:domain ex:Comment ;
    rdfs:range ex:BlogPost .

# Commit (automatic regeneration)
git add docs/examples/ontology/blog.ttl
git commit -m "feat: add Comment entity"
```

### Customize Templates

```bash
# Add validation to types template
vim docs/examples/templates/types.ts.hbs

# Add:
export function validate{{name}}(obj: {{name}}): boolean {
  // Validation logic
  return true;
}
```

### Create More Templates

```bash
# GraphQL schema
vim docs/examples/templates/graphql-schema.gql.hbs

# Prisma schema
vim docs/examples/templates/prisma-schema.prisma.hbs

# Database migrations
vim docs/examples/templates/migration.sql.hbs
```

## Common Workflows

### Daily Development

```bash
# 1. Pull latest changes
git pull origin main
# → post-merge hook regenerates if needed

# 2. Make changes to ontology
vim docs/examples/ontology/blog.ttl

# 3. Commit
git commit -am "feat: update blog schema"
# → pre-commit hook validates and regenerates

# 4. Push
git push origin main
```

### Reviewing Changes

```bash
# Check what changed in generated code
git diff generated/

# See what would be generated (dry-run)
./docs/examples/scripts/regenerate-from-ontology.sh --verbose

# Regenerate without committing
./docs/examples/scripts/regenerate-from-ontology.sh
git diff generated/
git restore generated/  # discard if needed
```

### Bypassing Hooks

```bash
# Skip hooks for WIP commits
git commit --no-verify -m "wip: work in progress"

# Temporarily disable
chmod -x .git/hooks/pre-commit

# Re-enable
chmod +x .git/hooks/pre-commit
```

## Troubleshooting

### Hook doesn't run

```bash
# Check if executable
ls -la .git/hooks/pre-commit

# Fix
chmod +x .git/hooks/pre-commit
```

### Validation fails

```bash
# Validate manually
ggen graph validate --file docs/examples/ontology/blog.ttl

# Check syntax
rapper -i turtle docs/examples/ontology/blog.ttl
```

### Generated code has errors

```bash
# Check template
cat docs/examples/templates/types.ts.hbs

# Regenerate with verbose output
./docs/examples/scripts/regenerate-from-ontology.sh --verbose

# Check TypeScript errors
tsc --noEmit --project generated/tsconfig.json
```

## Summary

You now have:
- ✅ Git hooks for automatic code regeneration
- ✅ RDF ontology defining your data model
- ✅ Templates for TypeScript types and API routes
- ✅ Automated validation on every commit
- ✅ Type-safe generated code

**Total setup time:** ~5 minutes

**Time saved per ontology change:** ~2-3 minutes (manual regeneration)

**Confidence gained:** 100% (always in sync, validated, type-safe)

## Learn More

- [Complete Hooks Guide](./HOOKS_GUIDE.md)
- [Hook Implementation Details](./README.md)
- [Template Guide](../templates/README.md)
- [RDF Best Practices](../ontology/README.md)
