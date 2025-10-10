# A Knowledge Hook for Documentation Sync

⏱️ **15 minutes** | 🎯 **Advanced** | 🏷️ `knowledge-hooks` `documentation` `automation` `markdown`

## What You'll Build

A knowledge hook system that automatically generates and keeps documentation in sync with your code. When modules/APIs change in the graph, documentation regenerates automatically, creating a single source of truth.

## Prerequisites

- Completed [Idempotent Module Wiring](./idempotent_module_wiring.md)
- Understanding of documentation patterns
- Familiarity with Markdown

## Step-by-Step Instructions

### 1. Extend the Module Graph with Documentation Metadata

Add this to your existing `modules.ttl` (from the Module Wiring recipe):

```turtle
@prefix doc: <http://example.org/documentation#> .

# Extend existing auth module with documentation
:authModule
    doc:description "Handles user authentication and authorization" ;
    doc:version "1.2.0" ;
    doc:author "Security Team" ;
    doc:hasMethod :authLogin, :authVerify ;
    doc:hasExample :authExample1 .

:authLogin a doc:Method ;
    doc:methodName "login" ;
    doc:signature "async login(username: string, password: string): Promise<Token>" ;
    doc:description "Authenticates a user and returns an access token" ;
    doc:returns "JWT access token valid for 1 hour" ;
    doc:throws "AuthenticationError if credentials are invalid" .

:authVerify a doc:Method ;
    doc:methodName "verify" ;
    doc:signature "async verify(token: string): Promise<User>" ;
    doc:description "Validates a token and returns the associated user" ;
    doc:returns "User object if token is valid" ;
    doc:throws "InvalidTokenError if token is expired or malformed" .

:authExample1 a doc:Example ;
    doc:title "Basic Login Flow" ;
    doc:code """
const auth = getService('auth');

// Login
const token = await auth.login('user@example.com', 'password123');
console.log('Token:', token);

// Verify
const user = await auth.verify(token);
console.log('User:', user);
""" .
```

### 2. Create the Documentation Generator Template

Create `docs-generator.tmpl`:

```handlebars
{{#each entities}}
{{#if (eq type "Module")}}
{{#if (getProperty props "doc:description")}}
# {{pascalCase props.name}} Module

> {{getProperty props "doc:description"}}

**Version**: {{getProperty props "doc:version" "1.0.0"}}
{{#if (getProperty props "doc:author")}}
**Maintained by**: {{getProperty props "doc:author"}}
{{/if}}

## Overview

The {{pascalCase props.name}} module provides {{props.provides}} capabilities for the application.

{{#if (hasRelationship ../relationships id "dependsOn")}}
### Dependencies

This module depends on:

{{#each (getRelated ../relationships ../entities id "dependsOn")}}
- **{{pascalCase props.name}}** (`{{props.provides}}`)
{{/each}}
{{/if}}

## API Reference

{{#each (getRelated ../relationships ../entities id "doc:hasMethod")}}
### `{{getProperty props "doc:methodName"}}`

{{getProperty props "doc:description"}}

**Signature**:
```typescript
{{getProperty props "doc:signature"}}
```

{{#if (getProperty props "doc:returns")}}
**Returns**: {{getProperty props "doc:returns"}}
{{/if}}

{{#if (getProperty props "doc:throws")}}
**Throws**: {{getProperty props "doc:throws"}}
{{/if}}

---

{{/each}}

{{#if (hasRelationship ../relationships id "doc:hasExample")}}
## Examples

{{#each (getRelated ../relationships ../entities id "doc:hasExample")}}
### {{getProperty props "doc:title"}}

```typescript
{{getProperty props "doc:code"}}
```

{{/each}}
{{/if}}

## Integration

To use this module in your application:

```typescript
import { getService } from '../registry';

// Get the service instance
const {{camelCase props.name}} = getService('{{props.name}}');

// Use the service
// (See examples above)
```

{{#if (hasRelationship ../relationships id "dependsOn")}}
**See also:**
{{#each (getRelated ../relationships ../entities id "dependsOn")}}
- [{{pascalCase props.name}} Module](./{{props.name}}.md)
{{/each}}
{{/if}}

---

*This documentation is auto-generated from the module graph. Last updated: {{timestamp}}*

{{/if}}
{{/if}}
{{/each}}
```

### 3. Update Your Helpers File

The helpers file we created earlier (`helpers-docs.js`) already has all the needed functions. Make sure it includes:

```javascript
module.exports = {
  timestamp: () => new Date().toISOString(),
  hasRelationship: (relationships, subjectId, predicate) => {
    return relationships.some(rel =>
      rel.subject === subjectId && rel.predicate === predicate
    );
  },
  getRelated: (relationships, entities, subjectId, predicate) => {
    const relatedIds = relationships
      .filter(rel => rel.subject === subjectId && rel.predicate === predicate)
      .map(rel => rel.object);
    return entities.filter(e => relatedIds.includes(e.id));
  },
  getProperty: (props, key, defaultValue = '') => {
    return props && props[key] !== undefined ? props[key] : defaultValue;
  },
  pascalCase: (str) => {
    return str.replace(/(?:^|[-_])(\w)/g, (_, c) => c ? c.toUpperCase() : '');
  },
  camelCase: (str) => {
    const pascal = str.replace(/(?:^|[-_])(\w)/g, (_, c) => c ? c.toUpperCase() : '');
    return pascal.charAt(0).toLowerCase() + pascal.slice(1);
  }
};
```

### 4. Generate the Documentation

```bash
# Create docs directory
mkdir -p docs/modules

# Generate documentation for each module
ggen generate docs-generator.tmpl \
  --data modules-with-docs.ttl \
  --output-dir docs/modules \
  --fan-out \
  --helpers helpers-docs.js
```

### 5. Verify Generated Documentation

Check `docs/modules/auth.md`:

```markdown
# Auth Module

> Handles user authentication and authorization

**Version**: 1.2.0
**Maintained by**: Security Team

## Overview

The Auth module provides Authentication capabilities for the application.

### Dependencies

This module depends on:

- **Config** (`Configuration`)

## API Reference

### `login`

Authenticates a user and returns an access token

**Signature**:
```typescript
async login(username: string, password: string): Promise<Token>
```

**Returns**: JWT access token valid for 1 hour

**Throws**: AuthenticationError if credentials are invalid

---

### `verify`

Validates a token and returns the associated user

**Signature**:
```typescript
async verify(token: string): Promise<User>
```

**Returns**: User object if token is valid

**Throws**: InvalidTokenError if token is expired or malformed

---

## Examples

### Basic Login Flow

```typescript
const auth = getService('auth');

// Login
const token = await auth.login('user@example.com', 'password123');
console.log('Token:', token);

// Verify
const user = await auth.verify(token);
console.log('User:', user);
```

---

*This documentation is auto-generated from the module graph. Last updated: 2025-10-09T23:17:00Z*
```

### 6. Create a Documentation Update Hook

Create `scripts/update-docs.sh`:

```bash
#!/bin/bash
set -e

echo "📚 Updating module documentation..."

# Regenerate documentation from graph
ggen generate docs-generator.tmpl \
  --data modules-with-docs.ttl \
  --output-dir docs/modules \
  --fan-out \
  --helpers helpers-docs.js

echo "✅ Documentation updated!"
echo ""
echo "Updated files:"
ls -1 docs/modules/*.md | sed 's/^/  - /'
```

Make it executable:

```bash
chmod +x scripts/update-docs.sh
```

### 7. Add a Git Pre-Commit Hook

Create `.git/hooks/pre-commit`:

```bash
#!/bin/bash

# Check if module graph changed
if git diff --cached --name-only | grep -q "modules.*\.ttl"; then
  echo "📚 Module graph changed, updating documentation..."
  ./scripts/update-docs.sh

  # Stage updated docs
  git add docs/modules/*.md

  echo "✅ Documentation synchronized"
fi
```

Make it executable:

```bash
chmod +x .git/hooks/pre-commit
```

### 8. Test the Hook

Modify `modules-with-docs.ttl` (add a new method):

```turtle
:authRefresh a doc:Method ;
    doc:methodName "refresh" ;
    doc:signature "async refresh(token: string): Promise<Token>" ;
    doc:description "Refreshes an access token" ;
    doc:returns "New JWT access token" .

:authModule doc:hasMethod :authRefresh .
```

Commit the change:

```bash
git add modules-with-docs.ttl
git commit -m "Add refresh token method"
```

The hook automatically runs and updates documentation!

## Expected Output

When you commit changes to the module graph:

```bash
$ git commit -m "Add refresh token method"
📚 Module graph changed, updating documentation...
📚 Updating module documentation...
✅ Documentation updated!

Updated files:
  - docs/modules/auth.md
  - docs/modules/config.md
  - docs/modules/database.md

✅ Documentation synchronized
[master abc1234] Add refresh token method
 2 files changed, 15 insertions(+), 2 deletions(-)
```

## Verification Steps

1. ✅ **Documentation generated**: `docs/modules/*.md` files exist
2. ✅ **Methods documented**: All `doc:hasMethod` relationships appear as API reference
3. ✅ **Examples included**: Code examples render correctly
4. ✅ **Dependencies shown**: Module dependencies listed
5. ✅ **Auto-update works**: Git hook regenerates docs on commit

## What's Happening

### Knowledge Graph as Source of Truth

```turtle
:authModule
    :name "auth" ;
    :provides "Authentication" ;
    doc:description "..." ;
    doc:hasMethod :authLogin .
```

The graph contains both **implementation metadata** (`:provides`, `:dependsOn`) and **documentation metadata** (`doc:description`, `doc:hasMethod`). This creates a single source of truth.

### Relationship Traversal for Documentation

```handlebars
{{#each (getRelated ../relationships ../entities id "doc:hasMethod")}}
  {{getProperty props "doc:methodName"}}
{{/each}}
```

The template traverses `doc:hasMethod` relationships to find all methods belonging to a module, then extracts their documentation properties.

### Bidirectional Sync

1. **Graph → Docs**: Template generates Markdown from graph
2. **Git Hook**: Automatically regenerates when graph changes
3. **Verification**: No manual doc updates needed

## Try These Experiments

1. **Add more documentation types**: Create `doc:Field`, `doc:Configuration` types
2. **Generate API clients**: Use same graph to generate TypeScript API client
3. **Extract from code**: Parse code comments and populate graph automatically
4. **Semantic search**: Index generated docs with embeddings for AI search
5. **Multi-format output**: Generate HTML, PDF, or OpenAPI from same graph

## Next Steps

### Related Patterns
- 📚 [Knowledge Graphs](../patterns/knowledge-graphs.md) - Semantic modeling
- 📚 [Fan-Out Generation](../patterns/fan-out.md) - Multi-file output
- 📚 [Git Hooks](../patterns/git-hooks.md) - Automation triggers

### More Recipes
- 🍳 [API Endpoint Generator](./api_endpoint_generator.md) - Similar pattern for REST APIs
- 🍳 [Code from Docs](../advanced/docs-to-code.md) - Reverse direction generation

### Advanced Topics
- 📖 [Semantic Extraction](../advanced/semantic-extraction.md) - Parse code to RDF
- 📖 [Living Documentation](../advanced/living-docs.md) - Always-current docs

## Common Issues

**Problem**: Documentation not updating in git commit
- **Solution**: Verify pre-commit hook is executable: `chmod +x .git/hooks/pre-commit`
- **Solution**: Check hook references correct script path

**Problem**: Methods not appearing in docs
- **Solution**: Verify `doc:hasMethod` relationship exists in graph
- **Solution**: Check helper functions are loading correctly (`--helpers` flag)

**Problem**: Markdown formatting broken
- **Solution**: Escape special characters in `doc:description` properties
- **Solution**: Use triple-quoted strings in Turtle for multi-line text

**Problem**: Documentation out of sync with code
- **Solution**: Add a CI check that verifies docs match graph
- **Solution**: Run `./scripts/update-docs.sh` in CI pipeline

---

🎉 **Excellent!** You've created a living documentation system powered by your knowledge graph. Documentation now updates automatically whenever the graph changes, ensuring it never drifts from reality.
