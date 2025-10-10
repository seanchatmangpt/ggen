# Chapter 7: The `market` Noun

## What is the GGen Market?

The **market** is a public repository of reusable knowledge patterns, templates, and ontologies created by the GGen community.

Think of it as:
- **NPM** for knowledge graphs
- **Docker Hub** for templates
- **GitHub** for domain patterns

Instead of reinventing domain models, you can:
- **Search** for existing patterns (e.g., "e-commerce", "authentication")
- **Install** them into your project
- **Extend** them with custom logic
- **Publish** your own patterns back to the community

## Why the Market Exists

**Problem:** Every e-commerce site rebuilds the same domain model (User, Product, Order, Cart). Every SaaS app rebuilds multi-tenancy. Every API rebuilds authentication.

**Solution:** Define these patterns **once**, as knowledge graphs, and share them.

**Example:** Instead of hand-coding a User entity with email, password, roles, and permissions for the 1000th time, install `@ggen/auth-user` and generate it in 30 seconds.

## Market Structure

The market is organized into **packages**, each containing:

1. **Knowledge graphs** (RDF/OWL files)
2. **Templates** (code generators)
3. **Shapes** (SHACL validation rules)
4. **Documentation** (README, examples)
5. **Metadata** (package.json)

### Package Types

**1. Ontology Packages**

Domain knowledge without generation logic.

**Example:** `@ggen/ecommerce-ontology`

Contains:
- Entities: Product, Order, Cart, Payment, Shipping
- Relationships: Order contains OrderItems, User has Cart
- Constraints: Prices must be non-negative, emails must be unique

**Usage:**
```bash
ggen market install @ggen/ecommerce-ontology
ggen graph merge --source market/ecommerce-ontology.ttl
```

**2. Template Packages**

Code generators for specific tech stacks.

**Example:** `@ggen/nestjs-crud`

Generates:
- NestJS controllers
- Services with CRUD methods
- DTOs and validation
- Swagger documentation

**Usage:**
```bash
ggen market install @ggen/nestjs-crud
```

```yaml
# ggen.config.yaml
generation:
  targets:
    - template: "@ggen/nestjs-crud"
      output: src/api/
```

**3. Full-Stack Packages**

Complete patterns with ontology + templates.

**Example:** `@ggen/saas-starter`

Includes:
- Ontology: Tenant, User, Subscription, Plan
- Templates: Backend API, Frontend types, Database schema
- Shapes: Multi-tenancy validation
- Docs: Architecture guide

**Usage:**
```bash
ggen project init my-saas --template @ggen/saas-starter
```

## Core Commands

### `ggen market search`

Find packages in the market.

**Basic search:**
```bash
# Search by keyword
ggen market search ecommerce

# Search by tag
ggen market search --tag authentication

# Search by type
ggen market search --type ontology
```

**Advanced search:**
```bash
# Full-text search
ggen market search "user authentication with OAuth2"

# Filter by author
ggen market search --author @ggen-official

# Filter by popularity
ggen market search --sort downloads

# Filter by updated date
ggen market search --updated-since 2024-01-01

# Combine filters
ggen market search "graphql api" --tag api --type template --sort stars
```

**Example output:**

```
Found 12 packages matching "authentication"

📦 @ggen/auth-user (⭐ 1.2k, ⬇ 45k)
   User authentication with email/password and JWT
   Tags: auth, user, jwt
   Updated: 2 days ago

📦 @ggen/oauth2-pattern (⭐ 890, ⬇ 23k)
   OAuth2 authentication flow (Google, GitHub, etc.)
   Tags: auth, oauth2, social
   Updated: 1 week ago

📦 @ggen/rbac-permissions (⭐ 650, ⬇ 18k)
   Role-based access control with permissions
   Tags: auth, rbac, permissions
   Updated: 3 weeks ago

...
```

### `ggen market install`

Install a package into your project.

**Basic install:**
```bash
# Install a package
ggen market install @ggen/auth-user

# Install multiple packages
ggen market install @ggen/auth-user @ggen/rbac-permissions

# Install specific version
ggen market install @ggen/auth-user@1.2.3
```

**Installation options:**
```bash
# Install as dev dependency (templates only, not merged into graph)
ggen market install @ggen/nestjs-crud --dev

# Install and auto-merge knowledge
ggen market install @ggen/ecommerce-ontology --merge

# Install with custom namespace
ggen market install @ggen/auth-user --namespace myapp
```

**What happens:**

1. Package is downloaded to `~/.ggen/market/`
2. If ontology, added to `knowledge/market/`
3. If template, added to `templates/market/`
4. Metadata saved in `ggen.lock`

**Example:**

```bash
$ ggen market install @ggen/auth-user --merge

Installing @ggen/auth-user@1.2.3...
✓ Downloaded package (5.2 KB)
✓ Extracted to ~/.ggen/market/auth-user
✓ Copied knowledge/auth-user.ttl → knowledge/market/
✓ Merged into knowledge/domain.ttl

Added entities:
  - User (with properties: email, passwordHash, roles)
  - Role (with properties: name, permissions)
  - Session (with properties: token, expiresAt)

Added relationships:
  - User hasMany Roles
  - Session belongsTo User

Updated ggen.lock
```

### `ggen market info`

Get detailed information about a package.

```bash
# Show package details
ggen market info @ggen/auth-user

# Show with examples
ggen market info @ggen/auth-user --examples

# Show dependencies
ggen market info @ggen/auth-user --dependencies
```

**Example output:**

```
📦 @ggen/auth-user v1.2.3

Description:
  Complete user authentication system with email/password,
  JWT tokens, and role-based access control.

Author: @ggen-official
License: MIT
Homepage: https://github.com/ggen/packages/auth-user

Tags: authentication, user, jwt, rbac
Downloads: 45,234
Stars: 1,245

Entities:
  - User (8 properties, 3 relationships)
  - Role (3 properties)
  - Session (5 properties, 1 relationship)

Templates:
  - auth-controller.hbs (NestJS authentication controller)
  - jwt-strategy.hbs (Passport JWT strategy)
  - user-service.hbs (User management service)

Dependencies:
  - @ggen/base-entity@^2.0.0
  - @ggen/validation-helpers@^1.1.0

Examples:
  ggen market install @ggen/auth-user
  ggen project generate --template @ggen/auth-user/jwt-strategy
```

### `ggen market update`

Update installed packages.

```bash
# Update all packages
ggen market update

# Update specific package
ggen market update @ggen/auth-user

# Update to specific version
ggen market update @ggen/auth-user@2.0.0

# Check for updates (dry-run)
ggen market update --check
```

**Example:**

```bash
$ ggen market update

Checking for updates...

Packages with updates available:
  @ggen/auth-user       1.2.3 → 1.3.0
  @ggen/nestjs-crud     2.1.0 → 2.2.1

Update all? (y/n): y

Updating @ggen/auth-user...
✓ Updated to 1.3.0
  Breaking changes:
    - Removed deprecated `User.username` property
    - JWT expiry now in milliseconds (was seconds)

Updating @ggen/nestjs-crud...
✓ Updated to 2.2.1
  New features:
    - Soft delete support
    - Pagination cursor mode

Run `ggen project generate` to apply changes.
```

### `ggen market publish`

Publish your own package to the market.

**First-time setup:**
```bash
# Create account
ggen market signup

# Login
ggen market login
```

**Publish workflow:**

**Step 1: Prepare package**

```
my-package/
├── package.json          # Metadata
├── knowledge/            # Ontology files
│   └── domain.ttl
├── templates/            # Template files
│   └── my-template.hbs
├── shapes/               # Validation rules
│   └── shapes.ttl
├── README.md             # Documentation
└── examples/             # Usage examples
    └── basic.md
```

**`package.json`:**

```json
{
  "name": "@myorg/my-package",
  "version": "1.0.0",
  "description": "My awesome GGen package",
  "author": "Your Name",
  "license": "MIT",
  "keywords": ["ggen", "ontology", "ecommerce"],
  "ggen": {
    "type": "ontology",
    "entities": ["Product", "Category"],
    "dependencies": {
      "@ggen/base-entity": "^2.0.0"
    }
  }
}
```

**Step 2: Test locally**

```bash
# Validate package
ggen market validate .

# Test in a project
ggen market install ./my-package --link
```

**Step 3: Publish**

```bash
# Publish
ggen market publish

# Publish with tag
ggen market publish --tag beta

# Dry-run (preview)
ggen market publish --dry-run
```

**Example:**

```bash
$ ggen market publish

Publishing @myorg/my-package@1.0.0...

Validating package...
✓ package.json valid
✓ knowledge/domain.ttl valid RDF
✓ templates syntax valid
✓ No missing dependencies

Packaging...
✓ Created tarball (12.4 KB)

Uploading to registry...
✓ Published successfully

View at: https://market.ggen.io/packages/@myorg/my-package

Share with:
  ggen market install @myorg/my-package
```

### `ggen market unpublish`

Remove a published package (use with caution).

```bash
# Unpublish specific version
ggen market unpublish @myorg/my-package@1.0.0

# Unpublish all versions (requires confirmation)
ggen market unpublish @myorg/my-package --force
```

## Working with Market Packages

### Browsing the Market

**Web interface:**
Visit `https://market.ggen.io` to browse packages visually.

Features:
- Search and filter
- View source code
- See dependency graphs
- Read documentation
- Check compatibility

**CLI interface:**
```bash
# List popular packages
ggen market list --sort downloads

# List recent packages
ggen market list --sort updated

# List by category
ggen market list --category authentication
```

### Installing and Using

**Example: E-commerce ontology**

```bash
# Search for e-commerce packages
ggen market search ecommerce

# View package details
ggen market info @ggen/ecommerce-ontology

# Install
ggen market install @ggen/ecommerce-ontology

# Merge into project
ggen graph merge market/ecommerce-ontology.ttl

# Generate code
ggen project generate
```

**Example: Authentication pattern**

```bash
# Install authentication ontology
ggen market install @ggen/auth-user --merge

# Install NestJS authentication templates
ggen market install @ggen/nestjs-auth-templates

# Configure generation
nano ggen.config.yaml
```

```yaml
generation:
  targets:
    - name: auth-controllers
      template: "@ggen/nestjs-auth-templates/controller"
      output: src/auth/
```

```bash
# Generate
ggen project generate --target auth-controllers
```

### Extending Market Packages

You can extend market packages rather than modifying them:

**Example: Extend User entity**

```turtle
# knowledge/extensions.ttl
@prefix : <http://myapp.com/ontology#> .
@prefix auth: <http://ggen.io/auth#> .

# Extend User from @ggen/auth-user
:AppUser rdfs:subClassOf auth:User ;
  :hasProperty :userAvatar ;
  :hasProperty :userBio .

:userAvatar a owl:DatatypeProperty ;
  rdfs:label "avatar" ;
  rdfs:domain :AppUser ;
  rdfs:range :URL .

:userBio a owl:DatatypeProperty ;
  rdfs:label "bio" ;
  rdfs:domain :AppUser ;
  rdfs:range xsd:string ;
  :maxLength 500 .
```

Now generate with the extended knowledge:

```bash
ggen project generate --entity AppUser
```

## Market Best Practices

### For Package Consumers

**1. Lock versions**

```yaml
# ggen.config.yaml
dependencies:
  "@ggen/auth-user": "1.2.3"  # Not "^1.2.3" or "latest"
```

**2. Review before merging**

```bash
# Install without merging
ggen market install @ggen/some-package

# Review the knowledge
cat market/some-package.ttl

# Merge selectively
ggen graph merge market/some-package.ttl --entities User,Role
```

**3. Test after updates**

```bash
ggen market update @ggen/auth-user
ggen project generate
npm test  # Ensure nothing broke
```

**4. Use namespaces**

Avoid naming conflicts by using package namespaces:

```turtle
@prefix myapp: <http://myapp.com/ontology#> .
@prefix auth: <http://ggen.io/auth#> .

# Your entities use myapp:
myapp:Product a owl:Class .

# Market entities use their namespace:
auth:User a owl:Class .
```

### For Package Publishers

**1. Follow semver**

- **Major (2.0.0)**: Breaking changes to ontology or templates
- **Minor (1.1.0)**: New entities, properties, or optional features
- **Patch (1.0.1)**: Bug fixes, documentation updates

**2. Document breaking changes**

```markdown
# CHANGELOG.md

## 2.0.0 (2024-01-15)

### Breaking Changes
- Removed `User.username` property (use `email` instead)
- JWT tokens now expire in milliseconds, not seconds
```

**3. Provide examples**

Include `examples/` directory with common use cases:

```markdown
# examples/basic-usage.md

## Basic Usage

Install the package:
```bash
ggen market install @myorg/my-package
```

Merge into your project:
```bash
ggen graph merge market/my-package.ttl
```

Generate code:
```bash
ggen project generate
```
```

**4. Test compatibility**

Test your package with popular templates:

```bash
# Test with NestJS templates
ggen project init test-nestjs --template nestjs
ggen market install ./my-package --link
ggen project generate

# Test with GraphQL templates
ggen project init test-graphql --template graphql
ggen market install ./my-package --link
ggen project generate
```

**5. Maintain dependencies**

Keep dependencies up to date:

```bash
ggen market update  # Update your dependencies
npm test            # Ensure tests pass
ggen market publish # Publish new version
```

## Real-World Examples

### Example 1: Building a SaaS App

```bash
# Initialize with SaaS starter
ggen project init my-saas --template @ggen/saas-starter

# Install additional packages
ggen market install @ggen/stripe-billing --merge
ggen market install @ggen/email-templates

# Generate code
ggen project generate

# Result: Complete SaaS with tenants, billing, and email
```

### Example 2: Custom E-commerce

```bash
# Start fresh
ggen project init my-store

# Install e-commerce base
ggen market install @ggen/ecommerce-ontology --merge

# Add custom features
nano knowledge/custom.ttl  # Add GiftCard, Wishlist, etc.

# Install templates
ggen market install @ggen/nestjs-crud
ggen market install @ggen/react-admin

# Generate
ggen project generate
```

### Example 3: Publishing a Pattern

```bash
# Create package
mkdir my-blog-ontology
cd my-blog-ontology

# Define knowledge
nano knowledge/blog.ttl  # Define Post, Comment, Tag

# Add metadata
nano package.json

# Test locally
ggen market validate .

# Publish
ggen market publish
```

## Summary

The market is GGen's secret weapon for **knowledge reuse**. Instead of rebuilding common patterns, you:

1. **Search** for existing packages
2. **Install** them into your project
3. **Extend** with custom logic
4. **Generate** working code
5. **Publish** your own patterns back

Key commands:
- `search` - Find packages
- `install` - Add to project
- `info` - View details
- `update` - Keep current
- `publish` - Share with community

Next chapter: **Generation Workflow**—how to orchestrate daily generation tasks.
