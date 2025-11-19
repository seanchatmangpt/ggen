# Tutorial Compatibility Matrix

**Last Updated:** 2025-11-19
**Current Version:** ggen 3.2.0

This document provides a comprehensive compatibility matrix showing which tutorial commands work with which ggen versions, helping users understand version requirements and migration paths.

---

## Version Overview

| Version | Release Date | Status | Notes |
|---------|--------------|--------|-------|
| **3.2.0** | 2025-11-19 | **Current** | Latest production release |
| **3.0.0** | 2025-11 | Stable | Major packs system release |
| **2.7.0** | 2025-11 | Stable | University business model docs |
| **2.6.0** | 2025-11 | Stable | Production-ready milestone |
| **2.5.x** | 2025-10 | Stable | Marketplace v2 |
| **2.0.x** | 2025 | Legacy | Pre-packs era |
| **1.x.x** | 2024 | Deprecated | Original implementation |

---

## Command Compatibility Matrix

### Core Commands

| Command | 3.2.0 | 3.0.0 | 2.7.0 | 2.6.0 | 2.5.x | Notes |
|---------|-------|-------|-------|-------|-------|-------|
| `ggen --version` | ✅ | ✅ | ✅ | ✅ | ✅ | Always available |
| `ggen --help` | ✅ | ✅ | ✅ | ✅ | ✅ | Always available |

### AI Commands (Tutorial: `ai-powered-generation.md`)

| Command | 3.2.0 | 3.0.0 | 2.7.0 | 2.6.0 | 2.5.x | Notes |
|---------|-------|-------|-------|-------|-------|-------|
| `ggen ai generate-ontology` | ✅ | ✅ | ✅ | ✅ | ✅ | Core AI feature |
| `ggen ai generate-ontology --prompt` | ✅ | ✅ | ✅ | ✅ | ✅ | Required param |
| `ggen ai generate-ontology --output` | ✅ | ✅ | ✅ | ✅ | ✅ | Required param |
| `ggen ai generate-ontology --model ollama:llama2` | ✅ | ✅ | ✅ | ✅ | ⚠️ | Local model support added 2.5.0 |
| `ggen ai chat --interactive` | ✅ | ✅ | ✅ | ✅ | ✅ | Interactive mode |
| `ggen ai chat --prompt --input --output` | ✅ | ✅ | ✅ | ✅ | ⚠️ | File I/O added 2.5.0 |
| `ggen ai analyze <path>` | ✅ | ✅ | ✅ | ⚠️ | ❌ | Added in 2.6.0 |
| `ggen ai analyze --focus` | ✅ | ✅ | ✅ | ⚠️ | ❌ | Focus parameter added 2.6.0 |
| `ggen ai analyze --suggest-improvements` | ✅ | ✅ | ✅ | ⚠️ | ❌ | Suggestions added 2.6.0 |

**Minimum Version:** 2.5.0 for basic AI commands, 2.6.0 for analysis features

---

### Template Commands (Tutorials: `ontology-to-code.md`, `zero-to-generated-code.md`)

| Command | 3.2.0 | 3.0.0 | 2.7.0 | 2.6.0 | 2.5.x | Notes |
|---------|-------|-------|-------|-------|-------|-------|
| `ggen template generate-rdf` | ✅ | ✅ | ✅ | ✅ | ✅ | Core template generation |
| `ggen template generate-rdf --ontology` | ✅ | ✅ | ✅ | ✅ | ✅ | Required param |
| `ggen template generate-rdf --template` | ✅ | ✅ | ✅ | ✅ | ✅ | Required param |
| `ggen template generate-rdf --output` | ✅ | ✅ | ✅ | ✅ | ✅ | Output path |
| `ggen template list` | ✅ | ✅ | ✅ | ✅ | ✅ | List available templates |
| `ggen template lint <template>` | ✅ | ✅ | ✅ | ✅ | ⚠️ | Linting added 2.5.0 |

**Built-in Templates:**

| Template | 3.2.0 | 3.0.0 | 2.7.0 | 2.6.0 | 2.5.x | Notes |
|----------|-------|-------|-------|-------|-------|-------|
| `rust-models` | ✅ | ✅ | ✅ | ✅ | ✅ | Rust struct generation |
| `typescript-models` | ✅ | ✅ | ✅ | ✅ | ✅ | TypeScript interface generation |
| `python-pydantic` | ✅ | ✅ | ✅ | ✅ | ⚠️ | Added in 2.5.0 |
| `rust-graphql-api` | ✅ | ✅ | ✅ | ✅ | ⚠️ | GraphQL support added 2.5.0 |
| `markdown-schema` | ✅ | ✅ | ✅ | ⚠️ | ❌ | Documentation gen added 2.6.0 |

**Minimum Version:** 2.5.0 for basic templates, 2.6.0 for full template suite

---

### Marketplace Commands (Tutorials: `marketplace-workflow.md`, `marketplace-quick-start.md`)

| Command | 3.2.0 | 3.0.0 | 2.7.0 | 2.6.0 | 2.5.x | Notes |
|---------|-------|-------|-------|-------|-------|-------|
| `ggen marketplace search` | ✅ | ✅ | ✅ | ✅ | ✅ | Search marketplace |
| `ggen marketplace search "<query>"` | ✅ | ✅ | ✅ | ✅ | ✅ | Query-based search |
| `ggen marketplace search --category` | ✅ | ✅ | ✅ | ⚠️ | ❌ | Category filter added 2.6.0 |
| `ggen marketplace search --min-maturity` | ✅ | ✅ | ✅ | ⚠️ | ❌ | Maturity filter added 2.6.0 |
| `ggen marketplace install <package>` | ✅ | ✅ | ✅ | ✅ | ✅ | Install package |
| `ggen marketplace install <package>@<version>` | ✅ | ✅ | ✅ | ✅ | ⚠️ | Version pinning added 2.5.0 |
| `ggen marketplace list` | ✅ | ✅ | ✅ | ✅ | ✅ | List installed |
| `ggen marketplace info <package>` | ✅ | ✅ | ✅ | ✅ | ⚠️ | Package info added 2.5.0 |
| `ggen marketplace publish` | ✅ | ✅ | ✅ | ✅ | ✅ | Publish to marketplace |
| `ggen marketplace publish --github-url` | ✅ | ✅ | ✅ | ⚠️ | ❌ | GitHub integration added 2.6.0 |
| `ggen marketplace validate` | ✅ | ✅ | ✅ | ⚠️ | ❌ | Validation added 2.6.0 |

**Minimum Version:** 2.5.0 for basic marketplace, 2.6.0 for full feature set

---

### Packs Commands (Tutorials: `packs-getting-started.md`, `packs-concepts.md`, `packs-reference.md`)

| Command | 3.2.0 | 3.0.0 | 2.7.0 | 2.6.0 | 2.5.x | Notes |
|---------|-------|-------|-------|-------|-------|-------|
| `ggen packs list` | ✅ | ✅ | ❌ | ❌ | ❌ | **NEW in 3.0.0** |
| `ggen packs list --category` | ✅ | ✅ | ❌ | ❌ | ❌ | Category filter |
| `ggen packs list --format` | ✅ | ✅ | ❌ | ❌ | ❌ | Output formatting |
| `ggen packs show --pack_id` | ✅ | ✅ | ❌ | ❌ | ❌ | Show pack details |
| `ggen packs validate --pack_id` | ✅ | ✅ | ❌ | ❌ | ❌ | Validate pack |
| `ggen packs install --pack_id` | ✅ | ✅ | ❌ | ❌ | ❌ | Install pack |
| `ggen packs install --dry_run` | ✅ | ✅ | ❌ | ❌ | ❌ | Preview installation |

**Available Packs:**

| Pack ID | 3.2.0 | 3.0.0 | Description |
|---------|-------|-------|-------------|
| `startup-essentials` | ✅ | ✅ | MVP development stack |
| `enterprise-backend` | ✅ | ✅ | Production backend services |
| `data-science` | ✅ | ✅ | ML/AI toolkit |
| `devops-automation` | ✅ | ✅ | Infrastructure automation |
| `frontend-modern` | ✅ | ✅ | Modern web UI |

**Minimum Version:** **3.0.0** (Packs are a 3.x feature)

---

### Project Commands (Tutorials: `getting-started.md`, `zero-to-generated-code.md`)

| Command | 3.2.0 | 3.0.0 | 2.7.0 | 2.6.0 | 2.5.x | Notes |
|---------|-------|-------|-------|-------|-------|-------|
| `ggen project new <name>` | ✅ | ✅ | ✅ | ✅ | ✅ | Create new project |
| `ggen project new --type` | ✅ | ✅ | ✅ | ✅ | ✅ | Project type |
| `ggen project new --framework` | ✅ | ✅ | ✅ | ✅ | ⚠️ | Framework option added 2.5.0 |
| `ggen project gen <name>` | ✅ | ✅ | ✅ | ✅ | ✅ | Generate from template |
| `ggen project gen --template` | ✅ | ✅ | ✅ | ✅ | ✅ | Template selection |
| `ggen project gen --vars` | ✅ | ✅ | ✅ | ✅ | ⚠️ | Variable passing added 2.5.0 |
| `ggen project watch` | ✅ | ✅ | ✅ | ⚠️ | ❌ | Watch mode added 2.6.0 |

**Minimum Version:** 2.5.0 for basic project commands, 2.6.0 for watch mode

---

### Graph Commands (Tutorial: `ontology-to-code.md`)

| Command | 3.2.0 | 3.0.0 | 2.7.0 | 2.6.0 | 2.5.x | Notes |
|---------|-------|-------|-------|-------|-------|-------|
| `ggen graph load <file>` | ✅ | ✅ | ✅ | ✅ | ✅ | Load RDF graph |
| `ggen graph query --sparql` | ✅ | ✅ | ✅ | ✅ | ✅ | SPARQL queries |
| `ggen graph export --format` | ✅ | ✅ | ✅ | ✅ | ✅ | Export graph |
| `ggen graph diff <file1> <file2>` | ✅ | ✅ | ✅ | ⚠️ | ❌ | Diff added in 2.6.0 |

**Supported Formats:**
- Turtle (.ttl) - ✅ All versions
- JSON-LD (.jsonld) - ✅ All versions
- N-Triples (.nt) - ✅ All versions
- RDF/XML (.rdf) - ⚠️ Added in 2.5.0

**Minimum Version:** 2.5.0 for all RDF formats

---

### Hook Commands (Tutorial: `zero-to-generated-code.md`)

| Command | 3.2.0 | 3.0.0 | 2.7.0 | 2.6.0 | 2.5.x | Notes |
|---------|-------|-------|-------|-------|-------|-------|
| `ggen hook create <type>` | ✅ | ✅ | ✅ | ✅ | ✅ | Create lifecycle hook |
| `ggen hook create --name` | ✅ | ✅ | ✅ | ✅ | ✅ | Hook name |
| `ggen hook create --command` | ✅ | ✅ | ✅ | ✅ | ✅ | Hook command |
| `ggen hook create --timeout` | ✅ | ✅ | ✅ | ✅ | ⚠️ | Timeout option added 2.5.0 |
| `ggen hook list` | ✅ | ✅ | ✅ | ⚠️ | ❌ | List hooks added 2.6.0 |
| `ggen hook remove <name>` | ✅ | ✅ | ✅ | ⚠️ | ❌ | Remove hook added 2.6.0 |
| `ggen hook monitor` | ✅ | ✅ | ✅ | ⚠️ | ❌ | Hook monitoring added 2.6.0 |

**Supported Hook Types:**
- `pre-commit` - ✅ All versions
- `post-commit` - ✅ All versions
- `pre-push` - ✅ All versions
- `post-merge` - ⚠️ Added in 2.5.0
- `pre-rebase` - ⚠️ Added in 2.6.0

**Minimum Version:** 2.5.0 for basic hooks, 2.6.0 for advanced features

---

### Utils Commands

| Command | 3.2.0 | 3.0.0 | 2.7.0 | 2.6.0 | 2.5.x | Notes |
|---------|-------|-------|-------|-------|-------|-------|
| `ggen utils doctor` | ✅ | ✅ | ✅ | ✅ | ✅ | Health check |
| `ggen utils doctor --verbose` | ✅ | ✅ | ✅ | ⚠️ | ❌ | Verbose output added 2.6.0 |

**Minimum Version:** 2.5.0

---

## Tutorial-Specific Compatibility

### Getting Started Tutorial (`getting-started.md`)

**Minimum Version:** 2.5.0
**Recommended Version:** 3.2.0

| Step | Commands Used | Min Version | Notes |
|------|---------------|-------------|-------|
| Installation | N/A | Any | Multiple install methods |
| Generate Ontology | `ggen ai generate-ontology` | 2.5.0 | AI required |
| Generate Rust Code | `ggen template generate-rdf --template rust-models` | 2.5.0 | Core feature |
| Modify & Regenerate | Same as above | 2.5.0 | Demonstrates workflow |

---

### Zero to Generated Code Tutorial (`zero-to-generated-code.md`)

**Minimum Version:** 2.5.0
**Recommended Version:** 3.2.0

| Step | Commands Used | Min Version | Notes |
|------|---------------|-------------|-------|
| Generate Ontology | `ggen ai generate-ontology` | 2.5.0 | AI required |
| Generate Rust | `ggen template generate-rdf --template rust-models` | 2.5.0 | Core feature |
| Generate TypeScript | `ggen template generate-rdf --template typescript-models` | 2.5.0 | Core feature |
| Modify Ontology | Manual edit | Any | No CLI needed |
| Regenerate Code | `ggen template generate-rdf` | 2.5.0 | Core feature |
| Generate Docs | `ggen template generate-rdf --template markdown-schema` | 2.6.0 | **Requires 2.6.0+** |
| Create Hook | `ggen hook create pre-commit` | 2.5.0 | Automation |

---

### AI-Powered Generation Tutorial (`ai-powered-generation.md`)

**Minimum Version:** 2.5.0
**Recommended Version:** 3.2.0

| Step | Commands Used | Min Version | Notes |
|------|---------------|-------------|-------|
| Generate Ontology | `ggen ai generate-ontology` | 2.5.0 | Core AI |
| Review & Refine | `ggen ai chat` | 2.5.0 | Interactive |
| Generate Code | `ggen template generate-rdf` | 2.5.0 | Code gen |
| AI Analysis | `ggen ai analyze` | 2.6.0 | **Requires 2.6.0+** |

---

### Ontology-to-Code Workflow Tutorial (`ontology-to-code.md`)

**Minimum Version:** 2.5.0
**Recommended Version:** 3.2.0

| Step | Commands Used | Min Version | Notes |
|------|---------------|-------------|-------|
| Create Ontology | Manual RDF creation | Any | No CLI needed |
| Generate Rust | `ggen template generate-rdf --template rust-models` | 2.5.0 | Core feature |
| Generate TypeScript | `ggen template generate-rdf --template typescript-models` | 2.5.0 | Core feature |
| Query Graph | `ggen graph query --sparql` | 2.5.0 | SPARQL execution |

---

### Marketplace Workflow Tutorial (`marketplace-workflow.md`)

**Minimum Version:** 2.5.0
**Recommended Version:** 3.2.0

| Step | Commands Used | Min Version | Notes |
|------|---------------|-------------|-------|
| Search | `ggen marketplace search` | 2.5.0 | Basic search |
| Install | `ggen marketplace install` | 2.5.0 | Package install |
| Use Template | `ggen project gen --template` | 2.5.0 | Project gen |
| Combine with Ontology | `ggen ai generate-ontology + template` | 2.5.0 | Multi-step |
| Publish | `ggen marketplace publish` | 2.5.0 | Publishing |

---

### Marketplace Quick Start Tutorial (`marketplace-quick-start.md`)

**Minimum Version:** 2.6.0
**Recommended Version:** 3.2.0

| Step | Commands Used | Min Version | Notes |
|------|---------------|-------------|-------|
| Search Templates | `ggen marketplace search` | 2.5.0 | Basic search |
| Search with Filters | `ggen marketplace search --min-maturity` | 2.6.0 | **Requires 2.6.0+** |
| Get Info | `ggen marketplace info` | 2.5.0 | Package details |
| Install | `ggen marketplace install` | 2.5.0 | Installation |
| Validate | `ggen marketplace validate` | 2.6.0 | **Requires 2.6.0+** |
| Publish | `ggen marketplace publish --github-url` | 2.6.0 | **Requires 2.6.0+** |

---

### Packs Getting Started Tutorial (`packs-getting-started.md`)

**Minimum Version:** **3.0.0**
**Recommended Version:** 3.2.0

| Step | Commands Used | Min Version | Notes |
|------|---------------|-------------|-------|
| Discover Packs | `ggen packs list` | 3.0.0 | **Requires 3.0.0+** |
| Explore Pack | `ggen packs show --pack_id` | 3.0.0 | **Requires 3.0.0+** |
| Validate Pack | `ggen packs validate --pack_id` | 3.0.0 | **Requires 3.0.0+** |
| Preview Install | `ggen packs install --dry_run` | 3.0.0 | **Requires 3.0.0+** |

**Important:** Packs are a **3.0.0+ feature**. All packs tutorials require ggen 3.0.0 or later.

---

### Packs Concepts Tutorial (`packs-concepts.md`)

**Minimum Version:** **3.0.0**
**Recommended Version:** 3.2.0

This is an explanatory document with no executable commands, but concepts apply to ggen 3.0.0+.

---

## Migration Guide

### Upgrading from 2.x to 3.x

**Breaking Changes:**
- None (3.x is backward compatible with 2.6+ commands)

**New Features:**
- ✅ Packs system (`ggen packs` commands)
- ✅ Enhanced marketplace filtering
- ✅ Improved performance

**Migration Steps:**
```bash
# 1. Check current version
ggen --version

# 2. Upgrade ggen
cargo install ggen --force
# or
brew upgrade ggen

# 3. Verify new version
ggen --version  # Should show 3.2.0

# 4. Existing projects work without changes
# 5. Optionally explore new packs feature
ggen packs list
```

---

### Upgrading from 1.x to 2.x

**Breaking Changes:**
- Marketplace v2 API changes (use `ggen marketplace install <package>` instead of old syntax)
- Template naming conventions changed

**Migration Steps:**
```bash
# 1. Backup existing projects
cp -r ~/.ggen ~/.ggen.backup

# 2. Upgrade ggen
cargo install ggen@2.6.0 --force

# 3. Update marketplace templates
ggen marketplace list
ggen marketplace install <updated-packages>

# 4. Regenerate code with new templates
ggen template generate-rdf --ontology domain.ttl --template rust-models
```

---

## Version Selection Guide

**Choose your version based on needs:**

### Use 3.2.0 (Current) if you need:
- ✅ Latest features (packs, enhanced marketplace)
- ✅ Best performance
- ✅ Latest bug fixes
- ✅ Active development

### Use 3.0.0 if you need:
- ✅ Packs system
- ✅ Stable 3.x baseline
- ⚠️ Slightly older (missing 3.2.0 improvements)

### Use 2.7.0 if you need:
- ✅ University documentation features
- ✅ Proven stability
- ❌ No packs support

### Use 2.6.0 if you need:
- ✅ Production-ready 2.x
- ✅ All marketplace features
- ❌ No packs support
- ⚠️ Consider upgrading to 3.x

### Use 2.5.x or older:
- ⚠️ **Not recommended** - upgrade to 3.2.0

---

## Testing Tutorial Commands

Use the automated test suite to validate commands:

```bash
# Run all tutorial tests
./tests/tutorial_validation.sh

# Run specific tutorial tests
./tests/tutorial_validation.sh --tutorial=marketplace

# Verbose output
./tests/tutorial_validation.sh --verbose

# Available test suites:
# - cli (installation)
# - ai (AI commands)
# - template (template generation)
# - marketplace (marketplace operations)
# - packs (packs system)
# - project (project commands)
# - graph (graph operations)
# - hook (lifecycle hooks)
# - utils (utility commands)
```

---

## Support & Resources

**Documentation:**
- [Full Documentation](../README.md)
- [Installation Guide](../docs/how-to-guides/installation.md)
- [CLI Reference](../docs/reference/cli.md)

**Community:**
- [GitHub Issues](https://github.com/seanchatmangpt/ggen/issues)
- [Discussions](https://github.com/seanchatmangpt/ggen/discussions)
- [Changelog](../CHANGELOG.md)

**Version-Specific Docs:**
- [v3.2.0 Release Notes](../docs/releases/RELEASE_v3.2.0.md)
- [v3.0.0 Release Notes](../docs/releases/RELEASE_v3.0.0.md)
- [v2.6.0 Release Checklist](../docs/releases/RELEASE_v2.6.0_CHECKLIST.md)

---

**Legend:**
- ✅ Fully supported
- ⚠️ Partially supported / Added in this version
- ❌ Not supported
