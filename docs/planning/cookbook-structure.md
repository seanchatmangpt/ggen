# ggen Cookbook Structure

## Document Purpose
This document defines the comprehensive structure and organization of the ggen Cookbook. The cookbook will guide users through common tasks, workflows, and patterns using a problem-solution-example format.

---

## 📖 Cookbook Organization

### Chapter 1: Introduction & Setup (5 recipes)
**Goal**: Get users productive in under 15 minutes

1.1. **What is ggen?** - Understanding graph-aware code generation
1.2. **Installation & Verification** - Platform-specific setup
1.3. **Your First Template** - Hello World in 5 minutes
1.4. **Understanding Template Structure** - YAML frontmatter + body
1.5. **Development Environment Setup** - IDE configuration, cargo-make

### Chapter 2: Quick Start Recipes (10 recipes)
**Goal**: 5-minute wins for common scenarios

2.1. **Generate a Rust Module** - Basic code generation
2.2. **Create Multiple Files at Once** - Batch generation
2.3. **Add Variables to Templates** - Variable substitution
2.4. **Use Built-in Filters** - capitalize, snake_case, etc.
2.5. **Generate for Different Languages** - Language-agnostic patterns
2.6. **Search the Marketplace** - Finding templates
2.7. **Install a Template Pack** - Using gpacks
2.8. **List Available Templates** - Discovery
2.9. **Generate with Command-Line Variables** - CLI overrides
2.10. **Check Generation Determinism** - Reproducible outputs

### Chapter 3: Template Authoring (15 recipes)
**Goal**: Master template creation

3.1. **Basic Template Anatomy** - Frontmatter fields explained
3.2. **Variable Scoping & Precedence** - CLI > frontmatter > defaults
3.3. **Conditional Blocks** - {% if %} logic
3.4. **Loops & Iterations** - {% for %} patterns
3.5. **Custom Filters** - Tera filter reference
3.6. **Including Sub-Templates** - Template composition
3.7. **Output Path Patterns** - Dynamic file paths
3.8. **Multi-File Generation** - Single template → multiple files
3.9. **Template Testing** - Validation strategies
3.10. **Error Handling** - Graceful failures
3.11. **Template Documentation** - Self-documenting templates
3.12. **Version Management** - Template versioning
3.13. **Default Values** - Sensible defaults
3.14. **Environment Variables** - Using env vars
3.15. **Template Debugging** - Troubleshooting techniques

### Chapter 4: RDF & Knowledge Graphs (12 recipes)
**Goal**: Leverage semantic data

4.1. **RDF Basics for ggen** - Triples, subjects, predicates, objects
4.2. **Inline RDF in Turtle Format** - Embedding RDF
4.3. **Loading External RDF Files** - graph.files reference
4.4. **Using Namespace Prefixes** - Common vocabularies (FOAF, RDFS, etc.)
4.5. **Basic SPARQL Queries** - SELECT queries
4.6. **Variable Extraction from SPARQL** - sparql.vars
4.7. **Complex SPARQL Patterns** - FILTER, OPTIONAL, UNION
4.8. **Graph Visualization** - Inspecting your knowledge graph
4.9. **Multi-Source RDF Merging** - Combining graphs
4.10. **RDF Validation** - SHACL patterns
4.11. **Performance Optimization** - Caching, indexing
4.12. **RDF Debugging** - Common pitfalls

### Chapter 5: File Injection & Modification (8 recipes)
**Goal**: Modify existing code safely

5.1. **Understanding Injection Modes** - before, after, append, prepend
5.2. **Pattern-Based Injection** - Using regex patterns
5.3. **Idempotent Updates** - skip_if to prevent duplicates
5.4. **Multi-Line Injection** - Block insertion
5.5. **Conditional Injection** - When to inject
5.6. **Injection with Indentation** - Preserving code style
5.7. **Safe Injection Testing** - Dry-run mode
5.8. **Rollback Strategies** - Undo injection

### Chapter 6: Deterministic Generation (6 recipes)
**Goal**: Achieve 100% reproducibility

6.1. **Fixed Seeds for RNG** - determinism.seed
6.2. **Timestamp Freezing** - Reproducible dates
6.3. **Hash-Based IDs** - Consistent identifiers
6.4. **Build Reproducibility** - CI/CD integration
6.5. **Testing Determinism** - Validation
6.6. **Debugging Non-Determinism** - Finding sources of randomness

### Chapter 7: Marketplace & gpacks (10 recipes)
**Goal**: Leverage community templates

7.1. **Browsing the Marketplace** - search, categories
7.2. **Installing Packages** - add command
7.3. **Version Pinning** - Specific versions
7.4. **Updating Packages** - update command
7.5. **Creating Your Own gpack** - Package structure
7.6. **Publishing to Marketplace** - Submission process
7.7. **Versioning Your gpack** - Semantic versioning
7.8. **Post-Quantum Signatures** - ML-DSA security
7.9. **Package Dependencies** - gpack composition
7.10. **Private Packages** - Custom registries

### Chapter 8: Multi-Language Patterns (12 recipes)
**Goal**: Generate code in any language

8.1. **Rust Module Generation** - Cargo projects
8.2. **Python Package Generation** - setup.py, __init__.py
8.3. **TypeScript/JavaScript** - NPM packages
8.4. **Go Modules** - go.mod patterns
8.5. **Java Classes** - Maven/Gradle
8.6. **C/C++ Headers** - Include guards
8.7. **SQL Schema** - DDL generation
8.8. **GraphQL Schemas** - Type definitions
8.9. **Protocol Buffers** - .proto files
8.10. **OpenAPI/Swagger** - API specs
8.11. **Markdown Documentation** - Docs generation
8.12. **Configuration Files** - YAML, TOML, JSON

### Chapter 9: GitHub Integration (7 recipes)
**Goal**: Automate deployments

9.1. **GitHub Pages Status** - pages-status command
9.2. **Workflow Monitoring** - workflow-status
9.3. **Triggering Workflows** - CI automation
9.4. **Release Automation** - Version bumps
9.5. **Documentation Deployment** - Auto-deploy docs
9.6. **GitHub Actions Integration** - Workflow recipes
9.7. **Pages Troubleshooting** - Common issues

### Chapter 10: Advanced Workflows (15 recipes)
**Goal**: Complex real-world scenarios

10.1. **Monorepo Code Generation** - Multi-project setup
10.2. **Microservices Scaffolding** - Service templates
10.3. **CLI Tool Generation** - Clap/structopt patterns
10.4. **API Client Generation** - From OpenAPI
10.5. **Database Migration Generation** - Schema evolution
10.6. **Test Suite Generation** - From specs
10.7. **Mock Data Generation** - Fixtures
10.8. **Configuration Management** - Environment-specific
10.9. **Multi-Target Builds** - Cross-platform
10.10. **Localization/i18n** - Multi-language support
10.11. **Documentation from Code** - Reverse engineering
10.12. **Code Migration** - Language translation
10.13. **Dependency Graphs** - RDF-based analysis
10.14. **Custom Linters** - Code quality rules
10.15. **Performance Benchmarks** - Auto-generated tests

### Chapter 11: Project Command Deep Dive (8 recipes)
**Goal**: Master project-level operations

11.1. **Project Initialization** - project init
11.2. **Project Structure** - .ggen directory
11.3. **Local vs Remote Templates** - Resolution order
11.4. **Project-Specific Variables** - Configuration
11.5. **Template Overrides** - Customization
11.6. **Batch Generation** - Multiple templates
11.7. **Project Templates** - Full scaffolding
11.8. **Migration Strategies** - Upgrading projects

### Chapter 12: CLI Commands Reference (10 recipes)
**Goal**: Comprehensive command guide

12.1. **gen Command** - Generation options
12.2. **search Command** - Finding templates
12.3. **add Command** - Installing packages
12.4. **list Command** - Discovery
12.5. **packs Command** - Installed packages
12.6. **show Command** - Package details
12.7. **categories Command** - Browsing
12.8. **update Command** - Updating
12.9. **remove Command** - Uninstalling
12.10. **completion Command** - Shell completions

### Chapter 13: Testing & Validation (8 recipes)
**Goal**: Ensure quality

13.1. **Template Unit Testing** - cargo make test
13.2. **BDD Testing** - Cucumber scenarios
13.3. **Determinism Testing** - cargo make deterministic
13.4. **Integration Testing** - End-to-end
13.5. **Coverage Analysis** - cargo make test-coverage
13.6. **Linting Templates** - Quality checks
13.7. **Security Audits** - cargo make audit
13.8. **CI/CD Integration** - cargo make ci

### Chapter 14: Performance & Optimization (7 recipes)
**Goal**: Fast, efficient generation

14.1. **Profiling Generation** - Benchmarking
14.2. **RDF Graph Optimization** - Indexing
14.3. **Template Caching** - Speed improvements
14.4. **Parallel Generation** - Concurrency
14.5. **Memory Management** - Large graphs
14.6. **Build Time Optimization** - Incremental builds
14.7. **Meeting SLOs** - Performance targets

### Chapter 15: Troubleshooting & FAQs (12 recipes)
**Goal**: Solve common problems

15.1. **Template Not Found** - Resolution issues
15.2. **RDF Parse Errors** - Syntax problems
15.3. **SPARQL Query Failures** - Debugging queries
15.4. **Variable Substitution Issues** - Missing vars
15.5. **Injection Failures** - Pattern mismatches
15.6. **Marketplace Connection** - Network issues
15.7. **Version Conflicts** - Dependency hell
15.8. **Non-Deterministic Output** - Randomness
15.9. **GitHub API Errors** - Authentication
15.10. **Build Failures** - cargo-make issues
15.11. **Path Problems** - File system issues
15.12. **Common Error Messages** - Interpretation guide

### Appendices

#### Appendix A: Recipe Template Format
**Standard structure for all recipes**

```markdown
## Recipe X.Y: [Title]

### Problem
What user problem does this solve? (1-2 sentences)

### Solution
High-level approach (2-3 sentences)

### Example
```bash
# Command to run
ggen gen template.tmpl --vars name=example
```

**Input Template** (`template.tmpl`):
```yaml
---
to: "src/{{name}}.rs"
vars:
  name: "example"
---
pub struct {{name | capitalize}} {}
```

**Generated Output** (`src/example.rs`):
```rust
pub struct Example {}
```

### Explanation
Step-by-step breakdown:
1. The `to` field specifies output path
2. Variables are defined in frontmatter
3. Tera filters transform values
4. Output is deterministic

### Expected Output
```
✅ Generated: src/example.rs
🔒 Deterministic: true
```

### Common Pitfalls
- Forgetting to capitalize struct names
- Missing frontmatter separators

### See Also
- Recipe 2.3: Add Variables to Templates
- Recipe 3.5: Custom Filters
```

#### Appendix B: Tera Filter Reference
Complete filter documentation

#### Appendix C: SPARQL Quick Reference
Common query patterns

#### Appendix D: RDF Vocabularies
Useful ontologies for code generation

#### Appendix E: cargo-make Command Matrix
All available tasks

#### Appendix F: Error Code Reference
Complete error catalog

#### Appendix G: Glossary
Term definitions

---

## 📊 Recipe Metadata Schema

Each recipe includes:
- **Recipe ID**: Chapter.Number (e.g., 2.3)
- **Title**: Clear, action-oriented
- **Problem**: User pain point
- **Solution**: Approach summary
- **Example**: Runnable code
- **Explanation**: Step-by-step breakdown
- **Expected Output**: What user should see
- **Common Pitfalls**: What to avoid
- **See Also**: Cross-references (minimum 2)
- **Tags**: Searchable keywords
- **Difficulty**: Beginner/Intermediate/Advanced
- **Estimated Time**: 5min/15min/30min/1hr

---

## 🎯 Style Guide

### Voice & Tone
- **Active voice**: "Generate a module" not "A module is generated"
- **Direct address**: "You can..." or "To achieve..."
- **Encouraging**: "This is easy once you understand..."
- **Practical**: Focus on doing, not theory

### Code Examples
- **Always runnable**: Copy-paste ready
- **Minimal**: Only essential code
- **Annotated**: Comments explain non-obvious parts
- **Realistic**: Based on real use cases

### Structure
- **Problem first**: Lead with user pain
- **Quick win**: Show solution immediately
- **Deep dive**: Explain after showing
- **Extend**: Link to related recipes

### Formatting
- **Consistent**: Use standard recipe template
- **Visual**: Use emojis for sections (✅ ❌ ⚠️ 💡)
- **Scannable**: Headers, bullets, code blocks
- **Accessible**: Clear language, no jargon

---

## 🔗 Cross-Reference Strategy

### Linking Patterns
- **Forward references**: "See Chapter 4 for RDF details"
- **Backward references**: "As shown in Recipe 2.1..."
- **Related recipes**: Minimum 2 per recipe
- **Progression**: Build on previous recipes

### Navigation Aids
- **Chapter summaries**: Key takeaways
- **Quick reference**: Command cheat sheets
- **Index**: Searchable by topic
- **Tags**: Filter by concern (testing, RDF, CLI, etc.)

---

## 📈 Success Metrics

### Reader Outcomes
- **5 min**: Run first successful generation
- **15 min**: Create custom template
- **30 min**: Use RDF/SPARQL
- **1 hour**: Publish to marketplace
- **2 hours**: Build production workflow

### Recipe Quality
- **Clarity**: Testable by new users
- **Completeness**: No missing steps
- **Correctness**: Verified examples
- **Currency**: Up-to-date with latest version

---

## 🚀 Implementation Plan

### Phase 1: Core Recipes (Chapters 1-3)
- Focus: Getting started, quick wins, basic templates
- Priority: High (foundation)
- Estimated: 30 recipes

### Phase 2: Advanced Features (Chapters 4-6)
- Focus: RDF, SPARQL, determinism
- Priority: High (differentiation)
- Estimated: 26 recipes

### Phase 3: Ecosystem (Chapters 7-9)
- Focus: Marketplace, multi-language, GitHub
- Priority: Medium (community)
- Estimated: 29 recipes

### Phase 4: Mastery (Chapters 10-14)
- Focus: Complex workflows, testing, performance
- Priority: Medium (power users)
- Estimated: 48 recipes

### Phase 5: Reference (Chapter 15 + Appendices)
- Focus: Troubleshooting, references
- Priority: Low (support)
- Estimated: 12 recipes + appendices

**Total Recipe Count**: ~145 recipes

---

## 📝 Recipe Template (Reusable)

```markdown
## Recipe X.Y: [Action-Oriented Title]

**Difficulty**: ⭐ Beginner | ⭐⭐ Intermediate | ⭐⭐⭐ Advanced
**Time**: 5min | 15min | 30min | 1hr
**Tags**: #tag1 #tag2 #tag3

### Problem
[What user problem does this solve? Be specific and relatable.]

### Solution
[High-level approach to solving the problem. 2-3 sentences max.]

### Prerequisites
- [Required knowledge or setup]
- [Dependencies needed]

### Step-by-Step

#### 1. [First Action]
```bash
# Command with explanation
ggen command --option value
```

#### 2. [Second Action]
[Explanation of what happens]

```yaml
# Template code
---
frontmatter: here
---
template body
```

#### 3. [Third Action]
[Expected result]

### Complete Example

**Input**: `template.tmpl`
```yaml
[Full template code]
```

**Command**:
```bash
ggen gen template.tmpl --vars key=value
```

**Output**: `generated/file.ext`
```
[Full generated code]
```

### Explanation
1. **Line 1-3**: [What happens and why]
2. **Line 4-6**: [Next concept]
3. **Result**: [Final outcome]

### Expected Output
```
✅ Success message
📄 File: path/to/file
🔒 Deterministic: true
```

### Verification
```bash
# How to verify it worked
cat generated/file.ext
```

### Common Pitfalls

❌ **Mistake**: [Common error]
- **Symptom**: [What user sees]
- **Fix**: [How to resolve]

❌ **Mistake**: [Another error]
- **Symptom**: [What happens]
- **Fix**: [Solution]

### Variations

💡 **Tip**: [Useful variation or enhancement]

### Troubleshooting

**Issue**: [Problem that might occur]
**Cause**: [Why it happens]
**Solution**: [How to fix]

### See Also
- Recipe X.Y: [Related recipe with brief description]
- Recipe X.Z: [Another related recipe]
- Appendix X: [Reference material]

### Next Steps
- [What to try next]
- [How to extend this recipe]
```

---

## 🎨 Visual Elements

### Emoji Guide
- ✅ Success/Correct
- ❌ Error/Incorrect
- ⚠️ Warning/Caution
- 💡 Tip/Insight
- 🔒 Determinism/Security
- 📄 File/Document
- 📦 Package/Module
- 🔗 Link/Reference
- 🚀 Performance/Speed
- 🧪 Testing/Experimental
- ⭐ Difficulty level
- 🎯 Goal/Objective

### Code Block Annotations
```yaml
---
# ← This is the frontmatter separator
to: "output/path.rs"  # ← Output destination
vars:                 # ← Variable definitions
  name: "value"       # ← Key-value pair
---
# ↓ Template body starts here
Code with {{name}} substitution
```

---

## 📚 Table of Contents Format

```markdown
# ggen Cookbook

## Table of Contents

### 1. Introduction & Setup
1.1. What is ggen?
1.2. Installation & Verification
[...]

### 2. Quick Start Recipes
2.1. Generate a Rust Module
2.2. Create Multiple Files at Once
[...]

### 15. Troubleshooting & FAQs
15.1. Template Not Found
[...]

### Appendices
A. Recipe Template Format
B. Tera Filter Reference
[...]
```

---

## 🔄 Maintenance Plan

### Version Updates
- Update examples when CLI changes
- Add new recipes for new features
- Archive deprecated patterns
- Refresh screenshots/outputs

### Community Contributions
- Accept recipe submissions
- Template for community recipes
- Review process
- Attribution standards

### Quality Assurance
- Test all recipes before publication
- Automated validation
- User feedback integration
- Continuous improvement

---

## 📖 Publishing Strategy

### Formats
1. **Markdown** (primary) - GitHub, docs site
2. **HTML** - GitHub Pages
3. **PDF** - Downloadable reference
4. **EPUB** - E-reader format

### Distribution
- GitHub repository (main source)
- GitHub Pages (rendered HTML)
- Package with ggen releases
- Community mirrors

### Versioning
- Cookbook version matches ggen version
- Major: Breaking changes to recipes
- Minor: New recipes added
- Patch: Fixes and clarifications

---

**Total Structure**: 15 Chapters + 7 Appendices = 145+ Recipes

This comprehensive structure ensures users can find solutions quickly, learn progressively, and master ggen from basics to advanced workflows.
