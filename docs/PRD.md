# ggen Product Requirements Document

**Vision**: Deterministic, ontology-driven code generation across languages
**Last Updated**: 2025-12-11

---

## Executive Summary

**ggen** is a language-agnostic, deterministic code generation CLI that uses RDF ontologies and SPARQL queries to project knowledge graphs into reproducible code across multiple programming languages.

**Key Differentiator**: Semantic validation + deterministic outputs + multi-language support via single ontology definition.

---

## Target Users

### 1. Backend Developers (Primary)

**Profile**:
- Languages: Rust, Python, JavaScript/TypeScript
- Experience: Mid to senior level (3+ years)
- Pain Points: Schema duplication across languages, boilerplate code, inconsistent API definitions

**Use Cases**:
- Generate REST API boilerplate from OpenAPI specs
- Create database models from RDF schemas
- Maintain consistent types across microservices
- Generate client SDKs from API definitions

**Success Metrics**:
- 80% reduction in boilerplate code writing
- Zero schema drift between services
- 100% reproducible code generation

---

### 2. Data Engineers (Secondary)

**Profile**:
- Technologies: RDF, SPARQL, knowledge graphs, semantic web
- Experience: Senior level with domain expertise
- Pain Points: Lack of tooling for RDF-driven development, manual schema evolution

**Use Cases**:
- Generate code from OWL ontologies
- Validate RDF data against SHACL constraints
- Transform knowledge graphs into application code
- Maintain ontology-driven architectures

**Success Metrics**:
- Support for all major RDF formats (Turtle, RDF/XML, N-Triples, JSON-LD)
- SPARQL 1.1 full compliance
- Sub-5s query performance on 1k+ triple graphs

---

### 3. AI Engineers (Emerging)

**Profile**:
- Technologies: LLMs, prompt engineering, AI-assisted development
- Experience: Early adopters, experimental mindset
- Pain Points: Non-deterministic LLM outputs, difficult to integrate AI into CI/CD

**Use Cases**:
- AI-enhanced code generation with schema validation
- LLM-powered template filling with type safety
- Semantic search over codebases using RDF
- Automated documentation generation

**Success Metrics**:
- Multi-provider support (OpenAI, Anthropic, Ollama)
- Graceful degradation (works without AI)
- Deterministic when AI disabled, creative when enabled

---

## Core Features (80/20 - Highest Value)

### Feature 1: RDF/SPARQL Processing (MUST HAVE)

**Problem**: No developer-friendly RDF tooling for code generation

**Solution**: Embedded Oxigraph engine with CLI interface

**Capabilities**:
- Load RDF from files (Turtle, RDF/XML, N-Triples, JSON-LD)
- Execute SPARQL 1.1 queries
- Export results as JSON, CSV, or back to RDF
- Persistent and in-memory storage modes
- Sub-5s performance on 1k+ triple graphs

**Example**:
```bash
# Load ontology
ggen graph load --file schema.ttl

# Query for classes
ggen graph query --sparql "SELECT ?class WHERE { ?class a owl:Class }"

# Export to JSON
ggen graph export --format json --output schema.json
```

**Priority**: P0 (critical path)
**Status**: ✅ Implemented (v4.0.0)

---

### Feature 2: Template-Based Code Generation (MUST HAVE)

**Problem**: Manual code writing from schemas is error-prone and slow

**Solution**: Tera template engine with RDF integration

**Capabilities**:
- Jinja2-like template syntax (familiar)
- Variables from SPARQL query results
- Filters for code generation (pascal_case, snake_case, camel_case)
- Multi-file generation from single template
- Deterministic outputs (same inputs → same outputs)

**Example**:
```jinja2
// Template: class.rs.tera
{% for class in classes %}
pub struct {{ class.name | pascal_case }} {
    {% for prop in class.properties %}
    pub {{ prop.name | snake_case }}: {{ prop.type }},
    {% endfor %}
}
{% endfor %}
```

**Priority**: P0 (critical path)
**Status**: ✅ Implemented (v4.0.0)

---

### Feature 3: Multi-Language Support (MUST HAVE)

**Problem**: Maintaining schemas in multiple languages is duplication nightmare

**Solution**: Single RDF ontology → multiple language outputs via templates

**Supported Languages** (via templates):
- ✅ Rust (primary target)
- ✅ Python (via community templates)
- ✅ JavaScript/TypeScript (via community templates)
- ✅ Any language (create custom template)

**Example**:
```bash
# Generate Rust structs
ggen generate --template rust/struct.rs.tera --context schema.ttl

# Generate Python dataclasses
ggen generate --template python/dataclass.py.tera --context schema.ttl

# Generate TypeScript interfaces
ggen generate --template typescript/interface.ts.tera --context schema.ttl
```

**Priority**: P0 (core value proposition)
**Status**: ✅ Implemented (v4.0.0)

---

### Feature 4: AI-Assisted Generation (SHOULD HAVE)

**Problem**: Templates handle structure, but LLMs handle nuanced logic

**Solution**: Hybrid approach - RDF for structure, AI for creativity

**Capabilities**:
- Multi-provider support (OpenAI, Anthropic, Ollama)
- Streaming responses for real-time feedback
- Token usage tracking and cost estimation
- Graceful degradation (works without AI)
- Environment-specific models (cheap in dev, expensive in prod)

**Example**:
```bash
# AI-enhanced generation
ggen ai generate \
  --template class.rs.tera \
  --context schema.ttl \
  --provider anthropic \
  --model claude-3-opus-20240229 \
  --prompt "Add comprehensive error handling and documentation"
```

**Priority**: P1 (differentiator, not blocker)
**Status**: ✅ Implemented (v4.0.0)

---

### Feature 5: Package Marketplace (SHOULD HAVE)

**Problem**: Every project recreates same templates and ontologies

**Solution**: npm-like package management for templates and ontologies

**Capabilities**:
- Template packages (ggen-templates-*)
- Ontology packages (ggen-ontologies-*)
- Semantic versioning (SemVer)
- Dependency resolution
- Lockfile-based reproducibility (ggen.lock)

**Example**:
```bash
# Install template package
ggen marketplace install ggen-templates-rust-rest-api

# Use installed template
ggen generate --template @rust-rest-api/endpoint.rs.tera

# Publish your own package
ggen marketplace publish --package my-templates
```

**Priority**: P1 (community growth, not core functionality)
**Status**: ✅ Implemented (v4.0.0)

---

### Feature 6: Lifecycle Hooks (NICE TO HAVE)

**Problem**: Manual validation and formatting after generation

**Solution**: Configurable pre/post generation hooks

**Capabilities**:
- Pre-generation: Validate prerequisites, check RDF syntax
- Post-generation: Format code, run linters, commit to git
- Environment-specific hooks (dev vs ci vs prod)
- Configurable via ggen.toml

**Example** (ggen.toml):
```toml
[lifecycle.phases.pre_generate]
scripts = ["scripts/validate-schema.sh"]

[lifecycle.phases.post_generate]
scripts = ["cargo fmt", "cargo clippy --fix"]
```

**Priority**: P2 (nice to have, not critical)
**Status**: ✅ Implemented (v4.0.0)

---

## Non-Goals (Explicitly Out of Scope)

### ❌ NOT a Build System

- ggen generates code; it does NOT compile/run it
- Use cargo, npm, pip, etc. for building
- Reason: Stay focused on generation, not execution

### ❌ NOT an IDE Plugin

- ggen is a CLI tool, not an editor extension
- IDEs can call ggen, but ggen doesn't integrate directly
- Reason: CLI-first design, IDE plugins are wrappers

### ❌ NOT a Schema Registry

- ggen reads RDF files; it does NOT host/serve them
- Use Git, S3, or dedicated registries for storage
- Reason: Keep tool focused, leverage existing infrastructure

### ❌ NOT a Validator (Primary Use Case)

- ggen can validate RDF syntax, but it's not a full validator
- Use SHACL validators for complex validation
- Reason: Validation is secondary, generation is primary

---

## Success Metrics

### Technical Metrics

- ✅ **Test Coverage**: 80%+ on critical paths (currently: 82.4%)
- ✅ **Build Performance**: < 5s incremental builds (currently: 0.8s)
- ✅ **Runtime Performance**: < 5s RDF processing for 1k+ triples (currently: 3.2s)
- ✅ **Determinism**: 100% reproducible outputs (verified in CI)
- ✅ **Correctness**: 1,168+ passing tests (currently: 1,168)

### User Adoption Metrics

- 🎯 **GitHub Stars**: 1,000+ (indicates interest)
- 🎯 **crates.io Downloads**: 10,000+/month (indicates usage)
- 🎯 **Marketplace Packages**: 50+ community templates
- 🎯 **Documentation Quality**: < 5 minute time-to-first-success

### Production Readiness (v4.0.0)

- ✅ **API Stability**: Semantic versioning enforced
- ✅ **Error Messages**: Actionable and helpful
- ✅ **Documentation**: Complete (tutorials, how-tos, reference, explanations)
- ✅ **CI/CD**: All checks automated
- ✅ **Release Process**: Automated GitHub Actions

---

## Roadmap (Future Versions)

### v4.1.0 (Q1 2025) - Performance & Polish

**Focus**: Optimize hot paths, improve UX

**Features**:
- Parallel SPARQL query execution (5-10x speedup)
- Template compilation cache (2x faster generation)
- Improved error messages with suggestions
- Interactive tutorial (`ggen tutorial`)

**Metrics**: Sub-1s generation for 100+ files

---

### v4.2.0 (Q2 2025) - Community Growth

**Focus**: Marketplace expansion, ecosystem building

**Features**:
- Marketplace web UI (browse templates online)
- Template testing framework (validate templates)
- Community template showcase
- Template analytics (usage stats)

**Metrics**: 100+ community templates, 50k+ monthly downloads

---

### v5.0.0 (Q3 2025) - Advanced AI Integration

**Focus**: LLM-powered features, semantic code search

**Features**:
- Code understanding via embeddings (semantic search)
- Natural language → SPARQL translation
- AI-powered template suggestions
- Automated ontology inference from code

**Metrics**: 80%+ accuracy on NL→SPARQL, sub-3s search

---

## Target Personas (Detailed)

### Persona 1: "Backend Bob" - Rust API Developer

**Background**:
- Senior backend engineer at mid-size startup
- Builds microservices in Rust
- Tired of writing same boilerplate for every service

**Goals**:
- Generate REST API boilerplate from OpenAPI specs
- Keep types consistent across 5+ microservices
- Reduce time spent on repetitive code

**Pain Points**:
- Copy-pasting code between services introduces bugs
- OpenAPI → Rust code generation tools are incomplete
- Schema changes require manual updates in 5+ places

**How ggen Helps**:
1. Define API schema once in RDF (OpenAPI → RDF converter)
2. Generate Rust structs, endpoints, tests from single source
3. Update schema → regenerate → guaranteed consistency

**Success**: Reduces API development time from 3 days → 1 day

---

### Persona 2: "Data Engineer Dana" - Knowledge Graph Expert

**Background**:
- Data engineer at enterprise company
- Builds knowledge graphs for data integration
- Deep RDF/SPARQL expertise, limited coding skills

**Goals**:
- Generate application code from OWL ontologies
- Validate RDF data against schemas
- Make ontologies usable by application developers

**Pain Points**:
- Developers don't understand RDF
- Manual translation of ontologies to code is error-prone
- No good tooling for ontology-driven development

**How ggen Helps**:
1. Design ontology in Protégé (OWL editor)
2. Export to Turtle, use ggen to generate Rust/Python code
3. Developers use generated types, Dana maintains ontology

**Success**: Bridges gap between semantic web and application development

---

### Persona 3: "AI Engineer Alex" - LLM Integration Specialist

**Background**:
- AI engineer at AI-first startup
- Integrates LLMs into development workflows
- Frustrated by non-deterministic LLM outputs

**Goals**:
- Use AI for code generation, but maintain quality
- Validate LLM outputs against schemas
- Integrate AI into CI/CD without breaking builds

**Pain Points**:
- Raw LLM outputs fail type checking
- Can't trust LLM to generate correct code
- Hard to use AI in automated pipelines

**How ggen Helps**:
1. Define schema in RDF (types, constraints)
2. Use ggen AI mode to generate code with LLM
3. ggen validates LLM output against schema, regenerates if invalid

**Success**: AI-enhanced generation with schema guarantees

---

## Competitive Analysis

### ggen vs. OpenAPI Generator

| Feature | ggen | OpenAPI Generator |
|---------|------|-------------------|
| Input format | RDF (any ontology) | OpenAPI spec only |
| Semantic validation | ✅ SPARQL constraints | ❌ JSON Schema only |
| Multi-language | ✅ Via templates | ✅ Built-in (40+ langs) |
| Extensibility | ✅ Custom templates | ⚠️ Java code generation |
| AI integration | ✅ Multi-provider | ❌ None |
| Determinism | ✅ Guaranteed | ⚠️ Templates can vary |

**When to use ggen**: Schema-first with semantic validation, AI-enhanced generation
**When to use OpenAPI Gen**: Pure OpenAPI, need 40+ languages out-of-box

---

### ggen vs. GraphQL Code Generator

| Feature | ggen | GraphQL Codegen |
|---------|------|-----------------|
| Input format | RDF | GraphQL schema |
| Query language | SPARQL | GraphQL |
| Code generation | ✅ Full control | ✅ Full control |
| Semantic reasoning | ✅ OWL inference | ❌ No reasoning |
| AI integration | ✅ Built-in | ❌ None |
| Ecosystem | 🚀 Growing | ✅ Mature (TypeScript-focused) |

**When to use ggen**: Ontology-driven, semantic reasoning, non-GraphQL projects
**When to use GraphQL Codegen**: Pure GraphQL projects, need mature ecosystem

---

### ggen vs. Protobuf Compiler (protoc)

| Feature | ggen | protoc |
|---------|------|--------|
| Input format | RDF | Protocol Buffers |
| Schema evolution | ✅ Flexible (RDF extensibility) | ⚠️ Strict versioning |
| Semantic validation | ✅ SPARQL | ❌ Type checking only |
| Code generation | ✅ Via templates | ✅ Built-in (10+ langs) |
| AI integration | ✅ Multi-provider | ❌ None |
| Performance | ✅ Sub-5s for 1k+ triples | ✅ Sub-second |

**When to use ggen**: Schema-first with semantic web, AI-enhanced
**When to use protoc**: RPC systems, need battle-tested serialization

---

## Pricing & Distribution

### Open Source (MIT License)

**Why Open Source**:
- Lower adoption barrier
- Community contributions (templates, ontologies)
- Trust through transparency

**Revenue Model**: None (pure open source)

**Future Consideration**: SaaS marketplace (template hosting, analytics)

---

## Risk Assessment

### Risk 1: Steep Learning Curve (RDF/SPARQL)

**Likelihood**: High
**Impact**: High (limits adoption)

**Mitigation**:
- Comprehensive tutorials for RDF beginners
- Converter tools (JSON → RDF, OpenAPI → RDF)
- Template marketplace (use without RDF knowledge)
- Interactive onboarding (`ggen tutorial`)

---

### Risk 2: Marketplace Quality Control

**Likelihood**: Medium
**Impact**: High (poor quality templates damage reputation)

**Mitigation**:
- Template validation framework
- Community ratings and reviews
- Official "verified" templates
- Automated quality checks in CI

---

### Risk 3: Performance at Scale

**Likelihood**: Low
**Impact**: Medium (slow adoption by large enterprises)

**Mitigation**:
- Continuous benchmarking (track SLOs)
- Parallel query execution (v4.1.0)
- Template compilation cache (v4.1.0)
- Performance budgets in CI

---

## Conclusion

**ggen is production-ready (v4.0.0)** with:
- ✅ 1,168+ passing tests
- ✅ 82.4% code coverage
- ✅ Sub-5s build times
- ✅ Comprehensive documentation
- ✅ Automated CI/CD

**Target Users**: Backend developers, data engineers, AI engineers
**Core Value**: Deterministic, ontology-driven code generation across languages
**Differentiator**: Semantic validation + AI integration + multi-language via single ontology

**Next Milestone**: 1,000 GitHub stars, 50+ marketplace templates by Q2 2025

---

**Detailed Requirements**: See `docs/architecture/` for technical specifications
**User Stories**: See `specs/` directory for feature specifications
**API Reference**: See `docs/reference/` for complete API documentation
