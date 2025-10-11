# Changelog

All notable changes to this project will be documented in this file.

## [1.0.0] - 2025-10-10

### 🚀 **AI-Powered Code Generation Revolution**

#### 🤖 **AI-Enhanced Generation**
- **AI-Powered Templates** - Generate templates using advanced LLMs (GPT-4o, Claude 3.5, Qwen3-coder:30b)
- **Intelligent SPARQL Generation** - Create semantic queries from natural language descriptions
- **Smart RDF Graph Generation** - Build ontologies and knowledge graphs with AI assistance
- **Iterative Validation** - Automatic template improvement based on validation feedback
- **Reference File Generation** - Automatic Rust reference files for generated RDF graphs

#### 🧠 **Intelligent Project Scaffolding**
- **Multi-Language Project Generation** - Create complete projects in Rust, Python, JavaScript, Go
- **Framework Integration** - Support for Actix-web, Axum, FastAPI, Django, Express, Next.js, Gin
- **CI/CD Setup** - Automatic GitHub Actions workflows for generated projects
- **Documentation Generation** - Auto-generated README files and project documentation
- **Testing Frameworks** - Integrated test setup for all supported languages

#### 🔍 **Natural Language Search**
- **Conversational Queries** - Find templates using natural language descriptions
- **AI-Powered Interpretation** - Smart keyword extraction and category mapping
- **Confidence Scoring** - AI confidence assessment for search suggestions
- **Context-Aware Results** - Results tailored to user intent and project context

#### 📋 **Smart Frontmatter**
- **AI-Generated Metadata** - Create YAML/JSON frontmatter using LLMs
- **Format Conversion** - Seamless conversion between YAML and JSON formats
- **Template Variable Extraction** - Automatic variable discovery from descriptions
- **Validation Integration** - Frontmatter validation with AI feedback

#### 🎯 **Latest Model Support**
- **OpenAI**: GPT-4o, GPT-4o-mini, GPT-4-turbo-preview
- **Anthropic**: Claude 3.5 Sonnet (20241022), Claude 3.5 Haiku (20241022)
- **Ollama**: Qwen3-coder:30b (default), Llama 2:70b, and custom models
- **Flexible Configuration** - Environment-based configuration with TOML support

#### 🔧 **Enhanced CLI**
- **New Commands**: `ai generate`, `ai sparql`, `ai graph`, `ai project`, `ai frontmatter`
- **Natural Language Market**: `market natural` for conversational package discovery
- **Improved Error Handling** - Structured error types with actionable messages
- **Environment Configuration** - Secure API key management via `.env` files

### 📚 **Comprehensive Documentation**
- **AI Migration Guide** - Complete migration guide for ggen-ai v2.0
- **Model Support Matrix** - Detailed compatibility and performance information
- **Best Practices Guide** - Production-ready patterns and recommendations
- **Troubleshooting Guide** - Common issues and solutions for AI features

### 🔒 **Security & Performance**
- **Environment-Based Configuration** - No hardcoded API keys
- **Structured Error Handling** - Type-safe error management
- **Performance SLOs** - All AI operations meet strict performance targets
- **Memory Safety** - Zero unsafe code in AI components

### 📦 **Marketplace Enhancements**
- **AI-Discovered Templates** - Templates found through natural language search
- **Enhanced Metadata** - Rich package information with AI-generated descriptions
- **Quality Scoring** - Template quality assessment with validation feedback

### 🔄 **Backward Compatibility**
- **Seamless Migration** - Existing templates and workflows continue to work
- **Gradual Adoption** - AI features are opt-in and don't affect existing functionality
- **Configuration Preservation** - Existing configurations remain valid

---

## [0.1.0] - 2025-01-XX

### Added
- Initial release of ggen CLI tool
- Language-agnostic code generation from RDF ontologies
- Template system with YAML frontmatter support
- Marketplace ecosystem for reusable gpacks (search, add, remove, update, packs)
- Deterministic code generation with manifest hashing
- Multi-language template support (Rust, Python, Bash, TypeScript, Go)
- CLI commands: gen, list, show, lint, validate, graph, hazard, completion
- RDF/SPARQL integration for semantic code generation
- Template validation and linting
- Shell completion generation (bash, zsh, fish)
- Comprehensive documentation and examples

### Features
- **Marketplace**: Search, install, and manage reusable code generation packs
- **Templates**: YAML frontmatter with RDF, SPARQL, and determinism support
- **Generation**: Deterministic, reproducible code projection from ontologies
- **Validation**: Template and RDF graph validation with SHACL
- **Performance**: Meets all SLOs (build ≤15s, incremental ≤2s, generation ≤3s)

### Documentation
- Installation guide
- Quick start tutorial
- Template development guide
- Marketplace usage instructions
- RDF/SPARQL integration guide
- CLI reference
- Troubleshooting guide

[1.0.0]: https://github.com/seanchatmangpt/ggen/releases/tag/v1.0.0
[0.1.0]: https://github.com/seanchatmangpt/ggen/releases/tag/v0.1.0