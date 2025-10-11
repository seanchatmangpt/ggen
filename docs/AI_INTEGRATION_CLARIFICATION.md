<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [AI Integration Clarification](#ai-integration-clarification)
  - [✅ **Correct Approach: CLI Commands, Not Template Frontmatter**](#-correct-approach-cli-commands-not-template-frontmatter)
  - [🎯 **Why CLI Commands Are Better**](#-why-cli-commands-are-better)
    - [**1. Separation of Concerns**](#1-separation-of-concerns)
    - [**2. User Experience**](#2-user-experience)
    - [**3. Maintainability**](#3-maintainability)
  - [🚀 **Available AI Commands**](#-available-ai-commands)
    - [**Template Generation**](#template-generation)
    - [**Template Validation**](#template-validation)
    - [**Project Generation**](#project-generation)
    - [**SPARQL & RDF Generation**](#sparql--rdf-generation)
    - [**MCP Server Integration**](#mcp-server-integration)
  - [📋 **Template System Status**](#-template-system-status)
    - [**✅ Current Template Features (No AI Dependencies)**](#-current-template-features-no-ai-dependencies)
    - [**✅ AI Features (CLI Commands)**](#-ai-features-cli-commands)
  - [🎯 **Best Practices**](#-best-practices)
    - [**For Template Authors**](#for-template-authors)
    - [**For Users**](#for-users)
    - [**For Developers**](#for-developers)
  - [🔧 **Implementation Details**](#-implementation-details)
    - [**Template System (`ggen-core`)**](#template-system-ggen-core)
    - [**AI Integration (`ggen-ai`)**](#ai-integration-ggen-ai)
    - [**CLI Commands (`cli/src/cmds/ai/`)**](#cli-commands-clisrccmdsai)
  - [📊 **Summary**](#-summary)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# AI Integration Clarification

## ✅ **Correct Approach: CLI Commands, Not Template Frontmatter**

The AI enhancement features (`ai_enhance`, `ai_complete`, `ai_validate`) are **NOT** template frontmatter fields. They are implemented as **CLI commands** for better separation of concerns and maintainability.

## 🎯 **Why CLI Commands Are Better**

### **1. Separation of Concerns**
- **Templates**: Focus on deterministic, reproducible code generation
- **AI Features**: Optional enhancements accessible via CLI commands
- **Clean Architecture**: No AI dependencies in core template rendering

### **2. User Experience**
- **Explicit Control**: Users choose when to use AI features
- **Predictable Output**: Templates remain deterministic without AI
- **Flexible Workflow**: AI can be used for generation, validation, or enhancement

### **3. Maintainability**
- **No Template Pollution**: Templates stay focused on their core purpose
- **Independent Evolution**: AI features can evolve without affecting templates
- **Clear Boundaries**: Easy to understand what uses AI vs. what doesn't

## 🚀 **Available AI Commands**

### **Template Generation**
```bash
# Generate templates using AI
ggen ai generate -d "REST API module" --ollama

# Generate with validation
ggen ai generate -d "Database model" --validate --threshold 0.8
```

### **Template Validation**
```bash
# Validate existing templates
ggen ai validate template.tmpl

# Strict validation
ggen ai validate template.tmpl --strict
```

### **Project Generation**
```bash
# AI-enhanced project generation
ggen project gen --ai --ai-provider ollama

# With specific model
ggen project gen --ai --ai-provider ollama --ai-model qwen3-coder:30b
```

### **SPARQL & RDF Generation**
```bash
# Generate SPARQL queries
ggen ai sparql -d "Find all users" -g schema.ttl

# Generate RDF graphs
ggen ai graph -d "User ontology" -o users.ttl
```

### **MCP Server Integration**
```bash
# Start AI MCP server
USE_OLLAMA=true OLLAMA_MODEL=qwen3-coder:30b cargo run --bin ggen-ai-mcp
```

## 📋 **Template System Status**

### **✅ Current Template Features (No AI Dependencies)**
- YAML frontmatter with comprehensive field support
- RDF/SPARQL integration with template variable substitution
- Deterministic output generation
- Multi-format support (Tera templates)
- Injection and shell hook capabilities
- Comprehensive error handling

### **✅ AI Features (CLI Commands)**
- AI-powered template generation from natural language
- Template validation with quality scoring
- SPARQL query generation from intent
- RDF graph generation from domain descriptions
- Multi-provider LLM support (Ollama, OpenAI, Anthropic)
- MCP server integration for AI assistants

## 🎯 **Best Practices**

### **For Template Authors**
1. **Keep templates deterministic** - No AI dependencies in frontmatter
2. **Use standard frontmatter fields** - `to`, `vars`, `rdf`, `sparql`, etc.
3. **Test without AI** - Ensure templates work with standard generation

### **For Users**
1. **Use AI for generation** - `ggen ai generate` for new templates
2. **Use AI for validation** - `ggen ai validate` for quality checks
3. **Use AI for enhancement** - `ggen project gen --ai` for AI-assisted projects

### **For Developers**
1. **Keep AI separate** - No AI code in `ggen-core` template system
2. **CLI integration** - All AI features accessible via commands
3. **Clean interfaces** - AI commands work with standard templates

## 🔧 **Implementation Details**

### **Template System (`ggen-core`)**
- **File**: `ggen-core/src/template.rs`
- **Status**: ✅ Complete, no AI dependencies
- **Focus**: Deterministic template processing

### **AI Integration (`ggen-ai`)**
- **File**: `ggen-ai/src/` (complete module)
- **Status**: ✅ Complete, CLI-based
- **Focus**: AI-powered generation and validation

### **CLI Commands (`cli/src/cmds/ai/`)**
- **File**: `cli/src/cmds/ai/mod.rs` and subcommands
- **Status**: ✅ Complete, comprehensive
- **Focus**: User-facing AI functionality

## 📊 **Summary**

| Aspect | Template Frontmatter | CLI Commands |
|--------|---------------------|--------------|
| **AI Features** | ❌ Not implemented | ✅ Fully implemented |
| **Determinism** | ✅ Guaranteed | ✅ Optional enhancement |
| **User Control** | ❌ Always active | ✅ Explicit choice |
| **Maintainability** | ❌ Mixed concerns | ✅ Clean separation |
| **Flexibility** | ❌ Limited | ✅ Full control |

**Conclusion**: The CLI-based approach is the correct implementation. Templates remain clean and deterministic, while AI features are available when needed through dedicated commands.
