# ggen Language Server Protocol (LSP) — Architecture & Product Requirements

**Document Type**: ARD (Architecture Requirements Document) + PRD (Product Requirements Document)  
**Status**: DRAFT / READY FOR IMPLEMENTATION  
**Date**: 2026-05-28  
**Author**: Claude Code (Workflow Discovery Phase)  
**Target Audience**: Implementation agents

---

## Executive Summary

ggen v26.5.21 lacks IDE support for the three primary file types it operates on:
- **RDF Turtle** (`.ttl` — specification ontologies)
- **Tera** (`.tera` — code generation templates)
- **TOML** (`ggen.toml` — project configuration)

This document specifies a **ggen-lsp crate** that implements LSP 3.17 (November 2022 spec) to provide:
- **Syntax validation** — catch RDF/Tera/TOML errors before `ggen sync`
- **Completion** — suggest RDF predicates, Tera filters, ggen config keys
- **Hover** — show SHACL shapes, template variable types, config documentation
- **Diagnostics** — warn on undefined Tera variables, SPARQL syntax errors, orphan RDF triples
- **Definition jumping** — navigate from template vars to ontology definitions
- **References** — find all templates that use a given ontology class

**Success Criteria**: LSP server runs standalone, integrates with VSCode plugin, passes Chicago TDD test suite (80%+ coverage).

---

## 1. Architecture & Crate Design

### 1.1 Crate Structure

```
crates/ggen-lsp/
├── Cargo.toml
├── src/
│   ├── lib.rs                    # LSP server entrypoint
│   ├── server.rs                 # LanguageServer trait implementation
│   ├── handlers/
│   │   ├── mod.rs
│   │   ├── completion.rs         # textDocument/completion
│   │   ├── hover.rs              # textDocument/hover
│   │   ├── definition.rs         # textDocument/definition
│   │   ├── references.rs         # textDocument/references
│   │   └── diagnostics.rs        # textDocument/publishDiagnostics
│   ├── analyzers/
│   │   ├── mod.rs
│   │   ├── rdf_analyzer.rs       # .ttl validation + introspection
│   │   ├── tera_analyzer.rs      # .tera parsing + variable tracking
│   │   └── toml_analyzer.rs      # ggen.toml schema validation
│   ├── state.rs                  # ServerState (document cache, settings)
│   ├── protocol.rs               # LSP message handling
│   ├── utils.rs                  # URI conversion, position mapping
│   └── error.rs                  # LspError enum
└── tests/
    ├── integration_test.rs        # E2E LSP protocol
    └── analyzer_test.rs           # Per-analyzer Chicago TDD
```

### 1.2 Dependencies

**Required (new)**:
```toml
tower-lsp = "0.20"              # LSP server framework
tokio = { version = "1", features = ["full"] }
serde_json = "1"
```

**Reuse from ggen-core**:
```toml
ggen-core = { path = "../ggen-core" }  # For RDF store, Tera parsing
ggen-config = { path = "../ggen-config" }  # For ggen.toml schema
oxigraph = "0.3"                # RDF query engine (already in ggen-core)
tera = "1"                      # Template engine
toml = "0.8"                    # TOML parsing
```

### 1.3 Entry Points

**Binary**: `ggen lsp` (via clap integration in ggen-cli)
```bash
ggen lsp --transport stdio          # For editor integration
ggen lsp --transport http --port 9999  # For debugging
```

**Library**: Direct LSP server construction for embedded use
```rust
use ggen_lsp::{GgenLanguageServer, ServerState};
let server = GgenLanguageServer::new(ServerState::default());
// tower-lsp runs server
```

---

## 2. LSP Feature Matrix by File Type

### 2.1 RDF Turtle Files (`.ttl`)

| Feature | Status | Implementation | Example |
|---------|--------|----------------|---------|
| **Syntax Validation** | 🟢 CORE | Parse with `oxigraph` + report RDF parse errors | Invalid TTL → `error: Invalid RDF syntax` |
| **Completion** | 🟢 CORE | Suggest `rdf:`, `rdfs:`, `owl:` predicates; suggest class URIs from ontology | Type `ex:Person` → suggest `:hasAge`, `:hasName`, `:parents` |
| **Hover** | 🟢 CORE | Show SHACL shape (constraints) for a class; show rdfs:comment | Hover `ex:Person` → show "An individual human subject with identity and properties" |
| **Diagnostics** | 🟡 EXTENDED | Warn on orphan triples (unused classes), OWL inconsistencies, undefined references | Warn: "Class :UnknownParent never used in sh:targetClass" |
| **Definition** | 🟢 CORE | Jump to class definition (find first sh:NodeShape or owl:Class) | Click `ex:Person` → jump to `[ex:Person a sh:NodeShape]` |
| **References** | 🟡 EXTENDED | Find all triples that reference a given class/predicate | Right-click `:Person` → show 12 other triples that reference `:Person` |
| **Semantic Tokens** | 🟢 CORE | Syntax highlighting: namespace prefixes (blue), classes (cyan), properties (green), literals (orange) | `ex:Person` (cyan), `sh:targetClass` (green), `"string"` (orange) |
| **Document Symbols** | 🟢 CORE | Outline view: list all sh:NodeShape, owl:Class, rdf:Property definitions | Tree: Classes, Properties, Constraints |
| **Workspace Symbols** | 🟡 EXTENDED | Find any RDF class/property across all .ttl files in project | Cmd-T: search "hasAge" → find in 3 files |
| **Code Lens** | 🟡 EXTENDED | "Show SHACL shape" lens on class definitions; "Run SPARQL" lens on queries | `ex:Person` → "Show shape (3 properties)" |
| **Type Hierarchy** | 🟡 EXTENDED | Show SHACL targetClass hierarchy (superclass/subclass tree) | Click `:Person` → show parent classes, used by classes |
| **Rename** | 🟡 EXTENDED | Refactor class/property names across all .ttl files (with validation) | Rename `ex:Person` → `ex:Individual` (updates 12 triples) |
| **Folding Ranges** | 🟢 CORE | Collapse/expand SHACL shapes, nested property definitions | Fold `sh:NodeShape [...]` blocks |
| **Inlay Hints** | 🟡 EXTENDED | Show inferred types for class definitions (rdfs:range hints) | `ex:hasAge` → hint: `xsd:integer` |

**Analyzer Module**: `rdf_analyzer.rs`

```rust
pub struct RdfAnalyzer {
    store: oxigraph::MemoryStore,  // Load .ttl into memory
}

impl RdfAnalyzer {
    pub fn validate(&self, content: &str) -> Vec<Diagnostic> { /* ... */ }
    pub fn completion_at(&self, line: usize, col: usize) -> Vec<CompletionItem> { /* ... */ }
    pub fn hover_at(&self, line: usize, col: usize) -> Option<Hover> { /* ... */ }
    pub fn definition_at(&self, line: usize, col: usize) -> Option<Location> { /* ... */ }
}
```

**Example: Completing RDF predicates**
```
Query: User types "ex:Person a sh:NodeShape ."
       Then "    sh:" at column 10
Response: CompletionItem { label: "sh:property", kind: Property, ... }
```

### 2.2 Tera Template Files (`.tera`)

| Feature | Status | Implementation | Example |
|---------|--------|----------------|---------|
| **Syntax Validation** | 🟢 CORE | Parse with `tera::Template` + report syntax errors | Missing `{% endif %}` → `error: Unclosed block` |
| **Completion** | 🟡 EXTENDED | Suggest Tera filters (e.g., `upper`, `trim`, `length`); suggest loop variable names; suggest template includes | Type `{% for item` → suggest `{% for item in items %}` |
| **Hover** | 🟡 EXTENDED | Show filter documentation (e.g., hover `upper` → "Convert string to uppercase"); show variable type from SPARQL | Hover `\|upper` → show filter docs; hover `item` → show inferred type |
| **Diagnostics** | 🟢 CORE | Warn on undefined variables (not in SPARQL binding list); warn on circular template includes | Warn: "Variable `unknown_var` not found in available bindings"; "Circular include: A → B → A" |
| **Definition** | 🟡 EXTENDED | Jump to filter definition (if custom) or external docs; jump to included template | Jump to `render_json` definition; click `{% include "header.tera" %}` → open `header.tera` |
| **References** | 🟡 EXTENDED | Find all templates that use a given filter or variable | Right-click `\|render_json` → show 5 template files using it |
| **Semantic Tokens** | 🟢 CORE | Syntax highlighting: template keywords (purple), variables (blue), filters (green), strings (orange) | `{% for %}` (purple), `{{ item }}` (blue), `\|upper` (green) |
| **Document Symbols** | 🟢 CORE | Outline: list all `{% set %}`, `{% for %}`, `{% if %}`, `{% include %}` blocks | Tree: Variables, Loops, Conditionals, Includes |
| **Rename** | 🟡 EXTENDED | Refactor variable names within template scope (respects block boundaries) | Rename loop var `item` → `doc` (updates 8 references in loop) |
| **Code Lens** | 🟡 EXTENDED | "Show variable type" lens on loop variables; "Show bindings" lens on SPARQL source | `{% for item in items %}` → "Type: ?" (from SPARQL query) |
| **Folding Ranges** | 🟢 CORE | Collapse/expand `{% for %}`, `{% if %}`, `{% block %}` structures | Fold loop blocks, conditional blocks |
| **Inlay Hints** | 🟡 EXTENDED | Show inferred variable types from SPARQL SELECT bindings | `{% for item in items %}` → hint: `item: unknown` (query bindings needed) |
| **Format Document** | 🟡 EXTENDED | Auto-format Tera templates (consistent indentation, spacing) | Format on save: indent blocks, align filters |

**Analyzer Module**: `tera_analyzer.rs`

```rust
pub struct TeraAnalyzer {
    template: tera::Template,
    available_vars: HashSet<String>,  // From SPARQL bindings
}

impl TeraAnalyzer {
    pub fn validate(&self, content: &str) -> Vec<Diagnostic> { /* ... */ }
    pub fn completion_at(&self, line: usize, col: usize) -> Vec<CompletionItem> { /* ... */ }
    pub fn hover_at(&self, line: usize, col: usize) -> Option<Hover> { /* ... */ }
}
```

**Example: Warning on undefined variable**
```
Template: {% set output = data | render_json %}
          {% for item in unknown_var %}
Diagnostic: "undefined variable `unknown_var` — available vars: data, output"
```

### 2.3 TOML Configuration (`ggen.toml`)

| Feature | Status | Implementation | Example |
|---------|--------|----------------|---------|
| **Schema Validation** | 🟢 CORE | Validate against ggen.toml schema (reuse `ggen-config::GgenConfig`) | Invalid `llm_provider` value → `error: Expected one of: groq, openai, ...` |
| **Completion** | 🟢 CORE | Suggest valid config keys; suggest valid enum values; suggest nested table paths | Type `llm_provider = "` → suggest `"groq"`, `"openai"`; type `[` → suggest `[ai]`, `[generation]` |
| **Hover** | 🟢 CORE | Show documentation for each config key (from schema); show type information; show default values | Hover `llm_provider` → show description, type (string), default value |
| **Diagnostics** | 🟢 CORE | Warn on unknown keys; warn on type mismatches; warn on deprecated keys | Unknown key `llm_timeput` → suggest "Did you mean `llm_timeout`?"; warn on deprecated `output_path` (use `output_dir`) |
| **Definition** | 🟡 EXTENDED | Jump to config key definition in schema | Click `llm_provider` → show definition in GgenConfig schema docs |
| **References** | 🟡 EXTENDED | Show where a config key is documented/referenced | Click `llm_provider` → show 3 docs that mention it |
| **Semantic Tokens** | 🟢 CORE | Syntax highlighting: keys (blue), strings (orange), numbers (green), booleans (cyan) | `llm_provider = "groq"` (key blue, value orange) |
| **Document Symbols** | 🟢 CORE | Outline: list all top-level sections and keys | Tree: [ai], [generation], [build], etc. |
| **Code Lens** | 🟡 EXTENDED | "Show schema" lens on top-level sections; "Show validation" lens | `[ai]` → "Show schema (4 required, 2 optional)" |
| **Format Document** | 🟢 CORE | Auto-format TOML (consistent indentation, spacing, key ordering) | Format on save: normalize spacing, sort keys |
| **Folding Ranges** | 🟢 CORE | Collapse/expand TOML sections (e.g., `[ai]...`, `[generation]...`) | Fold section blocks |
| **Inlay Hints** | 🟡 EXTENDED | Show config type and default value inline | `llm_provider` → hint: `(string, default: "openai")` |
| **Spell Check Integration** | 🟡 EXTENDED | Custom spell checking for ggen-specific values (ignore "groq", "Tera", etc.) | Don't flag LSP as typo in comments |

**Analyzer Module**: `toml_analyzer.rs`

```rust
pub struct TomlAnalyzer {
    schema: ggen_config::ConfigSchema,  // From ggen-config
}

impl TomlAnalyzer {
    pub fn validate(&self, content: &str) -> Vec<Diagnostic> { /* ... */ }
    pub fn completion_at(&self, line: usize, col: usize) -> Vec<CompletionItem> { /* ... */ }
    pub fn hover_at(&self, line: usize, col: usize) -> Option<Hover> { /* ... */ }
}
```

**Example: Suggesting valid enum values**
```
Config: [ai]
        llm_provider = "
Completion Items:
  - "groq" (description: "Groq AI inference API")
  - "openai" (description: "OpenAI GPT models")
  - "anthropic" (description: "Anthropic Claude models")
```

---

## 3. Protocol Implementation Details

### 3.1 LanguageServer Trait Implementation

**File**: `server.rs`

```rust
use tower_lsp::{LanguageServer, LspService, Server};
use lsp_types::*;

pub struct GgenLanguageServer {
    state: Arc<Mutex<ServerState>>,
}

#[tower_lsp::async_trait]
impl LanguageServer for GgenLanguageServer {
    async fn initialize(&self, params: InitializeParams) -> Result<InitializeResult> {
        // Declare full capability set: completion, hover, definition, references,
        // semanticTokens, documentSymbol, workspaceSymbol, codeLen, rename, foldingRange,
        // inlayHint, formatting, diagnostics
        Ok(InitializeResult {
            capabilities: ServerCapabilities {
                text_document_sync: Some(TextDocumentSyncCapability::Kind(TextDocumentSyncKind::FULL)),
                completion_provider: Some(CompletionOptions {
                    trigger_characters: Some(vec![
                        ":".to_string(), "@".to_string(), ".".to_string(), "{".to_string(),
                        "[".to_string(), "\"".to_string(), "|".to_string(), "{".to_string(),
                    ]),
                    ..Default::default()
                }),
                hover_provider: Some(HoverServerCapability::Simple(true)),
                definition_provider: Some(OneOf::Left(true)),
                references_provider: Some(OneOf::Left(true)),
                rename_provider: Some(OneOf::Right(RenameOptions {
                    prepare_provider: Some(true),
                    ..Default::default()
                })),
                document_symbol_provider: Some(OneOf::Left(true)),
                workspace_symbol_provider: Some(OneOf::Left(true)),
                code_lens_provider: Some(CodeLensOptions {
                    resolve_provider: Some(true),
                }),
                semantic_tokens_provider: Some(
                    SemanticTokensServerCapabilities::SemanticTokensOptions(
                        SemanticTokensOptions {
                            legend: SemanticTokensLegend {
                                token_types: vec![
                                    "namespace".to_string(), "class".to_string(), "property".to_string(),
                                    "variable".to_string(), "keyword".to_string(), "string".to_string(),
                                    "number".to_string(), "comment".to_string(), "function".to_string(),
                                ],
                                token_modifiers: vec![
                                    "declaration".to_string(), "definition".to_string(),
                                    "readonly".to_string(), "deprecated".to_string(),
                                ],
                            },
                            range: Some(OneOf::Left(true)),
                            full: Some(OneOf::Left(true)),
                            ..Default::default()
                        },
                    ),
                ),
                folding_range_provider: Some(FoldingRangeServerCapabilities::Simple(true)),
                document_formatting_provider: Some(OneOf::Left(true)),
                document_range_formatting_provider: Some(OneOf::Left(true)),
                inlay_hint_provider: Some(OneOf::Left(true)),
                call_hierarchy_provider: Some(CallHierarchyServerCapabilities::Simple(true)),
                type_hierarchy_provider: Some(TypeHierarchyServerCapabilities::Simple(true)),
                ..Default::default()
            },
            ..Default::default()
        })
    }

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        // Load document, run initial diagnostics, compute semantic tokens
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        // Incremental update, re-run diagnostics, update semantic tokens
    }

    async fn completion(&self, params: CompletionParams) -> Result<Option<CompletionResponse>> {
        // Dispatch to appropriate analyzer
    }

    async fn hover(&self, params: HoverParams) -> Result<Option<Hover>> {
        // Dispatch to appropriate analyzer
    }

    async fn goto_definition(&self, params: GotoDefinitionParams) -> Result<Option<GotoDefinitionResponse>> {
        // Dispatch to appropriate analyzer
    }

    async fn references(&self, params: ReferenceParams) -> Result<Option<Vec<Location>>> {
        // Dispatch to appropriate analyzer
    }

    async fn document_symbol(&self, params: DocumentSymbolParams) -> Result<Option<DocumentSymbolResponse>> {
        // Return outline of document (classes, properties, sections, variables)
    }

    async fn workspace_symbol(&self, params: WorkspaceSymbolParams) -> Result<Option<Vec<SymbolInformation>>> {
        // Search all files for matching symbols
    }

    async fn code_lens(&self, params: CodeLensParams) -> Result<Option<Vec<CodeLens>>> {
        // Return code lens (e.g., "Show SHACL shape", "Show bindings")
    }

    async fn code_lens_resolve(&self, code_lens: CodeLens) -> Result<CodeLens> {
        // Resolve code lens (compute actual command/data)
    }

    async fn semantic_tokens_full(&self, params: SemanticTokensParams) -> Result<Option<SemanticTokensResult>> {
        // Return full semantic tokens for syntax highlighting
    }

    async fn semantic_tokens_range(
        &self,
        params: SemanticTokensRangeParams,
    ) -> Result<Option<SemanticTokensRangeResult>> {
        // Return semantic tokens for range
    }

    async fn folding_range(&self, params: FoldingRangeParams) -> Result<Option<Vec<FoldingRange>>> {
        // Return foldable ranges (blocks, sections, shapes)
    }

    async fn formatting(&self, params: DocumentFormattingParams) -> Result<Option<Vec<TextEdit>>> {
        // Format entire document
    }

    async fn range_formatting(
        &self,
        params: DocumentRangeFormattingParams,
    ) -> Result<Option<Vec<TextEdit>>> {
        // Format range
    }

    async fn prepare_rename(&self, params: TextDocumentPositionParams) -> Result<Option<PrepareRenameResponse>> {
        // Check if rename is valid at position
    }

    async fn rename(&self, params: RenameParams) -> Result<Option<WorkspaceEdit>> {
        // Rename symbol across all files
    }

    async fn inlay_hint(&self, params: InlayHintParams) -> Result<Option<Vec<InlayHint>>> {
        // Return inlay hints (type hints, default values)
    }

    async fn prepare_call_hierarchy(
        &self,
        params: CallHierarchyPrepareParams,
    ) -> Result<Option<Vec<CallHierarchyItem>>> {
        // Prepare call hierarchy (template includes, macro calls)
    }

    async fn incoming_calls(&self, params: CallHierarchyIncomingCallsParams) -> Result<Option<Vec<CallHierarchyIncomingCall>>> {
        // Show templates that include this template
    }

    async fn outgoing_calls(&self, params: CallHierarchyOutgoingCallsParams) -> Result<Option<Vec<CallHierarchyOutgoingCall>>> {
        // Show templates included by this template
    }

    async fn prepare_type_hierarchy(
        &self,
        params: TypeHierarchyPrepareParams,
    ) -> Result<Option<Vec<TypeHierarchyItem>>> {
        // Prepare type hierarchy (class hierarchy)
    }

    async fn supertypes(&self, params: TypeHierarchySupertypesParams) -> Result<Option<Vec<TypeHierarchyItem>>> {
        // Show parent classes (SHACL targetClass hierarchy)
    }

    async fn subtypes(&self, params: TypeHierarchySubtypesParams) -> Result<Option<Vec<TypeHierarchyItem>>> {
        // Show child classes
    }
}
```

### 3.2 Handler Module Structure

**File**: `handlers/mod.rs`

```rust
pub mod completion;
pub mod hover;
pub mod definition;
pub mod references;
pub mod diagnostics;
pub mod semantic_tokens;
pub mod document_symbol;
pub mod workspace_symbol;
pub mod code_lens;
pub mod folding_range;
pub mod formatting;
pub mod rename;
pub mod inlay_hint;
pub mod call_hierarchy;
pub mod type_hierarchy;

use crate::state::ServerState;

/// Dispatch to appropriate analyzer based on file URI
pub async fn handle_completion(
    state: &ServerState,
    uri: &Url,
    position: Position,
) -> Option<CompletionResponse> {
    let doc = state.documents.get(uri)?;
    let analyzer = state.get_analyzer(uri)?;  // Returns RdfAnalyzer | TeraAnalyzer | TomlAnalyzer
    
    analyzer.completion_at(position.line, position.character)
}

/// Compute semantic tokens (syntax highlighting)
pub fn handle_semantic_tokens(
    state: &ServerState,
    uri: &Url,
) -> Option<Vec<SemanticToken>> {
    let analyzer = state.get_analyzer(uri)?;
    analyzer.semantic_tokens()
}

/// Return document outline (symbols)
pub fn handle_document_symbol(
    state: &ServerState,
    uri: &Url,
) -> Option<Vec<DocumentSymbol>> {
    let analyzer = state.get_analyzer(uri)?;
    analyzer.document_symbols()
}

/// Return code lenses (inline commands)
pub fn handle_code_lens(
    state: &ServerState,
    uri: &Url,
) -> Option<Vec<CodeLens>> {
    let analyzer = state.get_analyzer(uri)?;
    analyzer.code_lenses()
}

/// Return foldable ranges (blocks, sections)
pub fn handle_folding_range(
    state: &ServerState,
    uri: &Url,
) -> Option<Vec<FoldingRange>> {
    let analyzer = state.get_analyzer(uri)?;
    analyzer.folding_ranges()
}

/// Format document
pub fn handle_formatting(
    state: &ServerState,
    uri: &Url,
) -> Option<Vec<TextEdit>> {
    let analyzer = state.get_analyzer(uri)?;
    analyzer.format_document()
}

/// Rename symbol across files
pub fn handle_rename(
    state: &ServerState,
    uri: &Url,
    position: Position,
    new_name: String,
) -> Option<WorkspaceEdit> {
    let analyzer = state.get_analyzer(uri)?;
    analyzer.rename_symbol(position, &new_name)
}

/// Return inlay hints (type hints, defaults)
pub fn handle_inlay_hints(
    state: &ServerState,
    uri: &Url,
) -> Option<Vec<InlayHint>> {
    let analyzer = state.get_analyzer(uri)?;
    analyzer.inlay_hints()
}

/// Return call hierarchy (template includes)
pub fn handle_call_hierarchy(
    state: &ServerState,
    uri: &Url,
    position: Position,
) -> Option<Vec<CallHierarchyItem>> {
    let analyzer = state.get_analyzer(uri)?;
    analyzer.call_hierarchy_items(position)
}

/// Return type hierarchy (class hierarchy)
pub fn handle_type_hierarchy(
    state: &ServerState,
    uri: &Url,
    position: Position,
) -> Option<Vec<TypeHierarchyItem>> {
    let analyzer = state.get_analyzer(uri)?;
    analyzer.type_hierarchy_items(position)
}
```

### 3.3 State Management

**File**: `state.rs`

```rust
pub struct ServerState {
    /// Cached documents (URI → content)
    pub documents: HashMap<Url, String>,
    
    /// Parsed analyzers (URI → RdfAnalyzer | TeraAnalyzer | TomlAnalyzer)
    pub analyzers: HashMap<Url, DocumentAnalyzer>,
    
    /// Configuration (from ggen.toml or LSP client settings)
    pub config: ServerConfig,
}

pub enum DocumentAnalyzer {
    Rdf(RdfAnalyzer),
    Tera(TeraAnalyzer),
    Toml(TomlAnalyzer),
}

impl ServerState {
    pub fn get_analyzer(&self, uri: &Url) -> Option<&DocumentAnalyzer> {
        self.analyzers.get(uri)
    }
    
    pub fn file_type_from_uri(uri: &Url) -> FileType {
        match uri.path().ends_with(".ttl") {
            true => FileType::Rdf,
            false if uri.path().ends_with(".tera") => FileType::Tera,
            false if uri.path().ends_with("ggen.toml") => FileType::Toml,
            _ => FileType::Unknown,
        }
    }
}
```

---

## 4. Integration Points with ggen-core

### 4.1 RDF Store Access

**Reuse**: `ggen-core::graph::RdfStore` (existing oxigraph wrapper)

```rust
// In rdf_analyzer.rs
use ggen_core::graph::RdfStore;
use lsp_types::{SemanticToken, SemanticTokenType, SemanticTokenModifier};

pub struct RdfAnalyzer {
    store: RdfStore,  // Reuse existing ggen-core abstraction
    semantic_token_cache: Vec<SemanticToken>,  // Cached syntax highlighting
}

impl RdfAnalyzer {
    pub fn new_from_content(content: &str) -> Result<Self> {
        let store = RdfStore::load_turtle(content)?;
        let semantic_token_cache = Self::compute_semantic_tokens(content);
        Ok(Self { store, semantic_token_cache })
    }
    
    pub fn query_class_definition(&self, class_uri: &str) -> Option<Vec<Triple>> {
        self.store.query_sparql(&format!(
            "SELECT * WHERE {{ <{}> ?p ?o }}",
            class_uri
        ))
    }

    // Advanced features
    
    pub fn semantic_tokens(&self) -> Vec<SemanticToken> {
        self.semantic_token_cache.clone()
    }
    
    fn compute_semantic_tokens(content: &str) -> Vec<SemanticToken> {
        // Parse TTL and emit semantic tokens:
        // - namespace URIs (blue, "namespace" token type)
        // - class definitions (cyan, "class")
        // - properties (green, "property")
        // - literals (orange, "string")
        // Mark definitions with "definition" modifier
        // Mark deprecated classes with "deprecated" modifier
        vec![]  // Computed from content
    }
    
    pub fn document_symbols(&self) -> Vec<DocumentSymbol> {
        // Extract all sh:NodeShape, owl:Class, rdf:Property definitions
        // Return hierarchy: Classes → Properties → Constraints
        vec![]
    }
    
    pub fn code_lenses(&self) -> Vec<CodeLens> {
        // Emit code lenses:
        // "Show SHACL shape (N properties)" on class definitions
        // "Run SPARQL query" on SPARQL CONSTRUCT blocks
        // "Show parent classes" on class hierarchy
        vec![]
    }
    
    pub fn folding_ranges(&self) -> Vec<FoldingRange> {
        // Fold sh:NodeShape [...] blocks
        // Fold nested sh:property [...] definitions
        vec![]
    }
    
    pub fn inlay_hints(&self) -> Vec<InlayHint> {
        // Show rdfs:range type hints on properties
        // Show cardinality hints (sh:minCount, sh:maxCount)
        vec![]
    }
    
    pub fn type_hierarchy_items(&self, position: Position) -> Option<Vec<TypeHierarchyItem>> {
        // For a class at position, return hierarchy:
        // - Parents: rdfs:subClassOf superclasses
        // - Children: classes with this as parent
        Some(vec![])
    }
    
    pub fn rename_symbol(&self, position: Position, new_name: &str) -> Option<WorkspaceEdit> {
        // Rename class/property at position across all .ttl files
        // Update sh:targetClass, sh:path, all references
        Some(WorkspaceEdit::default())
    }
}
```

### 4.2 Tera Template Access

**Reuse**: `ggen-core::template::TemplateResolver`

```rust
// In tera_analyzer.rs
use ggen_core::template::TemplateResolver;
use lsp_types::{SemanticToken, SemanticTokenType, FoldingRange, InlayHint};
use std::collections::HashMap;

pub struct TeraAnalyzer {
    template: tera::Template,
    available_vars: HashSet<String>,  // Extracted from SPARQL CONSTRUCT query
    source: String,                    // Original source for position mapping
    includes: Vec<(String, Range)>,   // Included templates and their positions
    semantic_token_cache: Vec<SemanticToken>,
}

impl TeraAnalyzer {
    pub fn new_from_content(content: &str, sparql_bindings: &str) -> Result<Self> {
        let template = tera::Template::new("template", content)
            .map_err(|e| LspError::TeraParseError(e))?;
        
        // Extract variable names from SPARQL bindings
        let available_vars = Self::extract_sparql_vars(sparql_bindings);
        let includes = Self::extract_includes(content);
        let semantic_token_cache = Self::compute_semantic_tokens(content, &available_vars);
        
        Ok(Self {
            template,
            available_vars,
            source: content.to_string(),
            includes,
            semantic_token_cache,
        })
    }
    
    fn extract_sparql_vars(sparql: &str) -> HashSet<String> {
        // Parse "SELECT ?foo ?bar WHERE ..." → {"foo", "bar"}
        HashSet::new()
    }
    
    fn extract_includes(content: &str) -> Vec<(String, Range)> {
        // Find all {% include "..." %} and return template name + position
        vec![]
    }
    
    fn compute_semantic_tokens(content: &str, available_vars: &HashSet<String>) -> Vec<SemanticToken> {
        // Emit semantic tokens for:
        // - Template keywords (purple): {% for %}, {% if %}, {% block %}
        // - Variables (blue): {{ foo }}, {% for foo in ... %}
        // - Filters (green): |upper, |trim, |length
        // - Strings (orange): "..." inside templates
        // Mark variable declarations with "declaration" modifier
        // Mark undefined variables with "deprecated" modifier
        vec![]
    }

    // Advanced features
    
    pub fn semantic_tokens(&self) -> Vec<SemanticToken> {
        self.semantic_token_cache.clone()
    }
    
    pub fn document_symbols(&self) -> Vec<DocumentSymbol> {
        // Extract:
        // - All {% set VAR %} blocks
        // - All {% for LOOP_VAR %} blocks
        // - All {% if %} conditionals
        // - All {% include %} statements
        // Return tree: Variables, Loops, Conditionals, Includes
        vec![]
    }
    
    pub fn code_lenses(&self) -> Vec<CodeLens> {
        // Emit code lenses:
        // "Show variable type" on {% for item in items %}
        // "Show bindings" on template (link to SPARQL query)
        // "Go to included template" on {% include %}
        vec![]
    }
    
    pub fn folding_ranges(&self) -> Vec<FoldingRange> {
        // Fold {% for %}, {% if %}, {% block %} structures
        vec![]
    }
    
    pub fn format_document(&self) -> Option<Vec<TextEdit>> {
        // Auto-format: consistent indentation, spacing, align filters
        Some(vec![])
    }
    
    pub fn inlay_hints(&self) -> Vec<InlayHint> {
        // Show inferred variable types on {% for item in items %}
        // Show filter parameter hints
        // Show default values for filters
        vec![]
    }
    
    pub fn rename_symbol(&self, position: Position, new_name: &str) -> Option<WorkspaceEdit> {
        // Rename variable/loop var at position
        // Updates all references within template scope
        // Respects block boundaries (loop vars don't escape loops)
        Some(WorkspaceEdit::default())
    }
    
    pub fn call_hierarchy_items(&self, position: Position) -> Vec<CallHierarchyItem> {
        // For an include statement, return:
        // - The included template file
        // - Other templates that include this one
        // - Build call tree
        vec![]
    }
    
    pub fn goto_include(&self, position: Position) -> Option<Location> {
        // Jump to included template file
        // Find {% include "header.tera" %} at position → open header.tera
        None
    }
}
```

### 4.3 Config Schema Access

**Reuse**: `ggen-config::GgenConfig`

```rust
// In toml_analyzer.rs
use ggen_config::GgenConfig;

pub struct TomlAnalyzer {
    schema: serde_json::Value,  // JSON schema from GgenConfig
}

impl TomlAnalyzer {
    pub fn new() -> Self {
        let schema = serde_json::json!({
            "type": "object",
            "properties": {
                "llm_provider": {
                    "type": "string",
                    "enum": ["groq", "openai", "anthropic"],
                    "description": "LLM provider endpoint"
                },
                // ... more properties
            }
        });
        
        Self { schema }
    }
}
```

---

## 5. Test Strategy (Chicago TDD)

### 5.1 Test File Structure

```
crates/ggen-lsp/tests/
├── integration_test.rs          # E2E LSP protocol
├── rdf_analyzer_test.rs         # RDF-specific tests
├── tera_analyzer_test.rs        # Tera-specific tests
├── toml_analyzer_test.rs        # TOML-specific tests
└── fixtures/
    ├── example.ttl              # Sample RDF files
    ├── example.tera             # Sample templates
    └── ggen.toml                # Sample config
```

### 5.2 Chicago TDD Requirements

- **Real file I/O**: Use `TempDir` to create `.ttl`, `.tera`, `ggen.toml` files
- **Real analyzer execution**: Instantiate actual analyzers with real content
- **Real LSP protocol**: Send actual LSP JSON-RPC messages via stdio transport (not mocked)
- **State assertion**: Assert on actual `ServerState` changes, not mock interactions

### 5.3 Test Examples

**File**: `rdf_analyzer_test.rs`

```rust
#[tokio::test]
async fn test_rdf_completion_suggests_predicates() {
    let temp_dir = TempDir::new().unwrap();
    let rdf_file = temp_dir.path().join("example.ttl");
    
    // Real RDF file with some ontology
    std::fs::write(&rdf_file, r#"
        @prefix ex: <http://example.org/> .
        @prefix sh: <http://www.w3.org/ns/shacl#> .
        
        ex:Person a sh:NodeShape ;
            sh:property [
                sh:path ex:hasName ;
                sh:datatype xsd:string ;
            ] .
    "#).unwrap();
    
    // Real analyzer
    let content = std::fs::read_to_string(&rdf_file).unwrap();
    let analyzer = RdfAnalyzer::new_from_content(&content).unwrap();
    
    // Completion at position where user types "sh:"
    let completions = analyzer.completion_at(5, 10);  // Line 5, col 10
    
    // Assert on real completion results
    assert!(completions.iter().any(|c| c.label == "sh:property"));
    assert!(completions.iter().any(|c| c.label == "sh:datatype"));
}

#[tokio::test]
async fn test_rdf_hover_shows_shacl_shape() {
    let content = r#"
        @prefix ex: <http://example.org/> .
        ex:Person a sh:NodeShape .
    "#;
    
    let analyzer = RdfAnalyzer::new_from_content(content).unwrap();
    let hover = analyzer.hover_at(1, 4);  // Hover on "Person"
    
    assert!(hover.is_some());
    assert!(hover.unwrap().contents.to_string().contains("sh:NodeShape"));
}

#[tokio::test]
async fn test_tera_diagnostic_warns_undefined_variable() {
    let content = r#"
        {% set output = data | render_json %}
        {% for item in unknown_var %}
            {{ item.name }}
        {% endfor %}
    "#;
    
    let analyzer = TeraAnalyzer::new_from_content(content, "SELECT ?data").unwrap();
    let diagnostics = analyzer.validate(content);
    
    assert!(diagnostics.iter().any(|d| {
        d.message.contains("unknown_var") && d.severity == DiagnosticSeverity::WARNING
    }));
}

#[tokio::test]
async fn test_toml_completion_suggests_enum_values() {
    let content = r#"
        [ai]
        llm_provider = "
    "#;
    
    let analyzer = TomlAnalyzer::new();
    let completions = analyzer.completion_at(2, 21);  // After opening quote
    
    assert!(completions.iter().any(|c| c.label == "groq"));
    assert!(completions.iter().any(|c| c.label == "openai"));
}
```

**File**: `integration_test.rs` (E2E LSP protocol)

```rust
#[tokio::test]
async fn test_lsp_initialize_and_completion() {
    // Real LSP server
    let server = GgenLanguageServer::new(ServerState::default());
    
    // Real LSP message: initialize
    let init_result = server.initialize(InitializeParams {
        process_id: Some(ProcessId::U32(1234)),
        root_uri: Some(Url::from_file_path("/tmp/ggen").unwrap()),
        ..Default::default()
    }).await.unwrap();
    
    assert!(init_result.capabilities.completion_provider.is_some());
    
    // Real LSP message: didOpen
    let temp_dir = TempDir::new().unwrap();
    let ggen_toml = temp_dir.path().join("ggen.toml");
    std::fs::write(&ggen_toml, "[ai]\nllm_provider = ").unwrap();
    
    server.did_open(DidOpenTextDocumentParams {
        text_document: TextDocumentItem {
            uri: Url::from_file_path(&ggen_toml).unwrap(),
            language_id: "toml".to_string(),
            version: 1,
            text: std::fs::read_to_string(&ggen_toml).unwrap(),
        },
    }).await;
    
    // Real LSP message: completion
    let completions = server.completion(CompletionParams {
        text_document_position: TextDocumentPositionParams {
            text_document: TextDocumentIdentifier {
                uri: Url::from_file_path(&ggen_toml).unwrap(),
            },
            position: Position {
                line: 1,
                character: 21,
            },
        },
        ..Default::default()
    }).await.unwrap();
    
    assert!(completions.is_some());
}
```

### 5.4 Coverage Target

- **Minimum**: 80% code coverage
- **Mutation score**: ≥60%
- **Integration tests**: E2E LSP protocol with real files

---

## 6. Implementation Roadmap

### Phase 1: Foundation (Week 1)

- [ ] Create `ggen-lsp` crate with `Cargo.toml`
- [ ] Wire up `tower-lsp` + `LanguageServer` trait skeleton
- [ ] Implement `ServerState` + document caching
- [ ] Add `ggen lsp` CLI command (ggen-cli integration)
- [ ] Test: LSP server initializes and accepts connections

### Phase 2: RDF Analyzer (Week 1)

- [ ] Implement `RdfAnalyzer::validate()` (parse with oxigraph)
- [ ] Implement `RdfAnalyzer::completion_at()` (suggest RDF predicates)
- [ ] Implement `RdfAnalyzer::hover_at()` (show SHACL shapes)
- [ ] Implement `RdfAnalyzer::definition_at()` (jump to class def)
- [ ] Tests: 15+ Chicago TDD tests for RDF analyzer

### Phase 3: Tera Analyzer (Week 2)

- [ ] Implement `TeraAnalyzer::validate()` (parse with tera crate)
- [ ] Implement `TeraAnalyzer::completion_at()` (suggest filters + vars)
- [ ] Implement `TeraAnalyzer::hover_at()` (show filter docs)
- [ ] Diagnostic: warn on undefined variables
- [ ] Tests: 12+ Chicago TDD tests for Tera analyzer

### Phase 4: TOML Analyzer (Week 1.5)

- [ ] Implement `TomlAnalyzer::validate()` (schema validation)
- [ ] Implement `TomlAnalyzer::completion_at()` (suggest keys + values)
- [ ] Implement `TomlAnalyzer::hover_at()` (show key descriptions)
- [ ] Tests: 10+ Chicago TDD tests for TOML analyzer

### Phase 5: Integration & Claude Code Marketplace (Week 2)

- [ ] E2E LSP integration tests (real stdio/HTTP transport)
- [ ] **Claude Code Marketplace Plugin** — `ggen-lsp.md` plugin definition
  - Schema: expose `ggen lsp start` as marketplace skill
  - Installation: `claude-lsp-ggen` registered via `settings.json`
  - Activation: auto-start when editing `.ttl`, `.tera`, `ggen.toml` files
  - Configuration: allow user to set LSP transport (stdio/HTTP), custom port
- [ ] VSCode integration (detect and use ggen-lsp when available)
- [ ] Documentation + examples
- [ ] Performance benchmarking (startup < 200ms, completion < 100ms)

**Total Effort**: ~4-5 weeks, 1 dedicated agent (or 2 agents in parallel)

---

## 7. Error Handling

### 7.1 Error Enum

**File**: `error.rs`

```rust
#[derive(Debug)]
pub enum LspError {
    // RDF errors
    RdfParseError(String),
    RdfQueryError(String),
    
    // Tera errors
    TeraParseError(tera::Error),
    TeraRenderError(String),
    
    // TOML errors
    TomlParseError(toml::de::Error),
    TomlValidationError(String),
    
    // LSP protocol errors
    LspProtocolError(tower_lsp::LspError),
    
    // Generic
    IoError(std::io::Error),
    Unknown(String),
}

impl From<LspError> for tower_lsp::LspError {
    fn from(err: LspError) -> Self {
        tower_lsp::LspError {
            code: tower_lsp::ErrorCode::InternalError as i32,
            message: format!("{:?}", err),
            data: None,
        }
    }
}
```

### 7.2 Diagnostic Severity

- **Error** (red squiggle): Syntax errors, type mismatches, undefined variables
- **Warning** (yellow squiggle): Unused variables, orphan triples, potential issues
- **Information** (blue): Hints, suggestions, best practices
- **Hint**: Minor suggestions (e.g., "did you mean...?")

---

## 8. CLI Integration

### 8.1 ggen lsp Command

**File**: `crates/ggen-cli/src/cmds/lsp.rs`

```rust
use clap::Subcommand;

#[derive(Subcommand)]
pub enum LspCommand {
    /// Start LSP server
    #[command(about = "Start ggen Language Server Protocol (LSP)")]
    Start {
        /// Transport: stdio (for editor), http (for debugging)
        #[arg(long, default_value = "stdio")]
        transport: String,
        
        /// HTTP port (only if transport=http)
        #[arg(long, default_value = "9999")]
        port: u16,
    },
}

pub async fn run_lsp(cmd: LspCommand) -> Result<()> {
    match cmd {
        LspCommand::Start { transport, port } => {
            if transport == "stdio" {
                // Start LSP with stdio transport
                ggen_lsp::run_stdio().await?;
            } else if transport == "http" {
                // Start LSP with HTTP transport on given port
                ggen_lsp::run_http(port).await?;
            }
            Ok(())
        }
    }
}
```

**Usage**:
```bash
ggen lsp start --transport stdio                    # For VSCode
ggen lsp start --transport http --port 9999         # For debugging
```

---

## 9. Dependency Graph & Build Order

```
ggen-lsp
├── tower-lsp (0.20)
├── tokio (1.x)
├── ggen-core (existing)
│   ├── oxigraph (0.3)
│   └── tera (1.x)
├── ggen-config (existing)
│   └── toml (0.8)
└── serde_json (1.x)
```

**Build**: `cargo build -p ggen-lsp` (should take < 30s incremental)

---

## 10. Success Criteria (Definition of Done)

### 10.1 Code Quality

- [ ] `cargo make check` passes (no compiler errors/warnings)
- [ ] `cargo make lint` passes (clippy, rustfmt)
- [ ] `cargo make test -p ggen-lsp` — all tests pass
- [ ] **Coverage**: ≥80% (run `cargo tarpaulin -p ggen-lsp`)
- [ ] **No panics/unwraps** in production code (only in tests)

### 10.2 Functional Requirements

- [ ] LSP server initializes with correct capabilities
- [ ] Completion works for all 3 file types
- [ ] Hover works for all 3 file types
- [ ] Definition jumping works for RDF
- [ ] Diagnostics are reported correctly
- [ ] Server handles rapid document changes gracefully
- [ ] Server recovers from malformed input without crashing

### 10.3 Performance

- [ ] Server startup < 200ms
- [ ] Completion response < 100ms
- [ ] Diagnostics on large files (10k+ lines) < 500ms
- [ ] Memory usage < 100MB for typical projects

### 10.4 Documentation

- [ ] README.md with setup + usage examples
- [ ] Code comments for non-obvious logic
- [ ] Integration guide for VSCode plugin

### 10.5 Testing

- [ ] Unit tests: all analyzers (80+ test functions)
- [ ] Integration tests: E2E LSP protocol (10+ test scenarios)
- [ ] Real file I/O (TempDir, actual parsing)
- [ ] No mocks or test doubles

---

## 10.5 Claude Code Marketplace Plugin Integration

ggen-lsp integrates with Claude Code Marketplace as a **native language server plugin** (no LLM support — LSP is pure code intelligence for ggen file types).

### 10.5.1 Plugin Definition

**File**: `crates/ggen-lsp/marketplace/ggen-lsp.md` (Claude Code plugin manifest)

```markdown
# ggen Language Server Protocol (LSP)

Syntax highlighting, completion, diagnostics, and refactoring for ggen RDF/Tera/TOML files.

## Supported File Types

- `.ttl` — RDF Turtle specifications
- `.tera` — Tera code generation templates
- `ggen.toml` — ggen project configuration

## Features

- **Syntax validation** — Catch RDF parse errors, undefined Tera variables, TOML type mismatches
- **Completion** — Suggest RDF predicates, Tera filters, config keys
- **Hover** — Show documentation, SHACL shapes, type information
- **Definition jumping** — Navigate class definitions, template includes
- **Refactoring** — Rename symbols across files with validation
- **Semantic highlighting** — Color-code classes, properties, variables, literals
- **Document symbols** — Outline view for all definitions
- **Code lenses** — Inline commands ("Show SHACL shape", "Go to include")
- **Code folding** — Collapse RDF shapes, Tera blocks, TOML sections
- **Code formatting** — Auto-format with consistent indentation

## Installation

```bash
# Via Claude Code settings
echo '{"ggen-lsp": {"enabled": true, "transport": "stdio"}}' >> ~/.claude/settings.json
```

## Configuration

```json
{
  "ggen-lsp": {
    "enabled": true,
    "transport": "stdio",        // or "http" for debugging
    "port": 9999,                 // only if transport=http
    "auto_format_on_save": true,
    "show_hints": true,
    "workspace_symbol_depth": 5
  }
}
```

## Usage

### Editor Integration

- **Open `.ttl` file** → LSP server auto-starts in background
- **Type `sh:`** → Completion suggests SHACL predicates
- **Hover `ex:Person`** → Shows class documentation + shape
- **Right-click → Go to Definition** → Jump to class definition
- **Cmd+K Cmd+R** → Rename class across all files
- **Cmd+I** → Show document outline (symbols)

### Command Palette

- `ggen-lsp: Start Server` — Manually start LSP (if auto-start disabled)
- `ggen-lsp: Stop Server` — Manually stop LSP
- `ggen-lsp: Restart Server` — Restart if hung
- `ggen-lsp: Show Diagnostics` — View all validation errors
- `ggen-lsp: Format Document` — Auto-format current file

## Performance

- Startup: <200ms
- Completion: <100ms
- Diagnostics: <500ms for large files
- Memory: <100MB typical use

## Requirements

- ggen v26.5.21 or later
- Claude Code (any version with Marketplace support)
- 200MB free disk space

## Support

- Issues: [ggen GitHub](https://github.com/seanchatmangpt/ggen/issues)
- Docs: [ggen LSP Guide](./LSP-ARD-PRD.md)
```

### 10.5.2 Marketplace Registration

**File**: `crates/ggen-lsp/Cargo.toml` (plugin metadata)

```toml
[package]
name = "ggen-lsp"
version = "26.5.21"
edition = "2021"

[package.metadata.claude-code]
type = "language-server"                          # Marketplace plugin type
marketplace_plugin_id = "ggen-lsp"               # Unique plugin ID
marketplace_plugin_name = "ggen Language Server"
marketplace_display_name = "ggen (RDF/Tera/TOML)"
marketplace_description = "Syntax highlighting, completion, refactoring for ggen files"
marketplace_categories = ["language-support", "linting", "formatting"]
marketplace_icon = "icon.png"  # Store in marketplace/ directory
marketplace_readme = "marketplace/ggen-lsp.md"
marketplace_minimum_claude_code_version = "0.9.0"
marketplace_supported_languages = ["ttl", "tera", "toml"]
marketplace_binary = "ggen"  # Command to start: `ggen lsp start`
marketplace_binary_args = ["lsp", "start", "--transport", "stdio"]

# LSP server capabilities (for marketplace filtering)
marketplace_capabilities = [
  "completion",
  "hover",
  "definition",
  "references",
  "rename",
  "document-symbol",
  "workspace-symbol",
  "code-lens",
  "semantic-tokens",
  "folding-range",
  "formatting",
  "inlay-hint",
  "call-hierarchy",
  "type-hierarchy"
]
```

### 10.5.3 Claude Code Integration Hook

**File**: `.claude/hooks/ggen_lsp_marketplace_sync.sh` (auto-register on install)

```bash
#!/bin/bash
# Called when ggen-lsp is installed via marketplace

set -e

PLUGIN_DIR="${HOME}/.claude/plugins/ggen-lsp"
mkdir -p "${PLUGIN_DIR}"

# Copy plugin manifest
cp crates/ggen-lsp/marketplace/ggen-lsp.md "${PLUGIN_DIR}/"

# Register in settings.json
if ! grep -q '"ggen-lsp"' ~/.claude/settings.json 2>/dev/null; then
  cat >> ~/.claude/settings.json << 'EOF'
{
  "ggen-lsp": {
    "enabled": true,
    "transport": "stdio",
    "auto_format_on_save": true,
    "show_hints": true
  }
}
EOF
  echo "✓ ggen-lsp registered in Claude Code"
else
  echo "✓ ggen-lsp already registered"
fi

# Verify binary exists
if ! command -v ggen &> /dev/null; then
  echo "⚠ Warning: ggen binary not in PATH"
  echo "  Install with: cargo install --path crates/ggen-cli"
fi
```

### 10.5.4 Marketplace Distribution

**File Structure** (for marketplace upload):

```
ggen-lsp-marketplace-bundle/
├── ggen-lsp.md              # Plugin manifest (shown in marketplace)
├── icon.png                 # 128x128 plugin icon
├── INSTALL.md               # Installation instructions
├── LICENSE                  # Same as ggen repo
└── schema.json              # LSP capabilities + config schema
```

**Publication Steps** (manual, one-time):

1. Build ggen and extract `ggen` binary to bundle
2. Upload bundle to Claude Code Marketplace: `claude marketplace upload ggen-lsp-marketplace-bundle/`
3. Plugin available at: `https://marketplace.claude.ai/plugins/ggen-lsp`
4. Users install via Claude Code UI or `claude marketplace install ggen-lsp`



### 11.1 RDF File (.ttl) Completable Structure

```turtle
@prefix ex: <http://example.org/> .
@prefix sh: <http://www.w3.org/ns/shacl#> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

ex:Person a sh:NodeShape ;
    sh:targetClass ex:Person ;
    sh:property [
        sh:path ex:hasName ;        # Completion on "sh:"
        sh:datatype xsd:string ;    # Completion on "xsd:"
    ] ;
    sh:property [
        sh:path ex:hasAge ;
        sh:datatype xsd:integer ;
    ] .
```

**Completion points**:
- `@prefix` — suggest namespaces (rdf, rdfs, owl, sh, xsd, dcterms)
- `a ` — suggest class types (sh:NodeShape, owl:Class, rdf:Property)
- `;` in property list — suggest next properties (sh:minCount, sh:maxCount, sh:pattern)

### 11.2 Tera Template (.tera) Variable Tracking

```tera
{% set output_dir = config.output_dir %}
{% set files = [] %}

{% for file in generated_files %}
  {% set name = file.name | upper %}
  {% set path = output_dir ~ "/" ~ name %}
  
  Output: {{ path }}
  Type: {{ file.type }}
  
  {% if file.error %}
    Error: {{ file.error }}
  {% endif %}
{% endfor %}
```

**Completion context**:
- Inside `{{ ... }}` — suggest variables: `output_dir`, `files`, `name`, `path`, `file`, `config`
- After `|` — suggest filters: `upper`, `lower`, `trim`, `length`, `default`, `pluralize`

**Diagnostic context**:
- Line 6: `{{ file.error }}` — OK, `file` is from `{% for file ... %}`
- Line 9: If user typed `{{ unknown_var }}`, warn: "Variable not in scope; available: output_dir, files, ..."

### 11.3 TOML Config (ggen.toml) Enum Completion

```toml
[project]
name = "my-ggen-app"
version = "1.0.0"

[ai]
llm_provider = "groq"      # Completion suggests: groq, openai, anthropic
llm_model = "mixtral-8x7b" # Completion suggests models for selected provider

[generation]
output_dir = "generated"
validate_syntax = true     # Hover shows: "Validate generated code syntax (false to skip)"

[build]
parallel_jobs = 4          # Hover shows: "Number of parallel generation jobs (0 = auto)"
```

**Completion points**:
- `llm_provider = "` — suggest enum: "groq", "openai", "anthropic"
- `llm_model = "` — suggest models for selected provider
- `[` — suggest section names: `[ai]`, `[generation]`, `[build]`, `[templates]`

---

## 12. Next Steps for Implementation Agent

1. **Clone this document** to working memory
2. **Start with Phase 1** (foundation + RDF analyzer)
3. **Use Chicago TDD**: Write failing tests first, then implement
4. **Run `cargo make check`** frequently (should pass always)
5. **Report progress** as you complete each phase
6. **Flag blockers early** if ggen-core exports don't match expectations

---

---

## 12. Advanced tower-lsp Features Summary

This LSP implementation leverages **tower-lsp 0.20** advanced capabilities:

| Feature | tower-lsp Support | ggen Implementation | Value |
|---------|-------------------|-------------------|-------|
| **Semantic Tokens** | ✅ Full (SemanticTokensOptions) | Syntax highlighting with token types + modifiers | IDE syntax highlighting (TTL: namespaces, classes, properties; Tera: keywords, variables, filters; TOML: keys, values) |
| **Code Lenses** | ✅ Full (CodeLensOptions + resolve) | "Show SHACL shape", "Go to include", "Show type" lenses | Inline commands + metadata without modal dialogs |
| **Folding Ranges** | ✅ Full | Collapse RDF shapes, Tera blocks, TOML sections | Hide irrelevant code, focus on relevant sections |
| **Call Hierarchy** | ✅ Full (incomingCalls, outgoingCalls) | Template include graph, macro expansions | Trace template composition, detect circular includes |
| **Type Hierarchy** | ✅ Full (supertypes, subtypes) | SHACL class hierarchy (targetClass parents/children) | Navigate ontology class trees, understand inheritance |
| **Inlay Hints** | ✅ Full | Show types, defaults, cardinality hints | Reduce need for manual documentation, improve readability |
| **Rename** | ✅ Full (prepare + rename) | Refactor class/property/variable names across files | Safe multi-file refactoring with validation |
| **Format** | ✅ Full (document + range) | Auto-format RDF, Tera, TOML with consistent style | One-command code cleanup |
| **Document Symbols** | ✅ Full | Outline view of all definitions | Quick navigation within large files |
| **Workspace Symbols** | ✅ Full (with filtering) | Find any class/property/section in workspace | Project-wide search, cross-file navigation |
| **Diagnostics** | ✅ Full (with code actions) | Validation errors + repair suggestions | Catch errors early, suggest fixes |
| **Completion** | ✅ Full (with resolve) | Context-aware suggestions, filter docs on hover | Type-aware completion, reduce keystrokes |
| **Hover** | ✅ Full (with MarkupContent) | Rich documentation (Markdown), show types, shapes | In-editor reference docs |
| **Definition / References** | ✅ Full | Cross-file navigation, usage finding | Trace symbol usage across project |

**No LLM support**: Pure code intelligence (static analysis, parsing, text manipulation). No Claude AI integration — ggen-lsp is a self-contained language server.

---

**Document Version**: 1.1  
**Last Updated**: 2026-05-28 (Advanced Features + Marketplace Integration)  
**Status**: Ready for Implementation
