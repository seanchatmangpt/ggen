<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [MCP Integration Design for ggen](#mcp-integration-design-for-ggen)
  - [1. Architecture Overview](#1-architecture-overview)
    - [1.1 High-Level Design](#11-high-level-design)
    - [1.2 Design Principles](#12-design-principles)
    - [1.3 Implementation Strategy](#13-implementation-strategy)
  - [2. Tool Mapping: CLI Commands → MCP Tools](#2-tool-mapping-cli-commands-%E2%86%92-mcp-tools)
    - [2.1 Tool Naming Convention](#21-tool-naming-convention)
    - [2.2 Complete Tool Catalog (40 Tools)](#22-complete-tool-catalog-40-tools)
      - [Project Tools (9 tools)](#project-tools-9-tools)
      - [Market Tools (13 tools)](#market-tools-13-tools)
      - [Graph Tools (7 tools)](#graph-tools-7-tools)
      - [Template Tools (5 tools)](#template-tools-5-tools)
      - [Hook Tools (5 tools)](#hook-tools-5-tools)
      - [System Tools (1 tool)](#system-tools-1-tool)
  - [3. Transport Strategy](#3-transport-strategy)
    - [3.1 Transport Selection Matrix](#31-transport-selection-matrix)
    - [3.2 Transport Configuration](#32-transport-configuration)
    - [3.3 Streaming Support (Future)](#33-streaming-support-future)
  - [4. Error Handling](#4-error-handling)
    - [4.1 Error Mapping Strategy](#41-error-mapping-strategy)
    - [4.2 Error Response Schema](#42-error-response-schema)
    - [4.3 Error Recovery Patterns](#43-error-recovery-patterns)
  - [5. Security Model](#5-security-model)
    - [5.1 Security Layers](#51-security-layers)
    - [5.2 Path Validation](#52-path-validation)
    - [5.3 Configuration Security](#53-configuration-security)
  - [6. Tool Categories & Schemas](#6-tool-categories--schemas)
    - [6.1 Category Organization](#61-category-organization)
    - [6.2 Schema Design Patterns](#62-schema-design-patterns)
      - [Pattern A: Simple Input/Output](#pattern-a-simple-inputoutput)
      - [Pattern B: File-Based Operations](#pattern-b-file-based-operations)
      - [Pattern C: Query/Search Operations](#pattern-c-querysearch-operations)
      - [Pattern D: Graph/RDF Operations](#pattern-d-graphrdf-operations)
  - [7. Example Tool Schemas (Top 10 Priority Tools)](#7-example-tool-schemas-top-10-priority-tools)
    - [7.1 P0 Tools (Must-Have for MVP)](#71-p0-tools-must-have-for-mvp)
      - [1. `ggen_project_gen`](#1-ggen_project_gen)
      - [2. `ggen_project_plan`](#2-ggen_project_plan)
      - [3. `ggen_project_apply`](#3-ggen_project_apply)
      - [4. `ggen_market_search`](#4-ggen_market_search)
      - [5. `ggen_market_add`](#5-ggen_market_add)
      - [6. `ggen_market_list`](#6-ggen_market_list)
      - [7. `ggen_template_new`](#7-ggen_template_new)
      - [8. `ggen_template_list`](#8-ggen_template_list)
      - [9. `ggen_graph_query`](#9-ggen_graph_query)
      - [10. `ggen_system_version`](#10-ggen_system_version)
  - [8. Implementation Plan](#8-implementation-plan)
    - [8.1 Phase 1: MVP (Week 1-2)](#81-phase-1-mvp-week-1-2)
    - [8.2 Phase 2: Marketplace & Templates (Week 3-4)](#82-phase-2-marketplace--templates-week-3-4)
    - [8.3 Phase 3: Graph & Hooks (Week 5-6)](#83-phase-3-graph--hooks-week-5-6)
    - [8.4 Phase 4: Production Hardening (Week 7-8)](#84-phase-4-production-hardening-week-7-8)
  - [9. Technical Deep-Dive](#9-technical-deep-dive)
    - [9.1 Server Implementation (Rust)](#91-server-implementation-rust)
    - [9.2 Schema Management](#92-schema-management)
    - [9.3 Configuration System](#93-configuration-system)
  - [10. Testing Strategy](#10-testing-strategy)
    - [10.1 Test Pyramid](#101-test-pyramid)
    - [10.2 Test Coverage Goals](#102-test-coverage-goals)
    - [10.3 Example Test](#103-example-test)
  - [11. Deployment & Distribution](#11-deployment--distribution)
    - [11.1 Package Structure](#111-package-structure)
    - [11.2 Installation Methods](#112-installation-methods)
    - [11.3 CI/CD Pipeline](#113-cicd-pipeline)
  - [12. Performance Benchmarks](#12-performance-benchmarks)
    - [12.1 Target Latencies](#121-target-latencies)
    - [12.2 Optimization Strategies](#122-optimization-strategies)
  - [13. Future Enhancements](#13-future-enhancements)
    - [13.1 Advanced Features (Post-MVP)](#131-advanced-features-post-mvp)
    - [13.2 Integration Roadmap](#132-integration-roadmap)
  - [14. Success Metrics](#14-success-metrics)
    - [14.1 Technical Metrics](#141-technical-metrics)
    - [14.2 User Metrics](#142-user-metrics)
  - [15. References](#15-references)
    - [15.1 MCP Specification](#151-mcp-specification)
    - [15.2 ggen Documentation](#152-ggen-documentation)
    - [15.3 Related Patterns](#153-related-patterns)
  - [Appendix A: Complete Tool Registry (40 Tools)](#appendix-a-complete-tool-registry-40-tools)
    - [Project Tools (9)](#project-tools-9)
    - [Market Tools (13)](#market-tools-13)
    - [Graph Tools (7)](#graph-tools-7)
    - [Template Tools (5)](#template-tools-5)
    - [Hook Tools (5)](#hook-tools-5)
    - [System Tools (1)](#system-tools-1)
  - [Appendix B: Security Checklist](#appendix-b-security-checklist)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# MCP Integration Design for ggen

**Pattern Reference**: AGENT-READY INTERFACE (Pattern 013)
**Version**: 1.0.0
**Status**: Design Phase
**Author**: MCPArchitect Agent

---

## 1. Architecture Overview

### 1.1 High-Level Design

```
┌─────────────────────────────────────────────────────────────┐
│                     MCP Client Layer                        │
│              (Claude Desktop, IDEs, Custom)                 │
└─────────────────────────────┬───────────────────────────────┘
                              │ JSON-RPC 2.0
                              ▼
┌─────────────────────────────────────────────────────────────┐
│                   MCP Server (Rust/RMCP)                    │
├─────────────────────────────────────────────────────────────┤
│  Transport Layer: stdio | SSE | HTTP                        │
│  Tool Registry: 40+ ggen tools                              │
│  Schema Validation: JSON Schema per tool                    │
│  Security Layer: Path validation, sandboxing                │
└─────────────────────────────┬───────────────────────────────┘
                              │ Process spawn
                              ▼
┌─────────────────────────────────────────────────────────────┐
│                    ggen CLI Core                            │
│              (Existing Rust implementation)                 │
└─────────────────────────────────────────────────────────────┘
```

### 1.2 Design Principles

1. **Thin Wrapper Philosophy**: MCP server is a thin protocol adapter, NOT a reimplementation
2. **Schema-First**: Every tool has explicit JSON schema for parameters and responses
3. **Fail-Fast Validation**: Validate inputs at MCP layer before invoking CLI
4. **Transparent Errors**: Map CLI exit codes and stderr to structured MCP errors
5. **Stateless Operations**: Each MCP tool call is independent (except for session-based operations)
6. **Progressive Enhancement**: Start with core tools, expand to advanced features

### 1.3 Implementation Strategy

**Phase 1 (MVP)**: Core project tools (gen, plan, apply)
**Phase 2**: Market and template tools
**Phase 3**: Graph and hook tools
**Phase 4**: Advanced features (watch, streaming, hooks)

---

## 2. Tool Mapping: CLI Commands → MCP Tools

### 2.1 Tool Naming Convention

Format: `ggen_{noun}_{verb}`

Examples:
- `ggen_project_gen` (ggen gen)
- `ggen_market_search` (ggen market search)
- `ggen_graph_query` (ggen graph query)

### 2.2 Complete Tool Catalog (40 Tools)

#### Project Tools (9 tools)
| MCP Tool Name | CLI Command | Priority | Description |
|--------------|-------------|----------|-------------|
| `ggen_project_gen` | `ggen gen` | P0 | Generate code from template |
| `ggen_project_plan` | `ggen plan` | P0 | Plan multi-template project |
| `ggen_project_apply` | `ggen apply` | P0 | Apply planned changes |
| `ggen_project_diff` | `ggen diff` | P1 | Show diff of planned changes |
| `ggen_project_test` | `ggen test` | P1 | Run template tests |
| `ggen_project_freeze` | `ggen freeze` | P2 | Freeze current state |
| `ggen_project_inject` | `ggen inject` | P2 | Inject data into template |
| `ggen_project_validate` | `ggen validate` | P1 | Validate template/data |
| `ggen_project_watch` | `ggen watch` | P3 | Watch for changes (streaming) |

#### Market Tools (13 tools)
| MCP Tool Name | CLI Command | Priority | Description |
|--------------|-------------|----------|-------------|
| `ggen_market_search` | `ggen market search` | P0 | Search template marketplace |
| `ggen_market_add` | `ggen market add` | P0 | Add template to local registry |
| `ggen_market_remove` | `ggen market remove` | P1 | Remove template from registry |
| `ggen_market_list` | `ggen market list` | P0 | List local templates |
| `ggen_market_update` | `ggen market update` | P1 | Update template from source |
| `ggen_market_info` | `ggen market info` | P1 | Show template details |
| `ggen_market_publish` | `ggen market publish` | P2 | Publish template to marketplace |
| `ggen_market_unpublish` | `ggen market unpublish` | P2 | Remove from marketplace |
| `ggen_market_download` | `ggen market download` | P1 | Download template bundle |
| `ggen_market_upload` | `ggen market upload` | P2 | Upload template bundle |
| `ggen_market_version` | `ggen market version` | P1 | Manage template versions |
| `ggen_market_stats` | `ggen market stats` | P2 | Get marketplace statistics |
| `ggen_market_sync` | `ggen market sync` | P1 | Sync with remote registry |

#### Graph Tools (7 tools)
| MCP Tool Name | CLI Command | Priority | Description |
|--------------|-------------|----------|-------------|
| `ggen_graph_query` | `ggen graph query` | P1 | Run SPARQL query |
| `ggen_graph_load` | `ggen graph load` | P1 | Load RDF data |
| `ggen_graph_export` | `ggen graph export` | P1 | Export graph to file |
| `ggen_graph_validate` | `ggen graph validate` | P2 | Validate RDF/SHACL |
| `ggen_graph_stats` | `ggen graph stats` | P2 | Graph statistics |
| `ggen_graph_diff` | `ggen graph diff` | P2 | Diff two graphs |
| `ggen_graph_snapshot` | `ggen graph snapshot` | P2 | Create graph snapshot |

#### Template Tools (5 tools)
| MCP Tool Name | CLI Command | Priority | Description |
|--------------|-------------|----------|-------------|
| `ggen_template_new` | `ggen template new` | P0 | Create new template |
| `ggen_template_list` | `ggen template list` | P0 | List available templates |
| `ggen_template_show` | `ggen template show` | P1 | Show template content |
| `ggen_template_lint` | `ggen template lint` | P1 | Lint template syntax |
| `ggen_template_regenerate` | `ggen template regenerate` | P2 | Regenerate from source |

#### Hook Tools (5 tools)
| MCP Tool Name | CLI Command | Priority | Description |
|--------------|-------------|----------|-------------|
| `ggen_hook_create` | `ggen hook create` | P2 | Create new hook |
| `ggen_hook_list` | `ggen hook list` | P2 | List registered hooks |
| `ggen_hook_run` | `ggen hook run` | P2 | Execute hook manually |
| `ggen_hook_remove` | `ggen hook remove` | P2 | Remove hook |
| `ggen_hook_validate` | `ggen hook validate` | P2 | Validate hook config |

#### System Tools (1 tool)
| MCP Tool Name | CLI Command | Priority | Description |
|--------------|-------------|----------|-------------|
| `ggen_system_version` | `ggen --version` | P0 | Get ggen version info |

---

## 3. Transport Strategy

### 3.1 Transport Selection Matrix

| Use Case | Transport | Rationale |
|----------|-----------|-----------|
| Claude Desktop | stdio | Standard MCP integration |
| VSCode Extension | stdio | Local process, secure |
| Web IDE (Cloud) | SSE | Server-sent events for streaming |
| API Integration | HTTP | RESTful access, webhooks |
| CLI Testing | stdio | Direct process spawn |

### 3.2 Transport Configuration

```rust
// server/src/transport.rs
pub enum TransportMode {
    Stdio,           // Default: stdin/stdout
    Sse { port: u16 },  // Server-sent events
    Http { port: u16, cors: bool }, // HTTP API
}

impl TransportMode {
    pub fn from_env() -> Self {
        match std::env::var("GGEN_MCP_TRANSPORT") {
            Ok(val) if val == "sse" => {
                let port = std::env::var("GGEN_MCP_PORT")
                    .unwrap_or("3000".into())
                    .parse()
                    .unwrap_or(3000);
                TransportMode::Sse { port }
            }
            Ok(val) if val == "http" => {
                let port = std::env::var("GGEN_MCP_PORT")
                    .unwrap_or("3000".into())
                    .parse()
                    .unwrap_or(3000);
                TransportMode::Http { port, cors: true }
            }
            _ => TransportMode::Stdio,
        }
    }
}
```

### 3.3 Streaming Support (Future)

For tools like `ggen watch` and `ggen project apply --stream`:

```json
{
  "method": "tools/stream",
  "params": {
    "name": "ggen_project_watch",
    "arguments": {
      "template": "my-app.tmpl",
      "data": "data.yaml"
    }
  }
}
```

Response: Multiple messages over SSE channel.

---

## 4. Error Handling

### 4.1 Error Mapping Strategy

| CLI Error | MCP Error Code | Strategy |
|-----------|---------------|----------|
| Exit code 0 | Success (no error) | Return result |
| Exit code 1 | -32602 (Invalid params) | Parse stderr for validation errors |
| Exit code 2 | -32603 (Internal error) | Template engine error |
| Exit code 127 | -32601 (Method not found) | Unknown command |
| Timeout | -32000 (Server error) | Custom error |

### 4.2 Error Response Schema

```json
{
  "error": {
    "code": -32602,
    "message": "Template validation failed",
    "data": {
      "ggen_exit_code": 1,
      "stderr": "Error: Unknown placeholder {{unknown_var}}",
      "suggestion": "Add 'unknown_var' to your data file",
      "tool": "ggen_project_gen",
      "timestamp": "2025-10-10T12:34:56Z"
    }
  }
}
```

### 4.3 Error Recovery Patterns

```rust
// server/src/error.rs
pub struct GgenError {
    pub exit_code: i32,
    pub stderr: String,
    pub stdout: String,
    pub context: String,
}

impl GgenError {
    pub fn to_mcp_error(&self) -> McpError {
        let code = match self.exit_code {
            1 => ErrorCode::InvalidParams,
            2 => ErrorCode::InternalError,
            127 => ErrorCode::MethodNotFound,
            _ => ErrorCode::ServerError(-32000),
        };

        McpError {
            code,
            message: self.parse_error_message(),
            data: Some(json!({
                "ggen_exit_code": self.exit_code,
                "stderr": self.stderr,
                "stdout": self.stdout,
                "context": self.context,
            })),
        }
    }

    fn parse_error_message(&self) -> String {
        // Extract first line of stderr as primary message
        self.stderr
            .lines()
            .find(|line| line.starts_with("Error:"))
            .unwrap_or(&self.stderr)
            .trim()
            .to_string()
    }
}
```

---

## 5. Security Model

### 5.1 Security Layers

```
┌─────────────────────────────────────────┐
│  Layer 1: Input Validation              │
│  - JSON schema validation                │
│  - Path traversal prevention             │
│  - Size limits (file paths, data)        │
└─────────────────┬───────────────────────┘
                  ▼
┌─────────────────────────────────────────┐
│  Layer 2: Sandboxing                    │
│  - Working directory restrictions        │
│  - Read-only mode for queries            │
│  - Temporary file cleanup                │
└─────────────────┬───────────────────────┘
                  ▼
┌─────────────────────────────────────────┐
│  Layer 3: Resource Limits               │
│  - Execution timeout (30s default)       │
│  - Memory limits (configurable)          │
│  - Concurrent request limits             │
└─────────────────┬───────────────────────┘
                  ▼
┌─────────────────────────────────────────┐
│  Layer 4: Audit Logging                 │
│  - All tool invocations logged           │
│  - Failed attempts tracked               │
│  - Sensitive data redaction              │
└─────────────────────────────────────────┘
```

### 5.2 Path Validation

```rust
// server/src/security.rs
pub struct PathValidator {
    allowed_dirs: Vec<PathBuf>,
    deny_patterns: Vec<Regex>,
}

impl PathValidator {
    pub fn validate(&self, path: &str) -> Result<PathBuf, SecurityError> {
        let canonical = std::fs::canonicalize(path)
            .map_err(|_| SecurityError::InvalidPath)?;

        // Check against allowed directories
        if !self.allowed_dirs.iter().any(|d| canonical.starts_with(d)) {
            return Err(SecurityError::PathOutsideSandbox);
        }

        // Check deny patterns (e.g., .git, .env)
        if self.deny_patterns.iter().any(|re| re.is_match(path)) {
            return Err(SecurityError::DeniedPath);
        }

        Ok(canonical)
    }
}
```

### 5.3 Configuration Security

```yaml
# config/security.yaml
security:
  sandbox:
    enabled: true
    allowed_dirs:
      - "${HOME}/projects"
      - "${TMPDIR}/ggen"
    deny_patterns:
      - "^\\.git/"
      - "^\\.env"
      - ".*\\.key$"
      - ".*\\.pem$"

  limits:
    execution_timeout_secs: 30
    max_file_size_mb: 100
    max_concurrent_requests: 10

  audit:
    enabled: true
    log_file: "${HOME}/.ggen/mcp-audit.log"
    redact_patterns:
      - "password"
      - "api_key"
      - "secret"
```

---

## 6. Tool Categories & Schemas

### 6.1 Category Organization

Tools are grouped into 6 functional categories:

1. **Project Management** (9 tools): Core generation workflow
2. **Marketplace** (13 tools): Template discovery and sharing
3. **Knowledge Graph** (7 tools): RDF/SPARQL operations
4. **Template Authoring** (5 tools): Template creation/editing
5. **Hooks & Extensions** (5 tools): Lifecycle hooks
6. **System** (1 tool): Version and status

### 6.2 Schema Design Patterns

#### Pattern A: Simple Input/Output
Tools with minimal parameters and text output.

**Example**: `ggen_system_version`

```json
{
  "name": "ggen_system_version",
  "description": "Get ggen version and build information",
  "inputSchema": {
    "type": "object",
    "properties": {},
    "required": []
  },
  "outputSchema": {
    "type": "object",
    "properties": {
      "version": { "type": "string" },
      "commit": { "type": "string" },
      "build_date": { "type": "string" }
    }
  }
}
```

#### Pattern B: File-Based Operations
Tools that work with file paths and content.

**Example**: `ggen_project_gen`

```json
{
  "name": "ggen_project_gen",
  "description": "Generate code from template using data file",
  "inputSchema": {
    "type": "object",
    "properties": {
      "template": {
        "type": "string",
        "description": "Path to template file (.tmpl)",
        "pattern": "^[^\\0]+\\.tmpl$"
      },
      "data": {
        "type": "string",
        "description": "Path to data file (YAML/JSON/TOML)",
        "pattern": "^[^\\0]+\\.(yaml|yml|json|toml)$"
      },
      "output": {
        "type": "string",
        "description": "Output file path (optional, defaults to stdout)"
      },
      "overwrite": {
        "type": "boolean",
        "description": "Overwrite existing output file",
        "default": false
      }
    },
    "required": ["template", "data"]
  },
  "outputSchema": {
    "type": "object",
    "properties": {
      "content": { "type": "string" },
      "output_file": { "type": "string" },
      "bytes_written": { "type": "integer" }
    }
  }
}
```

#### Pattern C: Query/Search Operations
Tools that return lists or search results.

**Example**: `ggen_market_search`

```json
{
  "name": "ggen_market_search",
  "description": "Search template marketplace with filters",
  "inputSchema": {
    "type": "object",
    "properties": {
      "query": {
        "type": "string",
        "description": "Search query (template name, tags, description)"
      },
      "category": {
        "type": "string",
        "enum": ["web", "cli", "api", "data", "ml", "devops"]
      },
      "language": {
        "type": "string",
        "description": "Programming language filter"
      },
      "min_rating": {
        "type": "number",
        "minimum": 0,
        "maximum": 5
      },
      "limit": {
        "type": "integer",
        "default": 20,
        "minimum": 1,
        "maximum": 100
      },
      "offset": {
        "type": "integer",
        "default": 0,
        "minimum": 0
      }
    },
    "required": []
  },
  "outputSchema": {
    "type": "object",
    "properties": {
      "results": {
        "type": "array",
        "items": {
          "type": "object",
          "properties": {
            "id": { "type": "string" },
            "name": { "type": "string" },
            "description": { "type": "string" },
            "author": { "type": "string" },
            "rating": { "type": "number" },
            "downloads": { "type": "integer" },
            "tags": { "type": "array", "items": { "type": "string" } }
          }
        }
      },
      "total": { "type": "integer" },
      "page": { "type": "integer" }
    }
  }
}
```

#### Pattern D: Graph/RDF Operations
Tools that work with RDF triples and SPARQL.

**Example**: `ggen_graph_query`

```json
{
  "name": "ggen_graph_query",
  "description": "Execute SPARQL query against RDF graph",
  "inputSchema": {
    "type": "object",
    "properties": {
      "query": {
        "type": "string",
        "description": "SPARQL query (SELECT, CONSTRUCT, ASK, DESCRIBE)"
      },
      "graph": {
        "type": "string",
        "description": "Path to RDF file or named graph URI"
      },
      "format": {
        "type": "string",
        "enum": ["json", "xml", "csv", "tsv", "turtle"],
        "default": "json"
      },
      "limit": {
        "type": "integer",
        "description": "Result limit (overrides query LIMIT)",
        "minimum": 1
      }
    },
    "required": ["query"]
  },
  "outputSchema": {
    "type": "object",
    "properties": {
      "bindings": {
        "type": "array",
        "description": "Query results (format-dependent)"
      },
      "execution_time_ms": { "type": "integer" },
      "result_count": { "type": "integer" }
    }
  }
}
```

---

## 7. Example Tool Schemas (Top 10 Priority Tools)

### 7.1 P0 Tools (Must-Have for MVP)

#### 1. `ggen_project_gen`
**CLI**: `ggen gen <template> <data> [output]`

```json
{
  "name": "ggen_project_gen",
  "description": "Generate code from template using data file. Supports Handlebars syntax with helpers.",
  "inputSchema": {
    "type": "object",
    "properties": {
      "template": {
        "type": "string",
        "description": "Template file path (.tmpl extension)"
      },
      "data": {
        "type": "string",
        "description": "Data file path (YAML, JSON, or TOML)"
      },
      "output": {
        "type": "string",
        "description": "Output file path (optional, writes to stdout if omitted)"
      },
      "overwrite": {
        "type": "boolean",
        "default": false,
        "description": "Overwrite output file if it exists"
      }
    },
    "required": ["template", "data"]
  }
}
```

#### 2. `ggen_project_plan`
**CLI**: `ggen plan <config>`

```json
{
  "name": "ggen_project_plan",
  "description": "Create execution plan for multi-template project generation",
  "inputSchema": {
    "type": "object",
    "properties": {
      "config": {
        "type": "string",
        "description": "Project config file (ggen.yaml)"
      },
      "output": {
        "type": "string",
        "description": "Plan output file (optional)"
      },
      "dry_run": {
        "type": "boolean",
        "default": false,
        "description": "Show plan without creating files"
      }
    },
    "required": ["config"]
  }
}
```

#### 3. `ggen_project_apply`
**CLI**: `ggen apply <plan>`

```json
{
  "name": "ggen_project_apply",
  "description": "Execute planned changes to generate project structure",
  "inputSchema": {
    "type": "object",
    "properties": {
      "plan": {
        "type": "string",
        "description": "Plan file from 'ggen plan' command"
      },
      "force": {
        "type": "boolean",
        "default": false,
        "description": "Force apply even with conflicts"
      },
      "interactive": {
        "type": "boolean",
        "default": false,
        "description": "Prompt before each file operation"
      }
    },
    "required": ["plan"]
  }
}
```

#### 4. `ggen_market_search`
**CLI**: `ggen market search [query]`

```json
{
  "name": "ggen_market_search",
  "description": "Search template marketplace with filters",
  "inputSchema": {
    "type": "object",
    "properties": {
      "query": {
        "type": "string",
        "description": "Search query (name, tags, description)"
      },
      "category": {
        "type": "string",
        "enum": ["web", "cli", "api", "data", "ml", "devops", "mobile"]
      },
      "language": {
        "type": "string",
        "description": "Programming language (e.g., 'rust', 'python')"
      },
      "limit": {
        "type": "integer",
        "default": 20,
        "minimum": 1,
        "maximum": 100
      }
    },
    "required": []
  }
}
```

#### 5. `ggen_market_add`
**CLI**: `ggen market add <source>`

```json
{
  "name": "ggen_market_add",
  "description": "Add template to local registry from URL or file",
  "inputSchema": {
    "type": "object",
    "properties": {
      "source": {
        "type": "string",
        "description": "Template source (URL, file path, or marketplace ID)"
      },
      "name": {
        "type": "string",
        "description": "Local name for template (optional)"
      },
      "force": {
        "type": "boolean",
        "default": false,
        "description": "Overwrite if template already exists"
      }
    },
    "required": ["source"]
  }
}
```

#### 6. `ggen_market_list`
**CLI**: `ggen market list`

```json
{
  "name": "ggen_market_list",
  "description": "List all templates in local registry",
  "inputSchema": {
    "type": "object",
    "properties": {
      "format": {
        "type": "string",
        "enum": ["table", "json", "yaml"],
        "default": "table"
      },
      "filter": {
        "type": "string",
        "description": "Filter by name pattern (regex)"
      }
    },
    "required": []
  }
}
```

#### 7. `ggen_template_new`
**CLI**: `ggen template new <name>`

```json
{
  "name": "ggen_template_new",
  "description": "Create new template with boilerplate structure",
  "inputSchema": {
    "type": "object",
    "properties": {
      "name": {
        "type": "string",
        "description": "Template name (will create <name>.tmpl)"
      },
      "scaffold": {
        "type": "string",
        "enum": ["minimal", "standard", "full"],
        "default": "standard",
        "description": "Scaffold complexity level"
      },
      "output_dir": {
        "type": "string",
        "description": "Directory to create template in"
      }
    },
    "required": ["name"]
  }
}
```

#### 8. `ggen_template_list`
**CLI**: `ggen template list`

```json
{
  "name": "ggen_template_list",
  "description": "List available templates in current directory and registry",
  "inputSchema": {
    "type": "object",
    "properties": {
      "directory": {
        "type": "string",
        "description": "Directory to search (defaults to current)"
      },
      "include_registry": {
        "type": "boolean",
        "default": true,
        "description": "Include templates from local registry"
      }
    },
    "required": []
  }
}
```

#### 9. `ggen_graph_query`
**CLI**: `ggen graph query <sparql>`

```json
{
  "name": "ggen_graph_query",
  "description": "Execute SPARQL query against RDF knowledge graph",
  "inputSchema": {
    "type": "object",
    "properties": {
      "query": {
        "type": "string",
        "description": "SPARQL query string (SELECT, ASK, CONSTRUCT, DESCRIBE)"
      },
      "graph": {
        "type": "string",
        "description": "RDF file path or named graph URI"
      },
      "format": {
        "type": "string",
        "enum": ["json", "csv", "tsv", "xml", "turtle"],
        "default": "json"
      }
    },
    "required": ["query"]
  }
}
```

#### 10. `ggen_system_version`
**CLI**: `ggen --version`

```json
{
  "name": "ggen_system_version",
  "description": "Get ggen version, build info, and system capabilities",
  "inputSchema": {
    "type": "object",
    "properties": {
      "verbose": {
        "type": "boolean",
        "default": false,
        "description": "Include detailed build information"
      }
    },
    "required": []
  }
}
```

---

## 8. Implementation Plan

### 8.1 Phase 1: MVP (Week 1-2)

**Goal**: Core project generation tools working via MCP

**Deliverables**:
1. MCP server scaffold with stdio transport
2. Tool executor (spawn ggen CLI subprocess)
3. 10 P0 tools implemented with schemas
4. Basic error handling and logging
5. Claude Desktop integration test

**Success Metrics**:
- All 10 P0 tools callable from Claude Desktop
- Error messages properly formatted
- 90% test coverage on tool executor

### 8.2 Phase 2: Marketplace & Templates (Week 3-4)

**Goal**: Template discovery and management tools

**Deliverables**:
1. Remaining market tools (13 total)
2. Template authoring tools (5 total)
3. Enhanced error messages with suggestions
4. Path validation and sandboxing

**Success Metrics**:
- Search, add, list workflow works end-to-end
- Template creation from Claude Desktop
- Security audit passes

### 8.3 Phase 3: Graph & Hooks (Week 5-6)

**Goal**: Advanced RDF/SPARQL and extensibility

**Deliverables**:
1. Graph query tools (7 total)
2. Hook management tools (5 total)
3. SSE transport for streaming
4. Performance optimization

**Success Metrics**:
- Complex SPARQL queries execute in <1s
- Hooks trigger correctly from MCP calls
- Streaming watch mode works

### 8.4 Phase 4: Production Hardening (Week 7-8)

**Goal**: Production-ready, documented, tested

**Deliverables**:
1. Comprehensive test suite (unit + integration)
2. Security audit and penetration testing
3. Performance benchmarks
4. User documentation and examples
5. Claude Desktop app store submission

**Success Metrics**:
- 95% test coverage
- Zero critical security issues
- <100ms p99 latency for fast tools
- 10+ example workflows documented

---

## 9. Technical Deep-Dive

### 9.1 Server Implementation (Rust)

```rust
// server/src/main.rs
use rmcp::{Server, Tool, ToolInput, ToolOutput};
use std::process::Command;

#[tokio::main]
async fn main() -> Result<()> {
    let server = Server::new("ggen-mcp-server", "1.0.0");

    // Register all tools
    register_project_tools(&server);
    register_market_tools(&server);
    register_graph_tools(&server);
    register_template_tools(&server);
    register_hook_tools(&server);

    // Start transport
    let transport = TransportMode::from_env();
    server.start(transport).await?;

    Ok(())
}

fn register_project_tools(server: &Server) {
    server.register_tool(Tool {
        name: "ggen_project_gen",
        description: "Generate code from template",
        input_schema: load_schema("project_gen.json"),
        handler: Box::new(|input: ToolInput| async move {
            execute_ggen_command(vec![
                "gen",
                &input.get("template")?,
                &input.get("data")?,
                "--output", &input.get_or("output", "-"),
            ]).await
        }),
    });
}

async fn execute_ggen_command(args: Vec<&str>) -> Result<ToolOutput> {
    let output = Command::new("ggen")
        .args(&args)
        .output()
        .await?;

    if output.status.success() {
        Ok(ToolOutput {
            content: String::from_utf8(output.stdout)?,
            metadata: json!({
                "exit_code": 0,
                "execution_time_ms": 123,
            }),
        })
    } else {
        Err(GgenError::from_output(output).to_mcp_error())
    }
}
```

### 9.2 Schema Management

```rust
// server/src/schema.rs
pub struct SchemaRegistry {
    schemas: HashMap<String, JsonSchema>,
}

impl SchemaRegistry {
    pub fn load_all() -> Result<Self> {
        let mut schemas = HashMap::new();

        for entry in glob("schemas/*.json")? {
            let path = entry?;
            let name = path.file_stem().unwrap().to_str().unwrap();
            let schema = serde_json::from_str(&fs::read_to_string(&path)?)?;
            schemas.insert(name.to_string(), schema);
        }

        Ok(SchemaRegistry { schemas })
    }

    pub fn validate(&self, tool: &str, input: &Value) -> Result<()> {
        let schema = self.schemas.get(tool)
            .ok_or(SchemaError::NotFound)?;

        schema.validate(input)
            .map_err(|e| SchemaError::ValidationFailed(e))?;

        Ok(())
    }
}
```

### 9.3 Configuration System

```rust
// server/src/config.rs
#[derive(Deserialize)]
pub struct ServerConfig {
    pub transport: TransportConfig,
    pub security: SecurityConfig,
    pub logging: LoggingConfig,
}

impl ServerConfig {
    pub fn load() -> Result<Self> {
        // Priority: ENV > config file > defaults
        let mut config = Self::default();

        if let Ok(path) = env::var("GGEN_MCP_CONFIG") {
            config = config.merge_file(path)?;
        }

        config = config.merge_env()?;
        config.validate()?;

        Ok(config)
    }
}
```

---

## 10. Testing Strategy

### 10.1 Test Pyramid

```
                    ┌─────────────┐
                    │   E2E (5%)  │  Claude Desktop integration
                    └─────────────┘
                  ┌───────────────────┐
                  │ Integration (15%) │  Full tool execution
                  └───────────────────┘
              ┌─────────────────────────────┐
              │      Unit Tests (80%)       │  Schema, parser, executor
              └─────────────────────────────┘
```

### 10.2 Test Coverage Goals

| Component | Target Coverage | Test Types |
|-----------|----------------|------------|
| Tool executor | 95% | Unit, integration |
| Schema validation | 100% | Unit |
| Error handling | 90% | Unit, integration |
| Security layer | 100% | Unit, security |
| Transport layer | 85% | Integration |

### 10.3 Example Test

```rust
#[tokio::test]
async fn test_ggen_project_gen_success() {
    let server = create_test_server();

    let input = json!({
        "template": "tests/fixtures/hello.tmpl",
        "data": "tests/fixtures/data.yaml",
    });

    let result = server.call_tool("ggen_project_gen", input).await;

    assert!(result.is_ok());
    assert!(result.unwrap().content.contains("Hello, World"));
}

#[tokio::test]
async fn test_invalid_template_path() {
    let server = create_test_server();

    let input = json!({
        "template": "../../../etc/passwd",
        "data": "data.yaml",
    });

    let result = server.call_tool("ggen_project_gen", input).await;

    assert!(result.is_err());
    assert_eq!(result.unwrap_err().code, ErrorCode::InvalidParams);
}
```

---

## 11. Deployment & Distribution

### 11.1 Package Structure

```
ggen-mcp-server/
├── Cargo.toml
├── src/
│   ├── main.rs
│   ├── tools/
│   │   ├── project.rs
│   │   ├── market.rs
│   │   ├── graph.rs
│   │   ├── template.rs
│   │   └── hook.rs
│   ├── schema.rs
│   ├── executor.rs
│   ├── error.rs
│   ├── security.rs
│   └── transport.rs
├── schemas/
│   ├── project_gen.json
│   ├── market_search.json
│   └── ... (40 schemas)
├── config/
│   ├── default.yaml
│   └── security.yaml
└── tests/
```

### 11.2 Installation Methods

**Method 1: Cargo Install**
```bash
cargo install ggen-mcp-server
```

**Method 2: Claude Desktop Config**
```json
{
  "mcpServers": {
    "ggen": {
      "command": "ggen-mcp-server",
      "args": ["--transport", "stdio"],
      "env": {
        "GGEN_MCP_LOG_LEVEL": "info"
      }
    }
  }
}
```

**Method 3: Docker**
```bash
docker run -it ggen/mcp-server:latest
```

### 11.3 CI/CD Pipeline

```yaml
# .github/workflows/mcp-server.yml
name: MCP Server CI

on: [push, pull_request]

jobs:
  test:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3
      - uses: actions-rs/toolchain@v1
      - run: cargo test --all-features
      - run: cargo clippy -- -D warnings

  security:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3
      - run: cargo audit
      - run: cargo deny check

  publish:
    needs: [test, security]
    if: startsWith(github.ref, 'refs/tags/v')
    runs-on: ubuntu-latest
    steps:
      - run: cargo publish --token ${{ secrets.CARGO_TOKEN }}
```

---

## 12. Performance Benchmarks

### 12.1 Target Latencies

| Operation | Target (p50) | Target (p99) |
|-----------|-------------|-------------|
| Simple gen | 50ms | 200ms |
| Market search | 100ms | 500ms |
| SPARQL query | 200ms | 1000ms |
| Plan creation | 300ms | 2000ms |
| Apply plan | 1000ms | 5000ms |

### 12.2 Optimization Strategies

1. **Caching**: Schema validation results, CLI subprocess warm-up
2. **Batching**: Combine multiple tool calls into single ggen invocation
3. **Async**: Parallel execution for independent operations
4. **Process Pooling**: Keep ggen processes warm for repeated calls

---

## 13. Future Enhancements

### 13.1 Advanced Features (Post-MVP)

1. **Streaming Responses**: Real-time output for long-running operations
2. **Webhooks**: Trigger external systems on template events
3. **Collaborative Editing**: Multi-user template authoring via CRDTs
4. **AI-Assisted Templating**: LLM-powered template generation
5. **Visual Builder**: Web UI for template creation (SSE transport)

### 13.2 Integration Roadmap

- **VS Code Extension**: Direct integration with ggen MCP server
- **GitHub Actions**: Template generation in CI/CD pipelines
- **Jupyter Notebooks**: Interactive template development
- **Claude Code**: Deep integration with Claude's agentic workflows

---

## 14. Success Metrics

### 14.1 Technical Metrics

- **Reliability**: 99.9% uptime for MCP server
- **Performance**: <100ms p50 latency for fast tools
- **Security**: Zero critical vulnerabilities
- **Test Coverage**: >90% line coverage

### 14.2 User Metrics

- **Adoption**: 1000+ active users in first 3 months
- **Satisfaction**: >4.5/5 star rating in Claude app store
- **Template Growth**: 500+ templates in marketplace within 6 months
- **Community**: 50+ contributors to template library

---

## 15. References

### 15.1 MCP Specification
- **Protocol**: https://spec.modelcontextprotocol.io/
- **RMCP SDK**: https://github.com/modelcontextprotocol/rmcp

### 15.2 ggen Documentation
- **CLI Reference**: /docs/CLI-COMMANDS.md
- **Template Syntax**: /docs/TEMPLATE-SYNTAX.md
- **Graph Operations**: /docs/RDF-SPARQL.md

### 15.3 Related Patterns
- **Pattern 013**: AGENT-READY INTERFACE (this implementation)
- **Pattern 008**: COMPOSITIONAL GENERATION
- **Pattern 015**: SEMANTIC GRAPH INTEGRATION

---

## Appendix A: Complete Tool Registry (40 Tools)

### Project Tools (9)
1. `ggen_project_gen` - Generate from template
2. `ggen_project_plan` - Create execution plan
3. `ggen_project_apply` - Apply planned changes
4. `ggen_project_diff` - Show plan diff
5. `ggen_project_test` - Run template tests
6. `ggen_project_freeze` - Freeze current state
7. `ggen_project_inject` - Inject data
8. `ggen_project_validate` - Validate template/data
9. `ggen_project_watch` - Watch for changes

### Market Tools (13)
10. `ggen_market_search` - Search marketplace
11. `ggen_market_add` - Add template
12. `ggen_market_remove` - Remove template
13. `ggen_market_list` - List local templates
14. `ggen_market_update` - Update template
15. `ggen_market_info` - Template details
16. `ggen_market_publish` - Publish to marketplace
17. `ggen_market_unpublish` - Remove from marketplace
18. `ggen_market_download` - Download bundle
19. `ggen_market_upload` - Upload bundle
20. `ggen_market_version` - Manage versions
21. `ggen_market_stats` - Marketplace stats
22. `ggen_market_sync` - Sync with remote

### Graph Tools (7)
23. `ggen_graph_query` - SPARQL query
24. `ggen_graph_load` - Load RDF data
25. `ggen_graph_export` - Export graph
26. `ggen_graph_validate` - Validate RDF/SHACL
27. `ggen_graph_stats` - Graph statistics
28. `ggen_graph_diff` - Diff graphs
29. `ggen_graph_snapshot` - Create snapshot

### Template Tools (5)
30. `ggen_template_new` - Create template
31. `ggen_template_list` - List templates
32. `ggen_template_show` - Show content
33. `ggen_template_lint` - Lint template
34. `ggen_template_regenerate` - Regenerate

### Hook Tools (5)
35. `ggen_hook_create` - Create hook
36. `ggen_hook_list` - List hooks
37. `ggen_hook_run` - Execute hook
38. `ggen_hook_remove` - Remove hook
39. `ggen_hook_validate` - Validate hook

### System Tools (1)
40. `ggen_system_version` - Version info

---

## Appendix B: Security Checklist

- [ ] Path traversal prevention (canonicalization)
- [ ] Denied path patterns (secrets, .git, etc.)
- [ ] Execution timeout enforcement
- [ ] Memory limit configuration
- [ ] Rate limiting per client
- [ ] Audit logging enabled
- [ ] Sensitive data redaction
- [ ] Sandboxed execution environment
- [ ] Input schema validation
- [ ] Output size limits
- [ ] Error message sanitization
- [ ] Secure transport (TLS for HTTP/SSE)

---

**Document Status**: ✅ Complete
**Next Steps**: Begin Phase 1 implementation (server scaffold + P0 tools)
**Approval Required**: Technical Lead, Security Team
