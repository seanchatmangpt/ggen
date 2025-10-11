# GGen MCP Tools Reference

Complete reference for all 25+ MCP tools available in ggen-mcp.

## Quick Tool Index

| Category | Tools |
|----------|-------|
| **Project** | project_gen, project_plan, project_apply, project_diff |
| **Marketplace** | market_list, market_search, market_install, market_info, market_recommend, market_offline_search, market_cache_status, market_sync |
| **Graph** | graph_query, graph_load, graph_export |
| **Template** | template_create, template_validate |
| **Hook** | hook_register |
| **AI** | ai_generate_template, ai_generate_sparql, ai_generate_ontology, ai_generate_project, ai_extend_graph, ai_validate_and_improve, ai_list_providers |

---

## Project Tools (4)

### 1. project_gen

Generate files from a template with variables.

**Parameters:**
- `template` (string, required) - Template name or path
- `vars` (object, optional) - Template variables as key-value pairs
- `dry_run` (boolean, optional) - Preview without creating files
- `force` (boolean, optional) - Overwrite existing files

**Example:**
```json
{
  "template": "react-typescript-starter",
  "vars": {
    "project_name": "my-app",
    "enable_dark_theme": true
  },
  "dry_run": false
}
```

**Response:**
```json
{
  "status": "success",
  "generated_files": ["src/App.tsx", "package.json", ...],
  "output_dir": "/path/to/my-app",
  "next_steps": ["cd my-app", "npm install", "npm run dev"]
}
```

---

### 2. project_plan

Create an execution plan without applying changes.

**Parameters:**
- `template` (string, required) - Template name
- `vars` (object, optional) - Template variables

**Example:**
```json
{
  "template": "fastapi-microservice",
  "vars": {
    "project_name": "api",
    "port": 8000
  }
}
```

**Response:**
```json
{
  "status": "success",
  "plan": {
    "actions": [
      {"type": "create", "path": "main.py", "size": 1234},
      ...
    ],
    "total_files": 12,
    "total_size": "45.2 KB"
  }
}
```

---

### 3. project_apply

Apply a previously created execution plan.

**Parameters:**
- `plan` (string, required) - Plan file path or plan JSON

**Example:**
```json
{
  "plan": "/path/to/plan.json"
}
```

---

### 4. project_diff

Show differences between template output and existing files.

**Parameters:**
- `template` (string, required) - Template name
- `vars` (object, optional) - Template variables

**Example:**
```json
{
  "template": "react-typescript-starter",
  "vars": {"project_name": "my-app"}
}
```

**Response:**
```json
{
  "status": "success",
  "differences": [
    {
      "file": "package.json",
      "status": "modified",
      "diff": "@@ -5,7 +5,7 @@\n-version..."
    }
  ],
  "summary": {
    "modified": 3,
    "new": 2,
    "deleted": 1,
    "unchanged": 8
  }
}
```

---

## Marketplace Tools (8)

### 5. market_list

List available templates from the marketplace.

**Parameters:**
- `category` (string, optional) - Filter by category (web, api, cli, library, infrastructure)
- `tag` (string, optional) - Filter by tag

**Example:**
```json
{
  "category": "web",
  "tag": "react"
}
```

---

### 6. market_search

Search marketplace templates by query.

**Parameters:**
- `query` (string, required) - Search query
- `category` (string, optional) - Category filter
- `limit` (integer, optional) - Max results (default: 10)

**Example:**
```json
{
  "query": "REST API authentication",
  "category": "api",
  "limit": 5
}
```

**Response:**
```json
{
  "status": "success",
  "results": [
    {
      "id": "fastapi-auth-db",
      "name": "FastAPI Auth & DB",
      "rating": 4.8,
      "downloads": 12453,
      "relevance_score": 0.95
    }
  ]
}
```

---

### 7. market_install

Install a template from the marketplace.

**Parameters:**
- `package` (string, required) - Package ID or name
- `version` (string, optional) - Specific version (default: latest)

**Example:**
```json
{
  "package": "fastapi-auth-db",
  "version": "2.1.0"
}
```

---

### 8. market_info

Get detailed information about a specific package.

**Parameters:**
- `package_id` (string, required) - Package identifier

**Example:**
```json
{
  "package_id": "fastapi-auth-db"
}
```

**Response:**
```json
{
  "status": "success",
  "package": {
    "id": "fastapi-auth-db",
    "name": "FastAPI Auth & DB",
    "version": "2.1.0",
    "features": [...],
    "variables": {...},
    "stats": {...},
    "health": {...}
  }
}
```

---

### 9. market_recommend

Get personalized package recommendations.

**Parameters:**
- `based_on` (string, optional) - Base recommendations on installed package
- `category` (string, optional) - Category filter
- `limit` (integer, optional) - Max results

**Example:**
```json
{
  "based_on": "fastapi-auth-db",
  "limit": 5
}
```

---

### 10. market_offline_search

Search marketplace packages using cached offline data.

**Parameters:**
- `query` (string, required) - Search query
- `category` (string, optional) - Category filter
- `limit` (integer, optional) - Max results

**Example:**
```json
{
  "query": "docker kubernetes",
  "category": "infrastructure"
}
```

---

### 11. market_cache_status

Get status and statistics about the marketplace cache.

**Parameters:** None

**Example:**
```json
{}
```

**Response:**
```json
{
  "status": "success",
  "cache": {
    "size": "48.1 MB",
    "packages": 142,
    "last_sync": "2024-01-10T16:30:45Z",
    "health": "healthy"
  }
}
```

---

### 12. market_sync

Synchronize local cache with remote marketplace.

**Parameters:**
- `category` (string, optional) - Sync specific category only
- `force` (boolean, optional) - Force full resync

**Example:**
```json
{
  "category": "web",
  "force": false
}
```

---

## Graph Tools (3)

### 13. graph_query

Execute SPARQL query against RDF graph store.

**Parameters:**
- `sparql` (string, required) - SPARQL query
- `graph` (string, optional) - Graph name/URI

**Example:**
```json
{
  "sparql": "SELECT ?s ?p ?o WHERE { ?s ?p ?o } LIMIT 10",
  "graph": "http://ggen.ai/templates"
}
```

**Response:**
```json
{
  "status": "success",
  "results": {
    "bindings": [
      {"s": "...", "p": "...", "o": "..."}
    ],
    "count": 10
  }
}
```

---

### 14. graph_load

Load RDF data from file into graph store.

**Parameters:**
- `file` (string, required) - RDF file path
- `graph` (string, optional) - Target graph name
- `format` (string, optional) - RDF format (turtle, rdfxml, n3, jsonld)

**Example:**
```json
{
  "file": "/path/to/data.ttl",
  "graph": "http://ggen.ai/templates",
  "format": "turtle"
}
```

---

### 15. graph_export

Export RDF graph to file.

**Parameters:**
- `output` (string, required) - Output file path
- `graph` (string, optional) - Graph to export
- `format` (string, optional) - Export format

**Example:**
```json
{
  "output": "/path/to/export.rdf",
  "graph": "http://ggen.ai/templates",
  "format": "rdfxml"
}
```

---

## Template Tools (2)

### 16. template_create

Create a new template.

**Parameters:**
- `name` (string, required) - Template name
- `template_type` (string, optional) - Type (project, module, component)

**Example:**
```json
{
  "name": "python-microservice",
  "template_type": "project"
}
```

---

### 17. template_validate

Validate template syntax and structure.

**Parameters:**
- `template` (string, required) - Template path or name

**Example:**
```json
{
  "template": "python-microservice"
}
```

**Response:**
```json
{
  "status": "success",
  "valid": true,
  "checks": {
    "schema": "valid",
    "variables": "valid",
    "files": "valid"
  },
  "warnings": []
}
```

---

## Hook Tools (1)

### 18. hook_register

Register a lifecycle hook.

**Parameters:**
- `event` (string, required) - Hook event (pre_generate, post_generate, pre_file, post_file, pre_validate, post_validate)
- `command` (string, required) - Command to execute
- `name` (string, optional) - Hook name
- `description` (string, optional) - Description
- `priority` (integer, optional) - Execution priority
- `enabled` (boolean, optional) - Enable/disable (default: true)
- `conditions` (object, optional) - Conditional execution rules

**Example:**
```json
{
  "event": "post_generate",
  "command": "prettier --write .",
  "name": "format_code",
  "description": "Auto-format generated code",
  "priority": 1,
  "enabled": true
}
```

---

## AI Tools (7)

### 19. ai_generate_template

Generate a template from natural language description using AI.

**Parameters:**
- `description` (string, required) - Template description
- `template_type` (string, optional) - Template type
- `provider` (string, optional) - AI provider (anthropic, openai)

**Example:**
```json
{
  "description": "FastAPI microservice with JWT auth and PostgreSQL",
  "template_type": "project",
  "provider": "anthropic"
}
```

**Response:**
```json
{
  "status": "success",
  "template_name": "fastapi-auth-generated",
  "generated_files": [...],
  "features": [...],
  "variables": [...]
}
```

---

### 20. ai_generate_sparql

Generate SPARQL query from natural language intent using AI.

**Parameters:**
- `intent` (string, required) - Natural language query intent
- `schema` (string, optional) - Schema/ontology context
- `provider` (string, optional) - AI provider

**Example:**
```json
{
  "intent": "Find all templates that use React 18 or higher",
  "schema": "ggen ontology",
  "provider": "anthropic"
}
```

**Response:**
```json
{
  "status": "success",
  "generated_query": "PREFIX ggen: <...> SELECT...",
  "explanation": "This query finds..."
}
```

---

### 21. ai_generate_ontology

Generate RDF ontology from domain description using AI.

**Parameters:**
- `domain` (string, required) - Domain description
- `requirements` (string, optional) - Specific requirements
- `provider` (string, optional) - AI provider

**Example:**
```json
{
  "domain": "Software project templates and dependencies",
  "requirements": "Include versioning and compatibility",
  "provider": "anthropic"
}
```

---

### 22. ai_generate_project

Generate complete project structure from description using AI.

**Parameters:**
- `description` (string, required) - Project description
- `requirements` (string, optional) - Specific requirements
- `provider` (string, optional) - AI provider

**Example:**
```json
{
  "description": "REST API for task management with user auth",
  "requirements": "Use FastAPI, PostgreSQL, and Docker",
  "provider": "anthropic"
}
```

---

### 23. ai_extend_graph

Extend existing RDF graph with new knowledge using AI.

**Parameters:**
- `graph` (string, required) - Graph name/URI
- `context` (string, required) - Extension context
- `provider` (string, optional) - AI provider

**Example:**
```json
{
  "graph": "http://ggen.ai/templates",
  "context": "Add semantic relationships and design patterns",
  "provider": "anthropic"
}
```

---

### 24. ai_validate_and_improve

Validate and improve existing code or templates using AI.

**Parameters:**
- `content` (string, required) - Content to validate (path or inline)
- `content_type` (string, required) - Content type (template, code, config)
- `provider` (string, optional) - AI provider

**Example:**
```json
{
  "content": "python-microservice",
  "content_type": "template",
  "provider": "anthropic"
}
```

**Response:**
```json
{
  "status": "success",
  "validation": {
    "issues_found": [...],
    "security_concerns": [...]
  },
  "improvements": {
    "changes": [...],
    "recommendation": "..."
  }
}
```

---

### 25. ai_list_providers

List available AI providers and their capabilities.

**Parameters:** None

**Example:**
```json
{}
```

**Response:**
```json
{
  "status": "success",
  "providers": [
    {
      "name": "anthropic",
      "models": ["claude-3-opus", "claude-3-sonnet"],
      "capabilities": ["code", "sparql", "ontology"],
      "available": true
    },
    {
      "name": "openai",
      "models": ["gpt-4", "gpt-3.5-turbo"],
      "capabilities": ["code", "templates"],
      "available": true
    }
  ]
}
```

---

## Tool Categories Summary

### By Use Case

**Getting Started:**
1. market_search - Find templates
2. market_info - Review details
3. market_install - Install template
4. project_gen - Generate project

**Advanced Workflows:**
1. ai_generate_project - AI-powered project creation
2. graph_query - Query metadata
3. hook_register - Automate tasks
4. ai_validate_and_improve - Enhance quality

**Knowledge Management:**
1. graph_load - Import RDF data
2. ai_generate_ontology - Create ontologies
3. ai_extend_graph - Add relationships
4. graph_export - Share knowledge

**Template Development:**
1. template_create - Create new template
2. ai_generate_template - AI-generated templates
3. template_validate - Validate templates
4. market_install - Install as reference

---

## Common Patterns

### Pattern 1: Discover → Install → Generate
```
market_search → market_info → market_install → project_gen
```

### Pattern 2: AI Generate → Validate → Apply
```
ai_generate_template → template_validate → project_gen
```

### Pattern 3: Query → Analyze → Extend
```
graph_query → ai_generate_sparql → ai_extend_graph
```

### Pattern 4: Generate → Hook → Automate
```
project_gen → hook_register → (auto-formatting, testing, deployment)
```

---

## Response Format

All tools return a consistent JSON response:

```json
{
  "status": "success" | "error",
  "message": "Human-readable message",
  ... // Tool-specific data
}
```

**Error Response:**
```json
{
  "status": "error",
  "error": "Error message",
  "code": "ERROR_CODE",
  "details": {...}
}
```

---

## Environment Variables

Configure ggen-mcp behavior:

- `ANTHROPIC_API_KEY` - Anthropic API key for AI tools
- `OPENAI_API_KEY` - OpenAI API key for AI tools
- `GGEN_MCP_LOG` - Log level (debug, info, warn, error)
- `RUST_BACKTRACE` - Enable Rust backtraces (0, 1, full)

---

## Next Steps

- **Try the tools:** See [example-conversations/](example-conversations/)
- **Installation:** Follow [QUICK_START.md](QUICK_START.md)
- **Full guide:** Read [README.md](README.md)
- **Test tools:** Run `./test-mcp-tools.sh`
