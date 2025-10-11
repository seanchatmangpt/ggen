use serde_json::{json, Value};

/// Schema for project_gen tool
pub fn project_gen_schema() -> Value {
    json!({
        "type": "object",
        "required": ["template"],
        "properties": {
            "template": {
                "type": "string",
                "description": "Template name or path to use for generation"
            },
            "vars": {
                "type": "object",
                "description": "Template variables as key-value pairs"
            },
            "output": {
                "type": "string",
                "description": "Output directory path (default: current directory)"
            }
        }
    })
}

/// Schema for project_plan tool
pub fn project_plan_schema() -> Value {
    json!({
        "type": "object",
        "required": ["template"],
        "properties": {
            "template": {
                "type": "string",
                "description": "Template name or path"
            },
            "vars": {
                "type": "object",
                "description": "Template variables"
            }
        }
    })
}

/// Schema for project_apply tool
pub fn project_apply_schema() -> Value {
    json!({
        "type": "object",
        "required": ["plan"],
        "properties": {
            "plan": {
                "type": "string",
                "description": "Execution plan JSON or path to plan file"
            }
        }
    })
}

/// Schema for project_diff tool
pub fn project_diff_schema() -> Value {
    json!({
        "type": "object",
        "required": ["template"],
        "properties": {
            "template": {
                "type": "string",
                "description": "Template name or path"
            },
            "vars": {
                "type": "object",
                "description": "Template variables"
            },
            "target": {
                "type": "string",
                "description": "Target directory to compare against"
            }
        }
    })
}

/// Schema for market_list tool
pub fn market_list_schema() -> Value {
    json!({
        "type": "object",
        "properties": {
            "category": {
                "type": "string",
                "description": "Filter by category"
            },
            "tag": {
                "type": "string",
                "description": "Filter by tag"
            }
        }
    })
}

/// Schema for market_search tool
pub fn market_search_schema() -> Value {
    json!({
        "type": "object",
        "required": ["query"],
        "properties": {
            "query": {
                "type": "string",
                "description": "Search query string"
            },
            "limit": {
                "type": "number",
                "description": "Maximum number of results"
            }
        }
    })
}

/// Schema for market_install tool
pub fn market_install_schema() -> Value {
    json!({
        "type": "object",
        "required": ["package"],
        "properties": {
            "package": {
                "type": "string",
                "description": "Package name to install"
            },
            "version": {
                "type": "string",
                "description": "Specific version (optional)"
            }
        }
    })
}

/// Schema for market_recommend tool
pub fn market_recommend_schema() -> Value {
    json!({
        "type": "object",
        "properties": {
            "based_on": {
                "type": "string",
                "description": "Base recommendations on a specific installed package"
            },
            "category": {
                "type": "string",
                "description": "Recommend packages from specific category"
            },
            "limit": {
                "type": "number",
                "description": "Maximum number of recommendations",
                "default": 5
            },
            "explain": {
                "type": "boolean",
                "description": "Show recommendation reasoning",
                "default": false
            }
        }
    })
}

/// Schema for market_info tool
pub fn market_info_schema() -> Value {
    json!({
        "type": "object",
        "required": ["package_id"],
        "properties": {
            "package_id": {
                "type": "string",
                "description": "Package ID to get information for"
            },
            "examples": {
                "type": "boolean",
                "description": "Include usage examples",
                "default": false
            },
            "dependencies": {
                "type": "boolean",
                "description": "Include dependency information",
                "default": false
            },
            "health": {
                "type": "boolean",
                "description": "Include detailed health metrics",
                "default": false
            }
        }
    })
}

/// Schema for market_offline_search tool
pub fn market_offline_search_schema() -> Value {
    json!({
        "type": "object",
        "required": ["query"],
        "properties": {
            "query": {
                "type": "string",
                "description": "Search query string"
            },
            "category": {
                "type": "string",
                "description": "Filter by category"
            },
            "limit": {
                "type": "number",
                "description": "Maximum number of results",
                "default": 10
            }
        }
    })
}

/// Schema for market_cache_status tool
pub fn market_cache_status_schema() -> Value {
    json!({
        "type": "object",
        "properties": {}
    })
}

/// Schema for market_sync tool
pub fn market_sync_schema() -> Value {
    json!({
        "type": "object",
        "properties": {
            "category": {
                "type": "string",
                "description": "Sync specific category only"
            },
            "force": {
                "type": "boolean",
                "description": "Force sync even if cache is fresh",
                "default": false
            },
            "dry_run": {
                "type": "boolean",
                "description": "Show what would be synced without performing sync",
                "default": false
            }
        }
    })
}

/// Schema for graph_query tool
pub fn graph_query_schema() -> Value {
    json!({
        "type": "object",
        "required": ["sparql"],
        "properties": {
            "sparql": {
                "type": "string",
                "description": "SPARQL query string"
            },
            "graph": {
                "type": "string",
                "description": "Named graph IRI (optional)"
            }
        }
    })
}

/// Schema for graph_load tool
pub fn graph_load_schema() -> Value {
    json!({
        "type": "object",
        "required": ["file"],
        "properties": {
            "file": {
                "type": "string",
                "description": "Path to RDF file (Turtle, N-Triples, etc.)"
            },
            "graph": {
                "type": "string",
                "description": "Target named graph IRI"
            },
            "format": {
                "type": "string",
                "description": "RDF format (turtle, ntriples, rdfxml)",
                "enum": ["turtle", "ntriples", "rdfxml", "jsonld"]
            }
        }
    })
}

/// Schema for graph_export tool
pub fn graph_export_schema() -> Value {
    json!({
        "type": "object",
        "required": ["output"],
        "properties": {
            "output": {
                "type": "string",
                "description": "Output file path"
            },
            "graph": {
                "type": "string",
                "description": "Named graph IRI to export"
            },
            "format": {
                "type": "string",
                "description": "Output format",
                "enum": ["turtle", "ntriples", "rdfxml", "jsonld"]
            }
        }
    })
}

/// Schema for template_create tool
pub fn template_create_schema() -> Value {
    json!({
        "type": "object",
        "required": ["name", "content"],
        "properties": {
            "name": {
                "type": "string",
                "description": "Template name"
            },
            "content": {
                "type": "string",
                "description": "Template content (Handlebars syntax)"
            },
            "description": {
                "type": "string",
                "description": "Template description"
            },
            "tags": {
                "type": "array",
                "items": { "type": "string" },
                "description": "Template tags"
            }
        }
    })
}

/// Schema for template_validate tool
pub fn template_validate_schema() -> Value {
    json!({
        "type": "object",
        "required": ["template"],
        "properties": {
            "template": {
                "type": "string",
                "description": "Template name or path to validate"
            }
        }
    })
}

/// Schema for hook_register tool
pub fn hook_register_schema() -> Value {
    json!({
        "type": "object",
        "required": ["event", "command"],
        "properties": {
            "event": {
                "type": "string",
                "description": "Hook event type",
                "enum": ["pre_gen", "post_gen", "pre_apply", "post_apply"]
            },
            "command": {
                "type": "string",
                "description": "Command to execute"
            },
            "name": {
                "type": "string",
                "description": "Hook name"
            }
        }
    })
}

/// Schema for ai_generate_template tool
pub fn ai_generate_template_schema() -> Value {
    json!({
        "type": "object",
        "required": ["description"],
        "properties": {
            "description": {
                "type": "string",
                "description": "Natural language description of the template to generate"
            },
            "provider": {
                "type": "string",
                "description": "AI provider to use (ollama, openai, anthropic)",
                "default": "ollama"
            },
            "output_file": {
                "type": "string",
                "description": "Optional output file path for the generated template"
            },
            "validate": {
                "type": "boolean",
                "description": "Whether to validate and improve the generated template",
                "default": false
            }
        }
    })
}

/// Schema for ai_generate_sparql tool
pub fn ai_generate_sparql_schema() -> Value {
    json!({
        "type": "object",
        "required": ["description", "graph_file"],
        "properties": {
            "description": {
                "type": "string",
                "description": "Natural language description of the SPARQL query intent"
            },
            "graph_file": {
                "type": "string",
                "description": "Path to RDF graph file for context"
            },
            "provider": {
                "type": "string",
                "description": "AI provider to use (ollama, openai, anthropic)",
                "default": "ollama"
            },
            "output_file": {
                "type": "string",
                "description": "Optional output file path for the generated SPARQL query"
            }
        }
    })
}

/// Schema for ai_generate_ontology tool
pub fn ai_generate_ontology_schema() -> Value {
    json!({
        "type": "object",
        "required": ["description"],
        "properties": {
            "description": {
                "type": "string",
                "description": "Domain description for ontology generation"
            },
            "provider": {
                "type": "string",
                "description": "AI provider to use (ollama, openai, anthropic)",
                "default": "ollama"
            },
            "output_file": {
                "type": "string",
                "description": "Optional output file path for the generated ontology"
            },
            "requirements": {
                "type": "array",
                "items": { "type": "string" },
                "description": "Additional requirements for the ontology"
            }
        }
    })
}

/// Schema for ai_generate_project tool
pub fn ai_generate_project_schema() -> Value {
    json!({
        "type": "object",
        "required": ["description", "name"],
        "properties": {
            "description": {
                "type": "string",
                "description": "Natural language description of the project"
            },
            "name": {
                "type": "string",
                "description": "Project name"
            },
            "provider": {
                "type": "string",
                "description": "AI provider to use (ollama, openai, anthropic)",
                "default": "ollama"
            },
            "language": {
                "type": "string",
                "description": "Programming language (rust, python, javascript, go)",
                "default": "rust"
            },
            "framework": {
                "type": "string",
                "description": "Web framework to use (axum, fastapi, express, gin)"
            },
            "output_dir": {
                "type": "string",
                "description": "Output directory for generated project"
            }
        }
    })
}

/// Schema for ai_extend_graph tool
pub fn ai_extend_graph_schema() -> Value {
    json!({
        "type": "object",
        "required": ["graph_file", "description"],
        "properties": {
            "graph_file": {
                "type": "string",
                "description": "Path to existing RDF graph file to extend"
            },
            "description": {
                "type": "string",
                "description": "Description of new knowledge to add to the graph"
            },
            "provider": {
                "type": "string",
                "description": "AI provider to use (ollama, openai, anthropic)",
                "default": "ollama"
            }
        }
    })
}

/// Schema for ai_validate_and_improve tool
pub fn ai_validate_and_improve_schema() -> Value {
    json!({
        "type": "object",
        "required": ["content"],
        "properties": {
            "content": {
                "type": "string",
                "description": "Content to validate and improve (code or template)"
            },
            "content_type": {
                "type": "string",
                "description": "Type of content (code, template, documentation)",
                "default": "code"
            },
            "provider": {
                "type": "string",
                "description": "AI provider to use (ollama, openai, anthropic)",
                "default": "ollama"
            },
            "max_iterations": {
                "type": "number",
                "description": "Maximum validation iterations",
                "default": 3,
                "minimum": 1,
                "maximum": 10
            }
        }
    })
}

/// Schema for ai_list_providers tool
pub fn ai_list_providers_schema() -> Value {
    json!({
        "type": "object",
        "properties": {}
    })
}
