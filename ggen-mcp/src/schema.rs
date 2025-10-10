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
