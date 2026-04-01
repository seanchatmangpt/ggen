# SPARQL Query Guide

How SPARQL CONSTRUCT queries extract normalized context from RDF ontologies.

## Overview

The extraction stage (μ₂) uses a SPARQL CONSTRUCT query to transform raw RDF into a normalized graph structure. This normalized graph is then serialized to JSON and passed to Tera templates.

**Query file:** `crates/ggen-core/queries/mcp/extract-mcp-context.rq`

## Why CONSTRUCT vs SELECT?

**CONSTRUCT** returns an RDF graph (triples), while **SELECT** returns variable bindings.

For code generation, CONSTRUCT is better because:
- ✅ Produces structured, predictable output
- ✅ Handles missing data gracefully with COALESCE
- ✅ Normalizes URI structure
- ✅ Easier to serialize to JSON

## Query Structure

The query has 5 main sections:

```sparql
PREFIX mcp: <http://ggen.dev/mcp#>
PREFIX ctx: <http://ggen.dev/mcp/context#>
PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>

CONSTRUCT {
  # Server metadata
  ctx:server a mcp:McpServer ;
    mcp:serverName ?server_name ;
    mcp:serverVersion ?server_version ;
    mcp:hasCapabilitySet ctx:server/capabilities .

  # Capabilities
  ctx:server/capabilities mcp:tools ?has_tools ;
    mcp:resources ?has_resources .

  # Tools (one per tool)
  ctx:tool/{tool_name} a mcp:Tool ;
    mcp:name ?tool_name ;
    mcp:description ?tool_desc .

  # Tool arguments (one per argument)
  ctx:tool/{tool_name}/arg/{arg_name} a mcp:ToolArgument ;
    mcp:argumentName ?arg_name ;
    mcp:argumentType ?arg_type .
}
WHERE {
  # Match server
  ?server a mcp:McpServer ;
    mcp:serverName ?server_name ;
    OPTIONAL { ?server mcp:serverVersion ?server_version }
    OPTIONAL { ?server mcp:hasCapabilitySet/?mcp:tools ?has_tools }

  # Match tools
  OPTIONAL {
    ?server mcp:hasTool ?tool .
    ?tool mcp:name ?tool_name .
    OPTIONAL { ?tool mcp:description ?tool_desc }

    # Match tool arguments
    OPTIONAL {
      ?tool mcp:hasArgument ?arg .
      ?arg mcp:argumentName ?arg_name .
      ?arg mcp:argumentType ?arg_type .
    }
  }

  # Provide defaults
  BIND(COALESCE(?server_version, "1.0.0") as ?server_version_final)
  BIND(COALESCE(?has_tools, false) as ?has_tools_final)
}
```

## Normalization Patterns

### 1. Flat URI Structure

Instead of hierarchical RDF:
```
?server → mcp:hasTool → ?tool → mcp:hasArgument → ?arg
```

Use flat URIs:
```
ctx:tool/{name}
ctx:tool/{name}/arg/{arg_name}
```

**Benefits:**
- Predictable iteration in templates
- Easy to serialize to JSON
- No nested SPARQL queries needed

### 2. COALESCE for Defaults

```sparql
BIND(COALESCE(?version, "1.0.0") as ?version_final)
BIND(COALESCE(?required, false) as ?required_final)
```

Ensures templates always have values, even if ontology is incomplete.

### 3. URI Encoding

```sparql
BIND(REPLACE(STR(?tool_name), "[^a-zA-Z0-9_]", "_") as ?tool_name_safe)
```

Handles special characters in names for valid URIs.

### 4. Deterministic Node IDs

```sparql
BNODE(?tool_name)
```

Creates blank nodes with deterministic IDs based on name.

## Query Sections

### Server Metadata

```sparql
CONSTRUCT {
  ctx:server a mcp:McpServer ;
    mcp:serverName ?server_name ;
    mcp:serverVersion ?server_version ;
    mcp:serverDescription ?server_desc ;
    mcp:hasProtocolVersion ?protocol_version ;
    mcp:hasTransport ?transport_iri ;
    mcp:hasCapabilitySet ctx:server/capabilities .
}
WHERE {
  ?server a mcp:McpServer ;
    mcp:serverName ?server_name ;
    OPTIONAL { ?server mcp:serverVersion ?server_version }
    OPTIONAL { ?server mcp:serverDescription ?server_desc }
    OPTIONAL { ?server mcp:hasProtocolVersion ?protocol_version }
    OPTIONAL { ?server mcp:hasTransport ?transport_iri }

    BIND(COALESCE(?server_version, "1.0.0") as ?server_version_final)
    BIND(COALESCE(?protocol_version, "2025-11-25") as ?protocol_version_final)
}
```

### Capabilities

```sparql
CONSTRUCT {
  ctx:server/capabilities a mcp:CapabilitySet ;
    mcp:tools ?has_tools ;
    mcp:resources ?has_resources ;
    mcp:prompts ?has_prompts ;
    mcp:completions ?has_completions ;
    mcp:logging ?has_logging .
}
WHERE {
  ?server mcp:hasCapabilitySet ?caps .
  OPTIONAL { ?caps mcp:tools ?has_tools }
  OPTIONAL { ?caps mcp:resources ?has_resources }
  OPTIONAL { ?caps mcp:prompts ?has_prompts }
  OPTIONAL { ?caps mcp:completions ?has_completions }
  OPTIONAL { ?caps mcp:logging ?has_logging }

  BIND(COALESCE(?has_tools, false) as ?has_tools_final)
  BIND(COALESCE(?has_resources, false) as ?has_resources_final)
}
```

### Tools

```sparql
CONSTRUCT {
  ctx:tool/{tool_name} a mcp:Tool ;
    mcp:name ?tool_name ;
    mcp:description ?tool_desc ;
    mcp:implementedBy ?impl_path .
}
WHERE {
  ?server mcp:hasTool ?tool .
  ?tool mcp:name ?tool_name .
  OPTIONAL { ?tool mcp:description ?tool_desc }
  OPTIONAL { ?tool mcp:implementedBy ?impl_path }

  # URI encoding for safe names
  BIND(REPLACE(STR(?tool_name), "[^a-zA-Z0-9_]", "_") as ?tool_name_safe)
}
```

### Tool Arguments

```sparql
CONSTRUCT {
  ctx:tool/{tool_name}/arg/{arg_name} a mcp:ToolArgument ;
    mcp:argumentName ?arg_name ;
    mcp:argumentDescription ?arg_desc ;
    mcp:argumentType ?arg_type ;
    mcp:isRequired ?arg_required .
}
WHERE {
  ?server mcp:hasTool ?tool .
  ?tool mcp:name ?tool_name .
  ?tool mcp:hasArgument ?arg .
  ?arg mcp:argumentName ?arg_name .
  OPTIONAL { ?arg mcp:argumentDescription ?arg_desc }
  ?arg mcp:argumentType ?arg_type .
  OPTIONAL { ?arg mcp:isRequired ?arg_required }

  BIND(COALESCE(?arg_required, false) as ?arg_required_final)

  BIND(REPLACE(STR(?tool_name), "[^a-zA-Z0-9_]", "_") as ?tool_name_safe)
  BIND(REPLACE(STR(?arg_name), "[^a-zA-Z0-9_]", "_") as ?arg_name_safe)
}
```

### Resources

```sparql
CONSTRUCT {
  ctx:resource/{uri_safe} a mcp:Resource ;
    mcp:uri ?resource_uri ;
    mcp:resourceName ?resource_name ;
    mcp:description ?resource_desc ;
    mcp:mimeType ?mime_type .
}
WHERE {
  ?server mcp:hasResource ?resource .
  ?resource mcp:uri ?resource_uri .
  OPTIONAL { ?resource mcp:resourceName ?resource_name }
  OPTIONAL { ?resource mcp:description ?resource_desc }
  OPTIONAL { ?resource mcp:mimeType ?mime_type }

  # URI encode for safe node names
  BIND(REPLACE(STR(?resource_uri), "[^a-zA-Z0-9_]", "_") as ?uri_safe)
}
```

## Custom Queries

### Adding Custom Properties

To extract a custom property from your ontology:

**1. Add to ontology:**
```turtle
:MyTool a mcp:Tool ;
    mcp:name "my_tool" ;
    my:customProperty "custom value" .
```

**2. Extend CONSTRUCT:**
```sparql
CONSTRUCT {
  ctx:tool/{tool_name} a mcp:Tool ;
    mcp:name ?tool_name ;
    my:customProperty ?custom_value .
}
WHERE {
  ?server mcp:hasTool ?tool .
  ?tool mcp:name ?tool_name .
  OPTIONAL { ?tool my:customProperty ?custom_value }
}
```

**3. Use in template:**
```tera
{% for tool in tools %}
    // {{ tool.customProperty }}
{% endfor %}
```

### Filtering Tools

To only extract tools with a specific property:

```sparql
CONSTRUCT {
  ctx:tool/{tool_name} a mcp:Tool ;
    mcp:name ?tool_name .
}
WHERE {
  ?server mcp:hasTool ?tool .
  ?tool mcp:name ?tool_name .
  ?tool my:customProperty ?custom_value .  # Only tools with this property
  FILTER(?custom_value = "special")
}
```

### Aggregation

To count tools:

```sparql
CONSTRUCT {
  ctx:server mcp:toolCount ?tool_count .
}
WHERE {
  {
    SELECT (COUNT(?tool) as ?tool_count)
    WHERE {
      ?server mcp:hasTool ?tool .
    }
  }
}
```

## Testing Queries

### 1. Test with Robin (SPARQL CLI)

```bash
# Install robin
cargo install robin

# Run query
robin query \
  --file crates/ggen-core/queries/mcp/extract-mcp-context.rq \
  examples/minimal-server.ttl
```

### 2. Test with Oxigraph

```rust
use oxigraph::store::Store;
use oxigraph::sparql::Query;

let store = Store::new();
store.load_ttl(
    "examples/minimal-server.ttl",
    None,
    None,
)?;

let query = Query::parse(
    std::fs::read_to_string("crates/ggen-core/queries/mcp/extract-mcp-context.rq")?,
    None,
)?;

let results = store.query(query)?;
println!("{:?}", results);
```

### 3. Dry-Run Generation

```bash
ggen mcp generate \
  --ontology examples/minimal-server.ttl \
  --output /tmp/test \
  --dry-run
```

This shows the JSON context without generating files.

## Common Patterns

### Handling Missing Data

```sparql
OPTIONAL { ?tool mcp:description ?desc }
BIND(COALESCE(?desc, "No description") as ?desc_final)
```

### URI Encoding

```sparql
BIND(REPLACE(STR(?name), "[^a-zA-Z0-9_]", "_") as ?name_safe)
```

### Type Coercion

```sparql
BIND(xsd:integer(?count) as ?count_int)
BIND(xsd:boolean(?flag) as ?flag_bool)
BIND(STR(?uri) as ?uri_string)
```

### Conditional Logic

```sparql
BIND(IF(?required = true, "required", "optional") as ?arg_status)
```

## Performance Tips

### 1. Use OPTIONAL for Optional Fields

```sparql
# Good
OPTIONAL { ?tool mcp:description ?desc }

# Avoid
?tool mcp:description ?desc .  # Fails if description missing
```

### 2. Use COALESCE for Defaults

```sparql
# Good
BIND(COALESCE(?version, "1.0.0") as ?version_final)

# Avoid complex FILTER logic
FILTER(bound(?version))
```

### 3. Avoid Nested Queries

```sparql
# Good
OPTIONAL {
  ?tool mcp:hasArgument ?arg .
  ?arg mcp:argumentName ?arg_name .
}

# Avoid subqueries
{
  SELECT ?tool (GROUP_CONCAT(?arg_name) as ?args)
  WHERE {
    ?tool mcp:hasArgument ?arg .
    ?arg mcp:argumentName ?arg_name .
  }
}
```

## See Also

- [Code Generation Guide](../03-code-generation/) - Pipeline internals
- [RDF Schema Reference](../02-rdf-schema/) - Ontology classes
- [SPARQL 1.2 Spec](https://www.w3.org/TR/sparql11-query/) - Official spec
