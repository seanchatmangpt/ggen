# Knowledge Hooks Guide

## Overview

Knowledge Hooks provide semantic understanding to ChatMan CLI through RDF ontologies and SPARQL queries.

## What are Knowledge Hooks?

Knowledge Hooks integrate structured knowledge graphs into conversation flows:

- **RDF Ontologies**: Define semantic models
- **SPARQL Queries**: Query and reason over knowledge
- **Context Enhancement**: Enrich responses with domain knowledge
- **Pattern Matching**: Identify conversation patterns

## Using Knowledge Hooks

### Basic Usage

```rust
use chatman_cli::{ChatManager, Config, KnowledgeHook};

let mut manager = ChatManager::new(Config::default())?;

// Add hook from ontology file
let hook = KnowledgeHook::from_ontology("rdf/ontology.ttl")?;
manager.add_hook(hook)?;
```

### Ontology Structure

The ChatMan ontology defines:

1. **Core Classes**:
   - `chatman:Conversation` - Conversation sessions
   - `chatman:Message` - Individual messages
   - `chatman:User` / `chatman:Assistant` - Participants
   - `chatman:KnowledgeHook` - Integration points

2. **Properties**:
   - `chatman:hasMessage` - Links conversations to messages
   - `chatman:hasContent` - Message text content
   - `chatman:hasRole` - Sender role
   - `chatman:hasTimestamp` - Message timing

3. **AI Providers**:
   - `chatman:OpenAI` - OpenAI GPT models
   - `chatman:Anthropic` - Anthropic Claude models
   - `chatman:LocalLLM` - Local model support

### SPARQL Queries

ChatMan includes pre-built queries:

#### Get Conversation Context

```sparql
PREFIX chatman: <http://ggen.ai/ontology/chatman#>

SELECT ?message ?content ?role ?timestamp
WHERE {
    ?conversation chatman:hasMessage ?message .
    ?message chatman:hasContent ?content ;
             chatman:hasRole ?role ;
             chatman:hasTimestamp ?timestamp .
}
ORDER BY ?timestamp
```

#### Find Related Topics

```sparql
PREFIX chatman: <http://ggen.ai/ontology/chatman#>

SELECT DISTINCT ?topic ?message
WHERE {
    ?message chatman:hasContent ?content .
    ?message chatman:relatesToTopic ?topic .
    FILTER(CONTAINS(?content, "rust"))
}
```

## Custom Knowledge Hooks

Create custom hooks for your domain:

```rust
// Define custom ontology
let ontology = r#"
@prefix my: <http://example.com/ontology#> .
@prefix chatman: <http://ggen.ai/ontology/chatman#> .

my:TechnicalQuestion a owl:Class ;
    rdfs:subClassOf chatman:Message .
"#;

// Load custom hook
let hook = KnowledgeHook::from_string(ontology)?;
manager.add_hook(hook)?;
```

## Benefits

1. **Semantic Understanding**: Go beyond keyword matching
2. **Structured Knowledge**: Leverage domain expertise
3. **Query Flexibility**: SPARQL provides powerful querying
4. **Integration**: Connect to external knowledge bases
5. **Reasoning**: Infer new facts from existing knowledge

## Best Practices

1. **Modular Ontologies**: Keep ontologies focused and reusable
2. **Query Optimization**: Index frequently queried properties
3. **Version Control**: Track ontology changes with git
4. **Documentation**: Document custom classes and properties
5. **Testing**: Validate SPARQL queries with test data
