use anyhow::Result;
use std::collections::BTreeMap;
use tera::Context;

use crate::graph::{build_prolog, Graph};
use crate::pipeline::Pipeline;
use crate::template::Template;

#[cfg(test)]
mod e2e_tests {
    use super::*;

    #[test]
    fn test_graph_operations() -> Result<()> {
        let graph = Graph::new()?;
        
        // Test basic turtle insertion
        let ttl = r#"
            @prefix ex: <http://example.org/> .
            @prefix xsd: <http://www.w3.org/2001/XMLSchema#> .
            ex:alice ex:knows ex:bob .
            ex:bob ex:age "30"^^xsd:integer .
        "#;
        graph.insert_turtle(ttl)?;
        
        // Test quad insertion
        graph.insert_quad(
            "http://example.org/charlie",
            "http://example.org/works",
            "http://example.org/company"
        )?;
        
        // Test querying
        let results = graph.query("SELECT ?s ?p ?o WHERE { ?s ?p ?o }")?;
        if let oxigraph::sparql::QueryResults::Solutions(solutions) = results {
            let count = solutions.count();
            assert!(count >= 3, "Expected at least 3 triples, got {}", count);
        } else {
            panic!("Expected solutions");
        }
        
        // Test prolog building
        let mut prefixes = BTreeMap::new();
        prefixes.insert("ex".to_string(), "http://example.org/".to_string());
        let prolog = build_prolog(&prefixes, Some("http://example.org/"));
        assert!(prolog.contains("BASE <http://example.org/>"));
        assert!(prolog.contains("PREFIX ex: <http://example.org/>"));
        
        // Test query with prolog
        let _results = graph.query_with_prolog(
            "SELECT ?s WHERE { ?s a ex:Person }",
            &prefixes,
            Some("http://example.org/")
        )?;
        
        // Test graph state
        assert!(!graph.is_empty());
        assert!(graph.len() >= 3);
        
        Ok(())
    }

    #[test]
    fn test_template_parsing_and_rendering() -> Result<()> {
        let input = r#"---
to: example.rs
force: true
prefixes:
  ex: "http://example.org/"
rdf:
  - |
    @prefix ex: <http://example.org/> .
    ex:{{ name }} ex:knows ex:friend .
sparql:
  find_friends: "SELECT ?friend WHERE { ex:{{ name }} ex:knows ?friend }"
vars:
  name: "alice"
---
Hello {{ name }}!
This is a test template.
"#;

        // Test parsing
        let mut template = Template::parse(input)?;
        // Initially, frontmatter is not rendered, so to should be None
        assert_eq!(template.front.to, None);
        
        // Test frontmatter rendering
        let mut tera = tera::Tera::default();
        let mut vars = Context::new();
        vars.insert("name", "bob");
        
        template.render_frontmatter(&mut tera, &vars)?;
        // After rendering, frontmatter should be populated
        assert_eq!(template.front.to, Some("example.rs".to_string()));
        assert!(template.front.force);
        assert_eq!(template.front.prefixes.get("ex"), Some(&"http://example.org/".to_string()));
        assert_eq!(template.front.vars.get("name"), Some(&"alice".to_string()));
        
        // Test body rendering
        let rendered = template.render(&mut tera, &vars)?;
        assert!(rendered.contains("Hello bob!"));
        assert!(rendered.contains("This is a test template."));
        
        Ok(())
    }

    #[test]
    fn test_pipeline_end_to_end() -> Result<()> {
        let input = r#"---
to: generated.rs
base: "http://example.org/"
prefixes:
  ex: "http://example.org/"
  rdf: "http://www.w3.org/1999/02/22-rdf-syntax-ns#"
  xsd: "http://www.w3.org/2001/XMLSchema#"
rdf:
  - |
    @prefix ex: <http://example.org/> .
    ex:{{ name }} ex:knows ex:friend .
    ex:friend ex:name "Bob" .
    ex:friend ex:age "25"^^xsd:integer .
sparql:
  default: "SELECT ?s ?p ?o WHERE { ?s ?p ?o }"
  find_friend: "SELECT ?name WHERE { ex:{{ name }} ex:knows ?friend . ?friend ex:name ?name }"
vars:
  name: "alice"
---
// Generated code for {{ name }}
use std::collections::HashMap;

pub struct Person {
    pub name: String,
    pub age: u32,
}

impl Person {
    pub fn new(name: String, age: u32) -> Self {
        Self { name, age }
    }
}

// Environment info
// Generated in: {{ cwd }}
// User: {{ env.USER }}

// SPARQL query results
{% set all_triples = sparql(query="SELECT ?s ?p ?o WHERE { ?s ?p ?o }") %}
// Found {{ all_triples | length }} triples

{% set friend_name = sparql(query="SELECT ?name WHERE { ex:alice ex:knows ?friend . ?friend ex:name ?name }", var="name") %}
// Friend name: {{ friend_name }}

// Local name extraction
{% set local_name = local(iri="<http://example.org/alice>") %}
// Local name: {{ local_name }}

pub fn main() {
    let person = Person::new("{{ name }}".to_string(), 25);
    println!("Hello, {}!", person.name);
}
"#;

        let mut vars = Context::new();
        vars.insert("name", "alice");
        
        let mut pipeline = Pipeline::new(false)?;
        let output = pipeline.run(input, vars)?;
        
        // Verify the output contains expected elements
        assert!(output.contains("// Generated code for alice"));
        assert!(output.contains("pub struct Person"));
        assert!(output.contains("impl Person"));
        assert!(output.contains("pub fn main()"));
        assert!(output.contains("println!(\"Hello, {}!\", person.name)"));
        
        // Verify environment variables are injected
        assert!(output.contains("// Generated in:"));
        assert!(output.contains("// User:"));
        
        // Verify SPARQL function results
        assert!(output.contains("// Found"));
        assert!(output.contains("triples"));
        assert!(output.contains("// Friend name:"));
        assert!(output.contains("\"Bob\""));
        
        // Verify local function results
        assert!(output.contains("// Local name:"));
        assert!(output.contains("alice"));
        
        Ok(())
    }

    #[test]
    fn test_complex_template_with_multiple_rdf_blocks() -> Result<()> {
        let input = r#"---
to: complex.rs
base: "http://example.org/"
prefixes:
  ex: "http://example.org/"
  foaf: "http://xmlns.com/foaf/0.1/"
  rdfs: "http://www.w3.org/2000/01/rdf-schema#"
rdf_inline:
  - |
    @prefix ex: <http://example.org/> .
    @prefix foaf: <http://xmlns.com/foaf/0.1/> .
    @prefix xsd: <http://www.w3.org/2001/XMLSchema#> .
    ex:{{ name }} foaf:name "{{ name | title }}" .
    ex:{{ name }} foaf:age "{{ age }}"^^xsd:integer .
rdf:
  - |
    ex:{{ name }} ex:works ex:company .
    ex:company rdfs:label "Tech Corp" .
sparql:
  get_person: "SELECT ?name ?age WHERE { ex:{{ name }} foaf:name ?name . ex:{{ name }} foaf:age ?age }"
  get_company: "SELECT ?company WHERE { ex:{{ name }} ex:works ?company }"
vars:
  name: "alice"
  age: "30"
---
// Complex template with multiple RDF sources
{% set person_info = sparql(query="SELECT ?name ?age WHERE { ex:alice foaf:name ?name . ex:alice foaf:age ?age }") %}
{% set company_info = sparql(query="SELECT ?company WHERE { ex:alice ex:works ?company }") %}

pub struct Employee {
    pub name: String,
    pub age: u32,
    pub company: String,
}

impl Employee {
    pub fn from_rdf() -> Self {
        Self {
            name: "{{ name | title }}".to_string(),
            age: {{ age }},
            company: "Tech Corp".to_string(),
        }
    }
}
"#;

        let mut vars = Context::new();
        vars.insert("name", "alice");
        vars.insert("age", "30");
        
        let mut pipeline = Pipeline::new(false)?;
        let output = pipeline.run(input, vars)?;
        
        // Verify the output
        assert!(output.contains("pub struct Employee"));
        assert!(output.contains("impl Employee"));
        assert!(output.contains("name: \"Alice\""));
        assert!(output.contains("age: 30"));
        assert!(output.contains("company: \"Tech Corp\""));
        
        Ok(())
    }

    #[test]
    fn test_template_with_named_sparql_queries() -> Result<()> {
        let input = r#"---
to: queries.rs
prefixes:
  ex: "http://example.org/"
rdf:
  - |
    @prefix ex: <http://example.org/> .
    ex:alice ex:knows ex:bob .
    ex:bob ex:knows ex:charlie .
    ex:charlie ex:knows ex:alice .
sparql:
  find_friends: "SELECT ?friend WHERE { ex:alice ex:knows ?friend }"
  find_friends_of_friends: "SELECT ?friend WHERE { ex:alice ex:knows ?intermediate . ?intermediate ex:knows ?friend }"
  count_relationships: "SELECT (COUNT(?s) as ?count) WHERE { ?s ex:knows ?o }"
---
// Testing named SPARQL queries
{% set friends = sparql(query="SELECT ?friend WHERE { ex:alice ex:knows ?friend }") %}
{% set friends_of_friends = sparql(query="SELECT ?friend WHERE { ex:alice ex:knows ?intermediate . ?intermediate ex:knows ?friend }") %}
{% set relationship_count = sparql(query="SELECT (COUNT(?s) as ?count) WHERE { ?s ex:knows ?o }", var="count") %}

pub fn analyze_relationships() {
    println!("Direct friends: {{ friends | length }}");
    println!("Friends of friends: {{ friends_of_friends | length }}");
    println!("Total relationships: {{ relationship_count }}");
}
"#;

        let vars = Context::new();
        let mut pipeline = Pipeline::new(false)?;
        let output = pipeline.run(input, vars)?;
        
        // Verify the output contains the analysis function
        assert!(output.contains("pub fn analyze_relationships()"));
        assert!(output.contains("println!"));
        assert!(output.contains("Direct friends:"));
        assert!(output.contains("Friends of friends:"));
        assert!(output.contains("Total relationships:"));
        
        Ok(())
    }

    #[test]
    fn test_error_handling_invalid_sparql() -> Result<()> {
        let input = r#"---
to: error.rs
prefixes:
  ex: "http://example.org/"
rdf:
  - |
    @prefix ex: <http://example.org/> .
    ex:alice ex:knows ex:bob .
---
// This should handle SPARQL errors gracefully
{% set invalid_result = sparql(query="INVALID SPARQL QUERY") %}
Result: {{ invalid_result }}
"#;

        let vars = Context::new();
        let mut pipeline = Pipeline::new(false)?;
        
        // This should return an error due to invalid SPARQL
        let result = pipeline.run(input, vars);
        assert!(result.is_err(), "Expected error for invalid SPARQL query");
        
        Ok(())
    }

    #[test]
    fn test_local_function_variations() -> Result<()> {
        let input = r#"---
to: local_test.rs
---
// Testing local() function with various IRI formats
{% set name1 = local(iri="<http://example.org/person#alice>") %}
{% set name2 = local(iri="http://example.org/person/alice") %}
{% set name3 = local(iri="<http://example.org/alice>") %}
{% set name4 = local(iri="http://example.org/alice") %}

pub fn test_local_names() {
    assert_eq!("alice", "{{ name1 }}");
    assert_eq!("alice", "{{ name2 }}");
    assert_eq!("alice", "{{ name3 }}");
    assert_eq!("alice", "{{ name4 }}");
}
"#;

        let vars = Context::new();
        let mut pipeline = Pipeline::new(false)?;
        let output = pipeline.run(input, vars)?;
        
        // Verify all local name extractions work
        assert!(output.contains("assert_eq!(\"alice\", \"alice\")"));
        assert!(output.contains("pub fn test_local_names()"));
        
        Ok(())
    }

    #[test]
    fn test_environment_variable_injection() -> Result<()> {
        let input = r#"---
to: env_test.rs
---
// Testing environment variable injection
// User: {{ env.USER }}
// Home: {{ env.HOME }}
// Path: {{ env.PATH }}
// CWD: {{ cwd }}

pub fn print_environment() {
    println!("User: {}", std::env::var("USER").unwrap_or_default());
    println!("Home: {}", std::env::var("HOME").unwrap_or_default());
    println!("CWD: {}", std::env::current_dir().unwrap().display());
}
"#;

        let vars = Context::new();
        let mut pipeline = Pipeline::new(false)?;
        let output = pipeline.run(input, vars)?;
        
        // Verify environment variables are injected
        assert!(output.contains("// User:"));
        assert!(output.contains("// Home:"));
        assert!(output.contains("// Path:"));
        assert!(output.contains("// CWD:"));
        assert!(output.contains("pub fn print_environment()"));
        
        Ok(())
    }

    #[test]
    fn test_template_variable_precedence() -> Result<()> {
        let input = r#"---
to: precedence.rs
vars:
  name: "frontmatter_name"
  value: "frontmatter_value"
---
// Testing variable precedence
// Context name: {{ name }}
// Context value: {{ value }}

pub fn test_precedence() {
    let context_name = "{{ name }}";
    let context_value = "{{ value }}";
    println!("Context: {} = {}", context_name, context_value);
}
"#;

        let mut vars = Context::new();
        vars.insert("name", "context_name");
        vars.insert("value", "context_value");
        
        let mut pipeline = Pipeline::new(false)?;
        let output = pipeline.run(input, vars)?;
        
        // Verify the output contains the expected content
        // The pipeline processes frontmatter vars and merges them into context
        assert!(output.contains("// Context name:"));
        assert!(output.contains("// Context value:"));
        assert!(output.contains("let context_name ="));
        assert!(output.contains("let context_value ="));
        assert!(output.contains("pub fn test_precedence()"));
        
        Ok(())
    }
}
