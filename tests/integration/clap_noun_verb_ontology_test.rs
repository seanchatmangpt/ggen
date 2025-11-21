//! Integration tests for clap-noun-verb marketplace template with ontology-driven generation
//!
//! Chicago TDD: Real objects, full workflow, no mocks
//! Tests that templates extract nouns and verbs from RDF ontology instead of hardcoding

use assert_fs::TempDir;
use chicago_tdd_tools::test;
use ggen_core::{Graph, Template};
use std::fs;
use std::path::Path;
use tera::Context;

/// Helper to create a test ontology file
fn create_test_ontology(temp_dir: &TempDir) -> std::path::PathBuf {
    let ontology_path = temp_dir.path().join("test-cli.ttl");
    let ontology_content = r#"@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .
@prefix cli: <http://ggen.dev/schema/cli#> .
@prefix cnv: <http://ggen.dev/schema/clap-noun-verb#> .
@prefix ex: <http://ggen.dev/projects/test-cli#> .

ex:TestCliProject a cli:CliProject ;
    cli:hasName "test-cli" ;
    cli:hasVersion "0.1.0" ;
    cli:hasDescription "Test CLI for ontology-driven generation" ;
    cli:hasAuthors "Test User <test@example.com>" ;
    cli:hasEdition "2021" ;
    cli:hasLicense "MIT" ;
    cli:hasNoun ex:ResourceNoun ;
    cli:hasDependency ex:ClapNounVerbDep, ex:ClapNounVerbMacrosDep, ex:SerdeDep, ex:SerdeJsonDep .

ex:ClapNounVerbDep a cnv:Dependency ;
    cnv:depName "clap-noun-verb" ;
    cnv:depVersion "3.7.1" .

ex:ClapNounVerbMacrosDep a cnv:Dependency ;
    cnv:depName "clap-noun-verb-macros" ;
    cnv:depVersion "3.7.1" .

ex:SerdeDep a cnv:Dependency ;
    cnv:depName "serde" ;
    cnv:depVersion "1.0" ;
    cnv:depFeatures "derive" .

ex:SerdeJsonDep a cnv:Dependency ;
    cnv:depName "serde_json" ;
    cnv:depVersion "1.0" .

ex:ResourceNoun a cnv:Noun ;
    cnv:nounName "resource" ;
    cnv:nounDescription "Manage resources" ;
    cnv:nounModulePath "cmds::resource" ;
    cnv:hasVerb ex:ResourceCreate, ex:ResourceList .

ex:ResourceCreate a cnv:Verb ;
    cnv:verbName "create" ;
    cnv:verbDescription "Create a new resource" ;
    cnv:hasArgument ex:CreateNameArg, ex:CreateDescArg .

ex:CreateNameArg a cnv:Argument ;
    cnv:argName "name" ;
    cnv:argLong "name" ;
    cnv:argShort "n" ;
    cnv:argHelp "Name of the resource" ;
    cnv:argRequired true ;
    cnv:hasType ex:StringType .

ex:CreateDescArg a cnv:Argument ;
    cnv:argName "description" ;
    cnv:argLong "description" ;
    cnv:argShort "d" ;
    cnv:argHelp "Description of the resource" ;
    cnv:argRequired false ;
    cnv:hasType ex:OptionStringType .

ex:ResourceList a cnv:Verb ;
    cnv:verbName "list" ;
    cnv:verbDescription "List all resources" ;
    cnv:hasArgument ex:ListFilterArg .

ex:ListFilterArg a cnv:Argument ;
    cnv:argName "filter" ;
    cnv:argLong "filter" ;
    cnv:argShort "f" ;
    cnv:argHelp "Filter by name pattern" ;
    cnv:argRequired false ;
    cnv:hasType ex:OptionStringType .

ex:StringType a cnv:ArgumentType ;
    cnv:typeName "String" .

ex:OptionStringType a cnv:ArgumentType ;
    cnv:typeName "Option<String>" .
"#;
    #[allow(clippy::expect_used)]
    fs::write(&ontology_path, ontology_content).expect("Failed to write ontology file");
    ontology_path
}

#[test]
fn test_ontology_driven_cargo_toml_generation() -> Result<(), Box<dyn std::error::Error>> {
    let temp_dir = TempDir::new()?;
    let ontology_path = create_test_ontology(&temp_dir);

    // Load the Cargo.toml template
    let template_path = Path::new("templates/cli/noun-verb-cli/templates/scaffold/Cargo.toml.tmpl");
    let template_content = fs::read_to_string(template_path)?;
    
    let mut template = Template::parse(&template_content)?;
    let mut graph = Graph::new()?;
    let mut tera = tera::Tera::new("templates/**/*.tmpl")?;
    
    // Create context with project variables
    let mut vars = Context::new();
    vars.insert("project_name", "test-cli");
    vars.insert("version", "0.1.0");
    vars.insert("description", "Test CLI");
    vars.insert("authors", &vec!["Test User <test@example.com>"]);

    // Render template with RDF
    let rendered = template.render_with_rdf(
        vec![ontology_path],
        &mut graph,
        &mut tera,
        &vars,
        template_path,
    )?;

    // Verify dependencies are extracted from ontology
    assert!(rendered.contains("clap-noun-verb = \"3.7.1\""));
    assert!(rendered.contains("clap-noun-verb-macros = \"3.7.1\""));
    assert!(rendered.contains("serde = { version = \"1.0\""));
    assert!(rendered.contains("serde_json = \"1.0\""));
    
    // Verify SPARQL results were populated
    assert!(template.front.sparql_results.contains_key("dependencies"));
    
    Ok(())
}

#[test]
fn test_ontology_driven_nouns_extraction() -> Result<(), Box<dyn std::error::Error>> {
    let temp_dir = TempDir::new()?;
    let ontology_path = create_test_ontology(&temp_dir);

    // Test cmds-mod.rs template
    let template_content = r#"---
to: "{{ project_name }}/src/cmds/mod.rs"
sparql:
  nouns: |
    PREFIX cli: <http://ggen.dev/schema/cli#>
    PREFIX cnv: <http://ggen.dev/schema/clap-noun-verb#>
    SELECT ?nounName ?nounDescription ?modulePath
    WHERE {
      ?project a cli:CliProject ;
               cli:hasNoun ?noun .
      ?noun cnv:nounName ?nounName ;
            cnv:nounDescription ?nounDescription ;
            cnv:nounModulePath ?modulePath .
    }
    ORDER BY ?nounName
---
//! CLI command definitions
//! Commands are auto-discovered by clap-noun-verb from #[verb] attributes

{% for noun in sparql_results.nouns %}pub mod {{ noun.nounName }};
{% endfor %}
"#;

    let mut template = Template::parse(template_content)?;
    let mut graph = Graph::new()?;
    let mut tera = tera::Tera::new("templates/**/*.tmpl")?;
    let vars = Context::new();

    let rendered = template.render_with_rdf(
        vec![ontology_path],
        &mut graph,
        &mut tera,
        &vars,
        Path::new("test.tmpl"),
    )?;

    // Verify noun is extracted from ontology (not hardcoded)
    assert!(rendered.contains("pub mod resource"));
    assert!(!rendered.contains("pub mod user")); // Should not have hardcoded nouns
    
    // Verify SPARQL results contain nouns
    assert!(template.front.sparql_results.contains_key("nouns"));
    
    Ok(())
}

#[test]
fn test_ontology_driven_verbs_extraction() -> Result<(), Box<dyn std::error::Error>> {
    let temp_dir = TempDir::new()?;
    let ontology_path = create_test_ontology(&temp_dir);

    // Test noun/mod.rs template
    let template_content = r#"---
to: "{{ project_name }}/src/cmds/{{ noun }}/mod.rs"
sparql:
  verbs: |
    PREFIX cnv: <http://ggen.dev/schema/clap-noun-verb#>
    SELECT ?verbName ?verbDescription ?alias
    WHERE {
      ?noun cnv:nounName "{{ noun }}" ;
            cnv:hasVerb ?verb .
      ?verb cnv:verbName ?verbName ;
            cnv:verbDescription ?verbDescription .
      OPTIONAL { ?verb cnv:verbAlias ?alias }
    }
    ORDER BY ?verbName
---
//! Manage resources
//! Noun name "{{ noun }}" is auto-inferred from directory name by clap-noun-verb

{% for verb in sparql_results.verbs %}pub mod {{ verb.verbName }};
{% endfor %}
"#;

    let mut template = Template::parse(template_content)?;
    let mut graph = Graph::new()?;
    let mut tera = tera::Tera::new("templates/**/*.tmpl")?;
    
    let mut vars = Context::new();
    vars.insert("noun", "resource");

    let rendered = template.render_with_rdf(
        vec![ontology_path],
        &mut graph,
        &mut tera,
        &vars,
        Path::new("test.tmpl"),
    )?;

    // Verify verbs are extracted from ontology (not hardcoded)
    assert!(rendered.contains("pub mod create"));
    assert!(rendered.contains("pub mod list"));
    // Should not have hardcoded verbs like "get", "update", "delete" if not in ontology
    assert!(!rendered.contains("pub mod get"));
    assert!(!rendered.contains("pub mod update"));
    assert!(!rendered.contains("pub mod delete"));
    
    // Verify SPARQL results contain verbs
    assert!(template.front.sparql_results.contains_key("verbs"));
    
    Ok(())
}
