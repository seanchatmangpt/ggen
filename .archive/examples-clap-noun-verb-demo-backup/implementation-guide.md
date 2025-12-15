# Implementation Guide: RDF-to-CLI Generator

Step-by-step guide for implementing the RDF template system in ggen v2.

## Architecture Overview

```
┌─────────────────┐
│   sample-cli.ttl│  (User-defined CLI structure)
└────────┬────────┘
         │
         │ Parse RDF
         ▼
┌─────────────────┐
│  RDF Graph      │  (In-memory triple store)
│  (oxigraph)     │
└────────┬────────┘
         │
         │ SPARQL Queries
         ▼
┌─────────────────┐
│  Extracted Data │  (Structured Rust types)
│  - Project      │
│  - Nouns        │
│  - Verbs        │
│  - Arguments    │
└────────┬────────┘
         │
         │ Build Context
         ▼
┌─────────────────┐
│  Tera Context   │  (Template variables)
└────────┬────────┘
         │
         │ Render Templates
         ▼
┌─────────────────┐
│  Generated      │
│  Project Files  │
│  - Cargo.toml   │
│  - src/*.rs     │
│  - tests/*.rs   │
└─────────────────┘
```

## Phase 1: Core Data Structures

### 1.1 Define Rust Types

```rust
// ggen-ai/src/rdf/types.rs

use std::path::PathBuf;
use serde::{Serialize, Deserialize};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CliProject {
    pub name: String,
    pub version: String,
    pub description: String,
    pub authors: Vec<String>,
    pub edition: String,
    pub license: String,
    pub nouns: Vec<Noun>,
    pub dependencies: Vec<Dependency>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Noun {
    pub name: String,
    pub description: String,
    pub module_path: String,
    pub verbs: Vec<Verb>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Verb {
    pub name: String,
    pub description: String,
    pub alias: Option<String>,
    pub arguments: Vec<Argument>,
    pub validations: Vec<Validation>,
    pub execution_logic: Option<String>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Argument {
    pub name: String,
    pub long: Option<String>,
    pub short: Option<char>,
    pub help: String,
    pub required: bool,
    pub default: Option<String>,
    pub value_name: Option<String>,
    pub position: Option<usize>,
    pub arg_type: ArgumentType,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ArgumentType {
    pub name: String,  // "String", "PathBuf", "bool", etc.
    pub parser: Option<String>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Validation {
    pub rule: String,  // "file_exists", "regex", etc.
    pub pattern: Option<String>,
    pub message: String,
    pub arg_name: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Dependency {
    pub name: String,
    pub version: String,
    pub features: Vec<String>,
    pub optional: bool,
}
```

## Phase 2: RDF Parsing

### 2.1 Add Dependencies to Cargo.toml

```toml
[dependencies]
oxigraph = "0.4"
tera = "1.20"
serde = { version = "1.0", features = ["derive"] }
serde_json = "1.0"
anyhow = "1.0"
```

### 2.2 RDF Parser Implementation

```rust
// ggen-ai/src/rdf/parser.rs

use anyhow::Result;
use oxigraph::store::Store;
use oxigraph::model::*;
use std::path::Path;

pub struct RdfParser {
    store: Store,
}

impl RdfParser {
    pub fn new() -> Result<Self> {
        Ok(Self {
            store: Store::new()?,
        })
    }

    pub fn load_ttl(&mut self, path: &Path) -> Result<()> {
        let file = std::fs::File::open(path)?;
        self.store.load_from_reader(
            oxigraph::io::RdfFormat::Turtle,
            file,
        )?;
        Ok(())
    }

    pub fn load_schema(&mut self) -> Result<()> {
        // Load project-schema.ttl
        let schema_path = Path::new("examples/clap-noun-verb-demo/project-schema.ttl");
        self.load_ttl(schema_path)?;
        Ok(())
    }

    pub fn get_store(&self) -> &Store {
        &self.store
    }
}
```

## Phase 3: SPARQL Query Engine

### 3.1 Query Executor

```rust
// ggen-ai/src/rdf/query.rs

use anyhow::{Result, Context as _};
use oxigraph::store::Store;
use oxigraph::sparql::{QueryResults, QuerySolution};
use crate::rdf::types::*;

pub struct QueryExecutor<'a> {
    store: &'a Store,
}

impl<'a> QueryExecutor<'a> {
    pub fn new(store: &'a Store) -> Self {
        Self { store }
    }

    pub fn extract_project(&self) -> Result<CliProject> {
        let query = r#"
            PREFIX cli: <http://ggen.dev/schema/cli#>
            PREFIX ex: <http://ggen.dev/projects/example-cli#>

            SELECT ?name ?version ?description ?authors ?edition ?license
            WHERE {
                ex:MyCliProject a cli:CliProject ;
                    cli:hasName ?name ;
                    cli:hasVersion ?version ;
                    cli:hasDescription ?description ;
                    cli:hasAuthors ?authors ;
                    cli:hasEdition ?edition ;
                    cli:hasLicense ?license .
            }
        "#;

        if let QueryResults::Solutions(mut solutions) = self.store.query(query)? {
            if let Some(solution) = solutions.next() {
                let solution = solution?;
                return Ok(CliProject {
                    name: get_string(&solution, "name")?,
                    version: get_string(&solution, "version")?,
                    description: get_string(&solution, "description")?,
                    authors: vec![get_string(&solution, "authors")?],
                    edition: get_string(&solution, "edition")?,
                    license: get_string(&solution, "license")?,
                    nouns: vec![],  // Filled later
                    dependencies: vec![],  // Filled later
                });
            }
        }

        anyhow::bail!("No project found in RDF graph")
    }

    pub fn extract_nouns(&self) -> Result<Vec<Noun>> {
        let query = r#"
            PREFIX cli: <http://ggen.dev/schema/cli#>
            PREFIX cnv: <http://ggen.dev/schema/clap-noun-verb#>
            PREFIX ex: <http://ggen.dev/projects/example-cli#>

            SELECT ?noun ?nounName ?nounDescription ?modulePath
            WHERE {
                ex:MyCliProject cli:hasNoun ?noun .
                ?noun cnv:nounName ?nounName ;
                      cnv:nounDescription ?nounDescription ;
                      cnv:nounModulePath ?modulePath .
            }
            ORDER BY ?nounName
        "#;

        let mut nouns = Vec::new();

        if let QueryResults::Solutions(solutions) = self.store.query(query)? {
            for solution in solutions {
                let solution = solution?;
                let noun_uri = get_uri(&solution, "noun")?;

                nouns.push(Noun {
                    name: get_string(&solution, "nounName")?,
                    description: get_string(&solution, "nounDescription")?,
                    module_path: get_string(&solution, "modulePath")?,
                    verbs: self.extract_verbs_for_noun(&noun_uri)?,
                });
            }
        }

        Ok(nouns)
    }

    fn extract_verbs_for_noun(&self, noun_uri: &str) -> Result<Vec<Verb>> {
        let query = format!(r#"
            PREFIX cnv: <http://ggen.dev/schema/clap-noun-verb#>

            SELECT ?verb ?verbName ?verbDescription ?alias ?executionLogic
            WHERE {{
                <{noun_uri}> cnv:hasVerb ?verb .
                ?verb cnv:verbName ?verbName ;
                      cnv:verbDescription ?verbDescription .
                OPTIONAL {{ ?verb cnv:verbAlias ?alias }}
                OPTIONAL {{ ?verb cnv:executionLogic ?executionLogic }}
            }}
            ORDER BY ?verbName
        "#);

        let mut verbs = Vec::new();

        if let QueryResults::Solutions(solutions) = self.store.query(&query)? {
            for solution in solutions {
                let solution = solution?;
                let verb_uri = get_uri(&solution, "verb")?;

                verbs.push(Verb {
                    name: get_string(&solution, "verbName")?,
                    description: get_string(&solution, "verbDescription")?,
                    alias: get_optional_string(&solution, "alias"),
                    execution_logic: get_optional_string(&solution, "executionLogic"),
                    arguments: self.extract_arguments_for_verb(&verb_uri)?,
                    validations: self.extract_validations_for_verb(&verb_uri)?,
                });
            }
        }

        Ok(verbs)
    }

    fn extract_arguments_for_verb(&self, verb_uri: &str) -> Result<Vec<Argument>> {
        let query = format!(r#"
            PREFIX cnv: <http://ggen.dev/schema/clap-noun-verb#>

            SELECT ?argName ?long ?short ?help ?required ?default ?valueName ?position ?typeName
            WHERE {{
                <{verb_uri}> cnv:hasArgument ?arg .
                ?arg cnv:argName ?argName ;
                     cnv:argHelp ?help .

                OPTIONAL {{ ?arg cnv:argLong ?long }}
                OPTIONAL {{ ?arg cnv:argShort ?short }}
                OPTIONAL {{ ?arg cnv:argRequired ?required }}
                OPTIONAL {{ ?arg cnv:argDefault ?default }}
                OPTIONAL {{ ?arg cnv:argValueName ?valueName }}
                OPTIONAL {{ ?arg cnv:argPosition ?position }}

                OPTIONAL {{
                    ?arg cnv:hasType ?type .
                    ?type cnv:typeName ?typeName
                }}
            }}
            ORDER BY ?position ?argName
        "#);

        let mut arguments = Vec::new();

        if let QueryResults::Solutions(solutions) = self.store.query(&query)? {
            for solution in solutions {
                let solution = solution?;

                arguments.push(Argument {
                    name: get_string(&solution, "argName")?,
                    long: get_optional_string(&solution, "long"),
                    short: get_optional_string(&solution, "short")
                        .and_then(|s| s.chars().next()),
                    help: get_string(&solution, "help")?,
                    required: get_optional_bool(&solution, "required").unwrap_or(false),
                    default: get_optional_string(&solution, "default"),
                    value_name: get_optional_string(&solution, "valueName"),
                    position: get_optional_int(&solution, "position"),
                    arg_type: ArgumentType {
                        name: get_optional_string(&solution, "typeName")
                            .unwrap_or_else(|| "String".to_string()),
                        parser: None,
                    },
                });
            }
        }

        Ok(arguments)
    }

    fn extract_validations_for_verb(&self, verb_uri: &str) -> Result<Vec<Validation>> {
        // Similar pattern to extract_arguments_for_verb
        Ok(vec![])  // Simplified for brevity
    }

    pub fn extract_dependencies(&self) -> Result<Vec<Dependency>> {
        let query = r#"
            PREFIX cli: <http://ggen.dev/schema/cli#>
            PREFIX cnv: <http://ggen.dev/schema/clap-noun-verb#>
            PREFIX ex: <http://ggen.dev/projects/example-cli#>

            SELECT ?depName ?version ?features ?optional
            WHERE {
                ex:MyCliProject cli:hasDependency ?dep .
                ?dep cnv:depName ?depName ;
                     cnv:depVersion ?version .
                OPTIONAL { ?dep cnv:depFeatures ?features }
                OPTIONAL { ?dep cnv:depOptional ?optional }
            }
        "#;

        let mut deps = Vec::new();

        if let QueryResults::Solutions(solutions) = self.store.query(query)? {
            for solution in solutions {
                let solution = solution?;
                deps.push(Dependency {
                    name: get_string(&solution, "depName")?,
                    version: get_string(&solution, "version")?,
                    features: get_optional_string(&solution, "features")
                        .map(|s| vec![s])
                        .unwrap_or_default(),
                    optional: get_optional_bool(&solution, "optional").unwrap_or(false),
                });
            }
        }

        Ok(deps)
    }
}

// Helper functions
fn get_string(solution: &QuerySolution, var: &str) -> Result<String> {
    solution
        .get(var)
        .and_then(|term| term.as_str())
        .map(|s| s.to_string())
        .context(format!("Missing variable: {}", var))
}

fn get_optional_string(solution: &QuerySolution, var: &str) -> Option<String> {
    solution.get(var).and_then(|term| term.as_str()).map(|s| s.to_string())
}

fn get_uri(solution: &QuerySolution, var: &str) -> Result<String> {
    solution
        .get(var)
        .and_then(|term| {
            if let oxigraph::model::Term::NamedNode(node) = term {
                Some(node.as_str().to_string())
            } else {
                None
            }
        })
        .context(format!("Missing URI: {}", var))
}

fn get_optional_bool(solution: &QuerySolution, var: &str) -> Option<bool> {
    solution.get(var).and_then(|term| {
        term.as_str().and_then(|s| s.parse().ok())
    })
}

fn get_optional_int(solution: &QuerySolution, var: &str) -> Option<usize> {
    solution.get(var).and_then(|term| {
        term.as_str().and_then(|s| s.parse().ok())
    })
}
```

## Phase 4: Template Engine Integration

### 4.1 Template Renderer

```rust
// ggen-ai/src/rdf/renderer.rs

use anyhow::Result;
use tera::{Tera, Context};
use std::path::{Path, PathBuf};
use crate::rdf::types::CliProject;

pub struct TemplateRenderer {
    tera: Tera,
}

impl TemplateRenderer {
    pub fn new(template_dir: &Path) -> Result<Self> {
        let pattern = format!("{}/**/*.tmpl", template_dir.display());
        let tera = Tera::new(&pattern)?;

        Ok(Self { tera })
    }

    pub fn build_context(&self, project: &CliProject) -> Context {
        let mut context = Context::new();
        context.insert("project", project);
        context
    }

    pub fn render_file(&self, template: &str, context: &Context) -> Result<String> {
        Ok(self.tera.render(template, context)?)
    }

    pub fn render_all(&self, project: &CliProject, output_dir: &Path) -> Result<()> {
        let context = self.build_context(project);

        // Render Cargo.toml
        self.render_and_write("Cargo.toml.tmpl", &context, output_dir.join("Cargo.toml"))?;

        // Render main.rs
        std::fs::create_dir_all(output_dir.join("src"))?;
        self.render_and_write("main.rs.tmpl", &context, output_dir.join("src/main.rs"))?;

        // Render command.rs
        self.render_and_write("command.rs.tmpl", &context, output_dir.join("src/command.rs"))?;

        // Render lib.rs
        self.render_and_write("lib.rs.tmpl", &context, output_dir.join("src/lib.rs"))?;

        // Render cmds/mod.rs
        std::fs::create_dir_all(output_dir.join("src/cmds"))?;
        self.render_and_write("cmds/mod.rs.tmpl", &context, output_dir.join("src/cmds/mod.rs"))?;

        // Render each noun module
        for noun in &project.nouns {
            let noun_dir = output_dir.join("src/cmds").join(&noun.name);
            std::fs::create_dir_all(&noun_dir)?;

            // Render noun/mod.rs
            let mut noun_context = context.clone();
            noun_context.insert("noun", noun);
            self.render_and_write(
                "cmds/noun_mod.rs.tmpl",
                &noun_context,
                noun_dir.join("mod.rs"),
            )?;

            // Render each verb
            for verb in &noun.verbs {
                let mut verb_context = noun_context.clone();
                verb_context.insert("verb", verb);
                self.render_and_write(
                    "cmds/verb.rs.tmpl",
                    &verb_context,
                    noun_dir.join(format!("{}.rs", verb.name)),
                )?;
            }
        }

        // Render tests
        std::fs::create_dir_all(output_dir.join("tests"))?;
        self.render_and_write(
            "tests/integration.rs.tmpl",
            &context,
            output_dir.join("tests/integration_test.rs"),
        )?;

        // Render README
        self.render_and_write("README.md.tmpl", &context, output_dir.join("README.md"))?;

        // Copy .gitignore
        self.render_and_write("gitignore.tmpl", &context, output_dir.join(".gitignore"))?;

        Ok(())
    }

    fn render_and_write(
        &self,
        template: &str,
        context: &Context,
        output_path: PathBuf,
    ) -> Result<()> {
        let content = self.render_file(template, context)?;
        std::fs::write(output_path, content)?;
        Ok(())
    }
}
```

## Phase 5: Main Generator

### 5.1 CLI Generator Entry Point

```rust
// ggen-ai/src/rdf/generator.rs

use anyhow::Result;
use std::path::{Path, PathBuf};
use crate::rdf::{RdfParser, QueryExecutor, TemplateRenderer};

pub struct CliGenerator {
    template_dir: PathBuf,
}

impl CliGenerator {
    pub fn new(template_dir: PathBuf) -> Self {
        Self { template_dir }
    }

    pub fn generate_from_ttl(&self, ttl_path: &Path, output_dir: &Path) -> Result<()> {
        println!("Generating CLI project from {}", ttl_path.display());

        // Step 1: Parse RDF
        println!("  [1/5] Parsing RDF...");
        let mut parser = RdfParser::new()?;
        parser.load_schema()?;
        parser.load_ttl(ttl_path)?;

        // Step 2: Execute SPARQL queries
        println!("  [2/5] Extracting project structure...");
        let executor = QueryExecutor::new(parser.get_store());
        let mut project = executor.extract_project()?;
        project.nouns = executor.extract_nouns()?;
        project.dependencies = executor.extract_dependencies()?;

        // Step 3: Validate project
        println!("  [3/5] Validating project...");
        validate_project(&project)?;

        // Step 4: Render templates
        println!("  [4/5] Rendering templates...");
        let renderer = TemplateRenderer::new(&self.template_dir)?;
        std::fs::create_dir_all(output_dir)?;
        renderer.render_all(&project, output_dir)?;

        // Step 5: Post-generation
        println!("  [5/5] Running post-generation hooks...");
        run_post_generation(output_dir)?;

        println!("✓ CLI project generated at {}", output_dir.display());

        Ok(())
    }
}

fn validate_project(project: &crate::rdf::types::CliProject) -> Result<()> {
    if project.nouns.is_empty() {
        anyhow::bail!("Project must have at least one noun");
    }

    for noun in &project.nouns {
        if noun.verbs.is_empty() {
            anyhow::bail!("Noun '{}' must have at least one verb", noun.name);
        }
    }

    Ok(())
}

fn run_post_generation(output_dir: &Path) -> Result<()> {
    use std::process::Command;

    // Format code
    Command::new("cargo")
        .args(&["fmt"])
        .current_dir(output_dir)
        .status()?;

    // Check compilation
    Command::new("cargo")
        .args(&["check"])
        .current_dir(output_dir)
        .status()?;

    Ok(())
}
```

## Phase 6: Integration with ggen CLI

### 6.1 Add Command to ggen

```rust
// cli/src/commands/template/generate_rdf.rs

use anyhow::Result;
use clap::Args;
use std::path::PathBuf;
use crate::command::Command;
use ggen_ai::rdf::CliGenerator;

#[derive(Args, Debug)]
pub struct GenerateRdfArgs {
    /// Path to TTL file
    #[arg(value_name = "TTL_FILE")]
    pub ttl_file: PathBuf,

    /// Output directory
    #[arg(short = 'o', long = "output", default_value = ".")]
    pub output: PathBuf,

    /// Template directory
    #[arg(long = "templates", default_value = "examples/clap-noun-verb-demo/templates")]
    pub templates: PathBuf,
}

pub struct GenerateRdf {
    args: GenerateRdfArgs,
}

impl GenerateRdf {
    pub fn new(args: GenerateRdfArgs) -> Self {
        Self { args }
    }
}

impl Command for GenerateRdf {
    fn execute(&self) -> Result<()> {
        let generator = CliGenerator::new(self.args.templates.clone());
        generator.generate_from_ttl(&self.args.ttl_file, &self.args.output)?;
        Ok(())
    }
}
```

### 6.2 Wire Up in main.rs

```rust
// Add to TemplateActions enum
#[derive(Subcommand)]
enum TemplateActions {
    // ... existing commands

    /// Generate CLI from RDF/TTL file
    GenerateRdf(template::generate_rdf::GenerateRdfArgs),
}

// Add to match statement
match action {
    // ... existing matches

    TemplateActions::GenerateRdf(args) => {
        let cmd = template::generate_rdf::GenerateRdf::new(args);
        cmd.execute()?;
    }
}
```

## Phase 7: Testing

### 7.1 Integration Test

```rust
// cli/tests/rdf_generation_test.rs

use assert_cmd::Command;
use predicates::prelude::*;
use std::fs;
use tempfile::TempDir;

#[test]
fn test_generate_from_ttl() {
    let temp_dir = TempDir::new().unwrap();
    let output_dir = temp_dir.path().join("my-cli");

    let mut cmd = Command::cargo_bin("ggen").unwrap();
    cmd.arg("template")
        .arg("generate-rdf")
        .arg("examples/clap-noun-verb-demo/sample-cli.ttl")
        .arg("--output")
        .arg(&output_dir)
        .assert()
        .success();

    // Verify generated files
    assert!(output_dir.join("Cargo.toml").exists());
    assert!(output_dir.join("src/main.rs").exists());
    assert!(output_dir.join("src/cmds/template/generate.rs").exists());

    // Verify it compiles
    let status = std::process::Command::new("cargo")
        .args(&["check"])
        .current_dir(&output_dir)
        .status()
        .unwrap();

    assert!(status.success());
}
```

## Usage Example

```bash
# Generate CLI from TTL
ggen template generate-rdf examples/clap-noun-verb-demo/sample-cli.ttl -o ./my-new-cli

# Build and test
cd my-new-cli
cargo build
cargo test

# Run
cargo run -- template generate example-template --output ./output
cargo run -- project init my-project
```

## Next Steps

1. Implement error handling and validation
2. Add support for custom filters in Tera templates
3. Create interactive TUI for editing TTL files
4. Add watch mode for live regeneration
5. Support for multiple TTL files (imports)
6. Generate OpenAPI specs from TTL
7. Create visual graph of command structure
