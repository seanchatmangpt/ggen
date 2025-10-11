//! Standalone CLI tool for generating frontmatter as JSON then converting to YAML
//! This version doesn't depend on any ggen packages to avoid compilation issues

use clap::{Parser, Subcommand};
use serde_json::{json, Value};
use serde_yaml;
use std::fs;

#[derive(Parser)]
#[command(name = "frontmatter-cli")]
#[command(about = "Generate frontmatter as JSON then convert to YAML")]
struct Cli {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand)]
enum Commands {
    /// Generate frontmatter from template description
    Generate {
        /// Description of the template to generate
        #[arg(short, long)]
        description: String,

        /// Output file path
        #[arg(short, long)]
        output: Option<String>,

        /// Convert to YAML format
        #[arg(long)]
        yaml: bool,

        /// Include RDF ontology
        #[arg(long)]
        rdf: bool,

        /// Include SPARQL queries
        #[arg(long)]
        sparql: bool,

        /// Template type (user, api, query)
        #[arg(short, long, default_value = "user")]
        template_type: String,
    },

    /// Convert JSON frontmatter to YAML
    Convert {
        /// Input JSON file
        #[arg(short, long)]
        input: String,

        /// Output YAML file
        #[arg(short, long)]
        output: Option<String>,
    },

    /// Show example frontmatter
    Example {
        /// Template type
        #[arg(short, long, default_value = "user")]
        template_type: String,
    },
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let cli = Cli::parse();

    match cli.command {
        Commands::Generate {
            description,
            output,
            yaml,
            rdf,
            sparql,
            template_type,
        } => {
            generate_frontmatter(description, output, yaml, rdf, sparql, template_type)?;
        }
        Commands::Convert { input, output } => {
            convert_json_to_yaml(input, output)?;
        }
        Commands::Example { template_type } => {
            show_example(template_type)?;
        }
    }

    Ok(())
}

fn generate_frontmatter(
    description: String, output: Option<String>, yaml: bool, rdf: bool, sparql: bool,
    template_type: String,
) -> Result<(), Box<dyn std::error::Error>> {
    println!("🚀 Generating frontmatter for: {}", description);

    // Generate frontmatter as JSON based on template type
    let mut frontmatter_json = match template_type.as_str() {
        "user" => json!({
            "to": "src/models/{{name}}.rs",
            "vars": [
                {"name": "string"},
                {"email": "string"},
                {"role": "string"}
            ],
            "determinism": true
        }),
        "api" => json!({
            "to": "src/controllers/{{resource}}_controller.rs",
            "vars": [
                {"resource": "string"},
                {"actions": "array"}
            ],
            "determinism": true
        }),
        "query" => json!({
            "to": "queries/{{query_name}}.sparql",
            "vars": [
                {"query_name": "string"},
                {"domain": "string"}
            ],
            "determinism": true
        }),
        _ => json!({
            "to": "src/{{name}}.rs",
            "vars": [{"name": "string"}],
            "determinism": true
        }),
    };

    // Add RDF if requested
    if rdf {
        frontmatter_json["rdf"] = json!("@prefix ex: <http://example.org/> .\n@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .\n@prefix owl: <http://www.w3.org/2002/07/owl#> .\n\nex:User a owl:Class ;\n    rdfs:label \"User\" ;\n    rdfs:comment \"A user in the system\" .\n\nex:name a owl:DatatypeProperty ;\n    rdfs:domain ex:User ;\n    rdfs:range xsd:string ;\n    rdfs:label \"name\" .\n\nex:email a owl:DatatypeProperty ;\n    rdfs:domain ex:User ;\n    rdfs:range xsd:string ;\n    rdfs:label \"email\" .\n\nex:role a owl:DatatypeProperty ;\n    rdfs:domain ex:User ;\n    rdfs:range xsd:string ;\n    rdfs:label \"role\" .");
    }

    // Add SPARQL if requested
    if sparql {
        frontmatter_json["sparql"] = json!("SELECT ?name ?email ?role WHERE {\n    ?user a ex:User ;\n      ex:name ?name ;\n      ex:email ?email ;\n      ex:role ?role .\n}");
    }

    println!("\n📝 Generated JSON frontmatter:");
    println!("{}", serde_json::to_string_pretty(&frontmatter_json)?);

    if yaml {
        // Convert to YAML
        let frontmatter_yaml = serde_yaml::to_string(&frontmatter_json)?;

        println!("\n🔄 Converted to YAML:");
        println!("{}", frontmatter_yaml);

        // Save to file if output specified
        if let Some(output_path) = output {
            fs::write(&output_path, frontmatter_yaml)?;
            println!("\n💾 Saved to: {}", output_path);
        }
    } else {
        // Save JSON to file if output specified
        if let Some(output_path) = output {
            let json_content = serde_json::to_string_pretty(&frontmatter_json)?;
            fs::write(&output_path, json_content)?;
            println!("\n💾 Saved to: {}", output_path);
        }
    }

    Ok(())
}

fn convert_json_to_yaml(
    input: String, output: Option<String>,
) -> Result<(), Box<dyn std::error::Error>> {
    println!("🔄 Converting JSON to YAML: {}", input);

    // Read JSON file
    let json_content = fs::read_to_string(&input)?;
    let json_value: Value = serde_json::from_str(&json_content)?;

    // Convert to YAML
    let yaml_content = serde_yaml::to_string(&json_value)?;

    println!("\n📊 Converted YAML:");
    println!("{}", yaml_content);

    // Save to file if output specified
    if let Some(output_path) = output {
        fs::write(&output_path, yaml_content)?;
        println!("\n💾 Saved to: {}", output_path);
    }

    Ok(())
}

fn show_example(template_type: String) -> Result<(), Box<dyn std::error::Error>> {
    println!("📚 Example frontmatter for: {}", template_type);

    let example_json = match template_type.as_str() {
        "user" => json!({
            "to": "src/models/user.rs",
            "vars": [
                {"name": "string"},
                {"email": "string"},
                {"role": "string"}
            ],
            "rdf": "@prefix ex: <http://example.org/> .\nex:User a ex:Class .",
            "sparql": "SELECT ?name ?email WHERE { ?user ex:name ?name ; ex:email ?email }",
            "determinism": true
        }),
        "api" => json!({
            "to": "src/controllers/{{resource}}_controller.rs",
            "vars": [
                {"resource": "string"},
                {"actions": "array"}
            ],
            "rdf": "@prefix ex: <http://example.org/> .\nex:Controller a ex:Class .",
            "sparql": "SELECT ?resource ?action WHERE { ?controller ex:manages ?resource ; ex:hasAction ?action }",
            "determinism": true
        }),
        "query" => json!({
            "to": "queries/{{query_name}}.sparql",
            "vars": [
                {"query_name": "string"},
                {"domain": "string"}
            ],
            "rdf": "@prefix ex: <http://example.org/> .\nex:Query a ex:Class .",
            "sparql": "SELECT ?query ?domain WHERE { ?query ex:belongsTo ?domain }",
            "determinism": true
        }),
        _ => json!({
            "to": "src/{{name}}.rs",
            "vars": [{"name": "string"}],
            "determinism": true
        }),
    };

    println!("\n📝 JSON format:");
    println!("{}", serde_json::to_string_pretty(&example_json)?);

    println!("\n🔄 YAML format:");
    println!("{}", serde_yaml::to_string(&example_json)?);

    Ok(())
}
