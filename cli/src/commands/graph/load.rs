//! RDF load CLI command
//!
//! Delegates to domain layer for business logic

use clap::Args;
use clap_noun_verb_macros::verb;
use ggen_utils::error::Result;

#[derive(Args, Debug)]
pub struct LoadArgs {
    /// RDF file to load
    pub file: String,

    /// RDF format (turtle, ntriples, rdfxml, jsonld, n3)
    #[arg(long)]
    pub format: Option<String>,

    /// Base IRI for relative URIs
    #[arg(long)]
    pub base: Option<String>,

    /// Merge with existing graph
    #[arg(long)]
    pub merge: bool,
}

pub async fn run(args: &LoadArgs) -> Result<()> {
    println!("📊 Loading RDF graph...");
    println!("📁 File: {}", args.file);

    // Parse format if provided
    let format = if let Some(format_str) = &args.format {
        Some(parse_format(format_str)?)
    } else {
        None
    };

    // Delegate to domain layer
    let options = crate::domain::graph::LoadOptions {
        file_path: args.file.clone(),
        format,
        base_iri: args.base.clone(),
        merge: args.merge,
    };

    let stats = crate::domain::graph::load_rdf(options)?;

    // Display results
    if args.merge {
        println!(
            "✅ Merged {} triples from {} ({})",
            stats.triples_loaded,
            stats.file_path,
            stats.format.as_str()
        );
        println!("📊 Total triples in graph: {}", stats.total_triples);
    } else {
        println!(
            "✅ Loaded {} triples from {} ({})",
            stats.triples_loaded,
            stats.file_path,
            stats.format.as_str()
        );
    }

    Ok(())
}

fn parse_format(s: &str) -> Result<crate::domain::graph::RdfFormat> {
    match s.to_lowercase().as_str() {
        "turtle" | "ttl" => Ok(crate::domain::graph::RdfFormat::Turtle),
        "ntriples" | "nt" => Ok(crate::domain::graph::RdfFormat::NTriples),
        "rdfxml" | "rdf" | "xml" => Ok(crate::domain::graph::RdfFormat::RdfXml),
        "jsonld" | "json" => Ok(crate::domain::graph::RdfFormat::JsonLd),
        "n3" => Ok(crate::domain::graph::RdfFormat::N3),
        _ => Err(ggen_utils::error::Error::new(&format!(
            "Unsupported RDF format: {}. Supported formats: turtle, ntriples, rdfxml, jsonld, n3",
            s
        ))),
    }
}
