//! Simple RDF validation script
//! Validates a Turtle RDF file using Oxigraph

use ggen_core::Graph;
use std::env;
use std::path::Path;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args: Vec<String> = env::args().collect();
    
    if args.len() != 2 {
        eprintln!("Usage: {} <rdf-file.ttl>", args[0]);
        eprintln!("Example: {} marketplace/packages/io.ggen.nextjs.ontology-crud/ontology/task-management.ttl", args[0]);
        std::process::exit(1);
    }

    let file_path = &args[1];
    let path = Path::new(file_path);

    if !path.exists() {
        eprintln!("Error: File not found: {}", file_path);
        std::process::exit(1);
    }

    println!("🔍 Validating RDF file: {}", file_path);
    println!("");

    // Load and validate RDF
    match Graph::new() {
        Ok(graph) => {
            match graph.load_path(path) {
                Ok(()) => {
                    let triple_count = graph.len();
                    println!("✅ RDF syntax is valid!");
                    println!("   - Format: Turtle (.ttl)");
                    println!("   - Triples loaded: {}", triple_count);
                    
                    // Check if file has reasonable content
                    if triple_count == 0 {
                        eprintln!("⚠️  Warning: File contains no triples (may be empty)");
                        std::process::exit(1);
                    }
                    
                    println!("");
                    println!("✅ Validation complete!");
                    Ok(())
                }
                Err(e) => {
                    eprintln!("❌ RDF validation failed: {}", e);
                    eprintln!("");
                    eprintln!("The file may contain:");
                    eprintln!("  - Invalid Turtle syntax");
                    eprintln!("  - Malformed URIs");
                    eprintln!("  - Invalid RDF statements");
                    eprintln!("  - Encoding issues");
                    std::process::exit(1);
                }
            }
        }
        Err(e) => {
            eprintln!("❌ Failed to create graph: {}", e);
            std::process::exit(1);
        }
    }
}

