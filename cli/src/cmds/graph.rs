use clap::Args;
use rgen_core::graph::Graph;
use std::io::Write;
use std::path::PathBuf;
use utils::error::Result;

#[derive(Args, Debug)]
pub struct GraphArgs {
    #[arg(value_name = "SCOPE")]
    pub scope: String,
    #[arg(value_name = "ACTION")]
    pub action: String,

    /// Output format (turtle, ntriples, rdfxml, jsonld)
    #[arg(short, long, default_value = "turtle")]
    pub format: String,

    /// Output file path (default: stdout)
    #[arg(short, long)]
    pub output: Option<PathBuf>,

    /// Include prefixes in output
    #[arg(long)]
    pub include_prefixes: bool,
}

pub fn run(args: &GraphArgs) -> Result<()> {
    println!(
        "Exporting graph for scope: {}, action: {}",
        args.scope, args.action
    );

    // Load the appropriate graph based on scope and action
    let graph = load_graph_for_scope_action(&args.scope, &args.action)?;

    if graph.is_empty() {
        println!(
            "No data found for scope '{}' and action '{}'",
            args.scope, args.action
        );
        return Ok(());
    }

    // Export the graph in the requested format
    export_graph(
        &graph,
        &args.format,
        args.output.as_ref(),
        args.include_prefixes,
    )?;

    println!("Graph exported successfully");
    Ok(())
}

fn load_graph_for_scope_action(scope: &str, action: &str) -> Result<Graph> {
    let graph = Graph::new()?;

    // Load graphs based on scope and action
    match (scope, action) {
        ("cli", "export") => {
            // Load CLI-related graphs
            load_cli_graphs(&graph)?;
        }
        ("api", "export") => {
            // Load API-related graphs
            load_api_graphs(&graph)?;
        }
        ("core", "export") => {
            // Load core graphs
            load_core_graphs(&graph)?;
        }
        _ => {
            // Try to load from template locations
            let graph_paths = vec![
                format!("templates/{}/{}/graphs/{}.ttl", scope, action, scope),
                format!("templates/{}/graphs/{}.ttl", scope, scope),
            ];
            
            let mut found = false;
            for graph_path in graph_paths {
                if std::path::Path::new(&graph_path).exists() {
                    graph.load_path(&graph_path)?;
                    found = true;
                    break;
                }
            }
            
            if !found {
                println!(
                    "No graph found for scope '{}' and action '{}'",
                    scope, action
                );
            }
        }
    }

    Ok(graph)
}

fn load_cli_graphs(graph: &Graph) -> Result<()> {
    let cli_graph_paths = vec![
        "templates/cli/subcommand/graphs/cli.ttl",
        "templates/cli/graphs/cli.ttl",
    ];
    
    for cli_graph_path in cli_graph_paths {
        if std::path::Path::new(cli_graph_path).exists() {
            graph.load_path(cli_graph_path)?;
            println!("Loaded CLI graph from {}", cli_graph_path);
            return Ok(());
        }
    }
    Ok(())
}

fn load_api_graphs(graph: &Graph) -> Result<()> {
    // Look for API-related graphs
    let api_graph_paths = vec![
        "templates/api/endpoint/graphs/api.ttl",
        "templates/api/graphs/api.ttl",
    ];
    
    for api_graph_path in api_graph_paths {
        if std::path::Path::new(api_graph_path).exists() {
            graph.load_path(api_graph_path)?;
            println!("Loaded API graph from {}", api_graph_path);
            return Ok(());
        }
    }
    Ok(())
}

fn load_core_graphs(graph: &Graph) -> Result<()> {
    let core_graph_paths = vec![
        "templates/core/graphs/core.ttl",
        "templates/api/endpoint/graphs/api.ttl", // API endpoint contains core concepts
    ];
    
    for core_graph_path in core_graph_paths {
        if std::path::Path::new(core_graph_path).exists() {
            graph.load_path(core_graph_path)?;
            println!("Loaded core graph from {}", core_graph_path);
            return Ok(());
        }
    }
    Ok(())
}

fn export_graph(
    graph: &Graph, format: &str, output_path: Option<&PathBuf>, include_prefixes: bool,
) -> Result<()> {
    let format_lower = format.to_lowercase();

    match format_lower.as_str() {
        "turtle" | "ttl" => {
            export_turtle(graph, output_path, include_prefixes)?;
        }
        "ntriples" | "nt" => {
            export_ntriples(graph, output_path)?;
        }
        "rdfxml" | "xml" => {
            export_rdfxml(graph, output_path)?;
        }
        "jsonld" | "json" => {
            export_jsonld(graph, output_path)?;
        }
        _ => {
            return Err(utils::error::Error::new(&format!(
                "Unsupported format: {}. Supported formats: turtle, ntriples, rdfxml, jsonld",
                format
            )));
        }
    }

    Ok(())
}

fn export_turtle(
    graph: &Graph, output_path: Option<&PathBuf>, include_prefixes: bool,
) -> Result<()> {
    // Use streaming approach for large graphs
    let query = "CONSTRUCT { ?s ?p ?o } WHERE { ?s ?p ?o }";
    let results = graph.query(query)?;

    match results {
        oxigraph::sparql::QueryResults::Graph(graph_iter) => {
            // Use streaming writer for better memory efficiency
            let writer: Box<dyn std::io::Write> = if let Some(path) = output_path {
                Box::new(std::fs::File::create(path)?)
            } else {
                Box::new(std::io::stdout())
            };

            let mut writer = std::io::BufWriter::new(writer);

            // Write prefixes if requested
            if include_prefixes {
                writeln!(
                    writer,
                    "@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> ."
                )?;
                writeln!(
                    writer,
                    "@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> ."
                )?;
                writeln!(writer, "@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .")?;
                writeln!(writer, "@prefix sh: <http://www.w3.org/ns/shacl#> .")?;
                writeln!(writer, "@prefix ex: <http://example.org/> .")?;
                writeln!(writer)?;
            }

            // Stream triples to avoid loading everything into memory
            let mut triple_count = 0;
            for triple in graph_iter {
                let triple = triple.map_err(|e| anyhow::anyhow!("Graph iteration error: {}", e))?;
                let s = triple.subject.to_string();
                let p = triple.predicate.to_string();
                let o = triple.object.to_string();

                writeln!(writer, "{} {} {} .", s, p, o)?;
                triple_count += 1;

                // Flush periodically for large datasets
                if triple_count % 1000 == 0 {
                    writer.flush()?;
                }
            }

            writer.flush()?;

            if let Some(path) = output_path {
                println!(
                    "Turtle format exported to {} ({} triples)",
                    path.display(),
                    triple_count
                );
            }
        }
        _ => {
            return Err(utils::error::Error::new(
                "Expected graph results for CONSTRUCT query",
            ));
        }
    }

    Ok(())
}

fn export_ntriples(graph: &Graph, output_path: Option<&PathBuf>) -> Result<()> {
    let query = "CONSTRUCT { ?s ?p ?o } WHERE { ?s ?p ?o }";
    let results = graph.query(query)?;

    match results {
        oxigraph::sparql::QueryResults::Graph(graph_iter) => {
            // Use streaming writer for better memory efficiency
            let writer: Box<dyn std::io::Write> = if let Some(path) = output_path {
                Box::new(std::fs::File::create(path)?)
            } else {
                Box::new(std::io::stdout())
            };

            let mut writer = std::io::BufWriter::new(writer);
            let mut triple_count = 0;

            for triple in graph_iter {
                let triple = triple.map_err(|e| anyhow::anyhow!("Graph iteration error: {}", e))?;
                let s = triple.subject.to_string();
                let p = triple.predicate.to_string();
                let o = triple.object.to_string();

                writeln!(writer, "{} {} {} .", s, p, o)?;
                triple_count += 1;

                // Flush periodically for large datasets
                if triple_count % 1000 == 0 {
                    writer.flush()?;
                }
            }

            writer.flush()?;

            if let Some(path) = output_path {
                println!(
                    "N-Triples format exported to {} ({} triples)",
                    path.display(),
                    triple_count
                );
            }
        }
        _ => {
            return Err(utils::error::Error::new(
                "Expected graph results for CONSTRUCT query",
            ));
        }
    }

    Ok(())
}

fn export_rdfxml(graph: &Graph, output_path: Option<&PathBuf>) -> Result<()> {
    // For RDF/XML, we'll use a simple approach
    let query = "CONSTRUCT { ?s ?p ?o } WHERE { ?s ?p ?o }";
    let results = graph.query(query)?;

    match results {
        oxigraph::sparql::QueryResults::Graph(graph_iter) => {
            let mut xml_content = String::new();
            xml_content.push_str("<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n");
            xml_content
                .push_str("<rdf:RDF xmlns:rdf=\"http://www.w3.org/1999/02/22-rdf-syntax-ns#\"\n");
            xml_content
                .push_str("         xmlns:rdfs=\"http://www.w3.org/2000/01/rdf-schema#\">\n");

            for triple in graph_iter {
                let triple = triple.map_err(|e| anyhow::anyhow!("Graph iteration error: {}", e))?;
                let s = triple.subject.to_string();
                let p = triple.predicate.to_string();
                let o = triple.object.to_string();

                xml_content.push_str(&format!("  <rdf:Description rdf:about=\"{}\">\n", s));
                xml_content.push_str(&format!("    <{}>{}</{}>\n", p, o, p));
                xml_content.push_str("  </rdf:Description>\n");
            }

            xml_content.push_str("</rdf:RDF>\n");

            if let Some(path) = output_path {
                std::fs::write(path, xml_content)?;
                println!("RDF/XML format exported to {}", path.display());
            } else {
                print!("{}", xml_content);
            }
        }
        _ => {
            return Err(utils::error::Error::new(
                "Expected graph results for CONSTRUCT query",
            ));
        }
    }

    Ok(())
}

fn export_jsonld(graph: &Graph, output_path: Option<&PathBuf>) -> Result<()> {
    // For JSON-LD, we'll create a simple structure
    let query = "SELECT ?s ?p ?o WHERE { ?s ?p ?o }";
    let results = graph.query(query)?;

    match results {
        oxigraph::sparql::QueryResults::Solutions(solutions) => {
            let mut jsonld_content = serde_json::json!({
                "@context": {
                    "rdf": "http://www.w3.org/1999/02/22-rdf-syntax-ns#",
                    "rdfs": "http://www.w3.org/2000/01/rdf-schema#",
                    "xsd": "http://www.w3.org/2001/XMLSchema#",
                    "sh": "http://www.w3.org/ns/shacl#",
                    "ex": "http://example.org/"
                },
                "@graph": []
            });

            let mut triples = Vec::new();
            for solution in solutions {
                let solution =
                    solution.map_err(|e| anyhow::anyhow!("Query solution error: {}", e))?;
                let s = solution.get("s").unwrap().to_string();
                let p = solution.get("p").unwrap().to_string();
                let o = solution.get("o").unwrap().to_string();

                triples.push(serde_json::json!({
                    "@id": s,
                    p: o
                }));
            }

            jsonld_content["@graph"] = serde_json::Value::Array(triples);

            let json_string = serde_json::to_string_pretty(&jsonld_content)?;

            if let Some(path) = output_path {
                std::fs::write(path, json_string)?;
                println!("JSON-LD format exported to {}", path.display());
            } else {
                print!("{}", json_string);
            }
        }
        _ => {
            return Err(utils::error::Error::new(
                "Expected solutions for SELECT query",
            ));
        }
    }

    Ok(())
}
