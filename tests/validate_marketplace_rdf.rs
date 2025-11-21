//! Validate RDF files used in marketplace packages
//!
//! This test validates that all RDF ontology files in marketplace packages
//! are syntactically valid Turtle files that can be loaded by Oxigraph.

#[cfg(test)]
mod tests {
    use ggen_core::Graph;
    use std::path::Path;

    /// Validate the Next.js ontology package RDF file
    #[test]
    fn test_validate_nextjs_ontology_rdf() {
        let rdf_file = Path::new(
            "marketplace/packages/io.ggen.nextjs.ontology-crud/ontology/task-management.ttl",
        );

        assert!(
            rdf_file.exists(),
            "RDF file not found: {}",
            rdf_file.display()
        );

        // Create graph and load RDF file
        let graph = Graph::new().expect("Failed to create graph");

        // Load and validate RDF syntax
        graph
            .load_path(rdf_file)
            .unwrap_or_else(|_| panic!("Failed to load/validate RDF file: {}", rdf_file.display()));

        // Verify file contains triples
        let triple_count = graph.len();
        assert!(
            triple_count > 0,
            "RDF file contains no triples: {}",
            rdf_file.display()
        );

        println!("✅ RDF file validated: {}", rdf_file.display());
        println!("   - Triples: {}", triple_count);
    }

    /// Validate RDF file syntax (basic check)
    #[test]
    fn test_validate_rdf_syntax() {
        let rdf_file = Path::new(
            "marketplace/packages/io.ggen.nextjs.ontology-crud/ontology/task-management.ttl",
        );

        if !rdf_file.exists() {
            println!("⚠️  RDF file not found, skipping validation");
            return;
        }

        let graph = Graph::new().unwrap();

        // This will fail if syntax is invalid
        match graph.load_path(rdf_file) {
            Ok(()) => {
                let count = graph.len();
                assert!(count > 0, "File should contain triples");
                println!("✅ Valid Turtle syntax: {} triples", count);
            }
            Err(e) => {
                panic!("Invalid RDF syntax: {}", e);
            }
        }
    }
}
