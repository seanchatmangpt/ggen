use reasoner_cli::Result;

#[tokio::main]
async fn main() -> Result<()> {
    println!("=== Reasoner CLI Classification Example ===\n");

    // Example 1: Classify ontology
    println!("1. Classifying ontology with OWL 2 DL profile:");
    println!("   $ reasoner classifier classify --ontology pizza.owl --profile DL\n");

    // Example 2: Realize ABox
    println!("2. Realizing instance types:");
    println!("   $ reasoner classifier realize --ontology knowledge-base.owl\n");

    // Example 3: Materialize inferences
    println!("3. Materializing inferred triples:");
    println!("   $ reasoner classifier materialize --ontology ontology.owl --output inferred.owl\n");

    // Example 4: Profile performance
    println!("4. Profiling classification performance:");
    println!("   $ reasoner classifier profile --ontology large-ontology.owl\n");

    println!("✓ Examples complete");

    Ok(())
}
