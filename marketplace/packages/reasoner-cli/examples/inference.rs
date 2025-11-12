use reasoner_cli::Result;

#[tokio::main]
async fn main() -> Result<()> {
    println!("=== Reasoner CLI Inference Example ===\n");

    // Example 1: Derive inferences
    println!("1. Deriving inferences using OWL-DL regime:");
    println!("   $ reasoner inference derive --ontology medical.ttl --regime OWL-DL\n");

    // Example 2: Check entailment
    println!("2. Checking if axiom is entailed:");
    println!("   $ reasoner inference entail --ontology ontology.owl --axiom 'SubClassOf(Human Mammal)'\n");

    // Example 3: Check subsumption
    println!("3. Checking subsumption relationships:");
    println!("   $ reasoner inference subsume --ontology taxonomy.owl\n");

    // Example 4: Test equivalence
    println!("4. Testing class equivalence:");
    println!("   $ reasoner inference equivalent --ontology ontology.owl --class1 Person --class2 Human\n");

    // Example 5: Test inference correctness
    println!("5. Testing inference correctness:");
    println!("   $ reasoner inference test --ontology test-ontology.owl\n");

    println!("✓ Examples complete");

    Ok(())
}
