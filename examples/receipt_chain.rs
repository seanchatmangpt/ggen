//! Receipt Chain Example
//!
//! Demonstrates building a cryptographic receipt chain with Ed25519 signatures.
//! Each receipt is linked to the previous one via hash chaining.

use ggen_receipt::{generate_keypair, hash_data, Receipt, ReceiptChain};

fn main() -> Result<(), Box<dyn std::error::Error>> {
    println!("=== Receipt Chain Example ===\n");

    // Generate keypair for signing receipts
    let (signing_key, verifying_key) = generate_keypair();
    println!("Generated Ed25519 keypair");
    println!("Verifying key: {:?}\n", hex::encode(verifying_key.to_bytes()));

    // Create genesis receipt
    println!("Step 1: Creating genesis receipt");
    let input_data = b"initial_spec.ttl";
    let output_data = b"parsed_ontology.json";

    let genesis = Receipt::new(
        "parse-ontology".to_string(),
        vec![hash_data(input_data)],
        vec![hash_data(output_data)],
        None,
    )
    .sign(&signing_key)?;

    println!("  Operation: parse-ontology");
    println!("  Genesis hash: {}", genesis.hash()?);
    genesis.verify(&verifying_key)?;
    println!("  ✓ Signature verified\n");

    // Initialize chain
    let mut chain = ReceiptChain::from_genesis(genesis.clone())?;
    println!("Chain initialized with genesis receipt\n");

    // Step 2: Extract entities
    println!("Step 2: Extract entities from ontology");
    let extract_input = output_data;
    let extract_output = b"entities.json";

    let extract_receipt = Receipt::new(
        "extract-entities".to_string(),
        vec![hash_data(extract_input)],
        vec![hash_data(extract_output)],
        None,
    )
    .chain(&genesis)?
    .sign(&signing_key)?;

    println!("  Operation: extract-entities");
    println!("  Previous hash: {:?}", extract_receipt.previous_receipt_hash);
    extract_receipt.verify(&verifying_key)?;
    println!("  ✓ Signature verified");

    chain.append(extract_receipt.clone())?;
    println!("  ✓ Appended to chain\n");

    // Step 3: Generate code
    println!("Step 3: Generate code from entities");
    let gen_input = extract_output;
    let gen_output = b"generated_code.rs";

    let generate_receipt = Receipt::new(
        "generate-code".to_string(),
        vec![hash_data(gen_input)],
        vec![hash_data(gen_output)],
        None,
    )
    .chain(&extract_receipt)?
    .sign(&signing_key)?;

    println!("  Operation: generate-code");
    println!("  Previous hash: {:?}", generate_receipt.previous_receipt_hash);
    generate_receipt.verify(&verifying_key)?;
    println!("  ✓ Signature verified");

    chain.append(generate_receipt)?;
    println!("  ✓ Appended to chain\n");

    // Verify entire chain
    println!("Step 4: Verify entire chain");
    chain.verify(&verifying_key)?;
    println!("  ✓ Complete chain verified");
    println!("  Chain length: {} receipts", chain.len());
    println!("  Genesis: {}", chain.genesis().unwrap().operation_id);
    println!("  Latest: {}", chain.last().unwrap().operation_id);

    // Display chain integrity
    println!("\n=== Chain Integrity ===");
    for (i, receipt) in chain.receipts().iter().enumerate() {
        println!(
            "Receipt {}: {} (hash: {})",
            i,
            receipt.operation_id,
            &receipt.hash()?[..16]
        );
        if let Some(prev) = &receipt.previous_receipt_hash {
            println!("  └─ links to: {}...", &prev[..16]);
        }
    }

    println!("\n✓ Receipt chain example completed successfully");
    Ok(())
}
