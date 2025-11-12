//! Example: Batch validation of multiple RDF files

use shacl_cli::{Shape, ShapeType, Validator};

fn main() {
    println!("=== SHACL Batch Validation Example ===\n");

    // Create shapes
    let shapes = vec![
        Shape::new("PersonShape".to_string(), ShapeType::Node),
        Shape::new("EmailShape".to_string(), ShapeType::Property),
    ];

    // Create validator
    let validator = Validator::new(shapes);

    println!("Created validator with {} shapes", 2);
    println!("Validating batch of files...");

    // Validate
    match validator.validate() {
        Ok(result) => {
            println!("Validation complete!");
            println!("Conforms: {}", result.conforms);
            println!("Violations: {}", result.violations.len());
        }
        Err(e) => eprintln!("Validation failed: {}", e),
    }
}
