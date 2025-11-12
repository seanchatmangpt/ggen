//! Example: Creating and using node shapes

use shacl_cli::{Shape, ShapeType, Target};

fn main() {
    println!("=== SHACL Node Shape Example ===\n");

    // Create a node shape for Person
    let person_shape = Shape::new(
        "PersonShape".to_string(),
        ShapeType::Node,
    ).with_target(Target::Class("http://xmlns.com/foaf/0.1/Person".to_string()));

    println!("Created shape: {}", person_shape.name);
    println!("Type: {:?}", person_shape.shape_type);
    println!("Target: {:?}", person_shape.target);
}
