//! Example: Creating and using property shapes

use shacl_cli::{Shape, ShapeType, Constraint, ConstraintType, Severity};

fn main() {
    println!("=== SHACL Property Shape Example ===\n");

    // Create a property shape
    let email_shape = Shape::new(
        "EmailShape".to_string(),
        ShapeType::Property,
    );

    println!("Created shape: {}", email_shape.name);
    println!("Type: {:?}", email_shape.shape_type);

    // Create constraints
    let pattern_constraint = Constraint {
        constraint_type: ConstraintType::Pattern(
            r"^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\.[a-zA-Z]{2,}$".to_string()
        ),
        severity: Severity::Violation,
    };

    println!("Added pattern constraint for email validation");
}
