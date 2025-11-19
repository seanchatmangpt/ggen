//! Exo-Computing Paradigms Example
//!
//! This example explores different computational paradigms that alien
//! civilizations might use based on their physical constraints and
//! technological development.

use ggen_xeno::paradigms::{
    ComputationalParadigm,
    ComputationalSubstrate,
    ExoOperation,
    quantum_computing,
    biological_computing,
    plasma_computing,
    gravitational_computing,
};

fn main() {
    println!("=== EXOTICOMPUTINGPARADIGMS SHOWCASE ===\n");

    // Survey of computational paradigms
    println!("COMPUTATIONAL PARADIGM SURVEY");
    println!("==============================\n");

    let paradigms = vec![
        ComputationalParadigm::Digital,
        ComputationalParadigm::Quantum,
        ComputationalParadigm::Biological,
        ComputationalParadigm::Plasma,
        ComputationalParadigm::Gravitational,
        ComputationalParadigm::Photonic,
    ];

    for paradigm in &paradigms {
        println!("{:?} Computing:", paradigm);
        println!("  Complexity Class: {}", paradigm.complexity_class());
        println!("  Energy Efficiency: {:.3}x (relative to digital)", paradigm.energy_efficiency());
        println!("  Information Density: {:.2e} bits/m³", paradigm.information_density());
        println!("  Requires Exotic Physics: {}", paradigm.requires_exotic_physics());
        println!();
    }

    // Quantum Computing Substrate
    println!("QUANTUM COMPUTING SUBSTRATE");
    println!("============================\n");

    let quantum_sub = quantum_computing::quantum_substrate();
    print_substrate(&quantum_sub);

    let quantum_ops = quantum_computing::quantum_gates();
    println!("Available Quantum Gates:");
    for op in &quantum_ops {
        println!("  - {} ({}→{})", op.name, op.input_dimensions, op.output_dimensions);
        println!("    Complexity: {}, Reversible: {}", op.time_complexity, op.reversible);
    }
    println!();

    // Biological Computing
    println!("BIOLOGICAL COMPUTING SUBSTRATES");
    println!("================================\n");

    println!("1. DNA Computing:");
    let dna_sub = biological_computing::dna_substrate();
    print_substrate(&dna_sub);

    println!("2. Neural Computing:");
    let neural_sub = biological_computing::neural_substrate();
    print_substrate(&neural_sub);

    // Plasma Computing
    println!("PLASMA COMPUTING SUBSTRATES");
    println!("============================\n");

    println!("1. Magneticly Confined Plasma:");
    let plasma_sub = plasma_computing::plasma_substrate();
    print_substrate(&plasma_sub);

    println!("2. Stellar Core Computing:");
    let stellar_sub = plasma_computing::stellar_substrate();
    print_substrate(&stellar_sub);

    // Gravitational Computing
    println!("GRAVITATIONAL COMPUTING");
    println!("========================\n");

    let grav_sub = gravitational_computing::gravitational_substrate();
    print_substrate(&grav_sub);

    let grav_ops = gravitational_computing::gravitational_operations();
    println!("Gravitational Operations:");
    for op in &grav_ops {
        println!("  - {} ({}→{})", op.name, op.input_dimensions, op.output_dimensions);
        println!("    Complexity: {}", op.time_complexity);
    }
    println!();

    // Civilization Requirements
    println!("CIVILIZATION REQUIREMENTS (Kardashev Scale)");
    println!("============================================\n");

    let substrates = vec![
        ("Digital (Earth baseline)", 0.7),
        ("DNA Computing", 0.8),
        ("Neural Computing", 1.0),
        ("Quantum Computing", 1.2),
        ("Plasma Computing", 2.0),
        ("Stellar Computing", 2.5),
        ("Gravitational Computing", 3.0),
    ];

    for (name, level) in substrates {
        let tier = if level < 1.0 {
            "Type 0 (Pre-planetary)"
        } else if level < 2.0 {
            "Type I (Planetary)"
        } else if level < 3.0 {
            "Type II (Stellar)"
        } else {
            "Type III (Galactic)"
        };

        println!("  {:30} - Level {:.1} ({})", name, level, tier);
    }
    println!();

    // Custom Paradigm Creation
    println!("CUSTOM PARADIGM DEFINITION");
    println!("===========================\n");

    let custom_substrate = ComputationalSubstrate::new(
        ComputationalParadigm::Metamaterial,
        "Programmable matter lattice".to_string(),
    );

    println!("Custom Substrate Created:");
    println!("  Medium: {}", custom_substrate.medium);
    println!("  Paradigm: {:?}", custom_substrate.paradigm);
    println!("  Earth Compatible: {}", custom_substrate.earth_compatible());
    println!();

    let custom_op = ExoOperation::new(
        "MetamaterialTransform".to_string(),
        ComputationalParadigm::Metamaterial,
        3,
        3,
    );

    println!("Custom Operation:");
    println!("  Name: {}", custom_op.name);
    println!("  Dimensions: {} → {}", custom_op.input_dimensions, custom_op.output_dimensions);
    println!();

    // Comparative Analysis
    println!("COMPARATIVE ANALYSIS");
    println!("=====================\n");

    println!("For a civilization to build each computational substrate:");
    println!();

    let analysis = vec![
        ("Digital", "Moderate", "High", "Mature", "0.7"),
        ("Quantum", "Very High", "Medium", "Emerging", "1.2"),
        ("Biological", "Low", "Very High", "Experimental", "0.8"),
        ("Plasma", "Extreme", "Low", "Theoretical", "2.0"),
        ("Gravitational", "Impossible (for us)", "Very Low", "Speculative", "3.0"),
    ];

    println!("{:<20} {:<20} {:<15} {:<15} {:<10}",
             "Paradigm", "Construction Difficulty", "Reliability", "Status", "Kardashev");
    println!("{}", "-".repeat(80));

    for (paradigm, difficulty, reliability, status, kardashev) in analysis {
        println!("{:<20} {:<20} {:<15} {:<15} {:<10}",
                 paradigm, difficulty, reliability, status, kardashev);
    }
    println!();

    // Future Prospects
    println!("IMPLICATIONS FOR INTERSTELLAR COMMUNICATION");
    println!("============================================\n");

    println!("When encountering alien civilizations, their computational");
    println!("paradigm may indicate:");
    println!();
    println!("  1. Technological Level: Kardashev scale estimate");
    println!("  2. Physical Environment: Temperature, pressure, materials available");
    println!("  3. Cognitive Architecture: How they process information");
    println!("  4. Communication Constraints: What protocols might work");
    println!("  5. Compatibility: How well we can interface");
    println!();

    println!("Example scenarios:");
    println!("  - Plasma-based civs → High-energy signals, stellar engineering");
    println!("  - Quantum-based civs → Entangled communication, uncertainty-based logic");
    println!("  - Biological civs → Slow but efficient, distributed processing");
    println!("  - Gravitational civs → Spacetime manipulation, superluminal concepts");
    println!();

    println!("=== END OF EXO-COMPUTING SHOWCASE ===");
}

fn print_substrate(substrate: &ComputationalSubstrate) {
    println!("  Paradigm: {:?}", substrate.paradigm);
    println!("  Medium: {}", substrate.medium);

    if let Some(temp) = substrate.temperature {
        println!("  Operating Temperature: {:.2} K", temp);
    }

    if let Some(pressure) = substrate.pressure {
        println!("  Operating Pressure: {:.2e} atm", pressure);
    }

    if !substrate.exotic_conditions.is_empty() {
        println!("  Exotic Conditions:");
        for condition in &substrate.exotic_conditions {
            println!("    - {}", condition);
        }
    }

    println!("  Min Kardashev Level: {:.1}", substrate.min_kardashev_level);
    println!("  Earth Compatible: {}", substrate.earth_compatible());
    println!();
}
