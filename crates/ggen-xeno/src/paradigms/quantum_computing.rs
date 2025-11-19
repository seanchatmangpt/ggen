//! Quantum computing paradigm implementations.

use super::{ComputationalParadigm, ComputationalSubstrate, ExoOperation};

/// Create a quantum computational substrate
#[must_use]
pub fn quantum_substrate() -> ComputationalSubstrate {
    ComputationalSubstrate {
        paradigm: ComputationalParadigm::Quantum,
        medium: "Topological qubits in quantum vacuum".to_string(),
        temperature: Some(0.015), // 15 millikelvin
        pressure: Some(0.0),      // Vacuum
        exotic_conditions: vec![
            "Quantum coherence maintenance".to_string(),
            "Error correction via topological protection".to_string(),
        ],
        min_kardashev_level: 1.2,
    }
}

/// Quantum gate operations
#[must_use]
pub fn quantum_gates() -> Vec<ExoOperation> {
    vec![
        ExoOperation {
            name: "Hadamard".to_string(),
            paradigm: ComputationalParadigm::Quantum,
            input_dimensions: 1,
            output_dimensions: 1,
            time_complexity: "O(1)".to_string(),
            space_complexity: "O(1)".to_string(),
            reversible: true,
        },
        ExoOperation {
            name: "CNOT".to_string(),
            paradigm: ComputationalParadigm::Quantum,
            input_dimensions: 2,
            output_dimensions: 2,
            time_complexity: "O(1)".to_string(),
            space_complexity: "O(1)".to_string(),
            reversible: true,
        },
        ExoOperation {
            name: "Toffoli".to_string(),
            paradigm: ComputationalParadigm::Quantum,
            input_dimensions: 3,
            output_dimensions: 3,
            time_complexity: "O(1)".to_string(),
            space_complexity: "O(1)".to_string(),
            reversible: true,
        },
    ]
}
