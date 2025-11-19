//! Biological computing paradigm implementations.

use super::{ComputationalParadigm, ComputationalSubstrate};

/// Create a biological computational substrate
#[must_use]
pub fn dna_substrate() -> ComputationalSubstrate {
    ComputationalSubstrate {
        paradigm: ComputationalParadigm::Biological,
        medium: "DNA molecules in cellular medium".to_string(),
        temperature: Some(310.0), // Human body temperature
        pressure: Some(1.0),
        exotic_conditions: vec![
            "Biocompatible environment".to_string(),
            "Enzymatic processing".to_string(),
        ],
        min_kardashev_level: 0.8,
    }
}

/// Neural network substrate (biological)
#[must_use]
pub fn neural_substrate() -> ComputationalSubstrate {
    ComputationalSubstrate {
        paradigm: ComputationalParadigm::Biological,
        medium: "Engineered neurons in neural gel".to_string(),
        temperature: Some(298.0),
        pressure: Some(1.0),
        exotic_conditions: vec![
            "Nutrient circulation".to_string(),
            "Neurotransmitter synthesis".to_string(),
        ],
        min_kardashev_level: 1.0,
    }
}
