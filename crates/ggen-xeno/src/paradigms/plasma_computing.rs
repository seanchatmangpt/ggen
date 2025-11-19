//! Plasma-based computing paradigm.

use super::{ComputationalParadigm, ComputationalSubstrate};

/// Create a plasma computational substrate
#[must_use]
pub fn plasma_substrate() -> ComputationalSubstrate {
    ComputationalSubstrate {
        paradigm: ComputationalParadigm::Plasma,
        medium: "Ionized gas in magnetic confinement".to_string(),
        temperature: Some(1e6), // 1 million Kelvin
        pressure: Some(1e-8),   // High vacuum
        exotic_conditions: vec![
            "Strong magnetic fields".to_string(),
            "Plasma stability control".to_string(),
            "Electromagnetic shielding".to_string(),
        ],
        min_kardashev_level: 2.0,
    }
}

/// Star-core computation (for advanced civilizations)
#[must_use]
pub fn stellar_substrate() -> ComputationalSubstrate {
    ComputationalSubstrate {
        paradigm: ComputationalParadigm::Plasma,
        medium: "Stellar plasma manipulation".to_string(),
        temperature: Some(1e7), // 10 million Kelvin
        pressure: Some(1e15),
        exotic_conditions: vec![
            "Stellar engineering".to_string(),
            "Gravitational control".to_string(),
            "Fusion reaction management".to_string(),
        ],
        min_kardashev_level: 2.5,
    }
}
