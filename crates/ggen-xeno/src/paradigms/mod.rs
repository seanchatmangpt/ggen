//! Exo-computing paradigms for non-human computational models.
//!
//! This module explores alternative computational paradigms that might be
//! used by alien civilizations with different physical constraints or
//! technological approaches.

use serde::{Deserialize, Serialize};

pub mod quantum_computing;
pub mod biological_computing;
pub mod plasma_computing;
pub mod gravitational_computing;

/// Computational paradigm classifications
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum ComputationalParadigm {
    /// Traditional silicon-based digital computing
    Digital,

    /// Quantum computation using superposition and entanglement
    Quantum,

    /// Analog computation
    Analog,

    /// Biological or DNA-based computation
    Biological,

    /// Plasma-state computation
    Plasma,

    /// Gravitational wave computation
    Gravitational,

    /// Photonic/optical computation
    Photonic,

    /// Molecular or chemical computation
    Molecular,

    /// Metamaterial-based computation
    Metamaterial,

    /// Unknown or hybrid paradigm
    Unknown,
}

impl ComputationalParadigm {
    /// Get the theoretical computational complexity class
    #[must_use]
    pub const fn complexity_class(self) -> &'static str {
        match self {
            Self::Digital => "P",
            Self::Quantum => "BQP",
            Self::Analog => "P/poly",
            Self::Biological => "P",
            Self::Plasma => "PSPACE",
            Self::Gravitational => "BQP",
            Self::Photonic => "P",
            Self::Molecular => "P",
            Self::Metamaterial => "NP",
            Self::Unknown => "Unknown",
        }
    }

    /// Get estimated energy efficiency (relative to digital)
    #[must_use]
    pub const fn energy_efficiency(self) -> f64 {
        match self {
            Self::Digital => 1.0,
            Self::Quantum => 0.1,
            Self::Analog => 0.5,
            Self::Biological => 0.01,
            Self::Plasma => 100.0,
            Self::Gravitational => 1000.0,
            Self::Photonic => 0.1,
            Self::Molecular => 0.001,
            Self::Metamaterial => 10.0,
            Self::Unknown => 1.0,
        }
    }

    /// Check if paradigm requires exotic physics
    #[must_use]
    pub const fn requires_exotic_physics(self) -> bool {
        matches!(
            self,
            Self::Gravitational | Self::Plasma | Self::Metamaterial
        )
    }

    /// Get information density (bits per cubic meter)
    #[must_use]
    pub const fn information_density(self) -> f64 {
        match self {
            Self::Digital => 1e15,
            Self::Quantum => 1e20,
            Self::Analog => 1e12,
            Self::Biological => 1e18,
            Self::Plasma => 1e16,
            Self::Gravitational => 1e25,
            Self::Photonic => 1e17,
            Self::Molecular => 1e19,
            Self::Metamaterial => 1e16,
            Self::Unknown => 1e10,
        }
    }
}

/// Computational substrate - what the computation runs on
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct ComputationalSubstrate {
    /// Paradigm type
    pub paradigm: ComputationalParadigm,

    /// Physical medium
    pub medium: String,

    /// Operating temperature (Kelvin)
    pub temperature: Option<f64>,

    /// Operating pressure (atmospheres)
    pub pressure: Option<f64>,

    /// Required exotic conditions
    pub exotic_conditions: Vec<String>,

    /// Minimum civilization level to construct
    pub min_kardashev_level: f64,
}

impl ComputationalSubstrate {
    /// Create a new computational substrate
    #[must_use]
    pub fn new(paradigm: ComputationalParadigm, medium: String) -> Self {
        Self {
            paradigm,
            medium,
            temperature: None,
            pressure: None,
            exotic_conditions: Vec::new(),
            min_kardashev_level: 1.0,
        }
    }

    /// Check if substrate is compatible with current Earth technology
    #[must_use]
    pub fn earth_compatible(&self) -> bool {
        !self.paradigm.requires_exotic_physics()
            && self.min_kardashev_level <= 0.7 // Earth is ~0.7 on Kardashev scale
    }
}

/// Operation in an exotic computational paradigm
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ExoOperation {
    /// Operation name
    pub name: String,

    /// Paradigm it operates in
    pub paradigm: ComputationalParadigm,

    /// Input dimensionality
    pub input_dimensions: usize,

    /// Output dimensionality
    pub output_dimensions: usize,

    /// Time complexity
    pub time_complexity: String,

    /// Space complexity
    pub space_complexity: String,

    /// Whether operation is reversible
    pub reversible: bool,
}

impl ExoOperation {
    /// Create a new exo-operation
    #[must_use]
    pub fn new(
        name: String,
        paradigm: ComputationalParadigm,
        input_dimensions: usize,
        output_dimensions: usize,
    ) -> Self {
        Self {
            name,
            paradigm,
            input_dimensions,
            output_dimensions,
            time_complexity: "O(n)".to_string(),
            space_complexity: "O(1)".to_string(),
            reversible: false,
        }
    }
}
