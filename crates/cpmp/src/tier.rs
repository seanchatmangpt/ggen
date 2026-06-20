/// Storage tier for ontology content — determines where content is resolved from.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, serde::Serialize, serde::Deserialize)]
pub enum OntologyTier {
    /// Tier 0: embedded in the binary via `include_str!` — always available offline.
    Core,
    /// Tier 1: downloaded to XDG cache on first use, verified by BLAKE3 digest.
    Cached,
    /// Tier 2: URL reference only, resolved on demand when reasoning requires it.
    Referenced,
}

impl std::fmt::Display for OntologyTier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Core => f.write_str("Core"),
            Self::Cached => f.write_str("Cached"),
            Self::Referenced => f.write_str("Referenced"),
        }
    }
}

/// The standards body or project that maintains this ontology.
#[derive(Debug, Clone, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
pub enum OntologyAuthority {
    W3C,
    DCMI,
    OASISOpen,
    EDMCouncil,
    HL7,
    OGC,
    OMG,
    LinuxFoundation,
    GgenProject,
    Community,
    Domain(String),
}

impl std::fmt::Display for OntologyAuthority {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::W3C => f.write_str("W3C"),
            Self::DCMI => f.write_str("DCMI"),
            Self::OASISOpen => f.write_str("OASIS Open"),
            Self::EDMCouncil => f.write_str("EDM Council"),
            Self::HL7 => f.write_str("HL7"),
            Self::OGC => f.write_str("OGC"),
            Self::OMG => f.write_str("OMG"),
            Self::LinuxFoundation => f.write_str("Linux Foundation"),
            Self::GgenProject => f.write_str("ggen"),
            Self::Community => f.write_str("Community"),
            Self::Domain(s) => f.write_str(s),
        }
    }
}
