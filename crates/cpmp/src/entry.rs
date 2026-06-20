use crate::tier::{OntologyAuthority, OntologyTier};

/// Semantic capability that an ontology provides to the ggen pipeline.
#[derive(Debug, Clone, PartialEq, Eq, Hash, serde::Serialize, serde::Deserialize)]
pub enum Capability {
    RdfFoundation,
    OwlReasoning,
    ShaclValidation,
    DatatypeValidation,
    ProvenanceChain,
    Metadata,
    WorkflowPatterns,
    ProcessMining,
    KnowledgeOrganization,
    CatalogManagement,
    AgentDescription,
    StructuredData,
    DatasetDescription,
    UnitsOfMeasure,
    StatisticalData,
    AgentCommunication,
    FinancialInstruments,
    ComplianceRules,
    SecurityClassification,
    InfrastructureAsCode,
    MlMetadata,
    HcareData,
    Geospatial,
    ApiDescription,
    DataIntegration,
    JsonLdContext,
    CodeGeneration,
    CapabilityClassification,
    Custom(String),
}

/// Where the ontology content lives.
#[derive(Debug, Clone)]
pub enum OntologyContent {
    /// Tier 0: `'static` reference to `include_str!` data embedded in the binary.
    Embedded(&'static str),
    /// Tier 1: downloaded to the XDG cache; verified by BLAKE3.
    Cached {
        download_url: String,
        blake3_hex: Option<String>,
    },
    /// Tier 2: URL only; content not materialized until explicitly requested.
    Remote {
        landing_url: String,
    },
}

/// A single ontology entry in the Vision 2030 catalog.
pub struct OntologyEntry {
    pub iri: String,
    pub prefix: String,
    pub tier: OntologyTier,
    pub content: OntologyContent,
    pub capabilities: Vec<Capability>,
    pub label: String,
    pub authority: OntologyAuthority,
}

impl OntologyEntry {
    pub fn new(
        iri: impl Into<String>,
        prefix: impl Into<String>,
        label: impl Into<String>,
        tier: OntologyTier,
        content: OntologyContent,
        authority: OntologyAuthority,
        capabilities: Vec<Capability>,
    ) -> Self {
        Self {
            iri: iri.into(),
            prefix: prefix.into(),
            label: label.into(),
            tier,
            content,
            capabilities,
            authority,
        }
    }

    pub fn is_core(&self) -> bool {
        self.tier == OntologyTier::Core
    }

    pub fn embedded_content(&self) -> Option<&'static str> {
        match &self.content {
            OntologyContent::Embedded(s) => Some(s),
            _ => None,
        }
    }
}
