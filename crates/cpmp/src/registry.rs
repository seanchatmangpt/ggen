use std::collections::HashMap;
use std::sync::OnceLock;

use oxigraph::io::{RdfFormat, RdfParser};
use oxigraph::store::Store;

use crate::entry::{Capability, OntologyContent, OntologyEntry};
use crate::tier::{OntologyAuthority, OntologyTier};

// ── Tier 0: embedded at compile-time ────────────────────────────────────────

const CATALOG_TTL: &str = include_str!("../ontologies/catalog.ttl");

const RDF_SYNTAX_NS_TTL: &str = include_str!("../ontologies/core/rdf-syntax-ns.ttl");
const RDF_SCHEMA_TTL: &str = include_str!("../ontologies/core/rdf-schema.ttl");
const OWL2_TTL: &str = include_str!("../ontologies/core/owl2.ttl");
const XSD_DATATYPES_TTL: &str = include_str!("../ontologies/core/xmlschema-datatypes.ttl");
const SHACL_TTL: &str = include_str!("../ontologies/core/shacl.ttl");
const PROV_O_TTL: &str = include_str!("../ontologies/core/prov-o.ttl");
const DCTERMS_TTL: &str = include_str!("../ontologies/core/dcterms.ttl");
const OCEL2_TTL: &str = include_str!("../ontologies/core/ocel2.ttl");

// ── Error type ───────────────────────────────────────────────────────────────

#[derive(Debug, thiserror::Error)]
pub enum RegistryError {
    #[error("ontology not found: {0}")]
    NotFound(String),

    #[error("content unavailable for {iri}: {reason}")]
    Unavailable { iri: String, reason: String },

    #[error("digest mismatch for {iri}: expected {expected}, got {actual}")]
    DigestMismatch {
        iri: String,
        expected: String,
        actual: String,
    },

    #[error("triplestore error: {0}")]
    Store(#[from] oxigraph::store::StorageError),

    #[error("parse error loading {path}: {reason}")]
    Parse { path: &'static str, reason: String },
}

pub type RegistryResult<T> = Result<T, RegistryError>;

// ── Registry ─────────────────────────────────────────────────────────────────

/// Unified ontology catalog for the ggen Vision 2030 system.
///
/// Holds:
/// - An Oxigraph in-memory store loaded with the DCAT 2 `catalog.ttl`
/// - A `HashMap` of `OntologyEntry` instances keyed by canonical IRI
///
/// Access via `OntologyRegistry::global()` for the process-wide singleton.
pub struct OntologyRegistry {
    catalog_store: Store,
    entries: HashMap<String, OntologyEntry>,
}

impl OntologyRegistry {
    /// Build a new registry by loading `catalog.ttl` into Oxigraph and
    /// populating all ontology entries.
    pub fn new() -> RegistryResult<Self> {
        let catalog_store = Store::new()?;
        load_ttl_into_store(&catalog_store, CATALOG_TTL, "ontologies/catalog.ttl")?;
        let entries = build_entries();
        Ok(Self {
            catalog_store,
            entries,
        })
    }

    /// Process-wide singleton — initialised exactly once via `OnceLock`.
    ///
    /// # Panics
    ///
    /// Panics on first access if the Oxigraph store cannot be created (out of
    /// memory) or the embedded catalog TTL is malformed. Both indicate
    /// non-recoverable corruption of the binary.
    pub fn global() -> &'static OntologyRegistry {
        static REGISTRY: OnceLock<OntologyRegistry> = OnceLock::new();
        REGISTRY.get_or_init(|| Self::new().expect("OntologyRegistry initialisation failed"))
    }

    /// IRIs of all Tier 0 (Core) ontologies embedded in this binary.
    pub fn tier0_iris(&self) -> Vec<&str> {
        let mut iris: Vec<&str> = self
            .entries
            .values()
            .filter(|e| e.tier == OntologyTier::Core)
            .map(|e| e.iri.as_str())
            .collect();
        iris.sort_unstable();
        iris
    }

    /// Retrieve an entry by its canonical IRI.
    pub fn get(&self, iri: &str) -> Option<&OntologyEntry> {
        self.entries.get(iri)
    }

    /// All entries in the catalog.
    pub fn entries(&self) -> impl Iterator<Item = &OntologyEntry> {
        self.entries.values()
    }

    /// Load all Tier 0 embedded ontologies into an existing Oxigraph `Store`.
    /// Useful for bootstrapping ggen-core's triplestore with W3C foundations.
    pub fn load_tier0_into_store(&self, store: &Store) -> RegistryResult<()> {
        let core_entries: Vec<(&'static str, &str)> = vec![
            ("ontologies/core/rdf-syntax-ns.ttl", RDF_SYNTAX_NS_TTL),
            ("ontologies/core/rdf-schema.ttl", RDF_SCHEMA_TTL),
            ("ontologies/core/owl2.ttl", OWL2_TTL),
            ("ontologies/core/xmlschema-datatypes.ttl", XSD_DATATYPES_TTL),
            ("ontologies/core/shacl.ttl", SHACL_TTL),
            ("ontologies/core/prov-o.ttl", PROV_O_TTL),
            ("ontologies/core/dcterms.ttl", DCTERMS_TTL),
            ("ontologies/core/ocel2.ttl", OCEL2_TTL),
        ];
        for (path, content) in core_entries {
            load_ttl_into_store(store, content, path)?;
        }
        Ok(())
    }

    /// The number of triples in the catalog store.
    pub fn catalog_triple_count(&self) -> usize {
        self.catalog_store
            .quads_for_pattern(None, None, None, None)
            .count()
    }
}

// ── Helpers ──────────────────────────────────────────────────────────────────

fn load_ttl_into_store(store: &Store, content: &str, path: &'static str) -> RegistryResult<()> {
    store
        .load_from_reader(
            RdfParser::from_format(RdfFormat::Turtle),
            content.as_bytes(),
        )
        .map_err(|e| RegistryError::Parse {
            path,
            reason: e.to_string(),
        })
}

/// Build the canonical set of ontology entries for all 62 catalog items.
fn build_entries() -> HashMap<String, OntologyEntry> {
    let mut m = HashMap::new();

    // ── Tier 0: Core (8) ────────────────────────────────────────────────────

    tier0(
        &mut m,
        "http://www.w3.org/1999/02/22-rdf-syntax-ns#",
        "rdf",
        "RDF 1.1 Vocabulary",
        OntologyAuthority::W3C,
        RDF_SYNTAX_NS_TTL,
        vec![Capability::RdfFoundation],
    );

    tier0(
        &mut m,
        "http://www.w3.org/2000/01/rdf-schema#",
        "rdfs",
        "RDF Schema 1.1",
        OntologyAuthority::W3C,
        RDF_SCHEMA_TTL,
        vec![Capability::RdfFoundation],
    );

    tier0(
        &mut m,
        "http://www.w3.org/2002/07/owl#",
        "owl",
        "OWL 2 Vocabulary",
        OntologyAuthority::W3C,
        OWL2_TTL,
        vec![Capability::OwlReasoning],
    );

    tier0(
        &mut m,
        "http://www.w3.org/2001/XMLSchema#",
        "xsd",
        "XML Schema 1.1 Datatypes",
        OntologyAuthority::W3C,
        XSD_DATATYPES_TTL,
        vec![Capability::DatatypeValidation],
    );

    tier0(
        &mut m,
        "http://www.w3.org/ns/shacl#",
        "sh",
        "Shapes Constraint Language (SHACL)",
        OntologyAuthority::W3C,
        SHACL_TTL,
        vec![Capability::ShaclValidation],
    );

    tier0(
        &mut m,
        "http://www.w3.org/ns/prov#",
        "prov",
        "PROV-O: The PROV Ontology",
        OntologyAuthority::W3C,
        PROV_O_TTL,
        vec![Capability::ProvenanceChain],
    );

    tier0(
        &mut m,
        "http://purl.org/dc/terms/",
        "dcterms",
        "Dublin Core Metadata Terms",
        OntologyAuthority::DCMI,
        DCTERMS_TTL,
        vec![Capability::Metadata],
    );

    tier0(
        &mut m,
        "https://www.ocel-standard.org/2.0/",
        "ocel",
        "Object-Centric Event Log (OCEL) 2.0",
        OntologyAuthority::Community,
        OCEL2_TTL,
        vec![Capability::ProcessMining],
    );

    // ── Tier 1: Cached (11) ─────────────────────────────────────────────────

    cached(
        &mut m,
        "http://www.w3.org/2004/02/skos/core#",
        "skos",
        "SKOS Simple Knowledge Organization System",
        OntologyAuthority::W3C,
        "https://www.w3.org/TR/skos-reference/skos.rdf",
        vec![Capability::KnowledgeOrganization],
    );

    cached(
        &mut m,
        "http://www.w3.org/ns/dcat#",
        "dcat",
        "Data Catalog Vocabulary (DCAT) 2",
        OntologyAuthority::W3C,
        "https://www.w3.org/ns/dcat2.ttl",
        vec![Capability::CatalogManagement],
    );

    cached(
        &mut m,
        "http://xmlns.com/foaf/0.1/",
        "foaf",
        "Friend of a Friend (FOAF)",
        OntologyAuthority::Community,
        "http://xmlns.com/foaf/spec/index.rdf",
        vec![Capability::AgentDescription],
    );

    cached(
        &mut m,
        "https://schema.org/",
        "schema",
        "Schema.org",
        OntologyAuthority::Community,
        "https://schema.org/version/latest/schemaorg-current-https.ttl",
        vec![Capability::StructuredData],
    );

    cached(
        &mut m,
        "http://rdfs.org/ns/void#",
        "void",
        "Vocabulary of Interlinked Datasets (VoID)",
        OntologyAuthority::W3C,
        "http://rdfs.org/ns/void.ttl",
        vec![Capability::DatasetDescription],
    );

    cached(
        &mut m,
        "http://qudt.org/schema/qudt/",
        "qudt",
        "Quantities, Units, Dimensions and Types (QUDT)",
        OntologyAuthority::Community,
        "http://qudt.org/2.1/schema/qudt",
        vec![Capability::UnitsOfMeasure],
    );

    cached(
        &mut m,
        "http://www.w3.org/2006/time#",
        "time",
        "OWL-Time",
        OntologyAuthority::W3C,
        "https://www.w3.org/2006/time",
        vec![Capability::Custom("temporal-reasoning".into())],
    );

    cached(
        &mut m,
        "https://spdx.org/rdf/terms/",
        "spdx",
        "SPDX RDF Ontology",
        OntologyAuthority::LinuxFoundation,
        "https://spdx.org/rdf/spdx-terms-v2.3.ttl",
        vec![Capability::ComplianceRules],
    );

    cached(
        &mut m,
        "http://usefulinc.com/ns/doap#",
        "doap",
        "Description of a Project (DOAP)",
        OntologyAuthority::Community,
        "https://raw.githubusercontent.com/ewilderj/doap/master/schema/doap.rdf",
        vec![Capability::Metadata],
    );

    cached(
        &mut m,
        "http://purl.org/vocab/vann/",
        "vann",
        "VANN: Vocabulary Annotation",
        OntologyAuthority::Community,
        "http://purl.org/vocab/vann/vann-vocab-20100607.rdf",
        vec![Capability::Metadata],
    );

    cached(
        &mut m,
        "http://purl.org/linked-data/cube#",
        "qb",
        "RDF Data Cube Vocabulary",
        OntologyAuthority::W3C,
        "http://purl.org/linked-data/cube",
        vec![Capability::StatisticalData],
    );

    // ── Tier 2: Referenced (38) ─────────────────────────────────────────────

    referenced(
        &mut m,
        "https://spec.edmcouncil.org/fibo/ontology/FND/",
        "fibo-fnd",
        "FIBO Foundations (FND)",
        OntologyAuthority::EDMCouncil,
        "https://spec.edmcouncil.org/fibo/",
        vec![Capability::FinancialInstruments],
    );

    referenced(
        &mut m,
        "https://spec.edmcouncil.org/fibo/ontology/BE/",
        "fibo-be",
        "FIBO Business Entities (BE)",
        OntologyAuthority::EDMCouncil,
        "https://spec.edmcouncil.org/fibo/",
        vec![Capability::FinancialInstruments],
    );

    referenced(
        &mut m,
        "https://spec.edmcouncil.org/fibo/ontology/SEC/",
        "fibo-sec",
        "FIBO Securities (SEC)",
        OntologyAuthority::EDMCouncil,
        "https://spec.edmcouncil.org/fibo/",
        vec![Capability::FinancialInstruments],
    );

    referenced(
        &mut m,
        "http://www.estrellaproject.org/lkif-core/lkif-core.owl#",
        "lkif",
        "Legal Knowledge Interchange Format (LKIF-Core)",
        OntologyAuthority::Community,
        "http://www.estrellaproject.org/lkif-core/",
        vec![Capability::ComplianceRules],
    );

    referenced(
        &mut m,
        "http://hl7.org/fhir/",
        "fhir",
        "HL7 FHIR RDF",
        OntologyAuthority::HL7,
        "https://www.hl7.org/fhir/rdf.html",
        vec![Capability::HcareData],
    );

    referenced(
        &mut m,
        "http://docs.oasis-open.org/cti/ns/stix/",
        "stix",
        "STIX 2.1 Cyber Threat Intelligence",
        OntologyAuthority::OASISOpen,
        "https://oasis-open.github.io/cti-stix2-json-schemas/",
        vec![Capability::SecurityClassification],
    );

    referenced(
        &mut m,
        "https://ontology.unifiedcyberontology.org/uco/",
        "uco",
        "Unified Cyber Ontology (UCO)",
        OntologyAuthority::Community,
        "https://unifiedcyberontology.org/",
        vec![Capability::SecurityClassification],
    );

    referenced(
        &mut m,
        "https://w3id.org/iac#",
        "iac",
        "Infrastructure as Code Ontology",
        OntologyAuthority::Community,
        "https://w3id.org/iac",
        vec![Capability::InfrastructureAsCode],
    );

    referenced(
        &mut m,
        "http://docs.oasis-open.org/tosca/ns/2011/12/",
        "tosca",
        "TOSCA Topology and Orchestration",
        OntologyAuthority::OASISOpen,
        "https://www.oasis-open.org/committees/tosca/",
        vec![Capability::InfrastructureAsCode],
    );

    referenced(
        &mut m,
        "http://ml-schema.github.io/documentation/",
        "mls",
        "ML Schema",
        OntologyAuthority::Community,
        "http://ml-schema.github.io/documentation/ML_schema.html",
        vec![Capability::MlMetadata],
    );

    referenced(
        &mut m,
        "https://www.w3.org/ns/mls",
        "mls2",
        "W3C Machine Learning Schema",
        OntologyAuthority::W3C,
        "https://www.w3.org/community/ml-schema/",
        vec![Capability::MlMetadata],
    );

    referenced(
        &mut m,
        "http://www.yawlfoundation.org/yawlschema#",
        "yawl",
        "YAWL Workflow Ontology",
        OntologyAuthority::Community,
        "https://yawlfoundation.github.io/",
        vec![Capability::WorkflowPatterns],
    );

    referenced(
        &mut m,
        "https://www.omg.org/spec/BPMN/20100524/MODEL-XMI",
        "bpmn",
        "BPMN 2.0 Metamodel",
        OntologyAuthority::OMG,
        "https://www.omg.org/spec/BPMN/",
        vec![Capability::WorkflowPatterns],
    );

    referenced(
        &mut m,
        "http://www.w3.org/ns/r2rml#",
        "rr",
        "R2RML: RDB to RDF Mapping Language",
        OntologyAuthority::W3C,
        "https://www.w3.org/TR/r2rml/r2rml.ttl",
        vec![Capability::DataIntegration],
    );

    referenced(
        &mut m,
        "http://www.w3.org/ns/sparql-service-description#",
        "sd",
        "SPARQL 1.1 Service Description",
        OntologyAuthority::W3C,
        "https://www.w3.org/TR/sparql11-service-description/sd.ttl",
        vec![Capability::Custom("sparql-federation".into())],
    );

    referenced(
        &mut m,
        "http://www.w3.org/ns/hydra/core#",
        "hydra",
        "Hydra Core Vocabulary",
        OntologyAuthority::W3C,
        "http://www.w3.org/ns/hydra/core",
        vec![Capability::ApiDescription],
    );

    referenced(
        &mut m,
        "https://www.w3.org/ns/json-ld#",
        "jsonld",
        "JSON-LD 1.1 Vocabulary",
        OntologyAuthority::W3C,
        "https://www.w3.org/ns/json-ld",
        vec![Capability::JsonLdContext],
    );

    referenced(
        &mut m,
        "http://www.w3.org/ns/ldp#",
        "ldp",
        "Linked Data Platform (LDP)",
        OntologyAuthority::W3C,
        "https://www.w3.org/ns/ldp",
        vec![Capability::ApiDescription],
    );

    referenced(
        &mut m,
        "http://purl.org/ontology/bibo/",
        "bibo",
        "Bibliographic Ontology (BIBO)",
        OntologyAuthority::Community,
        "http://purl.org/ontology/bibo/bibo.owl",
        vec![Capability::Metadata],
    );

    referenced(
        &mut m,
        "http://purl.org/dc/elements/1.1/",
        "dc",
        "Dublin Core Elements 1.1",
        OntologyAuthority::DCMI,
        "https://www.dublincore.org/specifications/dublin-core/dcmi-terms/dublin_core_elements.ttl",
        vec![Capability::Metadata],
    );

    referenced(
        &mut m,
        "http://www.opengis.net/ont/geosparql#",
        "geo",
        "GeoSPARQL 1.1",
        OntologyAuthority::OGC,
        "http://www.opengis.net/ont/geosparql",
        vec![Capability::Geospatial],
    );

    referenced(
        &mut m,
        "http://www.w3.org/2003/01/geo/wgs84_pos#",
        "wgs84",
        "WGS84 Geo Positioning",
        OntologyAuthority::W3C,
        "https://www.w3.org/2003/01/geo/wgs84_pos.rdf",
        vec![Capability::Geospatial],
    );

    referenced(
        &mut m,
        "http://purl.obolibrary.org/obo/go.owl",
        "go",
        "Gene Ontology (GO)",
        OntologyAuthority::Community,
        "http://geneontology.org/",
        vec![Capability::Custom("biology".into())],
    );

    referenced(
        &mut m,
        "http://purl.obolibrary.org/obo/chebi.owl",
        "chebi",
        "ChEBI: Chemical Entities of Biological Interest",
        OntologyAuthority::Community,
        "https://www.ebi.ac.uk/chebi/",
        vec![Capability::Custom("chemistry".into())],
    );

    referenced(
        &mut m,
        "http://purl.org/linked-data/sdmx/2009/measure#",
        "sdmx-measure",
        "SDMX-RDF Measure Vocabulary",
        OntologyAuthority::Community,
        "https://sdmx.org/",
        vec![Capability::StatisticalData],
    );

    referenced(
        &mut m,
        "http://www.ontology-of-units-of-measure.org/resource/om-2/",
        "om",
        "Ontology of Units of Measure (OM) 2",
        OntologyAuthority::Community,
        "http://www.ontology-of-units-of-measure.org/resource/om-2/om-2.0.rdf",
        vec![Capability::UnitsOfMeasure],
    );

    referenced(
        &mut m,
        "http://www.w3.org/2008/05/skos-xl#",
        "skosxl",
        "SKOS-XL (eXtension for Labels)",
        OntologyAuthority::W3C,
        "https://www.w3.org/TR/skos-reference/skos-xl.rdf",
        vec![Capability::KnowledgeOrganization],
    );

    referenced(
        &mut m,
        "http://semanticscience.org/resource/",
        "sio",
        "Semanticscience Integrated Ontology (SIO)",
        OntologyAuthority::Community,
        "http://semanticscience.org/",
        vec![Capability::Custom("scientific-data".into())],
    );

    referenced(
        &mut m,
        "https://www.w3.org/ns/activitystreams#",
        "as",
        "Activity Streams 2.0",
        OntologyAuthority::W3C,
        "https://www.w3.org/ns/activitystreams-owl",
        vec![Capability::AgentCommunication],
    );

    referenced(
        &mut m,
        "https://ggen.io/ontology/core#",
        "ggen",
        "ggen Core Ontology",
        OntologyAuthority::GgenProject,
        "https://github.com/seanchatmangpt/ggen",
        vec![Capability::CodeGeneration],
    );

    referenced(
        &mut m,
        "https://ggen.io/ontology/pipeline#",
        "pipe",
        "ggen Pipeline Ontology",
        OntologyAuthority::GgenProject,
        "https://github.com/seanchatmangpt/ggen",
        vec![Capability::CodeGeneration],
    );

    referenced(
        &mut m,
        "https://ggen.io/ontology/marketplace#",
        "market",
        "ggen Marketplace Ontology",
        OntologyAuthority::GgenProject,
        "https://github.com/seanchatmangpt/ggen",
        vec![Capability::CapabilityClassification],
    );

    referenced(
        &mut m,
        "https://ggen.io/ontology/cpmp#",
        "cpmp",
        "Computer Project Mapping Protocol (CPMP)",
        OntologyAuthority::GgenProject,
        "https://github.com/seanchatmangpt/ggen",
        vec![Capability::CapabilityClassification],
    );

    referenced(
        &mut m,
        "https://google.github.io/A2A/specification/",
        "a2a",
        "Google A2A Protocol",
        OntologyAuthority::Community,
        "https://google.github.io/A2A/",
        vec![Capability::AgentCommunication],
    );

    referenced(
        &mut m,
        "https://ggen.io/ontology/genesis-core#",
        "genesis",
        "Genesis Core Ontology",
        OntologyAuthority::GgenProject,
        "https://github.com/seanchatmangpt/ggen",
        vec![Capability::CodeGeneration],
    );

    referenced(
        &mut m,
        "https://ggen.io/ontology/yawl-knhk#",
        "yknhk",
        "KNHK YAWL Pattern Ontology",
        OntologyAuthority::GgenProject,
        "https://github.com/seanchatmangpt/ggen",
        vec![Capability::WorkflowPatterns],
    );

    // ── Process Mining Ontologies (Van der Aalst) ──────────────────────────

    referenced(
        &mut m,
        "https://ggen.io/ontology/petri-net#",
        "pn",
        "Petri Net Ontology",
        OntologyAuthority::GgenProject,
        "https://github.com/seanchatmangpt/ggen",
        vec![Capability::ProcessMining, Capability::WorkflowPatterns],
    );

    referenced(
        &mut m,
        "https://ggen.io/ontology/dfg#",
        "dfg",
        "Directly-Follows Graph (DFG) Ontology",
        OntologyAuthority::GgenProject,
        "https://github.com/seanchatmangpt/ggen",
        vec![Capability::ProcessMining],
    );

    referenced(
        &mut m,
        "https://ggen.io/ontology/process-tree#",
        "ptree",
        "Process Tree Ontology (Inductive Miner)",
        OntologyAuthority::GgenProject,
        "https://github.com/seanchatmangpt/ggen",
        vec![Capability::ProcessMining],
    );

    referenced(
        &mut m,
        "https://ggen.io/ontology/performance-spectrum#",
        "ps",
        "Performance Spectrum Ontology",
        OntologyAuthority::GgenProject,
        "https://github.com/seanchatmangpt/ggen",
        vec![Capability::ProcessMining],
    );

    referenced(
        &mut m,
        "https://ggen.io/ontology/decision-mining#",
        "dm",
        "Decision Mining Ontology",
        OntologyAuthority::GgenProject,
        "https://github.com/seanchatmangpt/ggen",
        vec![Capability::ProcessMining],
    );

    m
}

// ── Builder helpers ───────────────────────────────────────────────────────────

fn tier0(
    m: &mut HashMap<String, OntologyEntry>, iri: &str, prefix: &str, label: &str,
    authority: OntologyAuthority, content: &'static str, capabilities: Vec<Capability>,
) {
    m.insert(
        iri.to_owned(),
        OntologyEntry::new(
            iri,
            prefix,
            label,
            OntologyTier::Core,
            OntologyContent::Embedded(content),
            authority,
            capabilities,
        ),
    );
}

fn cached(
    m: &mut HashMap<String, OntologyEntry>, iri: &str, prefix: &str, label: &str,
    authority: OntologyAuthority, download_url: &str, capabilities: Vec<Capability>,
) {
    m.insert(
        iri.to_owned(),
        OntologyEntry::new(
            iri,
            prefix,
            label,
            OntologyTier::Cached,
            OntologyContent::Cached {
                download_url: download_url.to_owned(),
                blake3_hex: None,
            },
            authority,
            capabilities,
        ),
    );
}

fn referenced(
    m: &mut HashMap<String, OntologyEntry>, iri: &str, prefix: &str, label: &str,
    authority: OntologyAuthority, landing_url: &str, capabilities: Vec<Capability>,
) {
    m.insert(
        iri.to_owned(),
        OntologyEntry::new(
            iri,
            prefix,
            label,
            OntologyTier::Referenced,
            OntologyContent::Remote {
                landing_url: landing_url.to_owned(),
            },
            authority,
            capabilities,
        ),
    );
}
