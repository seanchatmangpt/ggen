//! Configuration for the living documentation system

use serde::{Deserialize, Serialize};
use std::path::PathBuf;

/// Main configuration for the living documentation system
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Config {
    /// Ontology extraction configuration
    pub ontology_config: OntologyConfig,

    /// Code extraction configuration
    pub extractor_config: ExtractorConfig,

    /// Narrative generation configuration
    pub narrative_config: NarrativeConfig,

    /// NLU engine configuration
    pub nlu_config: NluConfig,

    /// Interactive interface configuration
    pub interface_config: InterfaceConfig,
}

impl Default for Config {
    fn default() -> Self {
        Self {
            ontology_config: OntologyConfig::default(),
            extractor_config: ExtractorConfig::default(),
            narrative_config: NarrativeConfig::default(),
            nlu_config: NluConfig::default(),
            interface_config: InterfaceConfig::default(),
        }
    }
}

/// Configuration for ontology extraction and management
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct OntologyConfig {
    /// RDF store path
    pub store_path: PathBuf,

    /// Base URI for ontology
    pub base_uri: String,

    /// Namespace prefix
    pub namespace_prefix: String,

    /// Enable caching
    pub enable_cache: bool,

    /// Cache size in MB
    pub cache_size_mb: usize,
}

impl Default for OntologyConfig {
    fn default() -> Self {
        Self {
            store_path: PathBuf::from(".ggen/living-docs/ontology"),
            base_uri: "http://ggen.dev/ontology/code#".to_string(),
            namespace_prefix: "code".to_string(),
            enable_cache: true,
            cache_size_mb: 100,
        }
    }
}

/// Configuration for code extraction
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ExtractorConfig {
    /// File patterns to include
    pub include_patterns: Vec<String>,

    /// File patterns to exclude
    pub exclude_patterns: Vec<String>,

    /// Extract private items
    pub extract_private: bool,

    /// Extract tests
    pub extract_tests: bool,

    /// Extract dependencies
    pub extract_dependencies: bool,

    /// Maximum depth for dependency analysis
    pub max_dependency_depth: usize,
}

impl Default for ExtractorConfig {
    fn default() -> Self {
        Self {
            include_patterns: vec!["**/*.rs".to_string()],
            exclude_patterns: vec!["**/target/**".to_string(), "**/tests/**".to_string()],
            extract_private: false,
            extract_tests: false,
            extract_dependencies: true,
            max_dependency_depth: 3,
        }
    }
}

/// Configuration for narrative generation
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct NarrativeConfig {
    /// Template directory
    pub template_dir: PathBuf,

    /// Output directory
    pub output_dir: PathBuf,

    /// Narrative style
    pub style: NarrativeStyle,

    /// Include examples in narratives
    pub include_examples: bool,

    /// Include diagrams
    pub include_diagrams: bool,

    /// Maximum narrative length
    pub max_length: usize,
}

impl Default for NarrativeConfig {
    fn default() -> Self {
        Self {
            template_dir: PathBuf::from(".ggen/living-docs/templates"),
            output_dir: PathBuf::from("docs/generated"),
            style: NarrativeStyle::Technical,
            include_examples: true,
            include_diagrams: true,
            max_length: 10000,
        }
    }
}

/// Narrative generation style
#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq, Eq)]
pub enum NarrativeStyle {
    /// Technical documentation style
    Technical,

    /// Conversational style
    Conversational,

    /// Tutorial style
    Tutorial,

    /// Reference style
    Reference,
}

/// Configuration for NLU engine
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct NluConfig {
    /// AI provider for NLU
    pub ai_provider: String,

    /// Model name
    pub model_name: String,

    /// Enable semantic parsing
    pub enable_semantic_parsing: bool,

    /// Enable query expansion
    pub enable_query_expansion: bool,

    /// Confidence threshold for updates
    pub confidence_threshold: f64,
}

impl Default for NluConfig {
    fn default() -> Self {
        Self {
            ai_provider: "openai".to_string(),
            model_name: "gpt-4".to_string(),
            enable_semantic_parsing: true,
            enable_query_expansion: true,
            confidence_threshold: 0.8,
        }
    }
}

/// Configuration for interactive interface
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct InterfaceConfig {
    /// Server bind address
    pub bind_address: String,

    /// Server port
    pub port: u16,

    /// Static assets directory
    pub assets_dir: PathBuf,

    /// Enable WebSocket
    pub enable_websocket: bool,

    /// Enable live reload
    pub enable_live_reload: bool,

    /// Max connections
    pub max_connections: usize,
}

impl Default for InterfaceConfig {
    fn default() -> Self {
        Self {
            bind_address: "127.0.0.1".to_string(),
            port: 8080,
            assets_dir: PathBuf::from(".ggen/living-docs/assets"),
            enable_websocket: true,
            enable_live_reload: true,
            max_connections: 100,
        }
    }
}
