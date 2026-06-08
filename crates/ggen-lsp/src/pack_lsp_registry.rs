use ggen_projection::protocol::{PackObservation, PackFinding, ProjectionSignature, CustomizationPoint, PackActionIntent};
use tower_lsp_max::lsp_types::Url;

pub trait PackObserver {
    fn source_id(&self) -> &str;
    fn domain(&self) -> &str;
    fn observe(&self, uri: &Url, content: &str) -> Vec<PackObservation>;
}

pub struct ClapNounVerbObserver;
impl PackObserver for ClapNounVerbObserver {
    fn source_id(&self) -> &str { "clap_noun_verb_pack_lsp" }
    fn domain(&self) -> &str { "clap-noun-verb" }
    fn observe(&self, uri: &Url, content: &str) -> Vec<PackObservation> {
        let mut obs = Vec::new();
        let path_str = uri.path();
        
        // Example: If it's a CLI file and missing a handler
        if path_str.ends_with("cli.rs") || path_str.ends_with("main.rs") {
            
            if content.contains("struct Opts") && !content.contains("fn handle") {
                obs.push(PackObservation {
                    pack_id: "ggen-pack-clap-noun-verb".to_string(),
                    source_id: self.source_id().to_string(),
                    domain: self.domain().to_string(),
                    document_uri: uri.to_string(),
                    findings: vec![PackFinding {
                        code: "CLAP-PACK-HANDLER-UNBOUND".to_string(),
                        message: "CLI domain missing handler".to_string(),
                        severity: 1, // Error
                        line: 0,
                        intents: vec![],
                    }],
                    projection_signatures: vec![],
                    customization_points: vec![],
                    action_intents: vec![],
                });
            }
        }
        obs
    }
}

pub struct TowerLspObserver;
impl PackObserver for TowerLspObserver {
    fn source_id(&self) -> &str { "tower_lsp_pack_lsp" }
    fn domain(&self) -> &str { "tower-lsp" }
    fn observe(&self, uri: &Url, content: &str) -> Vec<PackObservation> {
        let mut obs = Vec::new();
        let path_str = uri.path();
        
        // Example: if it's an LSP file with unguarded mutation
        if path_str.ends_with("server.rs") || path_str.ends_with("lsp.rs") {
            if content.contains("write_to_disk") && !content.contains("MutationGate") {
                obs.push(PackObservation {
                    pack_id: "ggen-pack-tower-lsp".to_string(),
                    source_id: self.source_id().to_string(),
                    domain: self.domain().to_string(),
                    document_uri: uri.to_string(),
                    findings: vec![PackFinding {
                        code: "TOWER-PACK-UNGUARDED-MUTATION".to_string(),
                        message: "LSP composition violates mutation expectations".to_string(),
                        severity: 1, // Error
                        line: 0,
                        intents: vec![],
                    }],
                    projection_signatures: vec![],
                    customization_points: vec![],
                    action_intents: vec![],
                });
            }
        }
        obs
    }
}

pub struct Registry {
    observers: Vec<Box<dyn PackObserver>>,
}

impl Registry {
    pub fn new() -> Self {
        Self { observers: Vec::new() }
    }
    
    pub fn register(&mut self, obs: Box<dyn PackObserver>) {
        self.observers.push(obs);
    }
    
    pub fn observe_all(&self, uri: &Url, content: &str) -> Vec<PackObservation> {
        let mut all_obs = Vec::new();
        for obs in &self.observers {
            all_obs.extend(obs.observe(uri, content));
        }
        all_obs
    }
}
