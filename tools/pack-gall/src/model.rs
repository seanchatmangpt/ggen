use serde::{Deserialize, Serialize};
use std::collections::BTreeMap;

pub const CONTRACT_SCHEMA: &str = "ggen.pack-gall.contract.v1";
pub const OBSERVATION_SCHEMA: &str = "ggen.pack-equivalence.report.v1";
pub const VERIFIER_SCHEMA: &str = "ggen.verifier.report.v1";
pub const RECEIPT_SCHEMA: &str = "ggen.pack-gall.receipt.v1";

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct Contract {
    pub schema: String,
    pub required_surfaces: Vec<RequiredSurface>,
    pub command_surfaces: Vec<CommandContract>,
    pub schema_tokens: Vec<SchemaTokenContract>,
    pub catalog_path: String,
    pub canonical_schema_path: String,
    pub verifier_schema_path: String,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct RequiredSurface {
    pub path: String,
    pub owner: String,
    pub class: String,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct CommandContract {
    pub noun: String,
    pub path: String,
    pub required_verbs: Vec<String>,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct SchemaTokenContract {
    pub path: String,
    pub tokens: Vec<String>,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct SurfaceEvidence {
    pub path: String,
    pub owner: String,
    pub class: String,
    pub bytes: u64,
    pub blake3: String,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct CommandEvidence {
    pub noun: String,
    pub path: String,
    pub observed_verbs: Vec<String>,
    pub required_verbs: Vec<String>,
    pub missing_verbs: Vec<String>,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct SchemaEvidence {
    pub path: String,
    pub required_tokens: Vec<String>,
    pub missing_tokens: Vec<String>,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct Provider {
    pub id: String,
    #[serde(default)]
    pub aliases: Vec<String>,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct Group {
    pub id: String,
    pub directory: String,
    #[serde(default)]
    pub dependencies: Vec<String>,
    #[serde(default)]
    pub common_packs: Vec<String>,
    pub provider_packs: BTreeMap<String, Vec<String>>,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct Catalog {
    pub schema: String,
    pub version: String,
    pub providers: Vec<Provider>,
    pub groups: Vec<Group>,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct ResolutionEvidence {
    pub provider: String,
    pub requested_group: String,
    pub resolved_groups: Vec<String>,
    pub directories: Vec<String>,
    pub packs: Vec<String>,
    pub plan_digest: String,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct CorpusEvidence {
    pub catalog_schema: String,
    pub catalog_version: String,
    pub provider_ids: Vec<String>,
    pub group_count: usize,
    pub unique_pack_count: usize,
    pub catalog_digest: String,
    pub representative_resolutions: Vec<ResolutionEvidence>,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct Observation {
    pub schema: String,
    pub contract_digest: String,
    pub source_digest: String,
    pub surfaces: Vec<SurfaceEvidence>,
    pub command_matrix: Vec<CommandEvidence>,
    pub schema_matrix: Vec<SchemaEvidence>,
    pub ownership: BTreeMap<String, String>,
    pub corpus: CorpusEvidence,
    pub canonical_schema_digest: String,
    pub verifier_schema_digest: String,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct Checkpoint {
    pub id: String,
    pub title: String,
    pub passed: bool,
    pub state: String,
    pub evidence: Vec<String>,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct VerifierReport {
    pub schema: String,
    pub source_digest: String,
    pub observation_digest: String,
    pub checkpoints: Vec<Checkpoint>,
    pub standing: String,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct Receipt {
    pub schema: String,
    pub operation: String,
    pub previous_digest: String,
    pub artifacts: BTreeMap<String, String>,
    pub digest_algorithm: String,
    pub digest: String,
}
