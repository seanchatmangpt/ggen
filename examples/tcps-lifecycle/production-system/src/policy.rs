use serde::Deserialize;
use std::fmt;

#[derive(Debug, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct ReleaseDocument {
    pub release: ReleasePolicy,
    pub promotion: PromotionPolicy,
}

#[derive(Debug, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct ReleasePolicy {
    pub source: String,
    pub require_clean_tree: bool,
    pub require_changed_tests: bool,
    pub require_trybuild: bool,
    pub require_lean_kernel: bool,
    pub require_praxis_admission: bool,
    pub require_sbom: bool,
    pub require_provenance: bool,
    pub require_checksums: bool,
    pub require_signed_artifact: bool,
    pub immutable_digest: bool,
}

#[derive(Debug, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct PromotionPolicy {
    pub environments: Vec<String>,
    pub rebuild_between_environments: bool,
    pub require_same_digest: bool,
    pub require_receipt_at_each_transition: bool,
}

impl ReleaseDocument {
    pub fn parse(input: &str) -> Result<Self, PolicyRefusal> {
        let document: Self = toml::from_str(input).map_err(|_| PolicyRefusal::MalformedPolicy)?;
        document.validate()?;
        Ok(document)
    }

    pub fn validate(&self) -> Result<(), PolicyRefusal> {
        let release = &self.release;
        if release.source != "cargo-cicd"
            || !release.require_clean_tree
            || !release.require_changed_tests
            || !release.require_trybuild
            || !release.require_lean_kernel
            || !release.require_praxis_admission
            || !release.require_sbom
            || !release.require_provenance
            || !release.require_checksums
            || !release.require_signed_artifact
            || !release.immutable_digest
        {
            return Err(PolicyRefusal::ReleaseRailIncomplete);
        }
        let expected = ["development", "integration", "preproduction", "production"];
        if self.promotion.environments.iter().map(String::as_str).collect::<Vec<_>>() != expected
            || self.promotion.rebuild_between_environments
            || !self.promotion.require_same_digest
            || !self.promotion.require_receipt_at_each_transition
        {
            return Err(PolicyRefusal::PromotionRailIncomplete);
        }
        Ok(())
    }
}

#[derive(Debug, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct DeploymentDocument {
    pub deployment: DeploymentPolicy,
    pub authorization: DeploymentAuthorization,
}

#[derive(Debug, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct DeploymentPolicy {
    pub environment: String,
    pub artifact_digest_from: String,
    pub strategy: String,
    pub canary_percent: u8,
    pub promotion_requires_green_minutes: u32,
    pub rollback_on_error_rate_percent: f64,
    pub rollback_on_receipt_gap: bool,
    pub require_workload_identity: bool,
    pub require_read_only_root: bool,
    pub require_non_root: bool,
    pub require_network_policy: bool,
    pub require_observability: bool,
    pub require_deployment_receipt: bool,
}

#[derive(Debug, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct DeploymentAuthorization {
    pub selector: String,
    pub authorizer: String,
    pub executor: String,
    pub require_distinct_actors: bool,
}

impl DeploymentDocument {
    pub fn parse(input: &str) -> Result<Self, PolicyRefusal> {
        let document: Self = toml::from_str(input).map_err(|_| PolicyRefusal::MalformedPolicy)?;
        document.validate()?;
        Ok(document)
    }

    pub fn validate(&self) -> Result<(), PolicyRefusal> {
        let deployment = &self.deployment;
        if deployment.environment != "production"
            || deployment.artifact_digest_from != "receipts/release.json#/artifact_digest"
            || deployment.strategy != "canary"
            || deployment.canary_percent == 0
            || deployment.canary_percent > 10
            || deployment.promotion_requires_green_minutes < 30
            || !(0.0..=1.0).contains(&deployment.rollback_on_error_rate_percent)
            || !deployment.rollback_on_receipt_gap
            || !deployment.require_workload_identity
            || !deployment.require_read_only_root
            || !deployment.require_non_root
            || !deployment.require_network_policy
            || !deployment.require_observability
            || !deployment.require_deployment_receipt
        {
            return Err(PolicyRefusal::DeploymentRailIncomplete);
        }
        let authorization = &self.authorization;
        if authorization.selector.is_empty()
            || authorization.authorizer.is_empty()
            || authorization.executor.is_empty()
            || !authorization.require_distinct_actors
            || authorization.selector == authorization.authorizer
            || authorization.selector == authorization.executor
            || authorization.authorizer == authorization.executor
        {
            return Err(PolicyRefusal::AuthorityCollapse);
        }
        Ok(())
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PolicyRefusal {
    MalformedPolicy,
    ReleaseRailIncomplete,
    PromotionRailIncomplete,
    DeploymentRailIncomplete,
    AuthorityCollapse,
}

impl fmt::Display for PolicyRefusal {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(match self {
            Self::MalformedPolicy => "policy does not conform to its strict schema",
            Self::ReleaseRailIncomplete => "release policy omits a required evidence gate",
            Self::PromotionRailIncomplete => "promotion policy permits rebuild or digest drift",
            Self::DeploymentRailIncomplete => "deployment policy omits a production safety fence",
            Self::AuthorityCollapse => "deployment selection, authorization, and execution collapse",
        })
    }
}

impl std::error::Error for PolicyRefusal {}
