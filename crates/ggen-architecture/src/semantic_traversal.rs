//! Deterministic semantic-traversal measurement for ggen manufacture.
//!
//! Repository count is topology. This module measures whether manufacture reuses
//! an admitted semantic fact identity or independently reinterprets it. It is
//! deliberately IO-free and has no actuation surface.

use serde::{Deserialize, Serialize};
use std::collections::BTreeMap;
use thiserror::Error;

pub const SEMANTIC_TRAVERSAL_RECEIPT_SCHEMA: &str = "ggen.semantic-traversal-receipt.v1";

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum SemanticUseMode {
    ProjectionReuse,
    IndependentInterpretation,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct SemanticFactUse {
    pub fact_id: String,
    pub semantic_digest: String,
    pub projection_id: String,
    pub mode: SemanticUseMode,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub struct ExactRatio {
    pub numerator: u64,
    pub denominator: u64,
}

impl ExactRatio {
    fn new(numerator: u64, denominator: u64) -> Option<Self> {
        (denominator != 0).then_some(Self {
            numerator,
            denominator,
        })
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct SemanticTraversalReport {
    pub semantic_fact_count: u64,
    pub interpretation_count: u64,
    pub projection_reuse_count: u64,
    pub mean_semantic_traversal: Option<ExactRatio>,
    pub projection_reuse_ratio: Option<ExactRatio>,
}

impl SemanticTraversalReport {
    pub fn from_uses(uses: &[SemanticFactUse]) -> Result<Self, SemanticTraversalRefusal> {
        let mut facts = BTreeMap::<&str, &str>::new();
        let mut interpretation_count = 0_u64;
        let mut projection_reuse_count = 0_u64;

        for usage in uses {
            if usage.fact_id.trim().is_empty() {
                return Err(SemanticTraversalRefusal::MissingFactIdentity);
            }
            if usage.semantic_digest.trim().is_empty() {
                return Err(SemanticTraversalRefusal::MissingSemanticDigest);
            }
            if usage.projection_id.trim().is_empty() {
                return Err(SemanticTraversalRefusal::MissingProjectionIdentity);
            }

            match facts.insert(usage.fact_id.as_str(), usage.semantic_digest.as_str()) {
                Some(previous) if previous != usage.semantic_digest => {
                    return Err(SemanticTraversalRefusal::SemanticIdentityDrift(
                        usage.fact_id.clone(),
                    ));
                }
                _ => {}
            }

            match usage.mode {
                SemanticUseMode::ProjectionReuse => projection_reuse_count += 1,
                SemanticUseMode::IndependentInterpretation => interpretation_count += 1,
            }
        }

        let semantic_fact_count = facts.len() as u64;
        let total_uses = interpretation_count + projection_reuse_count;

        Ok(Self {
            semantic_fact_count,
            interpretation_count,
            projection_reuse_count,
            mean_semantic_traversal: ExactRatio::new(interpretation_count, semantic_fact_count),
            projection_reuse_ratio: ExactRatio::new(projection_reuse_count, total_uses),
        })
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct SemanticTraversalReceipt {
    pub schema: String,
    pub exact_subject: String,
    pub manufacturer: String,
    pub report: SemanticTraversalReport,
    pub actuation_performed: bool,
    pub digest: String,
}

impl SemanticTraversalReceipt {
    pub fn manufacture(
        exact_subject: impl Into<String>, manufacturer: impl Into<String>, uses: &[SemanticFactUse],
    ) -> Result<Self, SemanticTraversalRefusal> {
        let exact_subject = exact_subject.into();
        let manufacturer = manufacturer.into();
        validate_exact_subject(&exact_subject)?;
        if !manufacturer.starts_with("repository:") || manufacturer.len() <= "repository:".len() {
            return Err(SemanticTraversalRefusal::InvalidManufacturer);
        }

        let report = SemanticTraversalReport::from_uses(uses)?;
        let digest = receipt_digest(&exact_subject, &manufacturer, &report, false)?;

        Ok(Self {
            schema: SEMANTIC_TRAVERSAL_RECEIPT_SCHEMA.to_string(),
            exact_subject,
            manufacturer,
            report,
            actuation_performed: false,
            digest,
        })
    }

    pub fn replay(&self) -> bool {
        if self.schema != SEMANTIC_TRAVERSAL_RECEIPT_SCHEMA || self.actuation_performed {
            return false;
        }
        if validate_exact_subject(&self.exact_subject).is_err() {
            return false;
        }
        if !self.manufacturer.starts_with("repository:") {
            return false;
        }

        receipt_digest(
            &self.exact_subject,
            &self.manufacturer,
            &self.report,
            self.actuation_performed,
        )
        .map(|digest| digest == self.digest)
        .unwrap_or(false)
    }
}

#[derive(Debug, Error, Clone, PartialEq, Eq)]
pub enum SemanticTraversalRefusal {
    #[error("REFUSED:INEXACT_SUBJECT")]
    InexactSubject,
    #[error("REFUSED:INVALID_MANUFACTURER")]
    InvalidManufacturer,
    #[error("REFUSED:SEMANTIC_FACT_WITHOUT_IDENTITY")]
    MissingFactIdentity,
    #[error("REFUSED:SEMANTIC_FACT_WITHOUT_DIGEST")]
    MissingSemanticDigest,
    #[error("REFUSED:PROJECTION_WITHOUT_IDENTITY")]
    MissingProjectionIdentity,
    #[error("REFUSED:SEMANTIC_IDENTITY_DRIFT:{0}")]
    SemanticIdentityDrift(String),
    #[error("REFUSED:SEMANTIC_TRAVERSAL_SERIALIZATION:{0}")]
    Serialization(String),
}

fn validate_exact_subject(subject: &str) -> Result<(), SemanticTraversalRefusal> {
    let (repository, sha) = subject
        .split_once('@')
        .ok_or(SemanticTraversalRefusal::InexactSubject)?;
    let mut repository_parts = repository.split('/');
    let owner = repository_parts.next().unwrap_or_default();
    let name = repository_parts.next().unwrap_or_default();
    if owner.is_empty()
        || name.is_empty()
        || repository_parts.next().is_some()
        || sha.len() != 40
        || !sha.chars().all(|character| character.is_ascii_hexdigit())
    {
        return Err(SemanticTraversalRefusal::InexactSubject);
    }
    Ok(())
}

fn receipt_digest(
    exact_subject: &str, manufacturer: &str, report: &SemanticTraversalReport,
    actuation_performed: bool,
) -> Result<String, SemanticTraversalRefusal> {
    let material = serde_json::to_vec(&(
        SEMANTIC_TRAVERSAL_RECEIPT_SCHEMA,
        exact_subject,
        manufacturer,
        report,
        actuation_performed,
    ))
    .map_err(|error| SemanticTraversalRefusal::Serialization(error.to_string()))?;
    Ok(blake3::hash(&material).to_hex().to_string())
}

#[cfg(test)]
mod tests {
    use super::*;

    fn usage(
        fact_id: &str, digest: &str, projection: &str, mode: SemanticUseMode,
    ) -> SemanticFactUse {
        SemanticFactUse {
            fact_id: fact_id.to_string(),
            semantic_digest: digest.to_string(),
            projection_id: projection.to_string(),
            mode,
        }
    }

    #[test]
    fn measures_reuse_without_collapsing_distinct_facts() {
        let report = SemanticTraversalReport::from_uses(&[
            usage(
                "capability:a",
                "sha256:a",
                "repo-one",
                SemanticUseMode::ProjectionReuse,
            ),
            usage(
                "capability:a",
                "sha256:a",
                "repo-two",
                SemanticUseMode::ProjectionReuse,
            ),
            usage(
                "capability:b",
                "sha256:b",
                "repo-three",
                SemanticUseMode::IndependentInterpretation,
            ),
        ])
        .unwrap();

        assert_eq!(report.semantic_fact_count, 2);
        assert_eq!(report.projection_reuse_count, 2);
        assert_eq!(report.interpretation_count, 1);
        assert_eq!(
            report.mean_semantic_traversal,
            Some(ExactRatio {
                numerator: 1,
                denominator: 2,
            })
        );
        assert_eq!(
            report.projection_reuse_ratio,
            Some(ExactRatio {
                numerator: 2,
                denominator: 3,
            })
        );
    }

    #[test]
    fn zero_denominator_is_unknown_not_zero() {
        let report = SemanticTraversalReport::from_uses(&[]).unwrap();
        assert_eq!(report.mean_semantic_traversal, None);
        assert_eq!(report.projection_reuse_ratio, None);
    }

    #[test]
    fn same_fact_with_different_digest_refuses() {
        let result = SemanticTraversalReport::from_uses(&[
            usage(
                "capability:a",
                "sha256:a",
                "repo-one",
                SemanticUseMode::ProjectionReuse,
            ),
            usage(
                "capability:a",
                "sha256:other",
                "repo-two",
                SemanticUseMode::ProjectionReuse,
            ),
        ]);
        assert_eq!(
            result,
            Err(SemanticTraversalRefusal::SemanticIdentityDrift(
                "capability:a".to_string()
            ))
        );
    }

    #[test]
    fn receipt_requires_exact_repository_head() {
        let result = SemanticTraversalReceipt::manufacture(
            "seanchatmangpt/ggen@main",
            "repository:ggen",
            &[],
        );
        assert_eq!(result, Err(SemanticTraversalRefusal::InexactSubject));
    }

    #[test]
    fn receipt_is_deterministic_and_replayable() {
        let uses = [usage(
            "capability:manufacture-with-ggen",
            "blake3:abc",
            "projection:ash-app",
            SemanticUseMode::ProjectionReuse,
        )];
        let subject = "seanchatmangpt/ggen@36caa86eba04de0eae1769e71142df86fd370854";
        let first =
            SemanticTraversalReceipt::manufacture(subject, "repository:ggen", &uses).unwrap();
        let second =
            SemanticTraversalReceipt::manufacture(subject, "repository:ggen", &uses).unwrap();
        assert_eq!(first.digest, second.digest);
        assert!(first.replay());
    }

    #[test]
    fn receipt_cannot_launder_actuation() {
        let mut receipt = SemanticTraversalReceipt::manufacture(
            "seanchatmangpt/ggen@36caa86eba04de0eae1769e71142df86fd370854",
            "repository:ggen",
            &[],
        )
        .unwrap();
        receipt.actuation_performed = true;
        assert!(!receipt.replay());
    }
}
