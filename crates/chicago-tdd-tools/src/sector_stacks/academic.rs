//! Academic Publishing Workflow
//!
//! Demonstrates the Chatman Equation applied to paper review workflow.
//! Complete pipeline: Submission → Desk Review → Reviewer Assignment →
//! Review Collection → Decision → Notification

use crate::sector_stacks::{OperationReceipt, OperationStatus, SectorOperation};
use hex;
use serde::{Deserialize, Serialize};
use sha2::{Digest, Sha256};

/// Paper submission
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PaperSubmission {
    /// Unique identifier for the paper
    pub paper_id: String,
    /// Paper title
    pub title: String,
    /// List of authors
    pub authors: Vec<String>,
    /// Paper abstract
    pub abstract_text: String,
    /// File size in bytes
    pub file_size_bytes: usize,
}

/// Reviewer assignment
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ReviewerAssignment {
    /// Paper identifier
    pub paper_id: String,
    /// Assigned reviewers
    pub reviewers: Vec<String>,
    /// Assignment date
    pub assignment_date: String,
}

/// Paper review
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Review {
    /// Reviewer identifier
    pub reviewer: String,
    /// Review score (0-5)
    pub score: f64,
    /// Review comments
    pub comments: String,
    /// Reviewer recommendation
    pub recommendation: ReviewRecommendation,
}

/// Reviewer recommendation
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum ReviewRecommendation {
    /// Accept paper
    Accept,
    /// Minor revisions needed
    MinorRevisions,
    /// Major revisions needed
    MajorRevisions,
    /// Reject paper
    Reject,
}

/// Editorial decision
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum Decision {
    /// Paper accepted
    Accepted,
    /// Minor revisions required
    MinorRevisions,
    /// Major revisions required
    MajorRevisions,
    /// Paper rejected
    Rejected,
}

impl Decision {
    /// Determine decision from reviews
    #[must_use]
    pub fn from_reviews(reviews: &[Review]) -> Self {
        if reviews.is_empty() {
            return Self::Rejected;
        }

        #[allow(clippy::cast_precision_loss)] // Precision loss acceptable for average calculation
        let avg_score = reviews.iter().map(|r| r.score).sum::<f64>() / reviews.len() as f64;
        let reject_count = reviews
            .iter()
            .filter(|r| r.recommendation == ReviewRecommendation::Reject)
            .count();

        // Deterministic decision algorithm
        if reject_count > 0 {
            Self::Rejected
        } else if avg_score >= 3.5 {
            Self::Accepted
        } else if avg_score >= 2.5 {
            Self::MinorRevisions
        } else {
            Self::MajorRevisions
        }
    }
}

/// Academic publishing operation
pub struct AcademicOperation {
    paper: PaperSubmission,
    reviews: Vec<Review>,
    decision: Decision,
}

impl AcademicOperation {
    /// Create new academic operation
    #[must_use]
    pub fn new(paper: PaperSubmission, reviews: Vec<Review>) -> Self {
        let decision = Decision::from_reviews(&reviews);
        Self {
            paper,
            reviews,
            decision,
        }
    }

    /// Get decision
    #[must_use]
    pub const fn decision(&self) -> Decision {
        self.decision
    }

    /// Generate reviewer assignment (deterministic)
    #[must_use]
    pub fn assign_reviewers(&self) -> ReviewerAssignment {
        // Deterministic assignment based on paper ID hash
        let mut hasher = Sha256::new();
        hasher.update(self.paper.paper_id.as_bytes());
        let hash = hasher.finalize();

        let hex = hex::encode(hash);
        let reviewers = vec![
            format!("reviewer_{}", &hex[0..8]),
            format!("reviewer_{}", &hex[8..16]),
            format!("reviewer_{}", &hex[16..24]),
        ];

        ReviewerAssignment {
            paper_id: self.paper.paper_id.clone(),
            reviewers,
            assignment_date: "2025-11-16".to_string(),
        }
    }

    /// Generate decision receipt
    #[must_use]
    pub fn generate_decision_receipt(&self) -> OperationReceipt {
        let decision_str = match self.decision() {
            Decision::Accepted => "Accepted",
            Decision::MinorRevisions => "Minor Revisions",
            Decision::MajorRevisions => "Major Revisions",
            Decision::Rejected => "Rejected",
        };

        let mut hasher = Sha256::new();
        hasher.update(self.paper.paper_id.as_bytes());
        for review in &self.reviews {
            hasher.update(review.score.to_string().as_bytes());
        }
        hasher.update(decision_str.as_bytes());
        let hash = hasher.finalize();

        OperationReceipt {
            id: format!("academic-{}", self.paper.paper_id),
            sector: "Academic".to_string(),
            operation: "Decision".to_string(),
            status: OperationStatus::Success,
            result: {
                #[allow(clippy::cast_precision_loss)]
                // Precision loss acceptable for average calculation
                let avg =
                    self.reviews.iter().map(|r| r.score).sum::<f64>() / self.reviews.len() as f64;
                format!("Decision: {decision_str} (Avg Score: {avg:.1})")
            },
            merkle_root: hex::encode(hash),
            timestamp: "2025-11-16T00:00:00Z".to_string(),
        }
    }
}

impl SectorOperation for AcademicOperation {
    fn sector_name(&self) -> &'static str {
        "Academic Publishing"
    }

    fn description(&self) -> &'static str {
        "Paper review and publication workflow"
    }

    fn is_deterministic(&self) -> bool {
        true // Decision is deterministic given reviews
    }

    fn generate_receipt(&self, _status: OperationStatus) -> OperationReceipt {
        self.generate_decision_receipt()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_decision_from_reviews_accept() {
        let reviews = vec![
            Review {
                reviewer: "reviewer1".to_string(),
                score: 4.0,
                comments: "Excellent work".to_string(),
                recommendation: ReviewRecommendation::Accept,
            },
            Review {
                reviewer: "reviewer2".to_string(),
                score: 3.8,
                comments: "Good contribution".to_string(),
                recommendation: ReviewRecommendation::Accept,
            },
        ];

        assert_eq!(Decision::from_reviews(&reviews), Decision::Accepted);
    }

    #[test]
    fn test_decision_from_reviews_reject() {
        let reviews = vec![Review {
            reviewer: "reviewer1".to_string(),
            score: 2.0,
            comments: "Not suitable".to_string(),
            recommendation: ReviewRecommendation::Reject,
        }];

        assert_eq!(Decision::from_reviews(&reviews), Decision::Rejected);
    }

    #[test]
    fn test_reviewer_assignment_deterministic() {
        let paper = PaperSubmission {
            paper_id: "paper-123".to_string(),
            title: "Test Paper".to_string(),
            authors: vec!["Author".to_string()],
            abstract_text: "Abstract".to_string(),
            file_size_bytes: 1000,
        };

        let reviews = vec![];
        let op1 = AcademicOperation::new(paper.clone(), reviews.clone());
        let op2 = AcademicOperation::new(paper, reviews);

        let assign1 = op1.assign_reviewers();
        let assign2 = op2.assign_reviewers();

        // Same paper should get same reviewers
        assert_eq!(assign1.reviewers, assign2.reviewers);
    }

    #[test]
    fn test_receipt_generation() {
        let paper = PaperSubmission {
            paper_id: "paper-456".to_string(),
            title: "Test Paper".to_string(),
            authors: vec!["Author".to_string()],
            abstract_text: "Abstract".to_string(),
            file_size_bytes: 1000,
        };

        let reviews = vec![Review {
            reviewer: "reviewer1".to_string(),
            score: 3.5,
            comments: "Good".to_string(),
            recommendation: ReviewRecommendation::Accept,
        }];

        let op = AcademicOperation::new(paper, reviews);
        let receipt = op.generate_receipt(OperationStatus::Success);

        assert_eq!(receipt.sector, "Academic");
        assert_eq!(receipt.status, OperationStatus::Success);
        assert!(!receipt.merkle_root.is_empty());
    }
}
