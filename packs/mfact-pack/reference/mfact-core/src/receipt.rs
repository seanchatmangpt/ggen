use crate::{Refusal, hash_bytes};
use serde::{Deserialize, Serialize};

/// A single fact in N-Quads format representation.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct Fact {
    pub subject: String,
    pub predicate: String,
    pub object: String,
    pub graph: Option<String>,
}

impl Fact {
    /// Returns the fact formatted as an N-Quad string.
    pub fn to_nquad(&self) -> String {
        match &self.graph {
            Some(g) => format!(
                "{} {} {} {} .",
                self.subject, self.predicate, self.object, g
            ),
            None => format!("{} {} {} .", self.subject, self.predicate, self.object),
        }
    }
}

/// The final deterministic receipt of the `ggen` engine.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Receipt {
    /// The final chained BLAKE3 hash of all canonical facts.
    pub execution_fold: String,
    /// Number of facts included in this execution fold.
    pub fact_count: usize,
}

/// Engine to generate precise cryptographic boundaries for execution flows.
/// Receipts are computed (BLAKE3), never asserted.
/// All facts are placed in canonical N-Quads order.
pub struct GgenReceiptEngine {
    seed: String,
}

impl GgenReceiptEngine {
    /// Creates a new `GgenReceiptEngine` bound to a specific seed.
    pub fn new(seed: &str) -> Result<Self, Refusal> {
        if seed.is_empty() {
            return Err(Refusal::ValidationFailed(
                "Seed cannot be empty".to_string(),
            ));
        }
        Ok(Self {
            seed: seed.to_string(),
        })
    }

    /// Computes a deterministic receipt by sorting facts into canonical N-Quads order,
    /// hashing each with BLAKE3, and chaining them with the seed.
    ///
    /// O(N log N) complexity due to sorting, where N is the number of facts.
    pub fn compute_receipt(&self, facts: &[Fact]) -> Result<Receipt, Refusal> {
        let mut nquads: Vec<String> = facts.iter().map(|f| f.to_nquad()).collect();

        // Canonical N-Quads order: sort lexicographically
        nquads.sort_unstable();

        // Refuse duplicate elements to maintain strict logical boundaries
        for i in 1..nquads.len() {
            if nquads[i] == nquads[i - 1] {
                return Err(Refusal::DuplicateElement(format!(
                    "Duplicate fact in receipt generation: {}",
                    nquads[i]
                )));
            }
        }

        let mut acc = hash_bytes(self.seed.as_bytes());

        for nquad in nquads.iter() {
            let fact_hash = hash_bytes(nquad.as_bytes());
            let mut combined = String::with_capacity(128); // 64 hex chars + 64 hex chars
            combined.push_str(&acc);
            combined.push_str(&fact_hash);
            acc = hash_bytes(combined.as_bytes());
        }

        Ok(Receipt {
            execution_fold: acc,
            fact_count: nquads.len(),
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_fact_to_nquad() {
        let f1 = Fact {
            subject: "<http://example.org/s>".to_string(),
            predicate: "<http://example.org/p>".to_string(),
            object: "\"Literal\"".to_string(),
            graph: None,
        };
        assert_eq!(
            f1.to_nquad(),
            "<http://example.org/s> <http://example.org/p> \"Literal\" ."
        );

        let f2 = Fact {
            subject: "<http://example.org/s>".to_string(),
            predicate: "<http://example.org/p>".to_string(),
            object: "\"Literal\"".to_string(),
            graph: Some("<http://example.org/g>".to_string()),
        };
        assert_eq!(
            f2.to_nquad(),
            "<http://example.org/s> <http://example.org/p> \"Literal\" <http://example.org/g> ."
        );
    }

    #[test]
    fn test_receipt_engine_empty_seed() {
        assert!(matches!(
            GgenReceiptEngine::new(""),
            Err(Refusal::ValidationFailed(_))
        ));
    }

    #[test]
    fn test_receipt_engine_determinism() {
        let engine = GgenReceiptEngine::new("test-seed").unwrap();

        let f1 = Fact {
            subject: "<s>".to_string(),
            predicate: "<p1>".to_string(),
            object: "\"o1\"".to_string(),
            graph: None,
        };
        let f2 = Fact {
            subject: "<s>".to_string(),
            predicate: "<p2>".to_string(),
            object: "\"o2\"".to_string(),
            graph: None,
        };

        // Regardless of input order, receipt should be identical
        let r1 = engine.compute_receipt(&[f1.clone(), f2.clone()]).unwrap();
        let r2 = engine.compute_receipt(&[f2.clone(), f1.clone()]).unwrap();

        assert_eq!(r1.execution_fold, r2.execution_fold);
        assert_eq!(r1.fact_count, 2);
    }

    #[test]
    fn test_receipt_engine_duplicate_rejection() {
        let engine = GgenReceiptEngine::new("test-seed").unwrap();

        let f1 = Fact {
            subject: "<s>".to_string(),
            predicate: "<p>".to_string(),
            object: "\"o\"".to_string(),
            graph: None,
        };

        // Exact duplicates should be refused
        let result = engine.compute_receipt(&[f1.clone(), f1.clone()]);
        assert!(matches!(result, Err(Refusal::DuplicateElement(_))));
    }
}
