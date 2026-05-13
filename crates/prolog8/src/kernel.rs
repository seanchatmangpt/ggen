use crate::ids::PredicateId;
use crate::types::*;
use anyhow::Result;
use std::collections::HashMap;

pub struct Prolog8Kernel {
    pub catalog: HashMap<PredicateId, PredicateMetadata>,
    pub fact_blocks: HashMap<PredicateId, Vec<FactBlock8>>,
}

pub struct PredicateMetadata {
    pub arity: u8,
    pub access_paths: [AccessPathType; 256],
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AccessPathType {
    ExactLookup,
    PrefixRangeScan,
    ColumnScan,
    IndexJoinSeed,
    FullScan,
    MaterializedView,
    Rejected,
}

impl Default for Prolog8Kernel {
    fn default() -> Self {
        Self::new()
    }
}

impl Prolog8Kernel {
    pub fn new() -> Self {
        Self {
            catalog: HashMap::new(),
            fact_blocks: HashMap::new(),
        }
    }

    pub fn load_catalog(&mut self, catalog: HashMap<PredicateId, PredicateMetadata>) {
        self.catalog = catalog;
    }

    pub fn load_fact_block(&mut self, block: FactBlock8) {
        self.fact_blocks
            .entry(block.pred_id)
            .or_default()
            .push(block);
    }

    /// Evaluates a query and returns a receipt and proof.
    pub fn query(&self, query: QueryAtom8) -> Result<(DecisionKind, Receipt8, Option<ProofNode8>)> {
        // Enforce constraints (FR-3: Arity Cap)
        if query.atom.arity > 8 {
            return Ok((
                DecisionKind::Invalid,
                self.dummy_receipt(DecisionKind::Invalid),
                None,
            ));
        }

        let pred_meta = match self.catalog.get(&query.atom.pred_id) {
            Some(meta) => meta,
            None => {
                // Negative Proof: missing fact/predicate
                let receipt = self.dummy_receipt(DecisionKind::Deny);
                return Ok((
                    DecisionKind::Deny,
                    receipt,
                    self.negative_proof(query.atom.pred_id, ProofKind::MissingFact),
                ));
            }
        };

        // FR-6: Binding-Mask Dispatch
        let access_path = pred_meta.access_paths[query.atom.binding_mask as usize];
        if access_path == AccessPathType::Rejected {
            let receipt = self.dummy_receipt(DecisionKind::Invalid);
            return Ok((DecisionKind::Invalid, receipt, None));
        }

        // Mock execution lookup
        let mut found = false;
        if let Some(blocks) = self.fact_blocks.get(&query.atom.pred_id) {
            for block in blocks {
                // Evaluate based on binding mask
                for row in &block.rows {
                    let mut matched = true;
                    for i in 0..query.atom.arity as usize {
                        if (query.atom.binding_mask & (1 << i)) != 0
                            && row.args[i] != query.atom.args[i]
                        {
                            matched = false;
                            break;
                        }
                    }
                    if matched {
                        found = true;
                        break;
                    }
                }
                if found {
                    break;
                }
            }
        }

        let decision = if found {
            DecisionKind::Allow
        } else {
            DecisionKind::Deny
        };
        let receipt = self.dummy_receipt(decision);

        let proof = if decision == DecisionKind::Allow
            && (query.proof_mode == ProofMode::Positive || query.proof_mode == ProofMode::Full)
        {
            Some(ProofNode8 {
                node_id: 1,
                kind: ProofKind::Fact,
                pred_id: query.atom.pred_id,
                rule_id: None,
                fact_hash: None,
                children: [0; 8],
                child_count: 0,
                node_hash: [0; 32],
            })
        } else if decision == DecisionKind::Deny
            && (query.proof_mode == ProofMode::Negative || query.proof_mode == ProofMode::Full)
        {
            self.negative_proof(query.atom.pred_id, ProofKind::MissingFact)
        } else {
            None
        };

        Ok((decision, receipt, proof))
    }

    fn negative_proof(&self, pred_id: PredicateId, kind: ProofKind) -> Option<ProofNode8> {
        Some(ProofNode8 {
            node_id: 2,
            kind,
            pred_id,
            rule_id: None,
            fact_hash: None,
            children: [0; 8],
            child_count: 0,
            node_hash: [0; 32],
        })
    }

    fn dummy_receipt(&self, decision: DecisionKind) -> Receipt8 {
        Receipt8 {
            engine_version: "0.1.0".to_string(),
            catalog_root: [0; 32],
            rule_root: [0; 32],
            fact_root: [0; 32],
            input_root: [0; 32],
            proof_root: [0; 32],
            output_root: [0; 32],
            decision,
            epoch: 0,
            receipt_hash: [0; 32],
        }
    }
}
