//! Receipt Chain Branching
//!
//! Git-like operations for contract execution history:
//! - Branch: Create parallel execution paths
//! - Merge: Combine branches with conflict detection
//! - Rebase: Replay receipts onto new base
//!
//! Useful for:
//! - A/B testing contract configurations
//! - Rollback scenarios
//! - Parallel execution with reconciliation

use crate::{MerkleProof, MerkleTree};
use chrono::{DateTime, Utc};
use mcp_core::crypto::{hash_sha256, hash_sha256_bytes};
use mcp_core::error::{McpError, McpResult};
use mcp_core::types::{ExecutionMetrics, Receipt};
use mcp_core::GENESIS_HASH;
use serde::{Deserialize, Serialize};
use std::collections::{HashMap, HashSet, VecDeque};

/// Default branch name
pub const DEFAULT_BRANCH: &str = "main";

/// Branch reference
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Branch {
    /// Branch name
    pub name: String,
    /// HEAD receipt hash (current tip of branch)
    pub head: String,
    /// Creation timestamp
    pub created_at: DateTime<Utc>,
    /// Branch point receipt hash (where this branch diverged)
    pub base: String,
    /// Parent branch name (None for the default branch)
    pub parent_branch: Option<String>,
}

impl Branch {
    /// Create a new branch
    pub fn new(name: impl Into<String>, head: impl Into<String>, base: impl Into<String>) -> Self {
        Self {
            name: name.into(),
            head: head.into(),
            created_at: Utc::now(),
            base: base.into(),
            parent_branch: None,
        }
    }

    /// Create a branch with parent reference
    pub fn with_parent(
        name: impl Into<String>,
        head: impl Into<String>,
        base: impl Into<String>,
        parent: impl Into<String>,
    ) -> Self {
        Self {
            name: name.into(),
            head: head.into(),
            created_at: Utc::now(),
            base: base.into(),
            parent_branch: Some(parent.into()),
        }
    }
}

/// Receipt chain with branching support
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct BranchingChain {
    /// All receipts indexed by their hash
    receipts: HashMap<String, Receipt>,
    /// Parent hash -> child hashes (one-to-many for branches)
    children: HashMap<String, Vec<String>>,
    /// All branches
    branches: HashMap<String, Branch>,
    /// Currently checked out branch
    current_branch: String,
}

impl Default for BranchingChain {
    fn default() -> Self {
        Self::new()
    }
}

impl BranchingChain {
    /// Create a new branching chain with default "main" branch
    pub fn new() -> Self {
        let main_branch = Branch::new(DEFAULT_BRANCH, GENESIS_HASH, GENESIS_HASH);
        let mut branches = HashMap::new();
        branches.insert(DEFAULT_BRANCH.to_string(), main_branch);

        Self {
            receipts: HashMap::new(),
            children: HashMap::new(),
            branches,
            current_branch: DEFAULT_BRANCH.to_string(),
        }
    }

    /// Get current branch name
    pub fn current_branch_name(&self) -> &str {
        &self.current_branch
    }

    /// Get current branch
    pub fn current_branch(&self) -> Option<&Branch> {
        self.branches.get(&self.current_branch)
    }

    /// Get current HEAD hash
    pub fn head(&self) -> String {
        self.branches
            .get(&self.current_branch)
            .map(|b| b.head.clone())
            .unwrap_or_else(|| GENESIS_HASH.to_string())
    }

    /// Get all branch names
    pub fn branch_names(&self) -> Vec<&str> {
        self.branches.keys().map(|s| s.as_str()).collect()
    }

    /// Create new branch from current HEAD
    pub fn branch(&mut self, name: &str) -> McpResult<Branch> {
        // Check if branch already exists
        if self.branches.contains_key(name) {
            return Err(McpError::ChainError(format!(
                "Branch '{}' already exists",
                name
            )));
        }

        let current_head = self.head();
        let branch = Branch::with_parent(name, &current_head, &current_head, &self.current_branch);

        self.branches.insert(name.to_string(), branch.clone());

        Ok(branch)
    }

    /// Create branch from a specific receipt hash
    pub fn branch_from(&mut self, name: &str, from_hash: &str) -> McpResult<Branch> {
        // Validate that the receipt exists (unless it's genesis)
        if from_hash != GENESIS_HASH && !self.receipts.contains_key(from_hash) {
            return Err(McpError::ChainError(format!(
                "Receipt '{}' not found",
                from_hash
            )));
        }

        if self.branches.contains_key(name) {
            return Err(McpError::ChainError(format!(
                "Branch '{}' already exists",
                name
            )));
        }

        let branch = Branch::with_parent(name, from_hash, from_hash, &self.current_branch);
        self.branches.insert(name.to_string(), branch.clone());

        Ok(branch)
    }

    /// Switch to existing branch
    pub fn checkout(&mut self, name: &str) -> McpResult<()> {
        if !self.branches.contains_key(name) {
            return Err(McpError::ChainError(format!(
                "Branch '{}' does not exist",
                name
            )));
        }

        self.current_branch = name.to_string();
        Ok(())
    }

    /// Add receipt to current branch
    pub fn add_receipt(&mut self, receipt: Receipt) -> McpResult<()> {
        let current_head = self.head();

        // Verify chain linkage
        if receipt.prev_hash != current_head {
            return Err(McpError::ChainError(format!(
                "Receipt prev_hash '{}' does not match current HEAD '{}'",
                receipt.prev_hash, current_head
            )));
        }

        let receipt_hash = receipt.receipt_hash.clone();

        // Add to children mapping
        self.children
            .entry(receipt.prev_hash.clone())
            .or_default()
            .push(receipt_hash.clone());

        // Store receipt
        self.receipts.insert(receipt_hash.clone(), receipt);

        // Update branch HEAD
        if let Some(branch) = self.branches.get_mut(&self.current_branch) {
            branch.head = receipt_hash;
        }

        Ok(())
    }

    /// Create and add a new receipt with auto-linking
    pub fn append_receipt(
        &mut self,
        operation: impl Into<String>,
        contract_id: impl Into<String>,
        input_hash: impl Into<String>,
        output_hash: impl Into<String>,
        metrics: ExecutionMetrics,
    ) -> McpResult<Receipt> {
        let current_head = self.head();
        let sequence = self.log().len() as u64 + 1;

        let receipt = Receipt::new(
            sequence,
            current_head,
            operation,
            contract_id,
            input_hash,
            output_hash,
            metrics,
        );

        self.add_receipt(receipt.clone())?;
        Ok(receipt)
    }

    /// Get a receipt by hash
    pub fn get_receipt(&self, hash: &str) -> Option<&Receipt> {
        self.receipts.get(hash)
    }

    /// Merge branch into current
    pub fn merge(&mut self, branch_name: &str) -> McpResult<MergeResult> {
        // Cannot merge branch into itself
        if branch_name == self.current_branch {
            return Err(McpError::ChainError(
                "Cannot merge branch into itself".to_string(),
            ));
        }

        // Get source branch
        let source_branch = self
            .branches
            .get(branch_name)
            .ok_or_else(|| McpError::ChainError(format!("Branch '{}' not found", branch_name)))?
            .clone();

        // Find common ancestor
        let merge_base = self
            .merge_base(&self.current_branch, branch_name)
            .ok_or_else(|| {
                McpError::ChainError("No common ancestor found for merge".to_string())
            })?;

        // Get receipts to merge (from merge_base to source HEAD, exclusive of base)
        // Clone to avoid borrow conflicts
        let source_receipts: Vec<Receipt> = self
            .get_receipts_since(&source_branch.head, &merge_base)
            .into_iter()
            .cloned()
            .collect();
        let current_receipts: Vec<Receipt> = self
            .get_receipts_since(&self.head(), &merge_base)
            .into_iter()
            .cloned()
            .collect();

        // Detect conflicts (convert Vec<Receipt> to &[&Receipt])
        let source_refs: Vec<&Receipt> = source_receipts.iter().collect();
        let current_refs: Vec<&Receipt> = current_receipts.iter().collect();
        let conflicts = self.detect_conflicts(&source_refs, &current_refs);

        if !conflicts.is_empty() {
            return Ok(MergeResult {
                success: false,
                merged_receipts: 0,
                conflicts,
                new_head: self.head(),
            });
        }

        // Fast-forward merge if current is at merge_base
        let current_head = self.head();
        let source_head = source_branch.head.clone();
        let source_receipts_count = source_receipts.len();

        if current_head == merge_base {
            // Fast-forward: just move HEAD to source HEAD
            if let Some(branch) = self.branches.get_mut(&self.current_branch) {
                branch.head = source_head.clone();
            }

            return Ok(MergeResult {
                success: true,
                merged_receipts: source_receipts_count,
                conflicts: vec![],
                new_head: source_head,
            });
        }

        // True merge: replay source receipts onto current
        let mut merged_count = 0;
        for source_receipt in source_receipts.iter().rev() {
            // Create new receipt with updated prev_hash
            let new_receipt = Receipt::new(
                (self.log().len() + 1) as u64,
                self.head(),
                &source_receipt.operation,
                &source_receipt.contract_id,
                &source_receipt.input_hash,
                &source_receipt.output_hash,
                source_receipt.metrics.clone(),
            );

            self.add_receipt(new_receipt)?;
            merged_count += 1;
        }

        Ok(MergeResult {
            success: true,
            merged_receipts: merged_count,
            conflicts: vec![],
            new_head: self.head(),
        })
    }

    /// Rebase current branch onto target
    pub fn rebase(&mut self, onto: &str) -> McpResult<RebaseResult> {
        // Get target branch
        let target_branch = self
            .branches
            .get(onto)
            .ok_or_else(|| McpError::ChainError(format!("Branch '{}' not found", onto)))?
            .clone();

        let current_branch_name = self.current_branch.clone();
        let current_branch = self
            .branches
            .get(&current_branch_name)
            .ok_or_else(|| McpError::ChainError("Current branch not found".to_string()))?
            .clone();

        // Find common ancestor
        let merge_base = self
            .merge_base(&current_branch_name, onto)
            .ok_or_else(|| McpError::ChainError("No common ancestor found for rebase".to_string()))?;

        // If already on target or no changes, nothing to do
        if current_branch.head == target_branch.head || current_branch.head == merge_base {
            return Ok(RebaseResult {
                success: true,
                replayed_receipts: 0,
                skipped_receipts: 0,
                new_head: target_branch.head,
            });
        }

        // Get receipts to replay (from current HEAD back to merge_base)
        // Clone to avoid borrow conflicts
        let receipts_to_replay: Vec<Receipt> = self
            .get_receipts_since(&current_branch.head, &merge_base)
            .into_iter()
            .cloned()
            .collect();
        let target_head = target_branch.head.clone();

        // Move HEAD to target
        if let Some(branch) = self.branches.get_mut(&self.current_branch) {
            branch.head = target_head.clone();
            branch.base = target_head.clone();
        }

        // Replay receipts
        let mut replayed = 0;
        let mut skipped = 0;

        for receipt in receipts_to_replay.iter().rev() {
            // Check if this receipt already exists in target history
            // (e.g., was cherry-picked or has same content)
            let content_hash = self.compute_content_hash(receipt);
            let already_exists = self.receipt_content_exists_since(&target_head, &content_hash);

            if already_exists {
                skipped += 1;
                continue;
            }

            // Create new receipt with updated sequence and prev_hash
            let new_receipt = Receipt::new(
                (self.log().len() + 1) as u64,
                self.head(),
                &receipt.operation,
                &receipt.contract_id,
                &receipt.input_hash,
                &receipt.output_hash,
                receipt.metrics.clone(),
            );

            self.add_receipt(new_receipt)?;
            replayed += 1;
        }

        Ok(RebaseResult {
            success: true,
            replayed_receipts: replayed,
            skipped_receipts: skipped,
            new_head: self.head(),
        })
    }

    /// Get receipts on current branch (ordered from oldest to newest)
    pub fn log(&self) -> Vec<&Receipt> {
        let head = self.head();
        self.get_ancestors(&head)
            .into_iter()
            .rev()
            .filter_map(|h| self.receipts.get(&h))
            .collect()
    }

    /// Get receipts between two points (exclusive of 'from', inclusive of 'to')
    pub fn log_range(&self, from: &str, to: &str) -> Vec<&Receipt> {
        self.get_receipts_since(to, from)
            .into_iter()
            .rev()
            .collect()
    }

    /// Find common ancestor of two branches
    pub fn merge_base(&self, branch_a: &str, branch_b: &str) -> Option<String> {
        let head_a = self.branches.get(branch_a)?.head.clone();
        let head_b = self.branches.get(branch_b)?.head.clone();

        // Get all ancestors of branch_a
        let ancestors_a: HashSet<String> = self.get_ancestors(&head_a).into_iter().collect();

        // Walk back from branch_b until we find a common ancestor
        let mut current = head_b;
        loop {
            if ancestors_a.contains(&current) {
                return Some(current);
            }

            // Move to parent
            if let Some(receipt) = self.receipts.get(&current) {
                current = receipt.prev_hash.clone();
            } else if current == GENESIS_HASH {
                // Genesis is always a common ancestor
                if ancestors_a.contains(&current) || current == GENESIS_HASH {
                    return Some(current);
                }
                return None;
            } else {
                return None;
            }
        }
    }

    /// Generate Merkle tree for branch
    pub fn branch_tree(&self, branch_name: &str) -> McpResult<MerkleTree> {
        let branch = self
            .branches
            .get(branch_name)
            .ok_or_else(|| McpError::ChainError(format!("Branch '{}' not found", branch_name)))?;

        let receipts = self.get_ancestors(&branch.head);
        let mut tree = MerkleTree::new();

        for hash in receipts.iter().rev() {
            let hash_bytes = hex::decode(hash)
                .map_err(|e| McpError::ChainError(format!("Invalid hash: {}", e)))?;
            if hash_bytes.len() != 32 {
                return Err(McpError::ChainError("Invalid hash length".to_string()));
            }
            let mut arr = [0u8; 32];
            arr.copy_from_slice(&hash_bytes);
            tree.add_leaf(arr);
        }

        tree.rebuild();
        Ok(tree)
    }

    /// Compute diff between two branches
    pub fn diff(&self, from: &str, to: &str) -> McpResult<ChainDiff> {
        let from_branch = self
            .branches
            .get(from)
            .ok_or_else(|| McpError::ChainError(format!("Branch '{}' not found", from)))?;
        let to_branch = self
            .branches
            .get(to)
            .ok_or_else(|| McpError::ChainError(format!("Branch '{}' not found", to)))?;

        let from_ancestors: HashSet<String> =
            self.get_ancestors(&from_branch.head).into_iter().collect();
        let to_ancestors: HashSet<String> =
            self.get_ancestors(&to_branch.head).into_iter().collect();

        let added: Vec<String> = to_ancestors.difference(&from_ancestors).cloned().collect();
        let removed: Vec<String> = from_ancestors.difference(&to_ancestors).cloned().collect();

        // Find modified (same content hash but different receipt hash)
        let mut modified = Vec::new();
        for added_hash in &added {
            if let Some(added_receipt) = self.receipts.get(added_hash) {
                let content_hash = self.compute_content_hash(added_receipt);
                for removed_hash in &removed {
                    if let Some(removed_receipt) = self.receipts.get(removed_hash) {
                        let removed_content = self.compute_content_hash(removed_receipt);
                        // Same input but different output = modified
                        if added_receipt.input_hash == removed_receipt.input_hash
                            && added_receipt.output_hash != removed_receipt.output_hash
                        {
                            modified.push((removed_hash.clone(), added_hash.clone()));
                        }
                    }
                }
            }
        }

        Ok(ChainDiff {
            added,
            removed,
            modified,
        })
    }

    /// Cherry-pick specific receipt to current branch
    pub fn cherry_pick(&mut self, receipt_hash: &str) -> McpResult<Receipt> {
        let source_receipt = self
            .receipts
            .get(receipt_hash)
            .ok_or_else(|| McpError::ChainError(format!("Receipt '{}' not found", receipt_hash)))?
            .clone();

        // Create new receipt with updated linkage
        let new_receipt = Receipt::new(
            (self.log().len() + 1) as u64,
            self.head(),
            &source_receipt.operation,
            &source_receipt.contract_id,
            &source_receipt.input_hash,
            &source_receipt.output_hash,
            source_receipt.metrics.clone(),
        );

        self.add_receipt(new_receipt.clone())?;
        Ok(new_receipt)
    }

    /// Delete a branch (cannot delete current branch)
    pub fn delete_branch(&mut self, name: &str) -> McpResult<()> {
        if name == self.current_branch {
            return Err(McpError::ChainError(
                "Cannot delete current branch".to_string(),
            ));
        }

        if name == DEFAULT_BRANCH {
            return Err(McpError::ChainError(
                "Cannot delete default branch".to_string(),
            ));
        }

        self.branches.remove(name).ok_or_else(|| {
            McpError::ChainError(format!("Branch '{}' does not exist", name))
        })?;

        Ok(())
    }

    /// Get total number of receipts across all branches
    pub fn total_receipts(&self) -> usize {
        self.receipts.len()
    }

    /// Get proof for a receipt on a specific branch
    pub fn proof_for_receipt(
        &self,
        receipt_hash: &str,
        branch_name: &str,
    ) -> McpResult<MerkleProof> {
        let tree = self.branch_tree(branch_name)?;
        let ancestors = self.get_ancestors(
            &self
                .branches
                .get(branch_name)
                .ok_or_else(|| McpError::ChainError(format!("Branch '{}' not found", branch_name)))?
                .head,
        );

        let index = ancestors
            .iter()
            .rev()
            .position(|h| h == receipt_hash)
            .ok_or_else(|| {
                McpError::ChainError(format!(
                    "Receipt '{}' not found on branch '{}'",
                    receipt_hash, branch_name
                ))
            })?;

        tree.proof(index)
            .ok_or_else(|| McpError::MerkleError("Failed to generate proof".to_string()))
    }

    // --- Private helper methods ---

    /// Get all ancestor hashes from a given hash back to genesis
    fn get_ancestors(&self, from: &str) -> Vec<String> {
        let mut ancestors = Vec::new();
        let mut current = from.to_string();

        while current != GENESIS_HASH {
            ancestors.push(current.clone());
            if let Some(receipt) = self.receipts.get(&current) {
                current = receipt.prev_hash.clone();
            } else {
                break;
            }
        }

        ancestors
    }

    /// Get receipts from 'from' back to 'until' (exclusive of until)
    fn get_receipts_since(&self, from: &str, until: &str) -> Vec<&Receipt> {
        let mut receipts = Vec::new();
        let mut current = from.to_string();

        while current != until && current != GENESIS_HASH {
            if let Some(receipt) = self.receipts.get(&current) {
                receipts.push(receipt);
                current = receipt.prev_hash.clone();
            } else {
                break;
            }
        }

        receipts
    }

    /// Detect conflicts between two sets of receipts
    fn detect_conflicts(&self, source: &[&Receipt], target: &[&Receipt]) -> Vec<MergeConflict> {
        let mut conflicts = Vec::new();

        for s in source {
            for t in target {
                // Same sequence number conflict
                if s.sequence == t.sequence && s.receipt_hash != t.receipt_hash {
                    conflicts.push(MergeConflict {
                        receipt_a: s.receipt_hash.clone(),
                        receipt_b: t.receipt_hash.clone(),
                        conflict_type: ConflictType::SequenceConflict,
                    });
                }

                // Same input, different output (divergent execution)
                if s.input_hash == t.input_hash && s.output_hash != t.output_hash {
                    conflicts.push(MergeConflict {
                        receipt_a: s.receipt_hash.clone(),
                        receipt_b: t.receipt_hash.clone(),
                        conflict_type: ConflictType::DivergentOutput,
                    });
                }
            }
        }

        conflicts
    }

    /// Compute content hash for a receipt (excludes receipt_id and prev_hash)
    fn compute_content_hash(&self, receipt: &Receipt) -> String {
        let content = format!(
            "{}|{}|{}|{}",
            receipt.operation, receipt.contract_id, receipt.input_hash, receipt.output_hash
        );
        hash_sha256(content.as_bytes())
    }

    /// Check if a receipt with the same content exists since a given point
    fn receipt_content_exists_since(&self, from: &str, content_hash: &str) -> bool {
        let receipts = self.get_receipts_since(from, GENESIS_HASH);
        for receipt in receipts {
            if self.compute_content_hash(receipt) == content_hash {
                return true;
            }
        }
        false
    }
}

/// Result of a merge operation
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct MergeResult {
    /// Whether the merge succeeded
    pub success: bool,
    /// Number of receipts merged
    pub merged_receipts: usize,
    /// Conflicts that prevented merge (if any)
    pub conflicts: Vec<MergeConflict>,
    /// New HEAD after merge
    pub new_head: String,
}

/// A merge conflict
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct MergeConflict {
    /// Receipt hash from source branch
    pub receipt_a: String,
    /// Receipt hash from target branch
    pub receipt_b: String,
    /// Type of conflict
    pub conflict_type: ConflictType,
}

/// Types of merge conflicts
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub enum ConflictType {
    /// Same sequence number, different content
    SequenceConflict,
    /// Same input hash, different output
    DivergentOutput,
    /// Overlapping epoch ranges
    EpochOverlap,
}

/// Result of a rebase operation
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RebaseResult {
    /// Whether the rebase succeeded
    pub success: bool,
    /// Number of receipts replayed
    pub replayed_receipts: usize,
    /// Number of receipts skipped (already present)
    pub skipped_receipts: usize,
    /// New HEAD after rebase
    pub new_head: String,
}

/// Diff between two chain states
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ChainDiff {
    /// Receipt hashes added in target
    pub added: Vec<String>,
    /// Receipt hashes removed from source
    pub removed: Vec<String>,
    /// Modified receipts (old_hash, new_hash)
    pub modified: Vec<(String, String)>,
}

impl ChainDiff {
    /// Check if there are no changes
    pub fn is_empty(&self) -> bool {
        self.added.is_empty() && self.removed.is_empty() && self.modified.is_empty()
    }

    /// Total number of changes
    pub fn total_changes(&self) -> usize {
        self.added.len() + self.removed.len() + self.modified.len()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn create_test_receipt(
        sequence: u64,
        prev_hash: &str,
        operation: &str,
        input: &str,
        output: &str,
    ) -> Receipt {
        Receipt::new(
            sequence,
            prev_hash,
            operation,
            "test-contract",
            hash_sha256(input.as_bytes()),
            hash_sha256(output.as_bytes()),
            ExecutionMetrics::default(),
        )
    }

    #[test]
    fn test_new_branching_chain() {
        let chain = BranchingChain::new();

        assert_eq!(chain.current_branch_name(), DEFAULT_BRANCH);
        assert_eq!(chain.head(), GENESIS_HASH);
        assert_eq!(chain.total_receipts(), 0);
        assert!(chain.log().is_empty());
    }

    #[test]
    fn test_add_receipt() {
        let mut chain = BranchingChain::new();

        let receipt = create_test_receipt(1, GENESIS_HASH, "execute", "input1", "output1");
        let receipt_hash = receipt.receipt_hash.clone();

        chain.add_receipt(receipt).unwrap();

        assert_eq!(chain.head(), receipt_hash);
        assert_eq!(chain.total_receipts(), 1);
        assert_eq!(chain.log().len(), 1);
    }

    #[test]
    fn test_add_receipt_chain_linkage() {
        let mut chain = BranchingChain::new();

        let receipt1 = create_test_receipt(1, GENESIS_HASH, "execute", "input1", "output1");
        let receipt1_hash = receipt1.receipt_hash.clone();
        chain.add_receipt(receipt1).unwrap();

        let receipt2 = create_test_receipt(2, &receipt1_hash, "execute", "input2", "output2");
        chain.add_receipt(receipt2).unwrap();

        assert_eq!(chain.total_receipts(), 2);
        assert_eq!(chain.log().len(), 2);
    }

    #[test]
    fn test_add_receipt_wrong_prev_hash() {
        let mut chain = BranchingChain::new();

        let receipt = create_test_receipt(1, "wrong-hash", "execute", "input", "output");
        let result = chain.add_receipt(receipt);

        assert!(result.is_err());
        assert!(matches!(result.unwrap_err(), McpError::ChainError(_)));
    }

    #[test]
    fn test_create_branch() {
        let mut chain = BranchingChain::new();

        let receipt = create_test_receipt(1, GENESIS_HASH, "execute", "input", "output");
        let head = receipt.receipt_hash.clone();
        chain.add_receipt(receipt).unwrap();

        let branch = chain.branch("feature").unwrap();

        assert_eq!(branch.name, "feature");
        assert_eq!(branch.head, head);
        assert_eq!(branch.base, head);
        assert_eq!(branch.parent_branch, Some(DEFAULT_BRANCH.to_string()));
    }

    #[test]
    fn test_create_duplicate_branch_fails() {
        let mut chain = BranchingChain::new();
        chain.branch("feature").unwrap();

        let result = chain.branch("feature");
        assert!(result.is_err());
    }

    #[test]
    fn test_checkout_branch() {
        let mut chain = BranchingChain::new();

        let receipt = create_test_receipt(1, GENESIS_HASH, "execute", "input", "output");
        chain.add_receipt(receipt).unwrap();

        chain.branch("feature").unwrap();
        chain.checkout("feature").unwrap();

        assert_eq!(chain.current_branch_name(), "feature");
    }

    #[test]
    fn test_checkout_nonexistent_branch() {
        let mut chain = BranchingChain::new();
        let result = chain.checkout("nonexistent");

        assert!(result.is_err());
    }

    #[test]
    fn test_branch_divergence() {
        let mut chain = BranchingChain::new();

        // Add receipt to main
        let receipt1 = create_test_receipt(1, GENESIS_HASH, "execute", "input1", "output1");
        let common_hash = receipt1.receipt_hash.clone();
        chain.add_receipt(receipt1).unwrap();

        // Create and checkout feature branch
        chain.branch("feature").unwrap();
        chain.checkout("feature").unwrap();

        // Add receipt to feature branch
        let receipt2 = create_test_receipt(2, &common_hash, "execute", "feature-input", "feature-output");
        let feature_hash = receipt2.receipt_hash.clone();
        chain.add_receipt(receipt2).unwrap();

        // Switch back to main and add different receipt
        chain.checkout(DEFAULT_BRANCH).unwrap();
        let receipt3 = create_test_receipt(2, &common_hash, "execute", "main-input", "main-output");
        let main_hash = receipt3.receipt_hash.clone();
        chain.add_receipt(receipt3).unwrap();

        // Verify divergence
        assert_ne!(
            chain.branches.get("feature").unwrap().head,
            chain.branches.get(DEFAULT_BRANCH).unwrap().head
        );

        // Both branches should have 2 receipts
        chain.checkout("feature").unwrap();
        assert_eq!(chain.log().len(), 2);

        chain.checkout(DEFAULT_BRANCH).unwrap();
        assert_eq!(chain.log().len(), 2);
    }

    #[test]
    fn test_merge_base() {
        let mut chain = BranchingChain::new();

        let receipt1 = create_test_receipt(1, GENESIS_HASH, "execute", "input1", "output1");
        let common_hash = receipt1.receipt_hash.clone();
        chain.add_receipt(receipt1).unwrap();

        chain.branch("feature").unwrap();
        chain.checkout("feature").unwrap();

        let receipt2 = create_test_receipt(2, &common_hash, "execute", "input2", "output2");
        chain.add_receipt(receipt2).unwrap();

        let merge_base = chain.merge_base(DEFAULT_BRANCH, "feature").unwrap();
        assert_eq!(merge_base, common_hash);
    }

    #[test]
    fn test_fast_forward_merge() {
        let mut chain = BranchingChain::new();

        // Main stays at genesis
        chain.branch("feature").unwrap();
        chain.checkout("feature").unwrap();

        // Add receipts to feature
        let receipt1 = create_test_receipt(1, GENESIS_HASH, "execute", "input1", "output1");
        let hash1 = receipt1.receipt_hash.clone();
        chain.add_receipt(receipt1).unwrap();

        let receipt2 = create_test_receipt(2, &hash1, "execute", "input2", "output2");
        let feature_head = receipt2.receipt_hash.clone();
        chain.add_receipt(receipt2).unwrap();

        // Switch to main and merge
        chain.checkout(DEFAULT_BRANCH).unwrap();
        let result = chain.merge("feature").unwrap();

        assert!(result.success);
        assert_eq!(result.merged_receipts, 2);
        assert!(result.conflicts.is_empty());
        assert_eq!(chain.head(), feature_head);
    }

    #[test]
    fn test_merge_with_conflict() {
        let mut chain = BranchingChain::new();

        // Add common receipt
        let receipt1 = create_test_receipt(1, GENESIS_HASH, "execute", "shared-input", "output1");
        let common_hash = receipt1.receipt_hash.clone();
        chain.add_receipt(receipt1).unwrap();

        // Create feature branch
        chain.branch("feature").unwrap();

        // Add conflicting receipt to main (same input, different output)
        let receipt2 = create_test_receipt(2, &common_hash, "execute", "conflict-input", "main-output");
        chain.add_receipt(receipt2).unwrap();

        // Switch to feature and add conflicting receipt
        chain.checkout("feature").unwrap();
        let receipt3 = create_test_receipt(2, &common_hash, "execute", "conflict-input", "feature-output");
        chain.add_receipt(receipt3).unwrap();

        // Try to merge main into feature
        let result = chain.merge(DEFAULT_BRANCH).unwrap();

        assert!(!result.success);
        assert!(!result.conflicts.is_empty());
        assert!(result.conflicts.iter().any(|c| c.conflict_type == ConflictType::DivergentOutput));
    }

    #[test]
    fn test_rebase() {
        let mut chain = BranchingChain::new();

        // Add receipts to main
        let receipt1 = create_test_receipt(1, GENESIS_HASH, "execute", "main-1", "output1");
        let main_hash1 = receipt1.receipt_hash.clone();
        chain.add_receipt(receipt1).unwrap();

        // Create feature from main
        chain.branch("feature").unwrap();

        // Add more to main
        let receipt2 = create_test_receipt(2, &main_hash1, "execute", "main-2", "output2");
        let main_hash2 = receipt2.receipt_hash.clone();
        chain.add_receipt(receipt2).unwrap();

        // Switch to feature and add receipt
        chain.checkout("feature").unwrap();
        let receipt3 = create_test_receipt(2, &main_hash1, "execute", "feature-1", "output3");
        chain.add_receipt(receipt3).unwrap();

        // Rebase feature onto main
        let result = chain.rebase(DEFAULT_BRANCH).unwrap();

        assert!(result.success);
        assert_eq!(result.replayed_receipts, 1);
        assert_eq!(result.skipped_receipts, 0);

        // Feature should now have 3 receipts (main's 2 + feature's 1 replayed)
        assert_eq!(chain.log().len(), 3);
    }

    #[test]
    fn test_cherry_pick() {
        let mut chain = BranchingChain::new();

        // Add receipt to main
        let receipt1 = create_test_receipt(1, GENESIS_HASH, "execute", "input1", "output1");
        let hash1 = receipt1.receipt_hash.clone();
        chain.add_receipt(receipt1).unwrap();

        // Create feature and add receipt
        chain.branch("feature").unwrap();
        chain.checkout("feature").unwrap();

        let receipt2 = create_test_receipt(2, &hash1, "execute", "cherry-input", "cherry-output");
        let cherry_hash = receipt2.receipt_hash.clone();
        chain.add_receipt(receipt2).unwrap();

        // Go back to main and cherry-pick
        chain.checkout(DEFAULT_BRANCH).unwrap();
        let picked = chain.cherry_pick(&cherry_hash).unwrap();

        // Should have same content but different hash (different prev_hash)
        assert_ne!(picked.receipt_hash, cherry_hash);
        assert_eq!(picked.operation, "execute");
        assert_eq!(chain.log().len(), 2);
    }

    #[test]
    fn test_diff() {
        let mut chain = BranchingChain::new();

        // Add common receipt
        let receipt1 = create_test_receipt(1, GENESIS_HASH, "execute", "input1", "output1");
        let hash1 = receipt1.receipt_hash.clone();
        chain.add_receipt(receipt1).unwrap();

        // Create feature
        chain.branch("feature").unwrap();
        chain.checkout("feature").unwrap();

        // Add to feature
        let receipt2 = create_test_receipt(2, &hash1, "execute", "feature-input", "feature-output");
        chain.add_receipt(receipt2).unwrap();

        // Switch to main and add different
        chain.checkout(DEFAULT_BRANCH).unwrap();
        let receipt3 = create_test_receipt(2, &hash1, "execute", "main-input", "main-output");
        chain.add_receipt(receipt3).unwrap();

        let diff = chain.diff(DEFAULT_BRANCH, "feature").unwrap();

        // Feature has 1 receipt not in main, main has 1 not in feature
        assert_eq!(diff.added.len(), 1);
        assert_eq!(diff.removed.len(), 1);
    }

    #[test]
    fn test_log_range() {
        let mut chain = BranchingChain::new();

        let receipt1 = create_test_receipt(1, GENESIS_HASH, "execute", "input1", "output1");
        let hash1 = receipt1.receipt_hash.clone();
        chain.add_receipt(receipt1).unwrap();

        let receipt2 = create_test_receipt(2, &hash1, "execute", "input2", "output2");
        let hash2 = receipt2.receipt_hash.clone();
        chain.add_receipt(receipt2).unwrap();

        let receipt3 = create_test_receipt(3, &hash2, "execute", "input3", "output3");
        let hash3 = receipt3.receipt_hash.clone();
        chain.add_receipt(receipt3).unwrap();

        // Get receipts between hash1 and hash3 (should be 2 and 3)
        let range = chain.log_range(&hash1, &hash3);

        assert_eq!(range.len(), 2);
        assert_eq!(range[0].sequence, 2);
        assert_eq!(range[1].sequence, 3);
    }

    #[test]
    fn test_branch_tree() {
        let mut chain = BranchingChain::new();

        let receipt1 = create_test_receipt(1, GENESIS_HASH, "execute", "input1", "output1");
        let hash1 = receipt1.receipt_hash.clone();
        chain.add_receipt(receipt1).unwrap();

        let receipt2 = create_test_receipt(2, &hash1, "execute", "input2", "output2");
        chain.add_receipt(receipt2).unwrap();

        let tree = chain.branch_tree(DEFAULT_BRANCH).unwrap();

        assert_eq!(tree.len(), 2);
        assert!(tree.root().is_some());
    }

    #[test]
    fn test_delete_branch() {
        let mut chain = BranchingChain::new();

        chain.branch("feature").unwrap();
        assert!(chain.branches.contains_key("feature"));

        chain.delete_branch("feature").unwrap();
        assert!(!chain.branches.contains_key("feature"));
    }

    #[test]
    fn test_cannot_delete_current_branch() {
        let mut chain = BranchingChain::new();

        chain.branch("feature").unwrap();
        chain.checkout("feature").unwrap();

        let result = chain.delete_branch("feature");
        assert!(result.is_err());
    }

    #[test]
    fn test_cannot_delete_default_branch() {
        let mut chain = BranchingChain::new();

        let result = chain.delete_branch(DEFAULT_BRANCH);
        assert!(result.is_err());
    }

    #[test]
    fn test_append_receipt() {
        let mut chain = BranchingChain::new();

        let receipt = chain
            .append_receipt(
                "execute",
                "contract-1",
                "input-hash",
                "output-hash",
                ExecutionMetrics::default(),
            )
            .unwrap();

        assert_eq!(receipt.sequence, 1);
        assert_eq!(receipt.prev_hash, GENESIS_HASH);
        assert_eq!(chain.log().len(), 1);
    }

    #[test]
    fn test_branch_from_specific_hash() {
        let mut chain = BranchingChain::new();

        let receipt1 = create_test_receipt(1, GENESIS_HASH, "execute", "input1", "output1");
        let hash1 = receipt1.receipt_hash.clone();
        chain.add_receipt(receipt1).unwrap();

        let receipt2 = create_test_receipt(2, &hash1, "execute", "input2", "output2");
        chain.add_receipt(receipt2).unwrap();

        // Branch from first receipt, not HEAD
        let branch = chain.branch_from("feature", &hash1).unwrap();

        assert_eq!(branch.head, hash1);
        assert_eq!(branch.base, hash1);
    }

    #[test]
    fn test_proof_for_receipt() {
        let mut chain = BranchingChain::new();

        let receipt1 = create_test_receipt(1, GENESIS_HASH, "execute", "input1", "output1");
        let hash1 = receipt1.receipt_hash.clone();
        chain.add_receipt(receipt1).unwrap();

        let receipt2 = create_test_receipt(2, &hash1, "execute", "input2", "output2");
        let hash2 = receipt2.receipt_hash.clone();
        chain.add_receipt(receipt2).unwrap();

        let proof = chain.proof_for_receipt(&hash1, DEFAULT_BRANCH).unwrap();

        assert!(proof.verify());
    }

    #[test]
    fn test_merge_into_self_fails() {
        let mut chain = BranchingChain::new();

        let result = chain.merge(DEFAULT_BRANCH);
        assert!(result.is_err());
    }

    #[test]
    fn test_chain_diff_is_empty() {
        let diff = ChainDiff {
            added: vec![],
            removed: vec![],
            modified: vec![],
        };

        assert!(diff.is_empty());
        assert_eq!(diff.total_changes(), 0);
    }

    #[test]
    fn test_chain_diff_total_changes() {
        let diff = ChainDiff {
            added: vec!["a".to_string(), "b".to_string()],
            removed: vec!["c".to_string()],
            modified: vec![("d".to_string(), "e".to_string())],
        };

        assert!(!diff.is_empty());
        assert_eq!(diff.total_changes(), 4);
    }
}

#[cfg(test)]
mod proptests {
    use super::*;
    use proptest::prelude::*;

    proptest! {
        #[test]
        fn prop_branch_names_are_unique(names: Vec<String>) {
            prop_assume!(!names.is_empty());
            prop_assume!(names.len() <= 10);

            let unique_names: Vec<_> = names.into_iter()
                .filter(|n| !n.is_empty() && n != DEFAULT_BRANCH)
                .collect::<std::collections::HashSet<_>>()
                .into_iter()
                .take(5)
                .collect();

            let mut chain = BranchingChain::new();

            for name in &unique_names {
                let _ = chain.branch(name);
            }

            // All successfully created branches should be retrievable
            for name in &unique_names {
                if chain.branches.contains_key(name) {
                    prop_assert!(chain.checkout(name).is_ok());
                }
            }
        }

        #[test]
        fn prop_add_receipt_updates_head(
            operations: Vec<String>,
        ) {
            prop_assume!(!operations.is_empty());
            prop_assume!(operations.len() <= 10);

            let mut chain = BranchingChain::new();
            let mut prev_hash = GENESIS_HASH.to_string();

            for (i, op) in operations.iter().enumerate() {
                let receipt = Receipt::new(
                    (i + 1) as u64,
                    &prev_hash,
                    op,
                    "test-contract",
                    hash_sha256(format!("input-{}", i).as_bytes()),
                    hash_sha256(format!("output-{}", i).as_bytes()),
                    ExecutionMetrics::default(),
                );
                let new_hash = receipt.receipt_hash.clone();
                chain.add_receipt(receipt).unwrap();
                prev_hash = new_hash.clone();

                prop_assert_eq!(chain.head(), new_hash);
            }

            prop_assert_eq!(chain.log().len(), operations.len());
        }

        #[test]
        fn prop_merge_base_symmetric(seed: u64) {
            let mut chain = BranchingChain::new();

            // Add common receipt
            let receipt = Receipt::new(
                1,
                GENESIS_HASH,
                "execute",
                "contract",
                hash_sha256(b"input"),
                hash_sha256(b"output"),
                ExecutionMetrics::default(),
            );
            chain.add_receipt(receipt).unwrap();

            // Create branch
            chain.branch("feature").unwrap();

            // merge_base should be symmetric
            let base_a = chain.merge_base(DEFAULT_BRANCH, "feature");
            let base_b = chain.merge_base("feature", DEFAULT_BRANCH);

            prop_assert_eq!(base_a, base_b);
        }

        #[test]
        fn prop_log_order_is_chronological(count in 1usize..=20) {
            let mut chain = BranchingChain::new();
            let mut prev_hash = GENESIS_HASH.to_string();

            for i in 0..count {
                let receipt = Receipt::new(
                    (i + 1) as u64,
                    &prev_hash,
                    "execute",
                    "contract",
                    hash_sha256(format!("input-{}", i).as_bytes()),
                    hash_sha256(format!("output-{}", i).as_bytes()),
                    ExecutionMetrics::default(),
                );
                prev_hash = receipt.receipt_hash.clone();
                chain.add_receipt(receipt).unwrap();
            }

            let log = chain.log();
            prop_assert_eq!(log.len(), count);

            // Verify chronological order (sequence numbers should be increasing)
            for (i, receipt) in log.iter().enumerate() {
                prop_assert_eq!(receipt.sequence, (i + 1) as u64);
            }
        }
    }
}
