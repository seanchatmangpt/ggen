//! Cross-Contract Proof Verification
//!
//! Proves that Contract B's input was Contract A's output
//! without revealing the actual data (text-blind linking).
//!
//! # Architecture
//!
//! This module provides cryptographic proof that data flows correctly between
//! contracts in a multi-contract pipeline. Three verification strategies:
//!
//! 1. **Direct**: Hash equality (same bytes flow unchanged)
//! 2. **Transformed**: Verified transformation (e.g., JSON subset extraction)
//! 3. **MerkleBridge**: Combined tree proving both receipts are authentic
//!
//! # Example
//!
//! ```
//! use mcp_merkle::cross::{DataFlowGraph, LinkProof};
//! use mcp_core::crypto::hash_sha256_bytes;
//!
//! // Build a data flow: ContractA -> ContractB -> ContractC
//! let mut graph = DataFlowGraph::new();
//! graph.add_contract("contract-a");
//! graph.add_contract("contract-b");
//! graph.add_contract("contract-c");
//!
//! // Link with direct hash equality
//! let hash = hash_sha256_bytes(b"shared-data");
//! let link = graph.link(
//!     "contract-a", "contract-b",
//!     LinkProof::Direct { hash }
//! ).unwrap();
//!
//! // Verify the entire flow
//! let verification = graph.verify().unwrap();
//! assert!(verification.valid);
//! ```

use crate::{MerkleProof, MerkleTree};
use mcp_core::crypto::{combine_hashes, hash_sha256_bytes};
use mcp_core::error::{McpError, McpResult};
use serde::{Deserialize, Serialize};
use std::collections::{HashMap, HashSet};

/// Link between two contracts proving data flow
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ContractLink {
    /// Unique identifier for this link
    pub link_id: String,
    /// Source contract that produced output
    pub source_contract: String,
    /// Receipt ID from source contract
    pub source_receipt_id: String,
    /// Hash of source contract's output
    pub source_output_hash: [u8; 32],
    /// Destination contract that consumed input
    pub dest_contract: String,
    /// Receipt ID from destination contract
    pub dest_receipt_id: String,
    /// Hash of destination contract's input
    pub dest_input_hash: [u8; 32],
    /// Proof that the link is valid
    pub link_proof: LinkProof,
}

impl ContractLink {
    /// Create a new contract link
    pub fn new(
        source_contract: &str,
        source_receipt_id: &str,
        source_output_hash: [u8; 32],
        dest_contract: &str,
        dest_receipt_id: &str,
        dest_input_hash: [u8; 32],
        link_proof: LinkProof,
    ) -> Self {
        let link_id = format!(
            "link-{}-{}-{}",
            source_contract,
            dest_contract,
            hex::encode(&source_output_hash[..8])
        );

        Self {
            link_id,
            source_contract: source_contract.to_string(),
            source_receipt_id: source_receipt_id.to_string(),
            source_output_hash,
            dest_contract: dest_contract.to_string(),
            dest_receipt_id: dest_receipt_id.to_string(),
            dest_input_hash,
            link_proof,
        }
    }

    /// Verify this link's proof
    pub fn verify(&self) -> McpResult<bool> {
        match &self.link_proof {
            LinkProof::Direct { hash } => {
                // Direct: source output == dest input == proof hash
                let valid = self.source_output_hash == *hash
                    && self.dest_input_hash == *hash;
                Ok(valid)
            }
            LinkProof::Transformed {
                transform_id: _,
                source_hash,
                dest_hash,
                transform_proof,
            } => {
                // Transformed: verify the transformation proof
                // The transform_proof contains the transformation evidence
                // For now, we verify that the proof commits to both hashes
                let expected_commitment = combine_hashes(source_hash, dest_hash);
                let proof_hash = hash_sha256_bytes(transform_proof);

                let valid = self.source_output_hash == *source_hash
                    && self.dest_input_hash == *dest_hash
                    && proof_hash == expected_commitment;
                Ok(valid)
            }
            LinkProof::MerkleBridge {
                combined_root,
                source_proof,
                dest_proof,
            } => {
                // MerkleBridge: both proofs verify against the combined root
                let source_valid = source_proof.verify_against_root(combined_root);
                let dest_valid = dest_proof.verify_against_root(combined_root);

                // Also verify the proofs are for the correct hashes
                let source_hash_matches = source_proof.leaf == self.source_output_hash;
                let dest_hash_matches = dest_proof.leaf == self.dest_input_hash;

                Ok(source_valid && dest_valid && source_hash_matches && dest_hash_matches)
            }
        }
    }

    /// Get the link ID
    pub fn id(&self) -> &str {
        &self.link_id
    }
}

/// Proof types for linking contracts
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum LinkProof {
    /// Direct hash equality (same bytes unchanged)
    Direct {
        /// The shared hash value
        hash: [u8; 32],
    },
    /// Hash of transformed data (e.g., JSON subset)
    Transformed {
        /// Identifier for the transformation applied
        transform_id: String,
        /// Hash of source data before transformation
        source_hash: [u8; 32],
        /// Hash of destination data after transformation
        dest_hash: [u8; 32],
        /// Proof of valid transformation (e.g., JSON path proof)
        transform_proof: Vec<u8>,
    },
    /// Merkle proof linking into combined tree
    MerkleBridge {
        /// Root of the combined Merkle tree
        combined_root: [u8; 32],
        /// Proof for source output in combined tree
        source_proof: MerkleProof,
        /// Proof for dest input in combined tree
        dest_proof: MerkleProof,
    },
}

impl LinkProof {
    /// Create a direct link proof (hash equality)
    pub fn direct(data: &[u8]) -> Self {
        LinkProof::Direct {
            hash: hash_sha256_bytes(data),
        }
    }

    /// Create a direct link proof from a pre-computed hash
    pub fn direct_from_hash(hash: [u8; 32]) -> Self {
        LinkProof::Direct { hash }
    }

    /// Create a transformed link proof
    pub fn transformed(
        transform_id: &str,
        source_data: &[u8],
        dest_data: &[u8],
    ) -> Self {
        let source_hash = hash_sha256_bytes(source_data);
        let dest_hash = hash_sha256_bytes(dest_data);

        // Create transform proof by committing to both hashes
        let commitment = combine_hashes(&source_hash, &dest_hash);

        LinkProof::Transformed {
            transform_id: transform_id.to_string(),
            source_hash,
            dest_hash,
            transform_proof: commitment.to_vec(),
        }
    }

    /// Create a Merkle bridge proof
    pub fn merkle_bridge(
        combined_root: [u8; 32],
        source_proof: MerkleProof,
        dest_proof: MerkleProof,
    ) -> Self {
        LinkProof::MerkleBridge {
            combined_root,
            source_proof,
            dest_proof,
        }
    }

    /// Get the proof type name
    pub fn proof_type(&self) -> &'static str {
        match self {
            LinkProof::Direct { .. } => "direct",
            LinkProof::Transformed { .. } => "transformed",
            LinkProof::MerkleBridge { .. } => "merkle_bridge",
        }
    }
}

/// Builder for contract data flows
#[derive(Debug, Clone, Default)]
pub struct DataFlowGraph {
    /// Registered contracts
    contracts: HashSet<String>,
    /// Links between contracts
    links: Vec<ContractLink>,
    /// Index: source contract -> links from that contract
    source_index: HashMap<String, Vec<usize>>,
    /// Index: dest contract -> links to that contract
    dest_index: HashMap<String, Vec<usize>>,
}

impl DataFlowGraph {
    /// Create a new empty data flow graph
    pub fn new() -> Self {
        Self {
            contracts: HashSet::new(),
            links: Vec::new(),
            source_index: HashMap::new(),
            dest_index: HashMap::new(),
        }
    }

    /// Add a contract to the graph
    pub fn add_contract(&mut self, contract_id: &str) {
        self.contracts.insert(contract_id.to_string());
    }

    /// Check if a contract exists in the graph
    pub fn has_contract(&self, contract_id: &str) -> bool {
        self.contracts.contains(contract_id)
    }

    /// Get all registered contracts
    pub fn contracts(&self) -> impl Iterator<Item = &String> {
        self.contracts.iter()
    }

    /// Link output of source to input of dest
    pub fn link(
        &mut self,
        source: &str,
        dest: &str,
        proof: LinkProof,
    ) -> McpResult<ContractLink> {
        // Ensure both contracts are registered
        if !self.contracts.contains(source) {
            return Err(McpError::ContractError(format!(
                "Source contract '{}' not registered in graph",
                source
            )));
        }
        if !self.contracts.contains(dest) {
            return Err(McpError::ContractError(format!(
                "Destination contract '{}' not registered in graph",
                dest
            )));
        }

        // Extract hashes from proof
        let (source_output_hash, dest_input_hash) = match &proof {
            LinkProof::Direct { hash } => (*hash, *hash),
            LinkProof::Transformed {
                source_hash,
                dest_hash,
                ..
            } => (*source_hash, *dest_hash),
            LinkProof::MerkleBridge {
                source_proof,
                dest_proof,
                ..
            } => (source_proof.leaf, dest_proof.leaf),
        };

        // Generate receipt IDs (in real usage, these would come from actual receipts)
        let source_receipt_id = format!("receipt-{}-{}", source, hex::encode(&source_output_hash[..4]));
        let dest_receipt_id = format!("receipt-{}-{}", dest, hex::encode(&dest_input_hash[..4]));

        let link = ContractLink::new(
            source,
            &source_receipt_id,
            source_output_hash,
            dest,
            &dest_receipt_id,
            dest_input_hash,
            proof,
        );

        // Update indices
        let link_index = self.links.len();
        self.source_index
            .entry(source.to_string())
            .or_default()
            .push(link_index);
        self.dest_index
            .entry(dest.to_string())
            .or_default()
            .push(link_index);

        self.links.push(link.clone());
        Ok(link)
    }

    /// Link with explicit receipt IDs and hashes
    pub fn link_with_receipts(
        &mut self,
        source: &str,
        source_receipt_id: &str,
        source_output_hash: [u8; 32],
        dest: &str,
        dest_receipt_id: &str,
        dest_input_hash: [u8; 32],
        proof: LinkProof,
    ) -> McpResult<ContractLink> {
        // Ensure both contracts are registered
        if !self.contracts.contains(source) {
            return Err(McpError::ContractError(format!(
                "Source contract '{}' not registered in graph",
                source
            )));
        }
        if !self.contracts.contains(dest) {
            return Err(McpError::ContractError(format!(
                "Destination contract '{}' not registered in graph",
                dest
            )));
        }

        let link = ContractLink::new(
            source,
            source_receipt_id,
            source_output_hash,
            dest,
            dest_receipt_id,
            dest_input_hash,
            proof,
        );

        // Update indices
        let link_index = self.links.len();
        self.source_index
            .entry(source.to_string())
            .or_default()
            .push(link_index);
        self.dest_index
            .entry(dest.to_string())
            .or_default()
            .push(link_index);

        self.links.push(link.clone());
        Ok(link)
    }

    /// Verify entire data flow graph
    pub fn verify(&self) -> McpResult<FlowVerification> {
        let mut links_verified = 0;
        let mut broken_links = Vec::new();

        // Verify each link
        for link in &self.links {
            match link.verify() {
                Ok(true) => {
                    links_verified += 1;
                }
                Ok(false) => {
                    broken_links.push(link.link_id.clone());
                }
                Err(e) => {
                    broken_links.push(format!("{}: {}", link.link_id, e));
                }
            }
        }

        // Count contracts that have at least one valid connection
        let mut connected_contracts = HashSet::new();
        for link in &self.links {
            if link.verify().unwrap_or(false) {
                connected_contracts.insert(&link.source_contract);
                connected_contracts.insert(&link.dest_contract);
            }
        }
        let contracts_verified = connected_contracts.len();

        Ok(FlowVerification {
            valid: broken_links.is_empty(),
            contracts_verified,
            links_verified,
            broken_links,
        })
    }

    /// Find all paths from source to sink
    pub fn trace_flow(&self, source: &str, sink: &str) -> Vec<Vec<&ContractLink>> {
        let mut all_paths = Vec::new();
        let mut current_path = Vec::new();
        let mut visited = HashSet::new();

        self.dfs_trace(source, sink, &mut current_path, &mut visited, &mut all_paths);

        all_paths
    }

    /// DFS helper for trace_flow
    fn dfs_trace<'a>(
        &'a self,
        current: &str,
        sink: &str,
        current_path: &mut Vec<&'a ContractLink>,
        visited: &mut HashSet<String>,
        all_paths: &mut Vec<Vec<&'a ContractLink>>,
    ) {
        if current == sink {
            if !current_path.is_empty() {
                all_paths.push(current_path.clone());
            }
            return;
        }

        if visited.contains(current) {
            return; // Cycle detected, skip
        }

        visited.insert(current.to_string());

        // Find all links from current contract
        if let Some(link_indices) = self.source_index.get(current) {
            for &idx in link_indices {
                let link = &self.links[idx];
                current_path.push(link);
                self.dfs_trace(&link.dest_contract, sink, current_path, visited, all_paths);
                current_path.pop();
            }
        }

        visited.remove(current);
    }

    /// Generate combined Merkle tree of all receipts
    pub fn combined_tree(&self) -> MerkleTree {
        let mut tree = MerkleTree::new();

        // Add all unique hashes from links
        let mut seen_hashes = HashSet::new();

        for link in &self.links {
            if seen_hashes.insert(link.source_output_hash) {
                tree.add_leaf(link.source_output_hash);
            }
            if seen_hashes.insert(link.dest_input_hash) {
                tree.add_leaf(link.dest_input_hash);
            }
        }

        tree.rebuild();
        tree
    }

    /// Get all links
    pub fn links(&self) -> &[ContractLink] {
        &self.links
    }

    /// Get links from a specific contract
    pub fn links_from(&self, contract: &str) -> Vec<&ContractLink> {
        self.source_index
            .get(contract)
            .map(|indices| indices.iter().map(|&i| &self.links[i]).collect())
            .unwrap_or_default()
    }

    /// Get links to a specific contract
    pub fn links_to(&self, contract: &str) -> Vec<&ContractLink> {
        self.dest_index
            .get(contract)
            .map(|indices| indices.iter().map(|&i| &self.links[i]).collect())
            .unwrap_or_default()
    }

    /// Find source contracts (no incoming links)
    pub fn sources(&self) -> Vec<&String> {
        self.contracts
            .iter()
            .filter(|c| !self.dest_index.contains_key(*c))
            .collect()
    }

    /// Find sink contracts (no outgoing links)
    pub fn sinks(&self) -> Vec<&String> {
        self.contracts
            .iter()
            .filter(|c| !self.source_index.contains_key(*c))
            .collect()
    }

    /// Get the number of contracts
    pub fn contract_count(&self) -> usize {
        self.contracts.len()
    }

    /// Get the number of links
    pub fn link_count(&self) -> usize {
        self.links.len()
    }
}

/// Result of verifying a data flow graph
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FlowVerification {
    /// Whether the entire flow is valid
    pub valid: bool,
    /// Number of contracts with valid connections
    pub contracts_verified: usize,
    /// Number of valid links
    pub links_verified: usize,
    /// List of broken link IDs
    pub broken_links: Vec<String>,
}

impl FlowVerification {
    /// Check if verification passed
    pub fn is_valid(&self) -> bool {
        self.valid
    }

    /// Get summary message
    pub fn summary(&self) -> String {
        if self.valid {
            format!(
                "Flow valid: {} contracts, {} links verified",
                self.contracts_verified, self.links_verified
            )
        } else {
            format!(
                "Flow invalid: {} broken links: {:?}",
                self.broken_links.len(),
                self.broken_links
            )
        }
    }
}

/// Create a Merkle bridge proof between two contract outputs
pub fn create_merkle_bridge(
    source_hash: [u8; 32],
    dest_hash: [u8; 32],
    additional_leaves: &[[u8; 32]],
) -> McpResult<LinkProof> {
    let mut tree = MerkleTree::new();

    // Add source and dest first
    tree.add_leaf(source_hash);
    tree.add_leaf(dest_hash);

    // Add any additional context
    for leaf in additional_leaves {
        tree.add_leaf(*leaf);
    }

    tree.rebuild();

    let combined_root = tree.root().ok_or_else(|| {
        McpError::MerkleError("Failed to compute combined root".to_string())
    })?;

    let source_proof = tree.proof(0).ok_or_else(|| {
        McpError::MerkleError("Failed to generate source proof".to_string())
    })?;

    let dest_proof = tree.proof(1).ok_or_else(|| {
        McpError::MerkleError("Failed to generate dest proof".to_string())
    })?;

    Ok(LinkProof::MerkleBridge {
        combined_root,
        source_proof,
        dest_proof,
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_direct_link_verification() {
        let data = b"shared-data-between-contracts";
        let hash = hash_sha256_bytes(data);

        let link = ContractLink::new(
            "contract-a",
            "receipt-a-001",
            hash,
            "contract-b",
            "receipt-b-001",
            hash,
            LinkProof::Direct { hash },
        );

        assert!(link.verify().unwrap());
    }

    #[test]
    fn test_direct_link_mismatch() {
        let hash_a = hash_sha256_bytes(b"data-a");
        let hash_b = hash_sha256_bytes(b"data-b");

        let link = ContractLink::new(
            "contract-a",
            "receipt-a-001",
            hash_a,
            "contract-b",
            "receipt-b-001",
            hash_b,
            LinkProof::Direct { hash: hash_a },
        );

        // Should fail because dest_input_hash != proof hash
        assert!(!link.verify().unwrap());
    }

    #[test]
    fn test_transformed_link_verification() {
        let source_data = b"full-json-object";
        let dest_data = b"extracted-field";

        let proof = LinkProof::transformed("json-extract", source_data, dest_data);

        let source_hash = hash_sha256_bytes(source_data);
        let dest_hash = hash_sha256_bytes(dest_data);

        let link = ContractLink::new(
            "contract-a",
            "receipt-a-001",
            source_hash,
            "contract-b",
            "receipt-b-001",
            dest_hash,
            proof,
        );

        assert!(link.verify().unwrap());
    }

    #[test]
    fn test_merkle_bridge_verification() {
        let source_hash = hash_sha256_bytes(b"source-output");
        let dest_hash = hash_sha256_bytes(b"dest-input");

        // Create the bridge proof
        let proof = create_merkle_bridge(source_hash, dest_hash, &[]).unwrap();

        let link = ContractLink::new(
            "contract-a",
            "receipt-a-001",
            source_hash,
            "contract-b",
            "receipt-b-001",
            dest_hash,
            proof,
        );

        assert!(link.verify().unwrap());
    }

    #[test]
    fn test_merkle_bridge_with_additional_context() {
        let source_hash = hash_sha256_bytes(b"source-output");
        let dest_hash = hash_sha256_bytes(b"dest-input");
        let context1 = hash_sha256_bytes(b"metadata");
        let context2 = hash_sha256_bytes(b"timestamp");

        let proof = create_merkle_bridge(source_hash, dest_hash, &[context1, context2]).unwrap();

        let link = ContractLink::new(
            "contract-a",
            "receipt-a-001",
            source_hash,
            "contract-b",
            "receipt-b-001",
            dest_hash,
            proof,
        );

        assert!(link.verify().unwrap());
    }

    #[test]
    fn test_data_flow_graph_basic() {
        let mut graph = DataFlowGraph::new();
        graph.add_contract("contract-a");
        graph.add_contract("contract-b");

        assert!(graph.has_contract("contract-a"));
        assert!(graph.has_contract("contract-b"));
        assert!(!graph.has_contract("contract-c"));
    }

    #[test]
    fn test_data_flow_graph_link() {
        let mut graph = DataFlowGraph::new();
        graph.add_contract("contract-a");
        graph.add_contract("contract-b");

        let hash = hash_sha256_bytes(b"shared-data");
        let link = graph
            .link("contract-a", "contract-b", LinkProof::Direct { hash })
            .unwrap();

        assert_eq!(link.source_contract, "contract-a");
        assert_eq!(link.dest_contract, "contract-b");
        assert_eq!(graph.link_count(), 1);
    }

    #[test]
    fn test_data_flow_graph_link_unregistered_contract() {
        let mut graph = DataFlowGraph::new();
        graph.add_contract("contract-a");

        let hash = hash_sha256_bytes(b"data");
        let result = graph.link("contract-a", "contract-b", LinkProof::Direct { hash });

        assert!(result.is_err());
    }

    #[test]
    fn test_data_flow_graph_verify() {
        let mut graph = DataFlowGraph::new();
        graph.add_contract("contract-a");
        graph.add_contract("contract-b");
        graph.add_contract("contract-c");

        let hash1 = hash_sha256_bytes(b"data-1");
        let hash2 = hash_sha256_bytes(b"data-2");

        graph
            .link("contract-a", "contract-b", LinkProof::Direct { hash: hash1 })
            .unwrap();
        graph
            .link("contract-b", "contract-c", LinkProof::Direct { hash: hash2 })
            .unwrap();

        let verification = graph.verify().unwrap();
        assert!(verification.valid);
        assert_eq!(verification.links_verified, 2);
        assert_eq!(verification.contracts_verified, 3);
    }

    #[test]
    fn test_data_flow_graph_trace_flow() {
        let mut graph = DataFlowGraph::new();
        graph.add_contract("a");
        graph.add_contract("b");
        graph.add_contract("c");
        graph.add_contract("d");

        let hash1 = hash_sha256_bytes(b"h1");
        let hash2 = hash_sha256_bytes(b"h2");
        let hash3 = hash_sha256_bytes(b"h3");

        // a -> b -> d
        // a -> c -> d
        graph.link("a", "b", LinkProof::Direct { hash: hash1 }).unwrap();
        graph.link("b", "d", LinkProof::Direct { hash: hash2 }).unwrap();
        graph.link("a", "c", LinkProof::Direct { hash: hash1 }).unwrap();
        graph.link("c", "d", LinkProof::Direct { hash: hash3 }).unwrap();

        let paths = graph.trace_flow("a", "d");
        assert_eq!(paths.len(), 2); // Two paths: a->b->d and a->c->d
    }

    #[test]
    fn test_data_flow_graph_combined_tree() {
        let mut graph = DataFlowGraph::new();
        graph.add_contract("a");
        graph.add_contract("b");

        let hash = hash_sha256_bytes(b"shared");
        graph.link("a", "b", LinkProof::Direct { hash }).unwrap();

        let tree = graph.combined_tree();
        assert!(tree.root().is_some());
        // Direct link has same hash for source and dest, so only 1 unique leaf
        assert_eq!(tree.len(), 1);
    }

    #[test]
    fn test_data_flow_graph_sources_and_sinks() {
        let mut graph = DataFlowGraph::new();
        graph.add_contract("source-1");
        graph.add_contract("middle");
        graph.add_contract("sink-1");

        let hash1 = hash_sha256_bytes(b"h1");
        let hash2 = hash_sha256_bytes(b"h2");

        graph.link("source-1", "middle", LinkProof::Direct { hash: hash1 }).unwrap();
        graph.link("middle", "sink-1", LinkProof::Direct { hash: hash2 }).unwrap();

        let sources = graph.sources();
        let sinks = graph.sinks();

        assert_eq!(sources.len(), 1);
        assert!(sources.contains(&&"source-1".to_string()));

        assert_eq!(sinks.len(), 1);
        assert!(sinks.contains(&&"sink-1".to_string()));
    }

    #[test]
    fn test_link_proof_types() {
        assert_eq!(LinkProof::direct(b"data").proof_type(), "direct");
        assert_eq!(
            LinkProof::transformed("t", b"a", b"b").proof_type(),
            "transformed"
        );

        let hash = hash_sha256_bytes(b"test");
        let bridge = create_merkle_bridge(hash, hash, &[]).unwrap();
        assert_eq!(bridge.proof_type(), "merkle_bridge");
    }

    #[test]
    fn test_flow_verification_summary() {
        let valid = FlowVerification {
            valid: true,
            contracts_verified: 3,
            links_verified: 2,
            broken_links: vec![],
        };
        assert!(valid.summary().contains("valid"));

        let invalid = FlowVerification {
            valid: false,
            contracts_verified: 2,
            links_verified: 1,
            broken_links: vec!["link-1".to_string()],
        };
        assert!(invalid.summary().contains("invalid"));
    }

    #[test]
    fn test_contract_link_id_generation() {
        let hash = hash_sha256_bytes(b"test-data");
        let link = ContractLink::new(
            "source",
            "receipt-s",
            hash,
            "dest",
            "receipt-d",
            hash,
            LinkProof::Direct { hash },
        );

        assert!(link.id().starts_with("link-source-dest-"));
    }

    #[test]
    fn test_links_from_and_to() {
        let mut graph = DataFlowGraph::new();
        graph.add_contract("a");
        graph.add_contract("b");
        graph.add_contract("c");

        let hash1 = hash_sha256_bytes(b"h1");
        let hash2 = hash_sha256_bytes(b"h2");

        graph.link("a", "b", LinkProof::Direct { hash: hash1 }).unwrap();
        graph.link("a", "c", LinkProof::Direct { hash: hash2 }).unwrap();

        let from_a = graph.links_from("a");
        assert_eq!(from_a.len(), 2);

        let to_b = graph.links_to("b");
        assert_eq!(to_b.len(), 1);
        assert_eq!(to_b[0].source_contract, "a");
    }
}

#[cfg(test)]
mod proptests {
    use super::*;
    use proptest::prelude::*;

    proptest! {
        #[test]
        fn prop_direct_link_self_consistent(data: Vec<u8>) {
            let hash = hash_sha256_bytes(&data);
            let link = ContractLink::new(
                "a", "r-a", hash,
                "b", "r-b", hash,
                LinkProof::Direct { hash },
            );
            prop_assert!(link.verify().unwrap());
        }

        #[test]
        fn prop_transformed_link_self_consistent(source: Vec<u8>, dest: Vec<u8>) {
            let source_hash = hash_sha256_bytes(&source);
            let dest_hash = hash_sha256_bytes(&dest);
            let proof = LinkProof::transformed("test-transform", &source, &dest);

            let link = ContractLink::new(
                "a", "r-a", source_hash,
                "b", "r-b", dest_hash,
                proof,
            );
            prop_assert!(link.verify().unwrap());
        }

        #[test]
        fn prop_merkle_bridge_self_consistent(source: Vec<u8>, dest: Vec<u8>) {
            let source_hash = hash_sha256_bytes(&source);
            let dest_hash = hash_sha256_bytes(&dest);

            let proof = create_merkle_bridge(source_hash, dest_hash, &[]).unwrap();

            let link = ContractLink::new(
                "a", "r-a", source_hash,
                "b", "r-b", dest_hash,
                proof,
            );
            prop_assert!(link.verify().unwrap());
        }

        #[test]
        fn prop_data_flow_graph_verify_valid(
            contracts in prop::collection::vec("[a-z]{3,8}", 2..5),
            data_items in prop::collection::vec(prop::collection::vec(any::<u8>(), 1..100), 1..4)
        ) {
            prop_assume!(contracts.len() >= 2);
            prop_assume!(contracts.iter().collect::<std::collections::HashSet<_>>().len() == contracts.len());

            let mut graph = DataFlowGraph::new();
            for c in &contracts {
                graph.add_contract(c);
            }

            // Create a chain of links
            for (i, data) in data_items.iter().enumerate() {
                if i + 1 < contracts.len() {
                    let hash = hash_sha256_bytes(data);
                    let _ = graph.link(&contracts[i], &contracts[i + 1], LinkProof::Direct { hash });
                }
            }

            let verification = graph.verify().unwrap();
            // All links should be valid since we construct them correctly
            prop_assert!(verification.broken_links.is_empty());
        }
    }
}
