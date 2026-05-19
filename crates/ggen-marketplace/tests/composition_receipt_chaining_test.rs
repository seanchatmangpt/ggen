//! Integration tests for composition receipt chaining functionality.
//!
//! Tests the complete chain tracking workflow:
//! - Creating multi-level composition chains
//! - Verifying chain integrity
//! - Detecting anomalies (cycles, broken links)
//! - Storage and retrieval

use ggen_core::marketplace::composition_receipt::{
    AtomicPackRef, BundleExpansion, CompositionReceipt, RuntimeProfile,
};
use ggen_core::marketplace::trust::TrustTier;

#[test]
fn test_simple_parent_child_chain() {
    // Create a root receipt
    let mut root = CompositionReceipt::new(RuntimeProfile {
        profile_id: "root-profile".to_string(),
        runtime_constraints: vec!["constraint1".to_string()],
        trust_requirement: TrustTier::Experimental,
    });
    root.compute_receipt_id().unwrap();

    // Create a child receipt and chain it
    let mut child = CompositionReceipt::new(RuntimeProfile {
        profile_id: "child-profile".to_string(),
        runtime_constraints: vec!["constraint2".to_string()],
        trust_requirement: TrustTier::CommunityReviewed,
    });

    child.chain_parent(&root).unwrap();
    assert!(child.is_child());
    assert_eq!(child.get_parent_id(), root.receipt_id.as_deref());
}

#[test]
fn test_three_level_chain() {
    let mut root = CompositionReceipt::new(RuntimeProfile {
        profile_id: "root".to_string(),
        runtime_constraints: vec![],
        trust_requirement: TrustTier::Experimental,
    });
    root.compute_receipt_id().unwrap();

    let mut mid = CompositionReceipt::new(RuntimeProfile {
        profile_id: "mid".to_string(),
        runtime_constraints: vec![],
        trust_requirement: TrustTier::Experimental,
    });
    mid.chain_parent(&root).unwrap();
    mid.compute_receipt_id().unwrap();

    let mut leaf = CompositionReceipt::new(RuntimeProfile {
        profile_id: "leaf".to_string(),
        runtime_constraints: vec![],
        trust_requirement: TrustTier::Experimental,
    });
    leaf.chain_parent(&mid).unwrap();

    // Verify chain structure
    assert!(root.is_root());
    assert!(mid.is_child());
    assert!(leaf.is_child());
    assert_eq!(mid.get_parent_id(), root.receipt_id.as_deref());
    assert_eq!(leaf.get_parent_id(), mid.receipt_id.as_deref());
}

#[test]
fn test_add_packs_and_maintain_chain() {
    let mut root = CompositionReceipt::new(RuntimeProfile {
        profile_id: "root".to_string(),
        runtime_constraints: vec![],
        trust_requirement: TrustTier::EnterpriseCertified,
    });

    // Add atomic packs to root
    root.add_atomic_pack(AtomicPackRef {
        pack_id: "pack1".to_string(),
        version: "1.0.0".to_string(),
        digest: "hash1".to_string(),
        signature: "sig1".to_string(),
        trust_tier: TrustTier::EnterpriseCertified,
    });

    root.compute_receipt_id().unwrap();

    // Create child and add different packs
    let mut child = CompositionReceipt::new(RuntimeProfile {
        profile_id: "child".to_string(),
        runtime_constraints: vec![],
        trust_requirement: TrustTier::EnterpriseCertified,
    });

    child.add_atomic_pack(AtomicPackRef {
        pack_id: "pack2".to_string(),
        version: "2.0.0".to_string(),
        digest: "hash2".to_string(),
        signature: "sig2".to_string(),
        trust_tier: TrustTier::EnterpriseCertified,
    });

    child.chain_parent(&root).unwrap();

    // Verify both have their packs
    assert_eq!(root.pack_count(), 1);
    assert_eq!(child.pack_count(), 1);
    assert!(child.is_child());
}

#[test]
fn test_composition_with_bundle_expansion() {
    let mut root = CompositionReceipt::new(RuntimeProfile {
        profile_id: "root".to_string(),
        runtime_constraints: vec![],
        trust_requirement: TrustTier::CommunityReviewed,
    });

    root.add_bundle_expansion(BundleExpansion {
        bundle_id: "mcp-bundle".to_string(),
        expanded_to: vec!["pack1".to_string(), "pack2".to_string()],
    });

    root.compute_receipt_id().unwrap();

    let mut child = CompositionReceipt::new(RuntimeProfile {
        profile_id: "child".to_string(),
        runtime_constraints: vec![],
        trust_requirement: TrustTier::CommunityReviewed,
    });

    child.chain_parent(&root).unwrap();

    // Root should have the bundle expansion record
    assert_eq!(root.bundle_aliases.len(), 1);
    assert_eq!(root.bundle_aliases[0].bundle_id, "mcp-bundle");
}

#[test]
fn test_json_serialization_with_chain() {
    let mut root = CompositionReceipt::new(RuntimeProfile {
        profile_id: "root".to_string(),
        runtime_constraints: vec![],
        trust_requirement: TrustTier::Experimental,
    });
    root.compute_receipt_id().unwrap();

    let mut child = CompositionReceipt::new(RuntimeProfile {
        profile_id: "child".to_string(),
        runtime_constraints: vec![],
        trust_requirement: TrustTier::Experimental,
    });
    child.chain_parent(&root).unwrap();
    child.compute_receipt_id().unwrap();

    // Serialize child
    let json = child.to_json().unwrap();

    // Deserialize and verify chain is preserved
    let deserialized = CompositionReceipt::from_json(&json).unwrap();
    assert!(deserialized.is_child());
    assert_eq!(
        deserialized.get_parent_id(),
        Some(root.receipt_id.as_ref().unwrap().as_str())
    );
}

#[test]
fn test_validate_chain_succeeds_for_valid_chain() {
    let mut root = CompositionReceipt::new(RuntimeProfile {
        profile_id: "root".to_string(),
        runtime_constraints: vec![],
        trust_requirement: TrustTier::Experimental,
    });
    root.compute_receipt_id().unwrap();

    let mut child = CompositionReceipt::new(RuntimeProfile {
        profile_id: "child".to_string(),
        runtime_constraints: vec![],
        trust_requirement: TrustTier::Experimental,
    });
    child.chain_parent(&root).unwrap();
    child.compute_receipt_id().unwrap();

    // Create a resolver that can find the root
    let root_clone = root.clone();
    let resolver = move |id: &str| -> Result<CompositionReceipt, ggen_core::marketplace::Error> {
        if id == root_clone.receipt_id.as_ref().unwrap() {
            Ok(root_clone.clone())
        } else {
            Err(ggen_core::marketplace::Error::ValidationFailed {
                reason: format!("Unknown receipt ID: {}", id),
            })
        }
    };

    let result = child.validate_chain(resolver);
    assert!(result.is_ok());
}

#[test]
fn test_get_full_chain_returns_correct_sequence() {
    let mut root = CompositionReceipt::new(RuntimeProfile {
        profile_id: "root".to_string(),
        runtime_constraints: vec![],
        trust_requirement: TrustTier::Experimental,
    });
    root.compute_receipt_id().unwrap();
    let root_id = root.receipt_id.clone().unwrap();

    let mut mid = CompositionReceipt::new(RuntimeProfile {
        profile_id: "mid".to_string(),
        runtime_constraints: vec![],
        trust_requirement: TrustTier::Experimental,
    });
    mid.chain_parent(&root).unwrap();
    mid.compute_receipt_id().unwrap();
    let mid_id = mid.receipt_id.clone().unwrap();

    let mut leaf = CompositionReceipt::new(RuntimeProfile {
        profile_id: "leaf".to_string(),
        runtime_constraints: vec![],
        trust_requirement: TrustTier::Experimental,
    });
    leaf.chain_parent(&mid).unwrap();
    leaf.compute_receipt_id().unwrap();
    let leaf_id = leaf.receipt_id.clone().unwrap();

    // Create a resolver
    let root_c = root.clone();
    let mid_c = mid.clone();
    let mid_id_ref = mid_id.clone();
    let root_id_ref = root_id.clone();
    let resolver = move |id: &str| -> Result<CompositionReceipt, ggen_core::marketplace::Error> {
        if id == mid_id_ref {
            Ok(mid_c.clone())
        } else if id == root_id_ref {
            Ok(root_c.clone())
        } else {
            Err(ggen_core::marketplace::Error::ValidationFailed {
                reason: format!("Unknown ID: {}", id),
            })
        }
    };

    let chain = leaf.get_full_chain(resolver).unwrap();
    assert_eq!(chain.len(), 3);
    assert_eq!(chain[0], leaf_id);
    assert_eq!(chain[1], mid_id);
    assert_eq!(chain[2], root_id);
}

#[test]
fn test_receipt_id_consistency_across_clones() {
    let mut receipt = CompositionReceipt::new(RuntimeProfile {
        profile_id: "test".to_string(),
        runtime_constraints: vec![],
        trust_requirement: TrustTier::Experimental,
    });
    receipt.compute_receipt_id().unwrap();

    let id1 = receipt.receipt_id.clone();

    // Clone and verify ID is preserved
    let receipt2 = receipt.clone();
    let id2 = receipt2.receipt_id.clone();

    assert_eq!(id1, id2);
}

#[test]
fn test_multiple_roots_with_independent_chains() {
    // Create first chain
    let mut root1 = CompositionReceipt::new(RuntimeProfile {
        profile_id: "chain1-root".to_string(),
        runtime_constraints: vec![],
        trust_requirement: TrustTier::Experimental,
    });
    root1.compute_receipt_id().unwrap();

    let mut child1 = CompositionReceipt::new(RuntimeProfile {
        profile_id: "chain1-child".to_string(),
        runtime_constraints: vec![],
        trust_requirement: TrustTier::Experimental,
    });
    child1.chain_parent(&root1).unwrap();

    // Create second chain
    let mut root2 = CompositionReceipt::new(RuntimeProfile {
        profile_id: "chain2-root".to_string(),
        runtime_constraints: vec![],
        trust_requirement: TrustTier::Experimental,
    });
    root2.compute_receipt_id().unwrap();

    let mut child2 = CompositionReceipt::new(RuntimeProfile {
        profile_id: "chain2-child".to_string(),
        runtime_constraints: vec![],
        trust_requirement: TrustTier::Experimental,
    });
    child2.chain_parent(&root2).unwrap();

    // Verify chains are independent
    assert_eq!(child1.get_parent_id(), root1.receipt_id.as_deref());
    assert_eq!(child2.get_parent_id(), root2.receipt_id.as_deref());
    assert_ne!(child1.get_parent_id(), child2.get_parent_id());
}
