// rust/genesis-lockchain/examples/full_workflow.rs
// Complete lockchain workflow: receipts → Merkle tree → quorum → storage

use genesis_lockchain::{LockchainStorage, MerkleTree, PeerId, QuorumManager, Receipt};

fn main() -> Result<(), Box<dyn std::error::Error>> {
    println!("=== KNHK Lockchain Full Workflow ===\n");
    println!("Demonstrating: hash(A) = hash(μ(O)) enforcement via lockchain\n");

    // Simulate 8-beat cycle with receipts
    let cycle_id = 100;
    let mut merkle_tree = MerkleTree::new();

    // Step 1: Beat execution generates receipts
    println!("STEP 1: Beat Execution (ticks 0-7)");
    println!("====================================");
    let receipts = [
        Receipt::new(cycle_id, 0, 1, 5, 0x1234567890abcdef),
        Receipt::new(cycle_id, 1, 2, 6, 0x2345678901bcdef0),
        Receipt::new(cycle_id, 2, 3, 7, 0x3456789012cdef01),
        Receipt::new(cycle_id, 3, 4, 4, 0x4567890123def012),
        Receipt::new(cycle_id, 0, 5, 8, 0x567890123def0123),
        Receipt::new(cycle_id, 1, 6, 5, 0x67890123def01234),
        Receipt::new(cycle_id, 2, 7, 6, 0x7890123def012345),
        Receipt::new(cycle_id, 3, 8, 7, 0x890123def0123456),
    ];

    for (i, receipt) in receipts.iter().enumerate() {
        let leaf_hash = merkle_tree.add_receipt(receipt);
        println!(
            "  Tick {}: shard={}, hook={}, ticks={} ✓",
            i, receipt.shard_id, receipt.hook_id, receipt.actual_ticks
        );
        println!(
            "    → Receipt hash: {:x?}...{:x?}",
            &leaf_hash[..4],
            &leaf_hash[28..]
        );
    }

    // Step 2: Pulse boundary - compute Merkle root
    println!("\nSTEP 2: Pulse Boundary (tick == 0)");
    println!("===================================");
    let root = merkle_tree.compute_root();
    println!(
        "  Merkle root computed from {} receipts",
        merkle_tree.leaf_count()
    );
    println!("  Root: {:x?}...{:x?}", &root[..8], &root[24..]);

    // Step 3: Quorum consensus
    println!("\nSTEP 3: Quorum Consensus (Byzantine Fault Tolerance)");
    println!("=====================================================");
    let peers = vec![
        PeerId("node1.knhk.io".to_string()),
        PeerId("node2.knhk.io".to_string()),
        PeerId("node3.knhk.io".to_string()),
    ];
    println!("  Peers: {} nodes", peers.len());
    println!("  Threshold: 3 votes (2/3 + 1)");

    let quorum = QuorumManager::new(peers, 3, PeerId("coordinator.knhk.io".to_string()));
    let proof = quorum.achieve_consensus(root, cycle_id)?;

    println!("  ✓ Quorum achieved!");
    println!("    Votes collected: {}", proof.vote_count());
    println!(
        "    Verification: {}",
        if proof.verify(3) {
            "✓ PASS"
        } else {
            "✗ FAIL"
        }
    );

    // Step 4: Persist to storage
    println!("\nSTEP 4: Lockchain Persistence");
    println!("==============================");
    let storage = LockchainStorage::new("/tmp/genesis-lockchain-demo")?;
    storage.persist_root(cycle_id, root, proof)?;
    println!("  ✓ Persisted to disk:");
    println!("    Cycle: {}", cycle_id);
    println!("    Root: {:x?}...{:x?}", &root[..4], &root[28..]);
    println!("    Storage: /tmp/genesis-lockchain-demo");

    // Step 5: Audit trail query
    println!("\nSTEP 5: Audit Trail Query");
    println!("==========================");
    if let Some(entry) = storage.get_root(cycle_id)? {
        println!("  Retrieved entry for cycle {}", entry.cycle);
        println!(
            "  Root matches: {}",
            if entry.root == root {
                "✓ YES"
            } else {
                "✗ NO"
            }
        );
        println!(
            "  Quorum proof intact: {}",
            if entry.proof.verify(3) {
                "✓ YES"
            } else {
                "✗ NO"
            }
        );
    }

    // Step 6: Individual receipt proof
    println!("\nSTEP 6: Merkle Proof (Receipt Audit)");
    println!("=====================================");
    let receipt_idx = 2;
    match merkle_tree.generate_proof(receipt_idx) {
        Ok(proof) => {
            println!("  Generating proof for receipt {}:", receipt_idx);
            println!(
                "    Leaf hash: {:x?}...{:x?}",
                &proof.leaf_hash[..4],
                &proof.leaf_hash[28..]
            );
            println!("    Proof path: {} hashes", proof.proof_hashes.len());
            println!(
                "    Verification: {}",
                if proof.verify() {
                    "✓ PASS"
                } else {
                    "✗ FAIL"
                }
            );
            println!(
                "\n  This proves receipt {} was included in cycle {} root",
                receipt_idx, cycle_id
            );
        }
        Err(e) => {
            println!("  ✗ Failed to generate proof: {}", e);
        }
    }

    // Step 7: Continuity check
    println!("\nSTEP 7: Chain Continuity Verification");
    println!("======================================");
    // Simulate multiple cycles
    for cycle in 101_u64..=105_u64 {
        let mut tree = MerkleTree::new();
        for i in 0_u32..4_u32 {
            tree.add_receipt(&Receipt::new(
                cycle,
                i,
                i + 1,
                5,
                0x1000 * cycle + (i as u64),
            ));
        }
        let root = tree.compute_root();
        let proof = quorum.achieve_consensus(root, cycle)?;
        storage.persist_root(cycle, root, proof)?;
    }

    let continuity = storage.verify_continuity(100, 105)?;
    println!(
        "  Cycles 100-105 continuity: {}",
        if continuity {
            "✓ CONTINUOUS"
        } else {
            "✗ GAP DETECTED"
        }
    );
    println!("  Total roots stored: {}", storage.root_count());

    // Summary
    println!("\n=== Workflow Complete ===");
    println!("\nLockchain Properties Demonstrated:");
    println!("  ✓ Receipt provenance (hash(A) = hash(μ(O)))");
    println!("  ✓ Merkle tree aggregation");
    println!("  ✓ Quorum consensus (Byzantine fault tolerance)");
    println!("  ✓ Persistent audit trail");
    println!("  ✓ Individual receipt verification");
    println!("  ✓ Chain continuity enforcement");
    println!("\nAudit Trail Location: /tmp/genesis-lockchain-demo");

    Ok(())
}
