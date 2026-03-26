//! Distributed Consensus Example - PBFT Demonstration
//!
//! This example demonstrates a practical Byzantine fault-tolerant consensus system
//! with a 4-node cluster tolerating 1 Byzantine fault.
//!
//! # Features
//! - Complete PBFT protocol implementation
//! - Cryptographic receipts with audit trails
//! - Byzantine fault tolerance (f=1 for n=4)
//! - Consensus verification and state tracking

use distributed_consensus::{PbftConfig, PbftConsensus};
use std::io::{self, BufRead};

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    // Initialize tracing for logging
    tracing_subscriber::fmt()
        .with_max_level(tracing::Level::INFO)
        .init();

    println!("╔════════════════════════════════════════════════════════╗");
    println!("║  Distributed Consensus System - PBFT Implementation   ║");
    println!("║                                                        ║");
    println!("║  Practical Byzantine Fault Tolerance (PBFT)           ║");
    println!("║  4-node cluster tolerating 1 Byzantine fault          ║");
    println!("╚════════════════════════════════════════════════════════╝\n");

    // Initialize the consensus system with 4 nodes (f=1 Byzantine tolerance)
    let config = PbftConfig::new(4)?;
    let consensus = PbftConsensus::new(config)?;

    println!("Configuration:");
    println!("  Total Nodes: {}", consensus.config().total_nodes);
    println!(
        "  Max Byzantine Faults (f): {}",
        consensus.config().max_faults
    );
    println!(
        "  Quorum Size (2f+1): {}\n",
        consensus.config().quorum_size()
    );

    // Run interactive command loop
    let stdin = io::stdin();
    let reader = stdin.lock();

    println!("Commands:");
    println!("  propose <value>  - Propose a value for consensus");
    println!("  consensus        - Show last consensus receipt");
    println!("  receipts         - Show all receipts");
    println!("  info             - Show system information");
    println!("  quit             - Exit\n");

    let mut receipts = Vec::new();

    for line in reader.lines() {
        let line = line?;
        let parts: Vec<&str> = line.trim().split_whitespace().collect();

        if parts.is_empty() {
            continue;
        }

        match parts[0] {
            "propose" => {
                if parts.len() < 2 {
                    println!("Usage: propose <value>");
                    continue;
                }

                let value = parts[1..].join(" ");
                println!("\n» Proposing consensus for value: \"{}\"", value);

                match consensus.run_consensus_round(value.clone()).await {
                    Ok(receipt) => {
                        println!(
                            "✓ Consensus reached in round {} (view {})",
                            receipt.round, receipt.view
                        );
                        println!("  Value: {}", receipt.value);
                        println!("  Content Hash: {}", &receipt.content_hash[..16]);
                        println!(
                            "  Signatures: {}/{}",
                            receipt.signatures.len(),
                            receipt.signatures.len()
                        );

                        // Store receipt
                        receipts.push(receipt);
                    }
                    Err(e) => {
                        println!("✗ Consensus failed: {}", e);
                    }
                }
            }

            "consensus" => {
                if let Some(receipt) = receipts.last() {
                    println!("\n{}", receipt.audit_report());
                } else {
                    println!("No consensus receipt available yet.");
                }
            }

            "receipts" => {
                println!("\n╔════════════════════════════════════════════════════════╗");
                println!("║                 Consensus Receipts                      ║");
                println!("╚════════════════════════════════════════════════════════╝\n");

                if receipts.is_empty() {
                    println!("No receipts yet. Use 'propose' to start consensus rounds.\n");
                } else {
                    for (i, receipt) in receipts.iter().enumerate() {
                        println!("Receipt #{}", i + 1);
                        println!("  Round: {}", receipt.round);
                        println!("  View: {}", receipt.view);
                        println!("  Value: {}", receipt.value);
                        println!("  Signatures: {}", receipt.signatures.len());
                        println!("  Hash: {}...", &receipt.content_hash[..16]);
                        println!();
                    }
                }
            }

            "info" => {
                println!("\n╔════════════════════════════════════════════════════════╗");
                println!("║              System Information                        ║");
                println!("╚════════════════════════════════════════════════════════╝\n");

                println!("Cluster Configuration:");
                println!("  Total Nodes: {}", consensus.config().total_nodes);
                println!("  Max Faults (f): {}", consensus.config().max_faults);
                println!(
                    "  Quorum Requirement: {} (2f+1)",
                    consensus.config().quorum_size()
                );
                println!();

                println!("Byzantine Tolerance:");
                println!(
                    "  Can tolerate {} simultaneous Byzantine nodes",
                    consensus.config().max_faults
                );
                println!(
                    "  Requires n = 3f+1 = {} nodes",
                    3 * consensus.config().max_faults + 1
                );
                println!();

                println!("Protocol Phases:");
                println!("  1. Pre-Prepare: Primary broadcasts value");
                println!("  2. Prepare:     Replicas confirm value (2f+1 threshold)");
                println!("  3. Commit:      Replicas commit after prepares");
                println!("  4. Decision:    Consensus finalized with receipts");
                println!();

                println!("Cryptographic Properties:");
                println!("  Signature Algorithm: Ed25519");
                println!("  Hash Algorithm: SHA-256");
                println!("  Quorum Verification: All signatures required");
                println!();

                println!("Safety Guarantees:");
                println!("  ✓ Agreement: All honest nodes agree on same value");
                println!("  ✓ Validity: Proposed value is decided if consensus");
                println!("  ✓ Termination: Bounded rounds with timeouts");
                println!();

                println!("Consensus Rounds: {}", receipts.len());
                println!();
            }

            "quit" | "exit" => {
                println!("Exiting distributed consensus system.");
                break;
            }

            _ => {
                println!(
                    "Unknown command: '{}'. Type 'help' for available commands.",
                    parts[0]
                );
            }
        }
    }

    Ok(())
}
