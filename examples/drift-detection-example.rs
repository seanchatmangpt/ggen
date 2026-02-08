//! Drift Detection Example
//!
//! This example demonstrates how to use the drift detection system to track
//! ontology changes and warn users when generated code is stale.
//!
//! ## What This Demonstrates
//!
//! - Creating a DriftDetector for project state tracking
//! - Checking for drift between current files and last sync
//! - Saving state after a successful sync operation
//! - Displaying drift warnings with change details
//!
//! ## How to Run
//!
//! ```bash
//! cargo run --example drift-detection-example
//! ```
//!
//! ## Expected Output
//!
//! The example will:
//! 1. Create a drift detector for the .ggen state directory
//! 2. Check if previous sync state exists
//! 3. Display drift status (Clean or Drifted)
//! 4. Show warning message if drift is detected
//! 5. Demonstrate saving state after sync

use ggen_core::drift::{DriftDetector, DriftStatus};
use std::path::Path;

fn main() -> ggen_utils::error::Result<()> {
    println!("=== Drift Detection Example ===\n");

    // Step 1: Create a drift detector for the project
    let state_dir = Path::new(".ggen");
    let detector = DriftDetector::new(state_dir)?;

    println!("1. Created drift detector for state directory: {}", state_dir.display());

    // Step 2: Check if we have previous state
    if detector.has_state() {
        println!("   ✓ Previous sync state found\n");

        // Step 3: Check for drift
        let ontology_path = Path::new("ontology.ttl");
        let manifest_path = Path::new("ggen.toml");

        println!("2. Checking drift between current files and last sync...");
        let status = detector.check_drift(ontology_path, manifest_path)?;

        match status {
            DriftStatus::Clean => {
                println!("   ✓ No drift detected - code is up to date!\n");
            }
            DriftStatus::Drifted { ref changes, days_since_sync } => {
                println!("   ⚠ Drift detected! ({} days since last sync)\n", days_since_sync);

                println!("   Changes:");
                for change in changes {
                    println!("     - {}", change.message);
                }
                println!();

                // Show warning message
                if let Some(warning) = status.warning_message() {
                    eprintln!("{}", warning);
                }
            }
        }
    } else {
        println!("   ℹ No previous sync state found\n");
        println!("   Run 'ggen sync' to create baseline state\n");
    }

    // Step 4: Simulate saving state after a sync
    println!("3. Example: Saving state after sync...");

    let ontology_path = Path::new("ontology.ttl");
    let manifest_path = Path::new("ggen.toml");

    // In a real scenario, this would be called by SyncExecutor after successful sync
    if ontology_path.exists() && manifest_path.exists() {
        detector.save_state_with_details(
            ontology_path,
            manifest_path,
            vec![Path::new("imports/schema.ttl").to_path_buf()],
            vec![
                ("add_timestamps".to_string(), "hash123".to_string()),
                ("materialize_derived".to_string(), "hash456".to_string()),
            ],
            12,   // files synced
            1234, // duration in ms
        )?;

        println!("   ✓ Drift state saved to: {}", detector.state_file_path().display());
    } else {
        println!("   ⚠ Skipping (ontology or manifest not found)");
    }

    println!("\n=== Example Complete ===");

    Ok(())
}

// Example output when no drift:
//
// === Drift Detection Example ===
//
// 1. Created drift detector for state directory: .ggen
//    ✓ Previous sync state found
//
// 2. Checking drift between current files and last sync...
//    ✓ No drift detected - code is up to date!
//
// 3. Example: Saving state after sync...
//    ✓ Drift state saved to: .ggen/sync-state.json
//
// === Example Complete ===

// Example output when drift detected:
//
// === Drift Detection Example ===
//
// 1. Created drift detector for state directory: .ggen
//    ✓ Previous sync state found
//
// 2. Checking drift between current files and last sync...
//    ⚠ Drift detected! (3 days since last sync)
//
//    Changes:
//      - Ontology changed (e3b0c442..a7b9c2d1) since last sync
//      - Import 'imports/schema.ttl' changed (abc12345..def67890) since last sync
//
// ⚠️  Ontology changed since last sync (3 days ago). Run 'ggen sync' to update.
//    - Ontology changed (e3b0c442..a7b9c2d1) since last sync
//    - Import 'imports/schema.ttl' changed (abc12345..def67890) since last sync
//
// 3. Example: Saving state after sync...
//    ✓ Drift state saved to: .ggen/sync-state.json
//
// === Example Complete ===
