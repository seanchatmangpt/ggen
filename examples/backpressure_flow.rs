//! Backpressure Flow Example
//!
//! Demonstrates kanban board flow control with WIP limits.
//! Pull-only model ensures λ ≤ μ (arrival rate ≤ service rate).

use ggen_backpressure::{KanbanBoard, KanbanConfig, Stage};
use tokio::time::{sleep, Duration};

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    println!("=== Backpressure Flow Example ===\n");
    println!("Demonstrating kanban board with WIP limits\n");

    // Create kanban board with strict limits
    let config = KanbanConfig {
        ready_limit: 3,
        in_progress_limit: 2,
        review_limit: 1,
    };

    let board = KanbanBoard::new(config);
    println!("Kanban board configured:");
    println!("  Ready: {} WIP", board.wip_limit(Stage::Ready).await);
    println!("  In Progress: {} WIP", board.wip_limit(Stage::InProgress).await);
    println!("  Review: {} WIP\n", board.wip_limit(Stage::Review).await);

    // Add work items to backlog
    println!("Adding items to backlog:");
    for i in 1..=7 {
        let item_id = format!("TASK-{}", i);
        board.add_to_backlog(item_id.clone()).await?;
        println!("  + {}", item_id);
    }
    println!("Backlog count: {}\n", board.count(Stage::Backlog).await);

    // Pull items through the flow
    println!("=== Flow Simulation ===\n");

    // Pull to Ready (respects limit)
    println!("Pull items to Ready (limit: 3):");
    for i in 1..=3 {
        let item_id = format!("TASK-{}", i);
        match board.pull(&item_id).await {
            Ok(_token) => println!("  ✓ Pulled {} to Ready", item_id),
            Err(e) => println!("  ✗ Failed to pull {}: {}", item_id, e),
        }
    }

    display_board_state(&board).await;

    // Try to exceed limit
    println!("\nAttempt to exceed Ready limit:");
    match board.pull("TASK-4").await {
        Ok(_) => println!("  ✗ Unexpectedly succeeded"),
        Err(e) => println!("  ✓ Correctly blocked: {}", e),
    }

    // Pull to In Progress
    println!("\nPull items to In Progress (limit: 2):");
    let _token1 = board.pull("TASK-1").await?;
    println!("  ✓ TASK-1 → In Progress");

    let _token2 = board.pull("TASK-2").await?;
    println!("  ✓ TASK-2 → In Progress");

    display_board_state(&board).await;

    // Try to exceed In Progress limit
    println!("\nAttempt to exceed In Progress limit:");
    match board.pull("TASK-3").await {
        Ok(_) => println!("  ✗ Unexpectedly succeeded"),
        Err(e) => println!("  ✓ Correctly blocked: {}", e),
    }

    // Complete one item (pull through Review to Done)
    println!("\nComplete TASK-1 (pull through Review):");
    let _token3 = board.pull("TASK-1").await?;
    println!("  ✓ TASK-1 → Review");

    let _token4 = board.pull("TASK-1").await?;
    println!("  ✓ TASK-1 → Done");

    display_board_state(&board).await;

    // Now capacity is available in In Progress
    println!("\nCapacity available - pull TASK-3:");
    let _token5 = board.pull("TASK-3").await?;
    println!("  ✓ TASK-3 → In Progress");

    display_board_state(&board).await;

    // Simulate continuous flow
    println!("\n=== Continuous Flow Simulation ===");
    println!("Simulating work flowing through pipeline...\n");

    // Pull remaining items from backlog to ready
    for i in 4..=5 {
        let item_id = format!("TASK-{}", i);
        board.pull(&item_id).await?;
        println!("  {} → Ready", item_id);
    }

    // Complete TASK-2
    board.pull("TASK-2").await?;
    println!("  TASK-2 → Review");
    board.pull("TASK-2").await?;
    println!("  TASK-2 → Done");

    // Complete TASK-3
    board.pull("TASK-3").await?;
    println!("  TASK-3 → Review");
    board.pull("TASK-3").await?;
    println!("  TASK-3 → Done");

    display_board_state(&board).await;

    // Final stats
    println!("\n=== Final Statistics ===");
    println!("Backlog: {}", board.count(Stage::Backlog).await);
    println!("Ready: {}", board.count(Stage::Ready).await);
    println!("In Progress: {}", board.count(Stage::InProgress).await);
    println!("Review: {}", board.count(Stage::Review).await);
    println!("Done: {}", board.count(Stage::Done).await);

    println!("\n✓ Backpressure flow example completed");
    println!("Key insight: Pull-only model prevents overload (λ ≤ μ)");

    Ok(())
}

async fn display_board_state(board: &KanbanBoard) {
    println!("\nBoard state:");
    println!("  Backlog: {}", board.count(Stage::Backlog).await);
    println!(
        "  Ready: {}/{} (capacity: {})",
        board.count(Stage::Ready).await,
        board.wip_limit(Stage::Ready).await,
        if board.has_capacity(Stage::Ready).await {
            "available"
        } else {
            "full"
        }
    );
    println!(
        "  In Progress: {}/{} (capacity: {})",
        board.count(Stage::InProgress).await,
        board.wip_limit(Stage::InProgress).await,
        if board.has_capacity(Stage::InProgress).await {
            "available"
        } else {
            "full"
        }
    );
    println!(
        "  Review: {}/{} (capacity: {})",
        board.count(Stage::Review).await,
        board.wip_limit(Stage::Review).await,
        if board.has_capacity(Stage::Review).await {
            "available"
        } else {
            "full"
        }
    );
    println!("  Done: {}", board.count(Stage::Done).await);
}
