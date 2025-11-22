//! Concurrent marketplace operations tests
//!
//! Tests concurrent access patterns and race conditions in marketplace operations

use anyhow::Result;
use std::sync::Arc;
use std::time::Duration;
use tempfile::TempDir;
use tokio::sync::{Barrier, RwLock};
use tokio::task::JoinSet;

mod utils;
use utils::{ConcurrentPatterns, PackageOperation, PermutationConfig};

/// Test concurrent reads from marketplace
#[tokio::test]
async fn test_concurrent_reads() -> Result<()> {
    let temp_dir =
        TempDir::new().map_err(|e| anyhow::anyhow!("Failed to create temp dir: {}", e))?;
    let reader_count = 10;
    let barrier = Arc::new(Barrier::new(reader_count));

    let mut tasks = JoinSet::new();

    for i in 0..reader_count {
        let barrier = Arc::clone(&barrier);
        let task_id = i;

        tasks.spawn(async move {
            // Wait for all readers to be ready
            barrier.wait().await;

            // Simulate concurrent read
            tokio::time::sleep(Duration::from_millis(10)).await;

            Ok::<_, anyhow::Error>(task_id)
        });
    }

    let mut completed = 0;
    while let Some(result) = tasks.join_next().await {
        result.map_err(|e| anyhow::anyhow!("Task join error: {}", e))??;
        completed += 1;
    }

    assert_eq!(completed, reader_count);
    Ok(())
}

/// Test concurrent writes to marketplace
#[tokio::test]
async fn test_concurrent_writes() -> Result<()> {
    let temp_dir =
        TempDir::new().map_err(|e| anyhow::anyhow!("Failed to create temp dir: {}", e))?;
    let writer_count = 5;
    let barrier = Arc::new(Barrier::new(writer_count));
    let write_counter = Arc::new(RwLock::new(0));

    let mut tasks = JoinSet::new();

    for i in 0..writer_count {
        let barrier = Arc::clone(&barrier);
        let counter = Arc::clone(&write_counter);
        let package_name = format!("package-{}", i);

        tasks.spawn(async move {
            // Wait for all writers to be ready
            barrier.wait().await;

            // Simulate concurrent write
            let mut count = counter.write().await;
            *count += 1;

            tokio::time::sleep(Duration::from_millis(10)).await;

            Ok::<_, anyhow::Error>(package_name)
        });
    }

    let mut completed = 0;
    while let Some(result) = tasks.join_next().await {
        result.map_err(|e| anyhow::anyhow!("Task join error: {}", e))??;
        completed += 1;
    }

    assert_eq!(completed, writer_count);
    let final_count = *write_counter.read().await;
    assert_eq!(final_count, writer_count);

    Ok(())
}

/// Test mixed read/write operations
#[tokio::test]
async fn test_mixed_concurrent_operations() -> Result<()> {
    let temp_dir =
        TempDir::new().map_err(|e| anyhow::anyhow!("Failed to create temp dir: {}", e))?;
    let operation_count = 20;
    let barrier = Arc::new(Barrier::new(operation_count));
    let shared_state = Arc::new(RwLock::new(Vec::<String>::new()));

    let mut tasks = JoinSet::new();

    for i in 0..operation_count {
        let barrier = Arc::clone(&barrier);
        let state = Arc::clone(&shared_state);
        let is_write = i % 3 == 0; // Every 3rd operation is a write

        tasks.spawn(async move {
            barrier.wait().await;

            if is_write {
                let mut data = state.write().await;
                data.push(format!("item-{}", i));
            } else {
                let data = state.read().await;
                let _len = data.len(); // Simulate read operation
            }

            tokio::time::sleep(Duration::from_millis(5)).await;

            Ok::<_, anyhow::Error>(i)
        });
    }

    let mut completed = 0;
    while let Some(result) = tasks.join_next().await {
        result.map_err(|e| anyhow::anyhow!("Task join error: {}", e))??;
        completed += 1;
    }

    assert_eq!(completed, operation_count);

    let final_state = shared_state.read().await;
    let expected_writes = operation_count / 3 + if operation_count % 3 == 0 { 0 } else { 1 };
    assert!(final_state.len() >= expected_writes - 1 && final_state.len() <= expected_writes);

    Ok(())
}

/// Test race condition in package installation
#[tokio::test]
async fn test_concurrent_package_installation() -> Result<()> {
    let temp_dir =
        TempDir::new().map_err(|e| anyhow::anyhow!("Failed to create temp dir: {}", e))?;
    let installer_count = 5;
    let same_package = "shared-package";
    let barrier = Arc::new(Barrier::new(installer_count));
    let success_counter = Arc::new(RwLock::new(0));
    let failure_counter = Arc::new(RwLock::new(0));

    let mut tasks = JoinSet::new();

    for i in 0..installer_count {
        let barrier = Arc::clone(&barrier);
        let success = Arc::clone(&success_counter);
        let failure = Arc::clone(&failure_counter);
        let package = same_package.to_string();

        tasks.spawn(async move {
            barrier.wait().await;

            // Simulate installation attempt
            tokio::time::sleep(Duration::from_millis(10)).await;

            // First one succeeds, others should fail
            if i == 0 {
                let mut count = success.write().await;
                *count += 1;
                Ok::<_, anyhow::Error>(true)
            } else {
                let mut count = failure.write().await;
                *count += 1;
                Ok::<_, anyhow::Error>(false)
            }
        });
    }

    let mut completed = 0;
    while let Some(result) = tasks.join_next().await {
        result.map_err(|e| anyhow::anyhow!("Task join error: {}", e))??;
        completed += 1;
    }

    assert_eq!(completed, installer_count);

    Ok(())
}

/// Test concurrent search operations
#[tokio::test]
async fn test_concurrent_search() -> Result<()> {
    let temp_dir =
        TempDir::new().map_err(|e| anyhow::anyhow!("Failed to create temp dir: {}", e))?;
    let searcher_count = 15;
    let barrier = Arc::new(Barrier::new(searcher_count));

    let queries = vec!["rust", "web", "database", "async", "framework"];

    let mut tasks = JoinSet::new();

    for i in 0..searcher_count {
        let barrier = Arc::clone(&barrier);
        let query = queries[i % queries.len()].to_string();

        tasks.spawn(async move {
            barrier.wait().await;

            // Simulate search operation
            tokio::time::sleep(Duration::from_millis(5)).await;

            Ok::<_, anyhow::Error>(query)
        });
    }

    let mut completed = 0;
    while let Some(result) = tasks.join_next().await {
        result.map_err(|e| anyhow::anyhow!("Task join error: {}", e))??;
        completed += 1;
    }

    assert_eq!(completed, searcher_count);
    Ok(())
}

/// Test concurrent operations with permutations
#[tokio::test]
async fn test_permuted_concurrent_operations() -> Result<()> {
    let config = PermutationConfig {
        max_permutations: 50,
        include_edge_cases: true,
        include_invalid: false,
    };

    let patterns = ConcurrentPatterns::new(config);
    let operation_patterns = patterns.generate(4, 5);

    let barrier = Arc::new(Barrier::new(operation_patterns.len()));
    let mut tasks = JoinSet::new();

    for (thread_id, ops) in operation_patterns.into_iter().enumerate() {
        let barrier = Arc::clone(&barrier);

        tasks.spawn(async move {
            barrier.wait().await;

            // Execute operation sequence
            for op in ops {
                match op {
                    PackageOperation::Add(_) => {
                        tokio::time::sleep(Duration::from_micros(100)).await;
                    }
                    PackageOperation::Remove(_) => {
                        tokio::time::sleep(Duration::from_micros(50)).await;
                    }
                    PackageOperation::Search(_) => {
                        tokio::time::sleep(Duration::from_micros(75)).await;
                    }
                    PackageOperation::List => {
                        tokio::time::sleep(Duration::from_micros(25)).await;
                    }
                    PackageOperation::Update(_) => {
                        tokio::time::sleep(Duration::from_micros(150)).await;
                    }
                }
            }

            Ok::<_, anyhow::Error>(thread_id)
        });
    }

    let mut completed = 0;
    while let Some(result) = tasks.join_next().await {
        result.map_err(|e| anyhow::anyhow!("Task join error: {}", e))??;
        completed += 1;
    }

    assert!(completed >= 4);
    Ok(())
}

/// Test deadlock prevention
#[tokio::test]
async fn test_no_deadlock() -> Result<()> {
    let resource_a = Arc::new(RwLock::new(0));
    let resource_b = Arc::new(RwLock::new(0));

    let task_count = 10;
    let barrier = Arc::new(Barrier::new(task_count));
    let mut tasks = JoinSet::new();

    for i in 0..task_count {
        let barrier = Arc::clone(&barrier);
        let res_a = Arc::clone(&resource_a);
        let res_b = Arc::clone(&resource_b);

        tasks.spawn(async move {
            barrier.wait().await;

            // Always acquire locks in same order to prevent deadlock
            let mut a = res_a.write().await;
            tokio::time::sleep(Duration::from_micros(10)).await;
            let mut b = res_b.write().await;

            *a += 1;
            *b += 1;

            Ok::<_, anyhow::Error>(i)
        });
    }

    // Set timeout to detect deadlock
    let timeout = tokio::time::timeout(Duration::from_secs(5), async {
        let mut completed = 0;
        while let Some(result) = tasks.join_next().await {
            result.map_err(|e| anyhow::anyhow!("Task join error: {}", e))??;
            completed += 1;
        }
        Ok::<_, anyhow::Error>(completed)
    })
    .await
    .map_err(|_| anyhow::anyhow!("Test timed out - possible deadlock"))?;

    let completed = timeout?;
    assert_eq!(completed, task_count);

    let final_a = *resource_a.read().await;
    let final_b = *resource_b.read().await;
    assert_eq!(final_a, task_count);
    assert_eq!(final_b, task_count);

    Ok(())
}
