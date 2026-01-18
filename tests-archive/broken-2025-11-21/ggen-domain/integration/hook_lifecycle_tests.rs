//! Integration tests for complete hook lifecycle using Chicago TDD
//!
//! Tests the full workflow: create → list → monitor → remove

use chicago_tdd_tools::prelude::*;
use ggen_domain::hook::create::*;
use ggen_domain::hook::list::*;
use ggen_domain::hook::monitor::*;
use ggen_domain::hook::remove::*;
use std::fs;
use std::path::PathBuf;

/// Helper to get test hooks directory
fn test_hooks_dir() -> PathBuf {
    dirs::home_dir()
        .expect("Home directory should exist")
        .join(".ggen")
        .join("hooks")
}

/// Helper to clean up test hooks
fn cleanup_test_hooks(hook_id: &str) {
    let hook_file = test_hooks_dir().join(format!("{}.json", hook_id));
    let _ = fs::remove_file(hook_file);
}

async_test!(test_full_lifecycle_create_list_remove, {
    // Arrange & Act - Create
    let create_input = CreateInput {
        trigger: "file:change".to_string(),
        action: "echo 'test'".to_string(),
        name: Some("test-lifecycle-1".to_string()),
    };
    let create_result = execute_create(create_input).await;
    assert_ok!(&create_result);

    // Act - List
    let list_input = ListInput {
        verbose: false,
        filter: None,
    };
    let list_result = execute_list(list_input).await;
    assert_ok!(&list_result);
    let hooks = list_result.unwrap();

    // Assert - Hook exists in list
    let found = hooks.iter().find(|h| h.id == "test-lifecycle-1");
    assert!(found.is_some(), "Created hook should appear in list");
    let hook = found.unwrap();
    assert_eq!(hook.trigger, "file:change");
    assert_eq!(hook.action, "echo 'test'");

    // Act - Remove
    let remove_input = RemoveInput {
        hook_id: "test-lifecycle-1".to_string(),
        force: true,
    };
    let remove_result = execute_remove(remove_input).await;
    assert_ok!(&remove_result);

    // Act - List again
    let list_input2 = ListInput {
        verbose: false,
        filter: None,
    };
    let list_result2 = execute_list(list_input2).await;
    assert_ok!(&list_result2);
    let hooks2 = list_result2.unwrap();

    // Assert - Hook no longer exists
    let not_found = hooks2.iter().find(|h| h.id == "test-lifecycle-1");
    assert!(
        not_found.is_none(),
        "Removed hook should not appear in list"
    );
});

async_test!(test_create_multiple_hooks_and_list_all, {
    // Arrange & Act - Create multiple hooks
    for i in 1..=3 {
        let input = CreateInput {
            trigger: format!("trigger{}", i),
            action: format!("action{}", i),
            name: Some(format!("test-multi-hook-{}", i)),
        };
        let result = execute_create(input).await;
        assert_ok!(&result);
    }

    // Act - List all
    let list_input = ListInput {
        verbose: false,
        filter: None,
    };
    let list_result = execute_list(list_input).await;
    assert_ok!(&list_result);
    let hooks = list_result.unwrap();

    // Assert - All three hooks exist
    for i in 1..=3 {
        let hook_id = format!("test-multi-hook-{}", i);
        let found = hooks.iter().find(|h| h.id == hook_id);
        assert!(found.is_some(), "Hook {} should exist", hook_id);
    }

    // Cleanup
    for i in 1..=3 {
        cleanup_test_hooks(&format!("test-multi-hook-{}", i));
    }
});

async_test!(test_create_list_with_filter_remove, {
    // Arrange & Act - Create hooks with different triggers
    let file_hook = CreateInput {
        trigger: "file:change".to_string(),
        action: "action1".to_string(),
        name: Some("test-filter-file".to_string()),
    };
    let template_hook = CreateInput {
        trigger: "template:render".to_string(),
        action: "action2".to_string(),
        name: Some("test-filter-template".to_string()),
    };

    execute_create(file_hook).await.unwrap();
    execute_create(template_hook).await.unwrap();

    // Act - List with file filter
    let list_input = ListInput {
        verbose: false,
        filter: Some("file".to_string()),
    };
    let list_result = execute_list(list_input).await;
    assert_ok!(&list_result);
    let filtered_hooks = list_result.unwrap();

    // Assert - Only file hook in filtered results
    let file_found = filtered_hooks.iter().any(|h| h.id == "test-filter-file");
    assert!(file_found, "File hook should be in filtered results");

    // Act - Remove both hooks
    execute_remove(RemoveInput {
        hook_id: "test-filter-file".to_string(),
        force: true,
    })
    .await
    .unwrap();

    execute_remove(RemoveInput {
        hook_id: "test-filter-template".to_string(),
        force: true,
    })
    .await
    .unwrap();
});

async_test!(test_monitor_empty_hooks, {
    // Arrange
    let _ = fs::remove_dir_all(test_hooks_dir());

    let input = MonitorInput {
        graph: "test-graph".to_string(),
        interval: 1,
        once: true,
    };

    // Act
    let result = execute_monitor(input).await;

    // Assert
    assert_ok!(&result);
    let monitor_result = result.unwrap();
    assert_eq!(monitor_result.active_hooks, 0);
    assert_eq!(monitor_result.watching, 0);
    assert_eq!(monitor_result.hooks.len(), 0);
});

async_test!(test_create_then_monitor, {
    // Arrange & Act - Create hooks
    let hook1 = CreateInput {
        trigger: "graph:test-monitor".to_string(),
        action: "action1".to_string(),
        name: Some("test-monitor-hook-1".to_string()),
    };
    let hook2 = CreateInput {
        trigger: "graph:test-monitor".to_string(),
        action: "action2".to_string(),
        name: Some("test-monitor-hook-2".to_string()),
    };

    execute_create(hook1).await.unwrap();
    execute_create(hook2).await.unwrap();

    // Act - Monitor
    let monitor_input = MonitorInput {
        graph: "test-monitor".to_string(),
        interval: 1,
        once: true,
    };
    let monitor_result = execute_monitor(monitor_input).await;

    // Assert
    assert_ok!(&monitor_result);
    let result = monitor_result.unwrap();
    assert_eq!(result.active_hooks, 2, "Should monitor 2 hooks");
    assert_eq!(result.hooks.len(), 2);

    // Cleanup
    cleanup_test_hooks("test-monitor-hook-1");
    cleanup_test_hooks("test-monitor-hook-2");
});

async_test!(test_update_hook_by_removing_and_recreating, {
    // Arrange & Act - Create initial hook
    let initial = CreateInput {
        trigger: "old-trigger".to_string(),
        action: "old-action".to_string(),
        name: Some("test-update-hook".to_string()),
    };
    execute_create(initial).await.unwrap();

    // Act - Remove hook
    execute_remove(RemoveInput {
        hook_id: "test-update-hook".to_string(),
        force: true,
    })
    .await
    .unwrap();

    // Act - Create new version
    let updated = CreateInput {
        trigger: "new-trigger".to_string(),
        action: "new-action".to_string(),
        name: Some("test-update-hook".to_string()),
    };
    execute_create(updated).await.unwrap();

    // Act - List to verify
    let list_result = execute_list(ListInput {
        verbose: false,
        filter: None,
    })
    .await;

    // Assert
    assert_ok!(&list_result);
    let hooks = list_result.unwrap();
    let updated_hook = hooks.iter().find(|h| h.id == "test-update-hook").unwrap();
    assert_eq!(updated_hook.trigger, "new-trigger");
    assert_eq!(updated_hook.action, "new-action");

    // Cleanup
    cleanup_test_hooks("test-update-hook");
});

async_test!(test_concurrent_hook_operations, {
    // Arrange - Create multiple hooks concurrently
    let futures: Vec<_> = (1..=5)
        .map(|i| {
            let input = CreateInput {
                trigger: format!("concurrent-trigger-{}", i),
                action: format!("concurrent-action-{}", i),
                name: Some(format!("test-concurrent-{}", i)),
            };
            execute_create(input)
        })
        .collect();

    // Act - Wait for all creates
    for handle in futures {
        assert_ok!(&handle.await);
    }

    // Act - List all
    let list_result = execute_list(ListInput {
        verbose: false,
        filter: Some("concurrent".to_string()),
    })
    .await;

    // Assert
    assert_ok!(&list_result);
    let hooks = list_result.unwrap();
    let concurrent_hooks: Vec<_> = hooks
        .iter()
        .filter(|h| h.id.starts_with("test-concurrent-"))
        .collect();
    assert_eq!(
        concurrent_hooks.len(),
        5,
        "All concurrent hooks should be created"
    );

    // Cleanup
    for i in 1..=5 {
        cleanup_test_hooks(&format!("test-concurrent-{}", i));
    }
});

async_test!(test_hook_persistence_across_operations, {
    // Arrange & Act - Create hook
    let create_input = CreateInput {
        trigger: "persist-trigger".to_string(),
        action: "persist-action".to_string(),
        name: Some("test-persist".to_string()),
    };
    execute_create(create_input).await.unwrap();

    // Act - Multiple list operations
    for _ in 0..3 {
        let list_result = execute_list(ListInput {
            verbose: false,
            filter: None,
        })
        .await;
        assert_ok!(&list_result);

        let hooks = list_result.unwrap();
        let hook = hooks.iter().find(|h| h.id == "test-persist");
        assert!(hook.is_some(), "Hook should persist across multiple lists");
    }

    // Cleanup
    cleanup_test_hooks("test-persist");
});

async_test!(test_monitor_with_filtered_hooks, {
    // Arrange - Create hooks with different patterns
    execute_create(CreateInput {
        trigger: "graph:monitor-test:file".to_string(),
        action: "action1".to_string(),
        name: Some("test-monitor-filter-1".to_string()),
    })
    .await
    .unwrap();

    execute_create(CreateInput {
        trigger: "graph:different".to_string(),
        action: "action2".to_string(),
        name: Some("test-monitor-filter-2".to_string()),
    })
    .await
    .unwrap();

    // Act - Monitor with specific filter
    let monitor_result = execute_monitor(MonitorInput {
        graph: "monitor-test".to_string(),
        interval: 1,
        once: true,
    })
    .await;

    // Assert
    assert_ok!(&monitor_result);
    let result = monitor_result.unwrap();
    assert_eq!(result.active_hooks, 1, "Should only monitor filtered hooks");

    // Cleanup
    cleanup_test_hooks("test-monitor-filter-1");
    cleanup_test_hooks("test-monitor-filter-2");
});

async_test!(test_idempotent_create_operations, {
    // Arrange & Act - Create same hook twice
    let input = CreateInput {
        trigger: "idempotent-trigger".to_string(),
        action: "idempotent-action".to_string(),
        name: Some("test-idempotent".to_string()),
    };

    execute_create(input.clone()).await.unwrap();
    execute_create(input).await.unwrap();

    // Act - List
    let list_result = execute_list(ListInput {
        verbose: false,
        filter: None,
    })
    .await;

    // Assert - Only one instance should exist
    assert_ok!(&list_result);
    let hooks = list_result.unwrap();
    let count = hooks.iter().filter(|h| h.id == "test-idempotent").count();
    assert_eq!(
        count, 1,
        "Should have exactly one instance after duplicate creates"
    );

    // Cleanup
    cleanup_test_hooks("test-idempotent");
});
