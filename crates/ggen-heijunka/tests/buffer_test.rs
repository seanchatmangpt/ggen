//! Integration tests for WorkBuffer.

use ggen_heijunka::buffer::WorkBuffer;
use ggen_heijunka::WorkItem;
use std::time::Duration;

#[test]
fn test_buffer_spike_absorption() {
    // Arrange
    let mut buffer = WorkBuffer::new(100);

    // Act - Simulate arrival spike
    let spike_items: Vec<_> = (0..50).map(|i| WorkItem::new(i, 1.0)).collect();
    let pushed = buffer.push_batch(spike_items);

    // Assert - Buffer absorbed the spike
    assert_eq!(pushed, 50);
    assert_eq!(buffer.len(), 50);
    assert_eq!(buffer.utilization(), 0.5);
}

#[test]
fn test_buffer_overflow_prevention() {
    // Arrange
    let mut buffer = WorkBuffer::new(10);

    // Act - Try to overflow buffer
    for i in 0..15 {
        buffer.push(WorkItem::new(i, 1.0)).ok();
    }

    // Assert - Buffer should not exceed capacity
    assert_eq!(buffer.len(), 10);
    assert!(buffer.is_full());
    assert_eq!(buffer.available_capacity(), 0);
}

#[test]
fn test_burst_then_drain() {
    // Arrange
    let mut buffer = WorkBuffer::new(100);

    // Act - Burst arrival
    for i in 0..80 {
        buffer.push(WorkItem::new(i, 1.0)).unwrap();
    }

    assert_eq!(buffer.len(), 80);

    // Drain gradually
    let drained = buffer.pop_batch(20);
    assert_eq!(drained.len(), 20);
    assert_eq!(buffer.len(), 60);

    let drained2 = buffer.pop_batch(30);
    assert_eq!(drained2.len(), 30);
    assert_eq!(buffer.len(), 30);

    // Assert - FIFO order preserved
    assert_eq!(drained[0].id, 0);
    assert_eq!(drained[19].id, 19);
    assert_eq!(drained2[0].id, 20);
}

#[test]
fn test_multiple_spikes() {
    // Arrange
    let mut buffer = WorkBuffer::new(200);

    // Act - Multiple spikes with processing in between
    // Spike 1
    for i in 0..50 {
        buffer.push(WorkItem::new(i, 1.0)).unwrap();
    }
    assert_eq!(buffer.len(), 50);

    // Process some
    buffer.pop_batch(20);
    assert_eq!(buffer.len(), 30);

    // Spike 2
    for i in 50..120 {
        buffer.push(WorkItem::new(i, 1.0)).unwrap();
    }
    assert_eq!(buffer.len(), 100);

    // Process more
    buffer.pop_batch(40);
    assert_eq!(buffer.len(), 60);

    // Spike 3
    for i in 120..160 {
        buffer.push(WorkItem::new(i, 1.0)).unwrap();
    }

    // Assert - Buffer handled multiple spikes
    assert_eq!(buffer.len(), 100);
    assert!(!buffer.is_full());
}

#[test]
fn test_priority_based_spike_handling() {
    // Arrange
    let mut buffer = WorkBuffer::new(100);

    // Act - Spike with mixed priorities
    buffer.push(WorkItem::with_priority(1, 1.0, 1)).unwrap();
    buffer.push(WorkItem::with_priority(2, 1.0, 5)).unwrap();
    buffer.push(WorkItem::with_priority(3, 1.0, 3)).unwrap();
    buffer.push(WorkItem::with_priority(4, 1.0, 5)).unwrap();
    buffer.push(WorkItem::with_priority(5, 1.0, 2)).unwrap();

    // Sort by priority
    buffer.sort_by_priority();

    // Assert - High priority items first
    let item1 = buffer.pop().unwrap();
    let item2 = buffer.pop().unwrap();

    assert_eq!(item1.priority, 5);
    assert_eq!(item2.priority, 5);
}

#[test]
fn test_weight_based_spike_handling() {
    // Arrange
    let mut buffer = WorkBuffer::new(100);

    // Act - Spike with varied weights
    buffer.push(WorkItem::new(1, 1.0)).unwrap();
    buffer.push(WorkItem::new(2, 5.0)).unwrap();
    buffer.push(WorkItem::new(3, 2.0)).unwrap();
    buffer.push(WorkItem::new(4, 10.0)).unwrap();
    buffer.push(WorkItem::new(5, 3.0)).unwrap();

    // Sort by weight
    buffer.sort_by_weight();

    // Assert - Heavier items first
    let item1 = buffer.pop().unwrap();
    assert_eq!(item1.weight, 10.0);

    let item2 = buffer.pop().unwrap();
    assert_eq!(item2.weight, 5.0);
}

#[test]
fn test_age_based_spike_handling() {
    // Arrange
    let mut buffer = WorkBuffer::new(100);

    // Act - Create items with age differences
    let item1 = WorkItem::new(1, 1.0);
    std::thread::sleep(Duration::from_millis(10));

    let item2 = WorkItem::new(2, 1.0);
    std::thread::sleep(Duration::from_millis(10));

    let item3 = WorkItem::new(3, 1.0);

    // Push in reverse order
    buffer.push(item3).unwrap();
    buffer.push(item1).unwrap();
    buffer.push(item2).unwrap();

    // Sort by age (oldest first)
    buffer.sort_by_age();

    // Assert - Oldest items first
    let first = buffer.pop().unwrap();
    assert_eq!(first.id, 1);

    let second = buffer.pop().unwrap();
    assert_eq!(second.id, 2);

    let third = buffer.pop().unwrap();
    assert_eq!(third.id, 3);
}

#[test]
fn test_spike_with_filtering() {
    // Arrange
    let mut buffer = WorkBuffer::new(100);

    // Act - Spike with mixed item types
    for i in 0..50 {
        let priority = if i % 5 == 0 { 5 } else { 1 };
        buffer
            .push(WorkItem::with_priority(i, 1.0, priority))
            .unwrap();
    }

    // Filter high priority items
    let high_priority = buffer.filter(|item| item.priority >= 5);

    // Assert - Should find high priority items
    assert_eq!(high_priority.len(), 10);
    assert!(high_priority.iter().all(|item| item.priority >= 5));
}

#[test]
fn test_spike_with_selective_removal() {
    // Arrange
    let mut buffer = WorkBuffer::new(100);

    // Act - Spike with items to remove
    for i in 0..30 {
        let priority = if i < 10 { 1 } else { 5 };
        buffer
            .push(WorkItem::with_priority(i, 1.0, priority))
            .unwrap();
    }

    assert_eq!(buffer.len(), 30);

    // Remove low priority items
    let removed = buffer.remove_if(|item| item.priority < 3);

    // Assert
    assert_eq!(removed.len(), 10);
    assert_eq!(buffer.len(), 20);
}

#[test]
fn test_continuous_arrival_pattern() {
    // Arrange
    let mut buffer = WorkBuffer::new(50);

    // Act - Simulate continuous arrivals with processing
    for batch in 0..5 {
        // Add items
        for i in 0..15 {
            let id = batch * 15 + i;
            buffer.push(WorkItem::new(id, 1.0)).ok();
        }

        // Process some
        buffer.pop_batch(10);
    }

    // Assert - Buffer should have accumulated some items
    assert!(buffer.len() > 0);
    assert!(buffer.len() <= 50);
}

#[test]
fn test_buffer_utilization_tracking() {
    // Arrange
    let mut buffer = WorkBuffer::new(100);

    // Act & Assert - Track utilization changes
    assert_eq!(buffer.utilization(), 0.0);

    for i in 0..25 {
        buffer.push(WorkItem::new(i, 1.0)).unwrap();
    }
    assert_eq!(buffer.utilization(), 0.25);

    for i in 25..50 {
        buffer.push(WorkItem::new(i, 1.0)).unwrap();
    }
    assert_eq!(buffer.utilization(), 0.5);

    for i in 50..75 {
        buffer.push(WorkItem::new(i, 1.0)).unwrap();
    }
    assert_eq!(buffer.utilization(), 0.75);

    for i in 75..100 {
        buffer.push(WorkItem::new(i, 1.0)).unwrap();
    }
    assert_eq!(buffer.utilization(), 1.0);
}

#[test]
fn test_peek_without_removal() {
    // Arrange
    let mut buffer = WorkBuffer::new(10);

    // Act
    buffer.push(WorkItem::new(1, 1.0)).unwrap();
    buffer.push(WorkItem::new(2, 1.0)).unwrap();

    let peeked = buffer.peek();

    // Assert - Peek doesn't remove
    assert!(peeked.is_some());
    assert_eq!(peeked.unwrap().id, 1);
    assert_eq!(buffer.len(), 2);

    // Pop should get the same item
    let popped = buffer.pop().unwrap();
    assert_eq!(popped.id, 1);
}

#[test]
fn test_clear_after_spike() {
    // Arrange
    let mut buffer = WorkBuffer::new(100);

    // Act - Fill with spike
    for i in 0..80 {
        buffer.push(WorkItem::new(i, 1.0)).unwrap();
    }

    assert_eq!(buffer.len(), 80);

    // Clear
    buffer.clear();

    // Assert
    assert_eq!(buffer.len(), 0);
    assert!(buffer.is_empty());
    assert_eq!(buffer.utilization(), 0.0);
}

#[test]
fn test_fifo_order_preservation() {
    // Arrange
    let mut buffer = WorkBuffer::new(100);

    // Act - Push in order
    for i in 0..50 {
        buffer.push(WorkItem::new(i, 1.0)).unwrap();
    }

    // Pop and verify order
    for expected_id in 0..50 {
        let item = buffer.pop().unwrap();
        assert_eq!(item.id, expected_id);
    }

    // Assert - All items processed in order
    assert!(buffer.is_empty());
}

#[test]
fn test_batch_operations() {
    // Arrange
    let mut buffer = WorkBuffer::new(100);

    // Act - Batch push
    let items: Vec<_> = (0..30).map(|i| WorkItem::new(i, 1.0)).collect();
    let pushed = buffer.push_batch(items);

    assert_eq!(pushed, 30);

    // Batch pop
    let popped = buffer.pop_batch(15);
    assert_eq!(popped.len(), 15);
    assert_eq!(buffer.len(), 15);

    // Assert - Order preserved
    assert_eq!(popped[0].id, 0);
    assert_eq!(popped[14].id, 14);
}

#[test]
fn test_total_counters() {
    // Arrange
    let mut buffer = WorkBuffer::new(50);

    // Act
    for i in 0..30 {
        buffer.push(WorkItem::new(i, 1.0)).unwrap();
    }

    assert_eq!(buffer.total_submitted(), 30);

    for _ in 0..10 {
        buffer.pop().ok();
    }

    // Assert
    assert_eq!(buffer.total_submitted(), 30);
    assert_eq!(buffer.total_completed(), 10);
}
