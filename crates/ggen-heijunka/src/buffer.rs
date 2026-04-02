//! Work buffer for absorbing arrival spikes.
//!
//! This module provides a bounded buffer that absorbs burst arrivals
//! and enables smooth work distribution over time.

use crate::{HeijunkaError, Result, WorkItem};
use std::collections::VecDeque;

/// A bounded buffer for work items that prevents overflow.
pub struct WorkBuffer {
    items: VecDeque<WorkItem>,
    capacity: usize,
    total_submitted: u64,
    total_completed: u64,
}

impl WorkBuffer {
    /// Create a new work buffer with the specified capacity.
    pub fn new(capacity: usize) -> Self {
        Self {
            items: VecDeque::with_capacity(capacity),
            capacity,
            total_submitted: 0,
            total_completed: 0,
        }
    }

    /// Push a work item into the buffer.
    ///
    /// Returns an error if the buffer is at capacity.
    pub fn push(&mut self, item: WorkItem) -> Result<()> {
        if self.items.len() >= self.capacity {
            return Err(HeijunkaError::BufferOverflow {
                capacity: self.capacity,
            });
        }

        self.items.push_back(item);
        self.total_submitted += 1;
        Ok(())
    }

    /// Push multiple items, but stop if buffer would overflow.
    ///
    /// Returns the number of items successfully pushed.
    pub fn push_batch(&mut self, items: Vec<WorkItem>) -> usize {
        let mut pushed = 0;
        for item in items {
            if self.push(item).is_ok() {
                pushed += 1;
            } else {
                break;
            }
        }
        pushed
    }

    /// Pop a work item from the buffer.
    ///
    /// Returns an error if the buffer is empty.
    pub fn pop(&mut self) -> Result<WorkItem> {
        match self.items.pop_front() {
            Some(item) => {
                self.total_completed += 1;
                Ok(item)
            }
            None => Err(HeijunkaError::BufferUnderflow),
        }
    }

    /// Pop up to `count` items from the buffer.
    pub fn pop_batch(&mut self, count: usize) -> Vec<WorkItem> {
        let actual_count = count.min(self.items.len());
        let mut result = Vec::with_capacity(actual_count);

        for _ in 0..actual_count {
            if let Ok(item) = self.pop() {
                result.push(item);
            }
        }

        result
    }

    /// Peek at the next item without removing it.
    pub fn peek(&self) -> Option<&WorkItem> {
        self.items.front()
    }

    /// Get the number of items currently in the buffer.
    pub fn len(&self) -> usize {
        self.items.len()
    }

    /// Check if the buffer is empty.
    pub fn is_empty(&self) -> bool {
        self.items.is_empty()
    }

    /// Get the buffer capacity.
    pub fn capacity(&self) -> usize {
        self.capacity
    }

    /// Get the buffer utilization as a ratio (0.0 - 1.0).
    pub fn utilization(&self) -> f64 {
        self.items.len() as f64 / self.capacity as f64
    }

    /// Check if the buffer is at capacity.
    pub fn is_full(&self) -> bool {
        self.items.len() >= self.capacity
    }

    /// Get the available capacity.
    pub fn available_capacity(&self) -> usize {
        self.capacity - self.items.len()
    }

    /// Clear all items from the buffer.
    pub fn clear(&mut self) {
        self.items.clear();
    }

    /// Get total number of items submitted.
    pub fn total_submitted(&self) -> u64 {
        self.total_submitted
    }

    /// Get total number of items completed.
    pub fn total_completed(&self) -> u64 {
        self.total_completed
    }

    /// Sort items by priority (higher priority first).
    pub fn sort_by_priority(&mut self) {
        let mut items: Vec<_> = self.items.drain(..).collect();
        items.sort_by(|a, b| b.priority.cmp(&a.priority));
        self.items.extend(items);
    }

    /// Sort items by weight (heavier items first).
    pub fn sort_by_weight(&mut self) {
        let mut items: Vec<_> = self.items.drain(..).collect();
        items.sort_by(|a, b| {
            b.weight
                .partial_cmp(&a.weight)
                .unwrap_or(std::cmp::Ordering::Equal)
        });
        self.items.extend(items);
    }

    /// Sort items by age (oldest first).
    pub fn sort_by_age(&mut self) {
        let mut items: Vec<_> = self.items.drain(..).collect();
        items.sort_by_key(|item| item.submitted_at);
        self.items.extend(items);
    }

    /// Get items matching a predicate.
    pub fn filter<F>(&self, predicate: F) -> Vec<&WorkItem>
    where
        F: Fn(&WorkItem) -> bool,
    {
        self.items.iter().filter(|item| predicate(item)).collect()
    }

    /// Remove items matching a predicate.
    pub fn remove_if<F>(&mut self, predicate: F) -> Vec<WorkItem>
    where
        F: Fn(&WorkItem) -> bool,
    {
        let (matching, remaining): (VecDeque<_>, VecDeque<_>) =
            self.items.drain(..).partition(|item| predicate(item));

        self.items = remaining;
        matching.into()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_buffer_creation() {
        let buffer = WorkBuffer::new(10);
        assert_eq!(buffer.capacity(), 10);
        assert_eq!(buffer.len(), 0);
        assert!(buffer.is_empty());
    }

    #[test]
    fn test_push_and_pop() {
        let mut buffer = WorkBuffer::new(10);
        let item = WorkItem::new(1, 1.0);

        assert!(buffer.push(item).is_ok());
        assert_eq!(buffer.len(), 1);
        assert!(!buffer.is_empty());

        let popped = buffer.pop().unwrap();
        assert_eq!(popped.id, 1);
        assert_eq!(buffer.len(), 0);
        assert!(buffer.is_empty());
    }

    #[test]
    fn test_buffer_overflow() {
        let mut buffer = WorkBuffer::new(2);

        assert!(buffer.push(WorkItem::new(1, 1.0)).is_ok());
        assert!(buffer.push(WorkItem::new(2, 1.0)).is_ok());
        assert!(buffer.push(WorkItem::new(3, 1.0)).is_err());
    }

    #[test]
    fn test_buffer_underflow() {
        let mut buffer = WorkBuffer::new(10);
        assert!(buffer.pop().is_err());
    }

    #[test]
    fn test_utilization() {
        let mut buffer = WorkBuffer::new(10);

        buffer.push(WorkItem::new(1, 1.0)).ok();
        buffer.push(WorkItem::new(2, 1.0)).ok();
        buffer.push(WorkItem::new(3, 1.0)).ok();
        buffer.push(WorkItem::new(4, 1.0)).ok();
        buffer.push(WorkItem::new(5, 1.0)).ok();

        assert_eq!(buffer.utilization(), 0.5);
    }

    #[test]
    fn test_is_full() {
        let mut buffer = WorkBuffer::new(2);

        assert!(!buffer.is_full());

        buffer.push(WorkItem::new(1, 1.0)).ok();
        assert!(!buffer.is_full());

        buffer.push(WorkItem::new(2, 1.0)).ok();
        assert!(buffer.is_full());
    }

    #[test]
    fn test_available_capacity() {
        let mut buffer = WorkBuffer::new(10);

        assert_eq!(buffer.available_capacity(), 10);

        buffer.push(WorkItem::new(1, 1.0)).ok();
        buffer.push(WorkItem::new(2, 1.0)).ok();
        buffer.push(WorkItem::new(3, 1.0)).ok();

        assert_eq!(buffer.available_capacity(), 7);
    }

    #[test]
    fn test_peek() {
        let mut buffer = WorkBuffer::new(10);

        assert!(buffer.peek().is_none());

        buffer.push(WorkItem::new(1, 1.0)).ok();
        buffer.push(WorkItem::new(2, 1.0)).ok();

        let peeked = buffer.peek().unwrap();
        assert_eq!(peeked.id, 1);
        assert_eq!(buffer.len(), 2); // Peek doesn't remove
    }

    #[test]
    fn test_clear() {
        let mut buffer = WorkBuffer::new(10);

        buffer.push(WorkItem::new(1, 1.0)).ok();
        buffer.push(WorkItem::new(2, 1.0)).ok();
        buffer.push(WorkItem::new(3, 1.0)).ok();

        assert_eq!(buffer.len(), 3);

        buffer.clear();
        assert_eq!(buffer.len(), 0);
        assert!(buffer.is_empty());
    }

    #[test]
    fn test_push_batch() {
        let mut buffer = WorkBuffer::new(5);

        let items = vec![
            WorkItem::new(1, 1.0),
            WorkItem::new(2, 1.0),
            WorkItem::new(3, 1.0),
        ];

        let pushed = buffer.push_batch(items);
        assert_eq!(pushed, 3);
        assert_eq!(buffer.len(), 3);
    }

    #[test]
    fn test_push_batch_overflow() {
        let mut buffer = WorkBuffer::new(2);

        let items = vec![
            WorkItem::new(1, 1.0),
            WorkItem::new(2, 1.0),
            WorkItem::new(3, 1.0),
            WorkItem::new(4, 1.0),
        ];

        let pushed = buffer.push_batch(items);
        assert_eq!(pushed, 2);
        assert_eq!(buffer.len(), 2);
    }

    #[test]
    fn test_pop_batch() {
        let mut buffer = WorkBuffer::new(10);

        buffer.push(WorkItem::new(1, 1.0)).ok();
        buffer.push(WorkItem::new(2, 1.0)).ok();
        buffer.push(WorkItem::new(3, 1.0)).ok();
        buffer.push(WorkItem::new(4, 1.0)).ok();
        buffer.push(WorkItem::new(5, 1.0)).ok();

        let popped = buffer.pop_batch(3);
        assert_eq!(popped.len(), 3);
        assert_eq!(buffer.len(), 2);
        assert_eq!(popped[0].id, 1);
        assert_eq!(popped[1].id, 2);
        assert_eq!(popped[2].id, 3);
    }

    #[test]
    fn test_sort_by_priority() {
        let mut buffer = WorkBuffer::new(10);

        buffer.push(WorkItem::with_priority(1, 1.0, 1)).ok();
        buffer.push(WorkItem::with_priority(2, 1.0, 5)).ok();
        buffer.push(WorkItem::with_priority(3, 1.0, 3)).ok();

        buffer.sort_by_priority();

        let items: Vec<_> = buffer.items.iter().map(|i| i.id).collect();
        assert_eq!(items, vec![2, 3, 1]);
    }

    #[test]
    fn test_sort_by_weight() {
        let mut buffer = WorkBuffer::new(10);

        buffer.push(WorkItem::new(1, 1.0)).ok();
        buffer.push(WorkItem::new(2, 3.0)).ok();
        buffer.push(WorkItem::new(3, 2.0)).ok();

        buffer.sort_by_weight();

        let items: Vec<_> = buffer.items.iter().map(|i| i.id).collect();
        assert_eq!(items, vec![2, 3, 1]);
    }

    #[test]
    fn test_sort_by_age() {
        let mut buffer = WorkBuffer::new(10);

        let item1 = WorkItem::new(1, 1.0);
        std::thread::sleep(std::time::Duration::from_millis(10));
        let item2 = WorkItem::new(2, 1.0);
        std::thread::sleep(std::time::Duration::from_millis(10));
        let item3 = WorkItem::new(3, 1.0);

        buffer.push(item3).ok();
        buffer.push(item1).ok();
        buffer.push(item2).ok();

        buffer.sort_by_age();

        let items: Vec<_> = buffer.items.iter().map(|i| i.id).collect();
        assert_eq!(items, vec![1, 2, 3]);
    }

    #[test]
    fn test_filter() {
        let mut buffer = WorkBuffer::new(10);

        buffer.push(WorkItem::with_priority(1, 1.0, 1)).ok();
        buffer.push(WorkItem::with_priority(2, 1.0, 5)).ok();
        buffer.push(WorkItem::with_priority(3, 1.0, 3)).ok();

        let high_priority = buffer.filter(|item| item.priority >= 3);
        assert_eq!(high_priority.len(), 2);
    }

    #[test]
    fn test_remove_if() {
        let mut buffer = WorkBuffer::new(10);

        buffer.push(WorkItem::with_priority(1, 1.0, 1)).ok();
        buffer.push(WorkItem::with_priority(2, 1.0, 5)).ok();
        buffer.push(WorkItem::with_priority(3, 1.0, 3)).ok();

        let removed = buffer.remove_if(|item| item.priority < 3);
        assert_eq!(removed.len(), 1);
        assert_eq!(buffer.len(), 2);
    }

    #[test]
    fn test_total_counters() {
        let mut buffer = WorkBuffer::new(10);

        buffer.push(WorkItem::new(1, 1.0)).ok();
        buffer.push(WorkItem::new(2, 1.0)).ok();
        buffer.push(WorkItem::new(3, 1.0)).ok();

        assert_eq!(buffer.total_submitted(), 3);
        assert_eq!(buffer.total_completed(), 0);

        buffer.pop().ok();
        assert_eq!(buffer.total_completed(), 1);

        buffer.pop().ok();
        assert_eq!(buffer.total_completed(), 2);
    }
}
