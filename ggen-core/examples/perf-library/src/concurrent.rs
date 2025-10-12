//! Lock-free concurrent data structures
//!
//! Provides thread-safe data structures without locks using atomic operations.

use crossbeam::queue::{ArrayQueue, SegQueue};
use std::sync::Arc;

/// A lock-free queue using crossbeam
pub struct LockFreeQueue<T> {
    inner: Arc<SegQueue<T>>,
}

impl<T> LockFreeQueue<T> {
    /// Creates a new lock-free queue
    #[inline]
    pub fn new() -> Self {
        Self {
            inner: Arc::new(SegQueue::new()),
        }
    }

    /// Pushes an element to the back of the queue
    #[inline]
    pub fn push(&self, value: T) {
        self.inner.push(value);
    }

    /// Pops an element from the front of the queue
    #[inline]
    pub fn pop(&self) -> Option<T> {
        self.inner.pop()
    }

    /// Returns true if the queue is empty
    #[inline]
    pub fn is_empty(&self) -> bool {
        self.inner.is_empty()
    }
}

impl<T> Clone for LockFreeQueue<T> {
    fn clone(&self) -> Self {
        Self {
            inner: Arc::clone(&self.inner),
        }
    }
}

impl<T> Default for LockFreeQueue<T> {
    fn default() -> Self {
        Self::new()
    }
}

/// A lock-free bounded queue with fixed capacity
pub struct BoundedQueue<T> {
    inner: Arc<ArrayQueue<T>>,
}

impl<T> BoundedQueue<T> {
    /// Creates a new bounded queue with the specified capacity
    #[inline]
    pub fn new(capacity: usize) -> Self {
        Self {
            inner: Arc::new(ArrayQueue::new(capacity)),
        }
    }

    /// Attempts to push an element to the queue
    /// Returns Err(value) if the queue is full
    #[inline]
    pub fn push(&self, value: T) -> Result<(), T> {
        self.inner.push(value)
    }

    /// Pops an element from the queue
    #[inline]
    pub fn pop(&self) -> Option<T> {
        self.inner.pop()
    }

    /// Returns true if the queue is empty
    #[inline]
    pub fn is_empty(&self) -> bool {
        self.inner.is_empty()
    }

    /// Returns true if the queue is full
    #[inline]
    pub fn is_full(&self) -> bool {
        self.inner.is_full()
    }

    /// Returns the capacity of the queue
    #[inline]
    pub fn capacity(&self) -> usize {
        self.inner.capacity()
    }

    /// Returns the current length of the queue
    #[inline]
    pub fn len(&self) -> usize {
        self.inner.len()
    }
}

impl<T> Clone for BoundedQueue<T> {
    fn clone(&self) -> Self {
        Self {
            inner: Arc::clone(&self.inner),
        }
    }
}

/// A lock-free stack
pub struct LockFreeStack<T> {
    inner: Arc<SegQueue<T>>,
}

impl<T> LockFreeStack<T> {
    /// Creates a new lock-free stack
    #[inline]
    pub fn new() -> Self {
        Self {
            inner: Arc::new(SegQueue::new()),
        }
    }

    /// Pushes an element onto the stack
    #[inline]
    pub fn push(&self, value: T) {
        self.inner.push(value);
    }

    /// Pops an element from the stack
    #[inline]
    pub fn pop(&self) -> Option<T> {
        self.inner.pop()
    }

    /// Returns true if the stack is empty
    #[inline]
    pub fn is_empty(&self) -> bool {
        self.inner.is_empty()
    }
}

impl<T> Clone for LockFreeStack<T> {
    fn clone(&self) -> Self {
        Self {
            inner: Arc::clone(&self.inner),
        }
    }
}

impl<T> Default for LockFreeStack<T> {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use rayon::prelude::*;
    use std::sync::atomic::{AtomicUsize, Ordering};

    #[test]
    fn test_queue_basic() {
        let queue = LockFreeQueue::new();
        queue.push(1);
        queue.push(2);
        assert_eq!(queue.pop(), Some(1));
        assert_eq!(queue.pop(), Some(2));
        assert_eq!(queue.pop(), None);
    }

    #[test]
    fn test_concurrent_queue() {
        let queue = LockFreeQueue::new();
        let counter = Arc::new(AtomicUsize::new(0));

        // Spawn multiple threads pushing values
        (0..100).into_par_iter().for_each(|i| {
            queue.push(i);
        });

        // Spawn multiple threads popping values
        (0..100).into_par_iter().for_each(|_| {
            if queue.pop().is_some() {
                counter.fetch_add(1, Ordering::Relaxed);
            }
        });

        assert_eq!(counter.load(Ordering::Relaxed), 100);
    }

    #[test]
    fn test_bounded_queue() {
        let queue = BoundedQueue::new(3);
        assert!(queue.push(1).is_ok());
        assert!(queue.push(2).is_ok());
        assert!(queue.push(3).is_ok());
        assert!(queue.push(4).is_err());
        assert_eq!(queue.len(), 3);
        assert!(queue.is_full());
    }

    #[test]
    fn test_stack_basic() {
        let stack = LockFreeStack::new();
        stack.push(1);
        stack.push(2);
        assert_eq!(stack.pop(), Some(2));
        assert_eq!(stack.pop(), Some(1));
        assert_eq!(stack.pop(), None);
    }
}
