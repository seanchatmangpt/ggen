//! Agent Barrier - Synchronization primitives for agent coordination
//!
//! Provides barriers and synchronization points for coordinating
//! multiple agents executing in parallel.

use parking_lot::Mutex;
use std::sync::atomic::{AtomicUsize, Ordering};
use std::sync::Arc;
use tokio::sync::{Barrier as TokioBarrier, Notify};
use tracing::debug;

/// Async barrier for agent synchronization
#[derive(Debug)]
pub struct AgentBarrier {
    /// Inner tokio barrier
    inner: TokioBarrier,
    /// Number of agents
    count: usize,
    /// Barrier name for debugging
    name: String,
}

impl AgentBarrier {
    /// Create a new agent barrier
    pub fn new(count: usize) -> Self {
        Self {
            inner: TokioBarrier::new(count),
            count,
            name: "barrier".to_string(),
        }
    }

    /// Create a named barrier
    pub fn named(name: &str, count: usize) -> Self {
        Self {
            inner: TokioBarrier::new(count),
            count,
            name: name.to_string(),
        }
    }

    /// Wait at the barrier
    pub async fn wait(&self) {
        debug!("Agent waiting at barrier '{}' ({} agents)", self.name, self.count);
        let result = self.inner.wait().await;
        if result.is_leader() {
            debug!("Barrier '{}' released (leader)", self.name);
        }
    }

    /// Get the number of agents this barrier was created for
    pub fn count(&self) -> usize {
        self.count
    }

    /// Get barrier name
    pub fn name(&self) -> &str {
        &self.name
    }
}

/// Reusable barrier that can be waited on multiple times
#[derive(Debug)]
pub struct ReusableBarrier {
    /// Total count of waiters expected
    total: usize,
    /// Current waiting count
    waiting: AtomicUsize,
    /// Notifier for release
    notifier: Notify,
    /// Generation counter for barrier reuse
    generation: AtomicUsize,
    /// Barrier name
    name: String,
}

impl ReusableBarrier {
    /// Create a new reusable barrier
    pub fn new(count: usize) -> Arc<Self> {
        Arc::new(Self {
            total: count,
            waiting: AtomicUsize::new(0),
            notifier: Notify::new(),
            generation: AtomicUsize::new(0),
            name: "reusable-barrier".to_string(),
        })
    }

    /// Create a named reusable barrier
    pub fn named(name: &str, count: usize) -> Arc<Self> {
        Arc::new(Self {
            total: count,
            waiting: AtomicUsize::new(0),
            notifier: Notify::new(),
            generation: AtomicUsize::new(0),
            name: name.to_string(),
        })
    }

    /// Wait at the barrier
    pub async fn wait(&self) -> BarrierWaitResult {
        let my_gen = self.generation.load(Ordering::Acquire);
        let waiting = self.waiting.fetch_add(1, Ordering::AcqRel) + 1;

        debug!("Barrier '{}': {}/{} waiting", self.name, waiting, self.total);

        if waiting == self.total {
            // Last one to arrive, release everyone
            self.waiting.store(0, Ordering::Release);
            self.generation.fetch_add(1, Ordering::Release);
            self.notifier.notify_waiters();
            debug!("Barrier '{}' released by leader", self.name);
            BarrierWaitResult { is_leader: true }
        } else {
            // Wait for release
            loop {
                self.notifier.notified().await;
                let current_gen = self.generation.load(Ordering::Acquire);
                if current_gen != my_gen {
                    break;
                }
            }
            BarrierWaitResult { is_leader: false }
        }
    }

    /// Get current generation
    pub fn generation(&self) -> usize {
        self.generation.load(Ordering::Relaxed)
    }
}

/// Result from waiting at a barrier
#[derive(Debug, Clone)]
pub struct BarrierWaitResult {
    /// Whether this waiter was the leader (last to arrive)
    pub is_leader: bool,
}

impl BarrierWaitResult {
    /// Check if this waiter was the leader
    pub fn is_leader(&self) -> bool {
        self.is_leader
    }
}

/// Phased barrier for multi-stage synchronization
#[derive(Debug)]
pub struct PhasedBarrier {
    /// Barriers for each phase
    phases: Vec<TokioBarrier>,
    /// Current phase
    current_phase: AtomicUsize,
    /// Phase names
    phase_names: Vec<String>,
}

impl PhasedBarrier {
    /// Create a new phased barrier
    pub fn new(agent_count: usize, phase_count: usize) -> Self {
        let phases = (0..phase_count)
            .map(|_| TokioBarrier::new(agent_count))
            .collect();
        let phase_names = (0..phase_count)
            .map(|i| format!("phase-{}", i))
            .collect();

        Self {
            phases,
            current_phase: AtomicUsize::new(0),
            phase_names,
        }
    }

    /// Create with named phases
    pub fn with_phases(agent_count: usize, phase_names: Vec<String>) -> Self {
        let phases = phase_names
            .iter()
            .map(|_| TokioBarrier::new(agent_count))
            .collect();

        Self {
            phases,
            current_phase: AtomicUsize::new(0),
            phase_names,
        }
    }

    /// Wait at the current phase
    pub async fn wait(&self) -> PhaseWaitResult {
        let phase = self.current_phase.load(Ordering::Relaxed);
        if phase >= self.phases.len() {
            return PhaseWaitResult {
                phase,
                phase_name: "completed".to_string(),
                is_leader: false,
            };
        }

        debug!("Waiting at phase {} ({})", phase, self.phase_names[phase]);
        let result = self.phases[phase].wait().await;

        if result.is_leader() {
            self.current_phase.fetch_add(1, Ordering::Relaxed);
        }

        PhaseWaitResult {
            phase,
            phase_name: self.phase_names[phase].clone(),
            is_leader: result.is_leader(),
        }
    }

    /// Get current phase
    pub fn current_phase(&self) -> usize {
        self.current_phase.load(Ordering::Relaxed)
    }

    /// Get total phases
    pub fn total_phases(&self) -> usize {
        self.phases.len()
    }

    /// Check if all phases completed
    pub fn is_complete(&self) -> bool {
        self.current_phase() >= self.total_phases()
    }
}

/// Result from waiting at a phase
#[derive(Debug, Clone)]
pub struct PhaseWaitResult {
    /// Phase number (0-indexed)
    pub phase: usize,
    /// Phase name
    pub phase_name: String,
    /// Whether this waiter was the leader
    pub is_leader: bool,
}

/// Countdown latch for one-time synchronization
#[derive(Debug)]
pub struct CountdownLatch {
    /// Remaining count
    count: AtomicUsize,
    /// Notifier for release
    notifier: Notify,
    /// Initial count
    initial: usize,
}

impl CountdownLatch {
    /// Create a new countdown latch
    pub fn new(count: usize) -> Arc<Self> {
        Arc::new(Self {
            count: AtomicUsize::new(count),
            notifier: Notify::new(),
            initial: count,
        })
    }

    /// Count down by one
    pub fn count_down(&self) {
        let prev = self.count.fetch_sub(1, Ordering::AcqRel);
        debug!("Countdown: {}/{}", prev - 1, self.initial);
        if prev == 1 {
            self.notifier.notify_waiters();
            debug!("Countdown latch released");
        }
    }

    /// Wait for latch to reach zero
    pub async fn wait(&self) {
        if self.count.load(Ordering::Acquire) == 0 {
            return;
        }
        self.notifier.notified().await;
    }

    /// Get current count
    pub fn count(&self) -> usize {
        self.count.load(Ordering::Relaxed)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_agent_barrier() {
        let barrier = AgentBarrier::new(2);

        let barrier_clone = &barrier;
        let handle = tokio::spawn(async move {
            barrier_clone.wait().await;
        });

        barrier.wait().await;
        handle.await.unwrap();
    }

    #[tokio::test]
    async fn test_countdown_latch() {
        let latch = CountdownLatch::new(3);

        let latch_clone = Arc::clone(&latch);
        let handle = tokio::spawn(async move {
            latch_clone.wait().await;
        });

        latch.count_down();
        latch.count_down();
        latch.count_down();

        handle.await.unwrap();
        assert_eq!(latch.count(), 0);
    }

    #[tokio::test]
    async fn test_phased_barrier() {
        let barrier = PhasedBarrier::new(1, 3);

        barrier.wait().await;
        assert_eq!(barrier.current_phase(), 1);

        barrier.wait().await;
        assert_eq!(barrier.current_phase(), 2);

        barrier.wait().await;
        assert!(barrier.is_complete());
    }
}
