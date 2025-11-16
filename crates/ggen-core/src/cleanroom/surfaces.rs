//! Deterministic Surfaces - The 5 surfaces of test determinism
//!
//! This module provides control over the 5 key sources of non-determinism:
//! 1. **Time**: Frozen, stepped, or real time
//! 2. **RNG**: Seeded or real randomness
//! 3. **FileSystem**: Ephemeral, read-only, or real filesystem
//! 4. **Network**: Offline, limited, or real network access
//! 5. **Process**: Non-root, capability-dropped, or real process

use std::time::{Duration, SystemTime};
use serde::{Serialize, Deserialize};

/// Time surface configuration
#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq)]
pub enum TimeMode {
    /// Time is frozen at a specific seed (deterministic)
    Frozen(u64),
    /// Time steps forward by fixed increments (deterministic)
    Stepped { seed: u64, step_ms: u64 },
    /// Real system time (non-deterministic)
    Real,
}

/// RNG surface configuration
#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq)]
pub enum RngMode {
    /// RNG is seeded with specific value (deterministic)
    Seeded(u64),
    /// Real system randomness (non-deterministic)
    Real,
}

/// Filesystem surface configuration
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub enum FsMode {
    /// Ephemeral filesystem (tmpfs-backed, cleared on exit)
    Ephemeral,
    /// Read-only root with tmpfs for writes
    ReadOnly { root: String },
    /// Real filesystem (non-deterministic)
    Real,
}

/// Network surface configuration
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub enum NetMode {
    /// No network access (fully offline)
    Offline,
    /// Limited network (specific hosts/ports only)
    Limited { allow_hosts: Vec<String> },
    /// Real network access (non-deterministic)
    Real,
}

/// Process surface configuration
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub enum ProcMode {
    /// Non-root user with dropped capabilities
    NonRoot {
        uid: u32,
        gid: u32,
        drop_caps: Vec<String>,
    },
    /// Real process permissions (non-deterministic)
    Real,
}

/// Combined deterministic surfaces configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DeterministicSurfaces {
    pub time: TimeMode,
    pub rng: RngMode,
    pub fs: FsMode,
    pub net: NetMode,
    pub proc: ProcMode,
}

impl DeterministicSurfaces {
    /// Create new deterministic surfaces configuration
    pub fn new(
        time: TimeMode,
        rng: RngMode,
        fs: FsMode,
        net: NetMode,
        proc: ProcMode,
    ) -> Self {
        Self {
            time,
            rng,
            fs,
            net,
            proc,
        }
    }

    /// Create fully deterministic configuration (locked down)
    pub fn deterministic(seed: u64) -> Self {
        Self {
            time: TimeMode::Frozen(seed),
            rng: RngMode::Seeded(seed),
            fs: FsMode::Ephemeral,
            net: NetMode::Offline,
            proc: ProcMode::NonRoot {
                uid: 1000,
                gid: 1000,
                drop_caps: vec![
                    "CAP_NET_ADMIN".to_string(),
                    "CAP_SYS_ADMIN".to_string(),
                    "CAP_SYS_MODULE".to_string(),
                ],
            },
        }
    }

    /// Create real/permissive configuration (for integration tests)
    pub fn permissive() -> Self {
        Self {
            time: TimeMode::Real,
            rng: RngMode::Real,
            fs: FsMode::Real,
            net: NetMode::Real,
            proc: ProcMode::Real,
        }
    }

    /// Check if all surfaces are deterministic
    pub fn is_fully_deterministic(&self) -> bool {
        !matches!(self.time, TimeMode::Real)
            && !matches!(self.rng, RngMode::Real)
            && !matches!(self.fs, FsMode::Real)
            && !matches!(self.net, NetMode::Real)
            && !matches!(self.proc, ProcMode::Real)
    }

    /// Get time mode
    pub fn time_mode(&self) -> &TimeMode {
        &self.time
    }

    /// Get RNG mode
    pub fn rng_mode(&self) -> &RngMode {
        &self.rng
    }

    /// Get filesystem mode
    pub fn fs_mode(&self) -> &FsMode {
        &self.fs
    }

    /// Get network mode
    pub fn net_mode(&self) -> &NetMode {
        &self.net
    }

    /// Get process mode
    pub fn proc_mode(&self) -> &ProcMode {
        &self.proc
    }

    /// Calculate determinism score (0.0 = fully non-deterministic, 1.0 = fully deterministic)
    pub fn determinism_score(&self) -> f64 {
        let mut score = 0.0;
        let mut count = 0;

        // Time surface (20% weight)
        if !matches!(self.time, TimeMode::Real) {
            score += 0.2;
        }
        count += 1;

        // RNG surface (20% weight)
        if !matches!(self.rng, RngMode::Real) {
            score += 0.2;
        }
        count += 1;

        // Filesystem surface (20% weight)
        if !matches!(self.fs, FsMode::Real) {
            score += 0.2;
        }
        count += 1;

        // Network surface (20% weight)
        if !matches!(self.net, NetMode::Real) {
            score += 0.2;
        }
        count += 1;

        // Process surface (20% weight)
        if !matches!(self.proc, ProcMode::Real) {
            score += 0.2;
        }
        count += 1;

        score
    }
}

/// Controlled time provider
pub struct ControlledTime {
    mode: TimeMode,
    start: SystemTime,
    steps: u64,
}

impl ControlledTime {
    /// Create new controlled time
    pub fn new(mode: TimeMode) -> Self {
        Self {
            mode,
            start: SystemTime::now(),
            steps: 0,
        }
    }

    /// Get current time based on mode
    pub fn now(&mut self) -> SystemTime {
        match self.mode {
            TimeMode::Frozen(seed) => {
                // Return fixed time based on seed
                SystemTime::UNIX_EPOCH + Duration::from_secs(seed)
            }
            TimeMode::Stepped { seed, step_ms } => {
                // Step forward each time now() is called
                let offset = Duration::from_millis(step_ms * self.steps);
                self.steps += 1;
                SystemTime::UNIX_EPOCH + Duration::from_secs(seed) + offset
            }
            TimeMode::Real => {
                // Return real system time
                SystemTime::now()
            }
        }
    }

    /// Reset step counter (for stepped mode)
    pub fn reset(&mut self) {
        self.steps = 0;
    }
}

/// Controlled RNG provider
pub struct ControlledRng {
    mode: RngMode,
    state: u64,
}

impl ControlledRng {
    /// Create new controlled RNG
    pub fn new(mode: RngMode) -> Self {
        let state = match mode {
            RngMode::Seeded(seed) => seed,
            RngMode::Real => {
                // Use real random seed
                use std::collections::hash_map::RandomState;
                use std::hash::{BuildHasher, Hasher};
                let mut hasher = RandomState::new().build_hasher();
                hasher.write_u64(0);
                hasher.finish()
            }
        };

        Self { mode, state }
    }

    /// Generate next random number (xorshift64)
    pub fn next(&mut self) -> u64 {
        match self.mode {
            RngMode::Seeded(_) => {
                // Deterministic xorshift64
                self.state ^= self.state << 13;
                self.state ^= self.state >> 7;
                self.state ^= self.state << 17;
                self.state
            }
            RngMode::Real => {
                // Use real randomness
                use std::collections::hash_map::RandomState;
                use std::hash::{BuildHasher, Hasher};
                let mut hasher = RandomState::new().build_hasher();
                hasher.write_u64(self.state);
                self.state = hasher.finish();
                self.state
            }
        }
    }

    /// Generate random number in range [min, max)
    pub fn range(&mut self, min: u64, max: u64) -> u64 {
        let range = max - min;
        if range == 0 {
            return min;
        }
        min + (self.next() % range)
    }

    /// Generate random boolean
    pub fn bool(&mut self) -> bool {
        (self.next() & 1) == 1
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use chicago_tdd_tools::test;

    test!(test_time_frozen, {
        let mut time = ControlledTime::new(TimeMode::Frozen(42));

        let t1 = time.now();
        let t2 = time.now();

        // Frozen time should be identical
        assert_eq!(t1, t2);
    });

    test!(test_time_stepped, {
        let mut time = ControlledTime::new(TimeMode::Stepped {
            seed: 42,
            step_ms: 1000,
        });

        let t1 = time.now();
        let t2 = time.now();

        // Stepped time should advance
        assert!(t2 > t1);
    });

    test!(test_rng_seeded_deterministic, {
        let mut rng1 = ControlledRng::new(RngMode::Seeded(42));
        let mut rng2 = ControlledRng::new(RngMode::Seeded(42));

        let nums1: Vec<u64> = (0..10).map(|_| rng1.next()).collect();
        let nums2: Vec<u64> = (0..10).map(|_| rng2.next()).collect();

        // Same seed should produce same sequence
        assert_eq!(nums1, nums2);
    });

    test!(test_determinism_score, {
        let det = DeterministicSurfaces::deterministic(42);
        assert_eq!(det.determinism_score(), 1.0);

        let perm = DeterministicSurfaces::permissive();
        assert_eq!(perm.determinism_score(), 0.0);
    });

    test!(test_is_fully_deterministic, {
        let det = DeterministicSurfaces::deterministic(42);
        assert!(det.is_fully_deterministic());

        let perm = DeterministicSurfaces::permissive();
        assert!(!perm.is_fully_deterministic());
    });
}
