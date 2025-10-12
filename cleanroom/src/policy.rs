//! Security policies and determinism profiles.

use serde::{Deserialize, Serialize};

/// Security policy configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Policy {
    /// Network isolation policy
    pub net: NetProfile,
    /// Filesystem isolation policy
    pub fs: FsProfile,
    /// Process isolation settings
    pub proc: ProcProfile,
    /// Resource limits
    pub limits: ResourceLimits,
}

impl Policy {
    /// Create a locked-down policy (maximum security)
    pub fn locked() -> Self {
        Self {
            net: NetProfile::Offline,
            fs: FsProfile::ReadOnly {
                workdir: "workdir".to_string(),
            },
            proc: ProcProfile::Strict,
            limits: ResourceLimits::strict(),
        }
    }

    /// Create a permissive policy (minimum constraints)
    pub fn permissive() -> Self {
        Self {
            net: NetProfile::Open,
            fs: FsProfile::Writable {
                workdir: "workdir".to_string(),
            },
            proc: ProcProfile::Standard,
            limits: ResourceLimits::default(),
        }
    }
}

impl Policy {
    /// Serialize policy to JSON
    #[cfg(feature = "serde")]
    pub fn serialize(&self) -> Result<String, serde_json::Error> {
        serde_json::to_string_pretty(self)
    }

    /// Deserialize policy from JSON
    #[cfg(feature = "serde")]
    pub fn deserialize(json: &str) -> Result<Self, serde_json::Error> {
        serde_json::from_str(json)
    }

    /// Validate policy configuration for consistency and security
    pub fn validate(&self) -> Result<(), PolicyValidationError> {
        // Validate network profile
        match &self.net {
            NetProfile::Limited { allowed_ports } => {
                if allowed_ports.len() > 100 {
                    return Err(PolicyValidationError::TooManyPorts(allowed_ports.len()));
                }
                for &port in allowed_ports {
                    if port == 0 {
                        return Err(PolicyValidationError::InvalidPort(port));
                    }
                }
            }
            _ => {}
        }

        // Validate filesystem profile
        match &self.fs {
            FsProfile::ReadOnly { workdir } | FsProfile::Writable { workdir } => {
                if workdir.is_empty() {
                    return Err(PolicyValidationError::EmptyWorkdir);
                }
                if workdir.contains("..") {
                    return Err(PolicyValidationError::UnsafeWorkdir(workdir.clone()));
                }
            }
        }

        // Validate resource limits
        if let Some(cpu_secs) = self.limits.cpu_time_secs {
            if cpu_secs == 0 || cpu_secs > 3600 {
                return Err(PolicyValidationError::InvalidCpuLimit(cpu_secs));
            }
        }

        if let Some(memory_bytes) = self.limits.memory_bytes {
            if memory_bytes == 0 || memory_bytes > 8 * 1024 * 1024 * 1024 {
                return Err(PolicyValidationError::InvalidMemoryLimit(memory_bytes));
            }
        }

        if let Some(file_size_bytes) = self.limits.file_size_bytes {
            if file_size_bytes == 0 || file_size_bytes > 1024 * 1024 * 1024 {
                return Err(PolicyValidationError::InvalidFileSizeLimit(file_size_bytes));
            }
        }

        if let Some(process_count) = self.limits.process_count {
            if process_count == 0 || process_count > 1000 {
                return Err(PolicyValidationError::InvalidProcessLimit(process_count));
            }
        }

        Ok(())
    }

    /// Check if policy allows network access
    pub fn allows_network(&self) -> bool {
        !matches!(self.net, NetProfile::Offline)
    }

    /// Check if policy allows filesystem writes
    pub fn allows_writes(&self) -> bool {
        matches!(self.fs, FsProfile::Writable { .. })
    }

    /// Get the workdir path from filesystem profile
    pub fn workdir(&self) -> Option<&str> {
        match &self.fs {
            FsProfile::ReadOnly { workdir } | FsProfile::Writable { workdir } => Some(workdir),
        }
    }

    /// Convert policy to environment variables for execution
    pub fn to_env(&self) -> Vec<(String, String)> {
        let mut env = Vec::new();

        // Network policy
        match &self.net {
            NetProfile::Offline => {
                env.push(("CLEANROOM_NET".to_string(), "offline".to_string()));
            }
            NetProfile::Limited { allowed_ports } => {
                env.push(("CLEANROOM_NET".to_string(), "limited".to_string()));
                env.push((
                    "CLEANROOM_ALLOWED_PORTS".to_string(),
                    allowed_ports
                        .iter()
                        .map(|p| p.to_string())
                        .collect::<Vec<_>>()
                        .join(","),
                ));
            }
            NetProfile::Open => {
                env.push(("CLEANROOM_NET".to_string(), "open".to_string()));
            }
        }

        // Filesystem policy
        match &self.fs {
            FsProfile::ReadOnly { workdir } => {
                env.push(("CLEANROOM_FS".to_string(), "readonly".to_string()));
                env.push(("CLEANROOM_WORKDIR".to_string(), workdir.clone()));
            }
            FsProfile::Writable { workdir } => {
                env.push(("CLEANROOM_FS".to_string(), "writable".to_string()));
                env.push(("CLEANROOM_WORKDIR".to_string(), workdir.clone()));
            }
        }

        // Process policy
        match &self.proc {
            ProcProfile::Standard => {
                env.push(("CLEANROOM_PROC".to_string(), "standard".to_string()));
            }
            ProcProfile::Isolated => {
                env.push(("CLEANROOM_PROC".to_string(), "isolated".to_string()));
            }
            ProcProfile::Strict => {
                env.push(("CLEANROOM_PROC".to_string(), "strict".to_string()));
            }
        }

        // Resource limits
        if let Some(cpu_secs) = self.limits.cpu_time_secs {
            env.push(("CLEANROOM_CPU_LIMIT".to_string(), cpu_secs.to_string()));
        }
        if let Some(memory_bytes) = self.limits.memory_bytes {
            env.push((
                "CLEANROOM_MEMORY_LIMIT".to_string(),
                memory_bytes.to_string(),
            ));
        }
        if let Some(file_size_bytes) = self.limits.file_size_bytes {
            env.push((
                "CLEANROOM_FILE_SIZE_LIMIT".to_string(),
                file_size_bytes.to_string(),
            ));
        }
        if let Some(process_count) = self.limits.process_count {
            env.push((
                "CLEANROOM_PROCESS_LIMIT".to_string(),
                process_count.to_string(),
            ));
        }

        env
    }
}

/// Policy validation errors
#[derive(Debug, thiserror::Error)]
pub enum PolicyValidationError {
    #[error("too many allowed ports: {0} (max 100)")]
    TooManyPorts(usize),
    #[error("invalid port number: {0}")]
    InvalidPort(u16),
    #[error("workdir cannot be empty")]
    EmptyWorkdir,
    #[error("unsafe workdir path: {0}")]
    UnsafeWorkdir(String),
    #[error("invalid CPU time limit: {0} seconds (must be 1-3600)")]
    InvalidCpuLimit(u64),
    #[error("invalid memory limit: {0} bytes (must be 1-8GB)")]
    InvalidMemoryLimit(u64),
    #[error("invalid file size limit: {0} bytes (must be 1-1GB)")]
    InvalidFileSizeLimit(u64),
    #[error("invalid process count limit: {0} (must be 1-1000)")]
    InvalidProcessLimit(u32),
}

impl Default for Policy {
    fn default() -> Self {
        Self::locked()
    }
}

/// Time determinism profile
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum TimeProfile {
    /// Frozen at specific Unix timestamp
    Frozen(u64),
    /// Monotonic time only (no wall clock)
    Monotonic,
    /// System time (non-deterministic)
    System,
}

impl Default for TimeProfile {
    fn default() -> Self {
        Self::System
    }
}

/// RNG determinism profile
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum RngProfile {
    /// Seeded RNG
    Seed(u64),
    /// System RNG (non-deterministic)
    System,
}

impl Default for RngProfile {
    fn default() -> Self {
        Self::System
    }
}

/// Network isolation profile
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub enum NetProfile {
    /// No network access
    Offline,
    /// Limited network (allowlist)
    /// Limited network access with specific allowed ports
    Limited {
        /// List of allowed ports
        allowed_ports: Vec<u16>,
    },
    /// Full network access
    Open,
}

/// Filesystem isolation profile
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub enum FsProfile {
    /// Read-only filesystem with writable workdir
    ReadOnly {
        /// Workdir path
        workdir: String,
    },
    /// Writable filesystem
    Writable {
        /// Workdir path
        workdir: String,
    },
}

/// Process isolation profiles
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub enum ProcProfile {
    /// Standard process execution
    Standard,
    /// Isolated execution with security constraints
    Isolated,
    /// Maximum isolation for CI/production
    Strict,
}

impl Default for ProcProfile {
    fn default() -> Self {
        Self::Isolated
    }
}

/// Resource limits for execution
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ResourceLimits {
    /// CPU time limit in seconds
    pub cpu_time_secs: Option<u64>,
    /// Memory limit in bytes
    pub memory_bytes: Option<u64>,
    /// File size limit in bytes
    pub file_size_bytes: Option<u64>,
    /// Process count limit
    pub process_count: Option<u32>,
}

impl ResourceLimits {
    /// Strict limits for production use
    pub fn strict() -> Self {
        Self {
            cpu_time_secs: Some(300),                 // 5 minutes
            memory_bytes: Some(512 * 1024 * 1024),    // 512MB
            file_size_bytes: Some(100 * 1024 * 1024), // 100MB
            process_count: Some(1),
        }
    }
}

impl Default for ResourceLimits {
    fn default() -> Self {
        Self {
            cpu_time_secs: Some(60),                 // 1 minute
            memory_bytes: Some(256 * 1024 * 1024),   // 256MB
            file_size_bytes: Some(50 * 1024 * 1024), // 50MB
            process_count: Some(4),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    #[cfg(feature = "serde")]
    fn test_policy_serialization() {
        let policy = Policy::locked();
        let json = policy.serialize().expect("serialization should succeed");
        assert!(json.contains("Offline"));
        assert!(json.contains("ReadOnly"));
        assert!(json.contains("Strict"));

        let deserialized = Policy::deserialize(&json).expect("deserialization should succeed");
        assert_eq!(deserialized.net, policy.net);
        assert_eq!(deserialized.fs, policy.fs);
        assert_eq!(deserialized.proc, policy.proc);
    }

    #[test]
    fn test_policy_validation_valid() {
        let policy = Policy::locked();
        assert!(policy.validate().is_ok());

        let permissive = Policy::permissive();
        assert!(permissive.validate().is_ok());
    }

    #[test]
    fn test_policy_validation_invalid_ports() {
        let mut policy = Policy::locked();
        policy.net = NetProfile::Limited {
            allowed_ports: vec![0, 80, 443], // Port 0 is invalid
        };
        assert!(policy.validate().is_err());

        policy.net = NetProfile::Limited {
            allowed_ports: (1..=101).collect(), // Too many ports
        };
        assert!(policy.validate().is_err());
    }

    #[test]
    fn test_policy_validation_invalid_workdir() {
        let mut policy = Policy::locked();
        policy.fs = FsProfile::ReadOnly {
            workdir: "".to_string(), // Empty workdir
        };
        assert!(policy.validate().is_err());

        policy.fs = FsProfile::ReadOnly {
            workdir: "../unsafe".to_string(), // Unsafe path
        };
        assert!(policy.validate().is_err());
    }

    #[test]
    fn test_policy_validation_invalid_limits() {
        let mut policy = Policy::locked();

        // Invalid CPU limit
        policy.limits.cpu_time_secs = Some(0);
        assert!(policy.validate().is_err());
        policy.limits.cpu_time_secs = Some(3601);
        assert!(policy.validate().is_err());

        // Invalid memory limit
        policy.limits.cpu_time_secs = Some(60);
        policy.limits.memory_bytes = Some(0);
        assert!(policy.validate().is_err());
        policy.limits.memory_bytes = Some(9 * 1024 * 1024 * 1024);
        assert!(policy.validate().is_err());

        // Invalid file size limit
        policy.limits.memory_bytes = Some(256 * 1024 * 1024);
        policy.limits.file_size_bytes = Some(0);
        assert!(policy.validate().is_err());
        policy.limits.file_size_bytes = Some(2 * 1024 * 1024 * 1024);
        assert!(policy.validate().is_err());

        // Invalid process count
        policy.limits.file_size_bytes = Some(50 * 1024 * 1024);
        policy.limits.process_count = Some(0);
        assert!(policy.validate().is_err());
        policy.limits.process_count = Some(1001);
        assert!(policy.validate().is_err());
    }

    #[test]
    fn test_policy_network_access() {
        let locked = Policy::locked();
        assert!(!locked.allows_network());

        let permissive = Policy::permissive();
        assert!(permissive.allows_network());

        let mut policy = Policy::locked();
        policy.net = NetProfile::Limited {
            allowed_ports: vec![80, 443],
        };
        assert!(policy.allows_network());
    }

    #[test]
    fn test_policy_filesystem_writes() {
        let locked = Policy::locked();
        assert!(!locked.allows_writes());

        let permissive = Policy::permissive();
        assert!(permissive.allows_writes());

        let mut policy = Policy::locked();
        policy.fs = FsProfile::Writable {
            workdir: "workdir".to_string(),
        };
        assert!(policy.allows_writes());
    }

    #[test]
    fn test_policy_workdir() {
        let policy = Policy::locked();
        assert_eq!(policy.workdir(), Some("workdir"));

        let permissive = Policy::permissive();
        assert_eq!(permissive.workdir(), Some("workdir"));
    }

    #[test]
    fn test_resource_limits_strict() {
        let limits = ResourceLimits::strict();
        assert_eq!(limits.cpu_time_secs, Some(300));
        assert_eq!(limits.memory_bytes, Some(512 * 1024 * 1024));
        assert_eq!(limits.file_size_bytes, Some(100 * 1024 * 1024));
        assert_eq!(limits.process_count, Some(1));
    }

    #[test]
    fn test_resource_limits_default() {
        let limits = ResourceLimits::default();
        assert_eq!(limits.cpu_time_secs, Some(60));
        assert_eq!(limits.memory_bytes, Some(256 * 1024 * 1024));
        assert_eq!(limits.file_size_bytes, Some(50 * 1024 * 1024));
        assert_eq!(limits.process_count, Some(4));
    }
}
