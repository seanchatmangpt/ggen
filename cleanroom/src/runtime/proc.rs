//! Process determinism control
//!
//! Provides process isolation and security constraints for deterministic testing.

use crate::error::{CleanroomError, Result};

/// Controller for process determinism
#[derive(Debug, Clone)]
pub struct ProcController {
    profile: crate::policy::ProcProfile,
    uid: Option<u32>,
    gid: Option<u32>,
    capabilities: Vec<String>,
    resource_limits: crate::policy::ResourceLimits,
}

impl ProcController {
    /// Create a new process controller with the given profile
    pub fn new(profile: crate::policy::ProcProfile) -> Self {
        let (uid, gid, capabilities) = match profile {
            crate::policy::ProcProfile::Standard => (Some(1000), Some(1000), vec![]),
            crate::policy::ProcProfile::Isolated => (Some(1000), Some(1000), vec![]),
            crate::policy::ProcProfile::Strict => (Some(1000), Some(1000), vec![]),
        };

        Self {
            uid,
            gid,
            capabilities,
            resource_limits: crate::policy::ResourceLimits::default(),
            profile,
        }
    }

    /// Apply security constraints for the current profile
    pub fn apply_security_constraints(&self) -> Result<()> {
        match self.profile {
            crate::policy::ProcProfile::Standard => {
                self.drop_capabilities()?;
                self.set_user()?;
            }
            crate::policy::ProcProfile::Isolated => {
                self.drop_capabilities()?;
                self.set_user()?;
            }
            crate::policy::ProcProfile::Strict => {
                self.drop_capabilities()?;
                self.set_user()?;
            }
        }
        self.apply_resource_limits()?;
        Ok(())
    }

    /// Drop Linux capabilities (if supported)
    fn drop_capabilities(&self) -> Result<()> {
        // In a real implementation, this would use libc to drop capabilities
        // For now, we just simulate the behavior
        if !self.capabilities.is_empty() {
            return Err(CleanroomError::Policy(
                crate::error::PolicyError::SecurityViolation(
                    "Cannot drop capabilities in this environment".into(),
                ),
            ));
        }
        Ok(())
    }

    /// Set user ID for non-root execution
    fn set_user(&self) -> Result<()> {
        if let (Some(uid), Some(gid)) = (self.uid, self.gid) {
            // In a real implementation, this would use setuid/setgid
            // For now, we just simulate the behavior
            if uid == 0 || gid == 0 {
                return Err(CleanroomError::Policy(
                    crate::error::PolicyError::SecurityViolation("Cannot run as root user".into()),
                ));
            }
        }
        Ok(())
    }

    /// Apply resource limits
    fn apply_resource_limits(&self) -> Result<()> {
        // In a real implementation, this would use setrlimit
        // For now, we just validate the limits are reasonable
        if let Some(cpu_secs) = self.resource_limits.cpu_time_secs {
            if cpu_secs > 3600 {
                // 1 hour
                return Err(CleanroomError::Policy(
                    crate::error::PolicyError::SecurityViolation("CPU time limit too high".into()),
                ));
            }
        }

        if let Some(mem_bytes) = self.resource_limits.memory_bytes {
            if mem_bytes > 1024 * 1024 * 1024 {
                // 1GB
                return Err(CleanroomError::Policy(
                    crate::error::PolicyError::SecurityViolation("Memory limit too high".into()),
                ));
            }
        }

        Ok(())
    }

    /// Get current user ID
    pub fn uid(&self) -> Option<u32> {
        self.uid
    }

    /// Get current group ID
    pub fn gid(&self) -> Option<u32> {
        self.gid
    }

    /// Get current capabilities
    pub fn capabilities(&self) -> &[String] {
        &self.capabilities
    }

    /// Get resource limits
    pub fn resource_limits(&self) -> &crate::policy::ResourceLimits {
        &self.resource_limits
    }

    /// Get the current profile
    pub fn profile(&self) -> &crate::policy::ProcProfile {
        &self.profile
    }

    /// Check if running as non-root
    pub fn is_non_root(&self) -> bool {
        match (self.uid, self.gid) {
            (Some(uid), Some(gid)) => uid != 0 && gid != 0,
            _ => true, // Standard profile doesn't set uid/gid
        }
    }

    /// Check if capabilities are dropped
    pub fn capabilities_dropped(&self) -> bool {
        self.capabilities.is_empty()
    }

    /// Check if running in isolated mode
    pub fn is_isolated(&self) -> bool {
        matches!(self.profile, crate::policy::ProcProfile::Isolated)
    }

    /// Check if running in strict mode
    pub fn is_strict(&self) -> bool {
        matches!(self.profile, crate::policy::ProcProfile::Strict)
    }
}
