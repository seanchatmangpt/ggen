//! Network determinism control
//!
//! Provides network isolation and access control for deterministic testing.

use crate::error::{CleanroomError, Result};
use std::collections::HashSet;
use std::net::{IpAddr, Ipv4Addr, SocketAddr};

/// Controller for network determinism
#[derive(Debug, Clone)]
pub struct NetController {
    profile: crate::policy::NetProfile,
    allowed_ports: HashSet<u16>,
    allowed_ips: HashSet<IpAddr>,
}

impl NetController {
    /// Create a new network controller with the given profile
    pub fn new(profile: crate::policy::NetProfile) -> Self {
        let (allowed_ports, allowed_ips) = match &profile {
            crate::policy::NetProfile::Offline => (HashSet::new(), HashSet::new()),
            crate::policy::NetProfile::Limited { allowed_ports } => {
                let mut ports = HashSet::new();
                for port in allowed_ports {
                    ports.insert(*port);
                }
                // Allow localhost by default for limited access
                let mut ips = HashSet::new();
                ips.insert(IpAddr::V4(Ipv4Addr::LOCALHOST));
                (ports, ips)
            }
            crate::policy::NetProfile::Open => {
                // In open mode, no restrictions, but we still track for forensics
                (HashSet::new(), HashSet::new())
            }
        };

        Self {
            profile,
            allowed_ports,
            allowed_ips,
        }
    }

    /// Check if outbound connection to address is allowed
    pub fn is_connection_allowed(&self, addr: &SocketAddr) -> Result<bool> {
        match &self.profile {
            crate::policy::NetProfile::Offline => Ok(false),
            crate::policy::NetProfile::Limited { .. } => {
                let port_allowed =
                    self.allowed_ports.is_empty() || self.allowed_ports.contains(&addr.port());
                let ip_allowed =
                    self.allowed_ips.is_empty() || self.allowed_ips.contains(&addr.ip());
                Ok(port_allowed && ip_allowed)
            }
            crate::policy::NetProfile::Open => Ok(true),
        }
    }

    /// Check if DNS resolution is allowed
    pub fn is_dns_allowed(&self) -> Result<bool> {
        match &self.profile {
            crate::policy::NetProfile::Offline => Ok(false),
            crate::policy::NetProfile::Limited { .. } => Ok(true), // DNS typically allowed in limited mode
            crate::policy::NetProfile::Open => Ok(true),
        }
    }

    /// Add allowed port for limited network profile
    pub fn allow_port(&mut self, port: u16) -> Result<()> {
        match &self.profile {
            crate::policy::NetProfile::Limited { .. } => {
                self.allowed_ports.insert(port);
                Ok(())
            }
            _ => Err(CleanroomError::Policy(
                crate::error::PolicyError::SecurityViolation(
                    "Cannot modify port restrictions in non-limited profile".into(),
                ),
            )),
        }
    }

    /// Add allowed IP for limited network profile
    pub fn allow_ip(&mut self, ip: IpAddr) -> Result<()> {
        match &self.profile {
            crate::policy::NetProfile::Limited { .. } => {
                self.allowed_ips.insert(ip);
                Ok(())
            }
            _ => Err(CleanroomError::Policy(
                crate::error::PolicyError::SecurityViolation(
                    "Cannot modify IP restrictions in non-limited profile".into(),
                ),
            )),
        }
    }

    /// Block a specific port (for offline enforcement)
    pub fn block_port(&mut self, port: u16) -> Result<()> {
        match &self.profile {
            crate::policy::NetProfile::Offline => {
                self.allowed_ports.remove(&port);
                Ok(())
            }
            _ => Err(CleanroomError::Policy(
                crate::error::PolicyError::SecurityViolation(
                    "Cannot block ports in non-offline profile".into(),
                ),
            )),
        }
    }

    /// Get the current profile
    pub fn profile(&self) -> &crate::policy::NetProfile {
        &self.profile
    }

    /// Get all allowed ports
    pub fn allowed_ports(&self) -> &HashSet<u16> {
        &self.allowed_ports
    }

    /// Get all allowed IPs
    pub fn allowed_ips(&self) -> &HashSet<IpAddr> {
        &self.allowed_ips
    }

    /// Check if network access is completely disabled
    pub fn is_offline(&self) -> bool {
        matches!(self.profile, crate::policy::NetProfile::Offline)
    }

    /// Check if network access is restricted
    pub fn is_limited(&self) -> bool {
        matches!(self.profile, crate::policy::NetProfile::Limited { .. })
    }

    /// Check if network access is unrestricted
    pub fn is_open(&self) -> bool {
        matches!(self.profile, crate::policy::NetProfile::Open)
    }
}
