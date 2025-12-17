//! Platform detection and OS/Arch abstractions
//!
//! Provides type-safe platform identification with support for Linux, macOS,
//! and architecture detection (x86_64, aarch64).

use std::process::Command;
use crate::error::{PlatformError, Result};

/// Operating system enumeration
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, serde::Serialize, serde::Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum Os {
    /// Linux
    Linux,
    /// macOS (Darwin)
    Darwin,
    /// Windows
    Windows,
}

impl std::fmt::Display for Os {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Os::Linux => write!(f, "linux"),
            Os::Darwin => write!(f, "darwin"),
            Os::Windows => write!(f, "windows"),
        }
    }
}

/// CPU architecture enumeration
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, serde::Serialize, serde::Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum Arch {
    /// x86_64 / amd64
    X86_64,
    /// ARM64 / aarch64
    Aarch64,
}

impl std::fmt::Display for Arch {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Arch::X86_64 => write!(f, "x86_64"),
            Arch::Aarch64 => write!(f, "aarch64"),
        }
    }
}

/// Target platform with capabilities
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct Platform {
    /// Human-readable name
    pub name: String,
    /// Operating system
    pub os: Os,
    /// CPU architecture
    pub arch: Arch,
    /// Whether Docker is available
    pub docker_available: bool,
}

impl Platform {
    /// Detect the current platform
    pub fn current() -> Result<Self> {
        let os = detect_os()?;
        let arch = detect_arch()?;
        let docker_available = check_docker_availability();

        let name = match (os, arch) {
            (Os::Linux, Arch::X86_64) => "Linux x86_64",
            (Os::Linux, Arch::Aarch64) => "Linux ARM64",
            (Os::Darwin, Arch::X86_64) => "macOS x86_64",
            (Os::Darwin, Arch::Aarch64) => "macOS ARM64",
            (Os::Windows, Arch::X86_64) => "Windows x86_64",
            (Os::Windows, Arch::Aarch64) => "Windows ARM64",
        };

        Ok(Platform {
            name: name.to_string(),
            os,
            arch,
            docker_available,
        })
    }

    /// Check if this platform supports testcontainers
    pub fn supports_testcontainers(&self) -> bool {
        match self.os {
            Os::Linux => true,
            Os::Darwin => self.docker_available,
            Os::Windows => self.docker_available,
        }
    }

    /// Get the target triple for this platform
    pub fn target_triple(&self) -> &'static str {
        match (self.os, self.arch) {
            (Os::Linux, Arch::X86_64) => "x86_64-unknown-linux-gnu",
            (Os::Linux, Arch::Aarch64) => "aarch64-unknown-linux-gnu",
            (Os::Darwin, Arch::X86_64) => "x86_64-apple-darwin",
            (Os::Darwin, Arch::Aarch64) => "aarch64-apple-darwin",
            (Os::Windows, Arch::X86_64) => "x86_64-pc-windows-msvc",
            (Os::Windows, Arch::Aarch64) => "aarch64-pc-windows-msvc",
        }
    }

    /// Check if this is a macOS platform
    pub fn is_macos(&self) -> bool {
        self.os == Os::Darwin
    }

    /// Check if this is a Linux platform
    pub fn is_linux(&self) -> bool {
        self.os == Os::Linux
    }

    /// Check if platform uses native execution (vs container)
    pub fn uses_native_execution(&self) -> bool {
        self.is_macos()
    }

    /// Check if platform uses container execution
    pub fn uses_container_execution(&self) -> bool {
        self.is_linux()
    }
}

impl std::fmt::Display for Platform {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name)
    }
}

impl PartialEq for Platform {
    fn eq(&self, other: &Self) -> bool {
        self.os == other.os && self.arch == other.arch
    }
}

impl Eq for Platform {}

impl std::hash::Hash for Platform {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.os.hash(state);
        self.arch.hash(state);
    }
}

/// Detect the current operating system
fn detect_os() -> Result<Os> {
    match std::env::consts::OS {
        "linux" => Ok(Os::Linux),
        "macos" => Ok(Os::Darwin),
        "windows" => Ok(Os::Windows),
        os => Err(PlatformError::UnsupportedOs(os.to_string()).into()),
    }
}

/// Detect the current CPU architecture
fn detect_arch() -> Result<Arch> {
    match std::env::consts::ARCH {
        "x86_64" => Ok(Arch::X86_64),
        "aarch64" => Ok(Arch::Aarch64),
        arch => Err(PlatformError::UnsupportedArch(arch.to_string()).into()),
    }
}

/// Check if Docker is available by running `docker --version`
fn check_docker_availability() -> bool {
    Command::new("docker")
        .arg("--version")
        .output()
        .map(|output| output.status.success())
        .unwrap_or(false)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_os_display() {
        assert_eq!(Os::Linux.to_string(), "linux");
        assert_eq!(Os::Darwin.to_string(), "darwin");
    }

    #[test]
    fn test_arch_display() {
        assert_eq!(Arch::X86_64.to_string(), "x86_64");
        assert_eq!(Arch::Aarch64.to_string(), "aarch64");
    }

    #[test]
    fn test_current_platform() {
        let platform = Platform::current().expect("Failed to detect platform");
        assert!(!platform.name.is_empty());
        assert!(platform.supports_testcontainers());
    }

    #[test]
    fn test_target_triple() {
        let platform = Platform {
            name: "test".to_string(),
            os: Os::Linux,
            arch: Arch::X86_64,
            docker_available: true,
        };
        assert_eq!(platform.target_triple(), "x86_64-unknown-linux-gnu");
    }

    #[test]
    fn test_platform_equality() {
        let p1 = Platform {
            name: "Linux x86_64".to_string(),
            os: Os::Linux,
            arch: Arch::X86_64,
            docker_available: true,
        };
        let p2 = Platform {
            name: "Different name".to_string(),
            os: Os::Linux,
            arch: Arch::X86_64,
            docker_available: false,
        };
        assert_eq!(p1, p2); // Names and docker_available don't affect equality
    }

    #[test]
    fn test_platform_is_macos() {
        let macos = Platform {
            name: "macOS".to_string(),
            os: Os::Darwin,
            arch: Arch::Aarch64,
            docker_available: true,
        };
        assert!(macos.is_macos());
        assert!(!macos.is_linux());
    }

    #[test]
    fn test_platform_execution_type() {
        let macos = Platform {
            name: "macOS".to_string(),
            os: Os::Darwin,
            arch: Arch::Aarch64,
            docker_available: true,
        };
        assert!(macos.uses_native_execution());
        assert!(!macos.uses_container_execution());

        let linux = Platform {
            name: "Linux".to_string(),
            os: Os::Linux,
            arch: Arch::X86_64,
            docker_available: true,
        };
        assert!(!linux.uses_native_execution());
        assert!(linux.uses_container_execution());
    }
}
