//! Real compiler invocations for part manufacturing
//!
//! Uses actual `std::process::Command` to invoke:
//! - wasm-pack for WASM32 targets
//! - erlc for Erlang/AtomVM BEAM targets
//! - cargo for ARM Cortex-M and native Rust targets
//!
//! No mocks. Real compilers only.

use crate::utils::error::Result;
use std::path::PathBuf;
use std::process::Command;
use tempfile::TempDir;

/// Part compiler with real toolchain invocations
#[derive(Clone, Debug)]
pub struct PartCompiler {
    // In production, would hold toolchain paths and configuration
}

impl PartCompiler {
    /// Create a new part compiler
    pub fn new() -> Self {
        Self {}
    }

    /// Compile source to target format
    ///
    /// # Errors
    ///
    /// Returns error if compilation fails (non-zero exit code)
    pub async fn compile(&self, part_type: &str, source: &str) -> Result<Vec<u8>> {
        match part_type {
            "wasm32" => self.compile_wasm(source).await,
            "beam" | "atomvm-beam" => self.compile_erlang(source).await,
            "arm-cortex-m" => self.compile_arm(source).await,
            "native" => self.compile_native(source).await,
            _ => Err(crate::utils::error::ggen_error!(
                "Unsupported part type: {}",
                part_type
            )),
        }
    }

    async fn compile_wasm(&self, source: &str) -> Result<Vec<u8>> {
        // Create temporary workspace
        let temp = TempDir::new().map_err(|e| {
            crate::utils::error::ggen_error!("Failed to create temp dir: {}", e)
        })?;

        let work_dir = temp.path();

        // Create minimal Cargo.toml for WASM
        let cargo_toml = r#"[package]
name = "genesis-part"
version = "0.1.0"
edition = "2021"

[lib]
crate-type = ["cdylib"]

[dependencies]
"#;

        let cargo_path = work_dir.join("Cargo.toml");
        std::fs::write(&cargo_path, cargo_toml).map_err(|e| {
            crate::utils::error::ggen_error!("Failed to write Cargo.toml: {}", e)
        })?;

        // Write source to lib.rs
        let src_dir = work_dir.join("src");
        std::fs::create_dir(&src_dir).map_err(|e| {
            crate::utils::error::ggen_error!("Failed to create src dir: {}", e)
        })?;

        let lib_rs = src_dir.join("lib.rs");
        std::fs::write(&lib_rs, source).map_err(|e| {
            crate::utils::error::ggen_error!("Failed to write lib.rs: {}", e)
        })?;

        // Invoke wasm-pack
        let output = Command::new("wasm-pack")
            .args(["build", "--target", "web", "--opt-level", "z"])
            .current_dir(work_dir)
            .output()
            .map_err(|e| {
                crate::utils::error::ggen_error!("Failed to invoke wasm-pack: {}", e)
            })?;

        if !output.status.success() {
            let stderr = String::from_utf8_lossy(&output.stderr);
            return Err(crate::utils::error::ggen_error!(
                "wasm-pack compilation failed: {}",
                stderr
            ));
        }

        // Read compiled .wasm file
        let wasm_path = work_dir
            .join("pkg")
            .join("genesis_part_bg.wasm");
        std::fs::read(&wasm_path).map_err(|e| {
            crate::utils::error::ggen_error!("Failed to read compiled WASM: {}", e)
        })
    }

    async fn compile_erlang(&self, source: &str) -> Result<Vec<u8>> {
        // Create temporary directory
        let temp = TempDir::new().map_err(|e| {
            crate::utils::error::ggen_error!("Failed to create temp dir: {}", e)
        })?;

        let work_dir = temp.path();

        // Write source to .erl file
        let erl_path = work_dir.join("genesis_part.erl");
        std::fs::write(&erl_path, source).map_err(|e| {
            crate::utils::error::ggen_error!("Failed to write .erl file: {}", e)
        })?;

        // Create output directory
        let output_dir = work_dir.join("beam");
        std::fs::create_dir(&output_dir).map_err(|e| {
            crate::utils::error::ggen_error!("Failed to create output dir: {}", e)
        })?;

        // Invoke erlc
        let output = Command::new("erlc")
            .args([
                "-o",
                output_dir.to_string_lossy().as_ref(),
                erl_path.to_string_lossy().as_ref(),
            ])
            .output()
            .map_err(|e| {
                crate::utils::error::ggen_error!("Failed to invoke erlc: {}", e)
            })?;

        if !output.status.success() {
            let stderr = String::from_utf8_lossy(&output.stderr);
            return Err(crate::utils::error::ggen_error!(
                "erlc compilation failed: {}",
                stderr
            ));
        }

        // Read compiled .beam file
        let beam_path = output_dir.join("genesis_part.beam");
        std::fs::read(&beam_path).map_err(|e| {
            crate::utils::error::ggen_error!("Failed to read compiled BEAM: {}", e)
        })
    }

    async fn compile_arm(&self, source: &str) -> Result<Vec<u8>> {
        // Create temporary workspace
        let temp = TempDir::new().map_err(|e| {
            crate::utils::error::ggen_error!("Failed to create temp dir: {}", e)
        })?;

        let work_dir = temp.path();

        // Create Cargo.toml for embedded ARM
        let cargo_toml = r#"[package]
name = "genesis-part-arm"
version = "0.1.0"
edition = "2021"

[lib]
crate-type = ["staticlib"]

[[bin]]
name = "genesis_part"
path = "src/main.rs"

[dependencies]
"#;

        let cargo_path = work_dir.join("Cargo.toml");
        std::fs::write(&cargo_path, cargo_toml).map_err(|e| {
            crate::utils::error::ggen_error!("Failed to write Cargo.toml: {}", e)
        })?;

        // Write source
        let src_dir = work_dir.join("src");
        std::fs::create_dir(&src_dir).map_err(|e| {
            crate::utils::error::ggen_error!("Failed to create src dir: {}", e)
        })?;

        let main_rs = src_dir.join("main.rs");
        std::fs::write(&main_rs, source).map_err(|e| {
            crate::utils::error::ggen_error!("Failed to write main.rs: {}", e)
        })?;

        // Invoke cargo for ARM (assumes target installed)
        let output = Command::new("cargo")
            .args([
                "build",
                "--release",
                "--target",
                "thumbv7em-none-eabihf",
            ])
            .current_dir(work_dir)
            .output()
            .map_err(|e| {
                crate::utils::error::ggen_error!("Failed to invoke cargo: {}", e)
            })?;

        if !output.status.success() {
            let stderr = String::from_utf8_lossy(&output.stderr);
            return Err(crate::utils::error::ggen_error!(
                "ARM compilation failed: {}",
                stderr
            ));
        }

        // Read compiled binary
        let binary_path = work_dir
            .join("target")
            .join("thumbv7em-none-eabihf")
            .join("release")
            .join("genesis_part");

        std::fs::read(&binary_path).map_err(|e| {
            crate::utils::error::ggen_error!("Failed to read compiled ARM binary: {}", e)
        })
    }

    async fn compile_native(&self, source: &str) -> Result<Vec<u8>> {
        // Create temporary workspace
        let temp = TempDir::new().map_err(|e| {
            crate::utils::error::ggen_error!("Failed to create temp dir: {}", e)
        })?;

        let work_dir = temp.path();

        // Create Cargo.toml for native
        let cargo_toml = r#"[package]
name = "genesis-part-native"
version = "0.1.0"
edition = "2021"

[lib]
crate-type = ["cdylib"]

[dependencies]
"#;

        let cargo_path = work_dir.join("Cargo.toml");
        std::fs::write(&cargo_path, cargo_toml).map_err(|e| {
            crate::utils::error::ggen_error!("Failed to write Cargo.toml: {}", e)
        })?;

        // Write source
        let src_dir = work_dir.join("src");
        std::fs::create_dir(&src_dir).map_err(|e| {
            crate::utils::error::ggen_error!("Failed to create src dir: {}", e)
        })?;

        let lib_rs = src_dir.join("lib.rs");
        std::fs::write(&lib_rs, source).map_err(|e| {
            crate::utils::error::ggen_error!("Failed to write lib.rs: {}", e)
        })?;

        // Invoke cargo build
        let output = Command::new("cargo")
            .args(&["build", "--lib", "--release"])
            .current_dir(work_dir)
            .output()
            .map_err(|e| {
                crate::utils::error::ggen_error!("Failed to invoke cargo: {}", e)
            })?;

        if !output.status.success() {
            let stderr = String::from_utf8_lossy(&output.stderr);
            return Err(crate::utils::error::ggen_error!(
                "Native compilation failed: {}",
                stderr
            ));
        }

        // Read compiled library
        #[cfg(target_os = "macos")]
        let lib_name = "libgenesis_part_native.dylib";
        #[cfg(target_os = "linux")]
        let lib_name = "libgenesis_part_native.so";
        #[cfg(target_os = "windows")]
        let lib_name = "genesis_part_native.dll";

        let lib_path = work_dir
            .join("target")
            .join("release")
            .join(lib_name);

        std::fs::read(&lib_path).map_err(|e| {
            crate::utils::error::ggen_error!("Failed to read compiled library: {}", e)
        })
    }
}

impl Default for PartCompiler {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_compiler_creation() {
        let _compiler = PartCompiler::new();
        // Compiler created successfully
    }

    #[tokio::test]
    async fn test_unsupported_part_type() {
        let compiler = PartCompiler::new();
        let result = compiler.compile("unknown", "source").await;
        assert!(result.is_err());
    }
}
