//! Build script for ggen-workflow native NIF crate
//!
//! This script configures the Rust compiler for Erlang NIF compilation
//! and sets up proper linking with the Erlang VM.

use std::env;

fn main() {
    // Tell cargo to invalidate the built crate whenever the wrapper changes
    println!("cargo:rerun-if-changed=build.rs");

    // Get the Erlang environment from rustler (may not be present during library build)
    if let Ok(rustler_codegen_dir) = env::var("DEP_RUSTLER_CODEGEN_DIR") {
        // Add the rustler codegen directory to the search path
        println!("cargo:rustc-env=RUSTLER_CODEGEN_DIR={}", rustler_codegen_dir);
    }

    // Emit rustler-cargo metadata
    println!("cargo:rustc-cfg=rlib");

    // Set up linker flags for the NIF
    let target_os = env::var("CARGO_CFG_TARGET_OS").unwrap_or_default();
    if target_os == "macos" {
        // On macOS, we need to ensure flat namespace for cdylib
        // The NIF symbols will be resolved at runtime by the Erlang VM
        println!("cargo:rustc-cdylib-link-arg=-Wl,-undefined,dynamic_lookup");
    } else if target_os == "linux" {
        // On Linux, allow undefined symbols for NIF
        println!("cargo:rustc-cdylib-link-arg=-Wl,--allow-shlib-undefined");
        println!("cargo:rustc-cdylib-link-arg=-Wl,--as-needed");
    }
}
