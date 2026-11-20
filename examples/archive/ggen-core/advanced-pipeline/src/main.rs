//! Advanced Pipeline Orchestration Example
//!
//! This example demonstrates:
//! - Pipeline creation and configuration
//! - Template rendering with frontmatter
//! - RDF/SPARQL integration in templates
//! - Injection modes (prepend, append, before, after, at_line)
//! - Idempotent generation
//! - Shell hooks (sh_before, sh_after)
//! - Multi-step code generation workflows
//! - Plan creation and application
//!
//! ## Usage
//! ```bash
//! cargo run --example advanced-pipeline
//! ```

mod basic_pipeline;
mod injection_demo;
mod multi_step;
mod sparql_template;

use anyhow::Result;

#[tokio::main]
async fn main() -> Result<()> {
    println!("╔════════════════════════════════════════════════════════════╗");
    println!("║     Advanced Pipeline Orchestration - ggen Examples       ║");
    println!("╚════════════════════════════════════════════════════════════╝\n");

    // Run all examples in sequence
    println!("1️⃣  Running Basic Pipeline Example...");
    basic_pipeline::run()?;
    println!("✅ Basic pipeline complete\n");

    println!("2️⃣  Running SPARQL Template Integration...");
    sparql_template::run()?;
    println!("✅ SPARQL integration complete\n");

    println!("3️⃣  Running Injection Modes Demo...");
    injection_demo::run()?;
    println!("✅ Injection modes complete\n");

    println!("4️⃣  Running Multi-Step Workflow...");
    multi_step::run()?;
    println!("✅ Multi-step workflow complete\n");

    println!("╔════════════════════════════════════════════════════════════╗");
    println!("║              All Examples Completed Successfully!         ║");
    println!("╚════════════════════════════════════════════════════════════╝");

    Ok(())
}
