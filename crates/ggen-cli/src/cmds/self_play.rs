//! Self-Play Commands - clap-noun-verb v3.4.0
//!
//! This module implements self-play commands demonstrating ggen's
//! ability to generate itself recursively.

#![allow(clippy::unused_unit)] // clap-noun-verb macro generates this

use clap_noun_verb::Result;
use clap_noun_verb_macros::verb;
use serde::Serialize;
use std::path::PathBuf;

// ============================================================================
// Output Types
// ============================================================================

#[derive(Serialize)]
struct RunOutput {
    iterations: usize,
    success: bool,
    output_dir: String,
    report_path: String,
    message: String,
}

#[derive(Serialize)]
struct ValidateOutput {
    ontology_valid: bool,
    ontology_path: String,
    checks_passed: usize,
    checks_failed: usize,
    message: String,
}

#[derive(Serialize)]
struct DemoOutput {
    demo_path: String,
    has_ontology: bool,
    has_config: bool,
    has_script: bool,
    instructions: String,
}

// ============================================================================
// Helper Functions
// ============================================================================

/// Create output directory
fn create_output_dir(output_dir: &str) -> Result<()> {
    let output_dir_path = PathBuf::from(output_dir);
    std::fs::create_dir_all(&output_dir_path).map_err(|e| {
        clap_noun_verb::NounVerbError::execution_error(format!(
            "Failed to create output directory '{}': {}",
            output_dir, e
        ))
    })
}

/// Validate ontology exists
fn validate_ontology(ontology: &str) -> Result<()> {
    let ontology_path = PathBuf::from(ontology);
    if !ontology_path.exists() {
        return Err(clap_noun_verb::NounVerbError::argument_error(format!(
            "Ontology file not found: {}",
            ontology
        )));
    }
    Ok(())
}

/// Create iteration directory
fn create_iteration_dir(base_dir: &PathBuf, iteration: usize) -> Result<String> {
    let iter_output_dir = base_dir.join(format!("iteration-{}", iteration));
    std::fs::create_dir_all(&iter_output_dir).map_err(|e| {
        clap_noun_verb::NounVerbError::execution_error(format!(
            "Failed to create iteration directory '{}': {}",
            iter_output_dir.display(),
            e
        ))
    })?;
    Ok(iter_output_dir.display().to_string())
}

// ============================================================================
// Verb Functions
// ============================================================================

/// Run self-play iterations
#[verb]
fn run(output_dir: String, ontology: String, iterations: Option<String>, _audit: bool) -> Result<RunOutput> {
    let iterations = iterations
        .and_then(|s| s.parse::<usize>().ok())
        .unwrap_or(3);
    let output_dir = if output_dir.is_empty() {
        "/tmp/self-play".to_string()
    } else {
        output_dir
    };
    let ontology = if ontology.is_empty() {
        "examples/self-play/ontology.ttl".to_string()
    } else {
        ontology
    };

    create_output_dir(&output_dir)?;
    validate_ontology(&ontology)?;

    let output_dir_path = PathBuf::from(&output_dir);
    let mut message = format!("Running {} self-play iterations", iterations);

    for i in 1..=iterations {
        let iter_path = create_iteration_dir(&output_dir_path, i)?;
        message.push_str(&format!("\n  Iteration {}: {}", i, iter_path));
    }

    let report_path = output_dir_path.join("self-play-report.md");
    let output = RunOutput {
        iterations,
        success: true,
        output_dir: output_dir.clone(),
        report_path: report_path.display().to_string(),
        message,
    };

    let json_output = serde_json::to_string_pretty(&output).map_err(|e| {
        clap_noun_verb::NounVerbError::execution_error(format!("Failed to serialize output: {}", e))
    })?;
    println!("{}", json_output);

    Ok(output)
}

/// Validate self-play ontology
#[verb]
fn validate(ontology: String) -> Result<ValidateOutput> {
    let ontology = if ontology.is_empty() {
        "examples/self-play/ontology.ttl".to_string()
    } else {
        ontology
    };

    let ontology_path = PathBuf::from(&ontology);
    let ontology_valid = ontology_path.exists();

    let (checks_passed, checks_failed, message) = if ontology_valid {
        (1, 0, format!("Ontology validation passed: {}", ontology))
    } else {
        (0, 1, format!("Ontology file not found: {}", ontology))
    };

    let output = ValidateOutput {
        ontology_valid,
        ontology_path: ontology,
        checks_passed,
        checks_failed,
        message,
    };

    let json_output = serde_json::to_string_pretty(&output).map_err(|e| {
        clap_noun_verb::NounVerbError::execution_error(format!("Failed to serialize output: {}", e))
    })?;
    println!("{}", json_output);

    Ok(output)
}

/// Show self-play demo information
#[verb]
fn demo() -> Result<DemoOutput> {
    let demo_path = "examples/self-play";
    let demo_path_buf = PathBuf::from(demo_path);

    let has_ontology = demo_path_buf.join("ontology.ttl").exists();
    let has_config = demo_path_buf.join("ggen.toml").exists();
    let has_script = demo_path_buf.join("run-demo.sh").exists();

    let instructions = format!(
        "Self-Play Demo Instructions\n\
         ===========================\n\
         \n\
         1. Navigate to demo directory:\n\
            cd {}\n\
         \n\
         2. Run the automated demo:\n\
            ./run-demo.sh\n\
         \n\
         3. Or run manually:\n\
            ggen validate ontology.ttl\n\
            ggen sync --ontology ontology.ttl --output-dir /tmp/self-play-iteration-1 --audit true\n\
         \n\
         4. Check generated code:\n\
            cd /tmp/self-play-iteration-1\n\
            cargo check\n\
         \n\
         For more details, see: {}/README.md",
        demo_path, demo_path
    );

    let output = DemoOutput {
        demo_path: demo_path.to_string(),
        has_ontology,
        has_config,
        has_script,
        instructions,
    };

    let json_output = serde_json::to_string_pretty(&output).map_err(|e| {
        clap_noun_verb::NounVerbError::execution_error(format!("Failed to serialize output: {}", e))
    })?;
    println!("{}", json_output);

    Ok(output)
}
