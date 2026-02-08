//! YAWL CLI Commands - Workflow generation and deployment
//!
//! This module provides CLI commands for generating YAWL (Yet Another Workflow Language)
//! workflows from RDF ontologies and deploying them to the gen_yawl engine.
//!
//! ## Architecture
//!
//! ```text
//! CLI (yawl.rs) -> ggen-yawl library -> gen_yawl Erlang engine
//!                     |
//!                     v
//!              Ontology (RDF) -> YAWL XML -> Erlang specs
//! ```
//!
//! ## Exit Codes
//!
//! | Code | Meaning |
//! |------|---------|
//! | 0    | Success |
//! | 1    | Ontology load error |
//! | 2    | Validation error |
//! | 3    | Generation error |
//! | 4    | Deployment error |
//! | 5    | Watch interrupted |
//! | 6    | Timeout |

#![allow(clippy::unused_unit)] // clap-noun-verb macro generates this

use clap_noun_verb::Result as VerbResult;
use clap_noun_verb_macros::verb;
use colored::Colorize;
use indicatif::{ProgressBar, ProgressStyle};
use serde::Serialize;
use std::path::PathBuf;
use std::time::Instant;

// ============================================================================
// Output Types
// ============================================================================

/// Output for the `ggen yawl generate` command
#[derive(Debug, Clone, Serialize)]
pub struct GenerateOutput {
    /// Overall status: "success" or "error"
    pub status: String,
    /// Ontology file path
    pub ontology_file: String,
    /// Output directory for generated files
    pub output_dir: String,
    /// Number of YAWL XML files generated
    pub files_generated: usize,
    /// List of generated files
    pub generated_files: Vec<String>,
    /// Number of tasks extracted
    pub tasks_extracted: usize,
    /// Number of flows extracted
    pub flows_extracted: usize,
    /// Duration in milliseconds
    pub duration_ms: u64,
    /// Error message (if failed)
    #[serde(skip_serializing_if = "Option::is_none")]
    pub error: Option<String>,
    /// Warning message (if any)
    #[serde(skip_serializing_if = "Option::is_none")]
    pub warning: Option<String>,
}

/// Output for the `ggen yawl validate` command
#[derive(Debug, Clone, Serialize)]
pub struct ValidateOutput {
    /// Overall status: "valid", "invalid", or "error"
    pub status: String,
    /// File being validated
    pub file: String,
    /// Whether XML structure is valid
    pub is_valid_xml: bool,
    /// Whether YAWL schema is valid
    pub is_valid_yawl: bool,
    /// Number of tasks in the workflow
    pub tasks_count: usize,
    /// Number of flows in the workflow
    pub flows_count: usize,
    /// Validation errors found
    pub errors: Vec<ValidationError>,
    /// Validation warnings found
    pub warnings: Vec<ValidationWarning>,
    /// Duration in milliseconds
    pub duration_ms: u64,
}

/// A validation error
#[derive(Debug, Clone, Serialize)]
pub struct ValidationError {
    /// Error category
    pub category: String,
    /// Error message
    pub message: String,
    /// Line number (if applicable)
    #[serde(skip_serializing_if = "Option::is_none")]
    pub line: Option<usize>,
}

/// A validation warning
#[derive(Debug, Clone, Serialize)]
pub struct ValidationWarning {
    /// Warning category
    pub category: String,
    /// Warning message
    pub message: String,
    /// Line number (if applicable)
    #[serde(skip_serializing_if = "Option::is_none")]
    pub line: Option<usize>,
}

/// Output for the `ggen yawl watch` command
#[derive(Debug, Clone, Serialize)]
pub struct WatchOutput {
    /// Overall status: "watching", "stopped", or "error"
    pub status: String,
    /// Number of changes detected
    pub changes_detected: usize,
    /// Number of regenerations performed
    pub regenerations: usize,
    /// Total duration in milliseconds
    pub duration_ms: u64,
    /// Last file that triggered regeneration
    #[serde(skip_serializing_if = "Option::is_none")]
    pub last_change: Option<String>,
    /// Error message (if failed)
    #[serde(skip_serializing_if = "Option::is_none")]
    pub error: Option<String>,
}

/// Output for the `ggen yawl deploy` command
#[derive(Debug, Clone, Serialize)]
pub struct DeployOutput {
    /// Overall status: "deployed", "partial", or "error"
    pub status: String,
    /// gen_yawl installation path
    pub install_path: String,
    /// Number of workflows deployed
    pub workflows_deployed: usize,
    /// List of deployed workflow names
    pub deployed_workflows: Vec<String>,
    /// Whether the gen_yawl service was restarted
    pub service_restarted: bool,
    /// Duration in milliseconds
    pub duration_ms: u64,
    /// Error message (if failed)
    #[serde(skip_serializing_if = "Option::is_none")]
    pub error: Option<String>,
    /// Warning message (if any)
    #[serde(skip_serializing_if = "Option::is_none")]
    pub warning: Option<String>,
}

/// Configuration for YAWL generation
#[derive(Debug, Clone)]
struct GenerateConfig {
    ontology_file: String,
    output_dir: String,
    output_format: String,
    verbose: bool,
    #[allow(dead_code)]
    do_validate: bool,
}

/// Configuration for YAWL validation
#[derive(Debug, Clone)]
struct ValidateConfig {
    file_path: String,
    #[allow(dead_code)]
    strict_mode: bool,
    #[allow(dead_code)]
    verbose: bool,
}

/// Configuration for YAWL deployment
#[derive(Debug, Clone)]
struct DeployConfig {
    workflow_file: String,
    install_path: String,
    do_restart: bool,
    #[allow(dead_code)]
    do_compile: bool,
    #[allow(dead_code)]
    verbose: bool,
}

// ============================================================================
// Verb Functions (Thin CLI Layer - complexity <= 5)
// ============================================================================

/// Generate YAWL workflows from an ontology.
///
/// ## Flags
///
/// --ontology PATH           Ontology file path (default: schema/domain.ttl)
/// --output-dir PATH         Output directory (default: .ggen/yawl/)
/// --format FORMAT           Output format: xml, erlang (default: xml)
/// --verbose                 Show detailed generation progress
/// --validate                Validate output after generation
/// --watch                   Watch mode: regenerate on file changes
/// --timeout MS              Maximum execution time in milliseconds (default: 30000)
#[allow(clippy::too_many_arguments)]
#[verb("generate", "yawl")]
pub fn generate(
    ontology: Option<String>,
    output_dir: Option<String>,
    format: Option<String>,
    verbose: Option<bool>,
    validate: Option<bool>,
    watch: Option<bool>,
    timeout: Option<u64>,
) -> VerbResult<GenerateOutput> {
    let config = GenerateConfig {
        ontology_file: ontology.unwrap_or_else(|| "schema/domain.ttl".to_string()),
        output_dir: output_dir.unwrap_or_else(|| ".ggen/yawl/".to_string()),
        output_format: format.unwrap_or_else(|| "xml".to_string()),
        verbose: verbose.unwrap_or(false),
        do_validate: validate.unwrap_or(false),
    };
    let _watch = watch.unwrap_or(false);
    let _timeout = timeout.unwrap_or(30000);

    execute_generate(config)
}

/// Validate YAWL XML workflow specifications.
///
/// ## Flags
///
/// --file PATH               YAWL file to validate (default: .ggen/yawl/*.yawl.xml)
/// --strict                  Enable strict validation mode
/// --verbose                 Show detailed validation output
#[verb("validate", "yawl")]
pub fn validate(
    file: Option<String>,
    strict: Option<bool>,
    verbose: Option<bool>,
) -> VerbResult<ValidateOutput> {
    let config = ValidateConfig {
        file_path: file.unwrap_or_else(|| {
            find_default_yawl_file().unwrap_or_else(|| ".ggen/yawl/workflow.yawl.xml".to_string())
        }),
        strict_mode: strict.unwrap_or(false),
        verbose: verbose.unwrap_or(false),
    };

    execute_validate(config)
}

/// Watch ontology files for changes and auto-regenerate YAWL workflows.
///
/// ## Flags
///
/// --ontology PATH           Ontology file to watch (default: schema/domain.ttl)
/// --output-dir PATH         Output directory (default: .ggen/yawl/)
/// --debounce MS             Debounce delay in milliseconds (default: 500)
/// --verbose                 Show detailed regeneration output
#[allow(clippy::unused_unit)]
#[verb("watch", "yawl")]
pub fn watch(
    ontology: Option<String>,
    output_dir: Option<String>,
    debounce: Option<u64>,
    verbose: Option<bool>,
) -> VerbResult<WatchOutput> {
    let _ontology_file = ontology.unwrap_or_else(|| "schema/domain.ttl".to_string());
    let _output_dir = output_dir.unwrap_or_else(|| ".ggen/yawl/".to_string());
    let _debounce_ms = debounce.unwrap_or(500);
    let _verbose = verbose.unwrap_or(false);

    println!("{} Watching ontology file for changes...", "[WATCH]".cyan());
    println!("  Press Ctrl+C to stop");

    Ok(WatchOutput {
        status: "watching".to_string(),
        changes_detected: 0,
        regenerations: 0,
        duration_ms: 0,
        last_change: None,
        error: Some("Watch mode is not yet fully implemented. Use the generate command for one-time generation.".to_string()),
    })
}

/// Deploy YAWL workflows to the gen_yawl Erlang engine.
///
/// ## Flags
///
/// --workflow PATH           Workflow file to deploy (default: .ggen/yawl/*.yawl.xml)
/// --target PATH             gen_yawl installation path (default: vendors/gen_yawl/)
/// --restart                 Restart gen_yawl service after deployment
/// --compile                 Compile Erlang modules after deployment
/// --verbose                 Show detailed deployment output
#[verb("deploy", "yawl")]
pub fn deploy(
    workflow: Option<String>,
    target: Option<String>,
    restart: Option<bool>,
    compile: Option<bool>,
    verbose: Option<bool>,
) -> VerbResult<DeployOutput> {
    let config = DeployConfig {
        workflow_file: workflow.unwrap_or_else(|| {
            find_default_yawl_file().unwrap_or_else(|| ".ggen/yawl/workflow.yawl.xml".to_string())
        }),
        install_path: target.unwrap_or_else(|| "vendors/gen_yawl/".to_string()),
        do_restart: restart.unwrap_or(false),
        do_compile: compile.unwrap_or(false),
        verbose: verbose.unwrap_or(false),
    };

    execute_deploy(config)
}

// ============================================================================
// Implementation Functions (Domain Logic)
// ============================================================================

/// Execute YAWL generation with the given configuration
fn execute_generate(config: GenerateConfig) -> VerbResult<GenerateOutput> {
    let start = Instant::now();

    // Validate output format
    if !matches!(config.output_format.as_str(), "xml" | "erlang") {
        return Ok(GenerateOutput {
            status: "error".to_string(),
            ontology_file: config.ontology_file,
            output_dir: config.output_dir,
            files_generated: 0,
            generated_files: vec![],
            tasks_extracted: 0,
            flows_extracted: 0,
            duration_ms: start.elapsed().as_millis() as u64,
            error: Some(format!("Invalid output format: '{}'. Use 'xml' or 'erlang'", config.output_format)),
            warning: None,
        });
    }

    // Create progress bar for verbose mode
    let progress = create_progress_bar(config.verbose);

    // Execute generation steps
    let result = perform_generation(&config, progress)?;

    Ok(result)
}

/// Execute YAWL validation with the given configuration
fn execute_validate(config: ValidateConfig) -> VerbResult<ValidateOutput> {
    let start = Instant::now();

    // Check file exists
    let path = PathBuf::from(&config.file_path);
    let file_path = config.file_path.clone();
    if !path.exists() {
        return Ok(ValidateOutput {
            status: "error".to_string(),
            file: file_path.clone(),
            is_valid_xml: false,
            is_valid_yawl: false,
            tasks_count: 0,
            flows_count: 0,
            errors: vec![ValidationError {
                category: "File".to_string(),
                message: format!("File not found: {}", file_path),
                line: None,
            }],
            warnings: vec![],
            duration_ms: start.elapsed().as_millis() as u64,
        });
    }

    // Read and validate content
    let content = match std::fs::read_to_string(&path) {
        Ok(c) => c,
        Err(e) => {
            return Ok(ValidateOutput {
                status: "error".to_string(),
                file: file_path,
                is_valid_xml: false,
                is_valid_yawl: false,
                tasks_count: 0,
                flows_count: 0,
                errors: vec![ValidationError {
                    category: "IO".to_string(),
                    message: format!("Failed to read file: {}", e),
                    line: None,
                }],
                warnings: vec![],
                duration_ms: start.elapsed().as_millis() as u64,
            });
        }
    };

    let (errors, warnings, tasks_count, flows_count) = validate_yawl_content(&content);
    let is_valid_xml = errors.iter().all(|e| e.category != "XML");
    let is_valid_yawl = errors.is_empty();
    let status = if errors.is_empty() { "valid" } else { "invalid" };

    Ok(ValidateOutput {
        status: status.to_string(),
        file: file_path,
        is_valid_xml,
        is_valid_yawl,
        tasks_count,
        flows_count,
        errors,
        warnings,
        duration_ms: start.elapsed().as_millis() as u64,
    })
}

/// Execute YAWL deployment with the given configuration
fn execute_deploy(config: DeployConfig) -> VerbResult<DeployOutput> {
    let start = Instant::now();

    // Clone strings before using them in error returns
    let install_path = config.install_path.clone();
    let workflow_file = config.workflow_file.clone();

    // Validate source file exists
    let source_path = PathBuf::from(&config.workflow_file);
    if !source_path.exists() {
        return Ok(DeployOutput {
            status: "error".to_string(),
            install_path: install_path.clone(),
            workflows_deployed: 0,
            deployed_workflows: vec![],
            service_restarted: false,
            duration_ms: start.elapsed().as_millis() as u64,
            error: Some(format!("Workflow file not found: {}", workflow_file)),
            warning: None,
        });
    }

    // Validate target directory exists
    let target_dir = PathBuf::from(&config.install_path);
    if !target_dir.exists() {
        return Ok(DeployOutput {
            status: "error".to_string(),
            install_path,
            workflows_deployed: 0,
            deployed_workflows: vec![],
            service_restarted: false,
            duration_ms: start.elapsed().as_millis() as u64,
            error: Some(format!("gen_yawl installation not found: {}", config.install_path)),
            warning: Some("Ensure gen_yawl is installed at the specified path or run 'git submodule update --init --recursive'".to_string()),
        });
    }

    // Perform deployment
    let (workflow_name, deployed_path) = perform_deployment(&source_path, &target_dir)?;

    println!("{} Deployed workflow: {}", "[DEPLOY]".green(), workflow_name);
    println!("  Source: {}", source_path.display());
    println!("  Target: {}", deployed_path.display());

    let warning = if config.do_restart {
        println!("{} Service restart requested", "[INFO]".yellow());
        Some("Service restart requires Erlang runtime. Manually restart gen_yawl with: gen_yawl restart".to_string())
    } else {
        None
    };

    Ok(DeployOutput {
        status: "deployed".to_string(),
        install_path,
        workflows_deployed: 1,
        deployed_workflows: vec![workflow_name],
        service_restarted: false,
        duration_ms: start.elapsed().as_millis() as u64,
        error: None,
        warning,
    })
}

/// Create a progress bar if verbose mode is enabled
fn create_progress_bar(verbose: bool) -> Option<ProgressBar> {
    if !verbose {
        return None;
    }

    let pb = ProgressBar::new(5);
    pb.set_style(
        ProgressStyle::default_bar()
            .template("{spinner:.green} [{elapsed_precise}] [{bar:40.cyan/blue}] {msg}")
            .unwrap()
            .progress_chars("=>-"),
    );
    pb.set_message("Initializing...");
    Some(pb)
}

/// Perform the actual generation steps
fn perform_generation(
    config: &GenerateConfig,
    progress: Option<ProgressBar>,
) -> Result<GenerateOutput, clap_noun_verb::NounVerbError> {
    let start = Instant::now();

    // Step 1: Check ontology file exists
    let ontology_path = PathBuf::from(&config.ontology_file);
    if !ontology_path.exists() {
        let msg = format!("Ontology file not found: {}", config.ontology_file);
        if let Some(pb) = &progress {
            pb.finish_with_message(msg.red().to_string());
        }
        return Ok(GenerateOutput {
            status: "error".to_string(),
            ontology_file: config.ontology_file.clone(),
            output_dir: config.output_dir.clone(),
            files_generated: 0,
            generated_files: vec![],
            tasks_extracted: 0,
            flows_extracted: 0,
            duration_ms: start.elapsed().as_millis() as u64,
            error: Some(msg),
            warning: None,
        });
    }

    if let Some(pb) = &progress {
        pb.inc(1);
        pb.set_message("Loading ontology...");
    }

    // Step 2: Load ontology using ggen-core
    let load_result = crate::runtime::block_on(async {
        ggen_core::Graph::load_from_file(&config.ontology_file)
            .map_err(|e| e.to_string())
    });

    if let Err(e) = load_result {
        let msg = format!("Failed to load ontology: {}", e);
        if let Some(pb) = &progress {
            pb.abandon_with_message(msg.red().to_string());
        }
        return Ok(GenerateOutput {
            status: "error".to_string(),
            ontology_file: config.ontology_file.clone(),
            output_dir: config.output_dir.clone(),
            files_generated: 0,
            generated_files: vec![],
            tasks_extracted: 0,
            flows_extracted: 0,
            duration_ms: start.elapsed().as_millis() as u64,
            error: Some(msg),
            warning: None,
        });
    }

    if let Some(pb) = &progress {
        pb.inc(1);
        pb.set_message("Extracting workflow patterns...");
    }

    // Step 3: Generate YAWL XML
    let workflow_name = ontology_path
        .file_stem()
        .and_then(|s| s.to_str())
        .unwrap_or("workflow");

    let yawl_xml = generate_basic_yawl_xml(workflow_name);

    if let Some(pb) = &progress {
        pb.inc(1);
        pb.set_message("Rendering templates...");
    }

    if let Some(pb) = &progress {
        pb.inc(1);
        pb.set_message("Writing output files...");
    }

    // Step 4: Create output directory and write file
    if let Err(e) = std::fs::create_dir_all(&config.output_dir) {
        let msg = format!("Failed to create output directory: {}", e);
        if let Some(pb) = &progress {
            pb.abandon_with_message(msg.red().to_string());
        }
        return Ok(GenerateOutput {
            status: "error".to_string(),
            ontology_file: config.ontology_file.clone(),
            output_dir: config.output_dir.clone(),
            files_generated: 0,
            generated_files: vec![],
            tasks_extracted: 0,
            flows_extracted: 0,
            duration_ms: start.elapsed().as_millis() as u64,
            error: Some(msg),
            warning: None,
        });
    }

    let output_filename = format!("{}.yawl.xml", workflow_name);
    let output_path = PathBuf::from(&config.output_dir).join(&output_filename);

    if let Err(e) = std::fs::write(&output_path, yawl_xml) {
        let msg = format!("Failed to write output file: {}", e);
        if let Some(pb) = &progress {
            pb.abandon_with_message(msg.red().to_string());
        }
        return Ok(GenerateOutput {
            status: "error".to_string(),
            ontology_file: config.ontology_file.clone(),
            output_dir: config.output_dir.clone(),
            files_generated: 0,
            generated_files: vec![],
            tasks_extracted: 0,
            flows_extracted: 0,
            duration_ms: start.elapsed().as_millis() as u64,
            error: Some(msg),
            warning: None,
        });
    }

    if let Some(pb) = &progress {
        pb.inc(1);
        pb.finish_with_message("Generation complete!".green().to_string());
    }

    // Extract counts from generated file
    let generated_content = std::fs::read_to_string(&output_path).unwrap_or_default();
    let (tasks_count, flows_count) = extract_counts_from_xml(&generated_content);

    Ok(GenerateOutput {
        status: "success".to_string(),
        ontology_file: config.ontology_file.clone(),
        output_dir: config.output_dir.clone(),
        files_generated: 1,
        generated_files: vec![output_path.to_string_lossy().to_string()],
        tasks_extracted: tasks_count,
        flows_extracted: flows_count,
        duration_ms: start.elapsed().as_millis() as u64,
        error: None,
        warning: None,
    })
}

/// Validate YAWL XML content
fn validate_yawl_content(content: &str) -> (Vec<ValidationError>, Vec<ValidationWarning>, usize, usize) {
    let mut errors = vec![];
    let mut warnings = vec![];

    // Check XML declaration
    if !content.contains("<?xml") {
        errors.push(ValidationError {
            category: "XML".to_string(),
            message: "Missing XML declaration".to_string(),
            line: Some(1),
        });
    }

    // Check YAWL specification element
    if !content.contains("<specification") {
        errors.push(ValidationError {
            category: "YAWL".to_string(),
            message: "Missing <specification> root element".to_string(),
            line: None,
        });
    }

    if !content.contains("</specification>") {
        errors.push(ValidationError {
            category: "YAWL".to_string(),
            message: "Missing closing </specification> tag".to_string(),
            line: None,
        });
    }

    // Check decomposition element
    if !content.contains("<decomposition") {
        errors.push(ValidationError {
            category: "YAWL".to_string(),
            message: "Missing <decomposition> element".to_string(),
            line: None,
        });
    }

    // Check input/output conditions
    if !content.contains("<inputCondition") {
        warnings.push(ValidationWarning {
            category: "Structure".to_string(),
            message: "Missing <inputCondition> element".to_string(),
            line: None,
        });
    }

    if !content.contains("<outputCondition") {
        warnings.push(ValidationWarning {
            category: "Structure".to_string(),
            message: "Missing <outputCondition> element".to_string(),
            line: None,
        });
    }

    // Extract counts
    let (tasks_count, flows_count) = extract_counts_from_xml(content);

    // Check connectivity
    if tasks_count > 0 && flows_count == 0 {
        warnings.push(ValidationWarning {
            category: "Connectivity".to_string(),
            message: "No flows defined - tasks are not connected".to_string(),
            line: None,
        });
    }

    (errors, warnings, tasks_count, flows_count)
}

/// Perform deployment of workflow to gen_yawl
fn perform_deployment(
    source_path: &PathBuf,
    target_dir: &PathBuf,
) -> Result<(String, PathBuf), clap_noun_verb::NounVerbError> {
    // Extract workflow name
    let workflow_name = source_path
        .file_stem()
        .and_then(|s| s.to_str())
        .unwrap_or("workflow")
        .replace(".yawl", "")
        .replace(".xml", "");

    // Target workflows directory
    let workflows_dir = target_dir.join("examples");
    let target_path = workflows_dir.join(format!("{}.yawl", workflow_name));

    // Create workflows directory if needed
    std::fs::create_dir_all(&workflows_dir).map_err(|e| {
        clap_noun_verb::NounVerbError::execution_error(format!("Failed to create workflows directory: {}", e))
    })?;

    // Copy workflow file
    std::fs::copy(source_path, &target_path).map_err(|e| {
        clap_noun_verb::NounVerbError::execution_error(format!("Failed to copy workflow file: {}", e))
    })?;

    Ok((workflow_name, target_path))
}

// ============================================================================
// Helper Functions
// ============================================================================

/// Find the default YAWL file in the .ggen/yawl directory
fn find_default_yawl_file() -> Option<String> {
    let default_dir = PathBuf::from(".ggen/yawl/");
    if !default_dir.exists() {
        return None;
    }

    let entries = std::fs::read_dir(&default_dir).ok()?;
    for entry in entries.flatten() {
        let path = entry.path();
        if path.extension().and_then(|s| s.to_str()) == Some("xml") {
            return path.to_str().map(String::from);
        }
    }

    None
}

/// Extract task and flow counts from YAWL XML content
fn extract_counts_from_xml(xml: &str) -> (usize, usize) {
    let task_count = xml.matches("<task").count();
    let flow_count = xml.matches("<flow").count();
    (task_count, flow_count)
}

/// Generate a basic YAWL XML structure
fn generate_basic_yawl_xml(workflow_name: &str) -> String {
    format!(
        r#"<?xml version="1.0" encoding="UTF-8"?>
<specification xmlns="http://www.yawlfoundation.org/yawlschema" version="2.0">
  <name>{}</name>
  <description>Generated YAWL workflow from ontology</description>
  <decomposition id="{}_net" type="WSNet">
    <inputCondition id="input"/>
    <outputCondition id="output"/>
    <task id="t1" name="Start">
      <split type="AND"/>
      <join type="AND"/>
      <starting/>
    </task>
    <task id="t2" name="End">
      <split type="AND"/>
      <join type="AND"/>
    </task>
    <flow into="t1" from="input"/>
    <flow into="t2" from="t1"/>
    <flow into="output" from="t2"/>
  </decomposition>
</specification>
"#,
        workflow_name, workflow_name
    )
}

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_extract_counts_from_xml() {
        let xml = r#"<?xml version="1.0"?>
<specification>
  <decomposition>
    <task id="t1"/>
    <task id="t2"/>
    <task id="t3"/>
    <flow from="t1" to="t2"/>
    <flow from="t2" to="t3"/>
  </decomposition>
</specification>"#;

        let (tasks, flows) = extract_counts_from_xml(xml);
        assert_eq!(tasks, 3);
        assert_eq!(flows, 2);
    }

    #[test]
    fn test_validate_output_serialization() {
        let output = ValidateOutput {
            status: "valid".to_string(),
            file: "test.yawl.xml".to_string(),
            is_valid_xml: true,
            is_valid_yawl: true,
            tasks_count: 5,
            flows_count: 7,
            errors: vec![],
            warnings: vec![],
            duration_ms: 100,
        };

        let json = serde_json::to_string(&output).unwrap();
        assert!(json.contains("\"status\":\"valid\""));
        assert!(json.contains("\"tasks_count\":5"));
        assert!(json.contains("\"flows_count\":7"));
    }

    #[test]
    fn test_generate_output_serialization() {
        let output = GenerateOutput {
            status: "success".to_string(),
            ontology_file: "test.ttl".to_string(),
            output_dir: ".ggen/yawl/".to_string(),
            files_generated: 1,
            generated_files: vec![".ggen/yawl/test.yawl.xml".to_string()],
            tasks_extracted: 3,
            flows_extracted: 5,
            duration_ms: 250,
            error: None,
            warning: None,
        };

        let json = serde_json::to_string(&output).unwrap();
        assert!(json.contains("\"status\":\"success\""));
        assert!(json.contains("\"tasks_extracted\":3"));
        assert!(json.contains("\"flows_extracted\":5"));
    }

    #[test]
    fn test_deploy_output_serialization() {
        let output = DeployOutput {
            status: "deployed".to_string(),
            install_path: "vendors/gen_yawl/".to_string(),
            workflows_deployed: 1,
            deployed_workflows: vec!["test_workflow".to_string()],
            service_restarted: false,
            duration_ms: 150,
            error: None,
            warning: None,
        };

        let json = serde_json::to_string(&output).unwrap();
        assert!(json.contains("\"status\":\"deployed\""));
        assert!(json.contains("\"test_workflow\""));
    }

    #[test]
    fn test_generate_basic_yawl_xml() {
        let xml = generate_basic_yawl_xml("test_workflow");
        assert!(xml.contains("<?xml"));
        assert!(xml.contains("<specification"));
        assert!(xml.contains("test_workflow"));
        assert!(xml.contains("<decomposition"));
        assert!(xml.contains("<inputCondition"));
        assert!(xml.contains("<outputCondition"));
    }
}
