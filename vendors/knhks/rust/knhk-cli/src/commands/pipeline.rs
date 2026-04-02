// rust/knhk-cli/src/commands/pipeline.rs
// Pipeline commands - ETL pipeline operations

use std::fs;
use std::path::PathBuf;
use serde::{Deserialize, Serialize};

#[cfg(feature = "std")]
use knhk_etl::integration::IntegratedPipeline;

use crate::commands::connect;

/// Pipeline execution status
#[derive(Debug, Serialize, Deserialize)]
struct PipelineStatus {
    last_execution: Option<u64>,  // timestamp_ms
    last_result: Option<PipelineExecutionResult>,
}

#[derive(Debug, Serialize, Deserialize)]
struct PipelineExecutionResult {
    receipts_written: usize,
    actions_sent: usize,
    lockchain_hashes: Vec<String>,
    timestamp_ms: u64,
}

/// Execute full ETL pipeline (Ingest → Emit)
pub fn run(connectors: Option<String>, schema: Option<String>) -> Result<(), String> {
    println!("Executing ETL pipeline...");
    
    // Load connector configuration
    let connector_storage = connect::load_connectors()?;
    
    // Determine connectors to use
    let connector_ids: Vec<String> = if let Some(ref conns) = connectors {
        conns.split(',').map(|s| s.trim().to_string()).collect()
    } else {
        // Use all registered connectors
        connector_storage.connectors.iter().map(|c| c.name.clone()).collect()
    };
    
    if connector_ids.is_empty() {
        return Err("No connectors specified or registered".to_string());
    }
    
    // Get schema (default or from config)
    let schema_iri = schema.unwrap_or_else(|| {
        // Try to get schema from first connector
        connector_storage.connectors.first()
            .map(|c| c.schema.clone())
            .unwrap_or_else(|| "urn:knhk:schema:default".to_string())
    });
    
    println!("  Connectors: {}", connector_ids.join(", "));
    println!("  Schema: {}", schema_iri);
    
    #[cfg(feature = "std")]
    {
        // Create integrated pipeline
        let mut pipeline = IntegratedPipeline::new(
            connector_ids,
            schema_iri,
            true,  // lockchain enabled
            vec![], // downstream endpoints (empty for now)
        );
        
        // Execute pipeline
        match pipeline.execute() {
            Ok(result) => {
                println!("✓ Pipeline execution completed");
                println!("  Receipts written: {}", result.receipts_written);
                println!("  Actions sent: {}", result.actions_sent);
                println!("  Lockchain hashes: {}", result.lockchain_hashes.len());
                
                // Save execution status
                let status = PipelineStatus {
                    last_execution: Some(get_current_timestamp_ms()),
                    last_result: Some(PipelineExecutionResult {
                        receipts_written: result.receipts_written,
                        actions_sent: result.actions_sent,
                        lockchain_hashes: result.lockchain_hashes,
                        timestamp_ms: get_current_timestamp_ms(),
                    }),
                };
                save_pipeline_status(&status)?;
                
                Ok(())
            }
            Err(e) => {
                Err(format!("Pipeline execution failed: {:?}", e))
            }
        }
    }
    
    #[cfg(not(feature = "std"))]
    {
        Err("Pipeline execution requires std feature".to_string())
    }
}

/// Show pipeline execution status and metrics
pub fn status() -> Result<(), String> {
    let status = load_pipeline_status()?;
    
    if let Some(ref result) = status.last_result {
        println!("Pipeline Status:");
        println!("  Last execution: {} ms ago", 
                 get_current_timestamp_ms().saturating_sub(result.timestamp_ms));
        println!("  Receipts written: {}", result.receipts_written);
        println!("  Actions sent: {}", result.actions_sent);
        println!("  Lockchain hashes: {}", result.lockchain_hashes.len());
        
        if let Some(latest_hash) = result.lockchain_hashes.last() {
            println!("  Latest hash: {}", latest_hash);
        }
    } else {
        println!("Pipeline Status: No executions recorded");
    }
    
    Ok(())
}

fn get_config_dir() -> Result<PathBuf, String> {
    #[cfg(target_os = "windows")]
    {
        let mut path = PathBuf::from(std::env::var("APPDATA").map_err(|_| "APPDATA not set")?);
        path.push("knhk");
        Ok(path)
    }
    
    #[cfg(not(target_os = "windows"))]
    {
        let home = std::env::var("HOME").map_err(|_| "HOME not set")?;
        let mut path = PathBuf::from(home);
        path.push(".knhk");
        Ok(path)
    }
}

fn get_current_timestamp_ms() -> u64 {
    #[cfg(feature = "std")]
    {
        use std::time::{SystemTime, UNIX_EPOCH};
        SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .map(|d| d.as_millis() as u64)
            .unwrap_or(0)
    }
    
    #[cfg(not(feature = "std"))]
    {
        0
    }
}

fn load_pipeline_status() -> Result<PipelineStatus, String> {
    let config_dir = get_config_dir()?;
    let status_file = config_dir.join("pipeline_status.json");
    
    if !status_file.exists() {
        return Ok(PipelineStatus {
            last_execution: None,
            last_result: None,
        });
    }
    
    let content = fs::read_to_string(&status_file)
        .map_err(|e| format!("Failed to read pipeline status file: {}", e))?;
    
    let status: PipelineStatus = serde_json::from_str(&content)
        .map_err(|e| format!("Failed to parse pipeline status file: {}", e))?;
    
    Ok(status)
}

fn save_pipeline_status(status: &PipelineStatus) -> Result<(), String> {
    let config_dir = get_config_dir()?;
    fs::create_dir_all(&config_dir)
        .map_err(|e| format!("Failed to create config directory: {}", e))?;
    
    let status_file = config_dir.join("pipeline_status.json");
    let content = serde_json::to_string_pretty(status)
        .map_err(|e| format!("Failed to serialize pipeline status: {}", e))?;
    
    fs::write(&status_file, content)
        .map_err(|e| format!("Failed to write pipeline status file: {}", e))?;
    
    Ok(())
}

