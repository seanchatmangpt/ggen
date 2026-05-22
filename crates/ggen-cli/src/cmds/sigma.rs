//! Six Sigma / DFLSS Commands
//!
//! This module provides commands for Design for Lean Six Sigma (DFLSS)
//! validation and quality engineering.

use crate::prelude::*;
use ggen_core::dflss::{execute_dflss, DflssReport};
use ggen_core::manifest::ManifestParser;
use serde::Serialize;
use std::path::{Path, PathBuf};

#[derive(Debug, Serialize)]
pub struct SigmaOutput {
    pub status: String,
    pub methodology: String,
    pub reports: Vec<DflssReport>,
    pub overall_passed: bool,
}

/// Run Design for Lean Six Sigma (DFLSS) validation
#[verb("sigma", "root")]
pub fn sigma(phase: Option<String>) -> clap_noun_verb::Result<SigmaOutput> {
    // 1. Setup paths and parse manifest
    let manifest_path = PathBuf::from("ggen.toml");
    let base_path = manifest_path.parent().unwrap_or(Path::new("."));

    let manifest = ManifestParser::parse(&manifest_path).map_err(|e| {
        clap_noun_verb::NounVerbError::execution_error(format!("Failed to parse manifest: {}", e))
    })?;

    // 2. Delegate to domain logic
    let reports = execute_dflss(&manifest, base_path, phase).map_err(|e| {
        clap_noun_verb::NounVerbError::execution_error(format!("Gate execution failed: {}", e))
    })?;

    // 3. Format and return
    let overall_passed = reports.iter().all(|r| r.passed);

    Ok(SigmaOutput {
        status: if overall_passed {
            "success".to_string()
        } else {
            "error".to_string()
        },
        methodology: "DMADV (DFLSS)".to_string(),
        reports,
        overall_passed,
    })
}
