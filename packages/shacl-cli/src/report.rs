//! Validation report generation

use crate::{Error, Result};

/// Validation report
#[derive(Debug)]
pub struct Report {
    pub format: ReportFormat,
}

#[derive(Debug)]
pub enum ReportFormat {
    Turtle,
    Json,
    Html,
    Markdown,
}

impl Report {
    /// Create a new report
    pub fn new(format: ReportFormat) -> Self {
        Self { format }
    }

    /// Generate report content
    pub fn generate(&self) -> Result<String> {
        Ok(String::from("# Validation Report\n"))
    }
}
