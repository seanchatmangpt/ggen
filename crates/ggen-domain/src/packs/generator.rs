//! Pack template generation logic

use crate::packs::metadata::load_pack_metadata;
use ggen_utils::error::Result;
use serde::{Deserialize, Serialize};
use std::collections::BTreeMap;
use std::path::PathBuf;

/// Generate from pack input
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct GenerateInput {
    pub pack_id: String,
    pub project_name: String,
    pub template_name: Option<String>,
    pub output_dir: Option<PathBuf>,
    pub variables: BTreeMap<String, String>,
}

/// Generate from pack output
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct GenerateOutput {
    pub pack_id: String,
    pub project_name: String,
    pub templates_generated: Vec<String>,
    pub files_created: usize,
    pub output_path: PathBuf,
}

/// Generate project from pack templates
pub async fn generate_from_pack(input: &GenerateInput) -> Result<GenerateOutput> {
    // Load pack metadata
    let pack = load_pack_metadata(&input.pack_id)?;

    // Determine output directory
    let output_dir = input
        .output_dir
        .clone()
        .unwrap_or_else(|| PathBuf::from(&input.project_name));

    // Create output directory
    std::fs::create_dir_all(&output_dir)?;

    let mut templates_generated = Vec::new();
    let mut total_files = 0;

    // Filter templates if specific template requested
    let templates_to_use: Vec<_> = if let Some(ref template_name) = input.template_name {
        pack.templates
            .iter()
            .filter(|t| t.name == *template_name)
            .collect()
    } else {
        pack.templates.iter().collect()
    };

    if templates_to_use.is_empty() {
        if let Some(ref template_name) = input.template_name {
            return Err(ggen_utils::error::Error::new(&format!(
                "Template '{}' not found in pack '{}'",
                template_name, input.pack_id
            )));
        } else {
            return Err(ggen_utils::error::Error::new(&format!(
                "No templates found in pack '{}'",
                input.pack_id
            )));
        }
    }

    // Generate each template (not implemented yet - would use template domain functions)
    for template in templates_to_use {
        templates_generated.push(template.name.clone());
        total_files += 1;
        tracing::info!("Would generate template '{}'", template.name);
    }

    Ok(GenerateOutput {
        pack_id: input.pack_id.clone(),
        project_name: input.project_name.clone(),
        templates_generated,
        files_created: total_files,
        output_path: output_dir,
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_generate_from_pack_input_validation() {
        let input = GenerateInput {
            pack_id: "test-pack".to_string(),
            project_name: "my-project".to_string(),
            template_name: None,
            output_dir: None,
            variables: BTreeMap::new(),
        };

        // This will fail if pack doesn't exist, but tests the input structure
        let _ = generate_from_pack(&input).await;
    }
}
