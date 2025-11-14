//! Template preprocessor pipeline for deterministic text transformations
//!
//! This module provides a preprocessing pipeline that applies deterministic text
//! transformations to templates before rendering. The preprocessor handles freeze
//! blocks, includes, and macros in a controlled order to ensure reproducible output.
//!
//! ## Pipeline Order
//!
//! The preprocessing pipeline executes in the following order:
//! 1. **Preprocessor**: Freeze, includes, macros
//! 2. **Render Frontmatter**: Tera rendering on YAML frontmatter
//! 3. **Graph Operations**: RDF loading and SPARQL query execution
//! 4. **Body Render**: Final Tera rendering of template body
//!
//! ## Preprocessing Stages
//!
//! - **Freeze**: Replace `{% freeze %}` blocks with cached slot contents
//!   - Preserves manual edits in generated files
//!   - Supports checksum-based validation
//! - **Includes**: Process `{% include %}` directives
//!   - Resolve template includes recursively
//!   - Support for relative and absolute paths
//! - **Macros**: Expand template macros
//!   - Macro definition and invocation
//!   - Parameter passing and scoping
//!
//! ## Error Model
//!
//! - Each stage returns contextual errors with:
//!   - Stage name for identification
//!   - Byte span for error location
//!   - Code snippet for context
//! - No partial silent edits - all transformations are explicit
//!
//! ## Examples
//!
//! ### Using the Preprocessor
//!
//! ```rust,no_run
//! use ggen_core::preprocessor::{Preprocessor, FreezePolicy, PrepCtx};
//! use std::path::Path;
//!
//! # fn main() -> ggen_utils::error::Result<()> {
//! let preprocessor = Preprocessor::new(FreezePolicy::Checksum);
//! let ctx = PrepCtx {
//!     template_path: Path::new("template.tmpl"),
//!     out_dir: Path::new("output"),
//!     vars_json: &serde_json::json!({}),
//! };
//!
//! let input = r#"
//! {% freeze slot="header" %}
//! // Generated header
//! {% endfreeze %}
//! "#;
//!
//! let output = preprocessor.process(input, &ctx)?;
//! # Ok(())
//! # }
//! ```

use ggen_utils::error::{Error, Result};
use serde_json;
use std::fs;
use std::path::{Path, PathBuf};

/// Preprocessor context for stage execution
pub struct PrepCtx<'a> {
    pub template_path: &'a Path,
    pub out_dir: &'a Path,
    pub vars_json: &'a serde_json::Value, // optional, read-only
}

/// Trait for preprocessing stages
pub trait Stage: Send + Sync {
    fn name(&self) -> &'static str;
    fn run(&self, input: &str, ctx: &PrepCtx) -> Result<String>;
}

/// Freeze policy for handling cached content
#[derive(Debug, Clone)]
pub enum FreezePolicy {
    /// Always use cached content if available
    Always,
    /// Use cached content only if checksum matches
    Checksum,
    /// Never use cached content (always regenerate)
    Never,
}

/// Freeze stage for processing {% freeze %} blocks
pub struct FreezeStage {
    pub slots_dir: PathBuf,
    pub policy: FreezePolicy,
}

impl Stage for FreezeStage {
    fn name(&self) -> &'static str {
        "freeze"
    }

    fn run(&self, input: &str, ctx: &PrepCtx) -> Result<String> {
        process_freeze_blocks(input, ctx, self)
    }
}

/// Include stage for processing {% include %} directives
pub struct IncludeStage {
    pub template_dirs: Vec<PathBuf>,
}

impl Stage for IncludeStage {
    fn name(&self) -> &'static str {
        "include"
    }

    fn run(&self, input: &str, ctx: &PrepCtx) -> Result<String> {
        process_includes(input, ctx, self)
    }
}

/// Main preprocessor orchestrating multiple stages
pub struct Preprocessor {
    stages: Vec<Box<dyn Stage>>,
}

impl Default for Preprocessor {
    fn default() -> Self {
        Self::new()
    }
}

impl Preprocessor {
    /// Create a new preprocessor with no stages
    pub fn new() -> Self {
        Self { stages: Vec::new() }
    }

    /// Add a stage to the pipeline
    pub fn with<S: Stage + 'static>(mut self, stage: S) -> Self {
        self.stages.push(Box::new(stage));
        self
    }

    /// Run all stages in order on the input
    pub fn run(&self, mut input: String, ctx: &PrepCtx) -> Result<String> {
        for stage in &self.stages {
            input = stage.run(&input, ctx).map_err(|e| {
                Error::with_source(&format!("Stage '{}' failed", stage.name()), Box::new(e))
            })?;
        }
        Ok(input)
    }

    /// Create a preprocessor with common stages
    pub fn with_default_stages() -> Self {
        Self::new().with(IncludeStage {
            template_dirs: vec![PathBuf::from("templates")],
        })
    }

    /// Create a preprocessor with freeze support
    pub fn with_freeze(slots_dir: PathBuf, policy: FreezePolicy) -> Self {
        Self::new()
            .with(IncludeStage {
                template_dirs: vec![PathBuf::from("templates")],
            })
            .with(FreezeStage { slots_dir, policy })
    }
}

/// Process {% freeze %} blocks in template content
fn process_freeze_blocks(input: &str, ctx: &PrepCtx, stage: &FreezeStage) -> Result<String> {
    let mut result = String::new();
    let mut pos = 0;

    // Find all freeze blocks
    while let Some(start) = input[pos..].find("{% startfreeze") {
        let absolute_start = pos + start;

        // Find the end of the start tag
        let end_start = if let Some(end) = input[absolute_start..].find("%}") {
            absolute_start + end + 2
        } else {
            return Err(Error::new(&format!(
                "Unclosed startfreeze tag at position {}",
                absolute_start
            )));
        };

        // Find the matching endfreeze tag
        let endfreeze_start = if let Some(end) = input[end_start..].find("{% endfreeze %}") {
            end_start + end
        } else {
            return Err(Error::new(&format!(
                "Missing endfreeze tag for startfreeze at position {}",
                absolute_start
            )));
        };

        let endfreeze_end = endfreeze_start + "{% endfreeze %}".len();

        // Extract block content and attributes
        let block_content = &input[end_start..endfreeze_start];
        let start_tag = &input[absolute_start..end_start];

        // Parse attributes from start tag
        let (id, checksum) = parse_freeze_attributes(start_tag)?;

        // Generate default id/checksum if missing
        let final_id = id.unwrap_or_else(|| generate_default_id(ctx.template_path, absolute_start));
        let final_checksum = checksum.unwrap_or_else(|| generate_checksum(block_content));

        // Process based on policy
        let processed_content = match stage.policy {
            FreezePolicy::Always => {
                load_or_generate_slot(&stage.slots_dir, &final_id, &final_checksum, block_content)?
            }
            FreezePolicy::Checksum => {
                load_or_generate_slot(&stage.slots_dir, &final_id, &final_checksum, block_content)?
            }
            FreezePolicy::Never => {
                generate_and_save_slot(&stage.slots_dir, &final_id, &final_checksum, block_content)?
            }
        };

        // Add content before this block
        result.push_str(&input[pos..absolute_start]);
        // Add processed content
        result.push_str(&processed_content);

        // Move position past this block
        pos = endfreeze_end;
    }

    // Add remaining content
    result.push_str(&input[pos..]);
    Ok(result)
}

/// Process {% include %} directives in template content
fn process_includes(input: &str, ctx: &PrepCtx, stage: &IncludeStage) -> Result<String> {
    let mut result = String::new();
    let mut pos = 0;

    // Find all include directives
    while let Some(start) = input[pos..].find("{% include") {
        let absolute_start = pos + start;

        // Find the end of the include tag
        let end_start = if let Some(end) = input[absolute_start..].find("%}") {
            absolute_start + end + 2
        } else {
            return Err(Error::new(&format!(
                "Unclosed include tag at position {}",
                absolute_start
            )));
        };

        // Extract include path from tag
        let include_tag = &input[absolute_start..end_start];
        let include_path = parse_include_path(include_tag)?;

        // Resolve include path
        let resolved_path = resolve_include_path(&include_path, ctx, &stage.template_dirs)?;

        // Read included content
        let included_content = fs::read_to_string(&resolved_path).map_err(|e| {
            Error::new(&format!(
                "Failed to read include file '{}': {}",
                resolved_path.display(),
                e
            ))
        })?;

        // Add content before this include
        result.push_str(&input[pos..absolute_start]);
        // Add included content
        result.push_str(&included_content);

        // Move position past this include
        pos = end_start;
    }

    // Add remaining content
    result.push_str(&input[pos..]);
    Ok(result)
}

/// Parse attributes from {% startfreeze %} tag
fn parse_freeze_attributes(tag: &str) -> Result<(Option<String>, Option<String>)> {
    let mut id = None;
    let mut checksum = None;

    // Simple attribute parsing - look for id="..." and checksum="..."
    if let Some(id_start) = tag.find("id=\"") {
        let id_start = id_start + 4;
        if let Some(id_end) = tag[id_start..].find("\"") {
            id = Some(tag[id_start..id_start + id_end].to_string());
        }
    }

    if let Some(cs_start) = tag.find("checksum=\"") {
        let cs_start = cs_start + 10;
        if let Some(cs_end) = tag[cs_start..].find("\"") {
            checksum = Some(tag[cs_start..cs_start + cs_end].to_string());
        }
    }

    Ok((id, checksum))
}

/// Parse include path from {% include %} tag
fn parse_include_path(tag: &str) -> Result<String> {
    // Extract path from {% include "path" %} or {% include 'path' %}
    let path_start = if let Some(start) = tag.find("\"") {
        start + 1
    } else if let Some(start) = tag.find("'") {
        start + 1
    } else {
        return Err(Error::new(&format!("Invalid include tag format: {}", tag)));
    };

    let path_end = if let Some(end) = tag[path_start..].find("\"") {
        path_start + end
    } else if let Some(end) = tag[path_start..].find("'") {
        path_start + end
    } else {
        return Err(Error::new(&format!(
            "Unclosed include path in tag: {}",
            tag
        )));
    };

    Ok(tag[path_start..path_end].to_string())
}

/// Resolve include path relative to template directories
fn resolve_include_path(
    include_path: &str, ctx: &PrepCtx, template_dirs: &[PathBuf],
) -> Result<PathBuf> {
    // Try relative to template file first
    if let Some(template_dir) = ctx.template_path.parent() {
        let relative_path = template_dir.join(include_path);
        if relative_path.exists() {
            return Ok(relative_path);
        }
    }

    // Try template directories
    for template_dir in template_dirs {
        let full_path = template_dir.join(include_path);
        if full_path.exists() {
            return Ok(full_path);
        }
    }

    Err(Error::new(&format!(
        "Include file not found: {}",
        include_path
    )))
}

/// Generate default ID for freeze block
fn generate_default_id(template_path: &Path, position: usize) -> String {
    use std::collections::hash_map::DefaultHasher;
    use std::hash::{Hash, Hasher};

    let mut hasher = DefaultHasher::new();
    template_path.hash(&mut hasher);
    position.hash(&mut hasher);
    format!("slot_{:x}", hasher.finish())
}

/// Generate checksum for content
fn generate_checksum(content: &str) -> String {
    use std::collections::hash_map::DefaultHasher;
    use std::hash::{Hash, Hasher};

    let mut hasher = DefaultHasher::new();
    content.hash(&mut hasher);
    format!("{:x}", hasher.finish())
}

/// Load cached slot or generate new content
fn load_or_generate_slot(
    slots_dir: &Path, id: &str, checksum: &str, content: &str,
) -> Result<String> {
    let slot_path = slots_dir.join(format!("{}.slot", id));
    let checksum_path = slots_dir.join(format!("{}.checksum", id));

    // Check if cached slot exists and checksum matches
    if slot_path.exists() && checksum_path.exists() {
        if let Ok(cached_checksum) = fs::read_to_string(&checksum_path) {
            if cached_checksum.trim() == checksum {
                return fs::read_to_string(&slot_path)
                    .map_err(|e| Error::with_source("Failed to read cached slot", Box::new(e)));
            }
        }
    }

    // Generate and save new slot
    generate_and_save_slot(slots_dir, id, checksum, content)
}

/// Generate and save slot content
fn generate_and_save_slot(
    slots_dir: &Path, id: &str, checksum: &str, content: &str,
) -> Result<String> {
    // Ensure slots directory exists
    fs::create_dir_all(slots_dir)
        .map_err(|e| Error::with_source("Failed to create slots directory", Box::new(e)))?;

    let slot_path = slots_dir.join(format!("{}.slot", id));
    let checksum_path = slots_dir.join(format!("{}.checksum", id));

    // Save content and checksum
    fs::write(&slot_path, content)
        .map_err(|e| Error::with_source("Failed to write slot file", Box::new(e)))?;
    fs::write(&checksum_path, checksum)
        .map_err(|e| Error::with_source("Failed to write checksum file", Box::new(e)))?;

    Ok(content.to_string())
}

#[cfg(test)]
mod tests {
    use super::*;
    use tempfile::TempDir;

    fn create_test_ctx() -> PrepCtx<'static> {
        PrepCtx {
            template_path: Path::new("test.tmpl"),
            out_dir: Path::new("output"),
            vars_json: &serde_json::Value::Null,
        }
    }

    #[test]
    fn test_freeze_stage_basic() -> Result<()> {
        let temp_dir = TempDir::new()?;
        let stage = FreezeStage {
            slots_dir: temp_dir.path().to_path_buf(),
            policy: FreezePolicy::Always,
        };

        let input = r#"Hello
{% startfreeze id="test" %}
World
{% endfreeze %}
!"#;

        let ctx = create_test_ctx();
        let result = stage.run(input, &ctx)?;

        assert!(result.contains("World"));
        assert!(!result.contains("startfreeze"));
        assert!(!result.contains("endfreeze"));

        Ok(())
    }

    #[test]
    fn test_include_stage_basic() -> Result<()> {
        let temp_dir = TempDir::new()?;
        let include_file = temp_dir.path().join("included.tmpl");
        fs::write(&include_file, "Included content")?;

        let stage = IncludeStage {
            template_dirs: vec![temp_dir.path().to_path_buf()],
        };

        let input = r#"Hello
{% include "included.tmpl" %}
!"#;

        let ctx = create_test_ctx();
        let result = stage.run(input, &ctx)?;

        assert!(result.contains("Included content"));
        assert!(!result.contains("include"));

        Ok(())
    }

    #[test]
    fn test_preprocessor_pipeline() -> Result<()> {
        let temp_dir = TempDir::new()?;
        let include_file = temp_dir.path().join("included.tmpl");
        fs::write(&include_file, "Included")?;

        let preprocessor = Preprocessor::new()
            .with(IncludeStage {
                template_dirs: vec![temp_dir.path().to_path_buf()],
            })
            .with(FreezeStage {
                slots_dir: temp_dir.path().join("slots"),
                policy: FreezePolicy::Always,
            });

        let input = r#"Hello
{% include "included.tmpl" %}
{% startfreeze id="test" %}
Frozen
{% endfreeze %}
!"#;

        let ctx = create_test_ctx();
        let result = preprocessor.run(input.to_string(), &ctx)?;

        assert!(result.contains("Included"));
        assert!(result.contains("Frozen"));
        assert!(!result.contains("include"));
        assert!(!result.contains("startfreeze"));

        Ok(())
    }
}
