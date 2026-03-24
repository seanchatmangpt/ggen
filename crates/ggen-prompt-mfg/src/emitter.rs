//! Deterministic Tera-based prompt emission
//!
//! Transforms prompt IR into final text via templates.
//! Guarantees: same IR → same output (deterministic).

use crate::ir::{BlockType, PromptIR, SectionType};
use crate::{PromptError, Result};
use tera::{Context, Tera};

/// Prompt emitter using Tera templates
///
/// # Determinism guarantees
///
/// - Templates are pre-compiled and immutable
/// - Variable ordering is stable (BTreeMap)
/// - No random or time-based generation
/// - Whitespace normalization is consistent
pub struct PromptEmitter {
    tera: Tera,
}

impl PromptEmitter {
    /// Create a new prompt emitter
    ///
    /// # Errors
    ///
    /// Returns error if template compilation fails
    pub fn new() -> Result<Self> {
        let mut tera = Tera::default();

        // Register deterministic templates
        tera.add_raw_template("prompt", include_str!("../templates/prompt.tera"))
            .map_err(|e| PromptError::Template(e.to_string()))?;

        tera.add_raw_template("section", include_str!("../templates/section.tera"))
            .map_err(|e| PromptError::Template(e.to_string()))?;

        Ok(Self { tera })
    }

    /// Emit deterministic prompt from IR
    ///
    /// # Errors
    ///
    /// Returns error if template rendering fails
    pub fn emit(&self, ir: &PromptIR) -> Result<String> {
        let context = self.build_context(ir)?;

        let rendered = self
            .tera
            .render("prompt", &context)
            .map_err(|e| PromptError::Template(e.to_string()))?;

        // Normalize whitespace for determinism
        Ok(normalize_whitespace(&rendered))
    }

    /// Build Tera context from IR
    fn build_context(&self, ir: &PromptIR) -> Result<Context> {
        let mut context = Context::new();

        // Build sections with proper structure for Tera
        #[derive(serde::Serialize)]
        struct SectionData {
            key: String,
            #[serde(rename = "type")]
            section_type: String,
            priority: i32,
            blocks: Vec<BlockData>,
        }

        #[derive(serde::Serialize)]
        struct BlockData {
            #[serde(rename = "type")]
            block_type: String,
            content: String,
        }

        let sections: Vec<SectionData> = ir
            .sections_ordered()
            .iter()
            .map(|(key, section)| {
                let blocks: Vec<BlockData> = section
                    .blocks
                    .iter()
                    .map(|block| BlockData {
                        block_type: self.block_type_string(&block.block_type),
                        content: block.content.clone(),
                    })
                    .collect();

                SectionData {
                    key: (*key).clone(),
                    section_type: self.section_type_string(&section.section_type),
                    priority: section.priority,
                    blocks,
                }
            })
            .collect();

        context.insert("sections", &sections);
        context.insert("metadata", &ir.metadata);
        context.insert("variables", &ir.variables);

        Ok(context)
    }

    fn section_type_string(&self, section_type: &SectionType) -> String {
        match section_type {
            SectionType::System => "system".to_string(),
            SectionType::User => "user".to_string(),
            SectionType::Assistant => "assistant".to_string(),
            SectionType::Custom(name) => name.clone(),
        }
    }

    fn block_type_string(&self, block_type: &BlockType) -> String {
        match block_type {
            BlockType::Text => "text".to_string(),
            BlockType::Code { language } => format!("code:{language}"),
            BlockType::Instruction => "instruction".to_string(),
            BlockType::Example => "example".to_string(),
            BlockType::Constraint => "constraint".to_string(),
        }
    }
}

impl Default for PromptEmitter {
    fn default() -> Self {
        Self::new().expect("Failed to initialize PromptEmitter")
    }
}

/// Normalize whitespace for deterministic output
///
/// - Removes trailing whitespace
/// - Normalizes line endings to \n
/// - Removes multiple consecutive blank lines
fn normalize_whitespace(text: &str) -> String {
    text.lines()
        .map(|line| line.trim_end())
        .collect::<Vec<_>>()
        .join("\n")
        .split("\n\n\n")
        .collect::<Vec<_>>()
        .join("\n\n")
        .trim()
        .to_string()
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ir::{ContentBlock, PromptMetadata, Section};
    use std::collections::BTreeMap;

    #[test]
    fn test_emitter_creation() {
        let emitter = PromptEmitter::new();
        assert!(emitter.is_ok());
    }

    #[test]
    fn test_whitespace_normalization() {
        let input = "line1  \nline2\n\n\nline3\n  ";
        let normalized = normalize_whitespace(input);
        assert_eq!(normalized, "line1\nline2\n\nline3");
    }

    #[test]
    fn test_emit_empty_ir() {
        let emitter = PromptEmitter::new().unwrap();
        let ir = PromptIR {
            sections: BTreeMap::new(),
            metadata: PromptMetadata {
                id: "test".to_string(),
                version: "1.0.0".to_string(),
                schema_version: "1.0.0".to_string(),
                source_ontology: "test://ontology".to_string(),
                construct_query: "".to_string(),
            },
            variables: BTreeMap::new(),
        };

        let result = emitter.emit(&ir);
        assert!(result.is_ok());
    }

    #[test]
    fn test_deterministic_emission() {
        let emitter = PromptEmitter::new().unwrap();

        let mut ir = PromptIR {
            sections: BTreeMap::new(),
            metadata: PromptMetadata {
                id: "test".to_string(),
                version: "1.0.0".to_string(),
                schema_version: "1.0.0".to_string(),
                source_ontology: "test://ontology".to_string(),
                construct_query: "".to_string(),
            },
            variables: BTreeMap::new(),
        };

        ir.add_section(
            "sec1".to_string(),
            Section {
                section_type: SectionType::System,
                blocks: vec![ContentBlock {
                    block_type: BlockType::Text,
                    content: "Test content".to_string(),
                    metadata: BTreeMap::new(),
                }],
                priority: 0,
            },
        );

        let output1 = emitter.emit(&ir).unwrap();
        let output2 = emitter.emit(&ir).unwrap();

        assert_eq!(output1, output2, "Emission must be deterministic");
    }
}
