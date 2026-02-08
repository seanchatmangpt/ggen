//! μ₃ (Emit): Tera template rendering, code generation
//!
//! This stage renders Elixir code from extracted data using templates.

use crate::error::Result;
use crate::extract::{ElixirModule, Entity};
use serde::Serialize;
use std::collections::HashMap;
use tera::{Tera, Value};

/// Emission stage (μ₃)
///
/// Renders Elixir code using Tera templates.
pub struct Emitter {
    tera: Tera,
}

#[derive(Debug, Serialize)]
struct TemplateContext {
    modules: Vec<ElixirModule>,
    config: GenerationConfig,
}

#[derive(Debug, Serialize)]
struct GenerationConfig {
    app_name: String,
    module_prefix: String,
    version: String,
}

impl Emitter {
    /// Create a new emitter with built-in templates
    pub fn new() -> Result<Self> {
        let mut tera = Tera::default();

        // Add built-in templates
        tera.add_raw_template(
            "ash_resource.tera",
            include_str!("templates/ash_resource.tera"),
        )
        .map_err(|e| {
            crate::error::CraftplanError::template_rendering("ash_resource.tera", e.to_string())
        })?;

        tera.add_raw_template(
            "context_module.tera",
            include_str!("templates/context_module.tera"),
        )
        .map_err(|e| {
            crate::error::CraftplanError::template_rendering("context_module.tera", e.to_string())
        })?;

        tera.add_raw_template("live_view.tera", include_str!("templates/live_view.tera"))
            .map_err(|e| {
                crate::error::CraftplanError::template_rendering("live_view.tera", e.to_string())
            })?;

        Ok(Self { tera })
    }

    /// Render an Ash resource module
    pub fn render_ash_resource(
        &self, entity: &Entity, config: &GenerationConfig,
    ) -> Result<String> {
        let mut context = HashMap::new();
        context.insert(
            "entity",
            serde_json::to_value(entity).map_err(|e| {
                crate::error::CraftplanError::TemplateRendering {
                    template_name: "ash_resource.tera".to_string(),
                    reason: format!("Failed to serialize entity: {}", e),
                }
            })?,
        );

        context.insert(
            "config",
            serde_json::to_value(config).map_err(|e| {
                crate::error::CraftplanError::TemplateRendering {
                    template_name: "ash_resource.tera".to_string(),
                    reason: format!("Failed to serialize config: {}", e),
                }
            })?,
        );

        self.tera
            .render("ash_resource.tera", &context)
            .map_err(|e| {
                crate::error::CraftplanError::template_rendering("ash_resource.tera", e.to_string())
            })
    }

    /// Render a context module
    pub fn render_context(
        &self, module_name: &str, entities: &[Entity], config: &GenerationConfig,
    ) -> Result<String> {
        let mut context = HashMap::new();
        context.insert("module_name", Value::String(module_name.to_string()));
        context.insert(
            "entities",
            serde_json::to_value(entities).map_err(|e| {
                crate::error::CraftplanError::TemplateRendering {
                    template_name: "context_module.tera".to_string(),
                    reason: format!("Failed to serialize entities: {}", e),
                }
            })?,
        );

        context.insert(
            "config",
            serde_json::to_value(config).map_err(|e| {
                crate::error::CraftplanError::TemplateRendering {
                    template_name: "context_module.tera".to_string(),
                    reason: format!("Failed to serialize config: {}", e),
                }
            })?,
        );

        self.tera
            .render("context_module.tera", &context)
            .map_err(|e| {
                crate::error::CraftplanError::template_rendering(
                    "context_module.tera",
                    e.to_string(),
                )
            })
    }

    /// Render a Phoenix LiveView
    pub fn render_live_view(&self, entity: &Entity, config: &GenerationConfig) -> Result<String> {
        let mut context = HashMap::new();
        context.insert(
            "entity",
            serde_json::to_value(entity).map_err(|e| {
                crate::error::CraftplanError::TemplateRendering {
                    template_name: "live_view.tera".to_string(),
                    reason: format!("Failed to serialize entity: {}", e),
                }
            })?,
        );

        context.insert(
            "config",
            serde_json::to_value(config).map_err(|e| {
                crate::error::CraftplanError::TemplateRendering {
                    template_name: "live_view.tera".to_string(),
                    reason: format!("Failed to serialize config: {}", e),
                }
            })?,
        );

        self.tera.render("live_view.tera", &context).map_err(|e| {
            crate::error::CraftplanError::template_rendering("live_view.tera", e.to_string())
        })
    }
}

impl Default for Emitter {
    fn default() -> Self {
        Self::new().expect("Failed to create emitter")
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_emitter_creation() {
        // Arrange & Act
        let emitter = Emitter::new();

        // Assert
        assert!(emitter.is_ok(), "Emitter should be created successfully");
    }

    #[test]
    fn test_emitter_default() {
        // Arrange & Act
        let emitter = Emitter::default();

        // Assert
        assert!(
            emitter.tera.get_template_names().count() > 0,
            "Should have templates loaded"
        );
    }
}
