//! μ₃ (Emit): Tera template rendering, code generation
//!
//! This stage renders Elixir code from extracted data using templates.

use crate::error::Result;
use crate::extract::Entity;
use serde::Serialize;
use tera::{Context, Tera, Value};

/// Emission stage (μ₃)
///
/// Renders Elixir code using Tera templates.
pub struct Emitter {
    tera: Tera,
    output_dir: std::path::PathBuf,
}

#[derive(Debug, Serialize)]
struct GenerationConfig {
    app_name: String,
    module_prefix: String,
    version: String,
}

impl Emitter {
    /// Create a new emitter with built-in templates
    pub fn new(output_dir: &std::path::Path) -> Result<Self> {
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

        Ok(Self {
            tera,
            output_dir: output_dir.to_path_buf(),
        })
    }

    /// Render an Ash resource module
    pub fn render_ash_resource(
        &self, entity: &Entity, config: &GenerationConfig,
    ) -> Result<String> {
        let mut context = Context::new();
        let entity_value = serde_json::to_value(entity).map_err(|e| {
            crate::error::CraftplanError::template_rendering(
                "ash_resource.tera",
                format!("Failed to serialize entity: {}", e),
            )
        })?;
        context.insert("entity", &entity_value);

        let config_value = serde_json::to_value(config).map_err(|e| {
            crate::error::CraftplanError::template_rendering(
                "ash_resource.tera",
                format!("Failed to serialize config: {}", e),
            )
        })?;
        context.insert("config", &config_value);

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
        let mut context = Context::new();
        context.insert("module_name", &Value::String(module_name.to_string()));

        let entities_value = serde_json::to_value(entities).map_err(|e| {
            crate::error::CraftplanError::template_rendering(
                "context_module.tera",
                format!("Failed to serialize entities: {}", e),
            )
        })?;
        context.insert("entities", &entities_value);

        let config_value = serde_json::to_value(config).map_err(|e| {
            crate::error::CraftplanError::template_rendering(
                "context_module.tera",
                format!("Failed to serialize config: {}", e),
            )
        })?;
        context.insert("config", &config_value);

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
        let mut context = Context::new();
        let entity_value = serde_json::to_value(entity).map_err(|e| {
            crate::error::CraftplanError::template_rendering(
                "live_view.tera",
                format!("Failed to serialize entity: {}", e),
            )
        })?;
        context.insert("entity", &entity_value);

        let config_value = serde_json::to_value(config).map_err(|e| {
            crate::error::CraftplanError::template_rendering(
                "live_view.tera",
                format!("Failed to serialize config: {}", e),
            )
        })?;
        context.insert("config", &config_value);

        self.tera.render("live_view.tera", &context).map_err(|e| {
            crate::error::CraftplanError::template_rendering("live_view.tera", e.to_string())
        })
    }

    /// Get the Tera instance for template access
    pub fn tera(&self) -> &Tera {
        &self.tera
    }

    /// Get output directory
    pub fn output_dir(&self) -> &std::path::Path {
        &self.output_dir
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_emitter_creation() {
        // Arrange & Act
        let temp_dir = tempfile::tempdir().unwrap();
        let emitter = Emitter::new(temp_dir.path());

        // Assert
        assert!(emitter.is_ok(), "Emitter should be created successfully");
    }

    #[test]
    fn test_emitter_default() {
        // Arrange & Act
        let temp_dir = tempfile::tempdir().unwrap();
        let emitter = Emitter::new(temp_dir.path()).expect("Failed to create emitter");

        // Assert
        assert!(
            emitter.tera().get_template_names().count() > 0,
            "Should have templates loaded"
        );
    }
}
