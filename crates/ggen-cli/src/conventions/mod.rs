//! File-based project conventions for zero-config generation
//!
//! This module provides automatic project structure detection and convention-based
//! code generation. It watches for changes to RDF files and triggers regeneration
//! based on project-specific conventions.

pub mod resolver;
pub mod planner;
pub mod watcher;
pub mod presets;

pub use resolver::{ConventionResolver, ProjectConventions};
pub use planner::{GenerationPlanner, GenerationPlan, GenerationTask, TemplateMetadata};
pub use watcher::ProjectWatcher;
pub use presets::ConventionPreset;
