//! File-based project conventions for zero-config generation
//!
//! This module provides automatic project structure detection and convention-based
//! code generation. It watches for changes to RDF files and triggers regeneration
//! based on project-specific conventions.

pub mod planner;
pub mod presets;
pub mod resolver;
pub mod watcher;

pub use planner::{GenerationPlan, GenerationPlanner, GenerationTask, TemplateMetadata};
pub use presets::ConventionPreset;
pub use resolver::{ConventionResolver, ProjectConventions};
pub use watcher::ProjectWatcher;
