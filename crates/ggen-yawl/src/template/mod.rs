//! Template rendering module for YAWL workflows.

pub mod context;
pub mod renderer;

pub use context::{TemplateContext, TaskContext, FlowContext, ConditionContext, VariableContext, ContextBuilder};
pub use renderer::TemplateRenderer;
