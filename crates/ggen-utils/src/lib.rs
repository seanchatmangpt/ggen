// backtrace is stable since 1.65.0, no feature flag needed

#![deny(warnings)] // Poka-Yoke: Prevent warnings at compile time - compiler enforces correctness

pub mod alert;
pub mod app_config;
pub mod enhanced_error;
pub mod error;
pub mod logger;
pub mod project_config;
pub mod time;
pub mod types;
pub mod user_level;

pub use project_config::{GgenConfig as UtilsGgenConfig, Project, RdfConfig};
