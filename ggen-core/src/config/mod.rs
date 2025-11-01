//! Configuration management for ggen
//!
//! This module provides configuration structures and utilities
//! for various ggen subsystems.

pub mod template_config;

pub use template_config::{
    GenerationOptions, MarketplaceSettings, TemplateConfig,
};
