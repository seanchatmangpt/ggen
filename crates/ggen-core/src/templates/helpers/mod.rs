//! Template helper functions for various languages and frameworks
//!
//! This module provides helper functions for generating code in different
//! programming languages and frameworks. Each submodule focuses on a specific
//! language or domain.

pub mod erlang;

pub use erlang::{
    format_app_resource, format_record, format_supervisor_child, snake_case_to_module,
};
