// Re-export error module utilities to avoid duplication
// All utility functions are now defined in error.rs
pub use crate::error::{
    error_response, get_bool_param, get_optional_object_param, get_optional_string_param,
    get_optional_u64_param, get_string_param, success_response,
};
