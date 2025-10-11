// Re-export error module utilities to avoid duplication
// All utility functions are now defined in error.rs
pub use crate::error::{
    get_string_param,
    get_optional_string_param,
    get_optional_u64_param,
    get_bool_param,
    get_optional_object_param,
    success_response,
    error_response,
};



