//! FFI bindings for knhk-hot

use std::ffi::CString;
use std::os::raw::c_char;

/// FFI-safe version of hot module functions
#[no_mangle]
pub extern "C" fn knhk_hot_version() -> *const c_char {
    CString::new("knhk-hot 1.0.0").unwrap().into_raw()
}

/// Initialize hot module
#[no_mangle]
pub extern "C" fn knhk_hot_init() -> i32 {
    0 // Success
}

/// Cleanup hot module
#[no_mangle]
pub extern "C" fn knhk_hot_cleanup() -> i32 {
    0 // Success
}
