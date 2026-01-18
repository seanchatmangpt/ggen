//! Utility functions for WASM module

use wasm_bindgen::prelude::*;

/// Set up better panic messages for debugging in browsers
pub fn set_panic_hook() {
    #[cfg(feature = "console_error_panic_hook")]
    console_error_panic_hook::set_once();
}

/// Convert bytes to hex string
#[allow(dead_code)]
pub fn bytes_to_hex(bytes: &[u8]) -> String {
    bytes.iter()
        .map(|b| format!("{:02x}", b))
        .collect()
}

/// Convert hex string to bytes
#[allow(dead_code)]
pub fn hex_to_bytes(hex: &str) -> Result<Vec<u8>, JsValue> {
    if hex.len() % 2 != 0 {
        return Err(JsValue::from_str("Hex string must have even length"));
    }

    (0..hex.len())
        .step_by(2)
        .map(|i| {
            u8::from_str_radix(&hex[i..i + 2], 16)
                .map_err(|e| JsValue::from_str(&format!("Invalid hex: {}", e)))
        })
        .collect()
}

/// Timing-safe comparison
#[allow(dead_code)]
pub fn constant_time_compare(a: &[u8], b: &[u8]) -> bool {
    if a.len() != b.len() {
        return false;
    }

    let mut result = 0u8;
    for (x, y) in a.iter().zip(b.iter()) {
        result |= x ^ y;
    }

    result == 0
}
