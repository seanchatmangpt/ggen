//! WebAssembly module for deployment example

use wasm_bindgen::prelude::*;
use serde::{Deserialize, Serialize};

#[wasm_bindgen]
pub struct WasmProcessor {
    multiplier: i32,
}

#[wasm_bindgen]
impl WasmProcessor {
    #[wasm_bindgen(constructor)]
    pub fn new(multiplier: i32) -> Self {
        Self { multiplier }
    }

    pub fn process(&self, value: i32) -> i32 {
        value * self.multiplier
    }

    pub fn greet(&self, name: &str) -> String {
        format!("Hello, {}! Multiplier: {}", name, self.multiplier)
    }
}

#[derive(Serialize, Deserialize)]
pub struct Config {
    pub name: String,
    pub value: i32,
}

#[wasm_bindgen]
pub fn process_config(config_json: &str) -> Result<String, JsValue> {
    let config: Config = serde_json::from_str(config_json)
        .map_err(|e| JsValue::from_str(&e.to_string()))?;

    let result = Config {
        name: config.name.to_uppercase(),
        value: config.value * 2,
    };

    serde_json::to_string(&result)
        .map_err(|e| JsValue::from_str(&e.to_string()))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_processor() {
        let proc = WasmProcessor::new(5);
        assert_eq!(proc.process(10), 50);
    }

    #[test]
    fn test_greet() {
        let proc = WasmProcessor::new(3);
        assert!(proc.greet("World").contains("Hello"));
    }
}
