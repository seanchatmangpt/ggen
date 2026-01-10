//! MCP Tool Export Example
//!
//! This example demonstrates:
//! 1. Converting Signature to MCP tool definitions
//! 2. Generating JSON Schema from signatures
//! 3. Creating tool catalogs for MCP servers
//! 4. Exporting tools as MCP-compatible JSON
//!
//! Run with: cargo run --example mcp_tool_export --package ggen-ai

use serde_json::{json, Value};
use std::collections::HashMap;

/// Simplified Signature for demonstration
#[derive(Debug, Clone)]
pub struct Signature {
    name: String,
    description: String,
    inputs: Vec<InputField>,
    outputs: Vec<OutputField>,
}

#[derive(Debug, Clone)]
pub struct InputField {
    name: String,
    type_annotation: String,
    description: String,
    required: bool,
    constraints: HashMap<String, String>,
}

#[derive(Debug, Clone)]
pub struct OutputField {
    name: String,
    type_annotation: String,
    description: String,
}

impl Signature {
    fn new(name: &str, description: &str) -> Self {
        Self {
            name: name.to_string(),
            description: description.to_string(),
            inputs: Vec::new(),
            outputs: Vec::new(),
        }
    }

    fn with_input(mut self, field: InputField) -> Self {
        self.inputs.push(field);
        self
    }

    fn with_output(mut self, field: OutputField) -> Self {
        self.outputs.push(field);
        self
    }
}

impl InputField {
    fn new(name: &str, type_annotation: &str) -> Self {
        Self {
            name: name.to_string(),
            type_annotation: type_annotation.to_string(),
            description: String::new(),
            required: false,
            constraints: HashMap::new(),
        }
    }

    fn with_description(mut self, desc: &str) -> Self {
        self.description = desc.to_string();
        self
    }

    fn required(mut self) -> Self {
        self.required = true;
        self
    }

    fn with_constraint(mut self, key: &str, value: &str) -> Self {
        self.constraints.insert(key.to_string(), value.to_string());
        self
    }
}

impl OutputField {
    fn new(name: &str, type_annotation: &str) -> Self {
        Self {
            name: name.to_string(),
            type_annotation: type_annotation.to_string(),
            description: String::new(),
        }
    }

    fn with_description(mut self, desc: &str) -> Self {
        self.description = desc.to_string();
        self
    }
}

/// MCP Tool Exporter
struct MCPExporter;

impl MCPExporter {
    /// Convert signature to JSON Schema
    fn signature_to_json_schema(signature: &Signature) -> Value {
        let mut properties = serde_json::Map::new();
        let mut required = Vec::new();

        for field in &signature.inputs {
            let mut prop = serde_json::json!({
                "type": Self::map_rust_type_to_json(&field.type_annotation),
                "description": &field.description,
            });

            // Add constraints as JSON Schema properties
            if let Some(obj) = prop.as_object_mut() {
                if let Some(pattern) = field.constraints.get("pattern") {
                    obj.insert("pattern".to_string(), json!(pattern));
                }
                if let Some(min_len) = field.constraints.get("min_length") {
                    if let Ok(len) = min_len.parse::<usize>() {
                        obj.insert("minLength".to_string(), json!(len));
                    }
                }
                if let Some(max_len) = field.constraints.get("max_length") {
                    if let Ok(len) = max_len.parse::<usize>() {
                        obj.insert("maxLength".to_string(), json!(len));
                    }
                }
                if let Some(min_val) = field.constraints.get("min_value") {
                    if let Ok(val) = min_val.parse::<f64>() {
                        obj.insert("minimum".to_string(), json!(val));
                    }
                }
                if let Some(max_val) = field.constraints.get("max_value") {
                    if let Ok(val) = max_val.parse::<f64>() {
                        obj.insert("maximum".to_string(), json!(val));
                    }
                }
            }

            properties.insert(field.name.clone(), prop);

            if field.required {
                required.push(field.name.clone());
            }
        }

        json!({
            "type": "object",
            "properties": properties,
            "required": required,
        })
    }

    /// Convert signature to MCP Tool Definition
    fn signature_to_mcp_tool(signature: &Signature) -> Value {
        json!({
            "name": signature.name,
            "description": signature.description,
            "inputSchema": Self::signature_to_json_schema(signature),
        })
    }

    /// Map Rust type to JSON Schema type
    fn map_rust_type_to_json(rust_type: &str) -> &'static str {
        match rust_type {
            "String" => "string",
            "i32" | "i64" | "u32" | "u64" | "integer" => "integer",
            "f32" | "f64" | "float" => "number",
            "bool" => "boolean",
            _ => "string",
        }
    }

    /// Create tool catalog
    fn create_tool_catalog(signatures: &[Signature]) -> Value {
        let mut tools = Vec::new();

        for sig in signatures {
            tools.push(Self::signature_to_mcp_tool(sig));
        }

        json!({
            "tools": tools,
            "version": "1.0.0",
            "generated_at": chrono::Local::now().to_rfc3339(),
            "tool_count": tools.len(),
        })
    }

    /// Export single tool to JSON file format
    fn export_tool_definition(signature: &Signature) -> String {
        let tool = Self::signature_to_mcp_tool(signature);
        serde_json::to_string_pretty(&tool).unwrap()
    }
}

fn main() {
    println!("=== MCP Tool Export Example ===\n");

    // Step 1: Create signatures
    println!("Step 1: Create Tool Signatures");

    let financial_analyzer = Signature::new(
        "FinancialAnalyzer",
        "Analyzes stock data and provides investment recommendations",
    )
    .with_input(
        InputField::new("ticker_symbol", "String")
            .with_description("Stock ticker symbol (e.g., AAPL)")
            .required()
            .with_constraint("pattern", "^[A-Z]{1,5}$")
            .with_constraint("min_length", "1")
            .with_constraint("max_length", "5"),
    )
    .with_input(
        InputField::new("period_days", "i32")
            .with_description("Historical analysis period in days")
            .required()
            .with_constraint("min_value", "1")
            .with_constraint("max_value", "365"),
    )
    .with_output(
        OutputField::new("recommendation", "String")
            .with_description("Investment recommendation (BUY/SELL/HOLD)"),
    );

    println!("  - Created: FinancialAnalyzer");

    let risk_calculator = Signature::new(
        "RiskCalculator",
        "Calculates portfolio risk metrics",
    )
    .with_input(
        InputField::new("portfolio_value", "f32")
            .with_description("Total portfolio value")
            .required()
            .with_constraint("min_value", "0"),
    )
    .with_input(
        InputField::new("volatility", "f32")
            .with_description("Expected volatility (0-1)")
            .required()
            .with_constraint("min_value", "0")
            .with_constraint("max_value", "1"),
    )
    .with_output(
        OutputField::new("var_95", "f32")
            .with_description("Value at Risk (95% confidence)"),
    );

    println!("  - Created: RiskCalculator");

    let weather_tool = Signature::new(
        "WeatherForecast",
        "Gets weather forecast for a location",
    )
    .with_input(
        InputField::new("city", "String")
            .with_description("City name")
            .required(),
    )
    .with_input(
        InputField::new("days", "i32")
            .with_description("Forecast days ahead")
            .with_constraint("min_value", "1")
            .with_constraint("max_value", "14"),
    )
    .with_output(
        OutputField::new("temperature", "f32")
            .with_description("Temperature in Celsius"),
    )
    .with_output(
        OutputField::new("condition", "String")
            .with_description("Weather condition"),
    );

    println!("  - Created: WeatherForecast");
    println!();

    // Step 2: Export individual tools to MCP format
    println!("Step 2: Export Individual Tools to MCP Format");
    println!();

    println!("--- FinancialAnalyzer MCP Definition ---");
    let financial_mcp = MCPExporter::export_tool_definition(&financial_analyzer);
    println!("{}", financial_mcp);
    println!();

    println!("--- RiskCalculator MCP Definition ---");
    let risk_mcp = MCPExporter::export_tool_definition(&risk_calculator);
    println!("{}", risk_mcp);
    println!();

    // Step 3: Generate tool catalog
    println!("Step 3: Generate Tool Catalog for MCP Server");
    let signatures = vec![financial_analyzer, risk_calculator, weather_tool];
    let catalog = MCPExporter::create_tool_catalog(&signatures);

    println!("--- Tool Catalog (JSON) ---");
    println!("{}", serde_json::to_string_pretty(&catalog).unwrap());
    println!();

    // Step 4: Export MCP Request/Response examples
    println!("Step 4: Example MCP Protocol Interactions");
    println!();

    println!("--- MCP Request: List Tools ---");
    let list_request = json!({
        "jsonrpc": "2.0",
        "id": 1,
        "method": "tools/list",
        "params": {}
    });
    println!("{}", serde_json::to_string_pretty(&list_request).unwrap());
    println!();

    println!("--- MCP Response: List Tools ---");
    let list_response = json!({
        "jsonrpc": "2.0",
        "id": 1,
        "result": {
            "tools": [
                {
                    "name": "FinancialAnalyzer",
                    "description": "Analyzes stock data and provides investment recommendations",
                    "inputSchema": {
                        "type": "object",
                        "properties": {
                            "ticker_symbol": {
                                "type": "string",
                                "pattern": "^[A-Z]{1,5}$"
                            },
                            "period_days": {
                                "type": "integer",
                                "minimum": 1,
                                "maximum": 365
                            }
                        },
                        "required": ["ticker_symbol", "period_days"]
                    }
                }
            ]
        }
    });
    println!("{}", serde_json::to_string_pretty(&list_response).unwrap());
    println!();

    println!("--- MCP Request: Call Tool ---");
    let call_request = json!({
        "jsonrpc": "2.0",
        "id": 2,
        "method": "tools/call",
        "params": {
            "name": "FinancialAnalyzer",
            "arguments": {
                "ticker_symbol": "AAPL",
                "period_days": 90
            }
        }
    });
    println!("{}", serde_json::to_string_pretty(&call_request).unwrap());
    println!();

    println!("--- MCP Response: Tool Result ---");
    let call_response = json!({
        "jsonrpc": "2.0",
        "id": 2,
        "result": {
            "recommendation": "BUY",
            "confidence": 0.85,
            "price_target": 175.50
        }
    });
    println!("{}", serde_json::to_string_pretty(&call_response).unwrap());
    println!();

    println!("=== Example completed successfully ===");
}
