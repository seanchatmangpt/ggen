//! Agent Integration Example: Basic Agent-Tool Workflow
//!
//! This example demonstrates the complete workflow of:
//! 1. Creating a Signature to define tool interface
//! 2. Registering a tool in the registry
//! 3. Creating an agent that uses the registry
//! 4. Executing agent tasks through tools
//!
//! Run with: cargo run --example agent_integration_basic --package ggen-ai
//! Expected output: Successful agent-tool interaction with validation

use serde_json::json;
use std::sync::Arc;
use uuid::Uuid;

// Mock implementations for this example
// In real code, use actual ggen-ai types

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
}

#[derive(Debug, Clone)]
pub struct OutputField {
    name: String,
    type_annotation: String,
    description: String,
}

impl Signature {
    fn new(name: &str, desc: &str) -> Self {
        Self {
            name: name.to_string(),
            description: desc.to_string(),
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

    fn input_names(&self) -> Vec<&str> {
        self.inputs.iter().map(|f| f.name.as_str()).collect()
    }

    fn output_names(&self) -> Vec<&str> {
        self.outputs.iter().map(|f| f.name.as_str()).collect()
    }
}

impl InputField {
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

/// Mock Tool Registry
#[derive(Debug, Clone)]
pub struct ToolRegistry {
    tools: Arc<std::sync::Mutex<std::collections::HashMap<String, Arc<ToolImpl>>>>,
}

#[derive(Debug)]
pub struct ToolImpl {
    pub name: String,
    pub signature: Signature,
}

impl ToolRegistry {
    fn new() -> Self {
        Self {
            tools: Arc::new(std::sync::Mutex::new(std::collections::HashMap::new())),
        }
    }

    fn register(&self, tool: Arc<ToolImpl>) -> Result<(), String> {
        let mut tools = self.tools.lock().unwrap();
        tools.insert(tool.name.clone(), tool);
        Ok(())
    }

    fn get(&self, name: &str) -> Option<Arc<ToolImpl>> {
        let tools = self.tools.lock().unwrap();
        tools.get(name).cloned()
    }

    fn list_all(&self) -> Vec<String> {
        let tools = self.tools.lock().unwrap();
        tools.keys().cloned().collect()
    }

    fn invoke(&self, tool_name: &str, input: &serde_json::Value) -> Result<serde_json::Value, String> {
        let tool = self.get(tool_name).ok_or(format!("Tool not found: {}", tool_name))?;

        // Validate input
        println!("[Registry] Validating input for tool: {}", tool.name);
        for input_field in &tool.signature.inputs {
            if !input.get(&input_field.name).is_some() {
                return Err(format!("Missing required field: {}", input_field.name));
            }
        }

        // Execute tool
        println!("[Registry] Executing tool: {}", tool.name);
        let mut output = serde_json::Map::new();
        output.insert("status".to_string(), json!("success"));
        output.insert("recommendation".to_string(), json!("BUY"));
        output.insert("confidence".to_string(), json!(0.85));

        Ok(serde_json::Value::Object(output))
    }
}

/// Mock Agent
#[derive(Debug)]
pub struct FinancialAgent {
    id: String,
    name: String,
    registry: Arc<ToolRegistry>,
}

impl FinancialAgent {
    fn new(registry: Arc<ToolRegistry>) -> Self {
        Self {
            id: Uuid::new_v4().to_string(),
            name: "FinancialAnalyst".to_string(),
            registry,
        }
    }

    fn initialize(&self) {
        println!("[Agent] Initializing agent: {} ({})", self.name, self.id);
    }

    fn execute_analysis(&self, ticker: &str, period_days: u32) -> Result<serde_json::Value, String> {
        println!("[Agent] Executing analysis for {} over {} days", ticker, period_days);

        // Prepare input
        let input = json!({
            "ticker_symbol": ticker,
            "period_days": period_days,
        });

        // Invoke tool through registry
        self.registry.invoke("FinancialAnalyzer", &input)
    }
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    println!("=== ggen Agent Integration Example ===\n");

    // Step 1: Define the signature
    println!("Step 1: Create Signature for FinancialAnalyzer");
    let signature = Signature::new(
        "FinancialAnalyzer",
        "Analyzes stock data and provides investment recommendations",
    )
    .with_input(
        InputField::new("ticker_symbol", "String")
            .with_description("Stock ticker symbol (e.g., AAPL)"),
    )
    .with_input(
        InputField::new("period_days", "u32")
            .with_description("Historical analysis period in days"),
    )
    .with_output(
        OutputField::new("recommendation", "String")
            .with_description("Investment recommendation (BUY/SELL/HOLD)"),
    )
    .with_output(
        OutputField::new("confidence", "f64")
            .with_description("Confidence level (0.0-1.0)"),
    );

    println!("  - Name: {}", signature.name);
    println!("  - Description: {}", signature.description);
    println!("  - Input fields: {:?}", signature.input_names());
    println!("  - Output fields: {:?}", signature.output_names());
    println!();

    // Step 2: Create and register tool
    println!("Step 2: Register Tool in Registry");
    let registry = Arc::new(ToolRegistry::new());

    let tool = Arc::new(ToolImpl {
        name: "FinancialAnalyzer".to_string(),
        signature: signature.clone(),
    });

    registry.register(tool)?;
    println!("  - Tool registered: FinancialAnalyzer");
    println!("  - Total tools: {}", registry.list_all().len());
    println!();

    // Step 3: Create agent
    println!("Step 3: Create and Initialize Agent");
    let agent = FinancialAgent::new(registry.clone());
    agent.initialize();
    println!();

    // Step 4: Execute agent workflow
    println!("Step 4: Execute Agent Workflow");
    println!("  - Invoking tool: FinancialAnalyzer");
    println!("  - Input: ticker_symbol=AAPL, period_days=90");

    match agent.execute_analysis("AAPL", 90) {
        Ok(result) => {
            println!();
            println!("SUCCESS: Agent execution completed");
            println!("Output:");
            println!("  - Status: {}", result["status"]);
            println!("  - Recommendation: {}", result["recommendation"]);
            println!("  - Confidence: {}", result["confidence"]);
        }
        Err(e) => {
            eprintln!("ERROR: {}", e);
        }
    }

    println!();
    println!("=== Example completed successfully ===");
    Ok(())
}
