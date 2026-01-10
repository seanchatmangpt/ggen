//! Tool Registry Usage Example
//!
//! This example demonstrates:
//! 1. Creating and registering multiple tools
//! 2. Discovering tools by name and domain
//! 3. Invoking tools with validation
//! 4. Error handling during tool invocation
//!
//! Run with: cargo run --example tool_registry_usage --package ggen-ai

use serde_json::json;
use std::collections::HashMap;
use std::sync::Arc;

/// Simplified Tool Registry for demonstration
#[derive(Debug, Clone)]
pub struct ToolRegistry {
    tools: Arc<std::sync::Mutex<HashMap<String, ToolEntry>>>,
    domain_index: Arc<std::sync::Mutex<HashMap<String, Vec<String>>>>,
}

#[derive(Debug, Clone)]
struct ToolEntry {
    name: String,
    domain: String,
    description: String,
    input_schema: serde_json::Value,
}

impl ToolRegistry {
    fn new() -> Self {
        Self {
            tools: Arc::new(std::sync::Mutex::new(HashMap::new())),
            domain_index: Arc::new(std::sync::Mutex::new(HashMap::new())),
        }
    }

    fn register(
        &self,
        name: &str,
        domain: &str,
        description: &str,
        input_schema: serde_json::Value,
    ) -> Result<(), String> {
        let entry = ToolEntry {
            name: name.to_string(),
            domain: domain.to_string(),
            description: description.to_string(),
            input_schema,
        };

        {
            let mut tools = self.tools.lock().unwrap();
            if tools.contains_key(name) {
                return Err(format!("Tool already registered: {}", name));
            }
            tools.insert(name.to_string(), entry);
        }

        // Update domain index
        {
            let mut index = self.domain_index.lock().unwrap();
            index
                .entry(domain.to_string())
                .or_insert_with(Vec::new)
                .push(name.to_string());
        }

        Ok(())
    }

    fn get(&self, name: &str) -> Option<ToolEntry> {
        let tools = self.tools.lock().unwrap();
        tools.get(name).cloned()
    }

    fn list_all(&self) -> Vec<String> {
        let tools = self.tools.lock().unwrap();
        tools.keys().cloned().collect()
    }

    fn find_by_domain(&self, domain: &str) -> Vec<String> {
        let index = self.domain_index.lock().unwrap();
        index.get(domain).cloned().unwrap_or_default()
    }

    fn count(&self) -> usize {
        let tools = self.tools.lock().unwrap();
        tools.len()
    }

    fn unregister(&self, name: &str) -> bool {
        let mut tools = self.tools.lock().unwrap();
        tools.remove(name).is_some()
    }
}

fn main() {
    println!("=== Tool Registry Usage Example ===\n");

    // Create registry
    println!("Step 1: Create Tool Registry");
    let registry = ToolRegistry::new();
    println!("  - Registry created\n");

    // Register tools
    println!("Step 2: Register Multiple Tools");

    let financial_schema = json!({
        "type": "object",
        "properties": {
            "ticker_symbol": {"type": "string"},
            "period_days": {"type": "integer"}
        },
        "required": ["ticker_symbol"]
    });

    registry
        .register(
            "FinancialAnalyzer",
            "finance",
            "Analyzes stock data",
            financial_schema,
        )
        .expect("Register FinancialAnalyzer");
    println!("  - Registered: FinancialAnalyzer (finance domain)");

    let risk_schema = json!({
        "type": "object",
        "properties": {
            "portfolio": {"type": "array"},
            "time_horizon": {"type": "string"}
        }
    });

    registry
        .register(
            "RiskCalculator",
            "finance",
            "Calculates portfolio risk",
            risk_schema,
        )
        .expect("Register RiskCalculator");
    println!("  - Registered: RiskCalculator (finance domain)");

    let weather_schema = json!({
        "type": "object",
        "properties": {
            "city": {"type": "string"}
        },
        "required": ["city"]
    });

    registry
        .register(
            "WeatherForecast",
            "weather",
            "Gets weather forecast",
            weather_schema,
        )
        .expect("Register WeatherForecast");
    println!("  - Registered: WeatherForecast (weather domain)");
    println!();

    // List all tools
    println!("Step 3: List All Tools");
    let all_tools = registry.list_all();
    println!("  - Total tools: {}", registry.count());
    for tool_name in &all_tools {
        if let Some(tool) = registry.get(tool_name) {
            println!("    * {}: {} ({})", tool.name, tool.description, tool.domain);
        }
    }
    println!();

    // Find by domain
    println!("Step 4: Discover Tools by Domain");

    let finance_tools = registry.find_by_domain("finance");
    println!("  - Finance domain tools ({}): {:?}", finance_tools.len(), finance_tools);

    let weather_tools = registry.find_by_domain("weather");
    println!("  - Weather domain tools ({}): {:?}", weather_tools.len(), weather_tools);

    let other_tools = registry.find_by_domain("other");
    println!("  - Other domain tools ({}): {:?}", other_tools.len(), other_tools);
    println!();

    // Get specific tool
    println!("Step 5: Get Tool Details");
    if let Some(tool) = registry.get("FinancialAnalyzer") {
        println!("  - Tool: {}", tool.name);
        println!("    Description: {}", tool.description);
        println!("    Domain: {}", tool.domain);
        println!("    Input Schema: {}", tool.input_schema);
    }
    println!();

    // Simulate tool invocation
    println!("Step 6: Simulate Tool Invocation");

    let tool_input = json!({
        "ticker_symbol": "AAPL",
        "period_days": 90
    });

    if let Some(tool) = registry.get("FinancialAnalyzer") {
        println!("  - Invoking: {}", tool.name);
        println!("  - Input: {}", tool_input);

        // Validate input against schema
        let has_ticker = tool_input.get("ticker_symbol").is_some();
        if has_ticker {
            println!("  - Validation: PASSED");
            println!("  - Output: {{\"recommendation\": \"BUY\", \"confidence\": 0.85}}");
        } else {
            println!("  - Validation: FAILED - Missing required field: ticker_symbol");
        }
    }
    println!();

    // Error handling: Tool not found
    println!("Step 7: Error Handling - Tool Not Found");
    match registry.get("NonExistentTool") {
        Some(tool) => println!("  - Found: {}", tool.name),
        None => println!("  - Tool not found: NonExistentTool"),
    }
    println!();

    // Error handling: Duplicate registration
    println!("Step 8: Error Handling - Duplicate Registration");
    match registry.register(
        "FinancialAnalyzer",
        "finance",
        "Duplicate",
        json!({}),
    ) {
        Ok(_) => println!("  - Registration succeeded"),
        Err(e) => println!("  - Registration failed: {}", e),
    }
    println!();

    // Unregister tool
    println!("Step 9: Unregister Tool");
    if registry.unregister("WeatherForecast") {
        println!("  - Unregistered: WeatherForecast");
    }
    println!("  - Remaining tools: {}", registry.count());
    println!();

    println!("=== Example completed successfully ===");
}
