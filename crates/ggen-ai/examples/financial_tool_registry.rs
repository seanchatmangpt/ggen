//! Example: Register and use financial domain tools
//!
//! Demonstrates how to:
//! 1. Create and validate tools with DSPy Signatures
//! 2. Register tools in the global registry
//! 3. Discover tools by domain/tag
//! 4. Export tools for agent use (OpenAPI, MCP)
//! 5. Validate inputs against tool schemas

use ggen_ai::{
    dspy::{field::InputField, field::OutputField, Signature},
    tool::{AuthScope, Tool, ToolSlo, ToolTag},
    tool_registry::REGISTRY,
};

fn main() -> Result<(), Box<dyn std::error::Error>> {
    println!("Financial Tool Registry Example");
    println!("================================\n");

    // Create financial domain tools
    let tools = create_financial_tools();

    // Register tools
    println!("Registering {} tools...", tools.len());
    {
        let mut registry = REGISTRY.write()?;
        for (id, tool) in tools {
            match registry.register(&id, tool) {
                Ok(()) => println!("  ✓ Registered '{}'", id),
                Err(e) => eprintln!("  ✗ Failed to register '{}': {}", id, e),
            }
        }
    }

    // List all tools
    println!("\nAll registered tools:");
    {
        let registry = REGISTRY.read()?;
        for (id, tool) in registry.list() {
            println!("  - {} (v{}): {}", id, tool.version, tool.description);
        }
    }

    // Find financial tools
    println!("\nFinancial domain tools:");
    {
        let registry = REGISTRY.read()?;
        let financial_tools = registry.list_by_tag(&ToolTag::Financial);
        for tool in financial_tools {
            println!("  - {} ({})", tool.id, tool.name);
        }
    }

    // Validate inputs
    println!("\nValidating inputs:");
    {
        let registry = REGISTRY.read()?;

        let valid_input = serde_json::json!({
            "transaction_amount": "1500.50",
            "currency": "USD"
        });

        match registry.validate_input("validate_transaction", &valid_input) {
            Ok(()) => println!("  ✓ Valid input for validate_transaction"),
            Err(e) => println!("  ✗ Invalid input: {}", e),
        }

        let invalid_input = serde_json::json!({ "amount": "1500" });
        match registry.validate_input("validate_transaction", &invalid_input) {
            Ok(()) => println!("  ✓ Valid input for validate_transaction"),
            Err(e) => println!("  ✗ Invalid input (expected): {}", e),
        }
    }

    // Export as OpenAPI
    println!("\nExporting as OpenAPI:");
    {
        let registry = REGISTRY.read()?;
        let openapi = registry.export_openapi();
        println!("  {} OpenAPI tools", openapi.as_array().unwrap().len());
        println!("  JSON:\n{}", serde_json::to_string_pretty(&openapi)?);
    }

    // Export as MCP
    println!("\nExporting as MCP tools:");
    {
        let registry = REGISTRY.read()?;
        let mcp = registry.export_mcp_tools();
        println!("  {} MCP tools", mcp.as_array().unwrap().len());
    }

    // Show registry statistics
    println!("\nRegistry statistics:");
    {
        let registry = REGISTRY.read()?;
        println!("  Total tools: {}", registry.count());
        println!(
            "  Tool IDs: {:?}",
            registry.tool_ids().iter().take(3).collect::<Vec<_>>()
        );
    }

    Ok(())
}

/// Create example financial domain tools
fn create_financial_tools() -> Vec<(String, Tool)> {
    vec![
        create_transaction_validator(),
        create_risk_analyzer(),
        create_portfolio_optimizer(),
        create_compliance_checker(),
    ]
}

/// Transaction validation tool
fn create_transaction_validator() -> (String, Tool) {
    let sig = Signature::new("ValidateTransaction", "Validates financial transactions")
        .with_input(InputField::new(
            "transaction_amount",
            "Transaction amount in numeric format",
            "String",
        ))
        .with_input(InputField::new(
            "currency",
            "Currency code (e.g., USD, EUR)",
            "String",
        ))
        .with_output(OutputField::new(
            "is_valid",
            "Whether transaction is valid",
            "bool",
        ))
        .with_output(OutputField::new(
            "validation_reason",
            "Reason for validation result",
            "String",
        ));

    let tool = Tool::new(
        "validate_transaction",
        "Transaction Validator",
        "1.0.0",
        "Validates financial transactions for compliance and correctness",
        sig,
    )
    .with_tag(ToolTag::Validation)
    .with_tag(ToolTag::Financial)
    .with_author("Finance Team")
    .with_documentation_url("https://docs.example.com/tools/validate-transaction")
    .with_slo(ToolSlo {
        timeout_ms: 5_000,
        max_retries: 3,
        cacheable: true,
    })
    .with_auth_scope(AuthScope::Authenticated)
    .with_metadata("domain", "finance")
    .with_metadata("risk_level", "high");

    ("validate_transaction".to_string(), tool)
}

/// Risk analysis tool
fn create_risk_analyzer() -> (String, Tool) {
    let sig = Signature::new("AnalyzeRisk", "Analyzes financial risk metrics")
        .with_input(InputField::new(
            "portfolio_value",
            "Total portfolio value",
            "f64",
        ))
        .with_input(InputField::new(
            "asset_classes",
            "Types of assets in portfolio",
            "Vec<String>",
        ))
        .with_output(OutputField::new(
            "risk_score",
            "Calculated risk score 0-100",
            "i32",
        ))
        .with_output(OutputField::new(
            "recommendations",
            "Risk mitigation recommendations",
            "Vec<String>",
        ));

    let tool = Tool::new(
        "analyze_risk",
        "Risk Analyzer",
        "2.1.0",
        "Analyzes and assesses financial portfolio risk",
        sig,
    )
    .with_tag(ToolTag::Analysis)
    .with_tag(ToolTag::Financial)
    .with_author("Risk Management Team")
    .with_slo(ToolSlo {
        timeout_ms: 10_000,
        max_retries: 2,
        cacheable: false,
    })
    .with_auth_scope(AuthScope::Authenticated)
    .with_metadata("domain", "finance")
    .with_metadata("complexity", "high");

    ("analyze_risk".to_string(), tool)
}

/// Portfolio optimization tool
fn create_portfolio_optimizer() -> (String, Tool) {
    let sig = Signature::new("OptimizePortfolio", "Optimizes investment portfolios")
        .with_input(InputField::new(
            "current_allocation",
            "Current asset allocation",
            "HashMap<String, f64>",
        ))
        .with_input(InputField::new(
            "risk_tolerance",
            "Investor's risk tolerance level",
            "String",
        ))
        .with_output(OutputField::new(
            "optimal_allocation",
            "Recommended asset allocation",
            "HashMap<String, f64>",
        ))
        .with_output(OutputField::new(
            "expected_return",
            "Expected portfolio return percentage",
            "f64",
        ));

    let tool = Tool::new(
        "optimize_portfolio",
        "Portfolio Optimizer",
        "1.5.0",
        "Optimizes portfolio allocation based on risk tolerance and constraints",
        sig,
    )
    .with_tag(ToolTag::Generation)
    .with_tag(ToolTag::Financial)
    .with_author("Portfolio Team")
    .with_slo(ToolSlo {
        timeout_ms: 15_000,
        max_retries: 1,
        cacheable: false,
    })
    .with_auth_scope(AuthScope::Admin)
    .with_metadata("domain", "finance")
    .with_metadata("model", "mean-variance-optimization");

    ("optimize_portfolio".to_string(), tool)
}

/// Compliance checking tool
fn create_compliance_checker() -> (String, Tool) {
    let sig = Signature::new("CheckCompliance", "Checks regulatory compliance")
        .with_input(InputField::new(
            "transaction_details",
            "Transaction details to check",
            "String",
        ))
        .with_input(InputField::new(
            "regulations",
            "Applicable regulations",
            "Vec<String>",
        ))
        .with_output(OutputField::new(
            "is_compliant",
            "Whether transaction is compliant",
            "bool",
        ))
        .with_output(OutputField::new(
            "violations",
            "List of regulatory violations if any",
            "Vec<String>",
        ));

    let tool = Tool::new(
        "check_compliance",
        "Compliance Checker",
        "1.2.0",
        "Checks financial transactions for regulatory compliance",
        sig,
    )
    .with_tag(ToolTag::Validation)
    .with_tag(ToolTag::Financial)
    .with_tag(ToolTag::Banking)
    .with_author("Compliance Team")
    .with_slo(ToolSlo {
        timeout_ms: 8_000,
        max_retries: 2,
        cacheable: true,
    })
    .with_auth_scope(AuthScope::Admin)
    .with_metadata("domain", "finance")
    .with_metadata("regulations", "KYC,AML,GDPR");

    ("check_compliance".to_string(), tool)
}
