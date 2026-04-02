use ai_templates::{Template, TemplateRegistry};
use serde_json::json;
use std::collections::HashMap;

fn main() {
    println!("=== AI Templates Demo ===\n");

    // Create registry
    let mut registry = TemplateRegistry::new();

    // Register templates
    registry.register(
        Template::new(
            "greeting",
            "Hello {{name}}, welcome to {{place}}!"
        )
        .with_description("Simple greeting template")
    );

    registry.register(
        Template::new(
            "api_config",
            "API Configuration:\n  Host: {{host}}\n  Port: {{port}}\n  Debug: {{debug}}"
        )
        .with_description("API configuration template")
    );

    registry.register(
        Template::new(
            "rust_function",
            "pub fn {{fn_name}}({{params}}) -> {{return_type}} {\n    {{body}}\n}"
        )
        .with_description("Rust function template")
    );

    println!("1. Simple greeting template");
    let mut vars = HashMap::new();
    vars.insert("name".to_string(), "Alice".to_string());
    vars.insert("place".to_string(), "the template engine".to_string());

    match registry.render("greeting", &vars) {
        Ok(result) => println!("   Result: {}\n", result),
        Err(e) => println!("   Error: {}\n", e),
    }

    println!("2. API configuration with JSON variables");
    let config = json!({
        "host": "api.example.com",
        "port": 8080,
        "debug": false
    });

    match registry.render_json("api_config", &config) {
        Ok(result) => println!("   Result:\n{}\n", result),
        Err(e) => println!("   Error: {}\n", e),
    }

    println!("3. Rust function code generation");
    let mut code_vars = HashMap::new();
    code_vars.insert("fn_name".to_string(), "double".to_string());
    code_vars.insert("params".to_string(), "x: i32".to_string());
    code_vars.insert("return_type".to_string(), "i32".to_string());
    code_vars.insert("body".to_string(), "x * 2".to_string());

    match registry.render("rust_function", &code_vars) {
        Ok(result) => println!("   Result:\n{}\n", result),
        Err(e) => println!("   Error: {}\n", e),
    }

    println!("4. List all registered templates");
    for template in registry.list() {
        println!("   - {}: {}", template.name, template.description);
    }
    println!("\nTotal templates: {}", registry.count());

    println!("\n✅ Demo complete!");
}
