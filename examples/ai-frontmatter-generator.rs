//! AI-powered frontmatter generator using Ollama qwen3-coder:30b

use ggen_ai::mcp::tools::AiMcpTools;
use serde_json::{json, Value};
use serde_yaml;

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    println!("🚀 AI-powered frontmatter generator with Ollama qwen3-coder:30b\n");

    // Initialize AI tools with Ollama client
    let tools = AiMcpTools::new().with_ollama();
    
    println!("✅ Initialized Ollama client with qwen3-coder:30b model");
    println!("📡 Make sure Ollama is running with qwen3-coder:30b model installed\n");

    // Generate frontmatter using AI
    let template_params = json!({
        "description": "A comprehensive user management system with authentication, roles, and permissions",
        "examples": [
            "Include email validation",
            "Add password hashing with bcrypt",
            "Support role-based access control",
            "Include user profile management",
            "Add audit logging capabilities"
        ],
        "language": "Rust",
        "framework": "Actix Web"
    });
    
    println!("📝 Generating frontmatter with AI...");
    match tools.ai_generate_template(template_params).await {
        Ok(result) => {
            println!("✅ AI template generation successful!");
            
            // Extract the template content
            if let Some(template) = result.get("template") {
                if let Some(body) = template.get("body") {
                    let body_content = body.as_str().unwrap_or("");
                    
                    // Parse the generated content to extract frontmatter
                    if let Some(frontmatter_yaml) = extract_frontmatter(body_content) {
                        println!("\n🔄 Extracted YAML frontmatter:");
                        println!("{}", frontmatter_yaml);
                        
                        // Convert YAML to JSON for demonstration
                        if let Ok(frontmatter_value) = serde_yaml::from_str::<Value>(&frontmatter_yaml) {
                            println!("\n📊 Converted to JSON:");
                            println!("{}", serde_json::to_string_pretty(&frontmatter_value)?);
                            
                            // Generate a complete template
                            let complete_template = format!(
                                "---\n{}---\n\n{}",
                                frontmatter_yaml,
                                generate_template_body(&frontmatter_value)
                            );
                            
                            println!("\n📄 Complete generated template:");
                            println!("{}", complete_template);
                        }
                    } else {
                        println!("❌ Could not extract frontmatter from generated content");
                        println!("Generated content: {}", body_content);
                    }
                }
            }
        }
        Err(e) => {
            println!("❌ AI template generation failed: {}", e);
            println!("💡 Make sure Ollama is running and qwen3-coder:30b model is installed");
            
            // Fallback: Generate frontmatter manually
            println!("\n🔄 Generating fallback frontmatter...");
            let fallback_frontmatter = generate_fallback_frontmatter();
            println!("{}", fallback_frontmatter);
        }
    }
    
    Ok(())
}

/// Extract YAML frontmatter from generated content
fn extract_frontmatter(content: &str) -> Option<String> {
    if content.contains("---\n") {
        let parts: Vec<&str> = content.split("---\n").collect();
        if parts.len() >= 2 {
            return Some(parts[1].trim().to_string());
        }
    }
    None
}

/// Generate template body based on frontmatter
fn generate_template_body(frontmatter: &Value) -> String {
    let to_path = frontmatter.get("to")
        .and_then(|v| v.as_str())
        .unwrap_or("src/{{name}}.rs");
    
    let vars = frontmatter.get("vars")
        .and_then(|v| v.as_array())
        .map(|arr| {
            arr.iter()
                .filter_map(|v| v.as_object())
                .filter_map(|obj| obj.keys().next())
                .collect::<Vec<_>>()
        })
        .unwrap_or_default();
    
    let mut body = String::new();
    body.push_str("use serde::{Deserialize, Serialize};\n");
    body.push_str("use std::collections::HashMap;\n\n");
    
    body.push_str("/// Generated struct from AI template\n");
    body.push_str("#[derive(Debug, Clone, Serialize, Deserialize)]\n");
    body.push_str("pub struct {{name | title}} {\n");
    
    for var in &vars {
        body.push_str(&format!("    pub {}: String,\n", var));
    }
    
    body.push_str("    pub metadata: HashMap<String, String>,\n");
    body.push_str("}\n\n");
    
    body.push_str("impl {{name | title}} {\n");
    body.push_str("    /// Create a new instance\n");
    body.push_str("    pub fn new(");
    
    for (i, var) in vars.iter().enumerate() {
        if i > 0 {
            body.push_str(", ");
        }
        body.push_str(&format!("{}: String", var));
    }
    
    body.push_str(") -> Self {\n");
    body.push_str("        Self {\n");
    
    for var in &vars {
        body.push_str(&format!("            {},\n", var));
    }
    
    body.push_str("            metadata: HashMap::new(),\n");
    body.push_str("        }\n");
    body.push_str("    }\n");
    body.push_str("}\n");
    
    body
}

/// Generate fallback frontmatter when AI is not available
fn generate_fallback_frontmatter() -> String {
    let frontmatter_json = json!({
        "to": "src/{{name}}.rs",
        "vars": [
            {"name": "string"},
            {"email": "string"},
            {"role": "string"},
            {"permissions": "array"}
        ],
        "rdf": "@prefix ex: <http://example.org/> .\n@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .\n@prefix owl: <http://www.w3.org/2002/07/owl#> .\n\nex:User a owl:Class ;\n    rdfs:label \"User\" ;\n    rdfs:comment \"A user in the system\" .\n\nex:name a owl:DatatypeProperty ;\n    rdfs:domain ex:User ;\n    rdfs:range xsd:string .\n\nex:email a owl:DatatypeProperty ;\n    rdfs:domain ex:User ;\n    rdfs:range xsd:string .\n\nex:role a owl:DatatypeProperty ;\n    rdfs:domain ex:User ;\n    rdfs:range xsd:string .",
        "sparql": "SELECT ?name ?email ?role WHERE {\n    ?user a ex:User ;\n      ex:name ?name ;\n      ex:email ?email ;\n      ex:role ?role .\n}",
        "determinism": true
    });
    
    serde_yaml::to_string(&frontmatter_json).unwrap_or_default()
}

