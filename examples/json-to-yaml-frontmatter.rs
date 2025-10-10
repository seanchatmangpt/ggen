//! Example showing how to generate frontmatter as JSON then convert to YAML

use serde_json::{json, Value};
use serde_yaml;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    println!("🚀 Generating frontmatter as JSON then converting to YAML\n");

    // Generate frontmatter as JSON
    let frontmatter_json = json!({
        "to": "src/{{name}}.rs",
        "vars": [
            {"name": "string"},
            {"email": "string"},
            {"role": "string"}
        ],
        "rdf": "@prefix ex: <http://example.org/> .\n@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .\n@prefix owl: <http://www.w3.org/2002/07/owl#> .\n\nex:User a owl:Class ;\n    rdfs:label \"User\" ;\n    rdfs:comment \"A user in the system\" .\n\nex:name a owl:DatatypeProperty ;\n    rdfs:domain ex:User ;\n    rdfs:range xsd:string ;\n    rdfs:label \"name\" .\n\nex:email a owl:DatatypeProperty ;\n    rdfs:domain ex:User ;\n    rdfs:range xsd:string ;\n    rdfs:label \"email\" .\n\nex:role a owl:DatatypeProperty ;\n    rdfs:domain ex:User ;\n    rdfs:range xsd:string ;\n    rdfs:label \"role\" .",
        "sparql": "SELECT ?name ?email ?role WHERE {\n    ?user a ex:User ;\n      ex:name ?name ;\n      ex:email ?email ;\n      ex:role ?role .\n}",
        "determinism": true
    });

    println!("📝 Generated JSON frontmatter:");
    println!("{}", serde_json::to_string_pretty(&frontmatter_json)?);

    // Convert JSON to YAML
    let frontmatter_yaml = serde_yaml::to_string(&frontmatter_json)?;
    
    println!("\n🔄 Converted to YAML:");
    println!("{}", frontmatter_yaml);

    // Create complete template with YAML frontmatter
    let template_content = format!(
        "---\n{}---\n\nuse serde::{{Deserialize, Serialize}};\nuse std::collections::HashMap;\n\n/// User model generated from RDF ontology\n#[derive(Debug, Clone, Serialize, Deserialize)]\npub struct {{{{name | title}}}} {{\n    pub name: String,\n    pub email: String,\n    pub role: String,\n    pub metadata: HashMap<String, String>,\n}}\n\nimpl {{{{name | title}}}} {{\n    /// Create a new user instance\n    pub fn new(name: String, email: String, role: String) -> Self {{\n        Self {{\n            name,\n            email,\n            role,\n            metadata: HashMap::new(),\n        }}\n    }}\n    \n    /// Validate user data\n    pub fn validate(&self) -> Result<(), String> {{\n        if self.name.is_empty() {{\n            return Err(\"Name cannot be empty\".to_string());\n        }}\n        \n        if !self.email.contains('@') {{\n            return Err(\"Invalid email format\".to_string());\n        }}\n        \n        if self.role.is_empty() {{\n            return Err(\"Role cannot be empty\".to_string());\n        }}\n        \n        Ok(())\n    }}\n}}",
        frontmatter_yaml.trim()
    );

    println!("\n📄 Complete template:");
    println!("{}", template_content);

    Ok(())
}

