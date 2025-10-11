//! Test fixtures and mock data for ggen-mcp testing
//!
//! This module provides pre-built test data and fixtures for consistent testing

use serde_json::{json, Value};

/// Sample Rust template fixture
pub fn rust_api_template() -> Value {
    json!({
        "name": "rust-api",
        "version": "1.0.0",
        "description": "REST API template in Rust",
        "files": [
            {
                "path": "src/main.rs",
                "content": "fn main() {\n    println!(\"Hello, API!\");\n}"
            },
            {
                "path": "Cargo.toml",
                "content": "[package]\nname = \"rust-api\"\nversion = \"0.1.0\"\n"
            }
        ],
        "variables": {
            "project_name": "rust-api",
            "author": "Test Author"
        }
    })
}

/// Sample TypeScript template fixture
pub fn typescript_app_template() -> Value {
    json!({
        "name": "typescript-app",
        "version": "1.0.0",
        "description": "TypeScript application template",
        "files": [
            {
                "path": "src/index.ts",
                "content": "console.log('Hello, TypeScript!');"
            },
            {
                "path": "package.json",
                "content": "{\n  \"name\": \"typescript-app\",\n  \"version\": \"1.0.0\"\n}"
            },
            {
                "path": "tsconfig.json",
                "content": "{\n  \"compilerOptions\": {\n    \"target\": \"ES2020\"\n  }\n}"
            }
        ],
        "variables": {}
    })
}

/// Sample Python template fixture
pub fn python_service_template() -> Value {
    json!({
        "name": "python-service",
        "version": "1.0.0",
        "description": "Python microservice template",
        "files": [
            {
                "path": "main.py",
                "content": "def main():\n    print('Hello, Python!')\n\nif __name__ == '__main__':\n    main()"
            },
            {
                "path": "requirements.txt",
                "content": "fastapi==0.104.0\nuvicorn==0.24.0"
            }
        ],
        "variables": {
            "service_name": "test-service"
        }
    })
}

/// Complex nested template fixture
pub fn complex_nested_template() -> Value {
    json!({
        "name": "full-stack-app",
        "version": "2.0.0",
        "description": "Full-stack application with multiple modules",
        "files": [
            {
                "path": "backend/src/main.rs",
                "content": "// Backend entry point"
            },
            {
                "path": "backend/Cargo.toml",
                "content": "[package]\nname = \"backend\""
            },
            {
                "path": "frontend/src/App.tsx",
                "content": "// React component"
            },
            {
                "path": "frontend/package.json",
                "content": "{\"name\": \"frontend\"}"
            },
            {
                "path": "docker-compose.yml",
                "content": "version: '3.8'"
            }
        ],
        "variables": {
            "app_name": "full-stack",
            "backend_port": "8080",
            "frontend_port": "3000"
        }
    })
}

/// Sample tool call fixtures
pub mod tool_calls {
    use serde_json::{json, Value};

    pub fn list_tools_request() -> Value {
        json!({
            "jsonrpc": "2.0",
            "id": 1,
            "method": "tools/list"
        })
    }

    pub fn generate_template_request(name: &str) -> Value {
        json!({
            "jsonrpc": "2.0",
            "id": 2,
            "method": "tools/call",
            "params": {
                "name": "generate_template",
                "arguments": {
                    "name": name,
                    "output_dir": "/tmp/test"
                }
            }
        })
    }

    pub fn validate_template_request(path: &str) -> Value {
        json!({
            "jsonrpc": "2.0",
            "id": 3,
            "method": "tools/call",
            "params": {
                "name": "validate_template",
                "arguments": {
                    "path": path
                }
            }
        })
    }
}

/// Sample resource fixtures
pub mod resources {
    use serde_json::{json, Value};

    pub fn template_resource_uri(name: &str) -> String {
        format!("template://{}", name)
    }

    pub fn config_resource() -> Value {
        json!({
            "uri": "config://ggen",
            "name": "GGen Configuration",
            "mimeType": "application/json",
            "contents": {
                "template_dir": "/templates",
                "cache_enabled": true
            }
        })
    }
}

/// Test data generators
pub mod generators {
    use rand::distributions::Alphanumeric;
    use rand::{thread_rng, Rng};

    /// Generate random template name
    pub fn random_template_name() -> String {
        let suffix: String = thread_rng()
            .sample_iter(&Alphanumeric)
            .take(8)
            .map(char::from)
            .collect();
        format!("test-template-{}", suffix.to_lowercase())
    }

    /// Generate random file content
    pub fn random_file_content(lines: usize) -> String {
        (0..lines)
            .map(|i| format!("// Line {}: {}", i, random_string(20)))
            .collect::<Vec<_>>()
            .join("\n")
    }

    fn random_string(len: usize) -> String {
        thread_rng()
            .sample_iter(&Alphanumeric)
            .take(len)
            .map(char::from)
            .collect()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_rust_api_template() {
        let template = rust_api_template();
        assert_eq!(template["name"], "rust-api");
        assert!(template["files"].as_array().unwrap().len() >= 2);
    }

    #[test]
    fn test_complex_template_structure() {
        let template = complex_nested_template();
        assert_eq!(template["files"].as_array().unwrap().len(), 5);
    }

    #[test]
    fn test_tool_call_fixtures() {
        let request = tool_calls::generate_template_request("test");
        assert_eq!(request["method"], "tools/call");
    }

    #[test]
    fn test_random_generators() {
        let name1 = generators::random_template_name();
        let name2 = generators::random_template_name();
        assert_ne!(name1, name2);
        assert!(name1.starts_with("test-template-"));
    }
}
