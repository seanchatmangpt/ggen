//! Template creation domain logic

use ggen_utils::error::Result;

/// Generate template content based on template type
pub fn generate_template_content(name: &str, template_type: &str) -> Result<String> {
    let timestamp = chrono::Utc::now().to_rfc3339();

    let content = match template_type {
        "rust" => format!(
            "---\nto: src/{{{{ name }}}}.rs\nvars:\n  name: \"{name}\"\n  author: \"{{{{ author }}}}\"\nrdf:\n  sources: []\nsparql:\n  queries: {{}}\ndeterminism:\n  seed: \"{timestamp}\"\n---\n\n// Generated Rust module: {name}\n// Author: {{{{ author }}}}\n// Generated at: {timestamp}\n\npub struct {{{{ name | pascal_case }}}} {{\n    pub id: String,\n    pub name: String,\n}}\n\nimpl {{{{ name | pascal_case }}}} {{\n    pub fn new(name: String) -> Self {{\n        Self {{\n            id: uuid::Uuid::new_v4().to_string(),\n            name,\n        }}\n    }}\n}}\n"
        ),
        "python" => format!(
            "---\nto: src/{{{{ name }}}}.py\nvars:\n  name: \"{name}\"\n  author: \"{{{{ author }}}}\"\nrdf:\n  sources: []\nsparql:\n  queries: {{}}\ndeterminism:\n  seed: \"{timestamp}\"\n---\n\n# Generated Python module: {name}\n# Author: {{{{ author }}}}\n# Generated at: {timestamp}\n\nfrom dataclasses import dataclass\nfrom typing import Optional\n\n@dataclass\nclass {{{{ name | pascal_case }}}}:\n    id: str\n    name: str\n\n    def __init__(self, name: str):\n        self.id = str(uuid.uuid4())\n        self.name = name\n"
        ),
        "typescript" => format!(
            "---\nto: src/{{{{ name }}}}.ts\nvars:\n  name: \"{name}\"\n  author: \"{{{{ author }}}}\"\nrdf:\n  sources: []\nsparql:\n  queries: {{}}\ndeterminism:\n  seed: \"{timestamp}\"\n---\n\n// Generated TypeScript module: {name}\n// Author: {{{{ author }}}}\n// Generated at: {timestamp}\n\nexport interface {{{{ name | pascal_case }}}} {{\n    id: string;\n    name: string;\n}}\n\nexport class {{{{ name | pascal_case }}}}Class implements {{{{ name | pascal_case }}}} {{\n    id: string;\n    name: string;\n\n    constructor(name: string) {{\n        this.id = crypto.randomUUID();\n        this.name = name;\n    }}\n}}\n"
        ),
        "generic" => format!(
            "---\nto: output/{{{{ name }}}}.txt\nvars:\n  name: \"{name}\"\n  author: \"{{{{ author }}}}\"\nrdf:\n  sources: []\nsparql:\n  queries: {{}}\ndeterminism:\n  seed: \"{timestamp}\"\n---\n\nGenerated file: {name}\nAuthor: {{{{ author }}}}\nGenerated at: {timestamp}\n\nThis is a generic template. Customize the content below:\n\nHello, {{{{ name }}}}!\n\nYour template content goes here.\n"
        ),
        _ => format!(
            "---\nto: output/{{{{ name }}}}.txt\nvars:\n  name: \"{name}\"\n  author: \"{{{{ author }}}}\"\nrdf:\n  sources: []\nsparql:\n  queries: {{}}\ndeterminism:\n  seed: \"{timestamp}\"\n---\n\nGenerated file: {name}\nAuthor: {{{{ author }}}}\nGenerated at: {timestamp}\n\nThis is a generic template. Customize the content below:\n\nHello, {{{{ name }}}}!\n\nYour template content goes here.\n"
        ),
    };

    Ok(content)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_generate_rust_template() {
        let content = generate_template_content("my_module", "rust").unwrap();
        assert!(content.contains("to: src/{{ name }}.rs"));
        assert!(content.contains("pub struct {{ name | pascal_case }}"));
        assert!(content.contains("name: \"my_module\""));
    }

    #[test]
    fn test_generate_python_template() {
        let content = generate_template_content("my_module", "python").unwrap();
        assert!(content.contains("to: src/{{ name }}.py"));
        assert!(content.contains("class {{ name | pascal_case }}"));
        assert!(content.contains("name: \"my_module\""));
    }

    #[test]
    fn test_generate_typescript_template() {
        let content = generate_template_content("my_module", "typescript").unwrap();
        assert!(content.contains("to: src/{{ name }}.ts"));
        assert!(content.contains("interface {{ name | pascal_case }}"));
        assert!(content.contains("name: \"my_module\""));
    }

    #[test]
    fn test_generate_generic_template() {
        let content = generate_template_content("test", "generic").unwrap();
        assert!(content.contains("to: output/{{ name }}.txt"));
        assert!(content.contains("Hello, {{ name }}!"));
    }

    #[test]
    fn test_generate_unknown_type_fallback() {
        let content = generate_template_content("test", "unknown").unwrap();
        assert!(content.contains("to: output/{{ name }}.txt"));
        assert!(content.contains("generic template"));
    }
}
