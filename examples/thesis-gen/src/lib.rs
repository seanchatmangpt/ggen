use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ThesisTemplate {
    pub title: String,
    pub author: String,
    pub content: String,
}

#[derive(Debug, Clone)]
pub struct ThesisGenerator {
    templates: Vec<ThesisTemplate>,
}

impl Default for ThesisGenerator {
    fn default() -> Self {
        Self::new()
    }
}

impl ThesisGenerator {
    pub fn new() -> Self {
        Self {
            templates: Vec::new(),
        }
    }

    pub fn add_template(&mut self, template: ThesisTemplate) {
        self.templates.push(template);
    }

    pub fn templates(&self) -> &[ThesisTemplate] {
        &self.templates
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_generator_creation() {
        let gen = ThesisGenerator::new();
        assert_eq!(gen.templates().len(), 0);
    }

    #[test]
    fn test_add_template() {
        let mut gen = ThesisGenerator::new();
        gen.add_template(ThesisTemplate {
            title: "Research Methods".to_string(),
            author: "Author Name".to_string(),
            content: "Introduction...".to_string(),
        });
        assert_eq!(gen.templates().len(), 1);
    }
}
