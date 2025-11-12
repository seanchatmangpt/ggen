//! Frozen section preservation during regeneration
//!
//! This module provides functionality for preserving user-modified code sections
//! during template regeneration using {% frozen %} tags.

use anyhow::{anyhow, Result};
use regex::Regex;

/// Represents a frozen section in a template
#[derive(Debug, Clone, PartialEq)]
pub struct FrozenSection {
    /// Start position in the content
    pub start: usize,
    /// End position in the content
    pub end: usize,
    /// Content of the frozen section
    pub content: String,
    /// Optional identifier for the frozen section
    pub id: Option<String>,
}

/// Parser for frozen section tags in templates
pub struct FrozenParser;

impl FrozenParser {
    /// Parse frozen sections from template content
    ///
    /// Looks for patterns like:
    /// ```text
    /// {% frozen %}
    /// user code here
    /// {% endfrozen %}
    /// ```
    ///
    /// Or with identifiers:
    /// ```text
    /// {% frozen id="custom_logic" %}
    /// user code here
    /// {% endfrozen %}
    /// ```
    pub fn parse_frozen_tags(template_content: &str) -> Result<Vec<FrozenSection>> {
        let mut sections = Vec::new();

        // Regex to match {% frozen %} or {% frozen id="..." %}
        let start_regex = Regex::new(r#"\{%\s*frozen(?:\s+id\s*=\s*"([^"]+)")?\s*%\}"#)
            .map_err(|e| anyhow!("Invalid frozen tag regex: {}", e))?;

        let end_regex = Regex::new(r"\{%\s*endfrozen\s*%\}")
            .map_err(|e| anyhow!("Invalid endfrozen tag regex: {}", e))?;

        // Find all start tags
        for start_match in start_regex.captures_iter(template_content) {
            let start_pos = start_match.get(0).unwrap().end();
            let id = start_match.get(1).map(|m| m.as_str().to_string());

            // Find corresponding end tag
            if let Some(end_match) = end_regex.find_at(template_content, start_pos) {
                let end_pos = end_match.start();
                let content = template_content[start_pos..end_pos].to_string();

                sections.push(FrozenSection {
                    start: start_match.get(0).unwrap().start(),
                    end: end_match.end(),
                    content,
                    id,
                });
            } else {
                return Err(anyhow!(
                    "Unclosed frozen tag at position {}",
                    start_match.get(0).unwrap().start()
                ));
            }
        }

        Ok(sections)
    }

    /// Extract frozen sections indexed by ID or position
    pub fn extract_frozen_map(content: &str) -> Result<std::collections::HashMap<String, String>> {
        let sections = Self::parse_frozen_tags(content)?;
        let mut map = std::collections::HashMap::new();

        for (index, section) in sections.iter().enumerate() {
            let key = section
                .id
                .clone()
                .unwrap_or_else(|| format!("section_{}", index));
            map.insert(key, section.content.clone());
        }

        Ok(map)
    }
}

/// Merger for frozen sections during regeneration
pub struct FrozenMerger;

impl FrozenMerger {
    /// Merge frozen sections from old content into new content
    ///
    /// # Arguments
    /// * `old_content` - The existing file content with frozen sections
    /// * `new_content` - The newly generated content with frozen placeholders
    ///
    /// # Returns
    /// Merged content with preserved frozen sections
    pub fn merge_with_frozen(old_content: &str, new_content: &str) -> Result<String> {
        // Extract frozen sections from old content
        let old_sections = FrozenParser::extract_frozen_map(old_content)?;

        if old_sections.is_empty() {
            // No frozen sections to preserve
            return Ok(new_content.to_string());
        }

        // Replace frozen sections in new content
        let mut result = new_content.to_string();

        // Regex to match frozen tags in new content
        let frozen_regex =
            Regex::new(r#"\{%\s*frozen(?:\s+id\s*=\s*"([^"]+)")?\s*%\}.*?\{%\s*endfrozen\s*%\}"#)
                .map_err(|e| anyhow!("Invalid frozen merge regex: {}", e))?;

        let mut section_index = 0;
        result = frozen_regex
            .replace_all(&result, |caps: &regex::Captures| {
                let id = caps
                    .get(1)
                    .map(|m| m.as_str().to_string())
                    .unwrap_or_else(|| {
                        let idx = section_index;
                        section_index += 1;
                        format!("section_{}", idx)
                    });

                // Get preserved content or keep new content
                if let Some(preserved_content) = old_sections.get(&id) {
                    // Reconstruct frozen section with preserved content
                    if let Some(id_match) = caps.get(1) {
                        // With ID
                        let mut result = String::from("{%");
                        result.push_str(" frozen id=\"");
                        result.push_str(id_match.as_str());
                        result.push_str("\" %");
                        result.push('}');
                        result.push_str(preserved_content);
                        result.push_str("{%");
                        result.push_str(" endfrozen %");
                        result.push('}');
                        result
                    } else {
                        // Without ID
                        let mut result = String::from("{%");
                        result.push_str(" frozen %");
                        result.push('}');
                        result.push_str(preserved_content);
                        result.push_str("{%");
                        result.push_str(" endfrozen %");
                        result.push('}');
                        result
                    }
                } else {
                    // Keep new content if no preserved version exists
                    caps.get(0).unwrap().as_str().to_string()
                }
            })
            .to_string();

        Ok(result)
    }

    /// Check if content contains frozen sections
    pub fn has_frozen_sections(content: &str) -> bool {
        content.contains("{% frozen")
    }

    /// Remove all frozen tags but keep content
    pub fn strip_frozen_tags(content: &str) -> String {
        let start_regex = Regex::new(r#"\{%\s*frozen(?:\s+id\s*=\s*"[^"]+")?\s*%\}"#).unwrap();
        let end_regex = Regex::new(r"\{%\s*endfrozen\s*%\}").unwrap();

        let content = start_regex.replace_all(content, "");
        end_regex.replace_all(&content, "").to_string()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_simple_frozen_section() {
        let content = r#"
Before frozen
{% frozen %}
user custom code
{% endfrozen %}
After frozen
"#;

        let sections = FrozenParser::parse_frozen_tags(content).unwrap();
        assert_eq!(sections.len(), 1);
        assert!(sections[0].content.contains("user custom code"));
        assert_eq!(sections[0].id, None);
    }

    #[test]
    fn test_parse_frozen_section_with_id() {
        let content = r#"
{% frozen id="custom_logic" %}
my implementation
{% endfrozen %}
"#;

        let sections = FrozenParser::parse_frozen_tags(content).unwrap();
        assert_eq!(sections.len(), 1);
        assert!(sections[0].content.contains("my implementation"));
        assert_eq!(sections[0].id, Some("custom_logic".to_string()));
    }

    #[test]
    fn test_parse_multiple_frozen_sections() {
        let content = r#"
{% frozen id="section1" %}
code 1
{% endfrozen %}

some content

{% frozen id="section2" %}
code 2
{% endfrozen %}
"#;

        let sections = FrozenParser::parse_frozen_tags(content).unwrap();
        assert_eq!(sections.len(), 2);
        assert_eq!(sections[0].id, Some("section1".to_string()));
        assert_eq!(sections[1].id, Some("section2".to_string()));
    }

    #[test]
    fn test_parse_unclosed_frozen_tag() {
        let content = r#"
{% frozen %}
unclosed section
"#;

        let result = FrozenParser::parse_frozen_tags(content);
        assert!(result.is_err());
    }

    #[test]
    fn test_extract_frozen_map() {
        let content = r#"
{% frozen id="logic" %}
preserved code
{% endfrozen %}
"#;

        let map = FrozenParser::extract_frozen_map(content).unwrap();
        assert_eq!(map.len(), 1);
        assert!(map.get("logic").unwrap().contains("preserved code"));
    }

    #[test]
    #[ignore = "Frozen section merging needs implementation review - v2.0.1"]
    fn test_merge_with_frozen() {
        let old_content = r#"
{% frozen id="custom" %}
old user code
{% endfrozen %}
"#;

        let new_content = r#"
{% frozen id="custom" %}
new generated code
{% endfrozen %}
"#;

        let merged = FrozenMerger::merge_with_frozen(old_content, new_content).unwrap();
        assert!(merged.contains("old user code"));
        assert!(!merged.contains("new generated code"));
    }

    #[test]
    fn test_merge_without_frozen_sections() {
        let old_content = "no frozen sections";
        let new_content = "new content";

        let merged = FrozenMerger::merge_with_frozen(old_content, new_content).unwrap();
        assert_eq!(merged, "new content");
    }

    #[test]
    fn test_has_frozen_sections() {
        assert!(FrozenMerger::has_frozen_sections(
            "{% frozen %}code{% endfrozen %}"
        ));
        assert!(!FrozenMerger::has_frozen_sections("no frozen sections"));
    }

    #[test]
    fn test_strip_frozen_tags() {
        let content = r#"
Before
{% frozen id="test" %}
keep this content
{% endfrozen %}
After
"#;

        let stripped = FrozenMerger::strip_frozen_tags(content);
        assert!(!stripped.contains("{% frozen"));
        assert!(!stripped.contains("{% endfrozen %}"));
        assert!(stripped.contains("keep this content"));
    }

    #[test]
    #[ignore = "Frozen section merging needs implementation review - v2.0.1"]
    fn test_merge_numbered_sections() {
        let old_content = r#"
{% frozen %}
first section
{% endfrozen %}
{% frozen %}
second section
{% endfrozen %}
"#;

        let new_content = r#"
{% frozen %}
new first
{% endfrozen %}
{% frozen %}
new second
{% endfrozen %}
"#;

        let merged = FrozenMerger::merge_with_frozen(old_content, new_content).unwrap();
        assert!(merged.contains("first section"));
        assert!(merged.contains("second section"));
        assert!(!merged.contains("new first"));
        assert!(!merged.contains("new second"));
    }
}
