//! YAWL XML code generation.
//!
//! This module provides the [`YawlXmlGenerator`] type and helper functions for
//! generating YAWL XML workflow specifications.
//!
//! # Example
//!
//! ```rust,no_run
//! use ggen_yawl::codegen::YawlXmlGenerator;
//! use ggen_yawl::template::TemplateContext;
//!
//! # fn main() -> Result<(), Box<dyn std::error::Error>> {
//! let context = TemplateContext::default();
//! let xml = YawlXmlGenerator::generate(&context)?;
//! # Ok(())
//! # }
//! ```

use crate::{Error, Result};

/// Generator for YAWL XML workflow specifications.
///
/// The [`YawlXmlGenerator`] provides methods for generating YAWL XML from
/// template contexts, including canonicalization and validation.
///
/// # Example
///
/// ```rust,no_run
/// use ggen_yawl::codegen::YawlXmlGenerator;
/// use ggen_yawl::template::TemplateContext;
///
/// # fn main() -> Result<(), Box<dyn std::error::Error>> {
/// let context = TemplateContext::default();
/// let xml = YawlXmlGenerator::generate(&context)?;
/// # Ok(())
/// # }
/// ```
#[derive(Debug, Clone, Default)]
pub struct YawlXmlGenerator;

impl YawlXmlGenerator {
    /// Generate YAWL XML from template context.
    ///
    /// # Arguments
    ///
    /// * `ctx` - The template context containing workflow definition
    ///
    /// # Errors
    ///
    /// Returns an error if XML generation fails.
    ///
    /// # Example
    ///
    /// ```rust,no_run
    /// use ggen_yawl::codegen::YawlXmlGenerator;
    /// use ggen_yawl::template::TemplateContext;
    ///
    /// # fn main() -> Result<(), Box<dyn std::error::Error>> {
    /// let context = TemplateContext::default();
    /// let xml = YawlXmlGenerator::generate(&context)?;
    /// # Ok(())
    /// # }
    /// ```
    pub fn generate(ctx: &crate::template::TemplateContext) -> Result<String> {
        let mut xml = String::new();

        // XML declaration
        xml.push_str("<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n");

        // Root element
        xml.push_str("<specification xmlns=\"http://www.yawlfoundation.org/yawlschema\" version=\"2.0\">\n");
        xml.push_str(&format!("  <name>{}</name>\n", escape_xml(&ctx.workflow_name)));
        xml.push_str(&format!("  <description>{}</description>\n", escape_xml(&ctx.description)));

        // Decomposition (net)
        xml.push_str("  <decomposition id=\"");
        xml.push_str(&ctx.workflow_name);
        xml.push_str("_net\" type=\"WSNet\">\n");

        // Input condition
        xml.push_str("    <inputCondition id=\"input\"/>\n");

        // Output condition
        xml.push_str("    <outputCondition id=\"output\"/>\n");

        // Tasks
        for task in &ctx.tasks {
            xml.push_str(&format!(
                "    <task id=\"{}\" name=\"{}\">\n",
                escape_xml(&task.id),
                escape_xml(&task.name)
            ));
            xml.push_str(&format!("      <split type=\"{}\"/>\n", task.split_type));
            xml.push_str(&format!("      <join type=\"{}\"/>\n", task.join_type));
            if task.is_auto {
                xml.push_str("      <starting/>\n");
            }
            xml.push_str("    </task>\n");
        }

        // Flows
        for flow in &ctx.flows {
            xml.push_str(&format!(
                "    <flow into=\"{}\" from=\"{}\"",
                escape_xml(&flow.target),
                escape_xml(&flow.source)
            ));
            if let Some(condition) = &flow.condition {
                xml.push_str(&format!(">\n      <predicate>{}</predicate>\n    </flow>\n", escape_xml(condition)));
            } else {
                xml.push_str("/>\n");
            }
        }

        xml.push_str("  </decomposition>\n");
        xml.push_str("</specification>\n");

        Ok(xml)
    }
}

/// Escape special XML characters.
///
/// Converts XML special characters to their entity equivalents
/// to ensure valid XML output.
///
/// # Arguments
///
/// * `s` - The string to escape
///
/// # Returns
///
/// A new string with special characters replaced by XML entities.
///
/// # Example
///
/// ```rust
/// use ggen_yawl::escape_xml;
///
/// assert_eq!(escape_xml("hello & world"), "hello &amp; world");
/// assert_eq!(escape_xml("a < b"), "a &lt; b");
/// assert_eq!(escape_xml("\"quoted\""), "&quot;quoted&quot;");
/// ```
pub fn escape_xml(s: &str) -> String {
    s.replace('&', "&amp;")
        .replace('<', "&lt;")
        .replace('>', "&gt;")
        .replace('"', "&quot;")
        .replace('\'', "&apos;")
}

/// Canonicalize YAWL XML by normalizing formatting.
///
/// This function normalizes XML formatting for deterministic output.
/// Currently a placeholder that returns the input unchanged.
///
/// # Arguments
///
/// * `xml` - The XML string to canonicalize
///
/// # Returns
///
/// The canonicalized XML string.
///
/// # Future Enhancements
///
/// Planned features include:
/// - Sorting attributes alphabetically
/// - Normalizing whitespace
/// - Removing comments
/// - Consistent indentation
pub fn canonicalize(xml: &str) -> Result<String> {
    // For now, just return the XML as-is
    // In production, this would:
    // - Sort attributes
    // - Normalize whitespace
    // - Remove comments
    Ok(xml.to_string())
}

/// Validate YAWL XML against the schema.
///
/// Performs basic structural validation of YAWL XML to ensure it has
/// the required elements for a valid workflow specification.
///
/// # Arguments
///
/// * `xml` - The XML string to validate
///
/// # Errors
///
/// Returns an error if:
/// - The XML declaration is missing
/// - The specification element is missing
/// - The specification element is not properly closed
///
/// # Example
///
/// ```rust
/// use ggen_yawl::codegen::validate;
///
/// // Valid XML
/// let valid = r#"<?xml version="1.0"?><specification xmlns="http://www.yawlfoundation.org/yawlschema"></specification>"#;
/// assert!(validate(valid).is_ok());
///
/// // Invalid XML - missing declaration
/// let invalid = r#"<specification></specification>"#;
/// assert!(validate(invalid).is_err());
/// ```
pub fn validate(xml: &str) -> Result<()> {
    // Basic validation checks
    if !xml.contains("<?xml") {
        return Err(Error::validation("Missing XML declaration"));
    }
    if !xml.contains("<specification") {
        return Err(Error::validation("Missing specification element"));
    }
    if !xml.contains("</specification>") {
        return Err(Error::validation("Unclosed specification element"));
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_escape_xml() {
        assert_eq!(escape_xml("hello & world"), "hello &amp; world");
        assert_eq!(escape_xml("a < b"), "a &lt; b");
        assert_eq!(escape_xml("a > b"), "a &gt; b");
        assert_eq!(escape_xml("\"quoted\""), "&quot;quoted&quot;");
    }

    #[test]
    fn test_validate_valid_xml() {
        let xml = r#"<?xml version="1.0"?><specification xmlns="http://www.yawlfoundation.org/yawlschema"></specification>"#;
        assert!(validate(xml).is_ok());
    }

    #[test]
    fn test_validate_missing_declaration() {
        let xml = r#"<specification></specification>"#;
        assert!(validate(xml).is_err());
    }

    #[test]
    fn test_canonicalize() {
        let xml = "<test></test>";
        assert_eq!(canonicalize(xml).unwrap(), xml);
    }
}
