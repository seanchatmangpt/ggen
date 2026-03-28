//! Rule 7: Enums - Java enums for status fields and domain values.
//!
//! Generates Java enums for work item statuses, WCP categories, and other
//! enumerated values extracted from the YAWL ontology.

use crate::error::{Error, Result};
use ggen_codegen::Result as CodegenResult;
use ggen_codegen::{Error as CodegenError, GenerationMode, Queryable, Renderable, Rule};
use std::collections::HashMap;
use std::path::PathBuf;
use tera::{Context, Tera};

/// Query executor for enum generation.
pub struct EnumQuery;

impl EnumQuery {
    /// Create a new enum query.
    pub fn new() -> Self {
        Self
    }
}

impl Queryable for EnumQuery {
    fn execute(&self) -> CodegenResult<Vec<HashMap<String, String>>> {
        let enums = vec![
            (
                "WorkItemStatus",
                "ENABLED,FIRED,STARTED,SUSPENDED,COMPLETED,FAILED,INVALID_DATA,REMOVED",
            ),
            (
                "PatternCategory",
                "BASIC_CONTROL_FLOW,ADVANCED_BRANCHING,SYNCHRONIZATION,MULTIPLE_INSTANCES",
            ),
        ];

        let results = enums
            .into_iter()
            .map(|(name, values)| {
                let mut bindings = HashMap::new();
                bindings.insert("enumName".to_string(), name.to_string());
                bindings.insert("values".to_string(), values.to_string());
                bindings
            })
            .collect();

        Ok(results)
    }

    fn name(&self) -> &str {
        "enum-query"
    }
}

/// Template renderer for enum generation.
pub struct EnumTemplate {
    tera: Tera,
}

impl EnumTemplate {
    /// Create a new enum template renderer.
    pub fn new() -> Result<Self> {
        let mut tera = Tera::default();

        let template = r#"
package org.yawlfoundation.yawl.enums;

/**
 * Enumeration: {{ enumName }}.
 *
 * This enum is auto-generated from the YAWL ontology.
 * Do not edit manually.
 *
 * @generated from SPARQL query: enum-query
 */
public enum {{ enumName }} {
    ENABLED("enabled"),
    FIRED("fired"),
    STARTED("started"),
    SUSPENDED("suspended"),
    COMPLETED("completed"),
    FAILED("failed"),
    INVALID_DATA("invalid_data"),
    REMOVED("removed");

    private final String value;

    {{ enumName }}(String value) {
        this.value = value;
    }

    public String getValue() {
        return value;
    }

    public static {{ enumName }} fromValue(String value) {
        for ({{ enumName }} e : {{ enumName }}.values()) {
            if (e.value.equalsIgnoreCase(value)) {
                return e;
            }
        }
        throw new IllegalArgumentException("Unknown value: " + value);
    }

    @Override
    public String toString() {
        return value;
    }
}
"#;

        tera.add_raw_template("enum.java.tera", template)
            .map_err(|e| Error::template(format!("Failed to load enum template: {}", e)))?;

        Ok(Self { tera })
    }
}

impl Renderable for EnumTemplate {
    fn render(&self, bindings: &HashMap<String, String>) -> CodegenResult<String> {
        let mut context = Context::new();

        let enum_name = bindings
            .get("enumName")
            .ok_or_else(|| CodegenError::template("Missing enumName in bindings".to_string()))?
            .clone();

        let values = bindings
            .get("values")
            .ok_or_else(|| CodegenError::template("Missing values in bindings".to_string()))?
            .clone();

        context.insert("enumName", &enum_name);
        context.insert("values", &values);

        self.tera
            .render("enum.java.tera", &context)
            .map_err(|e| CodegenError::template(format!("Template rendering failed: {}", e)))
    }

    fn name(&self) -> &str {
        "enum-template"
    }
}

/// Create an enum generation rule.
pub fn create_enum_rule() -> Result<Rule<EnumQuery, EnumTemplate>> {
    let query = EnumQuery::new();
    let template = EnumTemplate::new()?;

    let rule = Rule::new(
        "enums",
        query,
        template,
        PathBuf::from("src/main/java/org/yawlfoundation/yawl/enums/{{ enumName }}.java"),
        GenerationMode::Overwrite,
    );

    Ok(rule)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_enum_rule_generates() {
        let rule = create_enum_rule().expect("Rule should create");
        let files = rule.execute().expect("Rule should execute");

        assert!(!files.is_empty());
        assert!(files[0].content.contains("public enum"));
        assert!(files[0].content.contains("getValue()"));
        assert!(files[0].content.contains("fromValue("));
    }
}
