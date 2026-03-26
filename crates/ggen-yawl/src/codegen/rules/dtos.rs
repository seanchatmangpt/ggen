//! Rule 5: DTOs - Data Transfer Objects for REST API responses.
//!
//! Generates immutable DTO classes with @Data and @Builder annotations
//! for transferring YAWL entity data over REST APIs.

use crate::codegen::{GenerationMode, Queryable, Renderable, Rule};
use crate::error::{Error, Result};
use std::collections::HashMap;
use std::path::PathBuf;
use tera::{Context, Tera};

/// Query executor for DTO generation.
pub struct DtoQuery;

impl DtoQuery {
    /// Create a new DTO query.
    pub fn new() -> Self {
        Self
    }
}

impl Queryable for DtoQuery {
    fn execute(&self) -> Result<Vec<HashMap<String, String>>> {
        let entities = vec!["YWorkItem", "YTask", "YIdentifier", "YMarking", "YVariable"];

        let results = entities
            .into_iter()
            .map(|entity| {
                let mut bindings = HashMap::new();
                bindings.insert("dtoName".to_string(), format!("{}DTO", entity));
                bindings.insert("entityName".to_string(), entity.to_string());
                bindings
            })
            .collect();

        Ok(results)
    }

    fn name(&self) -> &str {
        "dto-query"
    }
}

/// Template renderer for DTO generation.
pub struct DtoTemplate {
    tera: Tera,
}

impl DtoTemplate {
    /// Create a new DTO template renderer.
    pub fn new() -> Result<Self> {
        let mut tera = Tera::default();

        let template = r#"
package org.yawlfoundation.yawl.dtos;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import org.yawlfoundation.yawl.elements.{{ entityName }};
import java.io.Serializable;

/**
 * Data Transfer Object for {{ entityName }}.
 *
 * Immutable DTO for transferring {{ entityName }} data over REST APIs.
 * This class is auto-generated from the YAWL ontology.
 * Do not edit manually.
 *
 * @generated from SPARQL query: dto-query
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class {{ dtoName }} implements Serializable {
    private static final long serialVersionUID = 1L;

    private String id;
    private String name;
    private String status;

    /**
     * Factory method: convert entity to DTO.
     */
    public static {{ dtoName }} from({{ entityName }} entity) {
        if (entity == null) {
            return null;
        }

        return {{ dtoName }}.builder()
            .id(entity.getId())
            .name(entity.getName())
            .status(entity.getStatus())
            .build();
    }

    /**
     * Convert DTO back to entity (partial).
     */
    public {{ entityName }} toEntity() {
        {{ entityName }} entity = new {{ entityName }}();
        entity.setId(this.id);
        entity.setName(this.name);
        entity.setStatus(this.status);
        return entity;
    }

    @Override
    public String toString() {
        return "{{ dtoName }}{" +
            "id='" + id + '\'' +
            ", name='" + name + '\'' +
            ", status='" + status + '\'' +
            '}';
    }
}
"#;

        tera.add_raw_template("dto.java.tera", template)
            .map_err(|e| Error::template(format!("Failed to load DTO template: {}", e)))?;

        Ok(Self { tera })
    }
}

impl Renderable for DtoTemplate {
    fn render(&self, bindings: &HashMap<String, String>) -> Result<String> {
        let mut context = Context::new();

        let entity_name = bindings
            .get("entityName")
            .ok_or_else(|| Error::template("Missing entityName in bindings".to_string()))?
            .clone();

        let dto_name = bindings
            .get("dtoName")
            .unwrap_or(&format!("{}DTO", entity_name))
            .clone();

        context.insert("entityName", &entity_name);
        context.insert("dtoName", &dto_name);

        self.tera
            .render("dto.java.tera", &context)
            .map_err(|e| Error::template(format!("Template rendering failed: {}", e)))
    }

    fn name(&self) -> &str {
        "dto-template"
    }
}

/// Create a DTO generation rule.
pub fn create_dto_rule() -> Result<Rule<DtoQuery, DtoTemplate>> {
    let query = DtoQuery::new();
    let template = DtoTemplate::new()?;

    let rule = Rule::new(
        "dtos",
        query,
        template,
        PathBuf::from("src/main/java/org/yawlfoundation/yawl/dtos/{{ dtoName }}.java"),
        GenerationMode::Overwrite,
    );

    Ok(rule)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_dto_rule_generates() {
        let rule = create_dto_rule().expect("Rule should create");
        let files = rule.execute().expect("Rule should execute");

        assert!(!files.is_empty());
        assert!(files[0].content.contains("@Data"));
        assert!(files[0].content.contains("@Builder"));
        assert!(files[0].content.contains("from("));
    }
}
