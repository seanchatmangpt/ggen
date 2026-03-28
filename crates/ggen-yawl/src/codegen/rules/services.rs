//! Rule 8: Service Classes - Business logic services for YAWL entities.
//!
//! Generates `@Service`-annotated classes that encapsulate business logic
//! for YAWL entities with transactional boundaries.

use crate::error::{Error, Result};
use ggen_codegen::Result as CodegenResult;
use ggen_codegen::{Error as CodegenError, GenerationMode, Queryable, Renderable, Rule};
use std::collections::HashMap;
use std::path::PathBuf;
use tera::{Context, Tera};

/// Query executor for service generation.
pub struct ServiceQuery;

impl ServiceQuery {
    /// Create a new service query.
    pub fn new() -> Self {
        Self
    }
}

impl Queryable for ServiceQuery {
    fn execute(&self) -> CodegenResult<Vec<HashMap<String, String>>> {
        let entities = vec!["YWorkItem", "YTask", "YNet", "YEngine"];

        let results = entities
            .into_iter()
            .map(|entity| {
                let mut bindings = HashMap::new();
                bindings.insert("entityName".to_string(), entity.to_string());
                bindings.insert("serviceName".to_string(), format!("{}Service", entity));
                bindings.insert(
                    "repositoryName".to_string(),
                    format!("{}Repository", entity),
                );
                bindings
            })
            .collect();

        Ok(results)
    }

    fn name(&self) -> &str {
        "service-query"
    }
}

/// Template renderer for service generation.
pub struct ServiceTemplate {
    tera: Tera,
}

impl ServiceTemplate {
    /// Create a new service template renderer.
    pub fn new() -> Result<Self> {
        let mut tera = Tera::default();

        let template = r#"
package org.yawlfoundation.yawl.services;

import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.yawlfoundation.yawl.elements.{{ entityName }};
import org.yawlfoundation.yawl.repositories.{{ repositoryName }};
import jakarta.persistence.EntityNotFoundException;
import java.util.List;
import java.util.Optional;

/**
 * Business Logic Service for {{ entityName }}.
 *
 * This service is auto-generated from the YAWL ontology.
 * Do not edit manually.
 *
 * @generated from SPARQL query: service-query
 */
@Slf4j
@Service
public class {{ serviceName }} {

    @Autowired
    private {{ repositoryName }} {{ entityName | lower }}Repo;

    /**
     * Retrieve all {{ entityName }} entities.
     */
    @Transactional(readOnly = true)
    public List<{{ entityName }}> findAll() {
        log.debug("Retrieving all {{ entityName }} entities");
        return {{ entityName | lower }}Repo.findAll();
    }

    /**
     * Retrieve {{ entityName }} by ID.
     */
    @Transactional(readOnly = true)
    public {{ entityName }} findById(String id) {
        log.debug("Retrieving {{ entityName }} with id: {}", id);
        return {{ entityName | lower }}Repo.findById(id)
            .orElseThrow(() -> new EntityNotFoundException("{{ entityName }} not found: " + id));
    }

    /**
     * Create or update {{ entityName }}.
     */
    @Transactional
    public {{ entityName }} save({{ entityName }} entity) {
        log.info("Saving {{ entityName }}: {}", entity.getId());
        return {{ entityName | lower }}Repo.save(entity);
    }

    /**
     * Delete {{ entityName }} by ID.
     */
    @Transactional
    public void deleteById(String id) {
        log.info("Deleting {{ entityName }} with id: {}", id);
        if (!{{ entityName | lower }}Repo.existsById(id)) {
            throw new EntityNotFoundException("{{ entityName }} not found: " + id);
        }
        {{ entityName | lower }}Repo.deleteById(id);
    }

    /**
     * Count total {{ entityName }} entities.
     */
    @Transactional(readOnly = true)
    public long count() {
        return {{ entityName | lower }}Repo.count();
    }
}
"#;

        tera.add_raw_template("service.java.tera", template)
            .map_err(|e| Error::template(format!("Failed to load service template: {}", e)))?;

        Ok(Self { tera })
    }
}

impl Renderable for ServiceTemplate {
    fn render(&self, bindings: &HashMap<String, String>) -> CodegenResult<String> {
        let mut context = Context::new();

        let entity_name = bindings
            .get("entityName")
            .ok_or_else(|| CodegenError::template("Missing entityName in bindings".to_string()))?
            .clone();

        let service_name = bindings
            .get("serviceName")
            .unwrap_or(&format!("{}Service", entity_name))
            .clone();

        let repo_name = bindings
            .get("repositoryName")
            .unwrap_or(&format!("{}Repository", entity_name))
            .clone();

        context.insert("entityName", &entity_name);
        context.insert("serviceName", &service_name);
        context.insert("repositoryName", &repo_name);

        self.tera
            .render("service.java.tera", &context)
            .map_err(|e| CodegenError::template(format!("Template rendering failed: {}", e)))
    }

    fn name(&self) -> &str {
        "service-template"
    }
}

/// Create a service generation rule.
pub fn create_service_rule() -> Result<Rule<ServiceQuery, ServiceTemplate>> {
    let query = ServiceQuery::new();
    let template = ServiceTemplate::new()?;

    let rule = Rule::new(
        "services",
        query,
        template,
        PathBuf::from("src/main/java/org/yawlfoundation/yawl/services/{{ serviceName }}.java"),
        GenerationMode::Overwrite,
    );

    Ok(rule)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_service_rule_generates() {
        let rule = create_service_rule().expect("Rule should create");
        let files = rule.execute().expect("Rule should execute");

        assert!(!files.is_empty());
        assert!(files[0].content.contains("@Service"));
        assert!(files[0].content.contains("@Transactional"));
        assert!(files[0].content.contains("findById(String id)"));
    }
}
