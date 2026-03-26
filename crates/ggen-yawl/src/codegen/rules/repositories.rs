//! Rule 4: Repository Interfaces - Spring Data JPA repositories for persistence.
//!
//! Generates `@Repository`-annotated Spring Data JPA interfaces for each persistent
//! YAWL entity (YWorkItemRepository, YTaskRepository, etc.)

use crate::error::{Error, Result};
use ggen_codegen::Result as CodegenResult;
use ggen_codegen::{Error as CodegenError, GenerationMode, Queryable, Renderable, Rule};
use std::collections::HashMap;
use std::path::PathBuf;
use tera::{Context, Tera};

/// Query executor for repository generation.
pub struct RepositoryQuery;

impl RepositoryQuery {
    /// Create a new repository query for extracting entity ID field types.
    pub fn new() -> Self {
        Self
    }
}

impl Queryable for RepositoryQuery {
    fn execute(&self) -> CodegenResult<Vec<HashMap<String, String>>> {
        // SPARQL Query 4.1: Entity ID Field Types from yawl-domain.ttl
        // Extracts entity name and ID field type (string, long, int, uuid, etc.)
        // Used to generate JpaRepository<Entity, IdType> generics
        let _query = "PREFIX yawl: <https://yawlfoundation.org/ontology#>\n\n\
            SELECT ?entity ?simpleName ?idFieldType\n\
            WHERE {\n\
              ?entity a yawl:Entity ;\n\
                      yawl:className ?entity_full_name ;\n\
                      yawl:hasIdField ?idField .\n\
              ?idField yawl:fieldType ?idFieldType .\n\
              BIND(REPLACE(STR(?entity_full_name), \".*\\\\\", \"\") AS ?simpleName)\n\
            }\n\
            ORDER BY ?simpleName"
            .to_string();

        // For now, return mock data that demonstrates the pattern.
        // In Phase 3 final, this will load the actual YAWL ontology and execute SPARQL.
        let entities = vec![
            ("YWorkItem", "String"),
            ("YTask", "String"),
            ("YIdentifier", "String"),
            ("YMarking", "String"),
            ("YVariable", "String"),
        ];

        let results = entities
            .into_iter()
            .map(|(entity, id_type)| {
                let mut bindings = HashMap::new();
                bindings.insert("entityName".to_string(), entity.to_string());
                bindings.insert(
                    "repositoryName".to_string(),
                    format!("{}Repository", entity),
                );
                bindings.insert("idType".to_string(), id_type.to_string());
                bindings
            })
            .collect();

        Ok(results)
    }

    fn name(&self) -> &str {
        "repository-query"
    }
}

/// Template renderer for repository generation.
pub struct RepositoryTemplate {
    tera: Tera,
}

impl RepositoryTemplate {
    /// Create a new repository template renderer.
    pub fn new() -> Result<Self> {
        let mut tera = Tera::default();

        let template = r#"
package org.yawlfoundation.yawl.repositories;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;
import org.yawlfoundation.yawl.elements.{{ entityName }};
import java.util.List;
import java.util.Optional;

/**
 * Spring Data JPA Repository for {{ entityName }}.
 *
 * This interface is auto-generated from the YAWL ontology.
 * Do not edit manually.
 *
 * @generated from SPARQL query: repository-query
 */
@Repository
public interface {{ repositoryName }} extends JpaRepository<{{ entityName }}, {{ idType }}> {

    /**
     * Find all {{ entityName }} entities.
     */
    List<{{ entityName }}> findAll();

    /**
     * Find {{ entityName }} by ID.
     */
    Optional<{{ entityName }}> findById({{ idType }} id);

    /**
     * Save or update {{ entityName }}.
     */
    {{ entityName }} save({{ entityName }} entity);

    /**
     * Delete {{ entityName }} by ID.
     */
    void deleteById({{ idType }} id);

    /**
     * Count total {{ entityName }} entities.
     */
    long count();
}
"#;

        tera.add_raw_template("repository.java.tera", template)
            .map_err(|e| Error::template(format!("Failed to load repository template: {}", e)))?;

        Ok(Self { tera })
    }
}

impl Renderable for RepositoryTemplate {
    fn render(&self, bindings: &HashMap<String, String>) -> CodegenResult<String> {
        let mut context = Context::new();

        let entity_name = bindings
            .get("entityName")
            .ok_or_else(|| CodegenError::template("Missing entityName in bindings".to_string()))?
            .clone();

        let repo_name = bindings
            .get("repositoryName")
            .unwrap_or(&format!("{}Repository", entity_name))
            .clone();

        let id_type = bindings
            .get("idType")
            .unwrap_or(&"String".to_string())
            .clone();

        context.insert("entityName", &entity_name);
        context.insert("repositoryName", &repo_name);
        context.insert("idType", &id_type);

        self.tera
            .render("repository.java.tera", &context)
            .map_err(|e| CodegenError::template(format!("Template rendering failed: {}", e)))
    }

    fn name(&self) -> &str {
        "repository-template"
    }
}

/// Create a repository generation rule.
pub fn create_repository_rule() -> Result<Rule<RepositoryQuery, RepositoryTemplate>> {
    let query = RepositoryQuery::new();
    let template = RepositoryTemplate::new()?;

    let rule = Rule::new(
        "repositories",
        query,
        template,
        PathBuf::from(
            "src/main/java/org/yawlfoundation/yawl/repositories/{{ repositoryName }}.java",
        ),
        GenerationMode::Overwrite,
    );

    Ok(rule)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_repository_rule_generates() {
        let rule = create_repository_rule().expect("Rule should create");
        let files = rule.execute().expect("Rule should execute");

        assert!(!files.is_empty());
        assert!(files[0].content.contains("@Repository"));
        assert!(files[0].content.contains("extends JpaRepository"));
    }

    #[test]
    fn test_repository_query_executes() {
        let query = RepositoryQuery::new();
        let results = query.execute().expect("Query should execute");

        assert!(results.len() >= 5);
        assert!(results
            .iter()
            .any(|r| r.get("entityName") == Some(&"YWorkItem".to_string())));
    }
}
