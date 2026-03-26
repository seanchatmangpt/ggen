//! Rule 6: REST Controllers - Spring REST endpoints for YAWL entities.
//!
//! Generates `@RestController`-annotated classes that expose CRUD operations
//! over HTTP REST APIs for YAWL entities.

use crate::error::{Error, Result};
use ggen_codegen::Result as CodegenResult;
use ggen_codegen::{Error as CodegenError, GenerationMode, Queryable, Renderable, Rule};
use std::collections::HashMap;
use std::path::PathBuf;
use tera::{Context, Tera};

/// Query executor for REST controller generation.
pub struct ControllerQuery;

impl ControllerQuery {
    /// Create a new controller query for extracting workflow conditions.
    pub fn new() -> Self {
        Self
    }
}

impl Queryable for ControllerQuery {
    fn execute(&self) -> CodegenResult<Vec<HashMap<String, String>>> {
        // SPARQL Query 6.1: Workflow Condition Types from yawl-workflow.ttl
        // Extracts: condition types (InputCondition, OutputCondition, InternalCondition)
        // Used to generate REST controller endpoints for workflow control elements
        let _query = "PREFIX yawl: <https://yawlfoundation.org/ontology#>\n\
            PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>\n\n\
            SELECT ?conditionType ?label ?comment\n\
            WHERE {\n\
              ?conditionType rdfs:subClassOf yawl:Condition ;\n\
                             rdfs:label ?label .\n\
              OPTIONAL { ?conditionType rdfs:comment ?comment }\n\
            }"
            .to_string();

        // For now, return mock data that demonstrates the pattern.
        // In Phase 3 final, this will load the actual YAWL ontology and execute SPARQL.
        let entities = vec!["YWorkItem", "YTask", "YNet", "YEngine"];

        let results = entities
            .into_iter()
            .map(|entity| {
                let mut bindings = HashMap::new();
                bindings.insert("entityName".to_string(), entity.to_string());
                bindings.insert(
                    "controllerName".to_string(),
                    format!("{}Controller", entity),
                );
                bindings.insert(
                    "path".to_string(),
                    format!("/api/{}", entity.to_lowercase()),
                );
                bindings
            })
            .collect();

        Ok(results)
    }

    fn name(&self) -> &str {
        "controller-query"
    }
}

/// Template renderer for REST controller generation.
pub struct ControllerTemplate {
    tera: Tera,
}

impl ControllerTemplate {
    /// Create a new controller template renderer.
    pub fn new() -> Result<Self> {
        let mut tera = Tera::default();

        let template = r#"
package org.yawlfoundation.yawl.controllers;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import org.yawlfoundation.yawl.elements.{{ entityName }};
import org.yawlfoundation.yawl.dtos.{{ entityName }}DTO;
import org.yawlfoundation.yawl.repositories.{{ entityName }}Repository;
import java.util.List;
import java.util.stream.Collectors;

/**
 * Spring REST Controller for {{ entityName }}.
 *
 * This controller is auto-generated from the YAWL ontology.
 * Do not edit manually.
 *
 * @generated from SPARQL query: controller-query
 */
@RestController
@RequestMapping("{{ path }}")
@CrossOrigin(origins = "*")
public class {{ controllerName }} {

    @Autowired
    private {{ entityName }}Repository repository;

    /**
     * GET: Retrieve all {{ entityName }} entities.
     */
    @GetMapping
    public ResponseEntity<List<{{ entityName }}DTO>> getAll() {
        List<{{ entityName }}DTO> dtos = repository.findAll()
            .stream()
            .map({{ entityName }}DTO::from)
            .collect(Collectors.toList());
        return ResponseEntity.ok(dtos);
    }

    /**
     * GET: Retrieve {{ entityName }} by ID.
     */
    @GetMapping("/{id}")
    public ResponseEntity<{{ entityName }}DTO> getById(@PathVariable String id) {
        return repository.findById(id)
            .map({{ entityName }}DTO::from)
            .map(ResponseEntity::ok)
            .orElse(ResponseEntity.notFound().build());
    }

    /**
     * POST: Create new {{ entityName }}.
     */
    @PostMapping
    public ResponseEntity<{{ entityName }}DTO> create(@RequestBody {{ entityName }}DTO dto) {
        {{ entityName }} entity = dto.toEntity();
        {{ entityName }} saved = repository.save(entity);
        return ResponseEntity.ok({{ entityName }}DTO.from(saved));
    }

    /**
     * PUT: Update existing {{ entityName }}.
     */
    @PutMapping("/{id}")
    public ResponseEntity<{{ entityName }}DTO> update(
            @PathVariable String id,
            @RequestBody {{ entityName }}DTO dto) {
        return repository.findById(id)
            .map(existing -> {
                existing.setName(dto.getName());
                existing.setStatus(dto.getStatus());
                {{ entityName }} updated = repository.save(existing);
                return ResponseEntity.ok({{ entityName }}DTO.from(updated));
            })
            .orElse(ResponseEntity.notFound().build());
    }

    /**
     * DELETE: Remove {{ entityName }} by ID.
     */
    @DeleteMapping("/{id}")
    public ResponseEntity<Void> delete(@PathVariable String id) {
        if (repository.existsById(id)) {
            repository.deleteById(id);
            return ResponseEntity.noContent().build();
        }
        return ResponseEntity.notFound().build();
    }
}
"#;

        tera.add_raw_template("controller.java.tera", template)
            .map_err(|e| Error::template(format!("Failed to load controller template: {}", e)))?;

        Ok(Self { tera })
    }
}

impl Renderable for ControllerTemplate {
    fn render(&self, bindings: &HashMap<String, String>) -> CodegenResult<String> {
        let mut context = Context::new();

        let entity_name = bindings
            .get("entityName")
            .ok_or_else(|| CodegenError::template("Missing entityName in bindings".to_string()))?
            .clone();

        let controller_name = bindings
            .get("controllerName")
            .unwrap_or(&format!("{}Controller", entity_name))
            .clone();

        let path = bindings
            .get("path")
            .unwrap_or(&format!("/api/{}", entity_name.to_lowercase()))
            .clone();

        context.insert("entityName", &entity_name);
        context.insert("controllerName", &controller_name);
        context.insert("path", &path);

        self.tera
            .render("controller.java.tera", &context)
            .map_err(|e| CodegenError::template(format!("Template rendering failed: {}", e)))
    }

    fn name(&self) -> &str {
        "controller-template"
    }
}

/// Create a REST controller generation rule.
pub fn create_controller_rule() -> Result<Rule<ControllerQuery, ControllerTemplate>> {
    let query = ControllerQuery::new();
    let template = ControllerTemplate::new()?;

    let rule = Rule::new(
        "controllers",
        query,
        template,
        PathBuf::from(
            "src/main/java/org/yawlfoundation/yawl/controllers/{{ controllerName }}.java",
        ),
        GenerationMode::Overwrite,
    );

    Ok(rule)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_controller_rule_generates() {
        let rule = create_controller_rule().expect("Rule should create");
        let files = rule.execute().expect("Rule should execute");

        assert!(!files.is_empty());
        assert!(files[0].content.contains("@RestController"));
        assert!(files[0].content.contains("@RequestMapping"));
        assert!(files[0].content.contains("@GetMapping"));
    }
}
