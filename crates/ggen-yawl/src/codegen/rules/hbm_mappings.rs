//! Hibernate HBM XML mapping generation (Rule 9).
//!
//! Generates Hibernate 5+ XML mapping files (.hbm.xml) as an alternative to JPA @Entity annotations.
//! This rule extracts entity definitions from the RDF graph and creates database-independent
//! mapping specifications.
//!
//! Output: `src/main/resources/hibernate/{{ EntityName }}.hbm.xml`

use crate::Result;
use ggen_codegen::{Error as CodegenError, GenerationMode, Queryable, Renderable, Rule};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::path::PathBuf;

/// Query executor for HBM mapping generation.
///
/// Executes SPARQL SELECT to extract entity mappings from the YAWL ontology.
pub struct HbmMappingQueryExecutor {
    /// SPARQL SELECT query that extracts entity metadata
    query: String,
}

impl HbmMappingQueryExecutor {
    /// Create a new HBM mapping query executor.
    pub fn new() -> Self {
        // SPARQL Query 9.1: Extract entities with property mappings for HBM generation
        // Extracts: entity URI, className, tableName, properties, packageName
        let query = "PREFIX yawl: <https://yawlfoundation.org/ontology#>\n\
            PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>\n\n\
            SELECT ?entity ?className ?tableName ?properties ?package\n\
            WHERE {\n\
              ?entity a yawl:Entity ;\n\
                      yawl:className ?className ;\n\
                      yawl:tableName ?tableName ;\n\
                      yawl:properties ?properties ;\n\
                      yawl:packageName ?package .\n\
            }\n\
            ORDER BY ?className"
            .to_string();

        Self { query }
    }

    /// SPARQL query text (for debugging)
    pub fn sparql(&self) -> &str {
        &self.query
    }
}

impl Default for HbmMappingQueryExecutor {
    fn default() -> Self {
        Self::new()
    }
}

impl Queryable for HbmMappingQueryExecutor {
    fn execute(&self) -> ggen_codegen::Result<Vec<HashMap<String, String>>> {
        // For now, return mock data that demonstrates HBM mapping extraction.
        // In Phase 3 final, this will load the actual YAWL ontology and execute SPARQL.

        let mut results = Vec::new();

        // Mock binding for YWorkItem with HBM properties
        let mut workitem = HashMap::new();
        workitem.insert("className".to_string(), "YWorkItem".to_string());
        workitem.insert("tableName".to_string(), "y_work_item".to_string());
        workitem.insert(
            "properties".to_string(),
            r#"[
            {"propertyName": "id", "columnName": "id", "hibernateType": "string", "nullable": false, "unique": true},
            {"propertyName": "status", "columnName": "status", "hibernateType": "string", "nullable": false, "unique": false},
            {"propertyName": "taskId", "columnName": "task_id", "hibernateType": "string", "nullable": false, "unique": false},
            {"propertyName": "createdAt", "columnName": "created_at", "hibernateType": "timestamp", "nullable": false, "unique": false}
        ]"#
            .to_string(),
        );
        workitem.insert("idProperty".to_string(), "id".to_string());
        workitem.insert("idGenerator".to_string(), "uuid".to_string());
        workitem.insert("package".to_string(), "org.yawlfoundation.yawl.elements".to_string());
        results.push(workitem);

        // Mock binding for YTask
        let mut task = HashMap::new();
        task.insert("className".to_string(), "YTask".to_string());
        task.insert("tableName".to_string(), "y_task".to_string());
        task.insert(
            "properties".to_string(),
            r#"[
            {"propertyName": "id", "columnName": "id", "hibernateType": "string", "nullable": false, "unique": true},
            {"propertyName": "name", "columnName": "name", "hibernateType": "string", "nullable": false, "unique": false},
            {"propertyName": "decomposition", "columnName": "decomposition", "hibernateType": "string", "nullable": true, "unique": false}
        ]"#
            .to_string(),
        );
        task.insert("idProperty".to_string(), "id".to_string());
        task.insert("idGenerator".to_string(), "uuid".to_string());
        task.insert("package".to_string(), "org.yawlfoundation.yawl.elements".to_string());
        results.push(task);

        Ok(results)
    }

    fn name(&self) -> &str {
        "hbm-mapping-query"
    }

    fn source(&self) -> Option<&str> {
        Some(self.sparql())
    }
}

/// Query results for HBM mapping generation.
///
/// Extracts entity metadata from RDF graph to populate HBM mappings.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct HbmMappingQuery {
    /// Entity class name (e.g., "User", "Order")
    pub entity_name: String,
    /// Entity table name in database (defaults to entity_name if not specified)
    pub table_name: String,
    /// Properties/fields of the entity
    pub properties: Vec<PropertyMapping>,
    /// Primary key field name
    pub id_property: String,
    /// ID generation strategy
    pub id_generator: IdGeneratorStrategy,
    /// Has optimistic locking version field
    pub has_version: bool,
    /// Version property name (if applicable)
    pub version_property: Option<String>,
    /// Package name for the entity class
    pub package: String,
    /// Catalog name (optional)
    pub catalog: Option<String>,
    /// Schema name (optional)
    pub schema: Option<String>,
}

/// Strategy for ID generation
#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq, Eq)]
pub enum IdGeneratorStrategy {
    /// Database-assigned ID (auto-increment)
    Identity,
    /// Sequence-based ID generation
    Sequence,
    /// UUID-based ID
    Uuid,
    /// Application-assigned ID
    Assigned,
}

impl std::fmt::Display for IdGeneratorStrategy {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Identity => write!(f, "identity"),
            Self::Sequence => write!(f, "sequence"),
            Self::Uuid => write!(f, "uuid"),
            Self::Assigned => write!(f, "assigned"),
        }
    }
}

/// Property mapping for entity fields
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct PropertyMapping {
    /// Property/field name in Java class
    pub property_name: String,
    /// Column name in database
    pub column_name: String,
    /// Hibernate type (e.g., "string", "long", "timestamp")
    pub hibernate_type: String,
    /// SQL type (e.g., "VARCHAR(255)", "BIGINT", "TIMESTAMP")
    pub sql_type: String,
    /// Whether field is nullable
    pub nullable: bool,
    /// Whether field is unique
    pub unique: bool,
    /// Whether field is a foreign key
    pub foreign_key: bool,
    /// Target entity for foreign keys
    pub foreign_key_target: Option<String>,
}

/// Template renderer for HBM mapping generation.
///
/// Renders YAWL entity bindings to Hibernate 5+ HBM XML mapping files.
pub struct HbmMappingTemplateRenderer;

impl HbmMappingTemplateRenderer {
    /// Create a new HBM mapping template renderer.
    pub fn new() -> Self {
        Self
    }
}

impl Default for HbmMappingTemplateRenderer {
    fn default() -> Self {
        Self::new()
    }
}

impl Renderable for HbmMappingTemplateRenderer {
    fn render(&self, bindings: &HashMap<String, String>) -> ggen_codegen::Result<String> {
        // Extract class metadata
        let class_name = bindings
            .get("className")
            .ok_or_else(|| CodegenError::template("Missing className in bindings".to_string()))?
            .clone();

        let table_name = bindings
            .get("tableName")
            .unwrap_or(&class_name)
            .clone();

        let id_property = bindings
            .get("idProperty")
            .unwrap_or(&"id".to_string())
            .clone();

        let id_generator = bindings
            .get("idGenerator")
            .unwrap_or(&"uuid".to_string())
            .clone();

        let package = bindings
            .get("package")
            .unwrap_or(&"com.example.model".to_string())
            .clone();

        let mut xml = String::new();

        // XML declaration
        xml.push_str("<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n");

        // Hibernate mapping DTD
        xml.push_str("<!DOCTYPE hibernate-mapping PUBLIC\n");
        xml.push_str("    \"-//Hibernate/Hibernate Mapping DTD 3.0//EN\"\n");
        xml.push_str("    \"http://www.hibernate.org/dtd/hibernate-mapping-3.0.dtd\">\n");

        // Mapping element with package
        xml.push_str(&format!(
            "<hibernate-mapping package=\"{}\">\n",
            package
        ));

        // Class element
        xml.push_str(&format!(
            "  <class name=\"{}\" table=\"{}\">\n",
            class_name, table_name
        ));

        // ID element (primary key)
        xml.push_str(&format!(
            "    <id name=\"{}\" column=\"{}\" type=\"string\">\n",
            id_property, id_property
        ));
        xml.push_str(&format!(
            "      <generator class=\"{}\"/>\n",
            id_generator
        ));
        xml.push_str("    </id>\n");

        // Properties from bindings
        if let Some(properties_json) = bindings.get("properties") {
            // Simple JSON parsing for demo purposes
            // In production, use serde_json for full parsing
            if properties_json.contains("propertyName") {
                // Skip the ID property as it's already handled
                let lines = properties_json.lines();
                for line in lines {
                    if line.contains("propertyName") && !line.contains(&id_property) {
                        // Extract property name from JSON
                        if let Some(start) = line.find("propertyName\": \"") {
                            if let Some(end) = line[start + 16..].find('"') {
                                let prop_name = &line[start + 16..start + 16 + end];

                                // Extract column name
                                let column = if let Some(col_start) = line.find("columnName\": \"") {
                                    if let Some(col_end) = line[col_start + 14..].find('"') {
                                        &line[col_start + 14..col_start + 14 + col_end]
                                    } else {
                                        prop_name
                                    }
                                } else {
                                    prop_name
                                };

                                // Extract type
                                let hib_type = if let Some(type_start) = line.find("hibernateType\": \"") {
                                    if let Some(type_end) = line[type_start + 16..].find('"') {
                                        &line[type_start + 16..type_start + 16 + type_end]
                                    } else {
                                        "string"
                                    }
                                } else {
                                    "string"
                                };

                                // Extract nullable
                                let nullable = line.contains("\"nullable\": true");
                                let not_null_attr = if nullable {
                                    " not-null=\"false\""
                                } else {
                                    " not-null=\"true\""
                                };

                                xml.push_str(&format!(
                                    "    <property name=\"{}\" column=\"{}\" type=\"{}\"{}/>​\n",
                                    prop_name, column, hib_type, not_null_attr
                                ));
                            }
                        }
                    }
                }
            }
        }

        xml.push_str("  </class>\n");
        xml.push_str("</hibernate-mapping>\n");

        Ok(xml)
    }

    fn name(&self) -> &str {
        "hbm-mapping-template"
    }

    fn source(&self) -> Option<&str> {
        Some("Embedded Tera template: Hibernate HBM XML mapping")
    }
}

/// Template data for rendering HBM XML
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct HbmMappingTemplate {
    /// Entity mapping
    pub entity: HbmMappingQuery,
}

impl HbmMappingTemplate {
    /// Create a new HBM mapping template
    pub fn new(entity: HbmMappingQuery) -> Self {
        Self { entity }
    }

    /// Render as Hibernate 5+ HBM XML
    pub fn render(&self) -> Result<String> {
        let mut xml = String::new();

        // XML declaration
        xml.push_str("<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n");

        // Hibernate mapping root element
        xml.push_str("<!DOCTYPE hibernate-mapping PUBLIC\n");
        xml.push_str("    \"-//Hibernate/Hibernate Mapping DTD 3.0//EN\"\n");
        xml.push_str("    \"http://www.hibernate.org/dtd/hibernate-mapping-3.0.dtd\">\n");

        xml.push_str("<hibernate-mapping");
        if let Some(package) = &self.entity.package.split('.').next() {
            if !package.is_empty() {
                xml.push_str(&format!(" package=\"{}\"", self.entity.package));
            }
        }
        xml.push_str(">\n");

        // Class element
        xml.push_str("  <class name=\"");
        xml.push_str(&self.entity.entity_name);
        xml.push_str("\" table=\"");
        xml.push_str(&self.entity.table_name);
        xml.push_str("\"");

        if let Some(catalog) = &self.entity.catalog {
            xml.push_str(&format!(" catalog=\"{}\"", catalog));
        }
        if let Some(schema) = &self.entity.schema {
            xml.push_str(&format!(" schema=\"{}\"", schema));
        }

        xml.push_str(">\n");

        // ID element (primary key)
        xml.push_str("    <id name=\"");
        xml.push_str(&self.entity.id_property);
        xml.push_str("\"");

        // Find ID column info
        if let Some(id_prop) = self
            .entity
            .properties
            .iter()
            .find(|p| p.property_name == self.entity.id_property)
        {
            xml.push_str(&format!(" column=\"{}\"", id_prop.column_name));
            xml.push_str(&format!(" type=\"{}\"", id_prop.hibernate_type));
        }
        xml.push_str(">\n");

        // Generator element
        xml.push_str("      <generator class=\"");
        xml.push_str(&self.entity.id_generator.to_string());
        xml.push_str("\"/>\n");

        xml.push_str("    </id>\n");

        // Version element (if optimistic locking)
        if self.entity.has_version {
            if let Some(version_prop) = &self.entity.version_property {
                if let Some(version_mapping) = self
                    .entity
                    .properties
                    .iter()
                    .find(|p| &p.property_name == version_prop)
                {
                    xml.push_str("    <version name=\"");
                    xml.push_str(version_prop);
                    xml.push_str("\" column=\"");
                    xml.push_str(&version_mapping.column_name);
                    xml.push_str("\" type=\"");
                    xml.push_str(&version_mapping.hibernate_type);
                    xml.push_str("\"/>\n");
                }
            }
        }

        // Property elements (non-ID fields)
        for prop in &self.entity.properties {
            if prop.property_name == self.entity.id_property {
                continue; // Skip ID, already handled
            }
            if self.entity.has_version
                && Some(&prop.property_name) == self.entity.version_property.as_ref()
            {
                continue; // Skip version, already handled
            }

            xml.push_str("    <property name=\"");
            xml.push_str(&prop.property_name);
            xml.push_str("\" column=\"");
            xml.push_str(&prop.column_name);
            xml.push_str("\" type=\"");
            xml.push_str(&prop.hibernate_type);
            xml.push_str("\"");

            if prop.nullable {
                xml.push_str(" not-null=\"false\"");
            } else {
                xml.push_str(" not-null=\"true\"");
            }

            if prop.unique {
                xml.push_str(" unique=\"true\"");
            }

            xml.push_str("/>\n");
        }

        xml.push_str("  </class>\n");
        xml.push_str("</hibernate-mapping>\n");

        Ok(xml)
    }
}

/// Create an HBM mapping generation rule.
///
/// Returns a fully configured Rule that executes the SPARQL query
/// and renders HBM XML mapping files from YAWL entity definitions.
pub fn create_hbm_mapping_rule() -> ggen_codegen::Result<Rule<HbmMappingQueryExecutor, HbmMappingTemplateRenderer>> {
    let query = HbmMappingQueryExecutor::new();
    let template = HbmMappingTemplateRenderer::new();

    let rule = Rule::new(
        "hbm-mappings",
        query,
        template,
        PathBuf::from("src/main/resources/hibernate/{{ className }}.hbm.xml"),
        GenerationMode::Overwrite,
    );

    Ok(rule)
}

/// Rule 9: Hibernate HBM Mappings
#[derive(Debug, Clone, Default)]
pub struct HbmMappingRule;

impl HbmMappingRule {
    /// Create a new HBM mapping rule
    pub fn new() -> Self {
        Self
    }

    /// Generate HBM mapping from entity metadata
    pub fn generate(&self, entity: HbmMappingQuery) -> Result<String> {
        let template = HbmMappingTemplate::new(entity);
        template.render()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_id_generator_strategy_display() {
        assert_eq!(IdGeneratorStrategy::Identity.to_string(), "identity");
        assert_eq!(IdGeneratorStrategy::Sequence.to_string(), "sequence");
        assert_eq!(IdGeneratorStrategy::Uuid.to_string(), "uuid");
        assert_eq!(IdGeneratorStrategy::Assigned.to_string(), "assigned");
    }

    #[test]
    fn test_property_mapping_creation() {
        let prop = PropertyMapping {
            property_name: "username".to_string(),
            column_name: "user_name".to_string(),
            hibernate_type: "string".to_string(),
            sql_type: "VARCHAR(255)".to_string(),
            nullable: false,
            unique: true,
            foreign_key: false,
            foreign_key_target: None,
        };

        assert_eq!(prop.property_name, "username");
        assert_eq!(prop.column_name, "user_name");
        assert!(!prop.nullable);
        assert!(prop.unique);
    }

    #[test]
    fn test_hbm_mapping_query_creation() {
        let query = HbmMappingQuery {
            entity_name: "User".to_string(),
            table_name: "users".to_string(),
            properties: vec![
                PropertyMapping {
                    property_name: "id".to_string(),
                    column_name: "user_id".to_string(),
                    hibernate_type: "long".to_string(),
                    sql_type: "BIGINT".to_string(),
                    nullable: false,
                    unique: true,
                    foreign_key: false,
                    foreign_key_target: None,
                },
                PropertyMapping {
                    property_name: "username".to_string(),
                    column_name: "user_name".to_string(),
                    hibernate_type: "string".to_string(),
                    sql_type: "VARCHAR(255)".to_string(),
                    nullable: false,
                    unique: true,
                    foreign_key: false,
                    foreign_key_target: None,
                },
                PropertyMapping {
                    property_name: "email".to_string(),
                    column_name: "email_addr".to_string(),
                    hibernate_type: "string".to_string(),
                    sql_type: "VARCHAR(255)".to_string(),
                    nullable: false,
                    unique: true,
                    foreign_key: false,
                    foreign_key_target: None,
                },
            ],
            id_property: "id".to_string(),
            id_generator: IdGeneratorStrategy::Identity,
            has_version: false,
            version_property: None,
            package: "com.example.model".to_string(),
            catalog: None,
            schema: None,
        };

        assert_eq!(query.entity_name, "User");
        assert_eq!(query.table_name, "users");
        assert_eq!(query.properties.len(), 3);
    }

    #[test]
    fn test_hbm_mapping_template_render_basic() {
        let entity = HbmMappingQuery {
            entity_name: "User".to_string(),
            table_name: "users".to_string(),
            properties: vec![
                PropertyMapping {
                    property_name: "id".to_string(),
                    column_name: "user_id".to_string(),
                    hibernate_type: "long".to_string(),
                    sql_type: "BIGINT".to_string(),
                    nullable: false,
                    unique: true,
                    foreign_key: false,
                    foreign_key_target: None,
                },
                PropertyMapping {
                    property_name: "username".to_string(),
                    column_name: "user_name".to_string(),
                    hibernate_type: "string".to_string(),
                    sql_type: "VARCHAR(255)".to_string(),
                    nullable: false,
                    unique: true,
                    foreign_key: false,
                    foreign_key_target: None,
                },
            ],
            id_property: "id".to_string(),
            id_generator: IdGeneratorStrategy::Identity,
            has_version: false,
            version_property: None,
            package: "com.example.model".to_string(),
            catalog: None,
            schema: None,
        };

        let template = HbmMappingTemplate::new(entity);
        let xml = template.render().expect("Should render HBM XML");

        // Verify XML structure
        assert!(xml.contains("<?xml version=\"1.0\" encoding=\"UTF-8\"?>"));
        assert!(xml.contains("<hibernate-mapping"));
        assert!(xml.contains("<class name=\"User\" table=\"users\""));
        assert!(xml.contains("<id name=\"id\""));
        assert!(xml.contains("<generator class=\"identity\"/>"));
        assert!(xml.contains("<property name=\"username\""));
        assert!(xml.contains("not-null=\"true\""));
        assert!(xml.contains("unique=\"true\""));
        assert!(xml.contains("</hibernate-mapping>"));
    }

    #[test]
    fn test_hbm_mapping_with_schema_catalog() {
        let entity = HbmMappingQuery {
            entity_name: "Order".to_string(),
            table_name: "orders".to_string(),
            properties: vec![PropertyMapping {
                property_name: "id".to_string(),
                column_name: "order_id".to_string(),
                hibernate_type: "long".to_string(),
                sql_type: "BIGINT".to_string(),
                nullable: false,
                unique: true,
                foreign_key: false,
                foreign_key_target: None,
            }],
            id_property: "id".to_string(),
            id_generator: IdGeneratorStrategy::Uuid,
            has_version: false,
            version_property: None,
            package: "com.example.model".to_string(),
            catalog: Some("ecommerce_db".to_string()),
            schema: Some("public".to_string()),
        };

        let template = HbmMappingTemplate::new(entity);
        let xml = template.render().expect("Should render HBM XML");

        assert!(xml.contains("catalog=\"ecommerce_db\""));
        assert!(xml.contains("schema=\"public\""));
        assert!(xml.contains("<generator class=\"uuid\"/>"));
    }

    #[test]
    fn test_hbm_mapping_with_version() {
        let entity = HbmMappingQuery {
            entity_name: "Product".to_string(),
            table_name: "products".to_string(),
            properties: vec![
                PropertyMapping {
                    property_name: "id".to_string(),
                    column_name: "product_id".to_string(),
                    hibernate_type: "long".to_string(),
                    sql_type: "BIGINT".to_string(),
                    nullable: false,
                    unique: true,
                    foreign_key: false,
                    foreign_key_target: None,
                },
                PropertyMapping {
                    property_name: "version".to_string(),
                    column_name: "version_num".to_string(),
                    hibernate_type: "integer".to_string(),
                    sql_type: "INT".to_string(),
                    nullable: false,
                    unique: false,
                    foreign_key: false,
                    foreign_key_target: None,
                },
                PropertyMapping {
                    property_name: "name".to_string(),
                    column_name: "product_name".to_string(),
                    hibernate_type: "string".to_string(),
                    sql_type: "VARCHAR(255)".to_string(),
                    nullable: false,
                    unique: false,
                    foreign_key: false,
                    foreign_key_target: None,
                },
            ],
            id_property: "id".to_string(),
            id_generator: IdGeneratorStrategy::Identity,
            has_version: true,
            version_property: Some("version".to_string()),
            package: "com.example.model".to_string(),
            catalog: None,
            schema: None,
        };

        let template = HbmMappingTemplate::new(entity);
        let xml = template.render().expect("Should render HBM XML");

        assert!(xml.contains("<version name=\"version\""));
        assert!(xml.contains("column=\"version_num\""));
        assert!(xml.contains("type=\"integer\""));
        assert!(xml.contains("<property name=\"name\""));
    }

    #[test]
    fn test_hbm_mapping_rule_generate() {
        let rule = HbmMappingRule::new();
        let entity = HbmMappingQuery {
            entity_name: "Account".to_string(),
            table_name: "accounts".to_string(),
            properties: vec![PropertyMapping {
                property_name: "id".to_string(),
                column_name: "account_id".to_string(),
                hibernate_type: "string".to_string(),
                sql_type: "VARCHAR(36)".to_string(),
                nullable: false,
                unique: true,
                foreign_key: false,
                foreign_key_target: None,
            }],
            id_property: "id".to_string(),
            id_generator: IdGeneratorStrategy::Uuid,
            has_version: false,
            version_property: None,
            package: "com.example.domain".to_string(),
            catalog: None,
            schema: None,
        };

        let xml = rule.generate(entity).expect("Should generate HBM");
        assert!(xml.contains("Account"));
        assert!(xml.contains("accounts"));
    }

    #[test]
    fn test_create_hbm_mapping_rule_factory() {
        let rule = create_hbm_mapping_rule().expect("Rule should create");
        assert_eq!(rule.name(), "hbm-mappings");
    }

    #[test]
    fn test_hbm_mapping_query_executor() {
        let query = HbmMappingQueryExecutor::new();
        assert_eq!(query.name(), "hbm-mapping-query");
        assert!(query.source().is_some());

        let results = query.execute().expect("Query should execute");
        assert!(results.len() >= 2, "Should have at least 2 entities");

        // Verify YWorkItem
        let workitem = results
            .iter()
            .find(|r| r.get("className") == Some(&"YWorkItem".to_string()))
            .expect("Should have YWorkItem");

        assert_eq!(workitem.get("tableName"), Some(&"y_work_item".to_string()));
        assert!(workitem.contains_key("properties"));
    }

    #[test]
    fn test_hbm_mapping_template_renderer() {
        let template = HbmMappingTemplateRenderer::new();
        assert_eq!(template.name(), "hbm-mapping-template");

        let mut bindings = HashMap::new();
        bindings.insert("className".to_string(), "YTask".to_string());
        bindings.insert("tableName".to_string(), "y_task".to_string());
        bindings.insert("idProperty".to_string(), "id".to_string());
        bindings.insert("idGenerator".to_string(), "uuid".to_string());
        bindings.insert("package".to_string(), "org.yawlfoundation.yawl.elements".to_string());
        bindings.insert(
            "properties".to_string(),
            r#"[{"propertyName": "name", "columnName": "name", "hibernateType": "string"}]"#
                .to_string(),
        );

        let output = template.render(&bindings).expect("Should render");
        assert!(output.contains("<?xml version"));
        assert!(output.contains("<hibernate-mapping"));
        assert!(output.contains("YTask"));
        assert!(output.contains("y_task"));
    }

    #[test]
    fn test_hbm_mapping_rule_execution() {
        let rule = create_hbm_mapping_rule().expect("Rule should create");
        assert_eq!(rule.name(), "hbm-mappings");

        let files = rule.execute().expect("Rule should execute");
        assert!(!files.is_empty(), "Should generate HBM mapping files");

        // Verify generated file has correct extension
        for file in &files {
            assert!(
                file.path.to_string_lossy().contains(".hbm.xml"),
                "Output should be HBM XML files"
            );
            assert!(file.content.contains("<?xml version"));
            assert!(file.source_rule == "hbm-mappings");
        }
    }

    #[test]
    fn test_nullable_properties_in_hbm() {
        let entity = HbmMappingQuery {
            entity_name: "OptionalData".to_string(),
            table_name: "optional_data".to_string(),
            properties: vec![
                PropertyMapping {
                    property_name: "id".to_string(),
                    column_name: "id".to_string(),
                    hibernate_type: "long".to_string(),
                    sql_type: "BIGINT".to_string(),
                    nullable: false,
                    unique: true,
                    foreign_key: false,
                    foreign_key_target: None,
                },
                PropertyMapping {
                    property_name: "description".to_string(),
                    column_name: "description".to_string(),
                    hibernate_type: "string".to_string(),
                    sql_type: "TEXT".to_string(),
                    nullable: true,
                    unique: false,
                    foreign_key: false,
                    foreign_key_target: None,
                },
            ],
            id_property: "id".to_string(),
            id_generator: IdGeneratorStrategy::Identity,
            has_version: false,
            version_property: None,
            package: "com.example".to_string(),
            catalog: None,
            schema: None,
        };

        let template = HbmMappingTemplate::new(entity);
        let xml = template.render().expect("Should render");

        // Description should have not-null="false"
        assert!(xml.contains("name=\"description\"") && xml.contains("not-null=\"false\""));
    }
}
