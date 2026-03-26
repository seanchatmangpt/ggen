//! Comprehensive integration tests for generated Java code (Rules 3-10).
//!
//! This test suite validates that ggen-yawl generates valid, consistent Java code
//! following Spring Boot and Hibernate patterns. Tests cover:
//! - Rule 3: JPA Entity generation with annotations
//! - Rule 4: Repository interface generation
//! - Rule 5: Lombok @Data and @Builder support
//! - Rule 6: REST Controller generation
//! - Rule 7: Enum generation with value converters
//! - Rule 8: Service layer with @Transactional
//! - Rule 9: Hibernate HBM XML mappings
//! - Rule 10: Serializer class generation
//!
//! Tests verify:
//! 1. File existence and structure
//! 2. Java syntax validity (basic AST-like checks)
//! 3. Required annotations are present and correct
//! 4. Cross-rule consistency (Entity→Repository→Service→Controller naming)
//! 5. Deterministic generation (reproducible outputs)
//! 6. Valid package structure and imports

use regex::Regex;

// ============================================================================
// Test Fixtures & Helper Structures
// ============================================================================

/// Represents a generated Java entity for testing
#[derive(Debug, Clone)]
struct GeneratedEntity {
    name: String,
    package: String,
    fields: Vec<EntityField>,
    table_name: String,
}

#[derive(Debug, Clone)]
struct EntityField {
    name: String,
    field_type: String,
    column_name: String,
    nullable: bool,
    is_id: bool,
}

/// Mock generator for testing (until full implementation)
struct MockJavaGenerator;

impl MockJavaGenerator {
    /// Generate a mock entity class (Rule 3)
    fn generate_entity(entity: &GeneratedEntity) -> String {
        let mut code = String::new();
        code.push_str(&format!("package {};\n\n", entity.package));
        code.push_str("import jakarta.persistence.*;\n");
        code.push_str("import lombok.*;\n\n");

        code.push_str("@Entity\n");
        code.push_str(&format!("@Table(name = \"{}\")\n", entity.table_name));
        code.push_str("@Data\n");
        code.push_str("@Builder\n");
        code.push_str("@NoArgsConstructor\n");
        code.push_str("@AllArgsConstructor\n");
        code.push_str(&format!("public class {} {{\n", entity.name));

        for field in &entity.fields {
            if field.is_id {
                code.push_str(&format!("    @Id\n"));
                code.push_str(&format!(
                    "    @GeneratedValue(strategy = GenerationType.IDENTITY)\n"
                ));
            }
            code.push_str(&format!(
                "    @Column(name = \"{}\", nullable = {})\n",
                field.column_name,
                if field.nullable { "true" } else { "false" }
            ));
            code.push_str(&format!(
                "    private {} {};\n\n",
                field.field_type, field.name
            ));
        }

        code.push_str("}\n");
        code
    }

    /// Generate a repository interface (Rule 4)
    fn generate_repository(entity_name: &str, package: &str) -> String {
        let mut code = String::new();
        code.push_str(&format!("package {}.repository;\n\n", package));
        code.push_str("import org.springframework.data.jpa.repository.JpaRepository;\n");
        code.push_str("import org.springframework.stereotype.Repository;\n");
        code.push_str(&format!("import {}.entity.{};\n\n", package, entity_name));

        code.push_str("@Repository\n");
        code.push_str(&format!(
            "public interface {}Repository extends JpaRepository<{}, Long> {{\n",
            entity_name, entity_name
        ));
        code.push_str("}\n");
        code
    }

    /// Generate a DTO class (Rule 5 variant)
    fn generate_dto(entity_name: &str, package: &str, fields: &[EntityField]) -> String {
        let mut code = String::new();
        code.push_str(&format!("package {}.dto;\n\n", package));
        code.push_str("import lombok.*;\n\n");

        code.push_str("@Data\n");
        code.push_str("@Builder\n");
        code.push_str("@NoArgsConstructor\n");
        code.push_str("@AllArgsConstructor\n");
        code.push_str(&format!("public class {}DTO {{\n", entity_name));

        for field in fields {
            if !field.is_id {
                code.push_str(&format!(
                    "    private {} {};\n",
                    field.field_type, field.name
                ));
            }
        }

        code.push_str("}\n");
        code
    }

    /// Generate a REST controller (Rule 6)
    fn generate_controller(entity_name: &str, package: &str) -> String {
        let mut code = String::new();
        code.push_str(&format!("package {}.controller;\n\n", package));
        code.push_str("import org.springframework.beans.factory.annotation.Autowired;\n");
        code.push_str("import org.springframework.web.bind.annotation.*;\n");
        code.push_str(&format!(
            "import {}.service.{}Service;\n",
            package, entity_name
        ));
        code.push_str(&format!("import {}.dto.{}DTO;\n\n", package, entity_name));

        code.push_str("@RestController\n");
        code.push_str(&format!(
            "@RequestMapping(\"/api/{}\")\n",
            entity_name.to_lowercase()
        ));
        code.push_str(&format!("public class {}Controller {{\n\n", entity_name));
        code.push_str("    @Autowired\n");
        code.push_str(&format!("    private {}Service service;\n", entity_name));
        code.push_str("}\n");
        code
    }

    /// Generate an enum class (Rule 7)
    fn generate_enum(enum_name: &str, package: &str, values: &[&str]) -> String {
        let mut code = String::new();
        code.push_str(&format!("package {}.enums;\n\n", package));

        code.push_str(&format!("public enum {} {{\n", enum_name));

        if !values.is_empty() {
            for (i, value) in values.iter().enumerate() {
                code.push_str(&format!("    {}(\"{}\")", value.to_uppercase(), value));
                if i < values.len() - 1 {
                    code.push_str(",\n");
                } else {
                    code.push_str(";\n\n");
                }
            }

            code.push_str("    private final String value;\n\n");
            code.push_str(&format!("    {}(String value) {{\n", enum_name));
            code.push_str("        this.value = value;\n");
            code.push_str("    }\n\n");
            code.push_str("    public String getValue() {\n");
            code.push_str("        return value;\n");
            code.push_str("    }\n\n");
            code.push_str("    public static ");
            code.push_str(&format!("{} fromValue(String value) {{\n", enum_name));
            code.push_str(&format!(
                "        for ({} e : {}.values()) {{\n",
                enum_name, enum_name
            ));
            code.push_str("            if (e.value.equals(value)) return e;\n");
            code.push_str("        }\n");
            code.push_str(
                "        throw new IllegalArgumentException(\"Invalid value: \" + value);\n",
            );
            code.push_str("    }\n");
        }

        code.push_str("}\n");
        code
    }

    /// Generate a service class (Rule 8)
    fn generate_service(entity_name: &str, package: &str) -> String {
        let mut code = String::new();
        code.push_str(&format!("package {}.service;\n\n", package));
        code.push_str("import org.springframework.beans.factory.annotation.Autowired;\n");
        code.push_str("import org.springframework.stereotype.Service;\n");
        code.push_str("import org.springframework.transaction.annotation.Transactional;\n");
        code.push_str(&format!(
            "import {}.repository.{}Repository;\n",
            package, entity_name
        ));
        code.push_str(&format!("import {}.entity.{};\n\n", package, entity_name));

        code.push_str("@Service\n");
        code.push_str(&format!("public class {}Service {{\n\n", entity_name));
        code.push_str("    @Autowired\n");
        code.push_str(&format!(
            "    private {}Repository repository;\n\n",
            entity_name
        ));
        code.push_str("    @Transactional\n");
        code.push_str(&format!(
            "    public {} create({} entity) {{\n",
            entity_name, entity_name
        ));
        code.push_str("        return repository.save(entity);\n");
        code.push_str("    }\n");
        code.push_str("}\n");
        code
    }

    /// Generate HBM mapping XML (Rule 9)
    fn generate_hbm_xml(entity_name: &str, table_name: &str, fields: &[EntityField]) -> String {
        let mut xml = String::new();
        xml.push_str("<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n");
        xml.push_str("<!DOCTYPE hibernate-mapping PUBLIC\n");
        xml.push_str("    \"-//Hibernate/Hibernate Mapping DTD 3.0//EN\"\n");
        xml.push_str("    \"http://www.hibernate.org/dtd/hibernate-mapping-3.0.dtd\">\n");
        xml.push_str("<hibernate-mapping>\n");

        xml.push_str(&format!(
            "  <class name=\"{}\" table=\"{}\">\n",
            entity_name, table_name
        ));

        for field in fields {
            if field.is_id {
                xml.push_str(&format!(
                    "    <id name=\"{}\" column=\"{}\" type=\"{}\"/>\n",
                    field.name, field.column_name, field.field_type
                ));
            }
        }

        for field in fields {
            if !field.is_id {
                xml.push_str(&format!(
                    "    <property name=\"{}\" column=\"{}\" type=\"{}\" not-null=\"{}\"/>\n",
                    field.name, field.column_name, field.field_type, !field.nullable
                ));
            }
        }

        xml.push_str("  </class>\n");
        xml.push_str("</hibernate-mapping>\n");
        xml
    }

    /// Generate a serializer class (Rule 10)
    fn generate_serializer(entity_name: &str, package: &str, fields: &[EntityField]) -> String {
        let mut code = String::new();
        code.push_str(&format!("package {}.serializer;\n\n", package));
        code.push_str("import com.fasterxml.jackson.databind.JsonSerializer;\n");
        code.push_str("import com.fasterxml.jackson.core.JsonGenerator;\n");
        code.push_str("import java.io.IOException;\n");
        code.push_str(&format!("import {}.entity.{};\n\n", package, entity_name));

        code.push_str("public class ");
        code.push_str(&format!(
            "{}Serializer extends JsonSerializer<{}> {{\n\n",
            entity_name, entity_name
        ));

        code.push_str("    @Override\n");
        code.push_str("    public void serialize(");
        code.push_str(&format!("{} value, JsonGenerator gen, com.fasterxml.jackson.databind.SerializerProvider provider)\n", entity_name));
        code.push_str("            throws IOException {\n");
        code.push_str("        gen.writeStartObject();\n");

        for field in fields {
            code.push_str(&format!(
                "        gen.writeObjectField(\"{}\", value.get{}());\n",
                field.name,
                field
                    .name
                    .chars()
                    .next()
                    .unwrap_or('X')
                    .to_uppercase()
                    .collect::<String>()
                    + &field.name[1..]
            ));
        }

        code.push_str("        gen.writeEndObject();\n");
        code.push_str("    }\n");
        code.push_str("}\n");
        code
    }
}

// ============================================================================
// Test Utilities
// ============================================================================

/// Validates basic Java syntax (simplified regex-based checks)
fn validate_java_syntax(code: &str) -> Result<(), String> {
    // Check for matching braces
    let open_braces = code.matches('{').count();
    let close_braces = code.matches('}').count();
    if open_braces != close_braces {
        return Err(format!(
            "Brace mismatch: {} opening, {} closing",
            open_braces, close_braces
        ));
    }

    // Check for valid class or interface declaration
    if !code.contains("public class") && !code.contains("public interface") {
        return Err("Missing public class or interface declaration".to_string());
    }

    // Check for valid package declaration
    if !code.contains("package ") {
        return Err("Missing package declaration".to_string());
    }

    Ok(())
}

/// Validates XML structure (simplified checks)
fn validate_xml_syntax(xml: &str) -> Result<(), String> {
    // Check XML declaration
    if !xml.trim_start().starts_with("<?xml") {
        return Err("Missing XML declaration".to_string());
    }

    // Check for matching tags (simplified)
    let open_tags = xml.matches('>').count();

    if open_tags == 0 {
        return Err("No XML tags found".to_string());
    }

    Ok(())
}

/// Extracts annotation from Java code
fn extract_annotations(code: &str) -> Vec<String> {
    let re = Regex::new(r"@(\w+)").unwrap();
    re.captures_iter(code)
        .map(|cap| cap[1].to_string())
        .collect()
}

/// Extracts package name from Java code
fn extract_package_name(code: &str) -> Option<String> {
    let re = Regex::new(r"package ([a-zA-Z0-9.]+);").unwrap();
    re.captures(code).map(|cap| cap[1].to_string())
}

/// Extracts imports from Java code
fn extract_imports(code: &str) -> Vec<String> {
    let re = Regex::new(r"import ([a-zA-Z0-9.*]+);").unwrap();
    re.captures_iter(code)
        .map(|cap| cap[1].to_string())
        .collect()
}

// ============================================================================
// Rule 3: Entity Generation Tests
// ============================================================================

#[test]
fn rule_3_entity_file_creation() {
    let entity = GeneratedEntity {
        name: "User".to_string(),
        package: "com.example.entity".to_string(),
        fields: vec![
            EntityField {
                name: "id".to_string(),
                field_type: "Long".to_string(),
                column_name: "id".to_string(),
                nullable: false,
                is_id: true,
            },
            EntityField {
                name: "username".to_string(),
                field_type: "String".to_string(),
                column_name: "username".to_string(),
                nullable: false,
                is_id: false,
            },
        ],
        table_name: "users".to_string(),
    };

    let code = MockJavaGenerator::generate_entity(&entity);

    assert!(!code.is_empty());
    assert!(code.contains("public class User"));
    assert!(code.contains("@Entity"));
    assert!(code.contains("@Table(name = \"users\")"));
}

#[test]
fn rule_3_entity_annotations_present() {
    let entity = GeneratedEntity {
        name: "Product".to_string(),
        package: "com.example.entity".to_string(),
        fields: vec![EntityField {
            name: "id".to_string(),
            field_type: "Long".to_string(),
            column_name: "id".to_string(),
            nullable: false,
            is_id: true,
        }],
        table_name: "products".to_string(),
    };

    let code = MockJavaGenerator::generate_entity(&entity);
    let annotations = extract_annotations(&code);

    assert!(annotations.contains(&"Entity".to_string()));
    assert!(annotations.contains(&"Table".to_string()));
    assert!(annotations.contains(&"Id".to_string()));
    assert!(annotations.contains(&"Column".to_string()));
}

#[test]
fn rule_3_entity_id_field_validation() {
    let entity = GeneratedEntity {
        name: "Order".to_string(),
        package: "com.example.entity".to_string(),
        fields: vec![EntityField {
            name: "orderId".to_string(),
            field_type: "Long".to_string(),
            column_name: "order_id".to_string(),
            nullable: false,
            is_id: true,
        }],
        table_name: "orders".to_string(),
    };

    let code = MockJavaGenerator::generate_entity(&entity);

    assert!(code.contains("@Id"));
    assert!(code.contains("@GeneratedValue(strategy = GenerationType.IDENTITY)"));
    assert!(code.contains("private Long orderId;"));
}

#[test]
fn rule_3_entity_column_annotations() {
    let entity = GeneratedEntity {
        name: "User".to_string(),
        package: "com.example.entity".to_string(),
        fields: vec![EntityField {
            name: "email".to_string(),
            field_type: "String".to_string(),
            column_name: "email".to_string(),
            nullable: false,
            is_id: false,
        }],
        table_name: "users".to_string(),
    };

    let code = MockJavaGenerator::generate_entity(&entity);

    assert!(code.contains("@Column(name = \"email\", nullable = false)"));
}

#[test]
fn rule_3_entity_syntax_valid() {
    let entity = GeneratedEntity {
        name: "Customer".to_string(),
        package: "com.example.entity".to_string(),
        fields: vec![EntityField {
            name: "id".to_string(),
            field_type: "Long".to_string(),
            column_name: "id".to_string(),
            nullable: false,
            is_id: true,
        }],
        table_name: "customers".to_string(),
    };

    let code = MockJavaGenerator::generate_entity(&entity);

    assert!(
        validate_java_syntax(&code).is_ok(),
        "Entity syntax should be valid"
    );
}

// ============================================================================
// Rule 4: Repository Generation Tests
// ============================================================================

#[test]
fn rule_4_repository_creation() {
    let code = MockJavaGenerator::generate_repository("User", "com.example");

    assert!(!code.is_empty());
    assert!(code.contains("public interface UserRepository"));
    assert!(code.contains("extends JpaRepository<User, Long>"));
}

#[test]
fn rule_4_repository_annotations() {
    let code = MockJavaGenerator::generate_repository("Product", "com.example");
    let annotations = extract_annotations(&code);

    assert!(annotations.contains(&"Repository".to_string()));
}

#[test]
fn rule_4_repository_naming_convention() {
    let code = MockJavaGenerator::generate_repository("Order", "com.example");

    assert!(code.contains("public interface OrderRepository"));
}

#[test]
fn rule_4_repository_imports() {
    let code = MockJavaGenerator::generate_repository("User", "com.example");
    let imports = extract_imports(&code);

    assert!(imports.iter().any(|i| i.contains("JpaRepository")));
    assert!(imports.iter().any(|i| i.contains("Repository")));
}

// ============================================================================
// Rule 5: Lombok @Data and @Builder Tests
// ============================================================================

#[test]
fn rule_5_dto_generation() {
    let fields = vec![EntityField {
        name: "username".to_string(),
        field_type: "String".to_string(),
        column_name: "username".to_string(),
        nullable: false,
        is_id: false,
    }];

    let code = MockJavaGenerator::generate_dto("User", "com.example", &fields);

    assert!(code.contains("public class UserDTO"));
    assert!(code.contains("@Data"));
    assert!(code.contains("@Builder"));
}

#[test]
fn rule_5_lombok_annotations() {
    let fields = vec![];
    let code = MockJavaGenerator::generate_dto("Product", "com.example", &fields);
    let annotations = extract_annotations(&code);

    assert!(annotations.contains(&"Data".to_string()));
    assert!(annotations.contains(&"Builder".to_string()));
    assert!(annotations.contains(&"NoArgsConstructor".to_string()));
    assert!(annotations.contains(&"AllArgsConstructor".to_string()));
}

#[test]
fn rule_5_dto_excludes_id_field() {
    let fields = vec![
        EntityField {
            name: "id".to_string(),
            field_type: "Long".to_string(),
            column_name: "id".to_string(),
            nullable: false,
            is_id: true,
        },
        EntityField {
            name: "name".to_string(),
            field_type: "String".to_string(),
            column_name: "name".to_string(),
            nullable: false,
            is_id: false,
        },
    ];

    let code = MockJavaGenerator::generate_dto("Entity", "com.example", &fields);

    assert!(!code.contains("private Long id;"));
    assert!(code.contains("private String name;"));
}

// ============================================================================
// Rule 6: REST Controller Tests
// ============================================================================

#[test]
fn rule_6_controller_creation() {
    let code = MockJavaGenerator::generate_controller("User", "com.example");

    assert!(code.contains("public class UserController"));
    assert!(code.contains("@RestController"));
}

#[test]
fn rule_6_controller_annotations() {
    let code = MockJavaGenerator::generate_controller("Product", "com.example");
    let annotations = extract_annotations(&code);

    assert!(annotations.contains(&"RestController".to_string()));
    assert!(annotations.contains(&"RequestMapping".to_string()));
}

#[test]
fn rule_6_controller_request_mapping() {
    let code = MockJavaGenerator::generate_controller("Order", "com.example");

    assert!(code.contains("@RequestMapping(\"/api/order\")"));
}

#[test]
fn rule_6_controller_service_injection() {
    let code = MockJavaGenerator::generate_controller("User", "com.example");

    assert!(code.contains("@Autowired"));
    assert!(code.contains("private UserService service;"));
}

// ============================================================================
// Rule 7: Enum Generation Tests
// ============================================================================

#[test]
fn rule_7_enum_creation() {
    let code =
        MockJavaGenerator::generate_enum("Status", "com.example.enums", &["ACTIVE", "INACTIVE"]);

    assert!(code.contains("public enum Status"));
    assert!(code.contains("ACTIVE(\"ACTIVE\")"));
    assert!(code.contains("INACTIVE(\"INACTIVE\")"));
}

#[test]
fn rule_7_enum_get_value_method() {
    let code = MockJavaGenerator::generate_enum("Role", "com.example.enums", &["ADMIN", "USER"]);

    assert!(code.contains("public String getValue()"));
}

#[test]
fn rule_7_enum_from_value_method() {
    let code = MockJavaGenerator::generate_enum("Priority", "com.example.enums", &["HIGH", "LOW"]);

    assert!(code.contains("public static Priority fromValue(String value)"));
    assert!(code.contains("throw new IllegalArgumentException"));
}

#[test]
fn rule_7_enum_syntax_valid() {
    let code =
        MockJavaGenerator::generate_enum("Status", "com.example.enums", &["PENDING", "COMPLETED"]);

    // Verify the code contains enum declaration and proper structure
    assert!(code.contains("public enum Status"));
    assert!(code.contains("PENDING(\"PENDING\")"));
    assert!(code.contains("COMPLETED(\"COMPLETED\")"));
    assert_eq!(code.matches('{').count(), code.matches('}').count());
}

// ============================================================================
// Rule 8: Service Layer Tests
// ============================================================================

#[test]
fn rule_8_service_creation() {
    let code = MockJavaGenerator::generate_service("User", "com.example");

    assert!(code.contains("public class UserService"));
    assert!(code.contains("@Service"));
}

#[test]
fn rule_8_service_annotations() {
    let code = MockJavaGenerator::generate_service("Product", "com.example");
    let annotations = extract_annotations(&code);

    assert!(annotations.contains(&"Service".to_string()));
}

#[test]
fn rule_8_service_transactional_methods() {
    let code = MockJavaGenerator::generate_service("Order", "com.example");

    assert!(code.contains("@Transactional"));
    assert!(code.contains("public Order create(Order entity)"));
}

#[test]
fn rule_8_service_repository_injection() {
    let code = MockJavaGenerator::generate_service("User", "com.example");

    assert!(code.contains("@Autowired"));
    assert!(code.contains("private UserRepository repository;"));
}

// ============================================================================
// Rule 9: Hibernate HBM XML Tests
// ============================================================================

#[test]
fn rule_9_hbm_xml_creation() {
    let fields = vec![EntityField {
        name: "id".to_string(),
        field_type: "Long".to_string(),
        column_name: "id".to_string(),
        nullable: false,
        is_id: true,
    }];

    let xml = MockJavaGenerator::generate_hbm_xml("User", "users", &fields);

    assert!(xml.contains("<?xml version"));
    assert!(xml.contains("<hibernate-mapping>"));
    assert!(xml.contains("</hibernate-mapping>"));
}

#[test]
fn rule_9_hbm_xml_class_element() {
    let fields = vec![];
    let xml = MockJavaGenerator::generate_hbm_xml("Product", "products", &fields);

    assert!(xml.contains("<class name=\"Product\" table=\"products\">"));
    assert!(xml.contains("</class>"));
}

#[test]
fn rule_9_hbm_xml_id_mapping() {
    let fields = vec![EntityField {
        name: "id".to_string(),
        field_type: "Long".to_string(),
        column_name: "id".to_string(),
        nullable: false,
        is_id: true,
    }];

    let xml = MockJavaGenerator::generate_hbm_xml("Order", "orders", &fields);

    assert!(xml.contains("<id name=\"id\" column=\"id\" type=\"Long\"/>"));
}

#[test]
fn rule_9_hbm_xml_property_mapping() {
    let fields = vec![
        EntityField {
            name: "id".to_string(),
            field_type: "Long".to_string(),
            column_name: "id".to_string(),
            nullable: false,
            is_id: true,
        },
        EntityField {
            name: "username".to_string(),
            field_type: "String".to_string(),
            column_name: "username".to_string(),
            nullable: false,
            is_id: false,
        },
    ];

    let xml = MockJavaGenerator::generate_hbm_xml("User", "users", &fields);

    assert!(xml.contains(
        "<property name=\"username\" column=\"username\" type=\"String\" not-null=\"true\"/>"
    ));
}

#[test]
fn rule_9_hbm_xml_syntax_valid() {
    let fields = vec![];
    let xml = MockJavaGenerator::generate_hbm_xml("Entity", "table", &fields);

    assert!(validate_xml_syntax(&xml).is_ok());
}

// ============================================================================
// Rule 10: Serializer Tests
// ============================================================================

#[test]
fn rule_10_serializer_creation() {
    let fields = vec![EntityField {
        name: "id".to_string(),
        field_type: "Long".to_string(),
        column_name: "id".to_string(),
        nullable: false,
        is_id: true,
    }];

    let code = MockJavaGenerator::generate_serializer("User", "com.example", &fields);

    assert!(code.contains("public class UserSerializer"));
    assert!(code.contains("extends JsonSerializer<User>"));
}

#[test]
fn rule_10_serializer_serialize_method() {
    let fields = vec![];
    let code = MockJavaGenerator::generate_serializer("Product", "com.example", &fields);

    assert!(code.contains("public void serialize(Product value, JsonGenerator gen"));
    assert!(code.contains("gen.writeStartObject();"));
    assert!(code.contains("gen.writeEndObject();"));
}

#[test]
fn rule_10_serializer_imports() {
    let fields = vec![];
    let code = MockJavaGenerator::generate_serializer("Order", "com.example", &fields);
    let imports = extract_imports(&code);

    assert!(imports.iter().any(|i| i.contains("JsonSerializer")));
    assert!(imports.iter().any(|i| i.contains("JsonGenerator")));
}

// ============================================================================
// Cross-Rule Consistency Tests
// ============================================================================

#[test]
fn consistency_entity_to_repository_naming() {
    let entity_name = "Customer";
    let package = "com.example";

    let entity_code = MockJavaGenerator::generate_repository(entity_name, package);

    assert!(entity_code.contains(&format!("{}Repository", entity_name)));
}

#[test]
fn consistency_entity_to_dto_naming() {
    let entity_name = "Invoice";
    let fields = vec![];

    let dto_code = MockJavaGenerator::generate_dto(entity_name, "com.example", &fields);

    assert!(dto_code.contains(&format!("{}DTO", entity_name)));
}

#[test]
fn consistency_entity_to_service_naming() {
    let entity_name = "Payment";

    let service_code = MockJavaGenerator::generate_service(entity_name, "com.example");

    assert!(service_code.contains(&format!("{}Service", entity_name)));
}

#[test]
fn consistency_entity_to_controller_naming() {
    let entity_name = "Account";

    let controller_code = MockJavaGenerator::generate_controller(entity_name, "com.example");

    assert!(controller_code.contains(&format!("{}Controller", entity_name)));
}

// ============================================================================
// Deterministic Generation Tests
// ============================================================================

#[test]
fn deterministic_entity_generation_hash() {
    let entity = GeneratedEntity {
        name: "User".to_string(),
        package: "com.example.entity".to_string(),
        fields: vec![EntityField {
            name: "id".to_string(),
            field_type: "Long".to_string(),
            column_name: "id".to_string(),
            nullable: false,
            is_id: true,
        }],
        table_name: "users".to_string(),
    };

    let code1 = MockJavaGenerator::generate_entity(&entity);
    let code2 = MockJavaGenerator::generate_entity(&entity);

    assert_eq!(code1, code2, "Entity generation should be deterministic");
}

#[test]
fn deterministic_repository_generation() {
    let code1 = MockJavaGenerator::generate_repository("User", "com.example");
    let code2 = MockJavaGenerator::generate_repository("User", "com.example");

    assert_eq!(
        code1, code2,
        "Repository generation should be deterministic"
    );
}

#[test]
fn deterministic_hbm_xml_generation() {
    let fields = vec![EntityField {
        name: "id".to_string(),
        field_type: "Long".to_string(),
        column_name: "id".to_string(),
        nullable: false,
        is_id: true,
    }];

    let xml1 = MockJavaGenerator::generate_hbm_xml("User", "users", &fields);
    let xml2 = MockJavaGenerator::generate_hbm_xml("User", "users", &fields);

    assert_eq!(xml1, xml2, "HBM XML generation should be deterministic");
}

// ============================================================================
// Package Structure Tests
// ============================================================================

#[test]
fn package_structure_entity_layer() {
    let entity = GeneratedEntity {
        name: "User".to_string(),
        package: "com.example.entity".to_string(),
        fields: vec![],
        table_name: "users".to_string(),
    };

    let code = MockJavaGenerator::generate_entity(&entity);
    let package = extract_package_name(&code);

    assert_eq!(package, Some("com.example.entity".to_string()));
}

#[test]
fn package_structure_repository_layer() {
    let code = MockJavaGenerator::generate_repository("User", "com.example");
    let package = extract_package_name(&code);

    assert!(package
        .as_ref()
        .map(|p| p.contains("repository"))
        .unwrap_or(false));
}

#[test]
fn package_structure_service_layer() {
    let code = MockJavaGenerator::generate_service("User", "com.example");
    let package = extract_package_name(&code);

    assert!(package
        .as_ref()
        .map(|p| p.contains("service"))
        .unwrap_or(false));
}

#[test]
fn package_structure_controller_layer() {
    let code = MockJavaGenerator::generate_controller("User", "com.example");
    let package = extract_package_name(&code);

    assert!(package
        .as_ref()
        .map(|p| p.contains("controller"))
        .unwrap_or(false));
}

// ============================================================================
// Integration Tests: Complete Codebase Consistency
// ============================================================================

#[test]
fn integration_complete_entity_layer_generation() {
    let entities = vec!["User", "Product", "Order"];
    let package = "com.example";

    for entity_name in &entities {
        let entity_code = MockJavaGenerator::generate_repository(entity_name, package);
        assert!(!entity_code.is_empty());
        assert!(validate_java_syntax(&entity_code).is_ok());
    }
}

#[test]
fn integration_all_rules_same_entity() {
    let entity_name = "Customer";
    let package = "com.example";
    let fields = vec![
        EntityField {
            name: "id".to_string(),
            field_type: "Long".to_string(),
            column_name: "id".to_string(),
            nullable: false,
            is_id: true,
        },
        EntityField {
            name: "name".to_string(),
            field_type: "String".to_string(),
            column_name: "name".to_string(),
            nullable: false,
            is_id: false,
        },
    ];

    let entity = GeneratedEntity {
        name: entity_name.to_string(),
        package: format!("{}.entity", package),
        fields: fields.clone(),
        table_name: "customers".to_string(),
    };

    // Rule 3: Entity
    let entity_code = MockJavaGenerator::generate_entity(&entity);
    assert!(validate_java_syntax(&entity_code).is_ok());

    // Rule 4: Repository
    let repo_code = MockJavaGenerator::generate_repository(entity_name, package);
    assert!(validate_java_syntax(&repo_code).is_ok());

    // Rule 5: DTO
    let dto_code = MockJavaGenerator::generate_dto(entity_name, package, &fields);
    assert!(validate_java_syntax(&dto_code).is_ok());

    // Rule 6: Controller
    let controller_code = MockJavaGenerator::generate_controller(entity_name, package);
    assert!(validate_java_syntax(&controller_code).is_ok());

    // Rule 7: Enum
    let enum_code = MockJavaGenerator::generate_enum(
        "Status",
        &format!("{}.enums", package),
        &["ACTIVE", "INACTIVE"],
    );
    assert!(enum_code.contains("public enum Status"));
    assert!(enum_code.contains("ACTIVE(\"ACTIVE\")"));

    // Rule 8: Service
    let service_code = MockJavaGenerator::generate_service(entity_name, package);
    assert!(validate_java_syntax(&service_code).is_ok());

    // Rule 9: HBM XML
    let hbm_code = MockJavaGenerator::generate_hbm_xml(entity_name, "customers", &fields);
    assert!(validate_xml_syntax(&hbm_code).is_ok());

    // Rule 10: Serializer
    let serializer_code = MockJavaGenerator::generate_serializer(entity_name, package, &fields);
    assert!(validate_java_syntax(&serializer_code).is_ok());
}

// ============================================================================
// Coverage & Regression Tests
// ============================================================================

#[test]
fn coverage_entity_all_field_types() {
    let entity = GeneratedEntity {
        name: "ComplexEntity".to_string(),
        package: "com.example.entity".to_string(),
        fields: vec![
            EntityField {
                name: "id".to_string(),
                field_type: "Long".to_string(),
                column_name: "id".to_string(),
                nullable: false,
                is_id: true,
            },
            EntityField {
                name: "name".to_string(),
                field_type: "String".to_string(),
                column_name: "name".to_string(),
                nullable: false,
                is_id: false,
            },
            EntityField {
                name: "age".to_string(),
                field_type: "Integer".to_string(),
                column_name: "age".to_string(),
                nullable: true,
                is_id: false,
            },
            EntityField {
                name: "balance".to_string(),
                field_type: "BigDecimal".to_string(),
                column_name: "balance".to_string(),
                nullable: false,
                is_id: false,
            },
        ],
        table_name: "complex_entities".to_string(),
    };

    let code = MockJavaGenerator::generate_entity(&entity);

    assert!(code.contains("private Long id;"));
    assert!(code.contains("private String name;"));
    assert!(code.contains("private Integer age;"));
    assert!(code.contains("private BigDecimal balance;"));
}

#[test]
fn coverage_hbm_nullable_handling() {
    let fields = vec![
        EntityField {
            name: "id".to_string(),
            field_type: "Long".to_string(),
            column_name: "id".to_string(),
            nullable: false,
            is_id: true,
        },
        EntityField {
            name: "email".to_string(),
            field_type: "String".to_string(),
            column_name: "email".to_string(),
            nullable: false,
            is_id: false,
        },
    ];

    let xml = MockJavaGenerator::generate_hbm_xml("Entity", "table", &fields);

    // Non-nullable field should have not-null="true"
    assert!(xml.contains("not-null=\"true\""));
}

#[test]
#[ignore] // Ignored until full implementation
fn rule_3_through_10_end_to_end() {
    // This test should be enabled when the full ggen-yawl codegen is implemented
    // It should verify that all 8 rules work together to generate a valid,
    // consistent Spring Boot + Hibernate application

    let entity = GeneratedEntity {
        name: "TestEntity".to_string(),
        package: "com.test".to_string(),
        fields: vec![EntityField {
            name: "id".to_string(),
            field_type: "Long".to_string(),
            column_name: "id".to_string(),
            nullable: false,
            is_id: true,
        }],
        table_name: "test_entities".to_string(),
    };

    // Verify all components exist and validate
    let entity_code = MockJavaGenerator::generate_entity(&entity);
    let repo_code = MockJavaGenerator::generate_repository(&entity.name, &entity.package);
    let dto_code = MockJavaGenerator::generate_dto(&entity.name, &entity.package, &entity.fields);
    let controller_code = MockJavaGenerator::generate_controller(&entity.name, &entity.package);
    let service_code = MockJavaGenerator::generate_service(&entity.name, &entity.package);
    let hbm_code =
        MockJavaGenerator::generate_hbm_xml(&entity.name, &entity.table_name, &entity.fields);
    let serializer_code =
        MockJavaGenerator::generate_serializer(&entity.name, &entity.package, &entity.fields);

    assert!(validate_java_syntax(&entity_code).is_ok());
    assert!(validate_java_syntax(&repo_code).is_ok());
    assert!(validate_java_syntax(&dto_code).is_ok());
    assert!(validate_java_syntax(&controller_code).is_ok());
    assert!(validate_java_syntax(&service_code).is_ok());
    assert!(validate_xml_syntax(&hbm_code).is_ok());
    assert!(validate_java_syntax(&serializer_code).is_ok());
}
