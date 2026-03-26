//! Integration tests for Spring Boot Application Generator (Rule 2).
//!
//! Tests verify that the Spring Boot application scaffold is generated correctly:
//! - pom.xml contains correct Maven structure and dependencies
//! - YawlApplication.java is a valid Spring Boot entry point
//! - application.properties has correct JPA/Spring configuration
//! - application-test.properties uses H2 for testing
//! - .gitignore excludes Maven/IntelliJ artifacts

#[cfg(test)]
mod spring_boot_app_tests {
    use std::collections::HashMap;
    use ggen_yawl::codegen::rules::spring_boot_app::*;
    use ggen_codegen::Queryable;
    use ggen_codegen::Renderable;

    // ============================================================================
    // Query Tests
    // ============================================================================

    #[test]
    fn test_spring_boot_app_query_creates_successfully() {
        let query = SpringBootAppQuery::new();
        assert_eq!(query.name(), "spring-boot-app-query");
    }

    #[test]
    fn test_spring_boot_app_query_has_sparql_source() {
        let query = SpringBootAppQuery::new();
        let sparql = query.source().expect("SPARQL source missing");
        assert!(sparql.contains("SELECT"));
        assert!(sparql.contains("?appName"));
        assert!(sparql.contains("?packageName"));
    }

    #[test]
    fn test_spring_boot_app_query_executes_and_returns_mock_data() {
        let query = SpringBootAppQuery::new();
        let results = query.execute().expect("Query execution failed");

        assert!(!results.is_empty(), "Query should return at least one result");
        let app_data = &results[0];

        assert_eq!(app_data.get("appName").unwrap(), "YawlApplication");
        assert_eq!(app_data.get("packageName").unwrap(), "org.yawlfoundation.yawl");
        assert_eq!(app_data.get("version").unwrap(), "0.1.0");
        assert_eq!(app_data.get("springBootVersion").unwrap(), "3.3.0");
    }

    // ============================================================================
    // POM Template Tests
    // ============================================================================

    #[test]
    fn test_pom_template_initializes() {
        let template = PomTemplate::new().expect("Failed to create POM template");
        assert!(!format!("{:?}", template).is_empty());
    }

    #[test]
    fn test_pom_template_renders_with_valid_bindings() {
        let template = PomTemplate::new().expect("POM template creation failed");
        let mut bindings = HashMap::new();
        bindings.insert("packageName".to_string(), "org.example.app".to_string());
        bindings.insert("appName".to_string(), "ExampleApp".to_string());
        bindings.insert("version".to_string(), "1.0.0".to_string());
        bindings.insert("springBootVersion".to_string(), "3.3.0".to_string());

        let rendered = template.render(&bindings).expect("Render failed");
        assert!(!rendered.is_empty());
        assert!(rendered.starts_with("<?xml version"));
    }

    #[test]
    fn test_pom_has_spring_boot_parent_pom() {
        let template = PomTemplate::new().expect("POM template creation failed");
        let mut bindings = HashMap::new();
        bindings.insert("packageName".to_string(), "org.yawlfoundation.yawl".to_string());
        bindings.insert("appName".to_string(), "YawlApplication".to_string());
        bindings.insert("version".to_string(), "0.1.0".to_string());
        bindings.insert("springBootVersion".to_string(), "3.3.0".to_string());

        let pom = template.render(&bindings).expect("Render failed");

        assert!(pom.contains("spring-boot-starter-parent"));
        assert!(pom.contains("<version>3.3.0</version>"));
        assert!(pom.contains("<relativePath/>"));
    }

    #[test]
    fn test_pom_has_correct_coordinates() {
        let template = PomTemplate::new().expect("POM template creation failed");
        let mut bindings = HashMap::new();
        bindings.insert("packageName".to_string(), "org.example.test".to_string());
        bindings.insert("appName".to_string(), "TestApp".to_string());
        bindings.insert("version".to_string(), "2.5.0".to_string());
        bindings.insert("springBootVersion".to_string(), "3.3.0".to_string());

        let pom = template.render(&bindings).expect("Render failed");

        assert!(pom.contains("<groupId>org.example.test</groupId>"));
        assert!(pom.contains("<version>2.5.0</version>"));
        assert!(pom.contains("<name>TestApp</name>"));
        assert!(pom.contains("<packaging>jar</packaging>"));
    }

    #[test]
    fn test_pom_has_required_spring_boot_starters() {
        let template = PomTemplate::new().expect("POM template creation failed");
        let mut bindings = HashMap::new();
        bindings.insert("packageName".to_string(), "org.test".to_string());
        bindings.insert("appName".to_string(), "TestApp".to_string());
        bindings.insert("version".to_string(), "1.0.0".to_string());
        bindings.insert("springBootVersion".to_string(), "3.3.0".to_string());

        let pom = template.render(&bindings).expect("Render failed");

        assert!(pom.contains("spring-boot-starter-web"));
        assert!(pom.contains("spring-boot-starter-data-jpa"));
        assert!(pom.contains("spring-boot-starter-validation"));
        assert!(pom.contains("spring-boot-starter-test"));
    }

    #[test]
    fn test_pom_has_jakarta_persistence_3_2() {
        let template = PomTemplate::new().expect("POM template creation failed");
        let mut bindings = HashMap::new();
        bindings.insert("packageName".to_string(), "org.test".to_string());
        bindings.insert("appName".to_string(), "TestApp".to_string());
        bindings.insert("version".to_string(), "1.0.0".to_string());
        bindings.insert("springBootVersion".to_string(), "3.3.0".to_string());

        let pom = template.render(&bindings).expect("Render failed");

        assert!(pom.contains("jakarta.persistence-api"));
        assert!(pom.contains("<version>3.2.0</version>"));
    }

    #[test]
    fn test_pom_has_hibernate_core() {
        let template = PomTemplate::new().expect("POM template creation failed");
        let mut bindings = HashMap::new();
        bindings.insert("packageName".to_string(), "org.test".to_string());
        bindings.insert("appName".to_string(), "TestApp".to_string());
        bindings.insert("version".to_string(), "1.0.0".to_string());
        bindings.insert("springBootVersion".to_string(), "3.3.0".to_string());

        let pom = template.render(&bindings).expect("Render failed");

        assert!(pom.contains("hibernate-core"));
    }

    #[test]
    fn test_pom_has_jackson_serialization() {
        let template = PomTemplate::new().expect("POM template creation failed");
        let mut bindings = HashMap::new();
        bindings.insert("packageName".to_string(), "org.test".to_string());
        bindings.insert("appName".to_string(), "TestApp".to_string());
        bindings.insert("version".to_string(), "1.0.0".to_string());
        bindings.insert("springBootVersion".to_string(), "3.3.0".to_string());

        let pom = template.render(&bindings).expect("Render failed");

        assert!(pom.contains("jackson-databind"));
        assert!(pom.contains("jackson-datatype-jsr310"));
    }

    #[test]
    fn test_pom_has_postgresql_and_h2_databases() {
        let template = PomTemplate::new().expect("POM template creation failed");
        let mut bindings = HashMap::new();
        bindings.insert("packageName".to_string(), "org.test".to_string());
        bindings.insert("appName".to_string(), "TestApp".to_string());
        bindings.insert("version".to_string(), "1.0.0".to_string());
        bindings.insert("springBootVersion".to_string(), "3.3.0".to_string());

        let pom = template.render(&bindings).expect("Render failed");

        assert!(pom.contains("postgresql"));
        assert!(pom.contains("<scope>runtime</scope>"));
        assert!(pom.contains("h2"));
        assert!(pom.contains("<scope>test</scope>"));
    }

    #[test]
    fn test_pom_has_spring_boot_maven_plugin() {
        let template = PomTemplate::new().expect("POM template creation failed");
        let mut bindings = HashMap::new();
        bindings.insert("packageName".to_string(), "org.test".to_string());
        bindings.insert("appName".to_string(), "TestApp".to_string());
        bindings.insert("version".to_string(), "1.0.0".to_string());
        bindings.insert("springBootVersion".to_string(), "3.3.0".to_string());

        let pom = template.render(&bindings).expect("Render failed");

        assert!(pom.contains("spring-boot-maven-plugin"));
    }

    #[test]
    fn test_pom_has_maven_compiler_plugin_java_21() {
        let template = PomTemplate::new().expect("POM template creation failed");
        let mut bindings = HashMap::new();
        bindings.insert("packageName".to_string(), "org.test".to_string());
        bindings.insert("appName".to_string(), "TestApp".to_string());
        bindings.insert("version".to_string(), "1.0.0".to_string());
        bindings.insert("springBootVersion".to_string(), "3.3.0".to_string());

        let pom = template.render(&bindings).expect("Render failed");

        assert!(pom.contains("maven-compiler-plugin"));
        assert!(pom.contains("<source>21</source>"));
        assert!(pom.contains("<target>21</target>"));
        assert!(pom.contains("<fork>false</fork>"));
        assert!(pom.contains("<java.version>21</java.version>"));
    }

    #[test]
    fn test_pom_has_utf8_encoding() {
        let template = PomTemplate::new().expect("POM template creation failed");
        let mut bindings = HashMap::new();
        bindings.insert("packageName".to_string(), "org.test".to_string());
        bindings.insert("appName".to_string(), "TestApp".to_string());
        bindings.insert("version".to_string(), "1.0.0".to_string());
        bindings.insert("springBootVersion".to_string(), "3.3.0".to_string());

        let pom = template.render(&bindings).expect("Render failed");

        assert!(pom.contains("<project.build.sourceEncoding>UTF-8</project.build.sourceEncoding>"));
    }

    // ============================================================================
    // Application Template Tests
    // ============================================================================

    #[test]
    fn test_application_template_initializes() {
        let template = ApplicationTemplate::new().expect("Failed to create Application template");
        assert!(!format!("{:?}", template).is_empty());
    }

    #[test]
    fn test_application_template_renders_spring_boot_main() {
        let template = ApplicationTemplate::new().expect("Application template creation failed");
        let mut bindings = HashMap::new();
        bindings.insert("packageName".to_string(), "org.example.app".to_string());
        bindings.insert("version".to_string(), "1.0.0".to_string());

        let code = template.render(&bindings).expect("Render failed");

        assert!(code.contains("package org.example.app;"));
        assert!(code.contains("public class YawlApplication"));
        assert!(code.contains("@SpringBootApplication"));
        assert!(code.contains("@EnableTransactionManagement"));
        assert!(code.contains("SpringApplication.run(YawlApplication.class, args)"));
    }

    #[test]
    fn test_application_has_correct_imports() {
        let template = ApplicationTemplate::new().expect("Application template creation failed");
        let mut bindings = HashMap::new();
        bindings.insert("packageName".to_string(), "org.test.app".to_string());
        bindings.insert("version".to_string(), "1.0.0".to_string());

        let code = template.render(&bindings).expect("Render failed");

        assert!(code.contains("import org.springframework.boot.SpringApplication"));
        assert!(code.contains("import org.springframework.boot.autoconfigure.SpringBootApplication"));
        assert!(code.contains("import org.springframework.transaction.annotation.EnableTransactionManagement"));
    }

    // ============================================================================
    // Application Properties Tests
    // ============================================================================

    #[test]
    fn test_application_properties_template_initializes() {
        let template = ApplicationPropertiesTemplate::new()
            .expect("Failed to create Application Properties template");
        assert!(!format!("{:?}", template).is_empty());
    }

    #[test]
    fn test_application_properties_renders_with_postgres() {
        let template = ApplicationPropertiesTemplate::new()
            .expect("Application Properties template creation failed");
        let mut bindings = HashMap::new();
        bindings.insert("appName".to_string(), "YawlApplication".to_string());
        bindings.insert("version".to_string(), "0.1.0".to_string());

        let props = template.render(&bindings).expect("Render failed");

        assert!(props.contains("spring.application.name=YawlApplication"));
        assert!(props.contains("spring.jpa.hibernate.ddl-auto=validate"));
        assert!(props.contains("spring.jpa.show-sql=false"));
        assert!(props.contains("spring.datasource.url=jdbc:postgresql://localhost:5432/yawl"));
        assert!(props.contains("org.postgresql.Driver"));
    }

    #[test]
    fn test_application_properties_has_jpa_formatting() {
        let template = ApplicationPropertiesTemplate::new()
            .expect("Application Properties template creation failed");
        let mut bindings = HashMap::new();
        bindings.insert("appName".to_string(), "YawlApplication".to_string());
        bindings.insert("version".to_string(), "0.1.0".to_string());

        let props = template.render(&bindings).expect("Render failed");

        assert!(props.contains("spring.jpa.properties.hibernate.format_sql=true"));
        assert!(props.contains("spring.jpa.properties.hibernate.jdbc.batch_size=20"));
        assert!(props.contains("spring.jpa.properties.hibernate.order_inserts=true"));
    }

    #[test]
    fn test_application_properties_has_connection_pool() {
        let template = ApplicationPropertiesTemplate::new()
            .expect("Application Properties template creation failed");
        let mut bindings = HashMap::new();
        bindings.insert("appName".to_string(), "YawlApplication".to_string());
        bindings.insert("version".to_string(), "0.1.0".to_string());

        let props = template.render(&bindings).expect("Render failed");

        assert!(props.contains("spring.datasource.hikari.maximum-pool-size=10"));
        assert!(props.contains("spring.datasource.hikari.minimum-idle=5"));
        assert!(props.contains("spring.datasource.hikari.connection-timeout=30000"));
    }

    #[test]
    fn test_application_properties_has_logging_config() {
        let template = ApplicationPropertiesTemplate::new()
            .expect("Application Properties template creation failed");
        let mut bindings = HashMap::new();
        bindings.insert("appName".to_string(), "YawlApplication".to_string());
        bindings.insert("version".to_string(), "0.1.0".to_string());

        let props = template.render(&bindings).expect("Render failed");

        assert!(props.contains("logging.level.root=INFO"));
        assert!(props.contains("logging.level.org.springframework.web=DEBUG"));
        assert!(props.contains("logging.pattern.console="));
    }

    #[test]
    fn test_application_properties_has_server_config() {
        let template = ApplicationPropertiesTemplate::new()
            .expect("Application Properties template creation failed");
        let mut bindings = HashMap::new();
        bindings.insert("appName".to_string(), "YawlApplication".to_string());
        bindings.insert("version".to_string(), "0.1.0".to_string());

        let props = template.render(&bindings).expect("Render failed");

        assert!(props.contains("server.port=8080"));
        assert!(props.contains("server.servlet.context-path=/api"));
        assert!(props.contains("server.tomcat.threads.max=200"));
    }

    // ============================================================================
    // Application Test Properties Tests
    // ============================================================================

    #[test]
    fn test_application_test_properties_template_initializes() {
        let template = ApplicationTestPropertiesTemplate::new()
            .expect("Failed to create Application Test Properties template");
        assert!(!format!("{:?}", template).is_empty());
    }

    #[test]
    fn test_application_test_properties_uses_h2() {
        let template = ApplicationTestPropertiesTemplate::new()
            .expect("Application Test Properties template creation failed");
        let mut bindings = HashMap::new();
        bindings.insert("appName".to_string(), "YawlApplication".to_string());

        let props = template.render(&bindings).expect("Render failed");

        assert!(props.contains("spring.datasource.url=jdbc:h2:mem:testdb"));
        assert!(props.contains("org.h2.Driver"));
        assert!(props.contains("spring.jpa.hibernate.ddl-auto=create-drop"));
    }

    #[test]
    fn test_application_test_properties_has_h2_console() {
        let template = ApplicationTestPropertiesTemplate::new()
            .expect("Application Test Properties template creation failed");
        let mut bindings = HashMap::new();
        bindings.insert("appName".to_string(), "YawlApplication".to_string());

        let props = template.render(&bindings).expect("Render failed");

        assert!(props.contains("spring.h2.console.enabled=true"));
        assert!(props.contains("spring.h2.console.path=/h2-console"));
    }

    #[test]
    fn test_application_test_properties_has_verbose_logging() {
        let template = ApplicationTestPropertiesTemplate::new()
            .expect("Application Test Properties template creation failed");
        let mut bindings = HashMap::new();
        bindings.insert("appName".to_string(), "YawlApplication".to_string());

        let props = template.render(&bindings).expect("Render failed");

        assert!(props.contains("logging.level.org.springframework.test=DEBUG"));
        assert!(props.contains("logging.level.org.hibernate.SQL=DEBUG"));
        assert!(props.contains("logging.level.org.hibernate.type.descriptor.sql.BasicBinder=TRACE"));
    }

    #[test]
    fn test_application_test_properties_separate_port() {
        let template = ApplicationTestPropertiesTemplate::new()
            .expect("Application Test Properties template creation failed");
        let mut bindings = HashMap::new();
        bindings.insert("appName".to_string(), "YawlApplication".to_string());

        let props = template.render(&bindings).expect("Render failed");

        assert!(props.contains("server.port=8081"));
        assert!(props.contains("server.servlet.context-path=/api/test"));
    }

    // ============================================================================
    // Gitignore Tests
    // ============================================================================

    #[test]
    fn test_gitignore_template_initializes() {
        let template = GitignoreTemplate::new().expect("Failed to create Gitignore template");
        assert!(!format!("{:?}", template).is_empty());
    }

    #[test]
    fn test_gitignore_ignores_maven_artifacts() {
        let template = GitignoreTemplate::new().expect("Gitignore template creation failed");
        let bindings = HashMap::new();

        let gitignore = template.render(&bindings).expect("Render failed");

        assert!(gitignore.contains("target/"));
        assert!(gitignore.contains("pom.xml.tag"));
        assert!(gitignore.contains("pom.xml.releaseBackup"));
        assert!(gitignore.contains("dependency-reduced-pom.xml"));
    }

    #[test]
    fn test_gitignore_ignores_ide_files() {
        let template = GitignoreTemplate::new().expect("Gitignore template creation failed");
        let bindings = HashMap::new();

        let gitignore = template.render(&bindings).expect("Render failed");

        assert!(gitignore.contains(".idea/"));
        assert!(gitignore.contains(".vscode/"));
        assert!(gitignore.contains("*.iml"));
        assert!(gitignore.contains("nbproject/"));
        assert!(gitignore.contains(".classpath"));
    }

    #[test]
    fn test_gitignore_ignores_build_artifacts() {
        let template = GitignoreTemplate::new().expect("Gitignore template creation failed");
        let bindings = HashMap::new();

        let gitignore = template.render(&bindings).expect("Render failed");

        assert!(gitignore.contains("*.class"));
        assert!(gitignore.contains("*.jar"));
        assert!(gitignore.contains("*.war"));
        assert!(gitignore.contains("*.log"));
    }

    #[test]
    fn test_gitignore_ignores_os_files() {
        let template = GitignoreTemplate::new().expect("Gitignore template creation failed");
        let bindings = HashMap::new();

        let gitignore = template.render(&bindings).expect("Render failed");

        assert!(gitignore.contains(".DS_Store"));
        assert!(gitignore.contains("thumbs.db"));
    }

    #[test]
    fn test_gitignore_ignores_application_profiles() {
        let template = GitignoreTemplate::new().expect("Gitignore template creation failed");
        let bindings = HashMap::new();

        let gitignore = template.render(&bindings).expect("Render failed");

        assert!(gitignore.contains("application-local.properties"));
        assert!(gitignore.contains("application-dev.properties"));
        assert!(gitignore.contains("application-prod.properties"));
    }

    // ============================================================================
    // Integration Tests
    // ============================================================================

    #[test]
    fn test_rule_creation_functions_work() {
        let _pom_rule = create_spring_boot_app_rule().expect("POM rule creation failed");
        let _app_rule = create_application_main_rule().expect("App rule creation failed");
        let _props_rule = create_application_properties_rule().expect("Props rule creation failed");
        let _test_props_rule = create_application_test_properties_rule().expect("Test props rule creation failed");
        let _gitignore_rule = create_gitignore_rule().expect("Gitignore rule creation failed");
        // If we reach here, all rules created successfully
    }

    #[test]
    fn test_pom_generation_matches_spring_boot_3_3_0() {
        let template = PomTemplate::new().expect("POM template creation failed");
        let mut bindings = HashMap::new();
        bindings.insert("packageName".to_string(), "org.yawlfoundation.yawl".to_string());
        bindings.insert("appName".to_string(), "YawlApplication".to_string());
        bindings.insert("version".to_string(), "0.1.0".to_string());
        bindings.insert("springBootVersion".to_string(), "3.3.0".to_string());

        let pom = template.render(&bindings).expect("Render failed");

        // Verify Spring Boot 3.3.0 compatibility
        assert!(pom.contains("spring-boot-starter-parent"));
        assert!(pom.contains("3.3.0"));
        assert!(pom.contains("jakarta.persistence-api"));
        assert!(pom.contains("hibernate-core"));
        assert!(pom.contains("PostgreSQLDialect"));
    }

    #[test]
    fn test_complete_application_scaffold_consistency() {
        // Test that all generated files reference same app name and package
        let query = SpringBootAppQuery::new();
        let results = query.execute().expect("Query failed");
        assert!(!results.is_empty());

        let bindings = &results[0];

        let pom_template = PomTemplate::new().expect("POM template failed");
        let pom = pom_template.render(bindings).expect("POM render failed");

        let app_template = ApplicationTemplate::new().expect("App template failed");
        let app = app_template.render(bindings).expect("App render failed");

        let props_template = ApplicationPropertiesTemplate::new().expect("Props template failed");
        let props = props_template.render(bindings).expect("Props render failed");

        // All should reference the same package
        let package = bindings.get("packageName").unwrap();
        assert!(pom.contains(package));
        assert!(app.contains(package));
        assert!(props.contains("YawlApplication"));

        // Main class reference in pom should match application class
        assert!(pom.contains("YawlApplication"));
        assert!(app.contains("YawlApplication"));
    }
}
