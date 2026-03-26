//! Rule 2: Spring Boot Application - Parent application structure and configuration.
//!
//! Generates the Spring Boot application scaffold including:
//! - pom.xml with Spring Boot parent and all dependencies
//! - YawlApplication.java main entry point class
//! - application.properties JPA and Spring configuration
//! - application-test.properties test profile
//! - .gitignore standard Maven/IntelliJ ignores

use crate::error::{Error, Result};
use ggen_codegen::Result as CodegenResult;
use ggen_codegen::{Error as CodegenError, GenerationMode, Queryable, Renderable, Rule};
use std::collections::HashMap;
use std::path::PathBuf;
use tera::{Context, Tera};

/// Query executor for Spring Boot application generation.
///
/// Extracts application metadata (name, package, version, Spring Boot version)
/// for Maven POM and application configuration.
pub struct SpringBootAppQuery {
    /// SPARQL SELECT query that extracts application metadata
    query: String,
}

impl SpringBootAppQuery {
    /// Create a new Spring Boot app query.
    pub fn new() -> Self {
        // SPARQL Query 2.1: Application metadata from yawl-domain.ttl
        // Extracts: appName, packageName, version, springBootVersion
        let query = "PREFIX yawl: <https://yawlfoundation.org/ontology#>\n\
            PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>\n\n\
            SELECT ?appName ?packageName ?version ?springBootVersion\n\
            WHERE {\n\
              ?app a yawl:Application ;\n\
                      yawl:appName ?appName ;\n\
                      yawl:packageName ?packageName ;\n\
                      yawl:version ?version ;\n\
                      yawl:springBootVersion ?springBootVersion .\n\
            }\n\
            LIMIT 1"
            .to_string();

        Self { query }
    }

    /// SPARQL query text (for debugging)
    pub fn sparql(&self) -> &str {
        &self.query
    }
}

impl Queryable for SpringBootAppQuery {
    fn execute(&self) -> CodegenResult<Vec<HashMap<String, String>>> {
        // For now, return mock data that demonstrates the pattern.
        // In Phase 3 final, this will load the actual YAWL ontology and execute SPARQL.

        let mut results = Vec::new();

        // Mock application metadata
        let mut app = HashMap::new();
        app.insert("appName".to_string(), "YawlApplication".to_string());
        app.insert("packageName".to_string(), "org.yawlfoundation.yawl".to_string());
        app.insert("version".to_string(), "0.1.0".to_string());
        app.insert("springBootVersion".to_string(), "3.3.0".to_string());
        results.push(app);

        Ok(results)
    }

    fn name(&self) -> &str {
        "spring-boot-app-query"
    }

    fn source(&self) -> Option<&str> {
        Some(self.sparql())
    }
}

/// Template renderer for pom.xml generation.
pub struct PomTemplate {
    tera: Tera,
}

impl PomTemplate {
    /// Create a new POM template renderer.
    pub fn new() -> Result<Self> {
        let mut tera = Tera::default();

        // Maven POM template with Spring Boot parent and dependencies
        let template = r#"<?xml version="1.0" encoding="UTF-8"?>
<project xmlns="http://maven.apache.org/POM/4.0.0"
         xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
         xsi:schemaLocation="http://maven.apache.org/POM/4.0.0
                            http://maven.apache.org/xsd/maven-4.0.0.xsd">
    <modelVersion>4.0.0</modelVersion>

    <!-- Spring Boot Parent POM -->
    <parent>
        <groupId>org.springframework.boot</groupId>
        <artifactId>spring-boot-starter-parent</artifactId>
        <version>{{ springBootVersion }}</version>
        <relativePath/>
    </parent>

    <!-- Project Coordinates -->
    <groupId>{{ packageName }}</groupId>
    <artifactId>yawl-app</artifactId>
    <version>{{ version }}</version>
    <name>{{ appName }}</name>
    <description>YAWL Workflow Engine - Spring Boot Application</description>
    <packaging>jar</packaging>

    <!-- Properties -->
    <properties>
        <project.build.sourceEncoding>UTF-8</project.build.sourceEncoding>
        <maven.compiler.source>21</maven.compiler.source>
        <maven.compiler.target>21</maven.compiler.target>
        <java.version>21</java.version>
    </properties>

    <!-- Dependencies -->
    <dependencies>
        <!-- Spring Boot Starters -->
        <dependency>
            <groupId>org.springframework.boot</groupId>
            <artifactId>spring-boot-starter-web</artifactId>
        </dependency>

        <dependency>
            <groupId>org.springframework.boot</groupId>
            <artifactId>spring-boot-starter-data-jpa</artifactId>
        </dependency>

        <dependency>
            <groupId>org.springframework.boot</groupId>
            <artifactId>spring-boot-starter-validation</artifactId>
        </dependency>

        <!-- Jakarta Persistence 3.2 -->
        <dependency>
            <groupId>jakarta.persistence</groupId>
            <artifactId>jakarta.persistence-api</artifactId>
            <version>3.2.0</version>
        </dependency>

        <!-- Hibernate Core -->
        <dependency>
            <groupId>org.hibernate.orm</groupId>
            <artifactId>hibernate-core</artifactId>
            <version>6.4.4.Final</version>
        </dependency>

        <!-- Jackson (JSON serialization) -->
        <dependency>
            <groupId>com.fasterxml.jackson.core</groupId>
            <artifactId>jackson-databind</artifactId>
            <version>2.17.0</version>
        </dependency>

        <dependency>
            <groupId>com.fasterxml.jackson.datatype</groupId>
            <artifactId>jackson-datatype-jsr310</artifactId>
            <version>2.17.0</version>
        </dependency>

        <!-- PostgreSQL Driver (Production) -->
        <dependency>
            <groupId>org.postgresql</groupId>
            <artifactId>postgresql</artifactId>
            <version>42.7.2</version>
            <scope>runtime</scope>
        </dependency>

        <!-- H2 Database (Testing) -->
        <dependency>
            <groupId>com.h2database</groupId>
            <artifactId>h2</artifactId>
            <version>2.2.224</version>
            <scope>test</scope>
        </dependency>

        <!-- Lombok (Optional annotation processor) -->
        <dependency>
            <groupId>org.projectlombok</groupId>
            <artifactId>lombok</artifactId>
            <optional>true</optional>
        </dependency>

        <!-- Test Dependencies -->
        <dependency>
            <groupId>org.springframework.boot</groupId>
            <artifactId>spring-boot-starter-test</artifactId>
            <scope>test</scope>
        </dependency>

        <dependency>
            <groupId>org.junit.jupiter</groupId>
            <artifactId>junit-jupiter-api</artifactId>
            <scope>test</scope>
        </dependency>

        <dependency>
            <groupId>org.junit.jupiter</groupId>
            <artifactId>junit-jupiter-engine</artifactId>
            <scope>test</scope>
        </dependency>
    </dependencies>

    <!-- Build Plugins -->
    <build>
        <plugins>
            <!-- Spring Boot Maven Plugin -->
            <plugin>
                <groupId>org.springframework.boot</groupId>
                <artifactId>spring-boot-maven-plugin</artifactId>
                <configuration>
                    <excludes>
                        <exclude>
                            <groupId>org.projectlombok</groupId>
                            <artifactId>lombok</artifactId>
                        </exclude>
                    </excludes>
                </configuration>
            </plugin>

            <!-- Maven Compiler Plugin -->
            <plugin>
                <groupId>org.apache.maven.plugins</groupId>
                <artifactId>maven-compiler-plugin</artifactId>
                <version>3.12.1</version>
                <configuration>
                    <source>21</source>
                    <target>21</target>
                    <fork>false</fork>
                </configuration>
            </plugin>

            <!-- Maven Jar Plugin -->
            <plugin>
                <groupId>org.apache.maven.plugins</groupId>
                <artifactId>maven-jar-plugin</artifactId>
                <version>3.3.0</version>
                <configuration>
                    <archive>
                        <manifest>
                            <mainClass>{{ packageName }}.YawlApplication</mainClass>
                        </manifest>
                    </archive>
                </configuration>
            </plugin>
        </plugins>
    </build>
</project>"#;

        tera.add_raw_template("pom.xml.tera", template)
            .map_err(|e| Error::template(format!("Failed to load POM template: {}", e)))?;

        Ok(Self { tera })
    }
}

impl Renderable for PomTemplate {
    fn render(&self, bindings: &HashMap<String, String>) -> CodegenResult<String> {
        let mut context = Context::new();

        // Extract application metadata
        let package_name = bindings
            .get("packageName")
            .ok_or_else(|| CodegenError::template("Missing packageName in bindings".to_string()))?
            .clone();

        let app_name = bindings
            .get("appName")
            .unwrap_or(&"YawlApplication".to_string())
            .clone();

        let version = bindings
            .get("version")
            .unwrap_or(&"0.1.0".to_string())
            .clone();

        let spring_boot_version = bindings
            .get("springBootVersion")
            .unwrap_or(&"3.3.0".to_string())
            .clone();

        context.insert("packageName", &package_name);
        context.insert("appName", &app_name);
        context.insert("version", &version);
        context.insert("springBootVersion", &spring_boot_version);

        self.tera
            .render("pom.xml.tera", &context)
            .map_err(|e| CodegenError::template(format!("Failed to render pom.xml: {}", e)))
    }
}

/// Template renderer for YawlApplication.java generation.
pub struct ApplicationTemplate {
    tera: Tera,
}

impl ApplicationTemplate {
    /// Create a new application template renderer.
    pub fn new() -> Result<Self> {
        let mut tera = Tera::default();

        let template = r#"package {{ packageName }};

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.transaction.annotation.EnableTransactionManagement;

/**
 * YAWL Workflow Engine - Spring Boot Application Entry Point
 *
 * This is the main application class that bootstraps the YAWL workflow engine
 * with Spring Boot and Spring Data JPA for persistent storage.
 *
 * @author YAWL Foundation
 * @version {{ version }}
 */
@SpringBootApplication
@EnableTransactionManagement
public class YawlApplication {

    public static void main(String[] args) {
        SpringApplication.run(YawlApplication.class, args);
    }
}
"#;

        tera.add_raw_template("YawlApplication.java.tera", template)
            .map_err(|e| Error::template(format!("Failed to load application template: {}", e)))?;

        Ok(Self { tera })
    }
}

impl Renderable for ApplicationTemplate {
    fn render(&self, bindings: &HashMap<String, String>) -> CodegenResult<String> {
        let mut context = Context::new();

        let package_name = bindings
            .get("packageName")
            .ok_or_else(|| CodegenError::template("Missing packageName in bindings".to_string()))?
            .clone();

        let version = bindings
            .get("version")
            .unwrap_or(&"0.1.0".to_string())
            .clone();

        context.insert("packageName", &package_name);
        context.insert("version", &version);

        self.tera
            .render("YawlApplication.java.tera", &context)
            .map_err(|e| CodegenError::template(format!("Failed to render YawlApplication.java: {}", e)))
    }
}

/// Template renderer for application.properties generation.
pub struct ApplicationPropertiesTemplate {
    tera: Tera,
}

impl ApplicationPropertiesTemplate {
    /// Create a new application properties template renderer.
    pub fn new() -> Result<Self> {
        let mut tera = Tera::default();

        let template = r#"# Application Configuration
spring.application.name={{ appName }}
spring.application.version={{ version }}

# JPA/Hibernate Configuration
spring.jpa.database-platform=org.hibernate.dialect.PostgreSQLDialect
spring.jpa.hibernate.ddl-auto=validate
spring.jpa.show-sql=false
spring.jpa.properties.hibernate.format_sql=true
spring.jpa.properties.hibernate.dialect=org.hibernate.dialect.PostgreSQLDialect
spring.jpa.properties.hibernate.jdbc.batch_size=20
spring.jpa.properties.hibernate.order_inserts=true
spring.jpa.properties.hibernate.order_updates=true

# PostgreSQL DataSource Configuration
spring.datasource.url=jdbc:postgresql://localhost:5432/yawl
spring.datasource.username=yawl
spring.datasource.password=yawl
spring.datasource.driver-class-name=org.postgresql.Driver

# Connection Pool Settings
spring.datasource.hikari.maximum-pool-size=10
spring.datasource.hikari.minimum-idle=5
spring.datasource.hikari.connection-timeout=30000

# Logging Configuration
logging.level.root=INFO
logging.level.org.springframework.web=DEBUG
logging.level.org.springframework.data=DEBUG
logging.level.org.hibernate=WARN
logging.pattern.console=%d{yyyy-MM-dd HH:mm:ss} - %logger{36} - %msg%n

# Server Configuration
server.port=8080
server.servlet.context-path=/api
server.tomcat.threads.max=200
server.tomcat.threads.min-spare=10
"#;

        tera.add_raw_template("application.properties.tera", template)
            .map_err(|e| Error::template(format!("Failed to load properties template: {}", e)))?;

        Ok(Self { tera })
    }
}

impl Renderable for ApplicationPropertiesTemplate {
    fn render(&self, bindings: &HashMap<String, String>) -> CodegenResult<String> {
        let mut context = Context::new();

        let app_name = bindings
            .get("appName")
            .unwrap_or(&"YawlApplication".to_string())
            .clone();

        let version = bindings
            .get("version")
            .unwrap_or(&"0.1.0".to_string())
            .clone();

        context.insert("appName", &app_name);
        context.insert("version", &version);

        self.tera
            .render("application.properties.tera", &context)
            .map_err(|e| CodegenError::template(format!("Failed to render application.properties: {}", e)))
    }
}

/// Template renderer for application-test.properties generation.
pub struct ApplicationTestPropertiesTemplate {
    tera: Tera,
}

impl ApplicationTestPropertiesTemplate {
    /// Create a new test properties template renderer.
    pub fn new() -> Result<Self> {
        let mut tera = Tera::default();

        let template = r#"# Test Profile Configuration (application-test.properties)
# Used for unit and integration tests with H2 in-memory database

spring.application.name={{ appName }}-test
spring.profiles.active=test

# H2 In-Memory Database Configuration
spring.datasource.url=jdbc:h2:mem:testdb
spring.datasource.driverClassName=org.h2.Driver
spring.datasource.username=sa
spring.datasource.password=

# JPA/Hibernate Configuration for Testing
spring.jpa.database-platform=org.hibernate.dialect.H2Dialect
spring.jpa.hibernate.ddl-auto=create-drop
spring.jpa.show-sql=true
spring.jpa.properties.hibernate.format_sql=true
spring.jpa.properties.hibernate.dialect=org.hibernate.dialect.H2Dialect
spring.jpa.defer-datasource-initialization=true

# H2 Console (for debugging tests)
spring.h2.console.enabled=true
spring.h2.console.path=/h2-console

# Logging Configuration (Verbose for testing)
logging.level.root=INFO
logging.level.org.springframework.test=DEBUG
logging.level.org.springframework.data=DEBUG
logging.level.org.hibernate.SQL=DEBUG
logging.level.org.hibernate.type.descriptor.sql.BasicBinder=TRACE
logging.pattern.console=%d{HH:mm:ss.SSS} [%thread] %-5level %logger{36} - %msg%n

# Server Configuration
server.port=8081
server.servlet.context-path=/api/test

# Test Timeouts
spring.test.mockmvc.print=true
"#;

        tera.add_raw_template("application-test.properties.tera", template)
            .map_err(|e| Error::template(format!("Failed to load test properties template: {}", e)))?;

        Ok(Self { tera })
    }
}

impl Renderable for ApplicationTestPropertiesTemplate {
    fn render(&self, bindings: &HashMap<String, String>) -> CodegenResult<String> {
        let mut context = Context::new();

        let app_name = bindings
            .get("appName")
            .unwrap_or(&"YawlApplication".to_string())
            .clone();

        context.insert("appName", &app_name);

        self.tera
            .render("application-test.properties.tera", &context)
            .map_err(|e| CodegenError::template(format!("Failed to render application-test.properties: {}", e)))
    }
}

/// Template renderer for .gitignore generation.
pub struct GitignoreTemplate {
    tera: Tera,
}

impl GitignoreTemplate {
    /// Create a new gitignore template renderer.
    pub fn new() -> Result<Self> {
        let mut tera = Tera::default();

        let template = r#"# Maven
target/
pom.xml.tag
pom.xml.releaseBackup
pom.xml.versionsBackup
pom.xml.next
release.properties
dependency-reduced-pom.xml
.flattened-pom.xml

# IDEs
.idea/
.vscode/
*.swp
*.swo
*.swn
*.sublime-project
*.sublime-workspace

# Eclipse
.classpath
.project
.settings/
*.launch

# NetBeans
nbproject/
build/
nbbuild/
dist/
nbdist/

# IntelliJ IDEA
*.iml
*.iws
*.ipr
out/

# OS
.DS_Store
.AppleDouble
.LSOverride
thumbs.db

# Build artifacts
*.class
*.jar
*.war
*.ear
*.zip
*.tar.gz

# Logs
*.log
logs/

# Dependencies
.m2/
node_modules/

# Temporary files
*.tmp
*.bak
*~

# Spring Boot
application-local.properties
application-dev.properties
application-prod.properties
"#;

        tera.add_raw_template("gitignore.tera", template)
            .map_err(|e| Error::template(format!("Failed to load gitignore template: {}", e)))?;

        Ok(Self { tera })
    }
}

impl Renderable for GitignoreTemplate {
    fn render(&self, _bindings: &HashMap<String, String>) -> CodegenResult<String> {
        let context = Context::new();

        self.tera
            .render("gitignore.tera", &context)
            .map_err(|e| CodegenError::template(format!("Failed to render .gitignore: {}", e)))
    }
}

/// Factory function to create Rule<SpringBootAppQuery, PomTemplate>
pub fn create_spring_boot_app_rule() -> Result<Rule<SpringBootAppQuery, PomTemplate>> {
    let query = SpringBootAppQuery::new();
    let template = PomTemplate::new()?;

    let rule = Rule::new(
        "spring-boot-pom",
        query,
        template,
        PathBuf::from("pom.xml"),
        GenerationMode::Overwrite,
    );

    Ok(rule)
}

/// Factory function to create Rule<SpringBootAppQuery, ApplicationTemplate>
pub fn create_application_main_rule() -> Result<Rule<SpringBootAppQuery, ApplicationTemplate>> {
    let query = SpringBootAppQuery::new();
    let template = ApplicationTemplate::new()?;

    let rule = Rule::new(
        "spring-boot-app-main",
        query,
        template,
        PathBuf::from("src/main/java/{{ packageName }}/YawlApplication.java"),
        GenerationMode::Overwrite,
    );

    Ok(rule)
}

/// Factory function to create Rule<SpringBootAppQuery, ApplicationPropertiesTemplate>
pub fn create_application_properties_rule() -> Result<Rule<SpringBootAppQuery, ApplicationPropertiesTemplate>> {
    let query = SpringBootAppQuery::new();
    let template = ApplicationPropertiesTemplate::new()?;

    let rule = Rule::new(
        "spring-boot-app-properties",
        query,
        template,
        PathBuf::from("src/main/resources/application.properties"),
        GenerationMode::Overwrite,
    );

    Ok(rule)
}

/// Factory function to create Rule<SpringBootAppQuery, ApplicationTestPropertiesTemplate>
pub fn create_application_test_properties_rule() -> Result<Rule<SpringBootAppQuery, ApplicationTestPropertiesTemplate>> {
    let query = SpringBootAppQuery::new();
    let template = ApplicationTestPropertiesTemplate::new()?;

    let rule = Rule::new(
        "spring-boot-app-test-properties",
        query,
        template,
        PathBuf::from("src/test/resources/application-test.properties"),
        GenerationMode::Overwrite,
    );

    Ok(rule)
}

/// Factory function to create Rule<SpringBootAppQuery, GitignoreTemplate>
pub fn create_gitignore_rule() -> Result<Rule<SpringBootAppQuery, GitignoreTemplate>> {
    let query = SpringBootAppQuery::new();
    let template = GitignoreTemplate::new()?;

    let rule = Rule::new(
        "spring-boot-gitignore",
        query,
        template,
        PathBuf::from(".gitignore"),
        GenerationMode::Overwrite,
    );

    Ok(rule)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_spring_boot_app_query_creates() {
        let query = SpringBootAppQuery::new();
        assert_eq!(query.name(), "spring-boot-app-query");
    }

    #[test]
    fn test_spring_boot_app_query_executes() {
        let query = SpringBootAppQuery::new();
        let results = query.execute().expect("Query execution failed");
        assert!(!results.is_empty());
        let app = &results[0];
        assert_eq!(app.get("appName").unwrap(), "YawlApplication");
        assert_eq!(app.get("packageName").unwrap(), "org.yawlfoundation.yawl");
    }

    #[test]
    fn test_pom_template_creates() {
        let template = PomTemplate::new().expect("Failed to create POM template");
        assert!(!format!("{:?}", template).is_empty());
    }

    #[test]
    fn test_pom_template_renders() {
        let template = PomTemplate::new().expect("Failed to create POM template");
        let mut bindings = HashMap::new();
        bindings.insert("packageName".to_string(), "org.example.app".to_string());
        bindings.insert("appName".to_string(), "ExampleApp".to_string());
        bindings.insert("version".to_string(), "1.0.0".to_string());
        bindings.insert("springBootVersion".to_string(), "3.3.0".to_string());

        let rendered = template.render(&bindings).expect("Render failed");
        assert!(rendered.contains("spring-boot-starter-parent"));
        assert!(rendered.contains("org.example.app"));
        assert!(rendered.contains("3.3.0"));
        assert!(rendered.contains("<packaging>jar</packaging>"));
    }

    #[test]
    fn test_application_template_renders() {
        let template = ApplicationTemplate::new().expect("Failed to create Application template");
        let mut bindings = HashMap::new();
        bindings.insert("packageName".to_string(), "org.example.app".to_string());
        bindings.insert("version".to_string(), "1.0.0".to_string());

        let rendered = template.render(&bindings).expect("Render failed");
        assert!(rendered.contains("package org.example.app;"));
        assert!(rendered.contains("class YawlApplication"));
        assert!(rendered.contains("SpringBootApplication"));
        assert!(rendered.contains("SpringApplication.run"));
    }

    #[test]
    fn test_application_properties_renders() {
        let template = ApplicationPropertiesTemplate::new().expect("Failed to create properties template");
        let mut bindings = HashMap::new();
        bindings.insert("appName".to_string(), "YawlApp".to_string());
        bindings.insert("version".to_string(), "0.1.0".to_string());

        let rendered = template.render(&bindings).expect("Render failed");
        assert!(rendered.contains("spring.application.name=YawlApp"));
        assert!(rendered.contains("spring.jpa.hibernate.ddl-auto=validate"));
        assert!(rendered.contains("spring.datasource.url=jdbc:postgresql://localhost:5432/yawl"));
        assert!(rendered.contains("server.port=8080"));
    }

    #[test]
    fn test_application_test_properties_renders() {
        let template = ApplicationTestPropertiesTemplate::new().expect("Failed to create test properties template");
        let mut bindings = HashMap::new();
        bindings.insert("appName".to_string(), "YawlApp".to_string());

        let rendered = template.render(&bindings).expect("Render failed");
        assert!(rendered.contains("spring.application.name=YawlApp-test"));
        assert!(rendered.contains("spring.jpa.hibernate.ddl-auto=create-drop"));
        assert!(rendered.contains("jdbc:h2:mem:testdb"));
        assert!(rendered.contains("server.port=8081"));
    }

    #[test]
    fn test_gitignore_renders() {
        let template = GitignoreTemplate::new().expect("Failed to create gitignore template");
        let bindings = HashMap::new();

        let rendered = template.render(&bindings).expect("Render failed");
        assert!(rendered.contains("target/"));
        assert!(rendered.contains(".idea/"));
        assert!(rendered.contains("*.log"));
        assert!(rendered.contains("application-local.properties"));
    }

    #[test]
    fn test_pom_contains_required_dependencies() {
        let template = PomTemplate::new().expect("Failed to create POM template");
        let mut bindings = HashMap::new();
        bindings.insert("packageName".to_string(), "org.test.app".to_string());
        bindings.insert("appName".to_string(), "TestApp".to_string());
        bindings.insert("version".to_string(), "1.0.0".to_string());
        bindings.insert("springBootVersion".to_string(), "3.3.0".to_string());

        let rendered = template.render(&bindings).expect("Render failed");
        // Check for all required dependencies
        assert!(rendered.contains("spring-boot-starter-web"));
        assert!(rendered.contains("spring-boot-starter-data-jpa"));
        assert!(rendered.contains("jakarta.persistence-api"));
        assert!(rendered.contains("hibernate-core"));
        assert!(rendered.contains("jackson-databind"));
        assert!(rendered.contains("postgresql"));
        assert!(rendered.contains("h2"));
        assert!(rendered.contains("spring-boot-maven-plugin"));
        assert!(rendered.contains("maven-compiler-plugin"));
    }

    #[test]
    fn test_pom_java_version_21() {
        let template = PomTemplate::new().expect("Failed to create POM template");
        let mut bindings = HashMap::new();
        bindings.insert("packageName".to_string(), "org.test.app".to_string());
        bindings.insert("appName".to_string(), "TestApp".to_string());
        bindings.insert("version".to_string(), "1.0.0".to_string());
        bindings.insert("springBootVersion".to_string(), "3.3.0".to_string());

        let rendered = template.render(&bindings).expect("Render failed");
        assert!(rendered.contains("<java.version>21</java.version>"));
        assert!(rendered.contains("<source>21</source>"));
        assert!(rendered.contains("<target>21</target>"));
    }
}
