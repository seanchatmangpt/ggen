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
pub struct SpringBootAppQuery {
    query: String,
}

impl SpringBootAppQuery {
    /// Create a new Spring Boot app query.
    pub fn new() -> Self {
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

    /// SPARQL query text
    pub fn sparql(&self) -> &str {
        &self.query
    }
}

impl Queryable for SpringBootAppQuery {
    fn execute(&self) -> CodegenResult<Vec<HashMap<String, String>>> {
        let mut results = Vec::new();
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
    pub fn new() -> Result<Self> {
        let mut tera = Tera::default();
        let template = r#"<?xml version="1.0" encoding="UTF-8"?>
<project xmlns="http://maven.apache.org/POM/4.0.0"
         xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
         xsi:schemaLocation="http://maven.apache.org/POM/4.0.0
                            http://maven.apache.org/xsd/maven-4.0.0.xsd">
    <modelVersion>4.0.0</modelVersion>
    <parent>
        <groupId>org.springframework.boot</groupId>
        <artifactId>spring-boot-starter-parent</artifactId>
        <version>{{ springBootVersion }}</version>
        <relativePath/>
    </parent>
    <groupId>{{ packageName }}</groupId>
    <artifactId>yawl-app</artifactId>
    <version>{{ version }}</version>
    <name>{{ appName }}</name>
    <description>YAWL Workflow Engine - Spring Boot Application</description>
    <packaging>jar</packaging>
    <properties>
        <project.build.sourceEncoding>UTF-8</project.build.sourceEncoding>
        <maven.compiler.source>21</maven.compiler.source>
        <maven.compiler.target>21</maven.compiler.target>
        <java.version>21</java.version>
    </properties>
    <dependencies>
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
        <dependency>
            <groupId>jakarta.persistence</groupId>
            <artifactId>jakarta.persistence-api</artifactId>
            <version>3.2.0</version>
        </dependency>
        <dependency>
            <groupId>org.hibernate.orm</groupId>
            <artifactId>hibernate-core</artifactId>
            <version>6.4.4.Final</version>
        </dependency>
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
        <dependency>
            <groupId>org.postgresql</groupId>
            <artifactId>postgresql</artifactId>
            <version>42.7.2</version>
            <scope>runtime</scope>
        </dependency>
        <dependency>
            <groupId>com.h2database</groupId>
            <artifactId>h2</artifactId>
            <version>2.2.224</version>
            <scope>test</scope>
        </dependency>
        <dependency>
            <groupId>org.projectlombok</groupId>
            <artifactId>lombok</artifactId>
            <optional>true</optional>
        </dependency>
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
    <build>
        <plugins>
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
        let package_name = bindings
            .get("packageName")
            .ok_or_else(|| CodegenError::template("Missing packageName".to_string()))?
            .clone();
        let app_name = bindings.get("appName").unwrap_or(&"YawlApplication".to_string()).clone();
        let version = bindings.get("version").unwrap_or(&"0.1.0".to_string()).clone();
        let spring_boot_version = bindings.get("springBootVersion").unwrap_or(&"3.3.0".to_string()).clone();
        context.insert("packageName", &package_name);
        context.insert("appName", &app_name);
        context.insert("version", &version);
        context.insert("springBootVersion", &spring_boot_version);
        self.tera
            .render("pom.xml.tera", &context)
            .map_err(|e| CodegenError::template(format!("Failed to render pom.xml: {}", e)))
    }

    fn name(&self) -> &str {
        "pom-template"
    }
}

pub struct ApplicationTemplate {
    tera: Tera,
}

impl ApplicationTemplate {
    pub fn new() -> Result<Self> {
        let mut tera = Tera::default();
        let template = r#"package {{ packageName }};
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.transaction.annotation.EnableTransactionManagement;

@SpringBootApplication
@EnableTransactionManagement
public class YawlApplication {
    public static void main(String[] args) {
        SpringApplication.run(YawlApplication.class, args);
    }
}"#;
        tera.add_raw_template("YawlApplication.java.tera", template)
            .map_err(|e| Error::template(format!("Failed to load application template: {}", e)))?;
        Ok(Self { tera })
    }
}

impl Renderable for ApplicationTemplate {
    fn render(&self, bindings: &HashMap<String, String>) -> CodegenResult<String> {
        let mut context = Context::new();
        let package_name = bindings.get("packageName").ok_or_else(|| CodegenError::template("Missing packageName".to_string()))?.clone();
        let version = bindings.get("version").unwrap_or(&"0.1.0".to_string()).clone();
        context.insert("packageName", &package_name);
        context.insert("version", &version);
        self.tera.render("YawlApplication.java.tera", &context)
            .map_err(|e| CodegenError::template(format!("Failed to render YawlApplication.java: {}", e)))
    }

    fn name(&self) -> &str {
        "application-template"
    }
}

pub struct ApplicationPropertiesTemplate {
    tera: Tera,
}

impl ApplicationPropertiesTemplate {
    pub fn new() -> Result<Self> {
        let mut tera = Tera::default();
        let template = r#"spring.application.name={{ appName }}
spring.application.version={{ version }}
spring.jpa.database-platform=org.hibernate.dialect.PostgreSQLDialect
spring.jpa.hibernate.ddl-auto=validate
spring.jpa.show-sql=false
spring.jpa.properties.hibernate.format_sql=true
spring.jpa.properties.hibernate.jdbc.batch_size=20
spring.datasource.url=jdbc:postgresql://localhost:5432/yawl
spring.datasource.username=yawl
spring.datasource.password=yawl
spring.datasource.driver-class-name=org.postgresql.Driver
spring.datasource.hikari.maximum-pool-size=10
logging.level.root=INFO
server.port=8080
server.servlet.context-path=/api"#;
        tera.add_raw_template("application.properties.tera", template)
            .map_err(|e| Error::template(format!("Failed to load properties template: {}", e)))?;
        Ok(Self { tera })
    }
}

impl Renderable for ApplicationPropertiesTemplate {
    fn render(&self, bindings: &HashMap<String, String>) -> CodegenResult<String> {
        let mut context = Context::new();
        let app_name = bindings.get("appName").unwrap_or(&"YawlApplication".to_string()).clone();
        let version = bindings.get("version").unwrap_or(&"0.1.0".to_string()).clone();
        context.insert("appName", &app_name);
        context.insert("version", &version);
        self.tera.render("application.properties.tera", &context)
            .map_err(|e| CodegenError::template(format!("Failed to render application.properties: {}", e)))
    }

    fn name(&self) -> &str {
        "application-properties-template"
    }
}

pub struct ApplicationTestPropertiesTemplate {
    tera: Tera,
}

impl ApplicationTestPropertiesTemplate {
    pub fn new() -> Result<Self> {
        let mut tera = Tera::default();
        let template = r#"spring.application.name={{ appName }}-test
spring.datasource.url=jdbc:h2:mem:testdb
spring.datasource.driverClassName=org.h2.Driver
spring.jpa.hibernate.ddl-auto=create-drop
spring.h2.console.enabled=true
server.port=8081"#;
        tera.add_raw_template("application-test.properties.tera", template)
            .map_err(|e| Error::template(format!("Failed to load test properties template: {}", e)))?;
        Ok(Self { tera })
    }
}

impl Renderable for ApplicationTestPropertiesTemplate {
    fn render(&self, bindings: &HashMap<String, String>) -> CodegenResult<String> {
        let mut context = Context::new();
        let app_name = bindings.get("appName").unwrap_or(&"YawlApplication".to_string()).clone();
        context.insert("appName", &app_name);
        self.tera.render("application-test.properties.tera", &context)
            .map_err(|e| CodegenError::template(format!("Failed to render application-test.properties: {}", e)))
    }

    fn name(&self) -> &str {
        "application-test-properties-template"
    }
}

pub struct GitignoreTemplate {
    tera: Tera,
}

impl GitignoreTemplate {
    pub fn new() -> Result<Self> {
        let mut tera = Tera::default();
        let template = r#"target/
.idea/
.vscode/
*.log
*.jar
*.class
.DS_Store
application-local.properties"#;
        tera.add_raw_template("gitignore.tera", template)
            .map_err(|e| Error::template(format!("Failed to load gitignore template: {}", e)))?;
        Ok(Self { tera })
    }
}

impl Renderable for GitignoreTemplate {
    fn render(&self, _bindings: &HashMap<String, String>) -> CodegenResult<String> {
        let context = Context::new();
        self.tera.render("gitignore.tera", &context)
            .map_err(|e| CodegenError::template(format!("Failed to render .gitignore: {}", e)))
    }

    fn name(&self) -> &str {
        "gitignore-template"
    }
}

pub fn create_spring_boot_app_rule() -> Result<Rule<SpringBootAppQuery, PomTemplate>> {
    let query = SpringBootAppQuery::new();
    let template = PomTemplate::new()?;
    let rule = Rule::new("spring-boot-pom", query, template, PathBuf::from("pom.xml"), GenerationMode::Overwrite);
    Ok(rule)
}

pub fn create_application_main_rule() -> Result<Rule<SpringBootAppQuery, ApplicationTemplate>> {
    let query = SpringBootAppQuery::new();
    let template = ApplicationTemplate::new()?;
    let rule = Rule::new("spring-boot-app", query, template, PathBuf::from("src/main/java/org/yawlfoundation/yawl/YawlApplication.java"), GenerationMode::Overwrite);
    Ok(rule)
}

pub fn create_application_properties_rule() -> Result<Rule<SpringBootAppQuery, ApplicationPropertiesTemplate>> {
    let query = SpringBootAppQuery::new();
    let template = ApplicationPropertiesTemplate::new()?;
    let rule = Rule::new("spring-boot-props", query, template, PathBuf::from("src/main/resources/application.properties"), GenerationMode::Overwrite);
    Ok(rule)
}

pub fn create_application_test_properties_rule() -> Result<Rule<SpringBootAppQuery, ApplicationTestPropertiesTemplate>> {
    let query = SpringBootAppQuery::new();
    let template = ApplicationTestPropertiesTemplate::new()?;
    let rule = Rule::new("spring-boot-test-props", query, template, PathBuf::from("src/test/resources/application-test.properties"), GenerationMode::Overwrite);
    Ok(rule)
}

pub fn create_gitignore_rule() -> Result<Rule<SpringBootAppQuery, GitignoreTemplate>> {
    let query = SpringBootAppQuery::new();
    let template = GitignoreTemplate::new()?;
    let rule = Rule::new("gitignore", query, template, PathBuf::from(".gitignore"), GenerationMode::Overwrite);
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
    }

    #[test]
    fn test_all_rule_factories() {
        let _pom = create_spring_boot_app_rule().expect("POM rule creation failed");
        let _app = create_application_main_rule().expect("App rule creation failed");
        let _props = create_application_properties_rule().expect("Props rule creation failed");
        let _test_props = create_application_test_properties_rule().expect("Test props rule creation failed");
        let _gitignore = create_gitignore_rule().expect("Gitignore rule creation failed");
    }
}
