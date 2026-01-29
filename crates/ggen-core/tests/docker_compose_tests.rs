//! Integration tests for Docker Compose generation
//!
//! These tests verify end-to-end Docker Compose generation from RDF
//! using Chicago TDD patterns (Arrange/Act/Assert with real collaborators).

use chicago_tdd_tools::prelude::*;
use ggen_core::docker::{DockerComposeGenerator, HealthCheck, ServiceConfig, ServicePort};
use ggen_core::Graph;
use tempfile::TempDir;

// Test basic Docker Compose generation from RDF
async_test!(test_generate_compose_from_rdf, {
    // Arrange
    let graph = Graph::new().expect("Failed to create graph");
    graph
        .insert_turtle(
            r#"
            @prefix docker: <http://ggen.io/docker#> .

            <http://example.org/redis> a docker:Service ;
                docker:name "redis" ;
                docker:image "redis:7-alpine" ;
                docker:port "6379" ;
                docker:healthCheck "redis-cli ping" ;
                docker:network "backend" .

            <http://example.org/postgres> a docker:Service ;
                docker:name "postgres" ;
                docker:image "postgres:15-alpine" ;
                docker:port "5432" .
            "#,
        )
        .expect("Failed to insert RDF");

    let generator = DockerComposeGenerator::new(&graph).expect("Failed to create generator");

    // Act
    let yaml = generator.generate().await.expect("Failed to generate compose");

    // Assert
    assert!(yaml.contains("version:"));
    assert!(yaml.contains("services:"));
    assert!(yaml.contains("redis:"));
    assert!(yaml.contains("image: redis:7-alpine"));
    assert!(yaml.contains("postgres:"));
    assert!(yaml.contains("image: postgres:15-alpine"));
    assert!(yaml.contains("6379:6379"));
    assert!(yaml.contains("5432:5432"));
    assert!(yaml.contains("networks:"));
    assert!(yaml.contains("backend:"));
});

// Test Docker Compose generation with environment variables
async_test!(test_generate_compose_with_environment, {
    // Arrange
    let graph = Graph::new().expect("Failed to create graph");
    graph
        .insert_turtle(
            r#"
            @prefix docker: <http://ggen.io/docker#> .

            <http://example.org/postgres> a docker:Service ;
                docker:name "postgres" ;
                docker:image "postgres:15-alpine" ;
                docker:port "5432" .

            <http://example.org/env1> docker:key "POSTGRES_USER" ;
                docker:value "admin" .

            <http://example.org/postgres> docker:environment <http://example.org/env1> .

            <http://example.org/env2> docker:key "POSTGRES_PASSWORD" ;
                docker:value "secret123" .

            <http://example.org/postgres> docker:environment <http://example.org/env2> .
            "#,
        )
        .expect("Failed to insert RDF");

    let generator = DockerComposeGenerator::new(&graph).expect("Failed to create generator");

    // Act
    let yaml = generator.generate().await.expect("Failed to generate compose");

    // Assert
    assert!(yaml.contains("postgres:"));
    assert!(yaml.contains("environment:"));
    assert!(yaml.contains("POSTGRES_USER: admin"));
    assert!(yaml.contains("POSTGRES_PASSWORD: secret123"));
});

// Test Docker Compose generation with volumes
async_test!(test_generate_compose_with_volumes, {
    // Arrange
    let graph = Graph::new().expect("Failed to create graph");
    graph
        .insert_turtle(
            r#"
            @prefix docker: <http://ggen.io/docker#> .

            <http://example.org/postgres> a docker:Service ;
                docker:name "postgres" ;
                docker:image "postgres:15-alpine" ;
                docker:port "5432" ;
                docker:volume "postgres-data:/var/lib/postgresql/data" .
            "#,
        )
        .expect("Failed to insert RDF");

    let generator = DockerComposeGenerator::new(&graph).expect("Failed to create generator");

    // Act
    let yaml = generator.generate().await.expect("Failed to generate compose");

    // Assert
    assert!(yaml.contains("volumes:"));
    assert!(yaml.contains("- postgres-data:/var/lib/postgresql/data"));
    assert!(yaml.contains("postgres-data:"));
    assert!(yaml.contains("driver: local"));
});

// Test Docker Compose generation with health checks
async_test!(test_generate_compose_with_healthchecks, {
    // Arrange
    let graph = Graph::new().expect("Failed to create graph");
    graph
        .insert_turtle(
            r#"
            @prefix docker: <http://ggen.io/docker#> .

            <http://example.org/redis> a docker:Service ;
                docker:name "redis" ;
                docker:image "redis:7-alpine" ;
                docker:port "6379" ;
                docker:healthCheck "redis-cli ping" .
            "#,
        )
        .expect("Failed to insert RDF");

    let generator = DockerComposeGenerator::new(&graph).expect("Failed to create generator");

    // Act
    let yaml = generator.generate().await.expect("Failed to generate compose");

    // Assert
    assert!(yaml.contains("healthcheck:"));
    assert!(yaml.contains("test:"));
    assert!(yaml.contains("- CMD"));
    assert!(yaml.contains("- redis-cli"));
    assert!(yaml.contains("- ping"));
    assert!(yaml.contains("interval:"));
    assert!(yaml.contains("timeout:"));
    assert!(yaml.contains("retries:"));
});

// Test Docker Compose generation with dependencies
async_test!(test_generate_compose_with_dependencies, {
    // Arrange
    let graph = Graph::new().expect("Failed to create graph");
    graph
        .insert_turtle(
            r#"
            @prefix docker: <http://ggen.io/docker#> .

            <http://example.org/postgres> a docker:Service ;
                docker:name "postgres" ;
                docker:image "postgres:15-alpine" ;
                docker:port "5432" .

            <http://example.org/app> a docker:Service ;
                docker:name "app" ;
                docker:image "myapp:latest" ;
                docker:port "8080" ;
                docker:dependsOn "postgres" .
            "#,
        )
        .expect("Failed to insert RDF");

    let generator = DockerComposeGenerator::new(&graph).expect("Failed to create generator");

    // Act
    let yaml = generator.generate().await.expect("Failed to generate compose");

    // Assert
    assert!(yaml.contains("app:"));
    assert!(yaml.contains("depends_on:"));
    assert!(yaml.contains("- postgres"));
});

// Test Docker Compose generation to file
async_test!(test_generate_compose_to_file, {
    // Arrange
    let temp_dir = TempDir::new().expect("Failed to create temp dir");
    let output_path = temp_dir.path().join("docker-compose.yml");

    let graph = Graph::new().expect("Failed to create graph");
    graph
        .insert_turtle(
            r#"
            @prefix docker: <http://ggen.io/docker#> .

            <http://example.org/redis> a docker:Service ;
                docker:name "redis" ;
                docker:image "redis:7-alpine" ;
                docker:port "6379" .
            "#,
        )
        .expect("Failed to insert RDF");

    let generator = DockerComposeGenerator::new(&graph).expect("Failed to create generator");

    // Act
    generator
        .generate_to_file(&output_path)
        .await
        .expect("Failed to write compose file");

    // Assert
    assert!(output_path.exists());
    let content = std::fs::read_to_string(&output_path).expect("Failed to read file");
    assert!(content.contains("version:"));
    assert!(content.contains("redis:"));
    assert!(content.contains("image: redis:7-alpine"));
});

// Test error handling for empty graph
async_test!(test_generate_compose_empty_graph, {
    // Arrange
    let graph = Graph::new().expect("Failed to create graph");
    let generator = DockerComposeGenerator::new(&graph).expect("Failed to create generator");

    // Act
    let result = generator.generate().await;

    // Assert
    assert!(result.is_err());
    let err = result.unwrap_err();
    assert!(err.to_string().contains("No services found"));
});

// Test service config builder pattern
test!(test_service_config_builder, {
    // Arrange & Act
    let service = ServiceConfig::new("redis", "redis:7-alpine")
        .with_port(ServicePort::tcp(6379, 6379))
        .with_env("REDIS_PASSWORD", "secret")
        .with_volume("redis-data:/data")
        .with_network("backend")
        .with_health_check(HealthCheck::redis())
        .with_dependency("postgres")
        .with_restart("always");

    // Assert
    assert_eq!(service.name, "redis");
    assert_eq!(service.image, "redis:7-alpine");
    assert_eq!(service.ports.len(), 1);
    assert_eq!(service.ports[0].host, 6379);
    assert_eq!(service.environment.get("REDIS_PASSWORD"), Some(&"secret".to_string()));
    assert_eq!(service.volumes, vec!["redis-data:/data"]);
    assert!(service.networks.contains(&"backend".to_string()));
    assert!(service.health_check.is_some());
    assert_eq!(service.depends_on, vec!["postgres"]);
    assert_eq!(service.restart, "always");
});

// Test service validation
test!(test_service_validation_success, {
    // Arrange
    let service = ServiceConfig::new("redis", "redis:7-alpine")
        .with_port(ServicePort::tcp(6379, 6379));

    // Act
    let result = service.validate();

    // Assert
    assert!(result.is_ok());
});

test!(test_service_validation_empty_name, {
    // Arrange
    let service = ServiceConfig::new("", "redis:7-alpine");

    // Act
    let result = service.validate();

    // Assert
    assert!(result.is_err());
    let err = result.unwrap_err();
    assert!(err.to_string().contains("name cannot be empty"));
});

test!(test_service_validation_empty_image, {
    // Arrange
    let service = ServiceConfig::new("redis", "");

    // Act
    let result = service.validate();

    // Assert
    assert!(result.is_err());
    let err = result.unwrap_err();
    assert!(err.to_string().contains("image cannot be empty"));
});

test!(test_service_validation_invalid_port, {
    // Arrange
    let service = ServiceConfig::new("redis", "redis:7")
        .with_port(ServicePort::tcp(0, 6379));

    // Act
    let result = service.validate();

    // Assert
    assert!(result.is_err());
    let err = result.unwrap_err();
    assert!(err.to_string().contains("Port numbers must be greater than 0"));
});

// Test health check presets
test!(test_health_check_redis_preset, {
    // Arrange & Act
    let health = HealthCheck::redis();

    // Assert
    assert_eq!(health.test, vec!["CMD", "redis-cli", "ping"]);
    assert_eq!(health.interval, "10s");
    assert_eq!(health.timeout, "5s");
    assert_eq!(health.retries, 5);
    assert_eq!(health.start_period, "30s");
});

test!(test_health_check_postgres_preset, {
    // Arrange & Act
    let health = HealthCheck::postgres("mydb");

    // Assert
    assert_eq!(health.test[0], "CMD-SHELL");
    assert!(health.test[1].contains("pg_isready"));
    assert!(health.test[1].contains("mydb"));
    assert_eq!(health.interval, "10s");
    assert_eq!(health.retries, 5);
});

test!(test_health_check_http_preset, {
    // Arrange & Act
    let health = HealthCheck::http("/api/health");

    // Assert
    assert!(health.test.contains(&"CMD".to_string()));
    assert!(health.test.contains(&"curl".to_string()));
    assert!(health.test.iter().any(|s| s.contains("/api/health")));
    assert_eq!(health.interval, "15s");
    assert_eq!(health.timeout, "5s");
});

// Test port configuration
test!(test_service_port_tcp, {
    // Arrange & Act
    let port = ServicePort::tcp(8080, 80);

    // Assert
    assert_eq!(port.host, 8080);
    assert_eq!(port.container, 80);
    assert_eq!(port.protocol, "tcp");
    assert_eq!(port.to_compose_format(), "8080:80");
});

test!(test_service_port_udp, {
    // Arrange & Act
    let port = ServicePort::udp(5353, 53);

    // Assert
    assert_eq!(port.host, 5353);
    assert_eq!(port.container, 53);
    assert_eq!(port.protocol, "udp");
    assert_eq!(port.to_compose_format(), "5353:53");
});

// Test custom namespace
async_test!(test_generate_compose_custom_namespace, {
    // Arrange
    let graph = Graph::new().expect("Failed to create graph");
    graph
        .insert_turtle(
            r#"
            @prefix svc: <http://example.org/service#> .

            <http://example.org/redis> a svc:Service ;
                svc:name "redis" ;
                svc:image "redis:7-alpine" ;
                svc:port "6379" .
            "#,
        )
        .expect("Failed to insert RDF");

    let generator = DockerComposeGenerator::with_namespace(&graph, "http://example.org/service#")
        .expect("Failed to create generator");

    // Act
    let yaml = generator.generate().await.expect("Failed to generate compose");

    // Assert
    assert!(yaml.contains("redis:"));
    assert!(yaml.contains("image: redis:7-alpine"));
});
