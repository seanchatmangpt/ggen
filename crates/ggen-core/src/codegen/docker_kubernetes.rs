//! Docker & Kubernetes manifest generation
//!
//! This module generates production-ready container and orchestration artifacts:
//! - Multi-stage Dockerfiles (language-aware)
//! - Kubernetes Deployment manifests
//! - Kubernetes Service manifests
//! - ConfigMap templates (configuration injection)
//! - Helm values.yaml templates

use std::fmt::Write;

/// Docker image selection by programming language
#[derive(Debug, Clone)]
pub enum Language {
    Rust,
    Go,
    Elixir,
    TypeScript,
    Python,
    Java,
}

impl Language {
    pub fn builder_image(&self) -> &'static str {
        match self {
            Language::Rust => "rust:latest",
            Language::Go => "golang:1.24-alpine",
            Language::Elixir => "elixir:latest",
            Language::TypeScript => "node:20-alpine",
            Language::Python => "python:3.12-slim",
            Language::Java => "openjdk:21-jdk-slim",
        }
    }

    pub fn runtime_image(&self) -> &'static str {
        match self {
            Language::Rust => "debian:bookworm-slim",
            Language::Go => "alpine:latest",
            Language::Elixir => "erlang:27-slim",
            Language::TypeScript => "node:20-alpine",
            Language::Python => "python:3.12-slim",
            Language::Java => "openjdk:21-jre-slim",
        }
    }

    pub fn build_command(&self) -> &'static str {
        match self {
            Language::Rust => "cargo build --release",
            Language::Go => "CGO_ENABLED=0 GOOS=linux go build -o app .",
            Language::Elixir => "MIX_ENV=prod mix deps.get && mix compile && mix release",
            Language::TypeScript => "npm ci && npm run build",
            Language::Python => "pip install --no-cache-dir -r requirements.txt",
            Language::Java => "mvnd clean package -DskipTests",
        }
    }

    pub fn binary_path(&self) -> &'static str {
        match self {
            Language::Rust => "/app/target/release/myapp",
            Language::Go => "/app/app",
            Language::Elixir => "/app/_build/prod/rel/app/bin/app",
            Language::TypeScript => "/app/dist/index.js",
            Language::Python => "/app",
            Language::Java => "/app/target/*.jar",
        }
    }
}

/// Service specification for Kubernetes manifests
#[derive(Debug, Clone)]
pub struct ServiceSpec {
    pub name: String,
    pub service_type: String,           // "ClusterIP", "LoadBalancer", "NodePort"
    pub replicas: u32,
    pub port: u32,
    pub target_port: u32,
    pub image: String,
    pub registry: String,               // "myregistry.azurecr.io"
    pub tag: String,
    pub env_vars: Vec<(String, String)>, // key, value pairs
}

impl Default for ServiceSpec {
    fn default() -> Self {
        Self {
            name: "default-service".to_string(),
            service_type: "ClusterIP".to_string(),
            replicas: 3,
            port: 8001,
            target_port: 8001,
            image: "default-app".to_string(),
            registry: "myregistry.azurecr.io".to_string(),
            tag: "latest".to_string(),
            env_vars: vec![],
        }
    }
}

/// Docker & Kubernetes manifest generator
pub struct DockerKubernetesGenerator;

impl DockerKubernetesGenerator {
    /// Generate multi-stage Dockerfile for a given language
    pub fn generate_dockerfile(language: &Language, service_name: &str) -> Result<String, String> {
        let mut output = String::new();

        writeln!(output, "# Multi-stage Dockerfile for {}", service_name)
            .map_err(|e| e.to_string())?;
        writeln!(output, "# Generated: production-ready container").map_err(|e| e.to_string())?;

        // Stage 1: Builder
        writeln!(output, "\nFROM {} as builder", language.builder_image())
            .map_err(|e| e.to_string())?;
        writeln!(output, "WORKDIR /app").map_err(|e| e.to_string())?;
        writeln!(output, "COPY . .").map_err(|e| e.to_string())?;

        // Language-specific build commands
        match language {
            Language::Rust => {
                writeln!(output, "RUN {} && cargo clean", language.build_command())
                    .map_err(|e| e.to_string())?;
            }
            Language::Go => {
                writeln!(output, "RUN {}", language.build_command()).map_err(|e| e.to_string())?;
            }
            Language::Elixir => {
                writeln!(output, "RUN {}", language.build_command()).map_err(|e| e.to_string())?;
            }
            Language::TypeScript => {
                writeln!(output, "RUN npm ci").map_err(|e| e.to_string())?;
                writeln!(output, "RUN {}", language.build_command()).map_err(|e| e.to_string())?;
            }
            Language::Python => {
                writeln!(output, "COPY requirements.txt .").map_err(|e| e.to_string())?;
                writeln!(output, "RUN {}", language.build_command()).map_err(|e| e.to_string())?;
            }
            Language::Java => {
                writeln!(output, "RUN {}", language.build_command()).map_err(|e| e.to_string())?;
            }
        }

        // Stage 2: Runtime
        writeln!(output, "\nFROM {}", language.runtime_image()).map_err(|e| e.to_string())?;
        writeln!(output, "WORKDIR /app").map_err(|e| e.to_string())?;
        writeln!(output, "RUN apt-get update && apt-get install -y ca-certificates && rm -rf /var/lib/apt/lists/* || true")
            .map_err(|e| e.to_string())?;

        // Copy built artifacts
        match language {
            Language::Rust => {
                writeln!(
                    output,
                    "COPY --from=builder /app/target/release/{} /app/{}",
                    service_name, service_name
                )
                .map_err(|e| e.to_string())?;
                writeln!(output, "ENTRYPOINT [\"/app/{}\"]", service_name)
                    .map_err(|e| e.to_string())?;
            }
            Language::Go => {
                writeln!(output, "COPY --from=builder /app/app /app/app")
                    .map_err(|e| e.to_string())?;
                writeln!(output, "ENTRYPOINT [\"/app/app\"]").map_err(|e| e.to_string())?;
            }
            Language::Elixir => {
                writeln!(output, "COPY --from=builder /app/_build/prod/rel /app/rel")
                    .map_err(|e| e.to_string())?;
                writeln!(output, "ENTRYPOINT [\"/app/rel/app/bin/app\"]")
                    .map_err(|e| e.to_string())?;
            }
            Language::TypeScript => {
                writeln!(output, "COPY --from=builder /app/node_modules /app/node_modules")
                    .map_err(|e| e.to_string())?;
                writeln!(output, "COPY --from=builder /app/dist /app/dist")
                    .map_err(|e| e.to_string())?;
                writeln!(output, "CMD [\"node\", \"dist/index.js\"]")
                    .map_err(|e| e.to_string())?;
            }
            Language::Python => {
                writeln!(output, "COPY --from=builder /usr/local/lib /usr/local/lib")
                    .map_err(|e| e.to_string())?;
                writeln!(output, "COPY . /app").map_err(|e| e.to_string())?;
                writeln!(output, "CMD [\"python\", \"app.py\"]").map_err(|e| e.to_string())?;
            }
            Language::Java => {
                writeln!(
                    output,
                    "COPY --from=builder /app/target/*.jar /app/app.jar"
                )
                .map_err(|e| e.to_string())?;
                writeln!(output, "ENTRYPOINT [\"java\", \"-jar\", \"/app/app.jar\"]")
                    .map_err(|e| e.to_string())?;
            }
        }

        // Healthcheck
        writeln!(output, "\nHEALTHCHECK --interval=30s --timeout=3s --start-period=5s --retries=3 \\")
            .map_err(|e| e.to_string())?;
        writeln!(output, "    CMD curl -f http://localhost:8080/health || exit 1")
            .map_err(|e| e.to_string())?;

        Ok(output)
    }

    /// Generate Kubernetes Deployment manifest
    pub fn generate_k8s_deployment(spec: &ServiceSpec) -> Result<String, String> {
        let mut output = String::new();

        writeln!(output, "apiVersion: apps/v1").map_err(|e| e.to_string())?;
        writeln!(output, "kind: Deployment").map_err(|e| e.to_string())?;
        writeln!(output, "metadata:").map_err(|e| e.to_string())?;
        writeln!(output, "  name: {}", spec.name).map_err(|e| e.to_string())?;
        writeln!(output, "  labels:").map_err(|e| e.to_string())?;
        writeln!(output, "    app: {}", spec.name).map_err(|e| e.to_string())?;
        writeln!(output, "    version: v1").map_err(|e| e.to_string())?;

        writeln!(output, "spec:").map_err(|e| e.to_string())?;
        writeln!(output, "  replicas: {}", spec.replicas).map_err(|e| e.to_string())?;
        writeln!(output, "  selector:").map_err(|e| e.to_string())?;
        writeln!(output, "    matchLabels:").map_err(|e| e.to_string())?;
        writeln!(output, "      app: {}", spec.name).map_err(|e| e.to_string())?;

        writeln!(output, "  template:").map_err(|e| e.to_string())?;
        writeln!(output, "    metadata:").map_err(|e| e.to_string())?;
        writeln!(output, "      labels:").map_err(|e| e.to_string())?;
        writeln!(output, "        app: {}", spec.name).map_err(|e| e.to_string())?;
        writeln!(output, "    spec:").map_err(|e| e.to_string())?;

        writeln!(output, "      containers:").map_err(|e| e.to_string())?;
        writeln!(output, "      - name: {}", spec.name).map_err(|e| e.to_string())?;
        writeln!(
            output,
            "        image: {}/{}:{}",
            spec.registry, spec.image, spec.tag
        )
        .map_err(|e| e.to_string())?;
        writeln!(output, "        imagePullPolicy: Always").map_err(|e| e.to_string())?;

        writeln!(output, "        ports:").map_err(|e| e.to_string())?;
        writeln!(output, "        - name: http").map_err(|e| e.to_string())?;
        writeln!(output, "          containerPort: {}", spec.target_port)
            .map_err(|e| e.to_string())?;
        writeln!(output, "          protocol: TCP").map_err(|e| e.to_string())?;

        // Environment variables
        if !spec.env_vars.is_empty() {
            writeln!(output, "        env:").map_err(|e| e.to_string())?;
            for (key, value) in &spec.env_vars {
                writeln!(output, "        - name: {}", key).map_err(|e| e.to_string())?;
                writeln!(output, "          value: \"{}\"", value).map_err(|e| e.to_string())?;
            }
        }

        // Liveness and readiness probes
        writeln!(output, "        livenessProbe:").map_err(|e| e.to_string())?;
        writeln!(output, "          httpGet:").map_err(|e| e.to_string())?;
        writeln!(output, "            path: /health").map_err(|e| e.to_string())?;
        writeln!(output, "            port: {}", spec.target_port)
            .map_err(|e| e.to_string())?;
        writeln!(output, "          initialDelaySeconds: 10").map_err(|e| e.to_string())?;
        writeln!(output, "          periodSeconds: 10").map_err(|e| e.to_string())?;
        writeln!(output, "          timeoutSeconds: 5").map_err(|e| e.to_string())?;
        writeln!(output, "          failureThreshold: 3").map_err(|e| e.to_string())?;

        writeln!(output, "        readinessProbe:").map_err(|e| e.to_string())?;
        writeln!(output, "          httpGet:").map_err(|e| e.to_string())?;
        writeln!(output, "            path: /ready").map_err(|e| e.to_string())?;
        writeln!(output, "            port: {}", spec.target_port)
            .map_err(|e| e.to_string())?;
        writeln!(output, "          initialDelaySeconds: 5").map_err(|e| e.to_string())?;
        writeln!(output, "          periodSeconds: 5").map_err(|e| e.to_string())?;

        // Resource limits
        writeln!(output, "        resources:").map_err(|e| e.to_string())?;
        writeln!(output, "          requests:").map_err(|e| e.to_string())?;
        writeln!(output, "            cpu: 100m").map_err(|e| e.to_string())?;
        writeln!(output, "            memory: 128Mi").map_err(|e| e.to_string())?;
        writeln!(output, "          limits:").map_err(|e| e.to_string())?;
        writeln!(output, "            cpu: 500m").map_err(|e| e.to_string())?;
        writeln!(output, "            memory: 512Mi").map_err(|e| e.to_string())?;

        Ok(output)
    }

    /// Generate Kubernetes Service manifest
    pub fn generate_k8s_service(spec: &ServiceSpec) -> Result<String, String> {
        let mut output = String::new();

        writeln!(output, "apiVersion: v1").map_err(|e| e.to_string())?;
        writeln!(output, "kind: Service").map_err(|e| e.to_string())?;
        writeln!(output, "metadata:").map_err(|e| e.to_string())?;
        writeln!(output, "  name: {}", spec.name).map_err(|e| e.to_string())?;
        writeln!(output, "  labels:").map_err(|e| e.to_string())?;
        writeln!(output, "    app: {}", spec.name).map_err(|e| e.to_string())?;

        writeln!(output, "spec:").map_err(|e| e.to_string())?;
        writeln!(output, "  type: {}", spec.service_type).map_err(|e| e.to_string())?;
        writeln!(output, "  selector:").map_err(|e| e.to_string())?;
        writeln!(output, "    app: {}", spec.name).map_err(|e| e.to_string())?;

        writeln!(output, "  ports:").map_err(|e| e.to_string())?;
        writeln!(output, "  - name: http").map_err(|e| e.to_string())?;
        writeln!(output, "    port: {}", spec.port).map_err(|e| e.to_string())?;
        writeln!(output, "    targetPort: {}", spec.target_port)
            .map_err(|e| e.to_string())?;
        writeln!(output, "    protocol: TCP").map_err(|e| e.to_string())?;

        // Load balancer specific configuration
        if spec.service_type == "LoadBalancer" {
            writeln!(output, "  sessionAffinity: ClientIP").map_err(|e| e.to_string())?;
            writeln!(output, "  sessionAffinityConfig:").map_err(|e| e.to_string())?;
            writeln!(output, "    clientIP:").map_err(|e| e.to_string())?;
            writeln!(output, "      timeoutSeconds: 10800").map_err(|e| e.to_string())?;
        }

        Ok(output)
    }

    /// Generate ConfigMap template for configuration injection
    pub fn generate_configmap(
        name: &str,
        config_pairs: &[(String, String)],
    ) -> Result<String, String> {
        let mut output = String::new();

        writeln!(output, "apiVersion: v1").map_err(|e| e.to_string())?;
        writeln!(output, "kind: ConfigMap").map_err(|e| e.to_string())?;
        writeln!(output, "metadata:").map_err(|e| e.to_string())?;
        writeln!(output, "  name: {}-config", name).map_err(|e| e.to_string())?;
        writeln!(output, "  namespace: default").map_err(|e| e.to_string())?;

        writeln!(output, "data:").map_err(|e| e.to_string())?;
        for (key, value) in config_pairs {
            writeln!(output, "  {}: |", key).map_err(|e| e.to_string())?;
            // Indent YAML multiline values
            for line in value.lines() {
                writeln!(output, "    {}", line).map_err(|e| e.to_string())?;
            }
        }

        Ok(output)
    }

    /// Generate Helm values.yaml template
    pub fn generate_helm_values(services: &[ServiceSpec]) -> Result<String, String> {
        let mut output = String::new();

        writeln!(output, "# Helm values template for multi-service deployment")
            .map_err(|e| e.to_string())?;
        writeln!(output, "# Auto-generated from service specifications")
            .map_err(|e| e.to_string())?;
        writeln!(output).map_err(|e| e.to_string())?;

        writeln!(output, "# Global configuration").map_err(|e| e.to_string())?;
        writeln!(output, "global:").map_err(|e| e.to_string())?;
        writeln!(output, "  namespace: default").map_err(|e| e.to_string())?;
        writeln!(output, "  registry: myregistry.azurecr.io").map_err(|e| e.to_string())?;
        writeln!(output, "  imagePullPolicy: Always").map_err(|e| e.to_string())?;
        writeln!(output).map_err(|e| e.to_string())?;

        writeln!(output, "# Per-service overrides").map_err(|e| e.to_string())?;
        writeln!(output, "services:").map_err(|e| e.to_string())?;

        for service in services {
            writeln!(output, "  {}:", service.name).map_err(|e| e.to_string())?;
            writeln!(output, "    enabled: true").map_err(|e| e.to_string())?;
            writeln!(output, "    replicas: {}", service.replicas)
                .map_err(|e| e.to_string())?;
            writeln!(output, "    image:").map_err(|e| e.to_string())?;
            writeln!(output, "      repository: {}/{}", service.registry, service.image)
                .map_err(|e| e.to_string())?;
            writeln!(output, "      tag: \"{}\"", service.tag).map_err(|e| e.to_string())?;
            writeln!(output, "    service:").map_err(|e| e.to_string())?;
            writeln!(output, "      type: {}", service.service_type).map_err(|e| e.to_string())?;
            writeln!(output, "      port: {}", service.port).map_err(|e| e.to_string())?;
            writeln!(output, "      targetPort: {}", service.target_port)
                .map_err(|e| e.to_string())?;
            writeln!(output, "    resources:").map_err(|e| e.to_string())?;
            writeln!(output, "      requests:").map_err(|e| e.to_string())?;
            writeln!(output, "        cpu: 100m").map_err(|e| e.to_string())?;
            writeln!(output, "        memory: 128Mi").map_err(|e| e.to_string())?;
            writeln!(output, "      limits:").map_err(|e| e.to_string())?;
            writeln!(output, "        cpu: 500m").map_err(|e| e.to_string())?;
            writeln!(output, "        memory: 512Mi").map_err(|e| e.to_string())?;

            if !service.env_vars.is_empty() {
                writeln!(output, "    env:").map_err(|e| e.to_string())?;
                for (key, value) in &service.env_vars {
                    writeln!(output, "      {}: \"{}\"", key, value)
                        .map_err(|e| e.to_string())?;
                }
            }
            writeln!(output).map_err(|e| e.to_string())?;
        }

        // Add common overrides
        writeln!(output, "# Common overrides").map_err(|e| e.to_string())?;
        writeln!(output, "autoscaling:").map_err(|e| e.to_string())?;
        writeln!(output, "  enabled: true").map_err(|e| e.to_string())?;
        writeln!(output, "  minReplicas: 2").map_err(|e| e.to_string())?;
        writeln!(output, "  maxReplicas: 10").map_err(|e| e.to_string())?;
        writeln!(output, "  targetCPUUtilizationPercentage: 80")
            .map_err(|e| e.to_string())?;

        Ok(output)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_dockerfile_generation_rust() {
        let result = DockerKubernetesGenerator::generate_dockerfile(&Language::Rust, "myapp");
        assert!(result.is_ok());
        let docker = result.unwrap();
        assert!(docker.contains("FROM rust:latest as builder"));
        assert!(docker.contains("FROM debian:bookworm-slim"));
        assert!(docker.contains("cargo build --release"));
    }

    #[test]
    fn test_dockerfile_generation_go() {
        let result = DockerKubernetesGenerator::generate_dockerfile(&Language::Go, "myapp");
        assert!(result.is_ok());
        let docker = result.unwrap();
        assert!(docker.contains("FROM golang:1.24-alpine as builder"));
        assert!(docker.contains("FROM alpine:latest"));
        assert!(docker.contains("CGO_ENABLED=0"));
    }

    #[test]
    fn test_k8s_deployment_generation() {
        let spec = ServiceSpec {
            name: "order-api".to_string(),
            replicas: 3,
            port: 8001,
            target_port: 8001,
            image: "order-api".to_string(),
            ..Default::default()
        };

        let result = DockerKubernetesGenerator::generate_k8s_deployment(&spec);
        assert!(result.is_ok());
        let manifest = result.unwrap();
        assert!(manifest.contains("kind: Deployment"));
        assert!(manifest.contains("name: order-api"));
        assert!(manifest.contains("replicas: 3"));
        assert!(manifest.contains("containerPort: 8001"));
    }

    #[test]
    fn test_k8s_service_generation() {
        let spec = ServiceSpec {
            name: "order-api".to_string(),
            service_type: "LoadBalancer".to_string(),
            port: 8001,
            target_port: 8001,
            ..Default::default()
        };

        let result = DockerKubernetesGenerator::generate_k8s_service(&spec);
        assert!(result.is_ok());
        let manifest = result.unwrap();
        assert!(manifest.contains("kind: Service"));
        assert!(manifest.contains("type: LoadBalancer"));
        assert!(manifest.contains("port: 8001"));
    }

    #[test]
    fn test_configmap_generation() {
        let config = vec![
            ("DATABASE_URL".to_string(), "postgres://localhost".to_string()),
            ("API_KEY".to_string(), "secret123".to_string()),
        ];

        let result = DockerKubernetesGenerator::generate_configmap("myapp", &config);
        assert!(result.is_ok());
        let cm = result.unwrap();
        assert!(cm.contains("kind: ConfigMap"));
        assert!(cm.contains("name: myapp-config"));
        assert!(cm.contains("DATABASE_URL"));
    }

    #[test]
    fn test_helm_values_generation() {
        let specs = vec![
            ServiceSpec {
                name: "api".to_string(),
                replicas: 3,
                port: 8001,
                image: "api".to_string(),
                ..Default::default()
            },
            ServiceSpec {
                name: "worker".to_string(),
                replicas: 2,
                port: 8002,
                image: "worker".to_string(),
                ..Default::default()
            },
        ];

        let result = DockerKubernetesGenerator::generate_helm_values(&specs);
        assert!(result.is_ok());
        let values = result.unwrap();
        assert!(values.contains("services:"));
        assert!(values.contains("api:"));
        assert!(values.contains("worker:"));
        assert!(values.contains("autoscaling:"));
    }

    #[test]
    fn test_service_spec_defaults() {
        let spec = ServiceSpec::default();
        assert_eq!(spec.name, "default-service");
        assert_eq!(spec.replicas, 3);
        assert_eq!(spec.port, 8001);
    }

    #[test]
    fn test_language_builder_images() {
        assert_eq!(Language::Rust.builder_image(), "rust:latest");
        assert_eq!(Language::Go.builder_image(), "golang:1.24-alpine");
        assert_eq!(Language::Elixir.builder_image(), "elixir:latest");
        assert_eq!(Language::TypeScript.builder_image(), "node:20-alpine");
    }

    #[test]
    fn test_language_build_commands() {
        assert!(Language::Rust.build_command().contains("cargo build"));
        assert!(Language::Go.build_command().contains("go build"));
        assert!(Language::Elixir.build_command().contains("mix"));
    }
}
