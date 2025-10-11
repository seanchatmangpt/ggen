<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [Microservice Architecture Generator](#microservice-architecture-generator)
  - [Context](#context)
  - [Problem](#problem)
  - [Forces](#forces)
  - [Solution](#solution)
  - [Implementation](#implementation)
    - [Service Template](#service-template)
    - [Infrastructure Template](#infrastructure-template)
  - [Usage](#usage)
  - [Verification](#verification)
  - [Benefits](#benefits)
  - [Next Steps](#next-steps)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Microservice Architecture Generator

## Context

Building microservice architectures is complex and error-prone. Teams need consistent patterns for service discovery, communication, data management, and deployment across multiple services.

## Problem

**How can we generate complete microservice ecosystems with consistent patterns, proper service discovery, and deployment automation?**

## Forces

- **Consistency**: All services should follow the same architectural patterns
- **Discoverability**: Services need to find and communicate with each other
- **Scalability**: Architecture should support horizontal scaling
- **Observability**: Comprehensive monitoring and logging across services
- **Security**: Secure service-to-service communication
- **Maintainability**: Easy to add, modify, and remove services

## Solution

**Generate complete microservice ecosystems using a template composition approach with service mesh integration, automated discovery, and deployment orchestration.**

## Implementation

### Service Template

```yaml
---
name: "Microservice"
description: "Generate a complete microservice with API, database, and deployment"
variables:
  - name: service_name
    description: "Name of the service (e.g., 'user-service')"
    type: string
    required: true
  - name: port
    description: "Port for the service"
    type: number
    default: 8080
  - name: database_type
    description: "Type of database (postgres, mongodb, redis)"
    type: enum
    values: ["postgres", "mongodb", "redis"]
    default: "postgres"
  - name: language
    description: "Programming language for the service"
    type: enum
    values: ["rust", "go", "nodejs", "python"]
    default: "rust"
dependencies:
  - service: "api-gateway"
    type: "depends_on"
  - service: "service-registry"
    type: "communicates_with"
---
# {{service_name}}/Dockerfile
FROM {{language}}-runtime:latest

WORKDIR /app
COPY . .

{{#if (eq language "rust")}}
RUN cargo build --release
EXPOSE {{port}}
CMD ["./target/release/{{service_name}}"]
{{/if}}

{{#if (eq language "go")}}
RUN go build -o {{service_name}} .
EXPOSE {{port}}
CMD ["./{{service_name}}"]
{{/if}}

{{#if (eq language "nodejs")}}
EXPOSE {{port}}
CMD ["npm", "start"]
{{/if}}
---
# {{service_name}}/src/main.rs (Rust example)
use actix_web::{web, App, HttpServer, Result};
use serde::{Deserialize, Serialize};

#[derive(Debug, Serialize, Deserialize)]
pub struct {{pascal_case service_name}}Request {
    pub id: String,
    pub data: serde_json::Value,
}

async fn health_check() -> Result<impl actix_web::Responder> {
    Ok("OK")
}

async fn create_{{snake_case service_name}}(
    req: web::Json<{{pascal_case service_name}}Request>
) -> Result<impl actix_web::Responder> {
    // TODO: Implement service logic
    Ok(format!("Created: {:?}", req.0))
}

#[actix_web::main]
async fn main() -> std::io::Result<()> {
    HttpServer::new(|| {
        App::new()
            .route("/health", web::get().to(health_check))
            .route("/{{kebab_case service_name}}", web::post().to(create_{{snake_case service_name}}))
    })
    .bind("0.0.0.0:{{port}}")?
    .run()
    .await
}
---
# {{service_name}}/docker-compose.yml
version: '3.8'
services:
  {{service_name}}:
    build: .
    ports:
      - "{{port}}:{{port}}"
    environment:
      - SERVICE_NAME={{service_name}}
      - CONSUL_HOST=consul
      - DATABASE_URL=postgresql://user:pass@postgres:5432/{{service_name}}
    depends_on:
      - consul
      - postgres
    networks:
      - microservice-network

  postgres:
    image: postgres:15
    environment:
      POSTGRES_DB: {{service_name}}
      POSTGRES_USER: user
      POSTGRES_PASSWORD: pass
    volumes:
      - postgres_data:/var/lib/postgresql/data
    networks:
      - microservice-network

  consul:
    image: consul:latest
    command: agent -server -bootstrap-expect=1 -ui -client=0.0.0.0
    ports:
      - "8500:8500"
    networks:
      - microservice-network

volumes:
  postgres_data:

networks:
  microservice-network:
    driver: bridge
---
# {{service_name}}/k8s/deployment.yml
apiVersion: apps/v1
kind: Deployment
metadata:
  name: {{service_name}}
  labels:
    app: {{service_name}}
spec:
  replicas: 3
  selector:
    matchLabels:
      app: {{service_name}}
  template:
    metadata:
      labels:
        app: {{service_name}}
      annotations:
        consul.hashicorp.com/connect-inject: "true"
    spec:
      containers:
      - name: {{service_name}}
        image: {{service_name}}:latest
        ports:
        - containerPort: {{port}}
        env:
        - name: SERVICE_NAME
          value: "{{service_name}}"
        - name: CONSUL_HTTP_ADDR
          value: "consul-server:8500"
---
# {{service_name}}/k8s/service.yml
apiVersion: v1
kind: Service
metadata:
  name: {{service_name}}
spec:
  selector:
    app: {{service_name}}
  ports:
  - port: {{port}}
    targetPort: {{port}}
  type: ClusterIP
---
# {{service_name}}/tests/integration_test.rs
#[cfg(test)]
mod tests {
    use super::*;
    use actix_web::{test, App};

    #[actix_rt::test]
    async fn test_health_check() {
        let app = test::init_service(App::new().route("/health", web::get().to(health_check))).await;
        let req = test::TestRequest::get().uri("/health").to_request();
        let resp = test::call_service(&app, req).await;

        assert!(resp.status().is_success());
    }
}
```

### Infrastructure Template

```yaml
---
name: "Microservice Infrastructure"
description: "Generate complete infrastructure for microservice ecosystem"
variables:
  - name: services
    description: "List of services to generate"
    type: array
    required: true
  - name: environment
    description: "Deployment environment"
    type: enum
    values: ["development", "staging", "production"]
    default: "development"
---
# docker-compose.yml (Complete ecosystem)
version: '3.8'
services:
{{#each services}}
  {{this.name}}:
    build: ./{{this.name}}
    ports:
      - "{{this.port}}:{{this.port}}"
    environment:
      - SERVICE_NAME={{this.name}}
      - ENVIRONMENT={{../environment}}
    depends_on:
      - consul
      - postgres
      - redis
    networks:
      - microservice-network

{{/each}}

  # Infrastructure services
  consul:
    image: consul:latest
    command: agent -server -bootstrap-expect=1 -ui -client=0.0.0.0
    ports:
      - "8500:8500"
    networks:
      - microservice-network

  postgres:
    image: postgres:15
    environment:
      POSTGRES_DB: microservices
      POSTGRES_USER: user
      POSTGRES_PASSWORD: pass
    volumes:
      - postgres_data:/var/lib/postgresql/data
    ports:
      - "5432:5432"
    networks:
      - microservice-network

  redis:
    image: redis:7-alpine
    ports:
      - "6379:6379"
    networks:
      - microservice-network

  # Monitoring
  prometheus:
    image: prom/prometheus:latest
    ports:
      - "9090:9090"
    volumes:
      - ./monitoring/prometheus.yml:/etc/prometheus/prometheus.yml
    networks:
      - microservice-network

  grafana:
    image: grafana/grafana:latest
    ports:
      - "3000:3000"
    environment:
      - GF_SECURITY_ADMIN_PASSWORD=admin
    networks:
      - microservice-network

volumes:
  postgres_data:

networks:
  microservice-network:
    driver: bridge
---
# k8s/namespace.yml
apiVersion: v1
kind: Namespace
metadata:
  name: {{environment}}-microservices
  labels:
    environment: {{environment}}
---
# k8s/ingress.yml
apiVersion: networking.k8s.io/v1
kind: Ingress
metadata:
  name: microservice-ingress
  namespace: {{environment}}-microservices
spec:
  ingressClassName: nginx
  rules:
  {{#each services}}
  - host: {{this.name}}.{{../environment}}.local
    http:
      paths:
      - path: /
        pathType: Prefix
        backend:
          service:
            name: {{this.name}}
            port:
              number: {{this.port}}
  {{/each}}
---
# monitoring/prometheus.yml
global:
  scrape_interval: 15s

scrape_configs:
  - job_name: 'prometheus'
    static_configs:
      - targets: ['localhost:9090']

  - job_name: 'microservices'
    kubernetes_sd_configs:
    - role: endpoints
      namespaces:
        names:
        - {{environment}}-microservices

  - job_name: 'consul'
    static_configs:
      - targets: ['consul:8500']
```

## Usage

1. **Generate individual services**:
   ```bash
   ggen generate microservice.tmpl \
     --set service_name=user-service \
     --set port=8081 \
     --set language=rust \
     --output ./services/user-service
   ```

2. **Generate complete ecosystem**:
   ```bash
   ggen generate microservice-infrastructure.tmpl \
     --set services='[{"name":"user-service","port":8081},{"name":"order-service","port":8082}]' \
     --set environment=development \
     --output ./microservice-ecosystem
   ```

3. **Deploy ecosystem**:
   ```bash
   cd ./microservice-ecosystem
   docker-compose up -d
   ```

## Verification

1. **Test service discovery**:
   ```bash
   curl http://consul:8500/v1/agent/services
   ```

2. **Test service communication**:
   ```bash
   curl http://user-service:8081/health
   curl http://order-service:8082/health
   ```

3. **Test monitoring**:
   ```bash
   curl http://prometheus:9090/api/v1/query?query=up
   ```

## Benefits

- **🚀 Rapid Prototyping**: Complete microservice ecosystem in minutes
- **🔧 Consistency**: All services follow the same architectural patterns
- **📊 Observability**: Built-in monitoring and logging
- **🔒 Security**: Service mesh provides secure communication
- **⚡ Scalability**: Easy horizontal scaling with Kubernetes
- **🔄 Reliability**: Health checks and circuit breakers

## Next Steps

- **Service Mesh Integration**: Add Istio or Linkerd for advanced traffic management
- **API Gateway**: Integrate with Kong or Ambassador for external API management
- **Database Per Service**: Add database-specific patterns for each service type
- **Event-Driven Architecture**: Add event sourcing and CQRS patterns
