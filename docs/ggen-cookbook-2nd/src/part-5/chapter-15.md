<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [Chapter 15 - Integration & Tooling](#chapter-15---integration--tooling)
  - [15.1 IDE/Editor Plugins](#151-ideeditor-plugins)
    - [Visual Studio Code Extension](#visual-studio-code-extension)
    - [IntelliJ IDEA Plugin](#intellij-idea-plugin)
    - [Vim/Neovim Integration](#vimneovim-integration)
  - [15.2 CI/CD Pipelines](#152-cicd-pipelines)
    - [GitHub Actions](#github-actions)
    - [GitLab CI](#gitlab-ci)
    - [Jenkins Pipeline](#jenkins-pipeline)
  - [15.3 API & SDKs](#153-api--sdks)
    - [REST API](#rest-api)
    - [JavaScript/TypeScript SDK](#javascripttypescript-sdk)
    - [Python SDK](#python-sdk)
    - [Rust SDK](#rust-sdk)
  - [15.4 Monitoring & Analytics](#154-monitoring--analytics)
    - [Generation Metrics](#generation-metrics)
    - [Template Usage Analytics](#template-usage-analytics)
    - [Performance Monitoring](#performance-monitoring)
  - [15.5 Custom Integrations](#155-custom-integrations)
    - [Custom Processors](#custom-processors)
    - [Event Hooks](#event-hooks)
  - [15.6 Best Practices for Integration](#156-best-practices-for-integration)
    - [Version Pinning](#version-pinning)
    - [Error Handling](#error-handling)
    - [Caching Strategies](#caching-strategies)
  - [15.7 Integration Examples](#157-integration-examples)
    - [Monorepo Setup](#monorepo-setup)
    - [Microservices Architecture](#microservices-architecture)
    - [Documentation Site Integration](#documentation-site-integration)
  - [Summary](#summary)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Chapter 15 - Integration & Tooling

Integration with development tools, CI/CD pipelines, and external systems is crucial for making GGen a seamless part of your development workflow. This chapter covers the various ways to integrate GGen into your existing toolchain and extend its capabilities.

## 15.1 IDE/Editor Plugins

GGen can be integrated into popular development environments to provide code generation capabilities directly within your editor.

### Visual Studio Code Extension

A VS Code extension provides:
- Template discovery and preview
- Real-time generation with variable input
- Integration with VS Code's command palette
- Template validation and linting

```typescript
// Example VS Code extension configuration
{
  "ggen.templates": [
    {
      "name": "React Component",
      "template": "react-component",
      "variables": ["componentName", "props"]
    }
  ]
}
```

### IntelliJ IDEA Plugin

The IntelliJ plugin offers:
- Live template generation
- Project scaffolding wizards
- Integration with IntelliJ's file templates
- Custom generator configuration

### Vim/Neovim Integration

For Vim users, GGen can be integrated through:
- Custom commands and mappings
- Integration with async job control
- Template completion and validation

## 15.2 CI/CD Pipelines

GGen excels in automated environments where consistent code generation is essential.

### GitHub Actions

```yaml
name: Generate Code
on: [push, pull_request]

jobs:
  generate:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v2
      - name: Install GGen
        run: cargo install ggen
      - name: Generate API Layer
        run: ggen gen api-endpoint --vars service=user role=admin
      - name: Generate Tests
        run: ggen gen test-suite --vars module=user
```

### GitLab CI

```yaml
stages:
  - generate
  - test

generate_code:
  stage: generate
  script:
    - ggen gen schema-models --vars database=postgres
    - ggen gen api-clients --vars language=typescript
  artifacts:
    paths:
      - generated/
```

### Jenkins Pipeline

```groovy
pipeline {
    stages {
        stage('Generate') {
            steps {
                sh 'ggen gen infrastructure --vars environment=staging'
                sh 'ggen validate generated/'
            }
        }
    }
}
```

## 15.3 API & SDKs

For programmatic access, GGen provides REST APIs and SDKs in multiple languages.

### REST API

GGen provides a REST API for remote code generation:

```bash
# Generate via HTTP API
curl -X POST http://localhost:8080/generate \
  -H "Content-Type: application/json" \
  -d '{
    "template": "api-endpoint",
    "variables": {"service": "user", "method": "create"},
    "output": "src/api/user/create.rs"
  }'
```

### JavaScript/TypeScript SDK

```typescript
import { GGenClient } from '@ggen/sdk';

const client = new GGenClient('http://localhost:8080');

await client.generate({
  template: 'react-component',
  variables: {
    componentName: 'UserProfile',
    props: ['name', 'email', 'avatar']
  },
  output: 'src/components/UserProfile.tsx'
});
```

### Python SDK

```python
from ggen import GGenClient

client = GGenClient("http://localhost:8080")

result = client.generate(
    template="fastapi-endpoint",
    variables={"endpoint": "users", "method": "GET"},
    output="app/routers/users.py"
)
```

### Rust SDK

```rust
use ggen_sdk::GGenClient;

let client = GGenClient::new("http://localhost:8080");

let result = client.generate(GenerateRequest {
    template: "cli-subcommand".to_string(),
    variables: HashMap::from([
        ("cmd".to_string(), "hello".to_string()),
    ]),
    output: "src/commands/hello.rs".to_string(),
});
```

## 15.4 Monitoring & Analytics

Understanding how GGen performs and what it generates is crucial for optimization.

### Generation Metrics

GGen provides detailed metrics about generation performance:

```bash
# Generate with metrics
ggen gen api-endpoint --metrics --output metrics.json

# Metrics include:
# - Generation time
# - Template complexity
# - Variable resolution count
# - Output file size
```

### Template Usage Analytics

Track which templates are used most frequently:

```bash
# Analyze template usage
ggen analytics templates --period 30d

# Output:
# Top Templates:
# 1. api-endpoint (45 uses)
# 2. cli-subcommand (32 uses)
# 3. test-suite (28 uses)
```

### Performance Monitoring

Monitor GGen's performance in production:

```bash
# Start performance monitoring
ggen monitor start --port 9090

# Access metrics at http://localhost:9090/metrics
# - generation_time_seconds
# - templates_generated_total
# - errors_total
# - cache_hit_ratio
```

## 15.5 Custom Integrations

For specialized use cases, GGen supports custom integrations through its plugin architecture.

### Custom Processors

Extend GGen's capabilities with custom processors:

```rust
use ggen_core::processor::{Processor, ProcessorContext};

pub struct CustomProcessor;

impl Processor for CustomProcessor {
    fn process(&self, ctx: &mut ProcessorContext) -> Result<()> {
        // Custom processing logic
        Ok(())
    }
}
```

### Event Hooks

Hook into GGen's lifecycle events:

```rust
use ggen_core::hooks::{Hook, GenerationEvent};

pub struct CustomHook;

impl Hook for CustomHook {
    fn on_generation_start(&self, event: &GenerationEvent) {
        println!("Starting generation for {}", event.template_name);
    }

    fn on_generation_complete(&self, event: &GenerationEvent) {
        println!("Completed generation in {:?}", event.duration);
    }
}
```

## 15.6 Best Practices for Integration

### Version Pinning

Always pin GGen versions in your integrations:

```toml
# Cargo.toml
[dependencies]
ggen = "1.0.0"
```

### Error Handling

Implement robust error handling for generation failures:

```typescript
try {
  await client.generate(templateRequest);
} catch (error) {
  if (error.code === 'TEMPLATE_NOT_FOUND') {
    // Fallback to manual creation
  } else if (error.code === 'VALIDATION_FAILED') {
    // Prompt user for corrections
  }
}
```

### Caching Strategies

Use appropriate caching to improve performance:

```bash
# Enable generation caching
ggen gen --cache --cache-dir .ggen-cache

# Cache is automatically invalidated when:
# - Template source changes
# - Variable values change
# - GGen version updates
```

## 15.7 Integration Examples

### Monorepo Setup

For large monorepos, organize GGen integration by domain:

```
monorepo/
├── tools/
│   └── ggen-wrapper.sh
├── services/
│   ├── user-service/
│   │   └── templates/
│   └── payment-service/
│       └── templates/
└── shared/
    ├── api-specs/
    └── schemas/
```

### Microservices Architecture

Each service maintains its own GGen configuration:

```yaml
# user-service/.ggen.toml
registry:
  local: ./templates
  remote: https://registry.ggen.io

variables:
  service_name: user-service
  language: rust
  framework: axum
```

### Documentation Site Integration

Automatically generate API documentation:

```bash
# Generate OpenAPI spec from templates
ggen gen openapi-spec --vars service=user > docs/openapi.yaml

# Generate SDK documentation
ggen gen sdk-docs --vars language=typescript > docs/sdk.md
```

## Summary

Integration with existing tools and workflows is essential for GGen adoption. The combination of IDE plugins, CI/CD integration, REST APIs, and custom extensions provides multiple pathways for incorporating GGen into your development process. Start with the integration method that best fits your current toolchain and expand from there.
