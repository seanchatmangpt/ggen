Frontmatter { to: Some("README.md"), from: None, force: false, unless_exists: false, inject: false, before: None, after: None, prepend: false, append: false, at_line: None, eof_last: false, skip_if: None, sh_before: None, sh_after: None, base: None, prefixes: {}, rdf_inline: [], rdf: ["src/rdf/project_structure.ttl", "src/rdf/test_framework.ttl"], sparql: {"container_services": "SELECT ?service ?port WHERE { ?service <http://example.org/hasPort> ?port . }", "project_structure": "SELECT ?component ?description WHERE { ?component <http://example.org/hasDescription> ?description . }", "test_framework_features": "SELECT ?feature ?category WHERE { ?feature <http://example.org/hasCategory> ?category . }", "test_lifecycle": "SELECT ?hook ?description WHERE { ?hook <http://example.org/hasDescription> ?description . }"}, vars: {"var0": Mapping {"name": String("project_name"), "description": String("Name of the project"), "default": String("ggen-cleanroom"), "type": String("string")}, "var1": Mapping {"type": String("string"), "name": String("project_description"), "description": String("Description of the project"), "default": String("Advanced Rust test harness crate providing isolated container-based testing with testcontainers, SPARQL-based test specifications, BDD framework integration, hyper-based HTTP testing, property-based testing with proptest, comprehensive assertion library, test orchestration and parallelism, test data management with fixtures, and production-grade test reporting.")}}, backup: None, idempotent: false, shape: [], determinism: None, freeze_policy: None, freeze_slots_dir: None, sparql_results: {} }
---
# {{ vars.project_name }}

{{ vars.project_description }}

## Overview

{{ vars.project_name }} is a comprehensive Rust testing framework designed for creating isolated, container-based tests with advanced features for modern software development. Built with production-grade reliability and developer experience in mind, it combines containerization, semantic web technologies, and testing best practices.

## Key Features

{{% for item in sparql.test_framework_features %}}
- **{{ item.feature }}**: {{ item.category }}
{{% endfor %}}

## Core Components

{{% for item in sparql.project_structure %}}
- `{{ item.component }}`: {{ item.description }}
{{% endfor %}}

## Container Services

{{% for item in sparql.container_services %}}
- **{{ item.service }}**: Port {{ item.port }}
{{% endfor %}}

## Test Lifecycle Hooks

{{% for item in sparql.test_lifecycle %}}
- **{{ item.hook }}**: {{ item.description }}
{{% endfor %}}

## Getting Started

### Installation

Add to your `Cargo.toml`: