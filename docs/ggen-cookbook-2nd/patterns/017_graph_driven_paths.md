# Pattern 017: Graph-Driven Paths

## Intent
Dynamically compute file output paths from RDF graph data, enabling intelligent file organization based on semantic relationships and metadata.

## Motivation
Hardcoded file paths create inflexible templates. By deriving paths from graph data, templates can automatically organize outputs by module, domain, architecture layer, or any graph-defined structure.

## Applicability
- Organizing microservices by bounded context
- Creating layered architecture (controllers/services/repositories)
- Grouping files by feature or module
- Dynamic documentation structure from ontology
- Multi-language code generation with language-specific paths

## Structure

```yaml
---
ggen:projection:
  query: "SELECT ?name ?layer ?module WHERE { ... }"
  template: "{{module}}/{{layer}}/{{name}}.rs"
---
```

## Implementation

### Basic Graph-Driven Paths

**File: `architecture.ttl`**
```turtle
@prefix arch: <http://architecture.example.org/> .
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .

# Define architectural layers
arch:PresentationLayer a arch:Layer ;
    arch:name "presentation" ;
    arch:path "src/presentation" .

arch:ApplicationLayer a arch:Layer ;
    arch:name "application" ;
    arch:path "src/application" .

arch:DomainLayer a arch:Layer ;
    arch:name "domain" ;
    arch:path "src/domain" .

arch:InfrastructureLayer a arch:Layer ;
    arch:name "infrastructure" ;
    arch:path "src/infrastructure" .

# Define components with layer assignments
arch:UserController a arch:Component ;
    arch:name "UserController" ;
    arch:belongsToLayer arch:PresentationLayer ;
    arch:module "users" ;
    arch:componentType "controller" .

arch:UserService a arch:Component ;
    arch:name "UserService" ;
    arch:belongsToLayer arch:ApplicationLayer ;
    arch:module "users" ;
    arch:componentType "service" .

arch:User a arch:Component ;
    arch:name "User" ;
    arch:belongsToLayer arch:DomainLayer ;
    arch:module "users" ;
    arch:componentType "entity" .

arch:UserRepository a arch:Component ;
    arch:name "UserRepository" ;
    arch:belongsToLayer arch:InfrastructureLayer ;
    arch:module "users" ;
    arch:componentType "repository" .

arch:ProductController a arch:Component ;
    arch:name "ProductController" ;
    arch:belongsToLayer arch:PresentationLayer ;
    arch:module "products" ;
    arch:componentType "controller" .

arch:ProductService a arch:Component ;
    arch:name "ProductService" ;
    arch:belongsToLayer arch:ApplicationLayer ;
    arch:module "products" ;
    arch:componentType "service" .
```

**File: `layered_template.tmpl`**
```handlebars
---
ggen:projection:
  query: |
    PREFIX arch: <http://architecture.example.org/>
    SELECT ?component ?name ?layerPath ?module ?componentType
    WHERE {
      ?component a arch:Component ;
                 arch:name ?name ;
                 arch:belongsToLayer ?layer ;
                 arch:module ?module ;
                 arch:componentType ?componentType .
      ?layer arch:path ?layerPath .
    }
  template: "{{layerPath}}/{{module}}/{{name | snake_case}}.rs"
---
// {{componentType | title}}: {{name}}
// Module: {{module}}
// Layer: {{layerPath}}

{{#if (eq componentType "controller")}}
use crate::application::{{module}}::{{name | replace: "Controller" with: "Service"}};

pub struct {{name}} {
    service: {{name | replace: "Controller" with: "Service"}},
}

impl {{name}} {
    pub fn new(service: {{name | replace: "Controller" with: "Service"}}) -> Self {
        Self { service }
    }

    pub async fn handle_request(&self) -> Response {
        // Controller logic
        self.service.execute().await
    }
}
{{/if}}

{{#if (eq componentType "service")}}
use crate::domain::{{module}}::{{name | replace: "Service" with: ""}};
use crate::infrastructure::{{module}}::{{name | replace: "Service" with: "Repository"}};

pub struct {{name}} {
    repository: {{name | replace: "Service" with: "Repository"}},
}

impl {{name}} {
    pub fn new(repository: {{name | replace: "Service" with: "Repository"}}) -> Self {
        Self { repository }
    }

    pub async fn execute(&self) -> Result<{{name | replace: "Service" with: ""}}, Error> {
        // Service logic
        self.repository.find().await
    }
}
{{/if}}

{{#if (eq componentType "entity")}}
use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct {{name}} {
    pub id: u64,
    // Domain entity fields
}

impl {{name}} {
    pub fn new(id: u64) -> Self {
        Self { id }
    }
}
{{/if}}

{{#if (eq componentType "repository")}}
use crate::domain::{{module}}::{{name | replace: "Repository" with: ""}};
use async_trait::async_trait;

#[async_trait]
pub trait {{name}} {
    async fn find(&self, id: u64) -> Result<{{name | replace: "Repository" with: ""}}, Error>;
    async fn save(&self, entity: {{name | replace: "Repository" with: ""}}) -> Result<(), Error>;
}

pub struct Postgres{{name}} {
    pool: PgPool,
}

#[async_trait]
impl {{name}} for Postgres{{name}} {
    async fn find(&self, id: u64) -> Result<{{name | replace: "Repository" with: ""}}, Error> {
        // Repository implementation
        todo!()
    }

    async fn save(&self, entity: {{name | replace: "Repository" with: ""}}) -> Result<(), Error> {
        // Repository implementation
        todo!()
    }
}
{{/if}}
```

**Generated file structure:**
```
src/
├── presentation/
│   ├── users/
│   │   └── user_controller.rs
│   └── products/
│       └── product_controller.rs
├── application/
│   ├── users/
│   │   └── user_service.rs
│   └── products/
│       └── product_service.rs
├── domain/
│   └── users/
│       └── user.rs
└── infrastructure/
    └── users/
        └── user_repository.rs
```

## Advanced Example: Microservices with Bounded Contexts

**File: `microservices.ttl`**
```turtle
@prefix ms: <http://microservices.example.org/> .

# Define bounded contexts
ms:UserManagement a ms:BoundedContext ;
    ms:name "user-management" ;
    ms:path "services/user-management/src" ;
    ms:port 8001 .

ms:ProductCatalog a ms:BoundedContext ;
    ms:name "product-catalog" ;
    ms:path "services/product-catalog/src" ;
    ms:port 8002 .

ms:OrderProcessing a ms:BoundedContext ;
    ms:name "order-processing" ;
    ms:path "services/order-processing/src" ;
    ms:port 8003 .

# Define service components
ms:UserAuthService a ms:Service ;
    ms:name "AuthService" ;
    ms:context ms:UserManagement ;
    ms:serviceType "auth" ;
    ms:hasEndpoint ms:LoginEndpoint, ms:RegisterEndpoint .

ms:LoginEndpoint a ms:Endpoint ;
    ms:path "/auth/login" ;
    ms:method "POST" .

ms:RegisterEndpoint a ms:Endpoint ;
    ms:path "/auth/register" ;
    ms:method "POST" .

ms:ProductSearchService a ms:Service ;
    ms:name "SearchService" ;
    ms:context ms:ProductCatalog ;
    ms:serviceType "search" ;
    ms:hasEndpoint ms:SearchEndpoint .

ms:SearchEndpoint a ms:Endpoint ;
    ms:path "/products/search" ;
    ms:method "GET" .

ms:OrderCreationService a ms:Service ;
    ms:name "OrderCreationService" ;
    ms:context ms:OrderProcessing ;
    ms:serviceType "creation" ;
    ms:hasEndpoint ms:CreateOrderEndpoint .

ms:CreateOrderEndpoint a ms:Endpoint ;
    ms:path "/orders" ;
    ms:method "POST" .
```

**File: `microservice_template.tmpl`**
```handlebars
---
ggen:projection:
  query: |
    PREFIX ms: <http://microservices.example.org/>
    SELECT ?service ?name ?contextPath ?serviceType ?contextName ?port
    WHERE {
      ?service a ms:Service ;
               ms:name ?name ;
               ms:context ?context ;
               ms:serviceType ?serviceType .
      ?context ms:path ?contextPath ;
               ms:name ?contextName ;
               ms:port ?port .
    }
  template: "{{contextPath}}/services/{{name | snake_case}}.rs"
---
// Service: {{name}}
// Bounded Context: {{contextName}}
// Type: {{serviceType}}

use axum::{Router, routing::{get, post}};
use std::net::SocketAddr;

pub struct {{name}} {
    config: ServiceConfig,
}

impl {{name}} {
    pub fn new(config: ServiceConfig) -> Self {
        Self { config }
    }

    pub fn router(&self) -> Router {
        Router::new()
{{#sparql}}
    PREFIX ms: <http://microservices.example.org/>
    SELECT ?endpointPath ?method
    WHERE {
      <{{service}}> ms:hasEndpoint ?endpoint .
      ?endpoint ms:path ?endpointPath ;
                ms:method ?method .
    }
{{/sparql}}
            .route("{{endpointPath}}", {{method | lowercase}}(handle_{{endpointPath | replace: "/" with: "_"}}))
{{/sparql}}
    }

    pub async fn serve(&self) -> Result<(), Error> {
        let addr = SocketAddr::from(([127, 0, 0, 1], {{port}}));
        let app = self.router();

        println!("{{contextName}} - {{name}} listening on {}", addr);
        axum::Server::bind(&addr)
            .serve(app.into_make_service())
            .await
            .map_err(Into::into)
    }
}

{{#sparql}}
    PREFIX ms: <http://microservices.example.org/>
    SELECT ?endpointPath ?method
    WHERE {
      <{{service}}> ms:hasEndpoint ?endpoint .
      ?endpoint ms:path ?endpointPath ;
                ms:method ?method .
    }
{{/sparql}}
async fn handle_{{endpointPath | replace: "/" with: "_"}}() -> Response {
    // TODO: Implement {{method}} {{endpointPath}}
    Response::new()
}

{{/sparql}}

#[derive(Debug, Clone)]
pub struct ServiceConfig {
    pub port: u16,
    pub context: String,
}
```

**Generated structure:**
```
services/
├── user-management/
│   └── src/
│       └── services/
│           └── auth_service.rs
├── product-catalog/
│   └── src/
│       └── services/
│           └── search_service.rs
└── order-processing/
    └── src/
        └── services/
            └── order_creation_service.rs
```

## Multi-Language Code Generation

**File: `languages.ttl`**
```turtle
@prefix lang: <http://languages.example.org/> .

lang:Rust a lang:Language ;
    lang:name "rust" ;
    lang:extension ".rs" ;
    lang:path "src-rust" ;
    lang:testPath "tests" .

lang:TypeScript a lang:Language ;
    lang:name "typescript" ;
    lang:extension ".ts" ;
    lang:path "src-ts" ;
    lang:testPath "__tests__" .

lang:Python a lang:Language ;
    lang:name "python" ;
    lang:extension ".py" ;
    lang:path "src-py" ;
    lang:testPath "tests" .

# Define models with language targets
lang:UserModel a lang:Model ;
    lang:name "User" ;
    lang:targetLanguage lang:Rust, lang:TypeScript, lang:Python ;
    lang:hasField lang:UserIdField, lang:UserNameField .

lang:UserIdField a lang:Field ;
    lang:name "id" ;
    lang:rustType "u64" ;
    lang:tsType "number" ;
    lang:pythonType "int" .

lang:UserNameField a lang:Field ;
    lang:name "name" ;
    lang:rustType "String" ;
    lang:tsType "string" ;
    lang:pythonType "str" .
```

**File: `multilang_template.tmpl`**
```handlebars
---
ggen:projection:
  query: |
    PREFIX lang: <http://languages.example.org/>
    SELECT ?model ?modelName ?language ?langName ?langPath ?extension
    WHERE {
      ?model a lang:Model ;
             lang:name ?modelName ;
             lang:targetLanguage ?language .
      ?language lang:name ?langName ;
                lang:path ?langPath ;
                lang:extension ?extension .
    }
  template: "{{langPath}}/models/{{modelName | snake_case}}{{extension}}"
---
{{#if (eq langName "rust")}}
use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct {{modelName}} {
{{#sparql}}
    PREFIX lang: <http://languages.example.org/>
    SELECT ?fieldName ?rustType
    WHERE {
      <{{model}}> lang:hasField ?field .
      ?field lang:name ?fieldName ;
             lang:rustType ?rustType .
    }
{{/sparql}}
    pub {{fieldName}}: {{rustType}},
{{/sparql}}
}
{{/if}}

{{#if (eq langName "typescript")}}
export interface {{modelName}} {
{{#sparql}}
    PREFIX lang: <http://languages.example.org/>
    SELECT ?fieldName ?tsType
    WHERE {
      <{{model}}> lang:hasField ?field .
      ?field lang:name ?fieldName ;
             lang:tsType ?tsType .
    }
{{/sparql}}
  {{fieldName}}: {{tsType}};
{{/sparql}}
}

export class {{modelName}}Impl implements {{modelName}} {
{{#sparql}}
    PREFIX lang: <http://languages.example.org/>
    SELECT ?fieldName ?tsType
    WHERE {
      <{{model}}> lang:hasField ?field .
      ?field lang:name ?fieldName ;
             lang:tsType ?tsType .
    }
{{/sparql}}
  {{fieldName}}: {{tsType}};
{{/sparql}}

  constructor(data: Partial<{{modelName}}>) {
    Object.assign(this, data);
  }
}
{{/if}}

{{#if (eq langName "python")}}
from dataclasses import dataclass
from typing import Optional

@dataclass
class {{modelName}}:
{{#sparql}}
    PREFIX lang: <http://languages.example.org/>
    SELECT ?fieldName ?pythonType
    WHERE {
      <{{model}}> lang:hasField ?field .
      ?field lang:name ?fieldName ;
             lang:pythonType ?pythonType .
    }
{{/sparql}}
    {{fieldName}}: {{pythonType}}
{{/sparql}}

    def to_dict(self) -> dict:
        return {
{{#sparql}}
    PREFIX lang: <http://languages.example.org/>
    SELECT ?fieldName
    WHERE {
      <{{model}}> lang:hasField ?field .
      ?field lang:name ?fieldName .
    }
{{/sparql}}
            '{{fieldName}}': self.{{fieldName}},
{{/sparql}}
        }
{{/if}}
```

**Generated structure:**
```
project/
├── src-rust/
│   └── models/
│       └── user.rs
├── src-ts/
│   └── models/
│       └── user.ts
└── src-py/
    └── models/
        └── user.py
```

## Dynamic Path Computation

**File: `computed_paths.tmpl`**
```handlebars
---
ggen:projection:
  query: |
    PREFIX app: <http://app.example.org/>
    SELECT ?component ?name ?version ?environment
    WHERE {
      ?component a app:Component ;
                 app:name ?name ;
                 app:version ?version ;
                 app:environment ?environment .
    }
  template: "deployments/{{environment}}/v{{version}}/{{name | kebab_case}}/config.yaml"
---
apiVersion: v1
kind: ConfigMap
metadata:
  name: {{name | kebab_case}}-config
  namespace: {{environment}}
  labels:
    app: {{name | kebab_case}}
    version: v{{version}}
    environment: {{environment}}
data:
  CONFIG_ENV: "{{environment}}"
  CONFIG_VERSION: "{{version}}"
  CONFIG_NAME: "{{name}}"
```

## Benefits

1. **Semantic Organization**: File structure mirrors domain model
2. **Automatic Refactoring**: Moving components in graph moves files
3. **Consistent Structure**: All outputs follow graph-defined patterns
4. **Multi-Target Generation**: Same template, different paths per language/environment
5. **Documentation Alignment**: Code structure matches architecture diagrams

## Related Patterns

- **014: Fan-Out Projection** - Generate many files with dynamic paths
- **015: Immutability First** - Protect files regardless of path
- **016: Hybrid Files** - Once-only generation with dynamic paths

## Best Practices

1. **Use semantic prefixes**: `src/{{layer}}/{{module}}/{{name}}`
2. **Validate paths**: Ensure graph data produces valid filesystem paths
3. **Normalize names**: Use filters like `snake_case`, `kebab_case`
4. **Test path generation**: Verify paths before bulk generation
5. **Document path schema**: Explain graph-to-path mapping

## Anti-Patterns

❌ **Hardcoded paths in projection templates:**
```yaml
template: "src/models/{{name}}.rs"  # Inflexible
```

✅ **Graph-driven paths:**
```yaml
template: "{{basePath}}/{{layer}}/{{name}}.rs"
```

❌ **Complex path logic in templates:**
```handlebars
{{#if (eq type "controller")}}src/presentation{{else}}src/domain{{/if}}
```

✅ **Path logic in graph:**
```turtle
?component arch:outputPath ?computedPath .
```

## See Also

- GGen Projection Documentation
- SPARQL Path Construction
- File Organization Patterns
- Multi-Language Code Generation
