use anyhow::Result;
use ggen_ai::client::GenAIClient;
use ggen_core::graph::Graph;
use serde::{Deserialize, Serialize};
use tracing::{info, debug};

#[derive(Debug, Serialize, Deserialize)]
pub struct SystemDesign {
    pub name: String,
    pub platform: String,
    pub components: Vec<Component>,
    pub api_endpoints: Vec<ApiEndpoint>,
    pub data_models: Vec<DataModel>,
    pub deployment: DeploymentConfig,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct Component {
    pub name: String,
    pub component_type: String,
    pub description: String,
    pub dependencies: Vec<String>,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct ApiEndpoint {
    pub path: String,
    pub method: String,
    pub description: String,
    pub request_schema: String,
    pub response_schema: String,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct DataModel {
    pub name: String,
    pub fields: Vec<Field>,
    pub relations: Vec<Relation>,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct Field {
    pub name: String,
    pub field_type: String,
    pub required: bool,
    pub description: String,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct Relation {
    pub target: String,
    pub relation_type: String,
    pub cardinality: String,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct DeploymentConfig {
    pub environment: String,
    pub database: String,
    pub cache: String,
    pub cdn: Option<String>,
}

pub struct Architect {
    ai_client: GenAIClient,
    graph: Graph,
}

impl Architect {
    pub fn new() -> Result<Self> {
        info!("Initializing Architect");

        let ai_client = GenAIClient::from_env()?;
        let graph = Graph::new();

        Ok(Self { ai_client, graph })
    }

    pub async fn design_system(&self, name: &str, platform: &str) -> Result<SystemDesign> {
        info!("Designing system: {} for platform: {}", name, platform);

        // Build design prompt
        let prompt = self.build_design_prompt(name, platform);

        // Query AI for architecture design
        debug!("Querying AI for architecture design");
        let response = self.ai_client
            .generate_with_options(&prompt, None)
            .await?;

        // Parse AI response into structured design
        let design = self.parse_ai_response(&response, name, platform)?;

        // Validate design coherence
        self.validate_design(&design)?;

        info!("System design complete with {} components", design.components.len());
        Ok(design)
    }

    fn build_design_prompt(&self, name: &str, platform: &str) -> String {
        format!(
            r#"Design a complete blog platform architecture with the following requirements:

Project Name: {}
Target Platform: {}

Please provide a comprehensive design including:

1. **Components**: All necessary frontend, backend, and infrastructure components
   - Authentication service
   - Blog post management service
   - Comment system
   - User management
   - Search functionality
   - Frontend application

2. **API Endpoints**: RESTful API design for all operations
   - CRUD operations for posts
   - User authentication/authorization
   - Comment management
   - Search and filtering

3. **Data Models**: Database schema for all entities
   - User model (id, username, email, password_hash, created_at, updated_at)
   - Post model (id, title, content, author_id, status, published_at, created_at, updated_at)
   - Comment model (id, post_id, author_id, content, created_at, updated_at)
   - Tag model and post-tag relationships

4. **Deployment Configuration**: Production-ready deployment setup
   - Database: PostgreSQL
   - Cache: Redis
   - Web server: nginx
   - Application runtime
   - CDN for static assets

Return a JSON structure with this exact format:
{{
  "components": [
    {{
      "name": "component_name",
      "component_type": "frontend|backend|database|cache",
      "description": "what it does",
      "dependencies": ["other_component_names"]
    }}
  ],
  "api_endpoints": [
    {{
      "path": "/api/posts",
      "method": "GET",
      "description": "List all posts",
      "request_schema": "{{}}",
      "response_schema": "{{\"posts\": [Post]}}"
    }}
  ],
  "data_models": [
    {{
      "name": "Post",
      "fields": [
        {{
          "name": "id",
          "field_type": "UUID",
          "required": true,
          "description": "Unique identifier"
        }}
      ],
      "relations": [
        {{
          "target": "User",
          "relation_type": "belongs_to",
          "cardinality": "many_to_one"
        }}
      ]
    }}
  ]
}}

Design a modern, scalable architecture following best practices."#,
            name, platform
        )
    }

    fn parse_ai_response(&self, response: &str, name: &str, platform: &str) -> Result<SystemDesign> {
        debug!("Parsing AI response into structured design");

        // Extract JSON from response (AI might include markdown formatting)
        let json_str = if let Some(start) = response.find('{') {
            if let Some(end) = response.rfind('}') {
                &response[start..=end]
            } else {
                response
            }
        } else {
            response
        };

        // Parse partial structure
        let partial: serde_json::Value = serde_json::from_str(json_str)
            .map_err(|e| anyhow::anyhow!("Failed to parse AI response as JSON: {}", e))?;

        // Extract components
        let components = partial["components"]
            .as_array()
            .map(|arr| {
                arr.iter()
                    .filter_map(|v| serde_json::from_value(v.clone()).ok())
                    .collect()
            })
            .unwrap_or_default();

        // Extract API endpoints
        let api_endpoints = partial["api_endpoints"]
            .as_array()
            .map(|arr| {
                arr.iter()
                    .filter_map(|v| serde_json::from_value(v.clone()).ok())
                    .collect()
            })
            .unwrap_or_default();

        // Extract data models
        let data_models = partial["data_models"]
            .as_array()
            .map(|arr| {
                arr.iter()
                    .filter_map(|v| serde_json::from_value(v.clone()).ok())
                    .collect()
            })
            .unwrap_or_default();

        // Create default deployment config
        let deployment = DeploymentConfig {
            environment: "production".to_string(),
            database: "postgresql".to_string(),
            cache: "redis".to_string(),
            cdn: Some("cloudflare".to_string()),
        };

        Ok(SystemDesign {
            name: name.to_string(),
            platform: platform.to_string(),
            components,
            api_endpoints,
            data_models,
            deployment,
        })
    }

    fn validate_design(&self, design: &SystemDesign) -> Result<()> {
        debug!("Validating design coherence");

        // Ensure we have essential components
        if design.components.is_empty() {
            return Err(anyhow::anyhow!("Design must include at least one component"));
        }

        if design.data_models.is_empty() {
            return Err(anyhow::anyhow!("Design must include at least one data model"));
        }

        // Validate component dependencies
        let component_names: Vec<_> = design.components.iter().map(|c| c.name.as_str()).collect();
        for component in &design.components {
            for dep in &component.dependencies {
                if !component_names.contains(&dep.as_str()) {
                    return Err(anyhow::anyhow!(
                        "Component '{}' has undefined dependency: '{}'",
                        component.name,
                        dep
                    ));
                }
            }
        }

        // Validate data model relations
        let model_names: Vec<_> = design.data_models.iter().map(|m| m.name.as_str()).collect();
        for model in &design.data_models {
            for relation in &model.relations {
                if !model_names.contains(&relation.target.as_str()) {
                    return Err(anyhow::anyhow!(
                        "Model '{}' has undefined relation target: '{}'",
                        model.name,
                        relation.target
                    ));
                }
            }
        }

        debug!("Design validation successful");
        Ok(())
    }
}
