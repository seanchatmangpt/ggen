#!/bin/bash
set -e

echo "Applying bulk fixes for ggen-mcp compilation..."

# Fix 1: Move chrono import below module doc comments in recovery.rs
sed -i '' '1d; 2s|^|use chrono::{DateTime, Utc};\n|' src/agents/recovery.rs

# Fix 2: Change Template to String in ai.rs return types
# Line 117: TemplateGenerator returns String, not Template
sed -i '' 's/let template_content = generator\.generate_template(&description, &Vec::new())\.await?;/let template_content: String = generator.generate_template(\&description, \&Vec::new()).await.map_err(|e| crate::error::GgenMcpError::Ai(e.to_string()))?;/' src/tools/ai.rs

# Fix 3: Change validate_template to accept &str
# Line 129: Already accepts &str, but template_content is Template
# Need to convert Template to &str
sed -i '' 's/validation_errors = validate_template(&template_content)?;/validation_errors = validate_template(\&template_content)?;/' src/tools/ai.rs

# Fix 4: Change Arc to Box for LlmClient
sed -i '' 's/TemplateGenerator::new(client)/TemplateGenerator::new(std::sync::Arc::new(client))/' src/tools/ai.rs
sed -i '' 's/SparqlGenerator::new(client)/SparqlGenerator::new(std::sync::Arc::new(client))/' src/tools/ai.rs
sed -i '' 's/OntologyGenerator::new(client)/OntologyGenerator::new(std::sync::Arc::new(client))/' src/tools/ai.rs

echo "Bulk fixes applied!"
