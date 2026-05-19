# Template Marketplace: Discover, Share, Collaborate

The template marketplace is mcpp's community ecosystem for discovering, sharing, and improving templates.

## Discovery and Search

### Finding Templates
```bash
# Search by keyword
mcpp marketplace search "rust service"

# Search by category
mcpp marketplace search --category "api" --category "rust"

# Search by author
mcpp marketplace search --author "core-team"

# Advanced search with filters
mcpp marketplace search "microservice" \
    --language "rust" \
    --framework "axum" \
    --min-rating 4.0
```

### Browse Categories
```bash
# List all categories
mcpp marketplace categories

# Browse specific category
mcpp marketplace browse --category "web-frameworks"

# View trending templates
mcpp marketplace trending

# View recently updated
mcpp marketplace recent
```

### Template Information
```bash
# View template details
mcpp marketplace info rust-service-template

# Output includes:
# - Description and features
# - Usage examples
# - Dependencies
# - Rating and reviews
# - Last updated
# - Download count
```

## Installation and Usage

### Installing Templates
```bash
# Install from marketplace
mcpp marketplace install rust-service-template

# Install specific version
mcpp marketplace install rust-service-template@v1.2.0

# Install to custom location
mcpp marketplace install rust-service-template --path ./my-templates/

# Update installed template
mcpp marketplace update rust-service-template
```

### Using Installed Templates
```bash
# List installed templates
mcpp template list

# Generate from installed template
mcpp template generate rust-service-template \
    --var name="UserService" \
    --var description="User management service"
```

## Sharing and Publishing

### Publishing Templates
```bash
# Prepare template for publishing
mcpp marketplace prepare templates/my-service.tmpl

# Validates template and creates metadata
# Checks for security issues
# Generates usage examples

# Publish to marketplace
mcpp marketplace publish templates/my-service.tmpl \
    --name "rust-service-template" \
    --description "Complete Rust microservice template" \
    --category "rust" "microservice" "api" \
    --license "MIT"

# Update existing template
mcpp marketplace update templates/my-service.tmpl --version 1.1.0
```

### Template Metadata
```yaml
# .marketplace/metadata.yaml (auto-generated)
name: rust-service-template
version: 1.0.0
description: Complete Rust microservice template with API endpoints
author: your-username
license: MIT
categories:
  - rust
  - microservice
  - api
frameworks:
  - axum
  - tokio
languages:
  - rust
tags:
  - service
  - api
  - database
  - authentication
requires:
  - mcpp >= 1.0.0
  - rust >= 1.70
examples:
  - name: Basic Service
    description: Simple user service
    variables:
      name: "UserService"
      description: "User management service"
  - name: Advanced Service
    description: Service with authentication
    variables:
      name: "AuthService"
      description: "Authentication service"
      auth_provider: "jwt"
```

## Rating and Reviews

### Rating Templates
```bash
# Rate a template (1-5 stars)
mcpp marketplace rate rust-service-template 5

# Rate with comment
mcpp marketplace rate rust-service-template 4 \
    --comment "Great template but missing error handling examples"
```

### Writing Reviews
```bash
# Write detailed review
mcpp marketplace review rust-service-template \
    --title "Excellent starting point" \
    --rating 5 \
    --content "This template saved me hours of boilerplate code. The generated service is production-ready and well-structured."

# View reviews
mcpp marketplace reviews rust-service-template

# Output includes:
# - Overall rating (4.2/5)
# - Review count (23 reviews)
# - Recent reviews with details
# - Most helpful reviews
```

## Collaboration

### Contributing to Templates
```bash
# Fork template for modification
mcpp marketplace fork rust-service-template

# Create branch for changes
git checkout -b feature/add-authentication

# Make improvements
# Edit template files
# Update metadata
# Add examples

# Submit pull request
mcpp marketplace pr create \
    --title "Add JWT authentication support" \
    --description "Added JWT authentication middleware and user session management"

# Original author reviews and merges
mcpp marketplace pr merge 123
```

### Collaborative Editing
```bash
# Multiple developers working on same template
mcpp marketplace collaborate rust-service-template

# Shows active contributors
# Provides conflict resolution tools
# Tracks changes and discussions

# Lock template for major changes
mcpp marketplace lock rust-service-template \
    --reason "Major refactoring in progress"

# Unlock when ready
mcpp marketplace unlock rust-service-template
```

## Analytics and Insights

### Template Analytics
```bash
# View template usage statistics
mcpp marketplace analytics rust-service-template

# Output includes:
# - Download count (1,234 downloads)
# - Usage frequency (45 uses/day)
# - Success rate (92% success rate)
# - Common errors and solutions
# - Performance metrics
```

### Community Insights
```bash
# View community trends
mcpp marketplace insights

# Output includes:
# - Most popular categories this month
# - Rising templates (new and trending)
# - Most requested features
# - Common pain points
# - Improvement suggestions
```

## Quality Assurance

### Template Validation
```bash
# Validate before publishing
mcpp marketplace validate templates/my-template.tmpl

# Checks:
# - Template syntax validity
# - RDF data accessibility
# - SPARQL query correctness
# - Security vulnerabilities
# - Performance characteristics
```

### Automated Testing
```bash
# Run marketplace tests
mcpp marketplace test rust-service-template

# Tests include:
# - Template renders correctly
# - Generated code compiles
# - Generated code passes tests
# - Documentation is accurate
# - Examples work as advertised
```

## Best Practices

### Creating High-Quality Templates

#### 1. Comprehensive Documentation
```yaml
---
to: "README.md"
vars:
  template_name: "{{ template_name }}"
  description: "{{ description }}"
examples:
  - name: "Basic Usage"
    description: "Simple example"
    command: "mcpp template generate {{ template_name }}"
  - name: "Advanced Usage"
    description: "With all options"
    command: "mcpp template generate {{ template_name }} --var option1=value1"
---
# {{ template_name | title }}

{{ description }}

## Installation

```bash
mcpp marketplace install {{ template_name }}
```

## Usage

### Basic Example

{{ examples[0].command }}

### Advanced Example

{{ examples[1].command }}

## Generated Structure

```
generated/
├── src/
│   └── service.rs
├── tests/
│   └── service_tests.rs
└── README.md
```
```

#### 2. Multiple Examples
```bash
# Template should work in multiple scenarios
mcpp marketplace examples rust-service-template

# Shows:
# - Basic microservice
# - Service with database
# - Service with authentication
# - Service with monitoring
```

#### 3. Error Handling
```yaml
---
# Template includes error handling examples
sh_after: "echo 'Template completed successfully'"
on_error: "echo 'Template failed - check logs'"
---
```

### Publishing Workflow

#### 1. Local Testing
```bash
# Test template thoroughly before publishing
mcpp template generate templates/my-service.tmpl --dry-run
mcpp template validate templates/my-service.tmpl
mcpp template test templates/my-service.tmpl
```

#### 2. Community Preview
```bash
# Share draft with community for feedback
mcpp marketplace preview templates/my-service.tmpl \
    --description "Draft template for review"

# Community provides feedback
# Iterate based on reviews
```

#### 3. Official Release
```bash
# Publish stable version
mcpp marketplace publish templates/my-service.tmpl \
    --version 1.0.0 \
    --changelog "Initial release with basic functionality"
```

## Advanced Features

### Template Dependencies
```yaml
# .marketplace/dependencies.yaml
dependencies:
  - name: "database-template"
    version: ">=1.0.0"
    type: "template"
  - name: "auth-template"
    version: ">=2.0.0"
    type: "template"

# Auto-installs dependencies when template is installed
mcpp marketplace install my-service-template
# Also installs: database-template@v1.2.0, auth-template@v2.1.0
```

### Template Composition
```bash
# Combine multiple templates
mcpp marketplace compose \
    --base rust-service-template \
    --add database-template \
    --add auth-template \
    --output composed-service.tmpl
```

### Template Versioning
```bash
# Semantic versioning
mcpp marketplace version rust-service-template patch  # 1.0.0 → 1.0.1
mcpp marketplace version rust-service-template minor  # 1.0.1 → 1.1.0
mcpp marketplace version rust-service-template major  # 1.1.0 → 2.0.0

# Version history
mcpp marketplace history rust-service-template
```

## Community Guidelines

### Template Standards
```markdown
# Template must include:
- Comprehensive frontmatter with examples
- Proper error handling
- Security considerations
- Performance optimizations
- Clear documentation
- Test coverage

# Template should avoid:
- Hardcoded values
- Platform-specific code
- Security vulnerabilities
- Performance issues
- Poor documentation
```

### Review Process
```bash
# Community reviews new templates
mcpp marketplace review rust-service-template

# Reviews check:
# - Code quality and style
# - Documentation completeness
# - Security best practices
# - Performance considerations
# - Example accuracy
```

## Troubleshooting

### "Template not found in marketplace"
**Solution**: Check template name and try searching
```bash
mcpp marketplace search "rust service"
mcpp marketplace info exact-template-name
```

### "Installation failed"
**Solution**: Check dependencies and permissions
```bash
mcpp marketplace validate my-template.tmpl
mcpp marketplace dependencies my-template.tmpl
chmod +r templates/
```

### "Template generates errors"
**Solution**: Report issues and check for updates
```bash
mcpp marketplace report rust-service-template "Template fails on Windows"
mcpp marketplace update rust-service-template
```

## Success Stories

### Template Ecosystem Growth
- **Year 1**: 100 community templates
- **Year 2**: 1,000+ templates across all languages
- **Year 3**: Template marketplace becomes primary discovery mechanism

### Developer Productivity
- **Average time saved**: 60% reduction in boilerplate writing
- **Template reuse rate**: 85% of projects use community templates
- **Error reduction**: 70% fewer template-related bugs

### Community Impact
- **Contributors**: 500+ active template maintainers
- **Downloads**: 100,000+ template downloads per month
- **Reviews**: 10,000+ community reviews and ratings

The template marketplace transforms mcpp from a tool into a community-driven ecosystem.
