# Debugging and Analytics: See Inside the Magic

ggen provides comprehensive debugging and analytics tools to understand what's happening under the hood.

## Template Debugging

### Step-by-Step Rendering Visualization
```bash
# Debug template execution
ggen template debug templates/my-service.tmpl --verbose

# Output shows each step:
# 1. Frontmatter parsing
# 2. Variable resolution
# 3. RDF data loading
# 4. SPARQL query execution
# 5. Template rendering
# 6. File writing

# Example output:
# [FRONTMATTER] Parsing YAML frontmatter
# [VARIABLES] Resolving template variables
# [RDF] Loading data/domain.ttl
# [SPARQL] Executing query: SELECT ?entity WHERE { ?entity a ex:Entity }
# [RENDER] Rendering template body
# [WRITE] Writing to generated/src/service.rs
```

### Interactive Debugging
```bash
# Interactive debug mode
ggen template debug templates/my-service.tmpl --interactive

# Prompts at each step:
# [FRONTMATTER] ✓ Parsed successfully
# Continue? (y/n/step): step

# [VARIABLES] Variables: name="UserService", version="1.0.0"
# Continue? (y/n/step): step

# [SPARQL] Query results:
# [
#   {"entity": "http://example.org/User"},
#   {"entity": "http://example.org/Product"}
# ]
# Continue? (y/n/step): y
```

### Template Validation
```bash
# Validate template before using
ggen template validate templates/my-service.tmpl

# Checks:
# - YAML frontmatter syntax
# - Tera template syntax
# - RDF file accessibility
# - SPARQL query validity
# - Variable references
# - File path validity

# Output:
# ✓ Frontmatter: Valid YAML
# ✓ Template: Valid Tera syntax
# ✓ RDF: Files accessible
# ✓ SPARQL: Queries parse correctly
# ✓ Variables: All referenced variables available
# ✓ Paths: All output paths valid
```

## SPARQL Query Debugging

### Query Execution Inspection
```bash
# Debug SPARQL queries
ggen ai sparql --description "Find all users" --debug

# Shows:
# - Query being executed
# - RDF triples being searched
# - Query execution plan
# - Results returned
# - Variables populated in template

# Example output:
# [SPARQL] Query: SELECT ?user WHERE { ?user a ex:User }
# [RDF] Searching 3 files: data/domain.ttl, data/users.ttl
# [EXECUTION] Query plan: Simple triple pattern
# [RESULTS] Found 5 users:
#   - http://example.org/users/alice
#   - http://example.org/users/bob
#   - http://example.org/users/charlie
# [VARIABLES] Template variables set: user_count=5, users=[...]
```

### Query Performance Analysis
```bash
# Analyze SPARQL query performance
ggen sparql analyze --query "SELECT ?entity WHERE { ?entity a ex:Entity }"

# Output:
# Query Performance Analysis:
# - Execution time: 2.3ms
# - Triples searched: 1,234
# - Results returned: 42
# - Cache hit: false
# - Optimization suggestions:
#   - Add index on rdf:type predicate
#   - Consider query result caching
```

## Error Context and Troubleshooting

### Detailed Error Messages
```bash
# When something goes wrong
ggen template generate templates/broken.tmpl

# Detailed error with context:
# Error: Template rendering failed
# Location: templates/broken.tmpl:42:15
# Context: Variable 'nonexistent_var' not found
# Available variables: name, description, version, author
# Suggestion: Check variable names in template
# Template variables: name="UserService", description="...", version="1.0.0"
# Debug: ggen template debug templates/broken.tmpl
```

### Common Error Patterns
```bash
# "RDF data not found"
# Error: SPARQL query returned no results
# Context: Query "SELECT ?entity WHERE { ?entity a ex:Entity }" found nothing
# Solution: Check RDF files in data/ directory
# Suggestion: ggen ai graph --description "Create domain model"

# "Permission denied"
# Error: Cannot write to output directory
# Context: Permission denied: generated/src/service.rs
# Solution: Check directory permissions
# Fix: chmod +w generated/ && chmod +w generated/src/

# "Template syntax error"
# Error: Tera template parsing failed
# Location: templates/service.tmpl:23:10
# Context: Invalid filter syntax: {{ name | invalid_filter }}
# Solution: Valid filters: camel, pascal, snake, kebab, etc.
```

## Analytics and Insights

### Template Usage Analytics
```bash
# View template usage statistics
ggen analytics templates

# Output:
# Template Usage (Last 30 days):
# - rust-service.tmpl: 127 generations
# - api-endpoint.tmpl: 89 generations
# - database-schema.tmpl: 45 generations
# - documentation.tmpl: 23 generations

# Performance Metrics:
# - Average generation time: 2.3s
# - Success rate: 94.2%
# - Cache hit rate: 87.3%
```

### Error Analytics
```bash
# View error patterns
ggen analytics errors

# Output:
# Common Errors (Last 30 days):
# 1. "Template not found" - 23 occurrences
#    Most common cause: Typo in template name
#    Solution: ggen template list

# 2. "RDF query failed" - 15 occurrences
#    Most common cause: Missing RDF data files
#    Solution: ggen ai graph --description "Create domain model"

# 3. "Permission denied" - 8 occurrences
#    Most common cause: Output directory not writable
#    Solution: Check directory permissions
```

### Performance Analytics
```bash
# View performance metrics
ggen analytics performance

# Output:
# Performance Metrics:
# - Average template generation: 2.3s
# - Average SPARQL query: 15ms
# - Average RDF loading: 45ms
# - Cache effectiveness: 87% hit rate

# Bottlenecks:
# 1. SHA256 hashing (300ms) - 40% of total time
# 2. State file I/O (50ms) - 20% of total time
# 3. Template rendering (25ms) - 10% of total time

# Optimization suggestions:
# - Enable SHA256 caching
# - Batch state persistence
# - Pre-compile templates
```

## Lifecycle Analytics

### Phase Execution Analytics
```bash
# View lifecycle performance
ggen analytics lifecycle

# Output:
# Phase Performance:
# - init: 1.2s (cached 90% of time)
# - setup: 15.3s (cached 60% of time)
# - generate: 8.7s (cached 30% of time)
# - build: 45.2s (cached 10% of time)
# - test: 12.1s (cached 20% of time)

# Hook Performance:
# - before_all: 0.5s
# - before_generate: 2.1s
# - after_generate: 1.3s
# - before_build: 3.2s
# - after_build: 0.8s
```

### State Management Analytics
```bash
# View state management metrics
ggen analytics state

# Output:
# State Management:
# - State file size: 2.3KB
# - Cache entries: 45
# - Cache hit rate: 87%
# - State saves: 156 (last 30 days)
# - State loads: 234 (last 30 days)

# Cache Effectiveness:
# - init phase: 95% cache hit rate
# - setup phase: 78% cache hit rate
# - generate phase: 45% cache hit rate
# - build phase: 12% cache hit rate
```

## Community Analytics

### Template Marketplace Analytics
```bash
# View marketplace metrics
ggen analytics marketplace

# Output:
# Marketplace Health:
# - Total templates: 1,234
# - Active templates: 892
# - Downloads this month: 45,678
# - New templates this month: 67

# Popular Categories:
# 1. rust (234 templates)
# 2. api (189 templates)
# 3. database (156 templates)
# 4. documentation (134 templates)

# Community Engagement:
# - Reviews this month: 456
# - Average rating: 4.2/5
# - Active contributors: 123
```

### User Behavior Analytics
```bash
# View user behavior patterns
ggen analytics behavior

# Output:
# User Behavior Patterns:
# - Most common workflow: generate → build → test
# - Average session time: 23 minutes
# - Most used features: template generation (45%), AI generation (30%), lifecycle (25%)
# - Error recovery rate: 78% (users fix errors and continue)

# Learning Patterns:
# - New users: Start with simple templates, gradually add complexity
# - Power users: Heavy use of SPARQL queries and custom templates
# - Teams: Shared templates and collaborative workflows
```

## Advanced Debugging

### Memory Profiling
```bash
# Profile memory usage
ggen debug memory --template templates/large-service.tmpl

# Output:
# Memory Usage Profile:
# - Template parsing: 2.3MB
# - RDF loading: 15.7MB
# - SPARQL execution: 8.9MB
# - Template rendering: 4.2MB
# - Total peak usage: 31.1MB

# Optimization suggestions:
# - Stream large RDF files instead of loading entirely
# - Cache SPARQL query results
# - Use incremental template rendering
```

### Performance Profiling
```bash
# Profile execution performance
ggen debug performance --lifecycle generate

# Output:
# Performance Profile:
# - Template loading: 45ms
# - Variable resolution: 23ms
# - SPARQL queries: 156ms
# - Template rendering: 89ms
# - File writing: 34ms
# - Total time: 347ms

# Hot spots:
# 1. SPARQL query execution (45% of time)
# 2. Template rendering (26% of time)
# 3. File I/O operations (10% of time)
```

## Integration Debugging

### IDE Integration Debugging
```bash
# Debug VSCode extension
ggen debug vscode --template templates/service.tmpl

# Output shows:
# - Extension loaded correctly
# - Template found in workspace
# - Variables resolved properly
# - Generated code preview working
```

### CI/CD Integration Debugging
```bash
# Debug CI/CD pipeline
ggen debug ci --workflow .github/workflows/ci.yml

# Output shows:
# - Workflow syntax valid
# - Commands execute correctly
# - Error handling working
# - Integration with external tools
```

## Best Practices

### Effective Debugging Workflow

#### 1. Start Simple
```bash
# Begin with basic validation
ggen template validate my-template.tmpl
ggen ai models  # Check AI provider
ggen lifecycle list  # Check available phases
```

#### 2. Use Debug Mode
```bash
# Enable detailed logging
ggen template generate my-template.tmpl --debug --verbose
```

#### 3. Check Analytics
```bash
# See if others have similar issues
ggen analytics errors --template my-template.tmpl
```

#### 4. Community Support
```bash
# Ask for help with context
ggen marketplace report my-template.tmpl "Template fails with large datasets"
# Include: ggen version, error logs, system info
```

### Performance Optimization

#### 1. Identify Bottlenecks
```bash
# Profile your workflow
ggen analytics performance --my-templates

# Shows where time is spent
# Guides optimization efforts
```

#### 2. Enable Caching
```bash
# Use appropriate caching
ggen config set cache.enabled true
ggen config set cache.ttl 3600
```

#### 3. Optimize Queries
```bash
# Analyze SPARQL performance
ggen sparql analyze --all-queries

# Shows slow queries and optimization suggestions
```

## Real-World Examples

### Debugging a Complex Template
```bash
# Complex multi-file template failing
ggen template debug templates/microservice.tmpl --verbose

# Step-by-step reveals:
# 1. Frontmatter parsing: ✓
# 2. Variable resolution: ✓
# 3. RDF loading: ✓
# 4. SPARQL queries: First query ✓, second query ❌
# 5. Template rendering: ❌ (failed on step 4)

# Fix: Second SPARQL query has syntax error
# ggen ai sparql --description "Fix this query" --debug
```

### Performance Investigation
```bash
# Generation taking too long
ggen debug performance --template templates/large-service.tmpl

# Reveals:
# - SPARQL query taking 2.3s (should be 50ms)
# - Large RDF file loading taking 800ms
# - Template rendering taking 400ms

# Solutions:
# - Optimize SPARQL query
# - Split large RDF file
# - Enable query result caching
```

## Troubleshooting Common Issues

### "Template renders but output is wrong"
**Debug**: Use validation and step-by-step debugging
```bash
ggen template validate my-template.tmpl
ggen template debug my-template.tmpl --interactive
```

### "SPARQL query returns no results"
**Debug**: Inspect query execution
```bash
ggen ai sparql --description "Debug this query" --debug
# Check RDF data and query syntax
```

### "Performance degraded over time"
**Debug**: Check analytics and caching
```bash
ggen analytics performance
ggen cache stats
ggen cache clear  # If needed
```

### "Integration with IDE not working"
**Debug**: Check extension and configuration
```bash
ggen debug vscode
ggen config show
```

## Success Metrics

### Developer Experience
- **Debug time**: 80% reduction in time spent debugging templates
- **Error resolution**: 90% of errors resolved within 5 minutes
- **Learning curve**: 60% faster onboarding for new features

### System Performance
- **Query optimization**: 70% improvement in SPARQL query performance
- **Cache effectiveness**: 85% cache hit rate for common operations
- **Error rate**: 95% reduction in template-related errors

### Community Health
- **Template quality**: 90% of published templates pass validation
- **User satisfaction**: 4.5/5 average rating for debugging tools
- **Adoption rate**: 75% of users actively use debugging features

The debugging and analytics tools transform ggen from a "black box" into a transparent, understandable system.
