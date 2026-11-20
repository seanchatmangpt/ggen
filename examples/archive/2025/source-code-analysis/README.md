# Source Code Analysis Example

This example demonstrates how to use `ggen ai from-source` to analyze existing code and extract reusable patterns.

## What is Source Code Analysis?

Source code analysis uses AI to:
- Extract patterns and templates from existing code
- Generate analysis reports with insights
- Create reusable templates for similar implementations
- Enable reverse engineering and migration workflows

## Use Cases

### 1. Pattern Extraction
Extract common patterns from your codebase:
```bash
ggen ai from-source source-code/config.rs --output templates/config-pattern.yaml
```

### 2. Template Creation
Turn working code into reusable templates:
```bash
ggen ai from-source source-code/api-handler.rs --output templates/api-handler.yaml
```

### 3. Migration & Modernization
Analyze legacy code and generate modern equivalents:
```bash
ggen ai from-source legacy/old-pattern.rs --output modern/new-pattern.yaml
```

### 4. Reverse Engineering
Understand complex codebases by extracting structure:
```bash
ggen ai from-source source-code/middleware.rs --output templates/middleware-pattern.yaml
```

## Workflow Steps

### Step 1: Analyze Source Code
```bash
ggen ai from-source source-code/config.rs --output templates/config-pattern.yaml
```

This generates two outputs:
- **Template file**: `templates/config-pattern.yaml` - Reusable template
- **Analysis report**: `templates/config-pattern-analysis.md` - Insights and patterns

### Step 2: Review Generated Template
```yaml
# Example template structure
name: Configuration Pattern
description: Extracted from config.rs
variables:
  - name: config_name
    type: string
    description: Name of configuration struct
files:
  - path: "{{config_name}}.rs"
    content: |
      // Generated template with variables
```

### Step 3: Review Analysis Report
```markdown
# Analysis Report

## Patterns Identified
- Builder pattern for configuration
- Validation methods
- Default implementations

## Recommendations
- Consider adding more validation
- Document required vs optional fields
```

### Step 4: Use Template to Generate Similar Code
```bash
ggen generate templates/config-pattern.yaml \
  --set config_name=database \
  --output generated/database-config.rs
```

### Step 5: Verify Generated Code
```bash
# Check syntax
rustc --crate-type lib generated/database-config.rs

# Run tests if available
cargo test
```

## Complete Workflow Script

Run the complete workflow:
```bash
./analyze-and-generate.sh
```

This script:
1. Analyzes multiple source files
2. Generates templates and reports
3. Uses templates to regenerate similar code
4. Validates all outputs

## Example Source Files

### config.rs
Configuration struct with builder pattern, validation, and defaults.

**Patterns**:
- Builder pattern
- Validation
- Default implementations
- Type-safe configuration

### user-model.rs
Database model with serialization and validation.

**Patterns**:
- Data model design
- Serialization (serde)
- Validation rules
- Database mapping

### api-handler.rs
REST API handler with error handling and middleware.

**Patterns**:
- Request handling
- Error management
- Response formatting
- Middleware integration

### middleware.rs
Common middleware pattern for request processing.

**Patterns**:
- Middleware design
- Request/response transformation
- Error handling
- Composable architecture

## Understanding Analysis Reports

Analysis reports (`*-analysis.md`) contain:

### Patterns Identified
Common design patterns found in the code:
- Architectural patterns (MVC, builder, etc.)
- Language-specific idioms
- Best practices observed

### Code Structure
High-level organization:
- Module structure
- Dependencies
- Key components

### Recommendations
Suggestions for improvement:
- Potential refactoring opportunities
- Missing error handling
- Documentation gaps
- Performance considerations

### Template Variables
Extracted variables for template reuse:
- Configuration names
- Type parameters
- Resource identifiers

### Usage Examples
How to use the generated template:
- Required variables
- Optional parameters
- Common configurations

## Testing with Mock Mode

Test without API calls:
```bash
ggen ai from-source source-code/config.rs \
  --output templates/config.yaml \
  --mock
```

Mock mode:
- Generates realistic templates
- Creates sample analysis reports
- No API costs
- Instant results

## Advanced Usage

### Analyze Multiple Files
```bash
for file in source-code/*.rs; do
  name=$(basename "$file" .rs)
  ggen ai from-source "$file" --output "templates/${name}-pattern.yaml"
done
```

### Compare Patterns
```bash
# Analyze similar implementations
ggen ai from-source source-code/config.rs --output templates/config-v1.yaml
ggen ai from-source other-code/config.rs --output templates/config-v2.yaml

# Compare analysis reports
diff templates/config-v1-analysis.md templates/config-v2-analysis.md
```

### Extract Cross-Language Patterns
```bash
# Analyze Rust implementation
ggen ai from-source rust/handler.rs --output templates/handler-rust.yaml

# Apply pattern to TypeScript
ggen generate templates/handler-rust.yaml \
  --set language=typescript \
  --output typescript/handler.ts
```

## Best Practices

### 1. Choose Representative Examples
- Pick well-written, idiomatic code
- Use code that follows best practices
- Include complete, working examples

### 2. Review Generated Templates
- Verify extracted variables make sense
- Check that patterns are generalized correctly
- Test templates with different inputs

### 3. Iterate on Analysis
- Refine source code if analysis is unclear
- Add comments to guide pattern extraction
- Use consistent naming conventions

### 4. Document Context
- Add comments explaining design decisions
- Document why patterns were chosen
- Include usage examples

### 5. Validate Generated Code
- Always compile/run generated code
- Test with various configurations
- Compare with original source

## Troubleshooting

### Template Too Specific
**Problem**: Generated template is too tied to original code

**Solution**: Use more generic source code or edit template to add variables

### Analysis Too Generic
**Problem**: Analysis report lacks specific insights

**Solution**: Provide more focused, pattern-rich source code

### Generated Code Doesn't Compile
**Problem**: Template application produces invalid code

**Solution**: Review template syntax and variable substitution

### Missing Patterns
**Problem**: Important patterns not identified

**Solution**: Add comments in source code highlighting patterns

## Next Steps

1. **Try the examples**: Run `./analyze-and-generate.sh`
2. **Analyze your code**: Use `from-source` on your own codebase
3. **Build template library**: Create reusable patterns for your team
4. **Share templates**: Contribute patterns to template registries

## Related Examples

- **Basic Templates**: Learn template fundamentals
- **Template Composition**: Combine multiple patterns
- **CI/CD Integration**: Automate pattern extraction in pipelines

## Resources

- [ggen AI Documentation](../../docs/ai-features.md)
- [Template Reference](../../docs/template-reference.md)
- [Pattern Library](../../docs/patterns.md)
