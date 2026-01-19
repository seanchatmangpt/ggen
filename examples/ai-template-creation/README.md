# AI-Powered Template Creation

This example demonstrates ggen's AI capabilities for creating, validating, and optimizing templates using natural language descriptions.

## Overview

ggen includes powerful AI features that allow you to:
- Generate templates from natural language descriptions
- Validate templates for correctness and best practices
- Optimize templates for performance and maintainability
- Iterate and improve templates based on validation feedback

## AI Commands

### 1. Generate Templates (`ggen ai generate`)

Create templates from natural language descriptions:

```bash
# Basic usage (requires API key)
ggen ai generate "A User struct with id, name, and email fields"

# Mock mode (no API key needed, uses pre-trained patterns)
ggen ai generate --mock "A User struct with id, name, and email fields"

# Generate with validation
ggen ai generate --validate "REST API for blog posts with CRUD operations"

# Save to file
ggen ai generate "Database model for products" > templates/product.ggen
```

### 2. Validate Templates (`ggen ai validate`)

Check templates for correctness, best practices, and potential issues:

```bash
# Validate a template file
ggen ai validate templates/user.ggen

# Validate with detailed output
ggen ai validate --verbose templates/api.ggen

# Validate multiple templates
ggen ai validate templates/*.ggen

# Example output:
# ✓ Valid template structure
# ✓ All types are defined
# ✗ Warning: Field 'email' lacks validation pattern
# Issues: 1 warning, 0 errors
```

### 3. Optimize Templates (`ggen ai optimize`)

Improve templates for better performance and maintainability:

```bash
# Optimize a template
ggen ai optimize templates/user.ggen

# Optimize with specific goals
ggen ai optimize --focus performance templates/api.ggen
ggen ai optimize --focus readability templates/models.ggen

# Show optimization suggestions
ggen ai optimize --dry-run templates/user.ggen
```

## Mock Mode vs Real AI

### Mock Mode (Recommended for Learning)

Mock mode uses pre-trained patterns and doesn't require API keys:

```bash
# Enable mock mode globally
export GGEN_MOCK_AI=true

# Or use --mock flag
ggen ai generate --mock "User struct with validation"
```

**Benefits:**
- No API keys or costs
- Instant responses
- Predictable outputs for testing
- Great for learning and CI/CD

**Limitations:**
- Limited to common patterns
- Less flexibility than real AI
- Cannot handle highly complex or novel requests

### Real AI Mode

Real mode uses Claude AI for advanced generation:

```bash
# Set API key
export ANTHROPIC_API_KEY="your-key-here"

# Generate with real AI
ggen ai generate "Complex event-sourced system with CQRS pattern"
```

**Benefits:**
- Handles complex requirements
- Learns from your codebase
- Provides detailed explanations
- Supports iterative refinement

**When to Use:**
- Complex domain models
- Custom architectural patterns
- Integration with existing systems
- Production template generation

## Iterative Improvement Workflow

The recommended workflow for creating high-quality templates:

### Step 1: Initial Generation

Start with a clear, concise description:

```bash
ggen ai generate --mock "User authentication system with JWT tokens" \
  > templates/auth.ggen
```

### Step 2: Validate

Check for issues and warnings:

```bash
ggen ai validate templates/auth.ggen

# Output:
# ✗ Missing: Password validation rules
# ✗ Warning: Token expiration not specified
# Issues: 2 errors, 1 warning
```

### Step 3: Improve Description

Refine your prompt based on validation feedback:

```bash
ggen ai generate --mock \
  "User authentication system with JWT tokens, \
   password validation (min 8 chars, symbols required), \
   token expiration 24 hours, \
   rate limiting on login attempts" \
  > templates/auth.ggen
```

### Step 4: Re-validate

Verify improvements:

```bash
ggen ai validate templates/auth.ggen

# Output:
# ✓ Valid template structure
# ✓ All security requirements met
# ✓ Performance optimizations applied
# Issues: 0
```

### Step 5: Generate Code

Once validated, generate the final implementation:

```bash
ggen generate templates/auth.ggen --language rust --output src/
```

## Example Prompts

### Simple: Single Struct

```bash
ggen ai generate --mock \
  "A Product struct with id (UUID), name, price (decimal), and quantity (integer)"
```

### Medium: REST API

```bash
ggen ai generate --mock \
  "REST API for blog posts with:
   - CRUD operations (Create, Read, Update, Delete)
   - Posts have title, content, author, and timestamps
   - Pagination support (limit and offset)
   - Author validation required
   - Rate limiting: 100 requests per minute"
```

### Complex: Domain Model

```bash
ggen ai generate --mock \
  "E-commerce order system with:
   - Order aggregate with items and total
   - Payment processing with retry logic
   - Inventory management with stock checking
   - Event sourcing for order history
   - State machine for order status (pending, confirmed, shipped, delivered)
   - Integration with payment gateway API"
```

### Domain-Specific: Database Schema

```bash
ggen ai generate --mock \
  "PostgreSQL schema for multi-tenant SaaS:
   - Tenant table with isolation policies
   - User table with tenant foreign key
   - Role-based access control (RBAC)
   - Audit logs for all changes
   - Soft deletes with timestamps
   - Indexes for performance"
```

## Best Practices for Prompts

### 1. Be Specific

❌ Bad: "Create a user system"
✅ Good: "User authentication with email/password, JWT tokens, and password reset"

### 2. Include Constraints

❌ Bad: "API for products"
✅ Good: "REST API for products with pagination (max 100 items), rate limiting (1000 req/hour), and caching"

### 3. Specify Data Types

❌ Bad: "User with name and age"
✅ Good: "User with name (string, max 100 chars), age (integer, 0-150), email (validated format)"

### 4. Mention Validations

❌ Bad: "Order with items"
✅ Good: "Order with items (1-50 items required), total > 0, customer email validated"

### 5. Reference Patterns

❌ Bad: "Complex system"
✅ Good: "Event-sourced system using CQRS pattern, with command/query separation and event store"

### 6. Include Business Rules

❌ Bad: "Shopping cart"
✅ Good: "Shopping cart with: max 100 items, free shipping over $50, discount codes, tax calculation based on location"

## Validation Examples

See the `validation-examples/` directory for:

### valid-template.ggen
A correctly structured template with no issues:
```
✓ Valid template structure
✓ All types are defined
✓ Proper validation rules
✓ Performance optimizations applied
Issues: 0
```

### invalid-template.ggen
Common issues and how to fix them:
```
✗ Missing field validation for 'email'
✗ Type 'Address' referenced but not defined
✗ Warning: No rate limiting specified
Issues: 2 errors, 1 warning
```

### fixed-template.ggen
The corrected version showing improvements:
```
✓ All issues resolved
✓ Email validation added (regex pattern)
✓ Address type definition added
✓ Rate limiting configured (100 req/min)
Issues: 0
```

## Cost Considerations

### Mock Mode (Free)
- No API costs
- Unlimited generations
- Suitable for 80% of use cases
- Perfect for learning and testing

### Real AI Mode (Paid)
- Claude API costs apply
- ~$0.01 - $0.10 per generation (depending on complexity)
- More cost-effective than manual template writing
- Recommended for production use

**Cost Optimization Tips:**
1. Start with mock mode to refine prompts
2. Use real AI only for complex, production templates
3. Cache validated templates for reuse
4. Use `--dry-run` to preview changes before committing

## Integration with CI/CD

Validate templates in your CI pipeline:

```yaml
# .github/workflows/validate-templates.yml
name: Validate Templates
on: [push, pull_request]

jobs:
  validate:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3
      - name: Install ggen
        run: cargo install --path .
      - name: Validate all templates
        run: |
          for template in templates/*.ggen; do
            ggen ai validate "$template" || exit 1
          done
```

## Advanced Usage

### Custom Validation Rules

Create custom validation rules:

```bash
# Define custom rules in .ggen/validation-rules.yaml
ggen ai validate --rules .ggen/validation-rules.yaml templates/
```

### Template Libraries

Share validated templates:

```bash
# Export validated templates
ggen ai export-validated templates/ --output library/

# Import from library
ggen ai import library/validated-templates.tar.gz
```

### AI-Assisted Refactoring

Use AI to refactor existing templates:

```bash
# Suggest improvements
ggen ai refactor templates/old-api.ggen --suggest

# Apply refactoring
ggen ai refactor templates/old-api.ggen --apply --backup
```

## Troubleshooting

### Mock Mode Not Working

```bash
# Verify mock mode is enabled
ggen ai config --show

# Reset mock data
ggen ai mock --reset
```

### Validation Fails

```bash
# Get detailed validation report
ggen ai validate --verbose --format json templates/user.ggen

# Check for syntax errors
ggen parse templates/user.ggen
```

### Generation Quality Issues

```bash
# Use more specific prompts
# Add examples to your prompt
# Try breaking complex prompts into smaller parts
# Use real AI mode for complex requirements
```

## Next Steps

1. **Try the workflow**: Run `./run-ai-workflow.sh` to see the full process
2. **Experiment with prompts**: Modify examples in `prompts.txt`
3. **Study validation**: Review examples in `validation-examples/`
4. **Read the docs**: Check `../docs/AI_FEATURES.md` for advanced features
5. **Join community**: Share templates and get feedback

## Resources

- **Documentation**: `docs/AI_FEATURES.md`
- **API Reference**: `docs/AI_API.md`
- **Prompt Library**: `examples/prompts/`
- **Community Templates**: https://github.com/ggen-templates
- **Discord**: Join for real-time help and sharing

---

**Pro Tip**: Start with simple templates in mock mode, validate frequently, and only use real AI for complex production templates. The iterative improvement workflow ensures high-quality, production-ready templates every time.
