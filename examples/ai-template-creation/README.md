# AI-Powered Template Creation

Learn to generate, validate, and optimize Rust code templates using ggen's AI capabilities and natural language descriptions.

**Status**: ✅ Complete and validated
**Difficulty**: ⭐⭐ Intermediate
**Time**: 30-45 minutes
**Focus**: AI-driven template generation and iterative improvement

---

## 1. Overview

This example demonstrates ggen's powerful AI integration for creating and refining code templates. Instead of writing templates manually, describe what you want in natural language, and ggen's AI generates complete templates that you can validate and iterate on.

**Key capabilities**:
- **Generate templates from natural language**: Describe code and get working templates
- **Validate generated templates**: Check quality, safety, and best practices
- **Optimize for performance/readability**: AI-powered template improvement
- **Iterate and refine**: Feedback loops with validation

**What you'll learn**:
- Natural language prompting techniques for code generation
- Template validation workflows
- Mock mode for cost-free development and testing
- Integration with real LLMs (Claude, GPT, etc.)
- Iterative template refinement patterns

**Key files**:
- `ggen.toml` - 5 generation rules for AI documentation
- `ontology/ai-workflows.ttl` - RDF specification of AI concepts
- `templates/` - 6 Tera templates for workflow guidance
- `validate.mjs` - Validation script
- `prompts.txt` - Example prompts library (existing)
- `run-ai-workflow.sh` - Demo script (existing)

---

## 2. Prerequisites

### Required Knowledge

- **ggen basics** - Understand template fundamentals (see `basic-template-generation/`)
- **Natural language** - Ability to describe code clearly
- **Rust fundamentals** - Understanding structs, functions, error handling

### Required Tools

- `ggen` (5.0.0+) with AI support
- `Node.js` 18+ (for validation script)
- Rust toolchain (optional, for compiling generated code)

### Optional: API Keys

For real AI generation (not required for learning):

```bash
# Anthropic Claude
export ANTHROPIC_API_KEY="sk-ant-..."

# OpenAI
export OPENAI_API_KEY="sk-..."

# Or use mock mode
export GGEN_MOCK_AI=true
```

### Environment Verification

```bash
# Check ggen version
ggen --version

# Verify AI support
ggen ai --help

# Check Node.js
node --version
```

---

## 3. Quick Start

### 5-Minute Demo

```bash
# Navigate to example
cd examples/ai-template-creation

# Validate environment
./validate.mjs

# Enable mock mode (no API key needed)
export GGEN_MOCK_AI=true

# Generate a template from description
ggen ai generate --mock "Create a User struct with id, name, and email"

# Validate the output
ggen ai validate generated_output.tera

# View the prompt library
cat prompts.txt

# Review the workflow guide
cat templates/ai-workflow-guide.tera
```

### Full Example Workflow

```bash
# Step 1: Generate
ggen ai generate --mock "User struct with validation methods"

# Step 2: Validate
ggen ai validate user_template.tera --verbose

# Step 3: Iterate (if needed)
ggen ai generate "Add builder pattern to User struct"

# Step 4: Validate again
ggen ai validate user_improved.tera

# Step 5: Optimize
ggen ai optimize user_improved.tera --focus readability

# Step 6: Deploy
cp user_improved.tera templates/user.tera
```

---

## 4. Architecture & Key Concepts

### The AI-Powered Generation Pipeline

```
┌──────────────────────────────────┐
│  Natural Language Description    │
│  "Create User struct with..."    │
└──────────────┬────────────────────┘
               │
               ▼
┌──────────────────────────────────┐
│  AI Code Generation              │
│  (Claude, GPT, or Mock)          │
└──────────────┬────────────────────┘
               │
               ▼
┌──────────────────────────────────┐
│  Template Output                 │
│  (Rust code or Tera template)    │
└──────────────┬────────────────────┘
               │
               ▼
┌──────────────────────────────────┐
│  Validation                      │
│  (Syntax, Safety, Coverage)      │
└──────────────┬────────────────────┘
               │
          Passed?
         /      \
        ✓        ✗
        │        │
        ▼        ▼
      Done   Iterate
```

### Three Generation Modes

| Mode | Cost | Speed | Features | Use Case |
|------|------|-------|----------|----------|
| **Mock** | Free | Instant | Basic patterns | Learning, testing, CI/CD |
| **Local (Ollama)** | Free | Slow | Good patterns | Offline, cost-free |
| **Cloud (API)** | Paid | Fast | Advanced | Production, complex requests |

### AI Commands

- `ggen ai generate`: Create code from description
- `ggen ai validate`: Check quality and safety
- `ggen ai optimize`: Improve performance/readability

### Validation Criteria

Templates are checked for:

- **Rust syntax**: Code must compile
- **Safety**: No unsafe blocks (unless documented)
- **Testing**: Tests included and meaningful
- **Documentation**: Public items have doc comments

### Mock Mode

Mock mode provides pre-trained examples for common patterns:

```bash
ggen ai generate --mock "..."
```

Benefits:
- ✅ No API keys
- ✅ Instant responses
- ✅ Reproducible for testing
- ✅ Works offline
- ✅ Zero cost

---

## 5. File Structure

```
examples/ai-template-creation/
├── ggen.toml                      # 5 generation rules
├── ontology/
│   └── ai-workflows.ttl           # RDF AI concept definitions
├── templates/                     # 6 Tera templates
│   ├── ai-workflow-guide.tera     # Workflow documentation
│   ├── prompt-library.tera        # Prompt examples
│   ├── mock-responses.tera        # Mock mode guide
│   ├── validation-rules.tera      # Validation checks
│   ├── iterative-workflow.tera    # Iteration patterns
│   └── command-reference.tera     # AI command reference
├── prompts.txt                    # Example prompts library
├── run-ai-workflow.sh             # Demo workflow script
├── validate.mjs                   # Validation script
├── validation-examples/           # Example validations
└── README.md                      # This file
```

---

## 6. Step-by-Step Tutorial

### Step 1: Understand Prompts (5 min)

Examine example prompts:

```bash
cat prompts.txt | head -20
```

Notice how prompts:
- Describe desired code clearly
- Specify data types and fields
- Mention error handling needs
- Include edge cases

### Step 2: Generate with Mock Mode (5 min)

Generate a template without API costs:

```bash
# Enable mock mode
export GGEN_MOCK_AI=true

# Generate
ggen ai generate "User struct with id, username, email, validation"

# Save output
ggen ai generate "..." > user_template.tera
```

### Step 3: Validate Output (5 min)

Check the generated template:

```bash
# Basic validation
ggen ai validate user_template.tera

# Detailed report
ggen ai validate --verbose user_template.tera
```

Validation checks:
- Valid Rust syntax
- Proper Tera formatting
- Safety constraints
- Test presence

### Step 4: Iterative Refinement (10 min)

If validation finds issues, iterate:

```bash
# Request improvements
ggen ai generate "Previous User struct, but add validation methods"

# Validate again
ggen ai validate user_v2.tera --verbose

# Continue iterating until validation passes
```

### Step 5: Optimize (5 min)

Make final improvements:

```bash
# Optimize for readability
ggen ai optimize user_v2.tera --focus readability

# Optimize for performance
ggen ai optimize user_v2.tera --focus performance

# See suggestions without modifying
ggen ai optimize --dry-run user_v2.tera
```

### Step 6: Deploy (2 min)

Save to project:

```bash
# Copy to templates directory
cp user_v2.tera templates/user.tera

# Use in generation rules
# Add to ggen.toml:
# [[generation.rules]]
# name = "generate-user"
# template = { file = "templates/user.tera" }
```

---

## 7. Configuration Reference

### ggen.toml Structure

```toml
[project]
name = "ai-template-creation"
version = "0.2.0"

[ontology]
source = "ontology/ai-workflows.ttl"

[[generation.rules]]
name = "generate-ai-workflow-guide"
template = { file = "templates/ai-workflow-guide.tera" }
output_file = "generated/AI_WORKFLOW_GUIDE.md"
```

### AI Command Options

```bash
# Generation
ggen ai generate [--mock] [--validate] [--output FILE] <PROMPT>

# Validation
ggen ai validate [--verbose] [--strict] <FILE>

# Optimization
ggen ai optimize [--focus ASPECT] [--dry-run] <FILE>
```

### Environment Variables

```bash
# Enable mock mode
export GGEN_MOCK_AI=true

# Set API provider
export GGEN_AI_PROVIDER=anthropic

# Set API key
export ANTHROPIC_API_KEY="your-key"

# Timeout settings
export GGEN_AI_TIMEOUT=30
```

---

## 8. Troubleshooting

### "ggen ai command not found"

**Cause**: ggen not installed or doesn't have AI support

**Solution**:
```bash
# Install ggen
cargo install ggen-cli

# Or build from source with features
cd /path/to/ggen
cargo install --path crates/ggen-cli --features=ai
```

### "API key not found"

**Cause**: Using real AI mode without setting credentials

**Solution**:
```bash
# Use mock mode (no key needed)
export GGEN_MOCK_AI=true
ggen ai generate --mock "..."

# Or set API key
export ANTHROPIC_API_KEY="sk-ant-..."
ggen ai generate "..."
```

### "Validation failed: unknown filter"

**Cause**: Generated template uses unsupported Tera filter

**Solution**:
1. Ask AI to use only standard filters
2. Use valid Tera filters: snake_case, pascal_case, capitalize, upper, lower, trim
3. Request optimization: `ggen ai optimize templates/file.tera`

### "Generated code doesn't compile"

**Cause**: AI generated syntactically invalid Rust

**Solution**:
```bash
# Add validation to generation
ggen ai generate --validate "Your prompt"

# Or validate output
ggen ai validate generated.tera

# Request correction
ggen ai generate "Fix the syntax errors in the previous output"
```

### "Too many tokens" error

**Cause**: Prompt or context is too large

**Solution**:
- Simplify your prompt description
- Generate smaller, focused templates
- Use mock mode for testing
- Split complex requests into multiple generations

---

## 9. Next Steps

### Progression Path

**Level 1 - Basic Generation** (you are here)
- ✅ Generate from natural language
- ✅ Use mock mode
- ✅ Validate templates

**Level 2 - Iterative Refinement** (next)
- Feedback loops with validation
- Optimization passes
- Building a template library

**Level 3 - Advanced Patterns** (advanced)
- Complex multi-file generation
- Cross-file references
- Domain-specific templates

**Level 4 - Integration** (expert)
- CI/CD pipelines with AI generation
- Automated template libraries
- Custom validation rules

### Practice Exercises

1. **Generate variations**
   - Create 3 different User struct implementations
   - Compare generated code
   - Vote on best version

2. **Build a template library**
   - Generate 5 common patterns (struct, endpoint, validator, etc.)
   - Validate all templates
   - Organize in templates/ directory

3. **Create custom validation rules**
   - Define project-specific requirements
   - Validate against those rules
   - Request fixes until validation passes

### Related Examples

- **`basic-template-generation/`** - Foundation for template concepts
- **`source-code-analysis/`** - Extract patterns from existing code
- **`openapi/`** - Generate API specifications
- **`complete-project-generation/`** - Full project scaffolding

### Resources

- `prompts.txt` - Curated prompt examples
- `run-ai-workflow.sh` - Demonstration script
- `templates/ai-workflow-guide.tera` - Detailed workflow guide
- `templates/command-reference.tera` - All AI commands
- Generated `PROMPT_LIBRARY.md` - Reusable prompts
- Generated `ITERATIVE_WORKFLOW.md` - Step-by-step patterns

### Success Criteria

Mastery achieved when you can:

- [ ] Explain mock mode and when to use it
- [ ] Write effective natural language prompts
- [ ] Validate generated templates
- [ ] Iterate based on validation feedback
- [ ] Optimize templates for specific goals
- [ ] Run full generation→validation→optimization→deployment cycle
- [ ] Troubleshoot common AI generation issues
- [ ] Create a reusable prompt library
- [ ] Build custom validation rules
- [ ] Integrate AI generation into ggen workflows

---

## Summary

This example demonstrates **AI-assisted template generation**: using natural language descriptions and LLMs to create production-quality code templates, then validating and iterating until they're perfect.

**Key learnings**:
- AI can assist with template creation
- Mock mode enables cost-free development
- Validation ensures quality
- Iteration improves templates
- Integration with ggen is seamless

**Next**: Explore iterative workflows in `templates/iterative-workflow.tera`

Happy generating! 🚀
