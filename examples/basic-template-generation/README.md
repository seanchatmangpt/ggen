# Basic Template Generation Example

A comprehensive tutorial demonstrating ggen's core functionality for generating Rust code from templates.

## Learning Objectives

After completing this example, you will understand:
- Basic template anatomy (YAML frontmatter + Tera templating)
- Variable substitution and filters
- Template validation
- Code generation workflow
- How to verify generated output

## Prerequisites

- ggen installed and available in your PATH
- Basic understanding of YAML and template syntax
- Rust toolchain (optional, for compiling generated code)

## What This Example Demonstrates

This example shows two fundamental templates:

1. **rust-module.tmpl** - Simple module generation
   - Basic variable substitution
   - YAML frontmatter configuration
   - Single-file generation

2. **rust-struct.tmpl** - Complex struct generation
   - Multiple variables with different types
   - Tera filters (capitalize, snake_case)
   - Conditional rendering
   - Complete struct with implementation block

## Step-by-Step Instructions

### Step 1: Examine Template Structure

Look at the template files to understand ggen's anatomy:

```bash
cat templates/rust-module.tmpl
cat templates/rust-struct.tmpl
```

Each template has:
- **YAML Frontmatter** (between `---` markers) defining configuration
- **Tera Template Body** with Rust code and variable substitutions

### Step 2: Understanding the Templates

The templates demonstrate:

**rust-module.tmpl** - Simple module with:
- `to:` directive specifying output file path
- `vars:` defining required variables
- Basic Tera filters (`snake_case`, `pascal_case`)
- Documentation generation

**rust-struct.tmpl** - Complex struct with:
- Multiple variable types (strings, booleans, arrays)
- Conditional rendering (`{% if has_id %}`)
- Loops (`{% for field in fields %}`)
- Builder pattern generation

### Step 3: Manual Template Testing

You can manually render these templates using the ggen library:

```rust
use ggen_core::template::Template;
use tera::{Context, Tera};

let input = std::fs::read_to_string("templates/rust-module.tmpl")?;
let mut template = Template::parse(&input)?;

let mut tera = Tera::default();
let mut vars = Context::new();
vars.insert("name", "UserService");
vars.insert("description", "User management module");

template.render_frontmatter(&mut tera, &vars)?;
let output = template.render(&mut tera, &vars)?;
println!("{}", output);
```

### Step 4: Test Template Syntax

The `run-example.sh` script provides utilities to test templates:

```bash
./run-example.sh                # Run full example
./run-example.sh show-templates # Display template source
./run-example.sh explain        # Explain template features
```

### Step 5: Create Your Own Templates

Try modifying the templates:

1. Change variable values
2. Add new fields to the struct
3. Modify the conditional logic
4. Add new methods to generated code

## Template Anatomy

### YAML Frontmatter

Each template starts with YAML frontmatter between `---` markers:

```yaml
---
to: generated/{{ name | snake_case }}.rs
vars:
  name: string
  description: string
---
```

**Key fields:**
- `to`: Output file path (supports variable substitution)
- `vars`: Variable definitions with types
- `inject`: Optional injection points for extending generated code
- `skip_if`: Optional condition to skip generation

### Tera Template Body

After the frontmatter, use Tera template syntax:

```rust
/// {{ description }}
pub mod {{ name | snake_case }} {
    // Your code here
}
```

**Common Tera features:**
- `{{ variable }}` - Variable substitution
- `{{ variable | filter }}` - Apply filters
- `{% if condition %}...{% endif %}` - Conditional rendering
- `{% for item in items %}...{% endfor %}` - Loops

## Understanding the Templates

### rust-module.tmpl

This template demonstrates:
- Simple variable substitution (`{{ name }}`)
- Snake case filter for Rust naming conventions
- Basic function generation
- Documentation comments

**Variables:**
- `name` (string): Module name
- `description` (string): Module description

### rust-struct.tmpl

This template demonstrates:
- Multiple variable types
- Complex filters (capitalize, snake_case)
- Conditional rendering (`{% if has_id %}`)
- Struct definition with derive macros
- Implementation block with methods
- Field visibility and documentation

**Variables:**
- `name` (string): Struct name
- `description` (string): Struct purpose
- `has_id` (boolean): Whether to include an ID field
- `fields` (array): Additional struct fields

## Expected Output

### user_service.rs

A simple module with:
- Module documentation
- Basic CRUD functions
- Type annotations
- Error handling structure

### user.rs

A complete struct with:
- Derive macros for common traits
- Fields with visibility and documentation
- Constructor method
- Getter methods
- Builder pattern support

## Troubleshooting

### Template Validation Fails

**Error:** `Invalid YAML frontmatter`
**Solution:** Check that your frontmatter is valid YAML and enclosed in `---` markers

**Error:** `Unknown filter: xyz`
**Solution:** Verify you're using supported Tera filters (see ggen documentation)

### Generation Fails

**Error:** `Variable 'x' not provided`
**Solution:** Ensure all required variables are defined in the command or config

**Error:** `Failed to create output directory`
**Solution:** Check file permissions and that the path is valid

### Generated Code Doesn't Compile

**Error:** Rust compilation errors
**Solution:**
- Check that generated code follows Rust syntax rules
- Verify variable values don't contain invalid characters
- Ensure struct fields have valid types

## Advanced Usage

### Custom Variables

Override default variables when generating:

```bash
ggen generate templates/rust-module.tmpl \
  --var name=auth_service \
  --var description="Authentication service module"
```

### Multiple Templates

Generate from multiple templates at once:

```bash
ggen generate templates/*.tmpl
```

### Output to Different Directory

```bash
ggen generate templates/rust-module.tmpl --output-dir ./src
```

## Next Steps

After mastering this example, explore:

1. **Conditional Generation** - Templates with if/else logic
2. **Loop Templates** - Generating multiple items from arrays
3. **Template Composition** - Using partials and includes
4. **Intent-Driven Generation** - Natural language to code
5. **Ontology Systems** - Domain modeling with templates

See other examples in the `examples/` directory for more advanced patterns.

## Reference

### Supported Filters

Common Tera filters used in templates:
- `snake_case` - Converts to snake_case
- `camel_case` - Converts to camelCase
- `pascal_case` - Converts to PascalCase
- `upper` - Converts to UPPERCASE
- `lower` - Converts to lowercase
- `capitalize` - Capitalizes first letter
- `trim` - Removes whitespace

### Variable Types

Supported variable types in frontmatter:
- `string` - Text values
- `boolean` - true/false
- `number` - Numeric values
- `array` - Lists of items
- `object` - Nested structures

## Resources

- ggen Documentation: See main README.md
- Tera Template Documentation: https://keats.github.io/tera/
- Example Templates: See `templates/` directory
- Test Scripts: See `run-example.sh`
