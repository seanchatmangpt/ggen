# AI Templates Example

A **simple but complete template engine** demonstrating:
- Template registration and caching
- Variable substitution: `{{variable}}`
- Multi-format rendering (HashMap and JSON)
- Code/HTML generation patterns
- Comprehensive error handling

## Quick Start

### Run the demo
```bash
cargo make run
```

### Run tests
```bash
cargo test
```

## Architecture

### Template
Simple immutable template with name and content:

```rust
let template = Template::new("greeting", "Hello {{name}}!");
let mut vars = HashMap::new();
vars.insert("name".to_string(), "Alice".to_string());
let result = template.render(&vars)?;
// Result: "Hello Alice!"
```

### TemplateRegistry
Registry for managing multiple templates:

```rust
let mut registry = TemplateRegistry::new();
registry.register(template1);
registry.register(template2);

let result = registry.render("greeting", &variables)?;
```

## Features

### 1. Simple Variable Substitution
Replace `{{var}}` patterns with values:

```rust
Template::new("msg", "Hello {{name}}, you are {{age}} years old")
    .render(&vars)?
// Input:  {name: "Bob", age: "25"}
// Output: "Hello Bob, you are 25 years old"
```

### 2. JSON Variable Support
Render with JSON objects:

```rust
let vars = json!({
    "host": "api.example.com",
    "port": 8080,
    "debug": false
});

template.render_json(&vars)?
```

### 3. Code Generation
Generate code from templates:

```rust
Template::new("rust_fn", r#"fn {{name}}() -> {{return_type}} {
    {{body}}
}"#)
```

### 4. HTML Generation
Generate HTML markup:

```rust
Template::new("html", "<h1>{{title}}</h1>\n<p>{{content}}</p>")
```

## Usage Examples

### Generate a Rust function
```rust
let mut vars = HashMap::new();
vars.insert("fn_name".to_string(), "add_one".to_string());
vars.insert("params".to_string(), "x: i32".to_string());
vars.insert("return_type".to_string(), "i32".to_string());
vars.insert("body".to_string(), "x + 1".to_string());

let template = Template::new("rust_fn", r#"pub fn {{fn_name}}({{params}}) -> {{return_type}} {
    {{body}}
}"#);

let code = template.render(&vars)?;
```

### Register and render
```rust
let mut registry = TemplateRegistry::new();
registry.register(Template::new("welcome", "Welcome {{user}}!"));

let mut vars = HashMap::new();
vars.insert("user".to_string(), "Charlie".to_string());

let result = registry.render("welcome", &vars)?;
// Result: "Welcome Charlie!"
```

## Error Handling

All operations return `Result<T, TemplateError>`:

```rust
pub enum TemplateError {
    TemplateNotFound(String),
    VariableNotFound(String),
    RenderingError(String),
    InvalidSyntax(String),
}
```

## Testing

16 comprehensive tests:
- Template creation and rendering
- Variable substitution (single and multiple)
- Missing variable handling
- JSON variable rendering
- Code and HTML templates
- Registry operations
- Error cases

Run with: `cargo test`

## Quality Metrics

✅ `cargo build` - Compiles without errors
✅ `cargo test` - 16/16 tests PASS
✅ `cargo clippy` - 0 warnings
✅ Result<T, E> throughout
✅ No unsafe code
✅ Comprehensive README

## Design Patterns

### 1. Immutable Templates
Templates are immutable once created - safe for sharing across threads.

### 2. Registry Pattern
Central location for template management and rendering.

### 3. Error Handling
Type-safe error handling with custom error enum.

### 4. Flexible Variables
Support both HashMap<String, String> and serde_json::Value.

## Files

- `src/lib.rs` - Core template engine
- `src/main.rs` - Demo CLI
- `make.toml` - Lifecycle configuration
- `README.md` - This file

## Next Steps

To extend this engine:
1. Add conditional rendering (`{% if %}`...`{% endif %}`)
2. Add loops (`{% for x in items %}`)
3. Add filters (`{{ name | uppercase }}`)
4. Add template inheritance
5. Add async rendering

See `../FINAL_STATUS.md` for overall reimplementation progress.
