# Simple Project Example

This example demonstrates a minimal `ggen.toml` configuration for basic code generation.

## Project Structure

```
simple-project/
├── ggen.toml           # Project configuration
├── templates/          # Template files
│   └── hello.tmpl      # Example template
├── generated/          # Generated output
└── README.md           # This file
```

## Configuration

The `ggen.toml` file contains minimal required configuration:

```toml
[project]
name = "simple-project"
version = "0.1.0"
description = "A simple project demonstrating ggen.toml basics"

[templates]
source_dir = "templates"
output_dir = "generated"
```

## Usage

### 1. Initialize the Project

```bash
cd examples/simple-project
ggen project init
```

### 2. Generate Code

```bash
ggen project generate
```

This will process all `.tmpl` files in the `templates/` directory and write output to `generated/`.

### 3. View Generated Files

```bash
ls -R generated/
cat generated/hello.txt
```

## Template Example

**templates/hello.tmpl:**
```
Hello from {{ project.name }}!
Version: {{ project.version }}
```

**Generated output (generated/hello.txt):**
```
Hello from simple-project!
Version: 0.1.0
```

## Next Steps

- Add more templates to `templates/`
- Customize `ggen.toml` with additional sections
- Try the [Workspace Example](../workspace-project/)
- Read the [User Guide](../../docs/ggen-toml-guide.md)

## Learn More

- [ggen.toml User Guide](../../docs/ggen-toml-guide.md)
- [Complete Reference](../../docs/ggen-toml-reference.md)
- [Migration Guide](../../docs/ggen-toml-migration.md)
