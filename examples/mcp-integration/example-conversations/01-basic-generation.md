# Example 1: Basic Project Generation

This example shows how to use ggen-mcp to generate projects from templates in Claude Desktop or Cline.

## Conversation Flow

### User:
```
I need to create a new Rust CLI application. Can you help me find and generate a suitable template?
```

### Claude:
I'll help you find and generate a Rust CLI template. Let me search the ggen marketplace for Rust CLI templates.

**Tool Call:** `market_search`
```json
{
  "query": "rust cli",
  "category": "cli",
  "limit": 10
}
```

**Response:**
```json
{
  "status": "success",
  "packages": [
    {
      "id": "rust-cli-boilerplate",
      "name": "Rust CLI Boilerplate",
      "description": "Production-ready Rust CLI with clap, error handling, and tests",
      "version": "1.2.0",
      "downloads": 15234,
      "rating": 4.8
    },
    {
      "id": "simple-rust-cli",
      "name": "Simple Rust CLI",
      "description": "Minimal Rust CLI template with basic structure",
      "version": "1.0.1",
      "downloads": 8901,
      "rating": 4.5
    }
  ]
}
```

I found several Rust CLI templates. The "rust-cli-boilerplate" looks most comprehensive with production-ready features. Let me get more details about it.

**Tool Call:** `market_info`
```json
{
  "package_id": "rust-cli-boilerplate"
}
```

**Response:**
```json
{
  "status": "success",
  "package": {
    "id": "rust-cli-boilerplate",
    "name": "Rust CLI Boilerplate",
    "description": "Production-ready Rust CLI with clap, error handling, and tests",
    "version": "1.2.0",
    "author": "rustacean-dev",
    "license": "MIT",
    "features": [
      "Command-line argument parsing with clap",
      "Structured error handling with anyhow",
      "Logging with tracing",
      "Comprehensive unit tests",
      "CI/CD configuration"
    ],
    "variables": {
      "project_name": {
        "type": "string",
        "description": "Name of your CLI project",
        "required": true
      },
      "author": {
        "type": "string",
        "description": "Author name",
        "default": "Your Name"
      },
      "description": {
        "type": "string",
        "description": "Project description",
        "required": true
      },
      "enable_logging": {
        "type": "boolean",
        "description": "Include tracing/logging setup",
        "default": true
      }
    }
  }
}
```

This template includes clap for argument parsing, error handling, logging, and tests. Would you like me to generate a project from this template?

### User:
```
Yes, generate a project called "my-tool" with description "A helpful CLI tool" and enable logging
```

### Claude:
I'll generate the Rust CLI project for you using the rust-cli-boilerplate template.

**Tool Call:** `project_gen`
```json
{
  "template": "rust-cli-boilerplate",
  "vars": {
    "project_name": "my-tool",
    "description": "A helpful CLI tool",
    "author": "Your Name",
    "enable_logging": true
  }
}
```

**Response:**
```json
{
  "status": "success",
  "generated_files": [
    "my-tool/Cargo.toml",
    "my-tool/src/main.rs",
    "my-tool/src/cli.rs",
    "my-tool/src/error.rs",
    "my-tool/src/lib.rs",
    "my-tool/tests/integration_test.rs",
    "my-tool/.gitignore",
    "my-tool/README.md",
    "my-tool/.github/workflows/ci.yml"
  ],
  "output_dir": "/path/to/my-tool",
  "next_steps": [
    "cd my-tool",
    "cargo build",
    "cargo test",
    "cargo run -- --help"
  ]
}
```

Perfect! I've generated your Rust CLI project "my-tool" with:

✅ **9 files created:**
- `Cargo.toml` - Project configuration with clap, anyhow, and tracing dependencies
- `src/main.rs` - Entry point with CLI setup
- `src/cli.rs` - Command-line argument definitions
- `src/error.rs` - Error handling with anyhow
- `src/lib.rs` - Core library code
- `tests/integration_test.rs` - Integration tests
- `.gitignore` - Git ignore rules
- `README.md` - Project documentation
- `.github/workflows/ci.yml` - CI/CD pipeline

**Next steps:**
```bash
cd my-tool
cargo build
cargo test
cargo run -- --help
```

Your CLI is ready to customize! The template includes:
- Command-line parsing with clap
- Error handling with anyhow
- Logging with tracing (enabled)
- Unit and integration tests
- GitHub Actions CI/CD

## Dry Run (Preview) Mode

### User:
```
Before generating, can I preview what files will be created?
```

### Claude:
Absolutely! Let me create a plan showing what will be generated without actually creating the files.

**Tool Call:** `project_plan`
```json
{
  "template": "rust-cli-boilerplate",
  "vars": {
    "project_name": "my-tool",
    "description": "A helpful CLI tool",
    "enable_logging": true
  }
}
```

**Response:**
```json
{
  "status": "success",
  "plan": {
    "template": "rust-cli-boilerplate",
    "version": "1.2.0",
    "output_dir": "/path/to/my-tool",
    "actions": [
      {
        "type": "create",
        "path": "Cargo.toml",
        "size": 456,
        "preview": "[package]\nname = \"my-tool\"\nversion = \"0.1.0\"\n..."
      },
      {
        "type": "create",
        "path": "src/main.rs",
        "size": 1234,
        "preview": "use clap::Parser;\nuse my_tool::cli::Cli;\n..."
      },
      {
        "type": "create",
        "path": "src/cli.rs",
        "size": 789,
        "preview": "use clap::Parser;\n\n#[derive(Parser, Debug)]\n..."
      }
    ],
    "total_files": 9,
    "total_size": "12.4 KB"
  }
}
```

This plan shows:
- **9 files** will be created
- **Total size:** 12.4 KB
- **Output directory:** `/path/to/my-tool`

You can review each file's preview. When ready, use `project_apply` to execute the plan, or `project_gen` with `dry_run: false` to generate directly.

## Checking Differences

### User:
```
I already have a my-tool directory. What will change if I regenerate?
```

### Claude:
Let me check the differences between the template and your existing files.

**Tool Call:** `project_diff`
```json
{
  "template": "rust-cli-boilerplate",
  "vars": {
    "project_name": "my-tool",
    "description": "A helpful CLI tool",
    "enable_logging": true
  }
}
```

**Response:**
```json
{
  "status": "success",
  "differences": [
    {
      "file": "Cargo.toml",
      "status": "modified",
      "diff": "@@ -5,7 +5,7 @@\n-version = \"0.1.0\"\n+version = \"1.0.0\"\n"
    },
    {
      "file": "src/error.rs",
      "status": "new",
      "reason": "File doesn't exist in current project"
    },
    {
      "file": "src/old_module.rs",
      "status": "deleted",
      "reason": "File exists but not in template"
    }
  ],
  "summary": {
    "modified": 1,
    "new": 1,
    "deleted": 1,
    "unchanged": 6
  }
}
```

Differences found:
- **Modified (1):** `Cargo.toml` - version will change from 0.1.0 to 1.0.0
- **New (1):** `src/error.rs` - will be added
- **Deleted (1):** `src/old_module.rs` - exists but not in template
- **Unchanged (6):** Other files match the template

⚠️ **Warning:** Regenerating will overwrite modified files and delete files not in the template.

Consider:
1. Backup your changes
2. Use `force: false` to abort on conflicts
3. Cherry-pick specific files instead of full regeneration

## Key Takeaways

1. **Discovery:** Use `market_search` to find templates
2. **Research:** Use `market_info` to review template details
3. **Preview:** Use `project_plan` or `dry_run: true` to preview
4. **Diff:** Use `project_diff` to check against existing files
5. **Generate:** Use `project_gen` to create your project

## Related Tools

- `market_list` - Browse all templates by category
- `market_recommend` - Get personalized suggestions
- `template_validate` - Validate custom templates
- `project_apply` - Apply a saved execution plan
