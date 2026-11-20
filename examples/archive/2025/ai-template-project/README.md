# AI Template Project Generator

A comprehensive CLI tool for generating complete template projects using AI and publishing them to the ggen marketplace. This tool follows core team best practices for project structure, code quality, and marketplace integration.

## Features

- **AI-Powered Generation** - Uses AI to generate complete project templates
- **Multi-Language Support** - Supports Rust, Python, JavaScript, Go, and more
- **Framework Integration** - Built-in support for popular frameworks
- **Marketplace Integration** - Automatic metadata generation and publishing
- **Quality Validation** - Built-in validation and quality checks
- **CI/CD Ready** - Generates GitHub Actions workflows
- **Documentation** - Auto-generates README and API documentation
- **Testing** - Includes test templates and configurations

## Installation

```bash
cd examples/ai-template-project
cargo build --release
```

## Usage

### Generate a Complete Project

```bash
# Generate a Python FastAPI service
cargo run -- generate \
  --description "A REST API for user management" \
  --name "user-api" \
  --language python \
  --framework fastapi \
  --tests --docs --ci \
  --output ./my-project

# Generate a Rust CLI application
cargo run -- generate \
  --description "A command-line tool for file management" \
  --name "file-manager" \
  --language rust \
  --framework clap \
  --tests --docs --ci

# Generate a JavaScript Express app
cargo run -- generate \
  --description "A web application with user authentication" \
  --name "auth-app" \
  --language javascript \
  --framework express \
  --tests --docs --publish
```

### List Available Templates

```bash
# List all templates
cargo run -- list

# Filter by language
cargo run -- list --language rust

# Filter by framework
cargo run -- list --framework fastapi

# Show only AI-generated templates
cargo run -- list --ai-generated
```

### Validate Projects

```bash
# Basic validation
cargo run -- validate --project ./my-project

# Strict validation with additional checks
cargo run -- validate --project ./my-project --strict
```

### Show Examples

```bash
# Display example projects and usage
cargo run -- examples
```

## Supported Languages and Frameworks

### Rust
- **actix-web** - High-performance web framework
- **axum** - Modern web framework
- **clap** - Command-line argument parser

### Python
- **fastapi** - Modern web framework for APIs
- **django** - Full-stack web framework
- **flask** - Lightweight web framework

### JavaScript
- **express** - Web application framework
- **next** - React framework
- **nestjs** - Node.js framework

### Go
- **gin** - HTTP web framework

## Generated Project Structure

Each generated project includes:

### Core Files
- **Configuration** - Language-specific config files (Cargo.toml, pyproject.toml, package.json, go.mod)
- **Source Code** - Main application files with framework integration
- **Library Interface** - Library/module structure for reusable components

### Optional Components
- **Tests** - Unit and integration test templates
- **Documentation** - README.md and API documentation
- **CI/CD** - GitHub Actions workflows
- **Framework Files** - Framework-specific handlers, routes, middleware

### Marketplace Metadata
- **ggen.toml** - Complete marketplace metadata
- **Quality Metrics** - AI confidence scores and validation results
- **Dependencies** - Complete dependency list
- **Features** - Project feature tags

## Example Generated Projects

### Python FastAPI Service

```python
# src/main.py
from fastapi import FastAPI, HTTPException
from pydantic import BaseModel
from typing import List, Optional
import uvicorn

app = FastAPI(
    title="user-api",
    description="A REST API for user management",
    version="0.1.0",
)

class User(BaseModel):
    id: int
    name: str
    email: str

class CreateUserRequest(BaseModel):
    name: str
    email: str

@app.get("/")
async def root():
    return {"message": "user-api is running!", "status": "healthy"}

@app.get("/users", response_model=List[User])
async def get_users():
    return [
        User(id=1, name="John Doe", email="john@example.com"),
        User(id=2, name="Jane Smith", email="jane@example.com"),
    ]

@app.post("/users", response_model=User)
async def create_user(user: CreateUserRequest):
    return User(id=3, name=user.name, email=user.email)

if __name__ == "__main__":
    uvicorn.run(app, host="127.0.0.1", port=8080)
```

### Rust CLI Application

```rust
// src/main.rs
use clap::{Arg, Command};

fn main() {
    let matches = Command::new("file-manager")
        .version("0.1.0")
        .author("AI Generated")
        .about("A command-line tool for file management")
        .arg(
            Arg::new("name")
                .short('n')
                .long("name")
                .value_name("NAME")
                .help("Your name")
                .required(true),
        )
        .get_matches();

    let name = matches.get_one::<String>("name").unwrap();
    println!("Hello, {}! Welcome to file-manager", name);
}
```

## Marketplace Integration

### Automatic Metadata Generation

The tool automatically generates comprehensive marketplace metadata:

```toml
[package]
name = "user-api"
version = "0.1.0"
description = "A REST API for user management"
authors = ["AI Generated <ai@ggen.dev>"]
license = "MIT"
repository = "https://github.com/ggen/user-api"
homepage = "https://ggen.dev/packages/user-api"
keywords = ["ai-generated", "template", "python"]
categories = ["templates"]

[package.metadata.ggen]
generated_by = "ggen-ai"
generation_date = "2025-10-10"
language = "python"
framework = Some("fastapi")
features = ["api"]
dependencies = ["pydantic", "typing-extensions", "fastapi", "uvicorn"]
quality_score = 0.85
ai_model = "qwen3-coder:30b"

[package.metadata.ggen.template]
type = "project"
structure = ["pyproject.toml", "src/main.py", "src/__init__.py", "tests/test_main.py", "README.md", ".github/workflows/ci.yml"]
files_count = 6
includes_tests = true
includes_docs = true
includes_ci = true

[package.metadata.ggen.ai]
prompt = "A REST API for user management"
confidence = 0.9
validation_passed = true
```

### Publishing to Marketplace

```bash
# Generate and publish in one command
cargo run -- generate \
  --description "A REST API for user management" \
  --name "user-api" \
  --language python \
  --framework fastapi \
  --publish

# Or publish an existing project
cargo run -- generate \
  --description "A REST API for user management" \
  --name "user-api" \
  --language python \
  --framework fastapi \
  --output ./existing-project \
  --publish
```

## Quality Assurance

### Validation Features

- **File Structure** - Validates required files and directories
- **Metadata** - Checks marketplace metadata completeness
- **Source Code** - Validates source file presence and structure
- **Tests** - Checks for test files and configurations
- **Documentation** - Validates documentation completeness
- **CI/CD** - Checks for CI/CD configuration

### Quality Metrics

- **AI Confidence** - Confidence score from AI generation
- **Validation Score** - Overall project validation score
- **Completeness** - Feature completeness assessment
- **Best Practices** - Adherence to language and framework best practices

## AI Integration

### Current Implementation

The tool currently uses a mock AI implementation that demonstrates the concept with:

- **Intent Recognition** - Understands project requirements
- **Framework Selection** - Chooses appropriate frameworks
- **Code Generation** - Generates production-ready code
- **Best Practices** - Follows language and framework conventions

### Future Enhancements

- **Real AI Integration** - Connect to Ollama with qwen3-coder:30b
- **Learning System** - Improve based on user feedback
- **Custom Prompts** - Allow custom AI prompts
- **Quality Improvement** - AI-driven quality enhancement

## Best Practices

### Project Structure

- **Modular Design** - Clear separation of concerns
- **Configuration Management** - Centralized configuration
- **Error Handling** - Comprehensive error handling
- **Logging** - Structured logging implementation
- **Testing** - Comprehensive test coverage

### Code Quality

- **Type Safety** - Strong typing where applicable
- **Documentation** - Comprehensive inline documentation
- **Code Style** - Consistent code formatting
- **Security** - Security best practices
- **Performance** - Performance optimization

### Marketplace Standards

- **Metadata Completeness** - Complete marketplace metadata
- **Quality Scores** - Transparent quality metrics
- **Versioning** - Semantic versioning
- **Dependencies** - Clear dependency management
- **Documentation** - Comprehensive user documentation

## Integration with ggen

### CLI Integration

```bash
# Use with ggen marketplace
ggen market search "python fastapi"
ggen market add "user-api"
ggen market publish --project ./my-project
```

### Template System

```bash
# Generate templates for ggen
ggen ai project --description "A REST API" --name "user-api" --language python
ggen project gen --template user-api
```

## Contributing

### Development Setup

```bash
# Clone the repository
git clone <repository-url>
cd examples/ai-template-project

# Build and test
cargo build
cargo test

# Run examples
cargo run -- examples
cargo run -- generate --help
```

### Adding New Languages

1. Add language support in `generate_project_structure`
2. Implement language-specific file generators
3. Add framework support
4. Update validation logic
5. Add tests and documentation

### Adding New Frameworks

1. Add framework detection logic
2. Implement framework-specific files
3. Add framework dependencies
4. Update project structure
5. Add framework-specific tests

## License

This project follows the same license as the main ggen project.

## Support

For issues and questions:
- GitHub Issues: [Create an issue](https://github.com/ggen/)
- Documentation: [ggen.dev](https://ggen.dev)
- Community: [Discord](https://discord.gg/ggen)
