# ggen Library Usage & Wrapping - Complete Index

Quick navigation index for all files and resources in this comprehensive example project.

## 📁 Project Files

### 📋 Main Documentation

| File | Description | Start Here? |
|------|-------------|-------------|
| [README.md](README.md) | Main project documentation and overview | ⭐ **YES** |
| [QUICKSTART.md](QUICKSTART.md) | 5-minute quick start guide | ⭐ **YES** |
| [SUMMARY.md](SUMMARY.md) | Complete project summary | ✓ |
| [INDEX.md](INDEX.md) | This navigation file | ✓ |

### 📚 Documentation

| File | Description | Level |
|------|-------------|-------|
| [docs/USAGE_GUIDE.md](docs/USAGE_GUIDE.md) | Comprehensive usage guide | Intermediate |
| [docs/API_REFERENCE.md](docs/API_REFERENCE.md) | Complete API reference | Advanced |

### 💻 Examples

#### Core Library Examples

| Example | Description | Key Concepts | Difficulty |
|---------|-------------|--------------|------------|
| [examples/basic-usage.rs](examples/basic-usage.rs) | Template loading and rendering | Templates, Context, Generator | Beginner |
| [examples/with-ai.rs](examples/with-ai.rs) | AI-powered template generation | LLM clients, AI generation | Intermediate |
| [examples/custom-pipeline.rs](examples/custom-pipeline.rs) | Multi-stage pipelines | Pipelines, Chaining | Intermediate |
| [examples/batch-processor.rs](examples/batch-processor.rs) | Parallel template processing | Concurrency, Tokio | Advanced |
| [examples/graph-operations.rs](examples/graph-operations.rs) | RDF/SPARQL operations | Graphs, SPARQL | Advanced |
| [examples/template-validation.rs](examples/template-validation.rs) | Template validation | Security, Validation | Intermediate |

**Run all examples:**
```bash
./run-examples.sh
```

### 🔧 Wrapper Implementations

#### REST API Wrapper

| Component | File | Description |
|-----------|------|-------------|
| Configuration | [wrappers/rest-api/Cargo.toml](wrappers/rest-api/Cargo.toml) | Dependencies and setup |
| Implementation | [wrappers/rest-api/src/main.rs](wrappers/rest-api/src/main.rs) | Actix-web HTTP server |

**Features:**
- ✅ RESTful endpoints
- ✅ OpenAPI/Swagger docs
- ✅ CORS support
- ✅ Rate limiting
- ✅ Error handling

**Run:**
```bash
cd wrappers/rest-api
cargo run
# Visit: http://localhost:8080/swagger-ui/
```

#### Custom CLI Wrapper

| Component | File | Description |
|-----------|------|-------------|
| Configuration | [wrappers/custom-cli/Cargo.toml](wrappers/custom-cli/Cargo.toml) | Dependencies and setup |
| Implementation | [wrappers/custom-cli/src/main.rs](wrappers/custom-cli/src/main.rs) | Enhanced CLI tool |

**Features:**
- ✅ Interactive mode
- ✅ AI-powered generation
- ✅ Template validation
- ✅ Colored output
- ✅ Progress bars

**Run:**
```bash
cd wrappers/custom-cli
cargo run -- --help
cargo run -- interactive
```

### 📝 Sample Templates

| Template | Description | Use Case |
|----------|-------------|----------|
| [templates/simple-hello.md](templates/simple-hello.md) | Simple greeting template | Learning basics |
| [templates/sample-api.md](templates/sample-api.md) | REST API endpoint | Real-world example |

### ⚙️ Configuration

| File | Purpose |
|------|---------|
| [Cargo.toml](Cargo.toml) | Main project configuration |
| [wrappers/rest-api/Cargo.toml](wrappers/rest-api/Cargo.toml) | REST API dependencies |
| [wrappers/custom-cli/Cargo.toml](wrappers/custom-cli/Cargo.toml) | CLI dependencies |

### 🚀 Automation

| File | Description |
|------|-------------|
| [run-examples.sh](run-examples.sh) | Run all examples automatically |

## 🎯 Learning Paths

### Path 1: Quick Start (15 minutes)

1. Read [QUICKSTART.md](QUICKSTART.md)
2. Run `cargo run --example basic-usage`
3. Modify [templates/simple-hello.md](templates/simple-hello.md)
4. Run your modified template

### Path 2: Core Concepts (1 hour)

1. Read [README.md](README.md) - Core Concepts section
2. Run [examples/basic-usage.rs](examples/basic-usage.rs)
3. Run [examples/custom-pipeline.rs](examples/custom-pipeline.rs)
4. Study [docs/USAGE_GUIDE.md](docs/USAGE_GUIDE.md)

### Path 3: AI Features (1 hour)

1. Set API key: `export OPENAI_API_KEY="your-key"`
2. Run [examples/with-ai.rs](examples/with-ai.rs)
3. Try CLI AI mode: `cd wrappers/custom-cli && cargo run -- ai --description "your idea"`
4. Read AI sections in [docs/API_REFERENCE.md](docs/API_REFERENCE.md)

### Path 4: Advanced Features (2 hours)

1. Run [examples/batch-processor.rs](examples/batch-processor.rs)
2. Run [examples/graph-operations.rs](examples/graph-operations.rs)
3. Explore [wrappers/rest-api/src/main.rs](wrappers/rest-api/src/main.rs)
4. Study advanced patterns in [docs/USAGE_GUIDE.md](docs/USAGE_GUIDE.md)

### Path 5: Build Your Own (Ongoing)

1. Choose a wrapper as template
2. Read [docs/API_REFERENCE.md](docs/API_REFERENCE.md)
3. Implement your custom logic
4. Reference examples as needed

## 📊 Feature Matrix

### What Each Example Demonstrates

| Feature | basic-usage | with-ai | custom-pipeline | batch-processor | graph-ops | validation |
|---------|-------------|---------|-----------------|-----------------|-----------|------------|
| Template Loading | ✅ | ✅ | ✅ | ✅ | ❌ | ✅ |
| Context Variables | ✅ | ❌ | ✅ | ✅ | ❌ | ✅ |
| AI Generation | ❌ | ✅ | ❌ | ❌ | ❌ | ✅ |
| Pipelines | ❌ | ❌ | ✅ | ❌ | ❌ | ❌ |
| Parallel Processing | ❌ | ❌ | ❌ | ✅ | ❌ | ❌ |
| RDF/SPARQL | ❌ | ❌ | ❌ | ❌ | ✅ | ❌ |
| Validation | ❌ | ✅ | ❌ | ❌ | ❌ | ✅ |
| Error Handling | ✅ | ✅ | ✅ | ✅ | ✅ | ✅ |

### What Each Wrapper Demonstrates

| Feature | REST API | Custom CLI |
|---------|----------|------------|
| HTTP Endpoints | ✅ | ❌ |
| OpenAPI Docs | ✅ | ❌ |
| Interactive Mode | ❌ | ✅ |
| Progress Bars | ❌ | ✅ |
| AI Integration | ✅ | ✅ |
| Template Caching | ✅ | ❌ |
| CORS Support | ✅ | ❌ |
| Colored Output | ❌ | ✅ |

## 🔍 Quick Reference

### Core APIs Used

**ggen-core:**
- `Template::from_file()`, `Template::from_str()`
- `Generator::new()`, `generator.generate()`
- `GenContext::new()`, `context.insert()`
- `PipelineBuilder::new().with_template().build()`
- `Graph::new()`, `graph.add_triple()`, `graph.query()`

**ggen-ai:**
- `GenAiClient::new()`, `GenAiClient::with_config()`
- `TemplateGenerator::new()`, `generator.generate_template()`
- `TemplateValidator::new()`, `validator.validate()`
- `MockClient::with_response()` (for testing)

### Common Commands

```bash
# Build everything
cargo build --examples

# Run specific example
cargo run --example <name>

# Run all examples
./run-examples.sh

# Run tests
cargo test

# Start REST API
cd wrappers/rest-api && cargo run

# Use custom CLI
cd wrappers/custom-cli && cargo run -- <command>
```

### Environment Variables

```bash
# For AI features
export OPENAI_API_KEY="sk-..."
export ANTHROPIC_API_KEY="sk-ant-..."

# For configuration
export GGEN_CACHE_DIR="./cache"
export GGEN_OUTPUT_DIR="./output"
export RUST_LOG="info"
```

## 📦 Dependencies Overview

### Core Dependencies
- `ggen-core` - Template engine and graph operations
- `ggen-ai` - AI-powered generation
- `tokio` - Async runtime
- `anyhow` - Error handling
- `serde` - Serialization

### Wrapper Dependencies

**REST API:**
- `actix-web` - Web framework
- `actix-cors` - CORS support
- `utoipa` - OpenAPI docs

**Custom CLI:**
- `clap` - CLI parsing
- `dialoguer` - Interactive prompts
- `indicatif` - Progress bars
- `colored` - Colored output

## 🐛 Troubleshooting

### Common Issues

**Issue:** API key not found
- **File:** [examples/with-ai.rs](examples/with-ai.rs)
- **Solution:** `export OPENAI_API_KEY="your-key"`

**Issue:** Template parse error
- **File:** [examples/template-validation.rs](examples/template-validation.rs)
- **Solution:** Check frontmatter YAML syntax

**Issue:** Port 8080 already in use
- **File:** [wrappers/rest-api/src/main.rs](wrappers/rest-api/src/main.rs)
- **Solution:** Change port in source or kill existing process

## 📞 Support

- **Main Repo:** https://github.com/seanchatmangpt/ggen
- **Issues:** https://github.com/seanchatmangpt/ggen/issues
- **Documentation:** See [README.md](README.md) and [docs/](docs/)

## ✅ Checklist

Use this to track your progress:

- [ ] Read [QUICKSTART.md](QUICKSTART.md)
- [ ] Run [examples/basic-usage.rs](examples/basic-usage.rs)
- [ ] Try [templates/simple-hello.md](templates/simple-hello.md)
- [ ] Run [examples/with-ai.rs](examples/with-ai.rs) (with API key)
- [ ] Explore [examples/custom-pipeline.rs](examples/custom-pipeline.rs)
- [ ] Test [examples/batch-processor.rs](examples/batch-processor.rs)
- [ ] Try [examples/graph-operations.rs](examples/graph-operations.rs)
- [ ] Review [examples/template-validation.rs](examples/template-validation.rs)
- [ ] Start [wrappers/rest-api](wrappers/rest-api/)
- [ ] Use [wrappers/custom-cli](wrappers/custom-cli/)
- [ ] Read [docs/USAGE_GUIDE.md](docs/USAGE_GUIDE.md)
- [ ] Study [docs/API_REFERENCE.md](docs/API_REFERENCE.md)
- [ ] Run all tests: `cargo test`
- [ ] Build your own wrapper!

---

**Navigation Tip:** Use `Ctrl+F` to search this index for specific topics, file names, or features.

Last Updated: 2025-01-11
