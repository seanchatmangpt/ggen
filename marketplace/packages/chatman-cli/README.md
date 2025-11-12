# ChatMan CLI

[![Crates.io](https://img.shields.io/crates/v/chatman-cli.svg)](https://crates.io/crates/chatman-cli)
[![Documentation](https://docs.rs/chatman-cli/badge.svg)](https://docs.rs/chatman-cli)
[![License](https://img.shields.io/badge/license-MIT%2FApache--2.0-blue.svg)](LICENSE-MIT)
[![Build Status](https://img.shields.io/github/actions/workflow/status/seanchatmangpt/ggen/ci.yml?branch=master)](https://github.com/seanchatmangpt/ggen/actions)

> **Knowledge Hook-powered chat automation framework with semantic understanding, AI integration, and production-grade conversation management.**

ChatMan CLI brings AI-powered conversational interfaces to your command-line applications with semantic knowledge understanding through RDF ontologies and SPARQL queries.

## Features

🧠 **Knowledge Hook Integration**
- Semantic conversation understanding via RDF ontologies
- SPARQL-based knowledge retrieval and reasoning
- Context-aware conversation flows

🤖 **AI Provider Support**
- OpenAI (GPT-3.5, GPT-4)
- Anthropic (Claude)
- Local LLM support (llama.cpp, etc.)
- Custom provider plugins

💬 **Conversation Management**
- Multi-turn context tracking
- Session persistence and recovery
- Conversation history export (JSON, YAML, Markdown)
- Template-based response generation

🚀 **Production Features**
- Async/await architecture with Tokio
- Comprehensive error handling
- Structured logging with tracing
- Rate limiting and retry logic
- Environment-based configuration

🎨 **CLI Excellence**
- Beautiful colored output
- Interactive prompts and menus
- Progress indicators for long operations
- Multiple output formats

## Installation

### From crates.io

```bash
cargo install chatman-cli
```

### From ggen marketplace

```bash
ggen market install chatman-cli
```

### From source

```bash
git clone https://github.com/seanchatmangpt/ggen
cd ggen/marketplace/packages/chatman-cli
cargo install --path .
```

## Quick Start

### As a CLI tool

```bash
# Interactive chat session
chatman chat

# Single question
chatman ask "What is the capital of France?"

# Chat with specific AI provider
chatman chat --provider openai --model gpt-4

# Export conversation
chatman chat --export conversation.json
```

### As a library

```rust
use chatman_cli::{ChatManager, Config, Message};
use anyhow::Result;

#[tokio::main]
async fn main() -> Result<()> {
    let config = Config::default();
    let mut manager = ChatManager::new(config)?;

    let response = manager.send_message(
        Message::user("Tell me about Rust async programming")
    ).await?;

    println!("Assistant: {}", response.content);
    Ok(())
}
```

### Knowledge Hook example

```rust
use chatman_cli::{ChatManager, KnowledgeHook};
use anyhow::Result;

#[tokio::main]
async fn main() -> Result<()> {
    let mut manager = ChatManager::new(Config::default())?;
    manager.add_hook(KnowledgeHook::from_ontology("rdf/chatman.ttl")?);

    let response = manager.send_message(
        Message::user("What conversation patterns do you support?")
    ).await?;

    println!("{}", response.content);
    Ok(())
}
```

## Configuration

### Environment variables

```bash
export CHATMAN_PROVIDER=openai
export CHATMAN_MODEL=gpt-4
export OPENAI_API_KEY=sk-...
export CHATMAN_MAX_HISTORY=100
export RUST_LOG=chatman_cli=debug
```

## Examples

See [examples/](examples/) directory:
- **basic.rs** - Simple chat interaction
- **knowledge_hooks.rs** - Using RDF ontologies
- **streaming.rs** - Real-time streaming
- **batch_processing.rs** - Batch processing

## Documentation

- [API Documentation](https://docs.rs/chatman-cli)
- [Architecture Guide](docs/architecture.md)
- [Knowledge Hooks Guide](docs/knowledge-hooks.md)
- [AI Provider Integration](docs/providers.md)
- [Deployment Guide](docs/deployment.md)

## Testing

```bash
cargo test --package chatman-cli
cargo test --test integration_test
```

## License

Licensed under either of:
- Apache License, Version 2.0 ([LICENSE-APACHE](LICENSE-APACHE))
- MIT license ([LICENSE-MIT](LICENSE-MIT))

at your option.

## Acknowledgments

Built on the [ggen](https://github.com/seanchatmangpt/ggen) framework.
