# agent-reasoning-mcp

Production-ready AI agent package for ggen marketplace.

## Features

See `package.toml` for complete feature list.

## Installation

```bash
ggen market install agent-reasoning-mcp
```

## Usage

```rust
use agent_reasoning_mcp::Agent;

let agent = Agent::new()?;
agent.run()?;
```

## Documentation

- [API Reference](docs/api.md)
- [Examples](examples/)
- [RDF Ontology](rdf/ontology.ttl)
- [SPARQL Queries](sparql/)

## Testing

```bash
cargo test --package agent_reasoning_mcp
```

## License

MIT
