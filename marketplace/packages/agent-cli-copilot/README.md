# agent-cli-copilot

Production-ready AI agent package for ggen marketplace.

## Features

See `package.toml` for complete feature list.

## Installation

```bash
ggen market add agent-cli-copilot
```

## Usage

```rust
use agent_cli_copilot::Agent;

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
cargo test --package agent_cli_copilot
```

## License

MIT
