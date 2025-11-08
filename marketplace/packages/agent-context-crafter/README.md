# agent-context-crafter

Production-ready AI agent package for ggen marketplace.

## Features

See `package.toml` for complete feature list.

## Installation

```bash
ggen market add agent-context-crafter
```

## Usage

```rust
use agent_context_crafter::Agent;

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
cargo test --package agent_context_crafter
```

## License

MIT
