# agent-memory-forge

Production-ready AI agent package for mcpp marketplace.

## Features

See `package.toml` for complete feature list.

## Installation

```bash
mcpp market install agent-memory-forge
```

## Usage

```rust
use agent_memory_forge::Agent;

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
cargo test --package agent_memory_forge
```

## License

MIT
