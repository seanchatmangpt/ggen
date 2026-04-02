# agent-cli-copilot API Reference

## Core Types

### Agent

Main agent struct.

```rust
pub struct Agent {
    config: Config,
    state: State,
}
```

### Config

Configuration options.

```rust
pub struct Config {
    pub enable_semantic: bool,
    pub rdf_store_path: PathBuf,
}
```

## Functions

### `new() -> Result<Agent>`

Creates a new agent instance.

### `run() -> Result<()>`

Runs the agent main loop.

## Examples

See `examples/` directory.
