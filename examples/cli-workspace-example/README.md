# CLI Workspace Example

This example demonstrates the complete CLI generation workflow using 2026 best practices.

## Structure

```
cli-workspace-example/
├── Cargo.toml              # Workspace manifest
└── crates/
    ├── example-cli/        # CLI presentation layer
    └── example-core/       # Domain/business logic layer
```

## Usage

### Generate the Project

```bash
# From the ggen project root
cd examples/cli-workspace-example
cargo build
```

### Run Commands

```bash
# From the generated project directory
cargo run -- user create --name "Alice"
cargo run -- user list
```

## Key Features Demonstrated

1. **Workspace Structure**: Separate CLI and domain crates
2. **Domain Function References**: CLI calls domain functions via stable paths
3. **clap-noun-verb v3.3.0**: Uses latest version with auto-inference
4. **Async Domain Logic**: Domain functions are async; CLI bridges to sync
5. **Type Safety**: Strong typing for domain inputs/outputs

## Architecture

### CLI Layer (`example-cli`)

- Uses `clap-noun-verb` v3.3.0 for command parsing
- Converts CLI args to domain input types
- Calls domain functions via `example_core::{module}::{verb}::execute`
- Formats output for CLI

### Domain Layer (`example-core`)

- Pure business logic, no CLI dependencies
- Async functions for all operations
- Strongly-typed input/output structs
- Testable in isolation

## Example Commands

```bash
# Create a user
cargo run -- user create --name "Alice" --email "alice@example.com"

# List users
cargo run -- user list

# Get user by ID
cargo run -- user get --id "123"

# Update user
cargo run -- user update --id "123" --name "Alice Updated"

# Delete user
cargo run -- user delete --id "123"
```

## Testing

```bash
# Run domain tests (no CLI needed)
cd crates/example-core
cargo test

# Run CLI tests
cd crates/example-cli
cargo test

# Run integration tests
cd ../..
cargo test
```

