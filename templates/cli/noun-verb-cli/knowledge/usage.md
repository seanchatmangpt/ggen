# Noun-Verb CLI Generator - Usage Guide

## Quick Reference

### Installation

```bash
# Add from marketplace
ggen market add noun-verb-cli-generator

# Verify installation
ggen market info noun-verb-cli-generator
```

### Basic Usage

```bash
# Generate a CLI with 3 nouns
ggen market use noun-verb-cli-generator \
  --var project_name="my-cli" \
  --var nouns="resource1,resource2,resource3" \
  --output ./my-cli-project

cd ./my-cli-project
cargo build
cargo test
```

### Variables Reference

| Variable | Type | Required | Default | Example |
|----------|------|----------|---------|---------|
| `project_name` | string | ✅ | - | "cloud-manager" |
| `nouns` | comma-separated | ✅ | - | "server,database,network" |
| `description` | string | ❌ | "A noun-verb CLI application" | "Cloud resource manager" |
| `authors` | array | ❌ | ["Your Name <email>"] | ["John Doe <john@example.com>"] |
| `version` | semver | ❌ | "0.1.0" | "1.0.0" |

## Generated Commands

For each noun, you get 5 CRUD operations:

### Create Command

```bash
<cli> <noun> create --name <name> [--description <desc>] [--format <fmt>] [--verbose]
```

**Example:**
```bash
cloud-manager server create --name web-1 --description "Web server" --verbose
```

### List Command

```bash
<cli> <noun> list [--filter <pattern>] [--limit <n>] [--format <fmt>] [--all]
```

**Example:**
```bash
cloud-manager server list --filter "web" --limit 10 --format json
```

### Get Command

```bash
<cli> <noun> get <id> [--format <fmt>] [--detailed]
```

**Example:**
```bash
cloud-manager server get web-1 --format yaml --detailed
```

### Update Command

```bash
<cli> <noun> update <id> [--name <name>] [--description <desc>] [--dry-run] [--verbose]
```

**Example:**
```bash
cloud-manager server update web-1 --name web-primary --dry-run
```

### Delete Command

```bash
<cli> <noun> delete <id> [--force] [--dry-run] [--verbose]
```

**Example:**
```bash
cloud-manager server delete web-1 --force --dry-run
```

## Common Workflows

### 1. Infrastructure Management

```bash
# Generate infrastructure CLI
ggen market use noun-verb-cli-generator \
  --var project_name="infra-cli" \
  --var nouns="server,database,loadbalancer,firewall"

# Use it
./target/debug/infra-cli server create --name prod-web-01
./target/debug/infra-cli database list --format json
./target/debug/infra-cli firewall get fw-123
```

### 2. User Management

```bash
# Generate user admin CLI
ggen market use noun-verb-cli-generator \
  --var project_name="user-admin" \
  --var nouns="user,team,role,permission"

# Use it
./target/debug/user-admin user create --name john --description "Admin"
./target/debug/user-admin team list --limit 20
./target/debug/user-admin role get admin --detailed
```

### 3. CI/CD Pipeline

```bash
# Generate pipeline CLI
ggen market use noun-verb-cli-generator \
  --var project_name="pipeline-cli" \
  --var nouns="pipeline,job,artifact,deployment"

# Use it
./target/debug/pipeline-cli pipeline create --name build-prod
./target/debug/pipeline-cli job list --filter "failed"
./target/debug/pipeline-cli deployment delete old-deploy --force
```

## Customization After Generation

### Adding Custom Verbs

Edit `src/cmds/<noun>/mod.rs`:

```rust
#[derive(Subcommand, Debug)]
pub enum Verb {
    Create(create::CreateArgs),
    List(list::ListArgs),
    Get(get::GetArgs),
    Update(update::UpdateArgs),
    Delete(delete::DeleteArgs),

    // Add custom verbs
    Start(start::StartArgs),
    Stop(stop::StopArgs),
    Restart(restart::RestartArgs),
}
```

### Adding Custom Fields

Edit `src/cmds/<noun>/<verb>.rs`:

```rust
#[derive(Debug, Args)]
pub struct CreateArgs {
    #[arg(short, long)]
    pub name: String,

    // Add custom fields
    #[arg(short, long)]
    pub region: Option<String>,

    #[arg(short, long, default_value = "t3.micro")]
    pub instance_type: String,
}
```

### Implementing Real Logic

Replace TODO comments:

```rust
pub async fn run(args: &CreateArgs) -> Result<()> {
    // Replace TODO with real implementation
    let client = MyApiClient::new()?;

    let resource = client.create_resource(
        &args.name,
        args.description.as_deref(),
    ).await?;

    println!("✅ Created: {}", resource.id);
    Ok(())
}
```

## Output Formats

All list and get commands support three formats:

### Table Format (Default)

```bash
$ my-cli resource list
NAME                 DESCRIPTION
------------------------------------------------------------
resource-1           First resource
resource-2           Second resource
resource-3           Third resource

Total: 3 resource(s)
```

### JSON Format

```bash
$ my-cli resource list --format json
[
  {
    "name": "resource-1",
    "description": "First resource"
  },
  {
    "name": "resource-2",
    "description": "Second resource"
  }
]
```

### YAML Format

```bash
$ my-cli resource get res-123 --format yaml
id: res-123
name: resource-res-123
description: Example resource
created_at: '2025-10-10T12:00:00Z'
status: active
```

## Testing Generated CLIs

### Unit Tests

```bash
# Run all unit tests
cargo test --lib

# Run specific module
cargo test --lib server

# Run with output
cargo test --lib -- --nocapture
```

### Integration Tests

```bash
# Run all integration tests
cargo test --test integration_test

# Run specific test
cargo test --test integration_test test_server_create

# Show output
cargo test --test integration_test -- --nocapture
```

### Manual Testing

```bash
# Build release binary
cargo build --release

# Test help
./target/release/my-cli --help
./target/release/my-cli resource --help

# Test commands
./target/release/my-cli resource create --name test
./target/release/my-cli resource list
./target/release/my-cli resource get test
```

## Troubleshooting

### Build Errors

**Issue:** Compilation errors after generation

**Solution:**
```bash
# Clean and rebuild
cargo clean
cargo build

# Check Rust version
rustc --version  # Should be 1.70+

# Update dependencies
cargo update
```

### Test Failures

**Issue:** Integration tests fail

**Solution:**
```bash
# Check if binary exists
cargo build

# Run tests with verbose output
cargo test -- --nocapture

# Run single test
cargo test test_help -- --nocapture
```

### Runtime Errors

**Issue:** CLI panics or shows errors

**Solution:**
```bash
# Run with debug output
RUST_LOG=debug ./target/debug/my-cli resource list

# Check help text
./target/debug/my-cli --help

# Verify arguments
./target/debug/my-cli resource create --help
```

## Best Practices

### 1. Use Descriptive Noun Names

```bash
# ✅ Good: Clear, domain-specific nouns
--var nouns="server,database,network"

# ❌ Bad: Generic, unclear nouns
--var nouns="thing,item,object"
```

### 2. Implement Validation

Add validation to your commands:

```rust
pub async fn run(args: &CreateArgs) -> Result<()> {
    // Validate inputs
    if args.name.trim().is_empty() {
        return Err(anyhow::anyhow!("Name cannot be empty"));
    }

    if args.name.len() > 64 {
        return Err(anyhow::anyhow!("Name too long (max 64 chars)"));
    }

    // Continue with logic...
}
```

### 3. Use Dry-Run for Testing

Always test destructive operations:

```bash
# Test before deleting
my-cli resource delete res-123 --dry-run

# Confirm output, then run for real
my-cli resource delete res-123 --force
```

### 4. JSON Output for Automation

Use JSON for scripts:

```bash
# Get JSON output
my-cli resource list --format json | jq '.[] | .name'

# Filter and process
my-cli resource list --format json | jq '.[] | select(.status == "active")'
```

## Next Steps

1. **Generate your first CLI** using the marketplace
2. **Build and test** with cargo
3. **Customize** by implementing TODOs
4. **Deploy** to users

Full documentation: [CLI_GENERATOR_GUIDE.md](../docs/CLI_GENERATOR_GUIDE.md)
