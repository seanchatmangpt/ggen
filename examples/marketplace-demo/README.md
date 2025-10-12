# Ggen Marketplace Demo

This demo project showcases how to use the ggen marketplace system to install and use packages.

## 🚀 Quick Start

1. **Initialize the project:**
   ```bash
   cd examples/marketplace-demo
   ggen lifecycle run init
   ```

2. **Install marketplace packages:**
   ```bash
   ggen lifecycle run setup
   ```

3. **Generate demo application:**
   ```bash
   ggen lifecycle run generate
   ```

4. **Build and run:**
   ```bash
   ggen lifecycle run build
   ggen lifecycle run run
   ```

## 📋 What This Demo Shows

### Marketplace Package Installation
The demo installs the `hello-world-utils` package from the marketplace:

```bash
ggen market add hello-world-utils
```

### Using Installed Packages
Once installed, the package can be used in your Rust code:

```rust
use hello_world_utils::{HelloWorld, HelloConfig};

fn main() -> anyhow::Result<()> {
    let hello = HelloWorld::default();
    println!("{}", hello.greet()); // "Hello World!"

    let config = HelloConfig {
        greeting: "Welcome".to_string(),
        name: "Marketplace".to_string(),
        repeat_count: 3,
    };

    let custom_hello = HelloWorld::new(config);
    for greeting in custom_hello.greet_many() {
        println!("{}", greeting);
    }

    Ok(())
}
```

## 🔧 Available Commands

### Marketplace Commands
```bash
# Search for packages
ggen market search "hello"

# Install a package
ggen market add hello-world-utils

# List installed packages
ggen market list --installed

# Get package information
ggen market info hello-world-utils

# Update packages
ggen market update
```

### Lifecycle Commands
```bash
# List available phases
ggen lifecycle list

# Run specific phase
ggen lifecycle run setup

# Show phase details
ggen lifecycle show generate

# Run multiple phases
ggen lifecycle pipeline "init setup generate build run"
```

## 📦 Installed Package Features

The `hello-world-utils` package provides:

- **HelloWorld struct** - Main greeting utility
- **HelloConfig** - Configuration management
- **Validation functions** - Input validation
- **JSON serialization** - Configuration export
- **Multiple greetings** - Batch greeting generation
- **Example applications** - Demo usage

## 🧪 Testing

Run the test suite:
```bash
ggen lifecycle run test
```

This tests:
- Package installation verification
- Code compilation
- Unit tests
- Integration tests

## 🔍 Exploring the Marketplace

### Search for Packages
```bash
# Search by name
ggen market search "hello"

# Search by category
ggen market search "utilities"

# Search by tags
ggen market search "demo"
```

### Package Information
```bash
# Get detailed info
ggen market info hello-world-utils

# See examples
ggen market info hello-world-utils --examples

# Check dependencies
ggen market info hello-world-utils --dependencies
```

### Browse Offline
```bash
# Browse cached marketplace data
ggen market offline search "rust"

# List categories offline
ggen market offline categories
```

## 🚀 Next Steps

1. **Explore other packages:**
   ```bash
   ggen market search "api"
   ggen market search "database"
   ```

2. **Install more packages:**
   ```bash
   ggen market add rig-mcp-integration
   ggen market add api-endpoint-templates
   ```

3. **Create your own package:**
   ```bash
   ggen market publish ./my-package
   ```

4. **Contribute to existing packages**
   - Fork the repository
   - Make improvements
   - Submit pull requests

## 🎯 Key Benefits

This demo shows how the ggen marketplace:

- **Simplifies dependency management** - Install packages with single commands
- **Provides curated packages** - Pre-vetted, high-quality packages
- **Enables code reuse** - Share and use common functionality
- **Supports versioning** - Manage package versions and updates
- **Facilitates collaboration** - Easy sharing of tools and templates

## 📚 Related Examples

- **Comprehensive Rust Showcase** - Full project using multiple marketplace packages
- **Template Examples** - See how to create and use templates
- **CLI Examples** - Command-line applications using marketplace packages

---

**This demo shows the power of the ggen marketplace ecosystem for Rust development!**
