#!/usr/bin/env bash
#
# Generate a complete noun-verb CLI using ggen templates
#
# Usage:
#   ./generate-noun-verb-cli.sh <project-name> <noun1,noun2,...> <output-dir>
#
# Example:
#   ./generate-noun-verb-cli.sh cloud-manager "server,database,network" /tmp/cloud-cli
#

set -euo pipefail

# Color codes
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m'

# Helper functions
info() {
    echo -e "${BLUE}ℹ️  ${NC}$1"
}

success() {
    echo -e "${GREEN}✅ ${NC}$1"
}

error() {
    echo -e "${RED}❌ ${NC}$1"
    exit 1
}

warn() {
    echo -e "${YELLOW}⚠️  ${NC}$1"
}

header() {
    echo -e "\n${BLUE}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
    echo -e "${BLUE}  $1${NC}"
    echo -e "${BLUE}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}\n"
}

# Parse arguments
if [ $# -lt 3 ]; then
    error "Usage: $0 <project-name> <noun1,noun2,...> <output-dir>"
fi

PROJECT_NAME="$1"
NOUNS="$2"
OUTPUT_DIR="$3"

# Convert comma-separated nouns to array
IFS=',' read -ra NOUN_ARRAY <<< "$NOUNS"

# Standard CRUD verbs
VERBS=("create" "list" "get" "update" "delete")

info "Project: $PROJECT_NAME"
info "Nouns: ${NOUN_ARRAY[*]}"
info "Output: $OUTPUT_DIR"

# Check if ggen binary exists
if ! command -v ggen &> /dev/null; then
    # Try to use cargo run
    if [ -f "Cargo.toml" ]; then
        GGEN_CMD="cargo run --release --"
        info "Using cargo run for ggen"
    else
        error "ggen binary not found. Build it first: cargo build --release"
    fi
else
    GGEN_CMD="ggen"
fi

# Create output directory
mkdir -p "$OUTPUT_DIR"
cd "$OUTPUT_DIR"

header "PHASE 1: SCAFFOLD CLI PROJECT"

info "Creating Cargo.toml..."
cat > Cargo.toml << 'EOF'
[package]
name = "PROJECT_NAME"
version = "0.1.0"
edition = "2021"
description = "A noun-verb CLI application"

[dependencies]
clap = { version = "4.5", features = ["derive", "env", "wrap_help"] }
tokio = { version = "1.38", features = ["full"] }
anyhow = "1.0"
serde = { version = "1.0", features = ["derive"] }
serde_json = "1.0"
serde_yaml = "0.9"

[dev-dependencies]
assert_cmd = "2"
assert_fs = "1"
predicates = "3"
tempfile = "3"

[[bin]]
name = "PROJECT_NAME"
path = "src/main.rs"
EOF

sed -i.bak "s/PROJECT_NAME/$PROJECT_NAME/g" Cargo.toml && rm Cargo.toml.bak
success "Created Cargo.toml"

# Create main.rs
info "Creating src/main.rs..."
mkdir -p src/cmds
cat > src/main.rs << 'EOF'
//! Noun-verb CLI application

use clap::Parser;
use std::process;

mod cmds;

#[derive(Parser, Debug)]
#[command(
    name = "PROJECT_NAME",
    about = "A noun-verb CLI application",
    version,
    propagate_version = true,
    arg_required_else_help = true
)]
struct Cli {
    #[command(subcommand)]
    command: cmds::Command,
}

#[tokio::main]
async fn main() {
    let cli = Cli::parse();

    if let Err(e) = cli.command.run().await {
        eprintln!("Error: {}", e);
        process::exit(1);
    }
}
EOF

sed -i.bak "s/PROJECT_NAME/$PROJECT_NAME/g" src/main.rs && rm src/main.rs.bak
success "Created src/main.rs"

# Create cmds/mod.rs with noun routing
info "Creating src/cmds/mod.rs..."
cat > src/cmds/mod.rs << 'EOF'
//! CLI command definitions

use clap::Subcommand;

NOUN_MODS

type Result<T> = std::result::Result<T, anyhow::Error>;

#[derive(Subcommand, Debug)]
pub enum Command {
NOUN_VARIANTS
}

impl Command {
    pub async fn run(&self) -> Result<()> {
        match self {
NOUN_MATCH_ARMS
        }
    }
}
EOF

# Build noun declarations
NOUN_MODS=""
NOUN_VARIANTS=""
NOUN_MATCH_ARMS=""

for noun in "${NOUN_ARRAY[@]}"; do
    NOUN_MODS="${NOUN_MODS}pub mod ${noun};\n"

    # Capitalize first letter for variant name
    NOUN_CAP="$(echo $noun | sed 's/.*/\u&/')"

    NOUN_VARIANTS="${NOUN_VARIANTS}    /// ${NOUN_CAP} management commands\n"
    NOUN_VARIANTS="${NOUN_VARIANTS}    ${NOUN_CAP}(${noun}::${NOUN_CAP}Cmd),\n"

    NOUN_MATCH_ARMS="${NOUN_MATCH_ARMS}            Command::${NOUN_CAP}(cmd) => cmd.run().await,\n"
done

# Replace placeholders
echo -e "$NOUN_MODS" > /tmp/noun_mods
echo -e "$NOUN_VARIANTS" > /tmp/noun_variants
echo -e "$NOUN_MATCH_ARMS" > /tmp/noun_match_arms

perl -i -pe 's/NOUN_MODS/`cat \/tmp\/noun_mods`/e' src/cmds/mod.rs
perl -i -pe 's/NOUN_VARIANTS/`cat \/tmp\/noun_variants`/e' src/cmds/mod.rs
perl -i -pe 's/NOUN_MATCH_ARMS/`cat \/tmp\/noun_match_arms`/e' src/cmds/mod.rs

success "Created src/cmds/mod.rs with ${#NOUN_ARRAY[@]} noun(s)"

header "PHASE 2: GENERATE NOUN MODULES"

for noun in "${NOUN_ARRAY[@]}"; do
    info "Creating noun module: $noun"

    NOUN_CAP="$(echo $noun | sed 's/.*/\u&/')"
    mkdir -p "src/cmds/$noun"

    # Create noun mod.rs with verb routing
    cat > "src/cmds/$noun/mod.rs" << EOF
//! ${NOUN_CAP} management commands

use clap::{Args, Subcommand};

$(for verb in "${VERBS[@]}"; do echo "pub mod $verb;"; done)

type Result<T> = std::result::Result<T, anyhow::Error>;

#[derive(Args, Debug)]
pub struct ${NOUN_CAP}Cmd {
    #[command(subcommand)]
    pub verb: Verb,
}

#[derive(Subcommand, Debug)]
pub enum Verb {
$(for verb in "${VERBS[@]}"; do
    VERB_CAP="$(echo $verb | sed 's/.*/\u&/')"
    echo "    /// ${VERB_CAP} a ${noun}"
    echo "    ///"
    echo "    /// Examples:"
    if [ "$verb" == "get" ] || [ "$verb" == "update" ] || [ "$verb" == "delete" ]; then
        echo "    ///   $PROJECT_NAME $noun $verb <id>"
    else
        echo "    ///   $PROJECT_NAME $noun $verb"
    fi
    echo "    ${VERB_CAP}(${verb}::${VERB_CAP}Args),"
    echo ""
done)
}

impl ${NOUN_CAP}Cmd {
    pub async fn run(&self) -> Result<()> {
        match &self.verb {
$(for verb in "${VERBS[@]}"; do
    VERB_CAP="$(echo $verb | sed 's/.*/\u&/')"
    echo "            Verb::${VERB_CAP}(args) => ${verb}::run(args).await,"
done)
        }
    }
}
EOF

    success "Created src/cmds/$noun/mod.rs"

    # Generate each verb implementation
    for verb in "${VERBS[@]}"; do
        info "  - Creating verb: $noun $verb"

        VERB_CAP="$(echo $verb | sed 's/.*/\u&/')"

        case "$verb" in
            "create")
                cat > "src/cmds/$noun/$verb.rs" << EOF
//! Create ${noun} command

use clap::Args;

type Result<T> = std::result::Result<T, anyhow::Error>;

#[derive(Debug, Args)]
pub struct ${VERB_CAP}Args {
    /// Name of the ${noun}
    #[arg(short, long)]
    pub name: String,

    /// Description
    #[arg(short, long)]
    pub description: Option<String>,

    /// Output format (json, yaml, table)
    #[arg(short = 'f', long, default_value = "table")]
    pub format: String,

    /// Enable verbose output
    #[arg(short, long)]
    pub verbose: bool,
}

pub async fn run(args: &${VERB_CAP}Args) -> Result<()> {
    if args.verbose {
        println!("Creating ${noun}: {}", args.name);
    }

    // TODO: Implement create logic
    println!("✅ ${NOUN_CAP} '{}' created successfully!", args.name);

    if let Some(desc) = &args.description {
        println!("   Description: {}", desc);
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_create_basic() {
        let args = ${VERB_CAP}Args {
            name: "test-${noun}".to_string(),
            description: Some("Test ${noun}".to_string()),
            format: "table".to_string(),
            verbose: false,
        };

        let result = run(&args).await;
        assert!(result.is_ok());
    }
}
EOF
                ;;

            "list")
                cat > "src/cmds/$noun/$verb.rs" << EOF
//! List ${noun}s command

use clap::Args;

type Result<T> = std::result::Result<T, anyhow::Error>;

#[derive(Debug, Args)]
pub struct ${VERB_CAP}Args {
    /// Filter by name pattern
    #[arg(short, long)]
    pub filter: Option<String>,

    /// Maximum number of items to display
    #[arg(short, long)]
    pub limit: Option<usize>,

    /// Output format (json, yaml, table)
    #[arg(short = 'f', long, default_value = "table")]
    pub format: String,

    /// Show all fields
    #[arg(short, long)]
    pub all: bool,
}

pub async fn run(args: &${VERB_CAP}Args) -> Result<()> {
    println!("📋 Listing ${noun}s...");

    // TODO: Implement list logic
    let items = vec![
        ("${noun}-1", "First ${noun}"),
        ("${noun}-2", "Second ${noun}"),
        ("${noun}-3", "Third ${noun}"),
    ];

    let filtered_items: Vec<_> = if let Some(filter) = &args.filter {
        items.into_iter()
            .filter(|(name, _)| name.contains(filter))
            .collect()
    } else {
        items.into_iter().collect()
    };

    let limited_items = if let Some(limit) = args.limit {
        &filtered_items[..limit.min(filtered_items.len())]
    } else {
        &filtered_items[..]
    };

    match args.format.as_str() {
        "json" => {
            let json_items: Vec<_> = limited_items
                .iter()
                .map(|(name, desc)| serde_json::json!({ "name": name, "description": desc }))
                .collect();
            println!("{}", serde_json::to_string_pretty(&json_items)?);
        }
        "table" => {
            println!("\n{:<20} {:<40}", "NAME", "DESCRIPTION");
            println!("{}", "-".repeat(60));
            for (name, desc) in limited_items {
                println!("{:<20} {:<40}", name, desc);
            }
            println!("\nTotal: {} ${noun}(s)", limited_items.len());
        }
        _ => {
            for (name, desc) in limited_items {
                println!("{}: {}", name, desc);
            }
        }
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_list_basic() {
        let args = ${VERB_CAP}Args {
            filter: None,
            limit: None,
            format: "table".to_string(),
            all: false,
        };

        let result = run(&args).await;
        assert!(result.is_ok());
    }
}
EOF
                ;;

            "get")
                cat > "src/cmds/$noun/$verb.rs" << EOF
//! Get ${noun} command

use clap::Args;

type Result<T> = std::result::Result<T, anyhow::Error>;

#[derive(Debug, Args)]
pub struct ${VERB_CAP}Args {
    /// ID or name of the ${noun}
    pub id: String,

    /// Output format (json, yaml, table)
    #[arg(short = 'f', long, default_value = "table")]
    pub format: String,

    /// Show detailed information
    #[arg(short, long)]
    pub detailed: bool,
}

pub async fn run(args: &${VERB_CAP}Args) -> Result<()> {
    println!("🔍 Getting ${noun}: {}", args.id);

    // TODO: Implement get logic
    let resource_data = serde_json::json!({
        "id": args.id,
        "name": format!("${noun}-{}", args.id),
        "description": "Example ${noun}",
        "created_at": "2025-10-10T12:00:00Z",
        "status": "active"
    });

    match args.format.as_str() {
        "json" => {
            println!("{}", serde_json::to_string_pretty(&resource_data)?);
        }
        "yaml" => {
            println!("{}", serde_yaml::to_string(&resource_data)?);
        }
        "table" => {
            println!("\n${NOUN_CAP} Details:");
            println!("{}", "=".repeat(50));
            if let Some(obj) = resource_data.as_object() {
                for (key, value) in obj {
                    println!("{:<15} {}", format!("{}:", key), value);
                }
            }
        }
        _ => println!("{:?}", resource_data),
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_get_basic() {
        let args = ${VERB_CAP}Args {
            id: "test-123".to_string(),
            format: "table".to_string(),
            detailed: false,
        };

        let result = run(&args).await;
        assert!(result.is_ok());
    }
}
EOF
                ;;

            "update"|"delete")
                if [ "$verb" == "update" ]; then
                    ARGS_FIELDS='    /// ID or name of the '"$noun"'
    pub id: String,

    /// New name
    #[arg(short, long)]
    pub name: Option<String>,

    /// New description
    #[arg(short, long)]
    pub description: Option<String>,

    /// Dry run (don'"'"'t apply changes)
    #[arg(long)]
    pub dry_run: bool,

    /// Enable verbose output
    #[arg(short, long)]
    pub verbose: bool,'
                else
                    ARGS_FIELDS='    /// ID or name of the '"$noun"'
    pub id: String,

    /// Force deletion without confirmation
    #[arg(short, long)]
    pub force: bool,

    /// Dry run (don'"'"'t actually delete)
    #[arg(long)]
    pub dry_run: bool,

    /// Enable verbose output
    #[arg(short, long)]
    pub verbose: bool,'
                fi

                cat > "src/cmds/$noun/$verb.rs" << EOF
//! ${VERB_CAP} ${noun} command

use clap::Args;

type Result<T> = std::result::Result<T, anyhow::Error>;

#[derive(Debug, Args)]
pub struct ${VERB_CAP}Args {
${ARGS_FIELDS}
}

pub async fn run(args: &${VERB_CAP}Args) -> Result<()> {
    if args.verbose {
        println!("${VERB_CAP}ing ${noun}: {}", args.id);
    }

    if args.dry_run {
        println!("🔍 Dry run mode - no changes will be applied");
    }

    // TODO: Implement ${verb} logic
    if !args.dry_run {
        println!("✅ ${NOUN_CAP} '{}' ${verb}d successfully!", args.id);
    } else {
        println!("Would ${verb} ${noun}: {}", args.id);
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_${verb}_basic() {
        let args = ${VERB_CAP}Args {
            id: "test-123".to_string(),
$(if [ "$verb" == "update" ]; then
echo "            name: Some(\"new-name\".to_string()),"
echo "            description: None,"
else
echo "            force: true,"
fi)
            dry_run: true,
            verbose: false,
        };

        let result = run(&args).await;
        assert!(result.is_ok());
    }
}
EOF
                ;;
        esac

        success "    Created src/cmds/$noun/$verb.rs"
    done
done

header "PHASE 3: CREATE INTEGRATION TESTS"

info "Creating tests/integration_test.rs..."
mkdir -p tests

cat > tests/integration_test.rs << EOF
//! Integration tests for $PROJECT_NAME

use assert_cmd::Command;
use predicates::prelude::*;

#[test]
fn test_help() {
    let mut cmd = Command::cargo_bin("$PROJECT_NAME").unwrap();
    cmd.arg("--help");
    cmd.assert()
        .success()
        .stdout(predicate::str::contains("$PROJECT_NAME"));
}

#[test]
fn test_version() {
    let mut cmd = Command::cargo_bin("$PROJECT_NAME").unwrap();
    cmd.arg("--version");
    cmd.assert()
        .success()
        .stdout(predicate::str::contains("0.1.0"));
}

$(for noun in "${NOUN_ARRAY[@]}"; do
    for verb in "${VERBS[@]}"; do
        cat << TESTEOF

#[test]
fn test_${noun}_${verb}() {
    let mut cmd = Command::cargo_bin("$PROJECT_NAME").unwrap();
    cmd.arg("$noun").arg("$verb");

$(if [ "$verb" == "create" ]; then
    echo '    cmd.arg("--name").arg("test-'"$noun"'");'
elif [ "$verb" == "get" ] || [ "$verb" == "update" ] || [ "$verb" == "delete" ]; then
    echo '    cmd.arg("test-123");'
fi)
$(if [ "$verb" == "delete" ]; then
    echo '    cmd.arg("--dry-run");'
fi)

    cmd.assert().success();
}
TESTEOF
    done
done)
EOF

success "Created tests/integration_test.rs"

header "PHASE 4: CREATE README"

info "Creating README.md..."
cat > README.md << 'EOFREADME'
# PROJECT_NAME

A noun-verb CLI application generated with ggen.

## Installation

```bash
cargo install --path .
```

## Usage

### General Help

```bash
PROJECT_NAME --help
```

NOUN_SECTIONS

## Development

### Build

```bash
cargo build
```

### Test

```bash
# Run all tests
cargo test

# Run unit tests
cargo test --lib

# Run integration tests
cargo test --test integration_test
```

### Run

```bash
cargo run -- --help
```

## Architecture

This CLI follows the **noun-verb pattern**:

```
PROJECT_NAME <noun> <verb> [arguments]
```

- **Noun**: The resource type (NOUN_LIST)
- **Verb**: The action to perform (create, list, get, update, delete)

### Project Structure

```
PROJECT_NAME/
├── Cargo.toml
├── src/
│   ├── main.rs          # Entry point
│   └── cmds/
│       ├── mod.rs       # Command router
│       └── <noun>/      # Noun-specific commands
│           ├── mod.rs   # Verb router for noun
│           ├── create.rs
│           ├── list.rs
│           ├── get.rs
│           ├── update.rs
│           └── delete.rs
└── tests/
    └── integration_test.rs
```

## Generated with ggen

This CLI was generated using [ggen](https://github.com/seanchatmangpt/ggen) templates.

## License

MIT OR Apache-2.0
EOFREADME

# Build noun sections for README
NOUN_SECTIONS=""
NOUN_LIST="${NOUN_ARRAY[0]}"

for noun in "${NOUN_ARRAY[@]}"; do
    NOUN_CAP="$(echo $noun | sed 's/.*/\u&/')"

    NOUN_SECTIONS="${NOUN_SECTIONS}### ${NOUN_CAP} Commands\n\n"
    NOUN_SECTIONS="${NOUN_SECTIONS}#### Create a ${noun}\n\n"
    NOUN_SECTIONS="${NOUN_SECTIONS}\`\`\`bash\n"
    NOUN_SECTIONS="${NOUN_SECTIONS}PROJECT_NAME ${noun} create --name \"my-${noun}\" --description \"Example ${noun}\"\n"
    NOUN_SECTIONS="${NOUN_SECTIONS}\`\`\`\n\n"

    NOUN_SECTIONS="${NOUN_SECTIONS}#### List ${noun}s\n\n"
    NOUN_SECTIONS="${NOUN_SECTIONS}\`\`\`bash\n"
    NOUN_SECTIONS="${NOUN_SECTIONS}PROJECT_NAME ${noun} list\n"
    NOUN_SECTIONS="${NOUN_SECTIONS}PROJECT_NAME ${noun} list --filter \"my\"\n"
    NOUN_SECTIONS="${NOUN_SECTIONS}PROJECT_NAME ${noun} list --format json\n"
    NOUN_SECTIONS="${NOUN_SECTIONS}\`\`\`\n\n"

    NOUN_SECTIONS="${NOUN_SECTIONS}#### Get a ${noun}\n\n"
    NOUN_SECTIONS="${NOUN_SECTIONS}\`\`\`bash\n"
    NOUN_SECTIONS="${NOUN_SECTIONS}PROJECT_NAME ${noun} get <id>\n"
    NOUN_SECTIONS="${NOUN_SECTIONS}PROJECT_NAME ${noun} get <id> --format json\n"
    NOUN_SECTIONS="${NOUN_SECTIONS}\`\`\`\n\n"

    NOUN_SECTIONS="${NOUN_SECTIONS}#### Update a ${noun}\n\n"
    NOUN_SECTIONS="${NOUN_SECTIONS}\`\`\`bash\n"
    NOUN_SECTIONS="${NOUN_SECTIONS}PROJECT_NAME ${noun} update <id> --name \"new-name\"\n"
    NOUN_SECTIONS="${NOUN_SECTIONS}PROJECT_NAME ${noun} update <id> --dry-run\n"
    NOUN_SECTIONS="${NOUN_SECTIONS}\`\`\`\n\n"

    NOUN_SECTIONS="${NOUN_SECTIONS}#### Delete a ${noun}\n\n"
    NOUN_SECTIONS="${NOUN_SECTIONS}\`\`\`bash\n"
    NOUN_SECTIONS="${NOUN_SECTIONS}PROJECT_NAME ${noun} delete <id> --force\n"
    NOUN_SECTIONS="${NOUN_SECTIONS}PROJECT_NAME ${noun} delete <id> --dry-run\n"
    NOUN_SECTIONS="${NOUN_SECTIONS}\`\`\`\n\n"
done

# Build comma-separated noun list
for i in "${!NOUN_ARRAY[@]}"; do
    if [ $i -gt 0 ]; then
        NOUN_LIST="${NOUN_LIST}, ${NOUN_ARRAY[$i]}"
    fi
done

echo -e "$NOUN_SECTIONS" > /tmp/noun_sections
perl -i -pe 's/NOUN_SECTIONS/`cat \/tmp\/noun_sections`/e' README.md
sed -i.bak "s/PROJECT_NAME/$PROJECT_NAME/g" README.md && rm README.md.bak
sed -i.bak "s/NOUN_LIST/$NOUN_LIST/g" README.md && rm README.md.bak

success "Created README.md"

header "PHASE 5: BUILD AND TEST"

info "Building project..."
if cargo build 2>&1 | tee /tmp/build.log; then
    success "Build successful!"
else
    error "Build failed. Check /tmp/build.log for details"
fi

info "Running tests..."
if cargo test 2>&1 | tee /tmp/test.log; then
    success "All tests passed!"
else
    warn "Some tests failed. Check /tmp/test.log for details"
fi

header "PHASE 6: VALIDATION COMPLETE"

success "Generated CLI project: $OUTPUT_DIR"
success "Project structure:"

tree -L 3 "$OUTPUT_DIR" || find "$OUTPUT_DIR" -type f | head -20

echo ""
info "Next steps:"
echo "  1. cd $OUTPUT_DIR"
echo "  2. cargo run -- --help"
echo "  3. Implement TODO items in src/cmds/*//*.rs"
echo "  4. cargo test"
echo "  5. cargo build --release"
echo ""
success "Done! 🎉"
