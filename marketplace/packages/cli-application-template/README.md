# CLI Application Template

Generate production-ready command-line interface (CLI) applications with full support for commands, arguments, options, configuration, and shell completions.

## Features

- **Multi-Language Support**: Generate CLIs in Rust, TypeScript, or Python
- **Rich Command Structure**: Root commands, subcommands, arguments, and options
- **Type-Safe Arguments**: Strong typing with validation (string, int, bool, enum, path)
- **Configuration Files**: TOML, YAML, and JSON config file support
- **Environment Variables**: Automatic binding to CLI options
- **Help Generation**: Beautiful, auto-generated help text
- **Shell Completion**: Bash, Zsh, Fish, and PowerShell completion scripts
- **Interactive Prompts**: Built-in support for user input
- **Validation**: Regex patterns, range checks, and custom validators

## Installation

```bash
ggen marketplace install cli-application-template
```

## Quick Start

### 1. Create CLI Specification (RDF)

```turtle
@prefix cli: <http://ggen.dev/ontology/cli#> .

:MyApp a cli:CLIApplication ;
    cli:commandName "myapp" ;
    cli:hasCommand :ProcessCommand .

:ProcessCommand a cli:Command ;
    cli:commandName "process" ;
    cli:commandAbout "Process input files" ;
    cli:hasArgument :InputArg ;
    cli:hasOption :OutputOpt .

:InputArg a cli:Argument ;
    cli:argumentName "input" ;
    cli:argumentType cli:PathType ;
    cli:argumentRequired true ;
    cli:argumentHelp "Input file path" .

:OutputOpt a cli:Option ;
    cli:optionLong "output" ;
    cli:optionShort "o" ;
    cli:optionType cli:PathType ;
    cli:optionHelp "Output file path" .
```

### 2. Generate CLI Application

```bash
# Rust with clap
ggen generate cli-application-template \
  --input spec.ttl \
  --output myapp-rs \
  --language rust

# TypeScript with Commander.js
ggen generate cli-application-template \
  --input spec.ttl \
  --output myapp-ts \
  --language typescript

# Python with Click
ggen generate cli-application-template \
  --input spec.ttl \
  --output myapp-py \
  --language python
```

### 3. Build and Run

**Rust:**
```bash
cd myapp-rs
cargo build --release
./target/release/myapp process input.txt --output output.txt
```

**TypeScript:**
```bash
cd myapp-ts
npm install
npm run build
node dist/index.js process input.txt --output output.txt
```

**Python:**
```bash
cd myapp-py
pip install -r requirements.txt
python cli.py process input.txt --output output.txt
```

## CLI Patterns

### Commands and Subcommands

```turtle
:GitApp a cli:CLIApplication ;
    cli:hasCommand :CommitCmd, :PushCmd, :ConfigCmd .

:ConfigCmd a cli:Command ;
    cli:commandName "config" ;
    cli:hasSubcommand :GetSubcmd, :SetSubcmd .

:GetSubcmd a cli:Subcommand ;
    cli:commandName "get" ;
    cli:hasArgument :KeyArg .
```

Generates:
```bash
myapp config get user.name
myapp config set user.name "John Doe"
```

### Arguments and Options

```turtle
:DeployCmd a cli:Command ;
    cli:hasArgument [
        cli:argumentName "environment" ;
        cli:argumentType cli:EnumType ;
        cli:argumentRequired true ;
        cli:optionEnumValues "dev,staging,prod"
    ] ;
    cli:hasOption [
        cli:optionLong "verbose" ;
        cli:optionShort "v" ;
        cli:optionType cli:BooleanType
    ] .
```

Generates:
```bash
myapp deploy prod --verbose
```

### Configuration Files

```turtle
:App cli:hasConfig [
    a cli:ConfigFile ;
    cli:configFormat "toml" ;
    cli:configPath "~/.myapp/config.toml"
] .
```

Generates support for:
```toml
[settings]
verbose = true
output_dir = "/tmp/output"
max_workers = 4
```

### Environment Variables

```turtle
:App cli:hasConfig [
    a cli:EnvVar ;
    cli:envVarPrefix "MYAPP_"
] .
```

Enables:
```bash
export MYAPP_VERBOSE=true
export MYAPP_OUTPUT_DIR=/tmp
myapp run
```

### Validation

```turtle
:EmailArg cli:hasValidator [
    cli:validatorType "regex" ;
    cli:validatorPattern "^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}$" ;
    cli:validatorErrorMessage "Invalid email format"
] .

:PortOpt cli:hasValidator [
    cli:validatorType "range" ;
    cli:validatorMin 1 ;
    cli:validatorMax 65535 ;
    cli:validatorErrorMessage "Port must be between 1 and 65535"
] .
```

### Interactive Prompts

```turtle
:InitCmd cli:hasPrompt [
    cli:promptType "input" ;
    cli:promptMessage "Project name:" ;
    cli:promptName "project_name"
] , [
    cli:promptType "select" ;
    cli:promptMessage "Choose template:" ;
    cli:promptChoices "rust,typescript,python"
] .
```

## Generated Structure

### Rust (clap)

```
myapp-rs/
├── Cargo.toml
├── src/
│   └── main.rs         # CLI implementation with clap derive
└── tests/
    └── integration.rs  # Chicago TDD tests
```

**Features:**
- Derive macros for clean syntax
- Type-safe arguments
- Built-in help generation
- Shell completion generation
- TOML/YAML/JSON config support

### TypeScript (Commander.js)

```
myapp-ts/
├── package.json
├── src/
│   └── index.ts        # CLI with Commander.js
├── tests/
│   └── cli.test.ts     # Chicago TDD tests
└── tsconfig.json
```

**Features:**
- Fluent API
- Inquirer for prompts
- Chalk for colored output
- YAML/JSON config support

### Python (Click)

```
myapp-py/
├── setup.py
├── cli.py              # CLI with Click decorators
├── tests/
│   └── test_cli.py     # Chicago TDD tests
└── requirements.txt
```

**Features:**
- Decorator-based syntax
- Rich library for beautiful output
- Path validation
- YAML/JSON/TOML config support

## Advanced Examples

### Multi-Command CLI (Git-like)

```turtle
:GitCLI a cli:CLIApplication ;
    cli:commandName "git" ;
    cli:hasCommand :Add, :Commit, :Push, :Pull, :Checkout .

:Commit a cli:Command ;
    cli:commandName "commit" ;
    cli:hasOption [
        cli:optionShort "m" ;
        cli:optionLong "message" ;
        cli:optionRequired true
    ] , [
        cli:optionShort "a" ;
        cli:optionLong "all" ;
        cli:optionType cli:BooleanType
    ] .
```

### Docker-like CLI

```turtle
:DockerCLI a cli:CLIApplication ;
    cli:hasCommand :Run, :Build, :Ps, :Stop .

:Run a cli:Command ;
    cli:hasArgument [
        cli:argumentName "image" ;
        cli:argumentRequired true
    ] ;
    cli:hasOption [
        cli:optionShort "d" ;
        cli:optionLong "detach" ;
        cli:optionType cli:BooleanType
    ] , [
        cli:optionShort "p" ;
        cli:optionLong "port" ;
        cli:optionMultiple true ;
        cli:optionType cli:StringType
    ] .
```

### Package Manager CLI (npm-like)

```turtle
:NPM a cli:CLIApplication ;
    cli:hasCommand :Install, :Run, :Test, :Publish .

:Install a cli:Command ;
    cli:hasArgument [
        cli:argumentName "package" ;
        cli:argumentRequired false ;
        cli:argumentMultiple true
    ] ;
    cli:hasOption [
        cli:optionLong "save-dev" ;
        cli:optionShort "D"
    ] .
```

## Testing

All generated CLIs include comprehensive Chicago TDD test suites:

```bash
# Rust
cargo test

# TypeScript
npm test

# Python
pytest
```

Tests cover:
- Command execution
- Argument parsing
- Option handling
- Config file loading
- Environment variables
- Validation
- Error handling
- Interactive prompts
- Shell completions

## Shell Completions

Generate completion scripts:

**Rust:**
```bash
myapp completion bash > /etc/bash_completion.d/myapp
myapp completion zsh > /usr/local/share/zsh/site-functions/_myapp
myapp completion fish > ~/.config/fish/completions/myapp.fish
```

**TypeScript/Python:**
Similar completion generation available.

## Best Practices

1. **Command Naming**: Use kebab-case for commands (`my-command`)
2. **Option Naming**: Use long form (`--option`) with short aliases (`-o`)
3. **Help Text**: Provide clear, concise descriptions
4. **Validation**: Validate early, fail fast with helpful error messages
5. **Config Files**: Support user config, project config, and CLI flags (in priority order)
6. **Exit Codes**: Use 0 for success, non-zero for errors
7. **Logging**: Support `-v` for verbose, `-q` for quiet
8. **Interactive**: Use prompts for missing required info, not just errors

## See Also

- [COMMANDS.md](./COMMANDS.md) - Complete command reference
- [CONFIG.md](./CONFIG.md) - Configuration patterns
- [EXAMPLES.md](./EXAMPLES.md) - Real-world CLI examples
- [API Reference](./API.md) - RDF ontology reference

## License

MIT
