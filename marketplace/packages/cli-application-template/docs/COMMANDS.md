# Command Reference

Complete reference for the CLI Application Template ontology and SPARQL queries.

## RDF Classes

### Core Classes

#### `cli:CLIApplication`
Root class for a CLI application.

**Properties:**
- `cli:commandName` (string): Application binary name
- `cli:hasCommand` (Command): Commands available in the application
- `cli:hasConfig` (Configuration): Configuration support
- `gen:targetLanguage` (string): Target language (rust, typescript, python)

**Example:**
```turtle
:MyApp a cli:CLIApplication ;
    cli:commandName "myapp" ;
    gen:targetLanguage "rust" .
```

#### `cli:Command`
A CLI command that performs an action.

**Properties:**
- `cli:commandName` (string): Command name
- `cli:commandAlias` (string): Command alias (optional)
- `cli:commandAbout` (string): Short description
- `cli:commandLongAbout` (string): Detailed description
- `cli:hasArgument` (Argument): Positional arguments
- `cli:hasOption` (Option): Named options/flags
- `cli:hasSubcommand` (Subcommand): Nested subcommands
- `cli:hasHelp` (HelpText): Help text customization
- `cli:hasPrompt` (InteractivePrompt): Interactive prompts

**Example:**
```turtle
:ProcessCmd a cli:Command ;
    cli:commandName "process" ;
    cli:commandAlias "proc" ;
    cli:commandAbout "Process input files" ;
    cli:commandLongAbout "Process input files with various transformations" .
```

#### `cli:Subcommand`
A nested command under a parent command.

**Properties:** Same as Command

**Example:**
```turtle
:ConfigCmd cli:hasSubcommand :GetSubcmd, :SetSubcmd .

:GetSubcmd a cli:Subcommand ;
    cli:commandName "get" ;
    cli:commandAbout "Get configuration value" .
```

### Argument and Option Classes

#### `cli:Argument`
A positional argument to a command.

**Properties:**
- `cli:argumentName` (string): Argument name
- `cli:argumentType` (ArgumentType): Data type
- `cli:argumentRequired` (boolean): Whether required
- `cli:argumentDefault` (string): Default value
- `cli:argumentHelp` (string): Help text
- `cli:argumentPosition` (integer): Position (0-indexed)
- `cli:hasValidator` (Validator): Validation rules

**Example:**
```turtle
:InputArg a cli:Argument ;
    cli:argumentName "input" ;
    cli:argumentType cli:PathType ;
    cli:argumentRequired true ;
    cli:argumentPosition 0 ;
    cli:argumentHelp "Input file path" .
```

#### `cli:Option`
A named option/flag for a command.

**Properties:**
- `cli:optionShort` (string): Short flag (e.g., "v" for -v)
- `cli:optionLong` (string): Long flag (e.g., "verbose" for --verbose)
- `cli:optionType` (ArgumentType): Data type
- `cli:optionRequired` (boolean): Whether required
- `cli:optionDefault` (string): Default value
- `cli:optionHelp` (string): Help text
- `cli:optionMultiple` (boolean): Accept multiple values
- `cli:optionEnumValues` (string): Allowed values (comma-separated)

**Example:**
```turtle
:VerboseOpt a cli:Option ;
    cli:optionShort "v" ;
    cli:optionLong "verbose" ;
    cli:optionType cli:BooleanType ;
    cli:optionHelp "Enable verbose output" .

:FormatOpt a cli:Option ;
    cli:optionLong "format" ;
    cli:optionType cli:EnumType ;
    cli:optionEnumValues "json,yaml,toml" ;
    cli:optionDefault "json" .
```

#### `cli:Flag`
A boolean flag (subclass of Option).

**Example:**
```turtle
:DebugFlag a cli:Flag ;
    cli:optionShort "d" ;
    cli:optionLong "debug" .
```

### Data Types

All argument types with their language-specific mappings:

#### `cli:StringType`
- Rust: `String`
- TypeScript: `string`
- Python: `str`

#### `cli:IntegerType`
- Rust: `i64`
- TypeScript: `number`
- Python: `int`

#### `cli:BooleanType`
- Rust: `bool`
- TypeScript: `boolean`
- Python: `bool`

#### `cli:FloatType`
- Rust: `f64`
- TypeScript: `number`
- Python: `float`

#### `cli:EnumType`
- Rust: `enum`
- TypeScript: `union`
- Python: `Literal`

Use with `cli:optionEnumValues`:
```turtle
:ModeOpt cli:optionType cli:EnumType ;
         cli:optionEnumValues "dev,staging,prod" .
```

#### `cli:PathType`
- Rust: `PathBuf`
- TypeScript: `string`
- Python: `Path`

### Validation Classes

#### `cli:Validator`
Validation rule for arguments/options.

**Properties:**
- `cli:validatorType` (string): "regex", "range", or "custom"
- `cli:validatorPattern` (string): Regex pattern
- `cli:validatorMin` (integer): Minimum value
- `cli:validatorMax` (integer): Maximum value
- `cli:validatorErrorMessage` (string): Error message

**Examples:**

**Regex validation:**
```turtle
:EmailArg cli:hasValidator [
    cli:validatorType "regex" ;
    cli:validatorPattern "^[^@]+@[^@]+\\.[^@]+$" ;
    cli:validatorErrorMessage "Invalid email format"
] .
```

**Range validation:**
```turtle
:PortOpt cli:hasValidator [
    cli:validatorType "range" ;
    cli:validatorMin 1 ;
    cli:validatorMax 65535 ;
    cli:validatorErrorMessage "Port must be between 1 and 65535"
] .
```

### Configuration Classes

#### `cli:ConfigFile`
Configuration file support.

**Properties:**
- `cli:configFormat` (string): "toml", "yaml", or "json"
- `cli:configPath` (string): Default config file path

**Example:**
```turtle
:App cli:hasConfig [
    a cli:ConfigFile ;
    cli:configFormat "toml" ;
    cli:configPath "~/.myapp/config.toml"
] .
```

#### `cli:EnvVar`
Environment variable binding.

**Properties:**
- `cli:envVarPrefix` (string): Prefix for env vars (e.g., "MYAPP_")
- `cli:envVarName` (string): Specific env var name

**Example:**
```turtle
:App cli:hasConfig [
    a cli:EnvVar ;
    cli:envVarPrefix "MYAPP_"
] .
```

This enables:
```bash
export MYAPP_VERBOSE=true
export MYAPP_OUTPUT_DIR=/tmp
myapp run
```

### Help and Documentation

#### `cli:HelpText`
Custom help text formatting.

**Properties:**
- `cli:helpTemplate` (string): Custom help template
- `cli:helpExample` (string): Usage example
- `cli:helpAfter` (string): Text after help

**Example:**
```turtle
:ProcessCmd cli:hasHelp [
    cli:helpExample "myapp process input.txt --output result.txt" ;
    cli:helpAfter "For more information, visit https://example.com/docs"
] .
```

### Shell Completion

#### `cli:ShellCompletion`
Shell completion script support.

**Properties:**
- `cli:completionShell` (string): "bash", "zsh", "fish", or "powershell"

**Example:**
```turtle
:App cli:hasCompletion [
    cli:completionShell "bash"
] , [
    cli:completionShell "zsh"
] .
```

### Interactive Prompts

#### `cli:InteractivePrompt`
Interactive prompt for user input.

**Properties:**
- `cli:promptType` (string): "input", "select", "multiselect", "confirm", "password"
- `cli:promptMessage` (string): Prompt text
- `cli:promptChoices` (string): Choices (comma-separated)

**Example:**
```turtle
:InitCmd cli:hasPrompt [
    cli:promptType "input" ;
    cli:promptMessage "Project name:"
] , [
    cli:promptType "select" ;
    cli:promptMessage "Choose language:" ;
    cli:promptChoices "rust,typescript,python"
] , [
    cli:promptType "confirm" ;
    cli:promptMessage "Initialize git repository?"
] .
```

## SPARQL Queries

### Query 1: Get Command Structure

Retrieves all commands and subcommands with hierarchy.

```sparql
SELECT ?command ?name ?about ?subcommand ?subname
WHERE {
  ?app a cli:CLIApplication .
  ?app cli:hasCommand ?command .
  ?command cli:commandName ?name .
  OPTIONAL { ?command cli:commandAbout ?about }
  OPTIONAL {
    ?command cli:hasSubcommand ?subcommand .
    ?subcommand cli:commandName ?subname .
  }
}
```

### Query 2: Get Arguments and Options

Extracts all arguments and options for commands.

```sparql
SELECT ?command ?argName ?optLong ?optShort
WHERE {
  ?command a cli:Command .
  OPTIONAL {
    ?command cli:hasArgument ?arg .
    ?arg cli:argumentName ?argName .
  }
  OPTIONAL {
    ?command cli:hasOption ?opt .
    ?opt cli:optionLong ?optLong .
    OPTIONAL { ?opt cli:optionShort ?optShort }
  }
}
```

### Query 3: Get Validation Rules

Retrieves validation rules for arguments.

```sparql
SELECT ?argument ?type ?pattern ?min ?max
WHERE {
  ?argument cli:hasValidator ?validator .
  ?validator cli:validatorType ?type .
  OPTIONAL { ?validator cli:validatorPattern ?pattern }
  OPTIONAL { ?validator cli:validatorMin ?min }
  OPTIONAL { ?validator cli:validatorMax ?max }
}
```

### Query 4: Get Configuration

Extracts configuration settings.

```sparql
SELECT ?configFormat ?configPath ?envPrefix
WHERE {
  ?app a cli:CLIApplication ;
       cli:hasConfig ?config .
  OPTIONAL {
    ?config cli:configFormat ?configFormat ;
            cli:configPath ?configPath .
  }
  OPTIONAL {
    ?config cli:envVarPrefix ?envPrefix .
  }
}
```

### Query 5-12: Additional Queries

See `sparql/queries.rq` for:
- Help text generation
- Rust struct generation
- TypeScript parser generation
- Shell completion generation
- Config file schema generation
- Interactive prompt generation
- Validation code generation

## Code Generation

### Rust (Clap v4)

Generated code uses:
- `#[derive(Parser)]` for CLI struct
- `#[command()]` attributes for metadata
- `#[arg()]` for arguments and options
- Subcommands via enums
- Type-safe argument parsing

### TypeScript (Commander.js)

Generated code uses:
- `program.command()` for commands
- `.argument()` and `.option()` for parameters
- `.action()` for command handlers
- Inquirer for interactive prompts
- Chalk for colored output

### Python (Click)

Generated code uses:
- `@click.command()` decorators
- `@click.argument()` and `@click.option()`
- Click types for validation
- Rich library for beautiful output
- Path validation

## Best Practices

1. **Naming Conventions:**
   - Commands: kebab-case (`my-command`)
   - Options: long form with short alias (`--option`, `-o`)
   - Arguments: lowercase (`filename`)

2. **Help Text:**
   - Keep `commandAbout` concise (one line)
   - Use `commandLongAbout` for details
   - Provide examples with `helpExample`

3. **Validation:**
   - Validate at CLI level, not business logic
   - Use meaningful error messages
   - Prefer enum types over regex when possible

4. **Configuration:**
   - Support config files for persistent settings
   - Use env vars for secrets/credentials
   - Allow CLI flags to override config

5. **Subcommands:**
   - Group related commands
   - Keep hierarchy shallow (2-3 levels max)
   - Use consistent naming within groups
