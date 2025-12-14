# clap-noun-verb - ggen Marketplace Package

**Generate type-safe Rust CLIs from RDF descriptions**

Transform Turtle RDF descriptions into production-ready Rust code with the three-layer architecture.

---

## Quick Start

### 1. Describe Your CLI

```turtle
# my-cli.ttl
@prefix cnv: <https://ggen.dev/clap-noun-verb/> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .

<http://example.com/my-cli> a cnv:CliProject ;
    cnv:projectName "my-cli" ;
    cnv:projectVersion "0.1.0" .

<#calc> a cnv:Noun ;
    cnv:nounName "calc" ;
    cnv:hasVerbs <#add> .

<#add> a cnv:Verb ;
    cnv:verbName "add" ;
    cnv:hasArguments <#left>, <#right> .

<#left> a cnv:Argument ;
    cnv:argumentName "left" ;
    cnv:argumentType <#i32Type> .

<#right> a cnv:Argument ;
    cnv:argumentName "right" ;
    cnv:argumentType <#i32Type> .

<#i32Type> a cnv:PrimitiveType ;
    cnv:rust-type "i32" .
```

### 2. Generate

```bash
ggen generate \
  --template marketplace/packages/clap-noun-verb/templates/cli-project.tmpl \
  --domain my-cli.ttl \
  --output ./my-cli/
```

### 3. Implement & Run

```bash
cd my-cli
# Edit src/domain.rs to implement your logic
cargo run -- calc add 10 5
# {"result": 15}
```

---

## What Gets Generated

```
my-cli/
├── Cargo.toml       # Dependencies configured
└── src/
    ├── main.rs      # CLI layer with #[noun] and #[verb] macros
    ├── domain.rs    # Domain layer with unimplemented!() stubs
    ├── error.rs     # Type-safe error handling
    └── lib.rs       # Library exports
```

---

## Architecture

```
     RDF Description (.ttl)
            │
            ▼
   ┌────────────────────┐
   │  ggen + template   │
   └────────────────────┘
            │
            ▼
   ┌────────────────────┐
   │    CLI Layer       │  main.rs - Argument parsing, validation
   │    (thin)          │  Uses #[noun] and #[verb] macros
   └────────────────────┘
            │
            ▼
   ┌────────────────────┐
   │   Domain Layer     │  domain.rs - Pure business logic
   │   (thick)          │  No CLI awareness, fully testable
   └────────────────────┘
            │
            ▼
   ┌────────────────────┐
   │   Error Layer      │  error.rs - DomainError, CliError
   │   (typed)          │  Automatic conversion between layers
   └────────────────────┘
```

---

## RDF Vocabulary

| Class | Purpose | Key Properties |
|-------|---------|----------------|
| `cnv:CliProject` | Project metadata | `cnv:projectName`, `cnv:projectVersion` |
| `cnv:Noun` | Resource type (subcommand) | `cnv:nounName`, `cnv:hasVerbs` |
| `cnv:Verb` | Action on resource | `cnv:verbName`, `cnv:hasArguments` |
| `cnv:Argument` | Command argument | `cnv:argumentName`, `cnv:argumentType` |
| `cnv:PrimitiveType` | Rust type | `cnv:rust-type` |

**Prefix**: `cnv: <https://ggen.dev/clap-noun-verb/>`

---

## Features

- **Type-Safe**: All commands validated at compile-time
- **Three Layers**: CLI, Domain, Error - cleanly separated
- **Zero-Cost**: Generics + macros, no trait objects
- **JSON Output**: Agent-ready structured output
- **Testable**: Domain layer has no CLI dependencies
- **RDF-Driven**: Single source of truth for CLI structure

---

## Files in This Package

```
clap-noun-verb/
├── package.toml                    # Marketplace metadata
├── README.md                       # This file
├── USAGE.md                        # Detailed usage guide
├── templates/
│   └── cli-project.tmpl           # Master template (generates all files)
└── examples/
    └── calculator.ttl             # Example CLI description
```

---

## Example

See [examples/calculator.ttl](examples/calculator.ttl) for a complete example.

Generate it:
```bash
ggen generate \
  --template marketplace/packages/clap-noun-verb/templates/cli-project.tmpl \
  --domain marketplace/packages/clap-noun-verb/examples/calculator.ttl \
  --output /tmp/calculator/
```

---

## Documentation

- **[USAGE.md](USAGE.md)** - Detailed integration guide
- **[Ontology](../../ontologies/clap-noun-verb.ttl)** - Full RDF vocabulary
- **[clap-noun-verb crate](https://crates.io/crates/clap-noun-verb)** - Runtime library

---

## License

MIT OR Apache-2.0
