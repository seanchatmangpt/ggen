# clap-noun-verb RDF Template System

Complete RDF-based template system for generating clap-noun-verb v3.2.0 CLI projects from TTL files.

## Overview

This system enables generating entire Rust CLI projects using the clap-noun-verb architecture by defining project structure in RDF/TTL files.

## Architecture

### 1. RDF Schema (`project-schema.ttl`)

Defines ontology for CLI projects:

- **cli:CliProject** - Root project definition
- **cnv:Noun** - Command nouns (template, project, etc.)
- **cnv:Verb** - Actions for nouns (generate, list, build, etc.)
- **cnv:Argument** - Command-line arguments (flags, positional)
- **cnv:ArgumentType** - Type constraints (String, PathBuf, bool)
- **cnv:Validation** - Validation rules
- **cnv:Dependency** - Cargo dependencies

### 2. Project Definition (`sample-cli.ttl`)

Example CLI project with:
- 2 nouns: `template`, `project`
- 5 verbs total:
  - template: generate, lint, show
  - project: init, build
- Complete argument specifications
- Validation rules
- Execution logic

### 3. Template Files

**File Tree Specification** (`cli-template.yaml`):
- Defines complete project structure
- Variable substitution patterns
- Rendering order
- Post-generation hooks

**Tera Templates** (`templates/*.tmpl`):
- `Cargo.toml.tmpl` - Cargo manifest
- `main.rs.tmpl` - Entry point with clap parser
- `command.rs.tmpl` - Command trait
- `lib.rs.tmpl` - Library exports
- `cmds/mod.rs.tmpl` - Command modules
- `cmds/noun_mod.rs.tmpl` - Noun module structure
- `cmds/verb.rs.tmpl` - Verb implementation
- `README.md.tmpl` - Documentation
- `tests/*.tmpl` - Integration and unit tests

## Usage

### 1. Define Your CLI in TTL

```turtle
@prefix cli: <http://ggen.dev/schema/cli#> .
@prefix cnv: <http://ggen.dev/schema/clap-noun-verb#> .

ex:MyProject a cli:CliProject ;
    cli:hasName "my-tool" ;
    cli:hasVersion "0.1.0" ;
    cli:hasNoun ex:MyNoun .

ex:MyNoun a cnv:Noun ;
    cnv:nounName "resource" ;
    cnv:hasVerb ex:CreateVerb .

ex:CreateVerb a cnv:Verb ;
    cnv:verbName "create" ;
    cnv:hasArgument ex:NameArg .
```

### 2. Generate Project with SPARQL

Query TTL to extract structure:

```sparql
PREFIX cli: <http://ggen.dev/schema/cli#>
PREFIX cnv: <http://ggen.dev/schema/clap-noun-verb#>

SELECT ?project ?name ?version
       ?noun ?nounName
       ?verb ?verbName
WHERE {
  ?project a cli:CliProject ;
           cli:hasName ?name ;
           cli:hasVersion ?version ;
           cli:hasNoun ?noun .

  ?noun cnv:nounName ?nounName ;
        cnv:hasVerb ?verb .

  ?verb cnv:verbName ?verbName .
}
```

### 3. Render Templates

Use extracted data to render Tera templates:

```rust
use tera::{Tera, Context};

let tera = Tera::new("templates/**/*.tmpl")?;
let mut context = Context::new();

context.insert("project", &project_data);
context.insert("nouns", &nouns_data);

let output = tera.render("main.rs.tmpl", &context)?;
```

### 4. Generated Project Structure

```
my-tool/
├── Cargo.toml
├── README.md
├── src/
│   ├── main.rs
│   ├── lib.rs
│   ├── command.rs
│   └── cmds/
│       ├── mod.rs
│       ├── template/
│       │   ├── mod.rs
│       │   ├── generate.rs
│       │   ├── lint.rs
│       │   └── show.rs
│       └── project/
│           ├── mod.rs
│           ├── init.rs
│           └── build.rs
└── tests/
    ├── integration_test.rs
    ├── template_tests.rs
    └── project_tests.rs
```

## SPARQL Queries for Code Generation

### Extract All Nouns and Verbs

```sparql
PREFIX cli: <http://ggen.dev/schema/cli#>
PREFIX cnv: <http://ggen.dev/schema/clap-noun-verb#>

SELECT ?nounName ?verbName ?verbDesc ?alias
WHERE {
  ?project cli:hasNoun ?noun .
  ?noun cnv:nounName ?nounName ;
        cnv:hasVerb ?verb .
  ?verb cnv:verbName ?verbName ;
        cnv:verbDescription ?verbDesc .
  OPTIONAL { ?verb cnv:verbAlias ?alias }
}
ORDER BY ?nounName ?verbName
```

### Extract Arguments for a Verb

```sparql
PREFIX cnv: <http://ggen.dev/schema/clap-noun-verb#>

SELECT ?argName ?long ?short ?help ?required ?default ?typeName
WHERE {
  ex:TemplateGenerate cnv:hasArgument ?arg .
  ?arg cnv:argName ?argName ;
       cnv:argHelp ?help .
  OPTIONAL { ?arg cnv:argLong ?long }
  OPTIONAL { ?arg cnv:argShort ?short }
  OPTIONAL { ?arg cnv:argRequired ?required }
  OPTIONAL { ?arg cnv:argDefault ?default }
  OPTIONAL {
    ?arg cnv:hasType ?type .
    ?type cnv:typeName ?typeName
  }
}
```

### Extract Dependencies

```sparql
PREFIX cli: <http://ggen.dev/schema/cli#>
PREFIX cnv: <http://ggen.dev/schema/clap-noun-verb#>

SELECT ?depName ?version ?features
WHERE {
  ex:MyCliProject cli:hasDependency ?dep .
  ?dep cnv:depName ?depName ;
       cnv:depVersion ?version .
  OPTIONAL { ?dep cnv:depFeatures ?features }
}
```

## Template Variable Reference

### Project Context

```
{{ project.name }}          - Package name
{{ project.version }}       - Semantic version
{{ project.description }}   - Description
{{ project.authors }}       - List of authors
{{ project.edition }}       - Rust edition
{{ project.license }}       - License
{{ project.dependencies }}  - Dependency list
{{ project.nouns }}         - Noun list
```

### Noun Context

```
{{ noun.name }}            - Noun identifier
{{ noun.description }}     - Description
{{ noun.module_path }}     - Module path
{{ noun.verbs }}          - Verb list
```

### Verb Context

```
{{ verb.name }}           - Verb identifier
{{ verb.description }}    - Description
{{ verb.alias }}          - Short alias
{{ verb.arguments }}      - Argument list
{{ verb.validations }}    - Validation rules
{{ verb.execution_logic }} - Rust code template
```

### Argument Context

```
{{ arg.name }}            - Argument name
{{ arg.long }}            - Long flag (--name)
{{ arg.short }}           - Short flag (-n)
{{ arg.help }}            - Help text
{{ arg.required }}        - Required flag
{{ arg.default }}         - Default value
{{ arg.type }}            - Rust type
{{ arg.position }}        - Position (for positional args)
```

## Validation Rules

Defined in TTL and enforced during generation:

1. **file_exists** - Path must exist
2. **path_valid** - Valid path format
3. **regex** - Match pattern
4. **range** - Numeric range
5. **enum** - One of allowed values

Example:
```turtle
ex:PathValidation a cnv:Validation ;
    cnv:validationRule "file_exists" ;
    cnv:validationMessage "File not found" .
```

Generated code:
```rust
fn validate(&self) -> Result<()> {
    if !self.args.path.exists() {
        anyhow::bail!("File not found: {:?}", self.args.path);
    }
    Ok(())
}
```

## Integration with ggen

This template system can be integrated into ggen v2 as:

```rust
// In ggen-ai crate
pub struct RdfTemplateGenerator {
    schema: RdfGraph,
    templates: Tera,
}

impl RdfTemplateGenerator {
    pub fn generate_from_ttl(&self, ttl_path: &Path) -> Result<GeneratedProject> {
        // 1. Parse TTL file
        let project = self.parse_ttl(ttl_path)?;

        // 2. Execute SPARQL queries
        let nouns = self.query_nouns(&project)?;
        let verbs = self.query_verbs(&project)?;
        let args = self.query_arguments(&project)?;

        // 3. Build template context
        let context = self.build_context(project, nouns, verbs, args)?;

        // 4. Render all templates
        let files = self.render_templates(&context)?;

        // 5. Write to disk
        self.write_project(files)?;

        Ok(GeneratedProject { path: output_dir })
    }
}
```

## Extension Points

### Custom Argument Types

Add new types in schema:
```turtle
ex:EmailType a cnv:ArgumentType ;
    cnv:typeName "String" ;
    cnv:typeParser "parse_email" .
```

### Custom Validations

```turtle
ex:EmailValidation a cnv:Validation ;
    cnv:validationRule "regex" ;
    cnv:validationPattern "^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}$" ;
    cnv:validationMessage "Invalid email format" .
```

### Template Hooks

Add in `cli-template.yaml`:
```yaml
post_generation:
  - command: "cargo fmt"
  - command: "cargo clippy -- -D warnings"
  - command: "cargo test"
  - command: "git init"
```

## Testing

Generated projects include comprehensive tests:

1. **Integration tests** - CLI argument parsing
2. **Unit tests** - Command logic
3. **Validation tests** - Argument validation
4. **Help text tests** - Documentation

Run with:
```bash
cargo test
```

## Benefits

1. **Declarative** - Define CLI in RDF, not code
2. **Type-safe** - Full Rust type system
3. **Validated** - Schema validation at generation time
4. **Testable** - Generated tests for all commands
5. **Maintainable** - Separate structure from implementation
6. **Extensible** - Add nouns/verbs without code changes
7. **Versioned** - Track CLI evolution in RDF

## Next Steps

1. Implement SPARQL query engine in ggen-ai
2. Add RDF parsing with oxigraph
3. Integrate Tera template engine
4. Create `ggen template generate --from-ttl` command
5. Add validation engine for generated projects
6. Build interactive CLI builder (TUI)

## References

- clap-noun-verb: https://github.com/sac/clap-noun-verb
- RDF Schema: https://www.w3.org/TR/rdf-schema/
- SPARQL: https://www.w3.org/TR/sparql11-query/
- Tera Templates: https://tera.netlify.app/
