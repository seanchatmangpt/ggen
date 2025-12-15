# RDF Template System Architecture

## System Overview

```
┌─────────────────────────────────────────────────────────────────────┐
│                    RDF-to-CLI Generation Pipeline                    │
└─────────────────────────────────────────────────────────────────────┘

INPUT                    PROCESSING                       OUTPUT
┌──────────┐            ┌──────────┐                   ┌──────────┐
│          │            │          │                   │          │
│ TTL File │──Parse────>│   RDF    │──Query───────────>│  Rust    │
│ (.ttl)   │            │  Graph   │                   │  Project │
│          │            │          │                   │          │
└──────────┘            └──────────┘                   └──────────┘
                              │                              ▲
                              │                              │
                              │        ┌──────────┐          │
                              └───────>│  SPARQL  │──────────┤
                                       │  Queries │          │
                                       └──────────┘          │
                                             │               │
                                             │               │
                                       ┌──────────┐          │
                                       │   Tera   │──────────┤
                                       │Templates │          │
                                       └──────────┘          │
                                             │               │
                                             └───────────────┘
```

## Component Architecture

### 1. RDF Schema Layer

```
project-schema.ttl
├── Ontology Definition
│   └── cli:CliOntology
│
├── Core Classes
│   ├── cli:CliProject
│   ├── cnv:Noun
│   ├── cnv:Verb
│   ├── cnv:Argument
│   ├── cnv:ArgumentType
│   ├── cnv:Validation
│   └── cnv:Dependency
│
└── Properties
    ├── Project Properties (name, version, description, etc.)
    ├── Noun Properties (nounName, nounDescription, etc.)
    ├── Verb Properties (verbName, verbDescription, etc.)
    └── Argument Properties (argName, argLong, argShort, etc.)
```

### 2. Data Flow Architecture

```
┌─────────────────────────────────────────────────────────────────────┐
│ Phase 1: RDF Parsing                                                │
├─────────────────────────────────────────────────────────────────────┤
│                                                                     │
│  sample-cli.ttl  ──>  RdfParser  ──>  oxigraph::Store              │
│                                                                     │
│  - Load schema (project-schema.ttl)                                │
│  - Load project definition (sample-cli.ttl)                        │
│  - Build in-memory triple store                                    │
│                                                                     │
└─────────────────────────────────────────────────────────────────────┘

┌─────────────────────────────────────────────────────────────────────┐
│ Phase 2: SPARQL Extraction                                          │
├─────────────────────────────────────────────────────────────────────┤
│                                                                     │
│  QueryExecutor::extract_project()                                   │
│    ├─> Query: SELECT project metadata                              │
│    └─> Result: CliProject { name, version, ... }                   │
│                                                                     │
│  QueryExecutor::extract_nouns()                                     │
│    ├─> Query: SELECT all nouns                                     │
│    ├─> For each noun:                                              │
│    │   └─> extract_verbs_for_noun()                                │
│    │       ├─> Query: SELECT verbs for this noun                   │
│    │       └─> For each verb:                                      │
│    │           ├─> extract_arguments_for_verb()                    │
│    │           └─> extract_validations_for_verb()                  │
│    └─> Result: Vec<Noun>                                           │
│                                                                     │
│  QueryExecutor::extract_dependencies()                              │
│    ├─> Query: SELECT all dependencies                              │
│    └─> Result: Vec<Dependency>                                     │
│                                                                     │
└─────────────────────────────────────────────────────────────────────┘

┌─────────────────────────────────────────────────────────────────────┐
│ Phase 3: Template Rendering                                         │
├─────────────────────────────────────────────────────────────────────┤
│                                                                     │
│  TemplateRenderer::build_context()                                  │
│    └─> Tera Context { project, nouns, dependencies }               │
│                                                                     │
│  TemplateRenderer::render_all()                                     │
│    ├─> Cargo.toml.tmpl       ──>  output/Cargo.toml                │
│    ├─> main.rs.tmpl          ──>  output/src/main.rs               │
│    ├─> command.rs.tmpl       ──>  output/src/command.rs            │
│    ├─> lib.rs.tmpl           ──>  output/src/lib.rs                │
│    ├─> cmds/mod.rs.tmpl      ──>  output/src/cmds/mod.rs           │
│    │                                                                │
│    └─> For each noun:                                              │
│        ├─> noun_mod.rs.tmpl  ──>  output/src/cmds/{noun}/mod.rs    │
│        └─> For each verb:                                          │
│            └─> verb.rs.tmpl  ──>  output/src/cmds/{noun}/{verb}.rs │
│                                                                     │
└─────────────────────────────────────────────────────────────────────┘

┌─────────────────────────────────────────────────────────────────────┐
│ Phase 4: Post-Generation                                            │
├─────────────────────────────────────────────────────────────────────┤
│                                                                     │
│  cargo fmt    ──>  Format generated code                            │
│  cargo check  ──>  Verify compilation                               │
│  cargo test   ──>  Run tests                                        │
│                                                                     │
└─────────────────────────────────────────────────────────────────────┘
```

## RDF Graph Structure

### Project Graph Example

```turtle
ex:MyCliProject ─┬─ cli:hasName ──────────> "my-cli"
                 ├─ cli:hasVersion ─────────> "0.1.0"
                 ├─ cli:hasNoun ──┬────────> ex:TemplateNoun
                 │                └────────> ex:ProjectNoun
                 └─ cli:hasDependency ──┬─> ex:ClapDep
                                        ├─> ex:AnyhowDep
                                        └─> ex:SerdeJsonDep

ex:TemplateNoun ─┬─ cnv:nounName ────────> "template"
                 └─ cnv:hasVerb ──┬──────> ex:TemplateGenerate
                                  ├──────> ex:TemplateLint
                                  └──────> ex:TemplateShow

ex:TemplateGenerate ─┬─ cnv:verbName ────> "generate"
                     ├─ cnv:verbAlias ───> "gen"
                     └─ cnv:hasArgument ─┬> ex:TemplateNameArg
                                         ├> ex:OutputPathArg
                                         └> ex:ForceFlag

ex:TemplateNameArg ─┬─ cnv:argName ─────> "template_name"
                    ├─ cnv:argPosition ─> 0
                    ├─ cnv:argHelp ─────> "Name of the template..."
                    └─ cnv:hasType ─────> ex:StringType
```

## Template Rendering Flow

### Variable Substitution

```
TTL Data                  Template Context              Rendered Output
─────────                 ────────────────              ───────────────

cli:hasName               {{ project.name }}            name = "my-cli"
"my-cli"

cnv:verbName              {{ verb.name }}               pub struct Generate
"generate"

cnv:argLong               {% for arg in args %}         #[arg(long = "output")]
"output"                  --{{ arg.long }}              pub output_path: PathBuf
                          {% endfor %}
```

### Template Hierarchy

```
cli-template.yaml
├── Root Files
│   ├── Cargo.toml.tmpl ────────> Cargo.toml
│   ├── README.md.tmpl ─────────> README.md
│   └── gitignore.tmpl ─────────> .gitignore
│
├── Source Files
│   ├── main.rs.tmpl ───────────> src/main.rs
│   ├── lib.rs.tmpl ────────────> src/lib.rs
│   └── command.rs.tmpl ────────> src/command.rs
│
├── Command Modules
│   ├── cmds/mod.rs.tmpl ───────> src/cmds/mod.rs
│   ├── noun_mod.rs.tmpl ───┬──> src/cmds/template/mod.rs
│   │                       └──> src/cmds/project/mod.rs
│   └── verb.rs.tmpl ───────┬──> src/cmds/template/generate.rs
│                           ├──> src/cmds/template/lint.rs
│                           ├──> src/cmds/template/show.rs
│                           ├──> src/cmds/project/init.rs
│                           └──> src/cmds/project/build.rs
│
└── Tests
    ├── integration.rs.tmpl ────> tests/integration_test.rs
    └── noun_test.rs.tmpl ──┬──> tests/template_tests.rs
                            └──> tests/project_tests.rs
```

## Generated Project Structure

```
my-cli/
│
├── Cargo.toml
├── README.md
├── .gitignore
│
├── src/
│   ├── main.rs                 (CLI entry point)
│   ├── lib.rs                  (Library exports)
│   ├── command.rs              (Command trait)
│   │
│   └── cmds/
│       ├── mod.rs
│       │
│       ├── template/
│       │   ├── mod.rs
│       │   ├── generate.rs     (template generate command)
│       │   ├── lint.rs         (template lint command)
│       │   └── show.rs         (template show command)
│       │
│       └── project/
│           ├── mod.rs
│           ├── init.rs         (project init command)
│           └── build.rs        (project build command)
│
└── tests/
    ├── integration_test.rs
    ├── template_tests.rs
    └── project_tests.rs
```

## Rust Type Mapping

### TTL to Rust Types

```
RDF Schema              SPARQL Result           Rust Type
──────────              ─────────────           ─────────

cnv:ArgumentType        typeName: "String"      String
  typeName "String"

cnv:ArgumentType        typeName: "PathBuf"     std::path::PathBuf
  typeName "PathBuf"

cnv:ArgumentType        typeName: "bool"        bool
  typeName "bool"

cnv:ArgumentType        typeName: "u32"         u32
  typeName "u32"

cnv:ArgumentType        typeName: "Vec<String>" Vec<String>
  typeName "Vec<String>"
```

## Extension Points

### 1. Custom Argument Types

```turtle
# Define in TTL
ex:EmailType a cnv:ArgumentType ;
    cnv:typeName "String" ;
    cnv:typeParser "parse_email" .

ex:EmailArg a cnv:Argument ;
    cnv:argName "email" ;
    cnv:hasType ex:EmailType .
```

```rust
// Generated in Rust
#[arg(value_parser = parse_email)]
pub email: String,

fn parse_email(s: &str) -> Result<String, String> {
    // Email validation logic
}
```

### 2. Custom Validation Rules

```turtle
# Define in TTL
ex:EmailValidation a cnv:Validation ;
    cnv:validationRule "regex" ;
    cnv:validationPattern "^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}$" ;
    cnv:validationMessage "Invalid email format" .
```

```rust
// Generated in Rust
fn validate(&self) -> Result<()> {
    let regex = Regex::new(r"^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\.[a-zA-Z]{2,}$")?;
    if !regex.is_match(&self.args.email) {
        anyhow::bail!("Invalid email format");
    }
    Ok(())
}
```

### 3. Custom Execution Logic

```turtle
# Define in TTL
ex:CustomVerb a cnv:Verb ;
    cnv:verbName "custom" ;
    cnv:executionLogic """
        println!(\"Custom logic here\");

        // Your implementation
        let result = do_something()?;

        Ok(())
    """ .
```

## Query Optimization

### Indexed Queries

```sparql
# BAD: Cartesian product
SELECT ?noun ?verb
WHERE {
    ?noun a cnv:Noun .
    ?verb a cnv:Verb .
}

# GOOD: Use relationships
SELECT ?noun ?verb
WHERE {
    ?noun cnv:hasVerb ?verb .
}
```

### Batched Queries

```rust
// BAD: Multiple queries
for noun in nouns {
    let verbs = query_verbs_for_noun(noun)?;
}

// GOOD: Single query with hierarchy
let all_data = query_complete_structure()?;
```

## Performance Characteristics

```
Operation                Time Complexity    Notes
─────────                ───────────────    ─────

RDF Parsing              O(n)               n = triples
SPARQL Query             O(m log m)         m = matching triples
Template Rendering       O(k)               k = template size
File Generation          O(f)               f = number of files

Total Generation         O(n + m log m + k + f)
```

## Testing Strategy

```
┌─────────────────────────────────────────────────────────────────────┐
│ Layer 1: Schema Validation                                          │
│   - Validate TTL against RDF schema                                 │
│   - Check ontology consistency                                      │
└─────────────────────────────────────────────────────────────────────┘

┌─────────────────────────────────────────────────────────────────────┐
│ Layer 2: SPARQL Query Testing                                       │
│   - Test each query independently                                   │
│   - Verify result shapes                                            │
└─────────────────────────────────────────────────────────────────────┘

┌─────────────────────────────────────────────────────────────────────┐
│ Layer 3: Template Rendering                                         │
│   - Test template syntax                                            │
│   - Verify variable substitution                                    │
└─────────────────────────────────────────────────────────────────────┘

┌─────────────────────────────────────────────────────────────────────┐
│ Layer 4: Generated Code Validation                                  │
│   - Compile generated project                                       │
│   - Run generated tests                                             │
│   - Verify CLI functionality                                        │
└─────────────────────────────────────────────────────────────────────┘
```

## Integration with ggen v2

```
ggen CLI
├── template generate              (Existing)
├── template lint                  (Existing)
└── template generate-rdf          (NEW)
    │
    ├─> RdfParser
    │   └─> oxigraph::Store
    │
    ├─> QueryExecutor
    │   ├─> extract_project()
    │   ├─> extract_nouns()
    │   └─> extract_dependencies()
    │
    ├─> TemplateRenderer
    │   └─> Tera templates
    │
    └─> Generated Project
        ├─> Cargo.toml
        ├─> src/**/*.rs
        └─> tests/**/*.rs
```

## Benefits of RDF Approach

1. **Declarative**: Define structure, not implementation
2. **Queryable**: SPARQL enables complex queries
3. **Extensible**: Add properties without breaking schema
4. **Validated**: RDF schema enforces constraints
5. **Interoperable**: Standard format for tooling
6. **Versioned**: Track evolution in semantic web
7. **Composable**: Import and extend schemas

## Future Enhancements

1. **Visual Editor**: TUI/GUI for editing TTL files
2. **Live Preview**: Watch mode for TTL changes
3. **Multi-file Projects**: Import multiple TTL files
4. **OpenAPI Generation**: Generate API specs from TTL
5. **Graph Visualization**: Visual command structure
6. **Custom Backends**: Support other frameworks (clap, structopt, etc.)
7. **Code Generation**: Support other languages (Python, Go, etc.)
