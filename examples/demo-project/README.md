# ggen v2.0 E2E Demo: Template + TTL Project Generation

This example demonstrates the complete workflow of generating a Rust project using ggen v2.0 with RDF/TTL definitions and Handlebars templates.

## Overview

This demo shows how to:
1. Define a project structure and metadata in RDF/TTL
2. Create Handlebars templates with SPARQL queries
3. Generate a complete, working Rust project
4. Validate the generated code compiles and runs

## File Structure

```
demo-project/
├── project.ttl              # RDF/TTL project definition
├── templates/
│   └── template.hbs         # Handlebars template with SPARQL
├── output/                  # Generated project (created by test.sh)
│   ├── Cargo.toml
│   ├── src/lib.rs
│   └── README.md
├── test.sh                  # E2E validation script
└── README.md                # This file
```

## RDF Schema Explanation

The `project.ttl` file uses standard vocabularies to define the project:

### Namespaces Used

- **DOAP** (Description of a Project): Project metadata
  - `doap:Project` - Defines this as a software project
  - `doap:name`, `doap:version`, `doap:description` - Basic metadata
  - `doap:license`, `doap:maintainer` - Licensing and authorship

- **FOAF** (Friend of a Friend): Person/author information
  - `foaf:Person` - Defines a person
  - `foaf:name`, `foaf:mbox` - Personal details

- **ggen** (custom): Rust-specific metadata
  - `ggen:RustProject` - Marks this as a Rust project
  - `ggen:edition` - Rust edition (2021)
  - `ggen:dependency` - Project dependencies
  - `ggen:module` - Code modules with functions
  - `ggen:testModule` - Test definitions

### Project Definition

```turtle
<project:calculator>
    a doap:Project, ggen:RustProject ;
    doap:name "calculator" ;
    doap:version "0.1.0" ;
    ggen:edition "2021" ;
```

This defines a Rust project named "calculator" version 0.1.0 using Rust 2021 edition.

### Dependencies

```turtle
ggen:dependency [
    ggen:name "serde" ;
    ggen:version "1.0" ;
    ggen:features "derive"
] ;
```

Dependencies are defined as blank nodes with name, version, and optional features.

### Modules and Functions

```turtle
ggen:module [
    ggen:name "operations" ;
    ggen:description "Basic arithmetic operations" ;
    ggen:functions (
        [ggen:name "add" ; ggen:signature "fn add(a: f64, b: f64) -> f64"]
        [ggen:name "divide" ; ggen:signature "fn divide(a: f64, b: f64) -> Result<f64, CalcError>"]
    )
]
```

Modules contain function definitions with signatures. Functions are organized as RDF lists.

## SPARQL Query Breakdown

The template uses SPARQL queries to extract data from the RDF:

### 1. Project Metadata Query

```sparql
SELECT ?name ?version ?description ?edition ?license
WHERE {
  ?project a ggen:RustProject ;
           doap:name ?name ;
           doap:version ?version ;
           doap:description ?description ;
           ggen:edition ?edition ;
           doap:license ?license .
}
```

**Purpose**: Extract basic project information for `Cargo.toml` and README.

**Results**: `name="calculator"`, `version="0.1.0"`, `edition="2021"`, etc.

### 2. Dependencies Query

```sparql
SELECT ?depName ?depVersion ?features
WHERE {
  ?project ggen:dependency ?dep .
  ?dep ggen:name ?depName ;
       ggen:version ?depVersion .
  OPTIONAL { ?dep ggen:features ?features }
}
```

**Purpose**: Extract all dependencies for `Cargo.toml`.

**Results**: List of dependencies with versions and optional features.

### 3. Module Query

```sparql
SELECT ?moduleName ?moduleDesc
WHERE {
  ?project ggen:module ?module .
  ?module ggen:name ?moduleName ;
          ggen:description ?moduleDesc .
}
```

**Purpose**: Extract modules to generate in `src/lib.rs`.

**Results**: Module names and descriptions for code generation.

### 4. Function Query (within module)

```sparql
SELECT ?funcName ?funcSig
WHERE {
  ?project ggen:module ?module .
  ?module ggen:name 'operations' ;
          ggen:functions ?funcList .
  ?funcList rdf:rest*/rdf:first ?func .
  ?func ggen:name ?funcName ;
        ggen:signature ?funcSig .
}
```

**Purpose**: Extract function signatures from module function lists.

**Uses**: `rdf:rest*/rdf:first` to traverse RDF lists (property path).

**Results**: Function names and signatures for implementation.

### 5. Test Cases Query

```sparql
SELECT ?testName ?assertion
WHERE {
  ?project ggen:testModule ?testMod .
  ?testMod ggen:testCase ?test .
  ?test ggen:name ?testName ;
        ggen:assertion ?assertion .
}
```

**Purpose**: Extract test definitions for `#[cfg(test)]` module.

**Results**: Test names and assertions.

## Template Structure

The Handlebars template uses several features:

### 1. File Generation Directive

```handlebars
{{file "Cargo.toml"}}
...content...
{{/file}}
```

This tells ggen to create a file with the enclosed content.

### 2. SPARQL Helper

```handlebars
{{#sparql "SELECT ..."}}
  Use {{variables}} from query results
{{/sparql}}
```

Executes SPARQL query and iterates over results.

### 3. Conditional Logic

```handlebars
{{#if (eq funcName "add")}}
    a + b
{{/if}}
```

Uses Handlebars helpers for conditional code generation.

### 4. Nested Queries

```handlebars
{{#sparql "SELECT modules..."}}
  {{#sparql "SELECT functions in module..."}}
    Generate function code
  {{/sparql}}
{{/sparql}}
```

SPARQL queries can be nested for hierarchical data extraction.

## Generated Project

The template generates three files:

### Cargo.toml
- Package metadata from RDF
- Dependencies with versions and features
- Dev dependencies section

### src/lib.rs
- Module documentation from RDF descriptions
- Error module with `thiserror` derives
- Operations module with functions
- Test module with assertions

### README.md
- Project description
- Feature list from modules
- Usage examples
- Dependency list
- License information

## Running the Example

### Prerequisites
- Rust 1.70+
- ggen v2.0 (or build from source)

### Execute Test

```bash
cd examples/demo-project
chmod +x test.sh
./test.sh
```

### Expected Output

```
==========================================
ggen v2.0 E2E Test: Template + TTL
==========================================

[1/6] Cleaning output directory...
✓ Output directory ready: .../output

[2/6] Validating input files...
✓ Template found: .../template.hbs
✓ RDF found: .../project.ttl

[3/6] Generating project from template + RDF...
✓ Project generated successfully

[4/6] Validating generated files...
✓ Found: Cargo.toml
✓ Found: src/lib.rs
✓ Found: README.md

[5/6] Validating file contents...
✓ Cargo.toml contains correct metadata and dependencies
✓ src/lib.rs contains correct modules and functions
✓ README.md contains correct documentation

[6/6] Compiling generated Rust project...
✓ Project compiled successfully

Running tests...
✓ All tests passed

==========================================
✓ E2E Test PASSED!
==========================================
```

## How It Works

1. **RDF Parsing**: ggen loads `project.ttl` into an in-memory RDF graph
2. **Template Processing**: Handlebars processes `template.hbs`
3. **SPARQL Execution**: Each `{{#sparql}}` block queries the RDF graph
4. **Code Generation**: Results populate template variables
5. **File Output**: `{{file}}` directives create output files
6. **Validation**: Test script validates files exist, content is correct, and code compiles

## Extending This Example

### Add New Dependencies

```turtle
ggen:dependency [
    ggen:name "tokio" ;
    ggen:version "1.0" ;
    ggen:features "full"
] ;
```

### Add New Modules

```turtle
ggen:module [
    ggen:name "advanced" ;
    ggen:description "Advanced mathematical operations" ;
    ggen:functions (
        [ggen:name "power" ; ggen:signature "fn power(base: f64, exp: f64) -> f64"]
    )
] ;
```

### Add Configuration

```turtle
ggen:config [
    ggen:key "max_precision" ;
    ggen:value "10"^^xsd:integer
] ;
```

Then query in template:

```sparql
SELECT ?key ?value
WHERE {
  ?project ggen:config ?config .
  ?config ggen:key ?key ;
          ggen:value ?value .
}
```

## Architecture Diagram

```
┌─────────────┐
│ project.ttl │  RDF definitions (data)
└──────┬──────┘
       │
       │ parsed by ggen
       ▼
┌─────────────┐
│  RDF Graph  │  In-memory triple store
└──────┬──────┘
       │
       │ queried by SPARQL
       ▼
┌──────────────┐
│ template.hbs │  Handlebars template (logic)
└──────┬───────┘
       │
       │ processed by ggen
       ▼
┌──────────────┐
│   Output     │  Generated Rust project
│              │
│ ├─Cargo.toml │
│ ├─src/lib.rs │
│ └─README.md  │
└──────────────┘
```

## Key Insights

1. **Separation of Concerns**: Data (RDF) is separate from presentation (templates)
2. **Reusability**: Same RDF can drive multiple templates (e.g., Python version)
3. **Validation**: RDF schemas can validate project definitions
4. **Extensibility**: Add new vocabularies without changing templates
5. **Traceability**: Generated code links back to RDF definitions

## Troubleshooting

### SPARQL Query Returns No Results

- Check namespace prefixes match RDF file
- Verify RDF syntax with online validator
- Use `SELECT *` to see all available data
- Check for typos in property names

### Generated Code Doesn't Compile

- Validate Rust syntax in template
- Check that all variables are properly escaped
- Ensure function signatures are complete
- Verify dependency versions are correct

### Test Script Fails

- Ensure ggen is in PATH or use `cargo run`
- Check file permissions on `test.sh`
- Verify output directory is writable
- Check Rust toolchain version

## Next Steps

1. **Customize**: Modify `project.ttl` to define your own project
2. **Extend**: Add new SPARQL queries to extract more data
3. **Generate**: Create templates for other languages (Python, Go, etc.)
4. **Integrate**: Use in CI/CD to generate projects from metadata
5. **Scale**: Generate multi-crate workspaces from complex RDF graphs

## Resources

- [SPARQL 1.1 Specification](https://www.w3.org/TR/sparql11-query/)
- [DOAP Vocabulary](https://github.com/ewilderj/doap)
- [RDF Primer](https://www.w3.org/TR/rdf11-primer/)
- [Handlebars Documentation](https://handlebarsjs.com/)
- [ggen Documentation](../../docs/)

---

**License**: MIT

**Generated by**: ggen v2.0 E2E Demo
