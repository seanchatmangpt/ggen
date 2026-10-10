# SHACL CLI - SHACL Validation, Constraint Enforcement, and Reporting

[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![License: Apache 2.0](https://img.shields.io/badge/License-Apache%202.0-blue.svg)](https://opensource.org/licenses/Apache-2.0)
[![Rust](https://img.shields.io/badge/rust-1.70%2B-orange.svg)](https://www.rust-lang.org)

A powerful command-line tool for SHACL (Shapes Constraint Language) validation, constraint enforcement, and comprehensive reporting. Built with the `clap-noun-verb` framework for intuitive RDF data quality assurance.

## Features

- **SHACL Core Support**: Complete implementation of SHACL Core constraints
- **SHACL-SPARQL**: Advanced SPARQL-based custom constraints
- **Shape Management**: Create, manage, and validate SHACL shapes
- **Constraint Types**: Node shapes, property shapes, logical constraints, property pair constraints
- **Validation Modes**: Single file, batch, continuous, and incremental validation
- **Rich Reporting**: Generate reports in Turtle, JSON, HTML, and Markdown formats
- **Visualization**: Interactive charts and graphs for validation results
- **Performance**: Optimized validation engine with parallel processing
- **Severity Levels**: Violation, Warning, and Info severity classification

## Installation

### From Source

```bash
git clone https://github.com/yourusername/shacl-cli
cd shacl-cli
cargo install --path .
```

### Using Cargo

```bash
cargo install shacl-cli
```

## Quick Start

### 1. Create a Node Shape

```bash
# Create a shape for validating Person entities
shacl-cli shape create \
  --name PersonShape \
  --target-class foaf:Person \
  --shape-type node
```

### 2. Add Constraints

```bash
# Require name property
shacl-cli constraint add PersonShape \
  --type minCount \
  --path foaf:name \
  --value 1 \
  --severity violation \
  --message "Person must have at least one name"

# Validate email format
shacl-cli constraint add PersonShape \
  --type pattern \
  --path foaf:mbox \
  --value "^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}$" \
  --message "Email must be valid format"

# Ensure age is integer
shacl-cli constraint add PersonShape \
  --type datatype \
  --path foaf:age \
  --value xsd:integer
```

### 3. Validate Data

```bash
# Validate RDF data against shapes
shacl-cli validation validate data.ttl \
  --shapes shapes.ttl \
  --format turtle

# Batch validate multiple files
shacl-cli validation batch ./data-directory \
  --shapes shapes.ttl \
  --recursive \
  --parallel 4
```

### 4. Generate Reports

```bash
# Generate HTML report with visualization
shacl-cli report generate validation-results.ttl \
  --format html \
  --group-by severity

# Export to JSON
shacl-cli report export report.ttl output.json \
  --format json

# Visualize results
shacl-cli report visualize report.ttl \
  --chart-type bar \
  --format svg \
  --interactive
```

## Command Reference

### Shape Commands

#### Create Shape

Create a new SHACL shape definition.

```bash
shacl-cli shape create [OPTIONS] --name <NAME> --target-class <CLASS>

Options:
  --name <NAME>                  Shape name
  --target-class <CLASS>         Target class IRI
  --shape-type <TYPE>            Shape type: node or property [default: node]
  --property-path <PATH>         Property path (for property shapes)
  --datatype <DATATYPE>          Expected datatype
  --min-count <COUNT>            Minimum cardinality
  --max-count <COUNT>            Maximum cardinality
  -f, --format <FORMAT>          Output format [default: turtle]
```

**Examples:**

```bash
# Node shape for Person class
shacl-cli shape create --name PersonShape --target-class foaf:Person

# Property shape for email validation
shacl-cli shape create \
  --name EmailShape \
  --shape-type property \
  --property-path foaf:mbox \
  --datatype xsd:string \
  --min-count 1

# Shape with multiple constraints
shacl-cli shape create \
  --name AgeShape \
  --target-class foaf:Person \
  --property-path foaf:age \
  --datatype xsd:integer \
  --min-count 0 \
  --max-count 1
```

#### List Shapes

List all defined SHACL shapes.

```bash
shacl-cli shape list [OPTIONS]

Options:
  --filter-type <TYPE>           Filter by shape type
  --filter-severity <SEVERITY>   Filter by severity level
  -f, --format <FORMAT>          Output format [default: table]
```

**Examples:**

```bash
# List all shapes
shacl-cli shape list

# List only property shapes
shacl-cli shape list --filter-type property

# List shapes with violation severity
shacl-cli shape list --filter-severity violation --format json
```

#### Show Shape

Display detailed information about a shape.

```bash
shacl-cli shape show <NAME> [OPTIONS]

Arguments:
  <NAME>  Shape name

Options:
  --include-constraints          Show all constraints
  -f, --format <FORMAT>          Output format [default: turtle]
```

**Examples:**

```bash
# Show shape details
shacl-cli shape show PersonShape

# Show with all constraints
shacl-cli shape show PersonShape --include-constraints

# Export as JSON
shacl-cli shape show PersonShape --format json
```

#### Compile Shape

Compile shapes into optimized validation engine.

```bash
shacl-cli shape compile <SHAPE-FILE> [OPTIONS]

Arguments:
  <SHAPE-FILE>  Path to shapes file

Options:
  --output <PATH>                Output path for compiled shapes
  --optimize <LEVEL>             Optimization level (0-3) [default: 2]
```

**Examples:**

```bash
# Compile shapes with default optimization
shacl-cli shape compile shapes.ttl

# Compile with maximum optimization
shacl-cli shape compile shapes.ttl --optimize 3 --output compiled.bin
```

#### Validate Shape

Validate the shape definition itself for correctness.

```bash
shacl-cli shape validate-shape <NAME> [OPTIONS]

Arguments:
  <NAME>  Shape name

Options:
  --strict                       Enable strict validation mode
```

**Examples:**

```bash
# Validate shape definition
shacl-cli shape validate-shape PersonShape

# Strict validation
shacl-cli shape validate-shape PersonShape --strict
```

### Constraint Commands

#### Add Constraint

Add a constraint to a shape.

```bash
shacl-cli constraint add <SHAPE> [OPTIONS] --type <TYPE>

Arguments:
  <SHAPE>  Shape name

Options:
  --type <TYPE>                  Constraint type
  --value <VALUE>                Constraint value
  --path <PATH>                  Property path
  --severity <SEVERITY>          Severity level [default: violation]
  --message <MESSAGE>            Validation message
```

**Constraint Types:**

- **Cardinality**: `minCount`, `maxCount`
- **Value**: `datatype`, `class`, `nodeKind`, `pattern`, `in`, `minInclusive`, `maxInclusive`
- **Logical**: `and`, `or`, `not`, `xone`
- **Property Pair**: `equals`, `disjoint`, `lessThan`, `lessThanOrEquals`

**Examples:**

```bash
# Minimum count constraint
shacl-cli constraint add PersonShape \
  --type minCount \
  --path foaf:name \
  --value 1 \
  --message "Must have at least one name"

# Datatype constraint
shacl-cli constraint add PersonShape \
  --type datatype \
  --path foaf:age \
  --value xsd:integer

# Pattern constraint for email
shacl-cli constraint add PersonShape \
  --type pattern \
  --path foaf:mbox \
  --value "^[\\w.-]+@[\\w.-]+\\.\\w+$"

# Class constraint
shacl-cli constraint add PersonShape \
  --type class \
  --path foaf:knows \
  --value foaf:Person

# Logical AND constraint
shacl-cli constraint add PersonShape \
  --type and \
  --value "constraint1,constraint2"
```

#### Remove Constraint

Remove a constraint from a shape.

```bash
shacl-cli constraint remove <SHAPE> <CONSTRAINT-ID>

Arguments:
  <SHAPE>          Shape name
  <CONSTRAINT-ID>  Constraint identifier
```

**Examples:**

```bash
# Remove specific constraint
shacl-cli constraint remove PersonShape constraint-123
```

#### Test Constraint

Test a constraint against sample data.

```bash
shacl-cli constraint test <CONSTRAINT-ID> <DATA-FILE> [OPTIONS]

Arguments:
  <CONSTRAINT-ID>  Constraint identifier
  <DATA-FILE>      Path to test data

Options:
  --show-matches               Show matching nodes
  --show-violations            Show violating nodes
```

**Examples:**

```bash
# Test constraint
shacl-cli constraint test constraint-123 test-data.ttl

# Show violations only
shacl-cli constraint test constraint-123 data.ttl --show-violations
```

#### Explain Constraint

Explain constraint logic and validation behavior.

```bash
shacl-cli constraint explain <CONSTRAINT-ID> [OPTIONS]

Arguments:
  <CONSTRAINT-ID>  Constraint identifier

Options:
  --include-examples           Include usage examples
  -f, --format <FORMAT>        Output format [default: text]
```

**Examples:**

```bash
# Explain constraint
shacl-cli constraint explain constraint-123

# Include examples
shacl-cli constraint explain constraint-123 --include-examples
```

#### Customize Constraint

Create custom SPARQL-based constraint.

```bash
shacl-cli constraint customize <SHAPE> <SPARQL-QUERY> [OPTIONS]

Arguments:
  <SHAPE>         Shape name
  <SPARQL-QUERY>  Path to SPARQL query file

Options:
  --severity <SEVERITY>        Severity level [default: violation]
  --message <MESSAGE>          Validation message
  --name <NAME>                Constraint name
```

**Examples:**

```bash
# Create custom SPARQL constraint
shacl-cli constraint customize PersonShape email-check.sparql \
  --name EmailDomainCheck \
  --severity warning \
  --message "Email should use company domain"
```

**SPARQL Query Example (email-check.sparql):**

```sparql
PREFIX foaf: <http://xmlns.com/foaf/0.1/>

SELECT $this
WHERE {
  $this foaf:mbox ?email .
  FILTER (!CONTAINS(str(?email), "@example.com"))
}
```

### Validation Commands

#### Validate

Validate RDF data against SHACL shapes.

```bash
shacl-cli validation validate <DATA-FILE> --shapes <SHAPES-FILE> [OPTIONS]

Arguments:
  <DATA-FILE>    Path to RDF data file

Options:
  --shapes <SHAPES-FILE>       Path to shapes file (required)
  -f, --format <FORMAT>        Output format [default: turtle]
  --fail-on-warning            Exit with error on warnings
  --max-violations <COUNT>     Maximum violations to report
```

**Examples:**

```bash
# Basic validation
shacl-cli validation validate data.ttl --shapes shapes.ttl

# Output as JSON
shacl-cli validation validate data.ttl \
  --shapes shapes.ttl \
  --format json

# Fail on warnings
shacl-cli validation validate data.ttl \
  --shapes shapes.ttl \
  --fail-on-warning

# Limit violation reporting
shacl-cli validation validate data.ttl \
  --shapes shapes.ttl \
  --max-violations 10
```

#### Batch Validate

Validate multiple data files in batch.

```bash
shacl-cli validation batch <DATA-DIRECTORY> --shapes <SHAPES-FILE> [OPTIONS]

Arguments:
  <DATA-DIRECTORY>  Directory containing data files

Options:
  --shapes <SHAPES-FILE>       Path to shapes file (required)
  --recursive                  Process subdirectories
  --parallel <COUNT>           Number of parallel workers
  --output-directory <PATH>    Output directory for reports
```

**Examples:**

```bash
# Batch validate directory
shacl-cli validation batch ./data --shapes shapes.ttl

# Recursive with parallel processing
shacl-cli validation batch ./data \
  --shapes shapes.ttl \
  --recursive \
  --parallel 4

# Save reports to directory
shacl-cli validation batch ./data \
  --shapes shapes.ttl \
  --output-directory ./reports
```

#### Continuous Validate

Continuously validate data with file watching.

```bash
shacl-cli validation continuous <WATCH-PATH> --shapes <SHAPES-FILE> [OPTIONS]

Arguments:
  <WATCH-PATH>  Path to watch for changes

Options:
  --shapes <SHAPES-FILE>       Path to shapes file (required)
  --interval <SECONDS>         Check interval in seconds [default: 5]
  --notify-on-change           Send notifications on validation changes
```

**Examples:**

```bash
# Watch directory for changes
shacl-cli validation continuous ./data --shapes shapes.ttl

# Custom check interval
shacl-cli validation continuous ./data \
  --shapes shapes.ttl \
  --interval 10

# With notifications
shacl-cli validation continuous ./data \
  --shapes shapes.ttl \
  --notify-on-change
```

#### Incremental Validate

Incrementally validate only changed data.

```bash
shacl-cli validation incremental <DATA-FILE> --shapes <SHAPES-FILE> [OPTIONS]

Arguments:
  <DATA-FILE>  Path to RDF data file

Options:
  --shapes <SHAPES-FILE>       Path to shapes file (required)
  --baseline <FILE>            Baseline validation results
  --cache-enabled              Enable validation caching
```

**Examples:**

```bash
# Incremental validation with baseline
shacl-cli validation incremental data.ttl \
  --shapes shapes.ttl \
  --baseline baseline-results.ttl

# With caching enabled
shacl-cli validation incremental data.ttl \
  --shapes shapes.ttl \
  --baseline baseline.ttl \
  --cache-enabled
```

### Report Commands

#### Generate Report

Generate validation report from results.

```bash
shacl-cli report generate <VALIDATION-FILE> [OPTIONS]

Arguments:
  <VALIDATION-FILE>  Validation results file

Options:
  -f, --format <FORMAT>        Output format [default: html]
  --template <TEMPLATE>        Custom report template
  --include-valid              Include valid nodes in report
  --group-by <FIELD>           Group results by field (severity, shape, etc.)
```

**Examples:**

```bash
# Generate HTML report
shacl-cli report generate results.ttl --format html

# Group by severity
shacl-cli report generate results.ttl \
  --format html \
  --group-by severity

# Include valid nodes
shacl-cli report generate results.ttl \
  --format markdown \
  --include-valid

# Custom template
shacl-cli report generate results.ttl \
  --format html \
  --template custom-template.html
```

#### Export Report

Export validation report to file.

```bash
shacl-cli report export <REPORT-FILE> <OUTPUT-PATH> [OPTIONS]

Arguments:
  <REPORT-FILE>   Report file to export
  <OUTPUT-PATH>   Output file path

Options:
  -f, --format <FORMAT>        Output format
  --compress                   Compress output file
```

**Examples:**

```bash
# Export to JSON
shacl-cli report export report.ttl output.json --format json

# Export with compression
shacl-cli report export report.ttl output.json.gz \
  --format json \
  --compress
```

#### Summarize Report

Generate summary statistics from validation report.

```bash
shacl-cli report summarize <REPORT-FILE> [OPTIONS]

Arguments:
  <REPORT-FILE>  Report file to summarize

Options:
  --group-by <FIELD>           Group by field
  --include-details            Include detailed statistics
```

**Examples:**

```bash
# Basic summary
shacl-cli report summarize report.ttl

# Group by shape
shacl-cli report summarize report.ttl --group-by shape

# Detailed statistics
shacl-cli report summarize report.ttl \
  --group-by severity \
  --include-details
```

#### Visualize Report

Create visual representation of validation report.

```bash
shacl-cli report visualize <REPORT-FILE> [OPTIONS]

Arguments:
  <REPORT-FILE>  Report file to visualize

Options:
  --chart-type <TYPE>          Chart type (bar, pie, line, etc.) [default: bar]
  -f, --format <FORMAT>        Output format (svg, png, html) [default: svg]
  --interactive                Generate interactive chart
```

**Examples:**

```bash
# Bar chart visualization
shacl-cli report visualize report.ttl --chart-type bar

# Interactive HTML chart
shacl-cli report visualize report.ttl \
  --chart-type pie \
  --format html \
  --interactive

# Export as SVG
shacl-cli report visualize report.ttl \
  --chart-type line \
  --format svg
```

## Advanced Usage

### Complex Shape Example

```bash
# Create organization shape with multiple constraints
cat > org-shape.ttl <<EOF
@prefix sh: <http://www.w3.org/ns/shacl#> .
@prefix org: <http://www.w3.org/ns/org#> .
@prefix foaf: <http://xmlns.com/foaf/0.1/> .

:OrganizationShape a sh:NodeShape ;
  sh:targetClass org:Organization ;
  sh:property [
    sh:path org:identifier ;
    sh:minCount 1 ;
    sh:maxCount 1 ;
    sh:datatype xsd:string ;
    sh:pattern "^ORG-[0-9]{5}$" ;
    sh:severity sh:Violation ;
    sh:message "Organization must have unique identifier matching ORG-XXXXX pattern" ;
  ] ;
  sh:property [
    sh:path foaf:name ;
    sh:minCount 1 ;
    sh:datatype xsd:string ;
    sh:minLength 3 ;
    sh:severity sh:Violation ;
  ] ;
  sh:property [
    sh:path org:hasSubOrganization ;
    sh:class org:Organization ;
    sh:severity sh:Info ;
  ] .
EOF

# Validate data against complex shape
shacl-cli validation validate organizations.ttl --shapes org-shape.ttl
```

### Custom SPARQL Constraint

```bash
# Create SPARQL constraint for business rules
cat > business-rule.sparql <<EOF
PREFIX org: <http://www.w3.org/ns/org#>
PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>

SELECT $this (?memberCount as ?value)
WHERE {
  $this org:hasMember ?member .
  {
    SELECT $this (COUNT(?member) as ?memberCount)
    WHERE {
      $this org:hasMember ?member .
    }
    GROUP BY $this
  }
  FILTER (?memberCount < 3)
}
EOF

# Add as custom constraint
shacl-cli constraint customize OrganizationShape business-rule.sparql \
  --name MinimumMembersRule \
  --severity warning \
  --message "Organization should have at least 3 members"
```

### Pipeline Validation Workflow

```bash
#!/bin/bash
# validation-pipeline.sh

# Step 1: Validate shapes
echo "Validating shapes..."
shacl-cli shape validate-shape PersonShape --strict
shacl-cli shape validate-shape OrganizationShape --strict

# Step 2: Compile shapes for performance
echo "Compiling shapes..."
shacl-cli shape compile all-shapes.ttl --optimize 3 --output compiled-shapes.bin

# Step 3: Batch validate data
echo "Validating data..."
shacl-cli validation batch ./data \
  --shapes compiled-shapes.bin \
  --recursive \
  --parallel 8 \
  --output-directory ./validation-results

# Step 4: Generate reports
echo "Generating reports..."
for result in ./validation-results/*.ttl; do
  shacl-cli report generate "$result" \
    --format html \
    --group-by severity
done

# Step 5: Create summary
echo "Creating summary..."
shacl-cli report summarize ./validation-results/*.ttl \
  --group-by shape \
  --include-details > summary.txt

# Step 6: Visualize results
echo "Creating visualizations..."
shacl-cli report visualize ./validation-results/combined.ttl \
  --chart-type bar \
  --format html \
  --interactive > validation-dashboard.html

echo "Validation pipeline complete!"
```

## Architecture

### Validation Flow

```
┌─────────────────┐
│   RDF Data      │
│   + Shapes      │
└────────┬────────┘
         │
         v
┌─────────────────┐
│ Shape Compiler  │
│ - Parse shapes  │
│ - Optimize      │
└────────┬────────┘
         │
         v
┌─────────────────┐
│ Validation      │
│ Engine          │
│ - Node shapes   │
│ - Property      │
│ - SPARQL        │
└────────┬────────┘
         │
         v
┌─────────────────┐
│ Report          │
│ Generator       │
│ - Format        │
│ - Visualize     │
└─────────────────┘
```

### Constraint Types

**Cardinality Constraints:**
- `sh:minCount` - Minimum property values
- `sh:maxCount` - Maximum property values

**Value Constraints:**
- `sh:datatype` - Expected datatype
- `sh:class` - Expected class
- `sh:nodeKind` - Node type (IRI, BlankNode, Literal)
- `sh:pattern` - Regex pattern matching
- `sh:in` - Enumerated values
- `sh:minInclusive` / `sh:maxInclusive` - Numeric ranges
- `sh:minLength` / `sh:maxLength` - String length

**Logical Constraints:**
- `sh:and` - All constraints must be satisfied
- `sh:or` - At least one constraint must be satisfied
- `sh:not` - Constraint must not be satisfied
- `sh:xone` - Exactly one constraint must be satisfied

**Property Pair Constraints:**
- `sh:equals` - Properties must have equal values
- `sh:disjoint` - Properties must not share values
- `sh:lessThan` - Value comparison
- `sh:lessThanOrEquals` - Value comparison

### Severity Levels

1. **Violation** (`sh:Violation`)
   - Critical constraint failures
   - Data does not conform to specification
   - Typically causes validation to fail

2. **Warning** (`sh:Warning`)
   - Non-critical issues
   - Best practice violations
   - Does not fail validation by default

3. **Info** (`sh:Info`)
   - Informational messages
   - Suggestions for improvement
   - Always passes validation

## Performance Optimization

### Compilation

Compile shapes for faster validation:

```bash
shacl-cli shape compile shapes.ttl --optimize 3
```

**Optimization Levels:**
- `0` - No optimization (fastest compilation)
- `1` - Basic optimization
- `2` - Standard optimization (default)
- `3` - Maximum optimization (slowest compilation, fastest validation)

### Parallel Processing

Use parallel workers for batch validation:

```bash
shacl-cli validation batch ./data \
  --shapes shapes.ttl \
  --parallel 8
```

### Incremental Validation

Validate only changed data:

```bash
shacl-cli validation incremental data.ttl \
  --shapes shapes.ttl \
  --baseline previous-results.ttl \
  --cache-enabled
```

## Integration Examples

### CI/CD Pipeline

```yaml
# .github/workflows/data-validation.yml
name: Data Validation

on: [push, pull_request]

jobs:
  validate:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4

      - name: Install shacl-cli
        run: cargo install shacl-cli

      - name: Validate shapes
        run: |
          shacl-cli shape validate-shape PersonShape --strict
          shacl-cli shape validate-shape OrganizationShape --strict

      - name: Validate data
        run: |
          shacl-cli validation batch ./data \
            --shapes ./shapes \
            --recursive \
            --fail-on-warning

      - name: Generate report
        if: always()
        run: |
          shacl-cli report generate results.ttl \
            --format html \
            --group-by severity > report.html

      - name: Upload report
        if: always()
        uses: actions/upload-artifact@v4
        with:
          name: validation-report
          path: report.html
```

### Pre-commit Hook

```bash
#!/bin/bash
# .git/hooks/pre-commit

echo "Validating RDF data..."

# Find changed .ttl files
CHANGED_FILES=$(git diff --cached --name-only --diff-filter=ACM | grep '\.ttl$')

if [ -z "$CHANGED_FILES" ]; then
  echo "No RDF files changed"
  exit 0
fi

# Validate each file
for file in $CHANGED_FILES; do
  echo "Validating $file..."
  shacl-cli validation validate "$file" \
    --shapes shapes.ttl \
    --fail-on-warning

  if [ $? -ne 0 ]; then
    echo "Validation failed for $file"
    exit 1
  fi
done

echo "All validations passed!"
exit 0
```

## Contributing

Contributions are welcome! Please feel free to submit a Pull Request.

## License

Licensed under either of:

- Apache License, Version 2.0 ([LICENSE-APACHE](LICENSE-APACHE) or http://www.apache.org/licenses/LICENSE-2.0)
- MIT license ([LICENSE-MIT](LICENSE-MIT) or http://opensource.org/licenses/MIT)

at your option.

## Resources

- [SHACL Specification](https://www.w3.org/TR/shacl/)
- [SHACL Advanced Features](https://www.w3.org/TR/shacl-af/)
- [clap-noun-verb Documentation](https://docs.rs/clap-noun-verb)
- [RDF 1.1 Turtle](https://www.w3.org/TR/turtle/)

## Acknowledgments

Built with:
- [clap-noun-verb](https://crates.io/crates/clap-noun-verb) - CLI framework
- [Oxigraph](https://crates.io/crates/oxigraph) - RDF database
- [Rio](https://crates.io/crates/rio) - RDF parsing
