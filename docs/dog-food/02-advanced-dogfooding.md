# Phase 2: Advanced Dogfooding

> **Priority**: **Medium** | **Timeline**: Weeks 5-12 | **Complexity**: Medium

Intermediate dogfooding requiring moderate infrastructure investment, focusing on semantic modeling, template-driven development, and AI assistance.

## 🎯 Objectives

1. Model ggen CLI structure in RDF ontology
2. Generate CLI commands from semantic definitions
3. Use AI for documentation generation
4. Implement template-driven feature development
5. Create SPARQL-based code analysis

## 📋 Prerequisites

✅ Phase 1 completed:
- Lifecycle integration working
- Marketplace packages published
- CI/CD using lifecycle
- Template generation tested

## 📋 Implementation Tasks

### Task 1: RDF Ontology Modeling (Weeks 5-6)

**Goal**: Model ggen's CLI structure, commands, and data flow in RDF ontology for semantic querying and generation.

#### 1.1 Create Core Ontology

Create `data/ontologies/ggen-core.ttl`:

```turtle
@prefix ggen: <http://ggen.io/ontology#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
@prefix owl: <http://www.w3.org/2002/07/owl#> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

# Core Classes

ggen:Command a owl:Class ;
    rdfs:label "CLI Command" ;
    rdfs:comment "A command executable via ggen CLI" .

ggen:Argument a owl:Class ;
    rdfs:label "Command Argument" ;
    rdfs:comment "A positional argument for a command" .

ggen:Option a owl:Class ;
    rdfs:label "Command Option" ;
    rdfs:comment "A flag or named option for a command" .

ggen:Module a owl:Class ;
    rdfs:label "Code Module" ;
    rdfs:comment "A Rust module in the ggen codebase" .

ggen:LifecyclePhase a owl:Class ;
    rdfs:label "Lifecycle Phase" ;
    rdfs:comment "A phase in the ggen project lifecycle" .

ggen:MarketplacePackage a owl:Class ;
    rdfs:label "Marketplace Package" ;
    rdfs:comment "A package available in the ggen marketplace" .

# Properties

ggen:hasArgument a owl:ObjectProperty ;
    rdfs:domain ggen:Command ;
    rdfs:range ggen:Argument ;
    rdfs:label "has argument" .

ggen:hasOption a owl:ObjectProperty ;
    rdfs:domain ggen:Command ;
    rdfs:range ggen:Option ;
    rdfs:label "has option" .

ggen:implementedIn a owl:ObjectProperty ;
    rdfs:domain ggen:Command ;
    rdfs:range ggen:Module ;
    rdfs:label "implemented in" .

ggen:dependsOn a owl:ObjectProperty ;
    rdfs:domain ggen:LifecyclePhase ;
    rdfs:range ggen:LifecyclePhase ;
    rdfs:label "depends on" .

ggen:hasComplexity a owl:DatatypeProperty ;
    rdfs:domain ggen:Module ;
    rdfs:range xsd:integer ;
    rdfs:label "has complexity" ;
    rdfs:comment "Cyclomatic complexity of the module" .

ggen:hasTestCoverage a owl:DatatypeProperty ;
    rdfs:domain ggen:Module ;
    rdfs:range xsd:float ;
    rdfs:label "has test coverage" ;
    rdfs:comment "Test coverage percentage (0-100)" .
```

#### 1.2 Model CLI Commands

Create `data/instances/ggen-commands.ttl`:

```turtle
@prefix ggen: <http://ggen.io/ontology#> .
@prefix inst: <http://ggen.io/instances#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .

# Market Command Group
inst:MarketCommand a ggen:Command ;
    rdfs:label "market" ;
    rdfs:comment "Marketplace package management commands" ;
    ggen:implementedIn inst:MarketModule .

inst:MarketSearch a ggen:Command ;
    rdfs:label "market search" ;
    rdfs:comment "Search marketplace packages" ;
    ggen:implementedIn inst:MarketSearchModule ;
    ggen:hasArgument inst:SearchQueryArg ;
    ggen:hasOption inst:CategoryOption .

inst:SearchQueryArg a ggen:Argument ;
    rdfs:label "query" ;
    rdfs:comment "Search query string" ;
    ggen:argumentType "string" ;
    ggen:required true .

inst:CategoryOption a ggen:Option ;
    rdfs:label "--category" ;
    rdfs:comment "Filter by package category" ;
    ggen:optionType "string" ;
    ggen:required false .

inst:MarketAdd a ggen:Command ;
    rdfs:label "market add" ;
    rdfs:comment "Install marketplace package" ;
    ggen:implementedIn inst:MarketAddModule ;
    ggen:hasArgument inst:PackageNameArg .

inst:PackageNameArg a ggen:Argument ;
    rdfs:label "package" ;
    rdfs:comment "Package name to install" ;
    ggen:argumentType "string" ;
    ggen:required true .

# Lifecycle Command Group
inst:LifecycleCommand a ggen:Command ;
    rdfs:label "lifecycle" ;
    rdfs:comment "Project lifecycle management" ;
    ggen:implementedIn inst:LifecycleModule .

inst:LifecycleRun a ggen:Command ;
    rdfs:label "lifecycle run" ;
    rdfs:comment "Run lifecycle phase" ;
    ggen:implementedIn inst:LifecycleRunModule ;
    ggen:hasArgument inst:PhaseNameArg .

inst:PhaseNameArg a ggen:Argument ;
    rdfs:label "phase" ;
    rdfs:comment "Phase name to execute" ;
    ggen:argumentType "string" ;
    ggen:required true .

# Module Metadata
inst:MarketModule a ggen:Module ;
    rdfs:label "cli/src/cmds/market/mod.rs" ;
    ggen:hasComplexity 15 ;
    ggen:hasTestCoverage 85.5 .

inst:MarketSearchModule a ggen:Module ;
    rdfs:label "cli/src/cmds/market/search.rs" ;
    ggen:hasComplexity 8 ;
    ggen:hasTestCoverage 92.0 .

inst:MarketAddModule a ggen:Module ;
    rdfs:label "cli/src/cmds/market/add.rs" ;
    ggen:hasComplexity 12 ;
    ggen:hasTestCoverage 88.0 .
```

#### 1.3 Create SPARQL Query Tool

Create `scripts/sparql-query.sh`:

```bash
#!/bin/bash
# Query ggen ontology with SPARQL

QUERY="$1"

if [ -z "$QUERY" ]; then
    echo "Usage: $0 '<SPARQL query>'"
    exit 1
fi

# Use ggen graph query
ggen graph query "$QUERY"
```

Example queries:

```bash
# Find all commands with their modules
./scripts/sparql-query.sh "
SELECT ?command ?label ?module
WHERE {
    ?command a ggen:Command ;
             rdfs:label ?label ;
             ggen:implementedIn ?module .
}
"

# Find modules with high complexity
./scripts/sparql-query.sh "
SELECT ?module ?complexity
WHERE {
    ?module a ggen:Module ;
            ggen:hasComplexity ?complexity .
    FILTER(?complexity > 10)
}
ORDER BY DESC(?complexity)
"

# Find commands missing tests
./scripts/sparql-query.sh "
SELECT ?command ?module
WHERE {
    ?command a ggen:Command ;
             ggen:implementedIn ?module .
    FILTER NOT EXISTS {
        ?module ggen:hasTestCoverage ?coverage .
    }
}
"
```

**Success Criteria**:
- [ ] Core ontology models all CLI commands
- [ ] Instance data covers current commands
- [ ] SPARQL queries return accurate results
- [ ] Documentation generated from ontology

---

### Task 2: Command Generation from RDF (Weeks 7-8)

**Goal**: Generate CLI command boilerplate directly from RDF definitions.

#### 2.1 Create RDF-to-Code Generator

Create `ggen-core/src/generators/command_generator.rs`:

```rust
use anyhow::Result;
use sophia::graph::inmem::FastGraph;
use sophia::term::SimpleTerm;
use std::fs;
use std::path::Path;

pub struct CommandGenerator {
    graph: FastGraph,
}

impl CommandGenerator {
    pub fn from_ttl(ttl_path: &Path) -> Result<Self> {
        let ttl_content = fs::read_to_string(ttl_path)?;
        let graph = sophia::parser::turtle::parse_str(&ttl_content)
            .collect_triples()?;

        Ok(Self { graph })
    }

    pub fn generate_command(&self, command_uri: &str, output_dir: &Path) -> Result<()> {
        let command_data = self.query_command(command_uri)?;

        // Generate Rust code
        let rust_code = self.generate_rust_code(&command_data)?;
        let output_path = output_dir.join(format!("{}.rs", command_data.module_name));
        fs::write(output_path, rust_code)?;

        // Generate tests
        let test_code = self.generate_test_code(&command_data)?;
        let test_path = output_dir.join(format!("../tests/{}_test.rs", command_data.module_name));
        fs::write(test_path, test_code)?;

        // Generate documentation
        let docs = self.generate_docs(&command_data)?;
        let docs_path = output_dir.join(format!("../../docs/commands/{}.md", command_data.name));
        fs::write(docs_path, docs)?;

        Ok(())
    }

    fn query_command(&self, uri: &str) -> Result<CommandData> {
        // Query RDF graph for command details
        let sparql = format!(r#"
            SELECT ?label ?comment ?arg ?option ?module
            WHERE {{
                <{uri}> rdfs:label ?label ;
                       rdfs:comment ?comment ;
                       ggen:implementedIn ?module .
                OPTIONAL {{ <{uri}> ggen:hasArgument ?arg }}
                OPTIONAL {{ <{uri}> ggen:hasOption ?option }}
            }}
        "#);

        // Execute SPARQL query and parse results
        // Implementation details...

        Ok(CommandData::default())
    }

    fn generate_rust_code(&self, data: &CommandData) -> Result<String> {
        // Generate Rust code from command data
        let code = format!(r#"
use anyhow::Result;
use clap::Args;

/// {description}
#[derive(Debug, Args)]
pub struct {struct_name} {{
    {arguments}
    {options}
}}

impl {struct_name} {{
    pub fn run(&self) -> Result<()> {{
        // TODO: Implement {command_name}
        println!("Running {command_name}...");
        Ok(())
    }}
}}
"#,
            description = data.description,
            struct_name = data.struct_name,
            command_name = data.name,
            arguments = data.generate_arguments(),
            options = data.generate_options(),
        );

        Ok(code)
    }
}

#[derive(Default)]
struct CommandData {
    name: String,
    description: String,
    struct_name: String,
    module_name: String,
    arguments: Vec<ArgumentData>,
    options: Vec<OptionData>,
}
```

#### 2.2 Add CLI Command for Generation

Add `ggen generate command` subcommand:

```bash
# Define new command in RDF
cat > data/instances/new-command.ttl << 'EOF'
@prefix inst: <http://ggen.io/instances#> .
@prefix ggen: <http://ggen.io/ontology#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .

inst:MarketVerify a ggen:Command ;
    rdfs:label "market verify" ;
    rdfs:comment "Verify marketplace package signatures" ;
    ggen:implementedIn inst:MarketVerifyModule ;
    ggen:hasOption inst:VerboseOption .

inst:MarketVerifyModule a ggen:Module ;
    rdfs:label "cli/src/cmds/market/verify.rs" .
EOF

# Generate command from RDF
ggen generate command \
    --from data/instances/new-command.ttl \
    --uri "http://ggen.io/instances#MarketVerify" \
    --output cli/src/cmds/market/
```

**Success Criteria**:
- [ ] Commands generated from RDF definitions
- [ ] Generated code matches handwritten quality
- [ ] Tests and docs generated automatically
- [ ] <10 minute turnaround for new commands

---

### Task 3: AI-Powered Documentation (Weeks 9-10)

**Goal**: Use `ggen ai generate` to create comprehensive documentation.

#### 3.1 Create Documentation Generation Workflow

Create `scripts/generate-docs.sh`:

```bash
#!/bin/bash
# Generate comprehensive documentation using AI

set -e

echo "Generating documentation with AI..."

# Generate API documentation
ggen ai generate \
    "Create comprehensive API documentation for the marketplace module in cli/src/cmds/market/. \
    Include all public functions, their parameters, return values, examples, and error cases. \
    Format as Markdown." \
    > docs/api/marketplace.md

# Generate architecture documentation
ggen ai generate \
    "Analyze the ggen codebase and create architectural documentation. \
    Include: component diagram, data flow, module dependencies, design patterns used. \
    Format as Markdown with Mermaid diagrams." \
    > docs/architecture/overview.md

# Generate user guide
ggen ai generate \
    "Create a comprehensive user guide for ggen CLI. \
    Include: getting started, common workflows, troubleshooting, best practices. \
    Use examples from examples/ directory. Format as Markdown." \
    > docs/user-guide/complete-guide.md

# Generate command reference
for cmd in $(ggen --list); do
    echo "Generating docs for: $cmd"
    ggen ai generate \
        "Create detailed command documentation for 'ggen $cmd'. \
        Include: description, usage, all options/arguments, examples, common errors. \
        Format as Markdown." \
        > "docs/commands/$cmd.md"
done

echo "✓ Documentation generation complete"
```

#### 3.2 Integrate into Lifecycle

Add to `make.toml`:

```toml
[tasks.docs]
description = "Generate comprehensive documentation"
script = '''
echo "Generating documentation..."
./scripts/generate-docs.sh

echo "Validating documentation..."
# Check for broken links, proper formatting
mdbook test docs/

echo "✓ Documentation generated and validated"
'''
```

#### 3.3 Add Documentation Validation

Create `scripts/validate-docs.sh`:

```bash
#!/bin/bash
# Validate generated documentation

# Check for broken links
echo "Checking for broken links..."
markdown-link-check docs/**/*.md

# Validate code examples
echo "Validating code examples..."
for file in docs/**/*.md; do
    # Extract and test code blocks
    # Implementation...
done

# Check documentation coverage
echo "Checking documentation coverage..."
ggen ai generate \
    "Analyze the codebase and list all public functions/commands without documentation. \
    Format as JSON with module, function, and reason." \
    > docs/coverage-report.json

# Fail if coverage < 80%
coverage=$(jq '.coverage_percentage' docs/coverage-report.json)
if (( $(echo "$coverage < 80" | bc -l) )); then
    echo "❌ Documentation coverage too low: $coverage%"
    exit 1
fi

echo "✓ Documentation validation passed"
```

**Success Criteria**:
- [ ] 80%+ of code documented by AI
- [ ] Documentation generated in <5 minutes
- [ ] AI-generated docs pass validation
- [ ] User feedback positive on doc quality

---

### Task 4: Template-Driven Feature Development (Weeks 11-12)

**Goal**: Create reusable templates for adding new features to ggen.

#### 4.1 Create Feature Template

Create `templates/feature.tmpl`:

```handlebars
{{!-- Feature Template for Ggen --}}
{{!-- Variables: feature_name, description, modules --}}

# Feature: {{feature_name}}

## Description
{{description}}

## Implementation Plan

### Phase 1: Core Implementation
{{#each modules}}
- [ ] Create module: {{this.path}}
  - {{this.description}}
  - Test coverage target: >90%
{{/each}}

### Phase 2: CLI Integration
- [ ] Add command: `ggen {{feature_command}}`
- [ ] Update help text
- [ ] Add to command routing

### Phase 3: Testing
- [ ] Unit tests
- [ ] Integration tests
- [ ] End-to-end tests
- [ ] Performance benchmarks

### Phase 4: Documentation
- [ ] API documentation
- [ ] User guide section
- [ ] Examples
- [ ] Migration guide (if applicable)

## File Structure

```
{{#each modules}}
{{this.path}}
{{this.path}}_test.rs
{{/each}}
docs/features/{{feature_name}}.md
examples/{{feature_name}}/
```

## RDF Model

```turtle
@prefix inst: <http://ggen.io/instances#> .
@prefix ggen: <http://ggen.io/ontology#> .

{{#each commands}}
inst:{{this.name}} a ggen:Command ;
    rdfs:label "{{this.label}}" ;
    rdfs:comment "{{this.description}}" ;
    ggen:implementedIn inst:{{this.module}} .
{{/each}}
```

## Generated Files

{{#each modules}}
### {{this.path}}

```rust
// Generated from feature template

use anyhow::Result;

/// {{this.description}}
pub struct {{this.struct_name}} {
    // TODO: Add fields
}

impl {{this.struct_name}} {
    pub fn new() -> Self {
        Self {}
    }

    // TODO: Add methods
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_{{this.test_name}}() -> Result<()> {
        // TODO: Add tests
        Ok(())
    }
}
```
{{/each}}
```

#### 4.2 Use Template for New Feature

Example: Adding multi-language support

```bash
# Define feature in RDF
cat > data/features/multi-language-support.ttl << 'EOF'
@prefix feature: <http://ggen.io/features#> .
@prefix ggen: <http://ggen.io/ontology#> .

feature:MultiLanguageSupport a ggen:Feature ;
    rdfs:label "Multi-Language Support" ;
    rdfs:comment "Support for Python, JavaScript, and Go code generation" ;
    ggen:hasModule feature:LanguageModule ;
    ggen:hasModule feature:PythonGenerator ;
    ggen:hasModule feature:JavaScriptGenerator ;
    ggen:hasModule feature:GoGenerator .
EOF

# Generate feature structure
ggen template generate templates/feature.tmpl \
    --feature_name "multi-language-support" \
    --description "Add Python, JavaScript, and Go generation" \
    --modules "language/mod.rs,language/python.rs,language/javascript.rs,language/go.rs"

# This generates:
# - Implementation plan in docs/features/multi-language-support.md
# - Module stubs in ggen-core/src/language/
# - Test stubs in tests/language/
# - RDF definitions in data/features/
# - Example projects in examples/multi-language-support/
```

#### 4.3 Validate Feature Implementation

Add feature validation to lifecycle:

```toml
[tasks.feature-validate]
description = "Validate feature implementation completeness"
script = '''
# Check all modules exist
# Check test coverage
# Check documentation
# Check RDF definitions

# Query for incomplete features
ggen graph query "
SELECT ?feature ?module
WHERE {
    ?feature a ggen:Feature ;
             ggen:hasModule ?module .
    FILTER NOT EXISTS {
        ?module ggen:hasImplementation true
    }
}
"
'''
```

**Success Criteria**:
- [ ] Feature template generates complete structure
- [ ] <30 minutes to scaffold new feature
- [ ] Generated code follows conventions
- [ ] Validation ensures completeness

---

## 📊 Phase 2 Success Metrics

### RDF Modeling
- [ ] Complete CLI structure modeled in RDF
- [ ] SPARQL queries answer architectural questions
- [ ] Command generation from RDF working
- [ ] <50% reduction in boilerplate coding

### AI Documentation
- [ ] 80%+ code documentation coverage
- [ ] <5 minute doc generation time
- [ ] Documentation validated automatically
- [ ] Positive user feedback on quality

### Template-Driven Development
- [ ] 3+ feature templates created
- [ ] New features use templates
- [ ] <30 minute feature scaffolding
- [ ] 90%+ test coverage for generated code

---

## 🎯 Next Steps

After completing Phase 2:

1. **Evaluate**: Measure ROI of RDF modeling and AI documentation
2. **Optimize**: Refine templates based on usage patterns
3. **Scale**: Apply learnings to more modules
4. **Plan Phase 3**: Design self-improving systems

**Continue to**: [Phase 3: Complete Dogfooding](03-complete-dogfooding.md)
