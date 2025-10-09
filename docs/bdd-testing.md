# BDD Testing Guide

This document describes the Behavior-Driven Development (BDD) test suite for rgen, which validates all claims made in the README.

## Overview

The BDD test suite uses the Cucumber framework to create executable specifications that verify every feature documented in the README. Each feature file corresponds to a section of the README and ensures that documented functionality works as described.

## Test Structure

```
tests/bdd/
├── features/           # Gherkin feature files
│   ├── installation.feature
│   ├── quickstart.feature
│   ├── template_generation.feature
│   ├── marketplace.feature
│   ├── cli_commands.feature
│   ├── determinism.feature
│   ├── multi_language.feature
│   └── rdf_sparql.feature
├── steps/             # Step definition implementations
│   ├── common_steps.rs
│   ├── installation_steps.rs
│   ├── quickstart_steps.rs
│   ├── template_steps.rs
│   ├── marketplace_steps.rs
│   ├── cli_steps.rs
│   ├── determinism_steps.rs
│   ├── multilang_steps.rs
│   └── rdf_steps.rs
├── world.rs          # Test world state management
└── bdd.rs            # Test runner
```

## Running BDD Tests

### Run All BDD Tests

```bash
cargo make test-bdd
```

### Run Specific Feature

```bash
cargo test --test bdd test_installation_features
cargo test --test bdd test_quickstart_features
cargo test --test bdd test_template_generation_features
cargo test --test bdd test_marketplace_features
cargo test --test bdd test_cli_commands_features
cargo test --test bdd test_determinism_features
cargo test --test bdd test_multi_language_features
cargo test --test bdd test_rdf_sparql_features
```

### Run with Coverage

```bash
cargo make test-bdd-coverage
```

### Generate Report

```bash
cargo make bdd-report
```

## Feature Coverage

### Installation Features (`installation.feature`)
- ✅ Install from crates.io
- ✅ Build from source
- ✅ System installation

### Quick Start Features (`quickstart.feature`)
- ✅ Generate from local template
- ✅ Search marketplace
- ✅ Install and use marketplace rpack

### Template Generation Features (`template_generation.feature`)
- ✅ Basic template with frontmatter
- ✅ Template with inline RDF
- ✅ Template with SPARQL
- ✅ Template with determinism seed

### Marketplace Features (`marketplace.feature`)
- ✅ Search by language
- ✅ Browse categories
- ✅ Get package details
- ✅ Install latest version
- ✅ Install specific version
- ✅ Update packages
- ✅ Use installed rpack

### CLI Commands Features (`cli_commands.feature`)
- ✅ Marketplace commands work
- ✅ Generation commands work
- ✅ Validation commands work
- ✅ Utility commands work

### Determinism Features (`determinism.feature`)
- ✅ Same inputs produce identical outputs
- ✅ Different seeds produce different outputs
- ✅ Manifest hash computation

### Multi-language Features (`multi_language.feature`)
- ✅ Generate for multiple languages using marketplace
- ✅ Generate for multiple languages using local templates

### RDF & SPARQL Features (`rdf_sparql.feature`)
- ✅ Load external RDF files
- ✅ Use inline RDF in Turtle format
- ✅ Execute SPARQL queries for variable extraction
- ✅ Use namespace prefixes

## Adding New Scenarios

### 1. Add Scenario to Feature File

Edit the appropriate `.feature` file in `tests/bdd/features/`:

```gherkin
Scenario: New functionality
  Given I have a clean project directory
  When I run "rgen new-command"
  Then the command should succeed
  And I should see "expected output"
```

### 2. Implement Step Definitions

Add step definitions to the appropriate `steps/*.rs` file:

```rust
#[when("I run {string}")]
fn run_command(world: &mut RgenWorld, command: &str) {
    // Implementation
}

#[then("I should see {string}")]
fn should_see_text(world: &mut RgenWorld, expected: &str) {
    // Implementation
}
```

### 3. Update Test Runner

Add the new step definitions to `tests/bdd.rs`:

```rust
#[tokio::test]
async fn test_new_feature() {
    let cucumber = Cucumber::<RgenWorld>::new()
        .features(&["tests/bdd/features/new_feature.feature"])
        .steps(steps::new_steps::steps());
    
    cucumber.run_and_exit().await;
}
```

## Debugging Failing Scenarios

### 1. Run with Verbose Output

```bash
cargo test --test bdd -- --nocapture
```

### 2. Run Single Scenario

```bash
cargo test --test bdd test_specific_feature -- --nocapture
```

### 3. Check World State

Add debugging output to step definitions:

```rust
#[then("I should see {string}")]
fn should_see_text(world: &mut RgenWorld, expected: &str) {
    println!("Expected: {}", expected);
    println!("Stdout: {}", world.last_stdout());
    println!("Stderr: {}", world.last_stderr());
    // ... rest of implementation
}
```

### 4. Inspect Generated Files

```rust
#[then("a file should be generated")]
fn file_should_be_generated(world: &mut RgenWorld) {
    let output_dir = world.project_dir.join("output");
    if output_dir.exists() {
        for entry in fs::read_dir(&output_dir).expect("Failed to read output dir") {
            let entry = entry.expect("Failed to read entry");
            let path = entry.path();
            if path.is_file() {
                let content = fs::read_to_string(&path).expect("Failed to read file");
                println!("Generated file {}: {}", path.display(), content);
            }
        }
    }
}
```

## Best Practices

### 1. Isolation
- Each scenario runs in its own temporary directory
- No shared state between scenarios
- Clean setup and teardown

### 2. Determinism
- Use fixed seeds for deterministic tests
- Avoid time-dependent assertions
- Mock external dependencies

### 3. Readability
- Use descriptive scenario names
- Keep step definitions focused
- Add comments for complex logic

### 4. Maintainability
- Reuse common step definitions
- Keep feature files focused on single concerns
- Update scenarios when README changes

## Integration with CI

BDD tests are automatically run in CI as part of the full test suite:

```yaml
- name: Run BDD Tests
  run: cargo make test-bdd
  
- name: Generate BDD Report
  run: cargo make bdd-report
  
- name: Upload BDD Results
  uses: actions/upload-artifact@v3
  with:
    name: bdd-results
    path: target/bdd-report.xml
```

## Coverage Expectations

The BDD test suite aims for 100% coverage of README claims:

- ✅ 100% of README installation methods
- ✅ 100% of README quick start steps
- ✅ 100% of README marketplace features
- ✅ 100% of README CLI commands
- ✅ 100% of README template features
- ✅ 100% of README multi-language examples
- ✅ 100% of README determinism claims
- ✅ 100% of README RDF/SPARQL features

## Success Criteria

1. Every README claim has a BDD scenario
2. All scenarios pass
3. Tests are maintainable and readable
4. Coverage report shows 100% feature validation
5. README can be auto-generated from passing tests

## Troubleshooting

### Common Issues

1. **Test fails with "binary not found"**
   - Ensure `cargo make build` has been run
   - Check that the binary exists in `target/debug/rgen`

2. **Mock server not working**
   - Verify mockito server is started correctly
   - Check that registry URL is set properly

3. **File not found errors**
   - Ensure temporary directories are created
   - Check file paths are relative to project directory

4. **Determinism test failures**
   - Verify seeds are set correctly
   - Check that outputs are actually deterministic

### Getting Help

- Check the cucumber-rs documentation
- Review existing step definitions for patterns
- Ask in the project's issue tracker
- Consult the README for expected behavior
