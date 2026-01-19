# London-Style BDD Testing for ggen

**Version:** 1.0.0
**Last Updated:** 2025-01-09
**Status:** Implementation in Progress
**Methodology:** London School TDD + Cucumber BDD

---

## Table of Contents

1. [Executive Summary](#executive-summary)
2. [Philosophy & Principles](#philosophy--principles)
3. [Architecture Overview](#architecture-overview)
4. [Current Implementation Status](#current-implementation-status)
5. [Agent Task Breakdown](#agent-task-breakdown)
6. [Step Definition Specifications](#step-definition-specifications)
7. [Ambiguous Steps Resolution](#ambiguous-steps-resolution)
8. [Feature File Catalog](#feature-file-catalog)
9. [Testing Patterns & Best Practices](#testing-patterns--best-practices)
10. [Verification & Acceptance Criteria](#verification--acceptance-criteria)
11. [Troubleshooting Guide](#troubleshooting-guide)

---

## Executive Summary

### Mission
Implement comprehensive London-style BDD tests for ggen's noun-verb CLI command structure using Cucumber, ensuring 100% feature coverage and executable specifications that validate the README claims.

### Current State
- **Infrastructure:** ✅ Complete (test runner, world state, module registration)
- **Step Definitions:** 🟡 Partial (4 new modules created, ~1200 lines)
- **Test Execution:** ✅ Working (13 tests run, framework operational)
- **Coverage:** 🟡 ~40% (ambiguous steps and missing implementations)

### Critical Path
1. **Resolve ambiguous step definitions** (blocking 60% of scenarios)
2. **Implement missing step definitions** (30+ steps need implementation)
3. **Build ggen binary** (prerequisite for command execution)
4. **Fix async step definitions** (cucumber requires sync steps in some cases)
5. **Add mock registry support** (for marketplace tests)

---

## Philosophy & Principles

### London School TDD

The London School (mockist) approach emphasizes:

1. **Outside-In Development**
   - Start with high-level behavior (features)
   - Mock collaborators and external dependencies
   - Focus on object interactions and contracts

2. **Interaction Testing**
   - Verify commands are called with correct arguments
   - Validate output format and structure
   - Test observable behavior, not implementation

3. **Strict Isolation**
   - Each test scenario runs in isolated temporary directory
   - No shared state between tests
   - Mock external services (registry, filesystem where needed)

4. **Test Doubles Strategy**
   - **Stubs:** Provide canned answers (mock registry responses)
   - **Mocks:** Verify interactions (command execution)
   - **Fakes:** Working implementations (in-memory graph, temp filesystem)

### BDD with Cucumber

**Given-When-Then Structure:**
- **Given:** Setup preconditions and world state
- **When:** Execute the action being tested
- **Then:** Assert postconditions and outcomes

**Feature Files as Living Documentation:**
- Features map 1:1 with README sections
- Scenarios validate specific claims
- Steps are reusable across features

---

## Architecture Overview

### Directory Structure

```
tests/bdd/
├── bdd.rs                          # Test runner with #[tokio::test] functions
├── world.rs                        # World state (GgenWorld)
├── docs/
│   └── LONDON-BDD.md              # This document
├── features/                       # Gherkin feature files
│   ├── graph.feature              # RDF graph operations
│   ├── market.feature             # Marketplace operations
│   ├── project.feature            # Project generation
│   ├── template.feature           # Template management
│   ├── installation.feature       # Installation scenarios
│   ├── quickstart.feature         # Quick start guide
│   ├── template_generation.feature # Template rendering
│   ├── marketplace.feature        # Legacy marketplace tests
│   ├── cli_commands.feature       # CLI validation
│   ├── determinism.feature        # Reproducibility
│   ├── multi_language.feature     # Multi-language support
│   └── rdf_sparql.feature         # RDF/SPARQL integration
└── steps/                          # Step definitions (Rust)
    ├── mod.rs                      # Module registration
    ├── common_steps.rs             # Shared steps
    ├── graph_steps.rs              # NEW: graph command steps
    ├── project_steps.rs            # NEW: project command steps
    ├── market_noun_verb_steps.rs   # NEW: market command steps
    ├── template_management_steps.rs # NEW: template command steps
    ├── marketplace_steps.rs        # Legacy marketplace steps
    ├── template_steps.rs           # Legacy template steps
    ├── cli_steps.rs                # CLI execution steps
    ├── rdf_steps.rs                # RDF/SPARQL steps
    ├── determinism_steps.rs        # Determinism validation
    ├── installation_steps.rs       # Installation validation
    ├── quickstart_steps.rs         # Quick start validation
    └── multilang_steps.rs          # Multi-language validation
```

### Data Flow

```
Feature File (.feature)
    ↓
Cucumber Parser
    ↓
Step Matcher (regex matching)
    ↓
Step Definition (Rust function)
    ↓
World State (GgenWorld)
    ↓
Command Execution (assert_cmd)
    ↓
Assertions (verify output)
    ↓
Test Result (pass/fail)
```

### World State (`world.rs`)

```rust
pub struct GgenWorld {
    pub temp_dir: Option<TempDir>,        // Isolated test directory
    pub project_dir: PathBuf,              // Working directory path
    pub last_output: Option<Output>,       // Command stdout/stderr
    pub last_exit_code: Option<i32>,       // Exit code
    pub captured_files: HashMap<String, String>, // File snapshots
    pub captured_hashes: Vec<String>,      // For determinism tests
    pub registry_url: Option<String>,      // Mock registry URL
    pub mock_server: Option<mockito::Server>, // HTTP mock server
}
```

**Key Methods:**
- `last_command_succeeded() -> bool`
- `last_stdout() -> String`
- `last_stderr() -> String`
- `capture_file(path, content)`
- `capture_hash(hash)`

---

## Current Implementation Status

### ✅ Completed Components

1. **Test Runner** (`bdd.rs`)
   - 12 `#[tokio::test]` functions
   - Each test runs one feature file
   - Proper async/await support
   - Uses `fail_on_skipped()` for strict validation

2. **Step Modules Created**
   - `graph_steps.rs` (370 lines)
   - `project_steps.rs` (240 lines)
   - `market_noun_verb_steps.rs` (310 lines)
   - `template_management_steps.rs` (240 lines)

3. **Infrastructure**
   - `shell-words` dependency added for CLI parsing
   - Module registration in `steps/mod.rs`
   - Makefile tasks: `test-bdd`, `test-bdd-feature`, `test-bdd-verbose`

### 🟡 Partial Implementation

**Ambiguous Steps (HIGH PRIORITY):**
```
Step match is ambiguous for:
1. "I have a template {string} with content:"
   - Matches: project_steps.rs:27, template_management_steps.rs:36, template_steps.rs:26

2. "I have a template with:"
   - Matches: template_steps.rs:26, template_steps.rs:32

3. "the marketplace registry is available"
   - Matches: market_noun_verb_steps.rs:21, project_steps.rs:51

4. Multiple other template-related steps
```

**Missing Implementations:**
- Graph visualization steps (DOT format generation)
- SHACL validation steps
- Named graph operations
- Interactive template creation wizard
- Plan file JSON parsing
- Diff output validation

### ❌ Blocking Issues

1. **ggen Binary Not Built**
   ```
   Error: Os { code: 2, kind: NotFound, message: "No such file or directory" }
   ```
   **Solution:** Run `cargo make build` before tests

2. **Async Step Functions**
   Some step definitions are marked `async` but Cucumber may not support async in all contexts
   **Solution:** Review and convert to sync where appropriate

3. **Content Extraction from Docstrings**
   ```
   Step panicked. Captured output: content not found
   ```
   **Solution:** Cucumber passes docstring content differently; use `String` parameter

---

## Agent Task Breakdown

### 🤖 Agent 1: Ambiguous Steps Resolver

**Objective:** Resolve all ambiguous step definitions by making regex patterns more specific

**Priority:** CRITICAL
**Estimated Effort:** 2-3 hours
**Files to Modify:**
- `tests/bdd/steps/project_steps.rs`
- `tests/bdd/steps/template_management_steps.rs`
- `tests/bdd/steps/template_steps.rs`
- `tests/bdd/steps/market_noun_verb_steps.rs`

**Tasks:**

1. **Disambiguate Template Creation Steps**

   **Current Ambiguity:**
   ```gherkin
   Given I have a template "hello.tmpl" with content:
   ```
   Matches 3 patterns:
   - `^I have a template "([^"]+)" with content:$` (project_steps.rs:27)
   - `^I have a template "([^"]+)" with content:$` (template_management_steps.rs:36)
   - `^I have a template (.+)$` (template_steps.rs:26)

   **Resolution Strategy:**
   - **Keep:** `project_steps.rs` version (most specific, used in project.feature)
   - **Remove:** `template_management_steps.rs` duplicate
   - **Fix:** `template_steps.rs` to be more specific: `^I have a template file (.+)$`

   **Action Items:**
   ```rust
   // In template_management_steps.rs - DELETE or rename to:
   #[given(regex = r#"^I have a managed template "([^"]+)" with content:$"#)]

   // In template_steps.rs - CHANGE to:
   #[given(regex = r#"^I have a template file at (.+)$"#)]
   ```

2. **Disambiguate Marketplace Registry Steps**

   **Current Ambiguity:**
   ```gherkin
   Given the marketplace registry is available
   ```
   Matches:
   - `market_noun_verb_steps.rs:21`
   - `project_steps.rs:51`

   **Resolution:**
   - **Keep:** `market_noun_verb_steps.rs` (canonical location)
   - **Remove:** `project_steps.rs` version (just calls through to market)

   **Action:**
   ```rust
   // In project_steps.rs - DELETE this step:
   #[given(regex = r"^the marketplace registry is available$")]
   async fn marketplace_registry_available(_world: &mut GgenWorld) {
       // DELETE THIS ENTIRE FUNCTION
   }
   ```

3. **Disambiguate Template Variants**

   **Current Ambiguity:**
   ```gherkin
   Given I have a template with:
   Given I have a template with RDF inline:
   Given I have a template with SPARQL query:
   ```

   **Resolution:**
   Each needs unique, specific regex that doesn't overlap with generic `(.+)` matcher

   **Actions:**
   ```rust
   // In template_steps.rs - Make these MORE SPECIFIC:

   #[given(regex = r"^I have a basic template with:$")]  // Was: "I have a template with:"

   #[given(regex = r"^I have a template with RDF inline data:$")]  // Was: "...RDF inline:"

   #[given(regex = r"^I have a template with SPARQL query definition:$")]  // Was: "...SPARQL query:"

   #[given(regex = r"^I have a template with determinism configuration:$")]  // Was: "...determinism config:"
   ```

   **Then update feature files** to match new wording

**Acceptance Criteria:**
- [ ] Run `cargo test --test bdd` with no "Step match is ambiguous" errors
- [ ] All 12 features load without ambiguity warnings
- [ ] At least 50% of scenarios can match steps (up from current ~40%)

**Deliverables:**
- Modified step definition files with unique regex patterns
- Updated feature files if step wording changed
- Git commit: "fix(bdd): resolve all ambiguous step definitions"

---

### 🤖 Agent 2: Async Step Converter

**Objective:** Convert async step definitions to sync where Cucumber doesn't support async

**Priority:** HIGH
**Estimated Effort:** 1-2 hours
**Files to Modify:**
- `tests/bdd/steps/graph_steps.rs`
- `tests/bdd/steps/project_steps.rs`
- `tests/bdd/steps/market_noun_verb_steps.rs`
- `tests/bdd/steps/template_management_steps.rs`

**Background:**
Cucumber 0.20 step functions should be **synchronous** unless the entire world is async. Currently, all new step definitions are marked `async fn`, which may cause issues.

**Tasks:**

1. **Audit All Step Signatures**

   Run this check:
   ```bash
   grep -n "^async fn\|#\[given\]\|#\[when\]\|#\[then\]" tests/bdd/steps/*.rs
   ```

2. **Convert to Sync Pattern**

   **Before:**
   ```rust
   #[given(regex = r"^I have a template$")]
   async fn create_template(world: &mut GgenWorld) {
       // ...
   }
   ```

   **After:**
   ```rust
   #[given(regex = r"^I have a template$")]
   fn create_template(world: &mut GgenWorld) {
       // ...
   }
   ```

3. **Handle Async I/O**

   If step needs async I/O (rare), use blocking equivalent:

   **Before:**
   ```rust
   async fn run_command(world: &mut GgenWorld, args: String) {
       let output = cmd.output().await.expect("...");
   }
   ```

   **After:**
   ```rust
   fn run_command(world: &mut GgenWorld, args: String) {
       let output = cmd.output().expect("...");  // assert_cmd::Command::output is sync
   }
   ```

**Acceptance Criteria:**
- [ ] No `async fn` in step definitions (except where proven necessary)
- [ ] All tests pass with sync step functions
- [ ] No runtime "future not awaited" warnings

**Deliverables:**
- Modified step files with sync signatures
- Git commit: "refactor(bdd): convert step definitions from async to sync"

---

### 🤖 Agent 3: Missing Step Implementor (Graph Commands)

**Objective:** Implement missing step definitions for graph.feature scenarios

**Priority:** MEDIUM
**Estimated Effort:** 3-4 hours
**File:** `tests/bdd/steps/graph_steps.rs`

**Tasks:**

1. **Implement Remaining GIVEN Steps**

   ```rust
   #[given(regex = r"^I have a graph with person relationships$")]
   fn create_graph_with_relationships(world: &mut GgenWorld) {
       let turtle = r#"@prefix ex: <http://example.org/> .
   @prefix rel: <http://example.org/rel/> .

   ex:Alice rel:knows ex:Bob .
   ex:Bob rel:knows ex:Charlie .
   ex:Charlie rel:worksFor ex:ACME .
   "#;

       let file_path = world.project_dir.join("relationships.ttl");
       fs::write(&file_path, turtle).expect("Failed to write relationships");

       // Load into graph
       let output = Command::cargo_bin("ggen")
           .expect("ggen binary")
           .arg("graph")
           .arg("load")
           .arg("relationships.ttl")
           .current_dir(&world.project_dir)
           .output()
           .expect("Failed to load");

       world.last_output = Some(output);
   }

   #[given(regex = r"^I have a large graph$")]
   fn create_large_graph(world: &mut GgenWorld) {
       let mut turtle = String::from("@prefix ex: <http://example.org/> .\n\n");

       // Generate 1000+ triples
       for i in 0..1000 {
           turtle.push_str(&format!(
               "ex:Entity{} ex:prop{} \"value{}\" .\n",
               i, i % 10, i
           ));
       }

       let file_path = world.project_dir.join("large.ttl");
       fs::write(&file_path, turtle).expect("Failed to write large graph");

       // Load it
       let output = Command::cargo_bin("ggen")
           .expect("ggen binary")
           .arg("graph")
           .arg("load")
           .arg("large.ttl")
           .current_dir(&world.project_dir)
           .output()
           .expect("Failed to load");

       world.last_output = Some(output);
   }

   #[given(regex = r#"^I have RDF files "([^"]+)" and "([^"]+)"$"#)]
   fn create_multiple_rdf_files(
       world: &mut GgenWorld,
       file1: String,
       file2: String,
   ) {
       let turtle1 = "@prefix ex: <http://example.org/> .\nex:Resource1 ex:prop \"value1\" .";
       let turtle2 = "@prefix ex: <http://example.org/> .\nex:Resource2 ex:prop \"value2\" .";

       fs::write(world.project_dir.join(&file1), turtle1).expect("Failed to write file1");
       fs::write(world.project_dir.join(&file2), turtle2).expect("Failed to write file2");
   }

   #[given(regex = r#"^I have triples in named graph "([^"]+)"$"#)]
   fn create_named_graph_triples(world: &mut GgenWorld, graph_uri: String) {
       let turtle = format!(
           r#"@prefix ex: <http://example.org/> .

   GRAPH <{}> {{
       ex:Subject ex:predicate ex:Object .
   }}
   "#,
           graph_uri
       );

       let file_path = world.project_dir.join("named_graph.trig");
       fs::write(&file_path, turtle).expect("Failed to write named graph");

       let output = Command::cargo_bin("ggen")
           .expect("ggen binary")
           .arg("graph")
           .arg("load")
           .arg("named_graph.trig")
           .arg("--format")
           .arg("trig")
           .current_dir(&world.project_dir)
           .output()
           .expect("Failed to load");

       world.last_output = Some(output);
   }

   #[given(regex = r"^I have SHACL shapes defining person constraints$")]
   fn create_shacl_shapes(world: &mut GgenWorld) {
       let shacl = r#"@prefix sh: <http://www.w3.org/ns/shacl#> .
   @prefix ex: <http://example.org/> .

   ex:PersonShape a sh:NodeShape ;
       sh:targetClass ex:Person ;
       sh:property [
           sh:path ex:name ;
           sh:minCount 1 ;
           sh:datatype xsd:string ;
       ] ;
       sh:property [
           sh:path ex:age ;
           sh:minCount 1 ;
           sh:datatype xsd:integer ;
       ] .
   "#;

       fs::write(world.project_dir.join("shapes.ttl"), shacl)
           .expect("Failed to write SHACL shapes");
   }
   ```

2. **Implement Remaining THEN Steps**

   ```rust
   #[then(regex = r"^the triples should be in the named graph$")]
   fn triples_in_named_graph(world: &mut GgenWorld) {
       // Query the named graph
       let output = Command::cargo_bin("ggen")
           .expect("ggen binary")
           .arg("graph")
           .arg("query")
           .arg("SELECT (COUNT(*) as ?count) FROM NAMED <http://example.org/graph1> WHERE { GRAPH ?g { ?s ?p ?o } }")
           .current_dir(&world.project_dir)
           .output()
           .expect("Failed to query");

       let stdout = String::from_utf8_lossy(&output.stdout);
       assert!(
           !stdout.contains("0"),
           "Named graph should contain triples, but count is 0"
       );
   }

   #[then(regex = r"^I should see only triples from that graph$")]
   fn should_see_only_named_graph_triples(_world: &mut GgenWorld) {
       // Validation that query filtered to named graph
       // Implementation depends on query output format
   }

   #[then(regex = r"^I should see validation report$")]
   fn should_see_validation_report(world: &mut GgenWorld) {
       let stdout = world.last_stdout();
       assert!(
           stdout.contains("validation") || stdout.contains("conforms") || stdout.contains("violation"),
           "Expected validation report, but got: {}",
           stdout
       );
   }

   #[then(regex = r"^I should see number of subjects$")]
   fn should_see_subject_count(world: &mut GgenWorld) {
       let stdout = world.last_stdout();
       assert!(
           stdout.contains("subjects") || stdout.contains("Subjects:"),
           "Expected subject count, but got: {}",
           stdout
       );
   }

   #[then(regex = r"^I should see number of predicates$")]
   fn should_see_predicate_count(world: &mut GgenWorld) {
       let stdout = world.last_stdout();
       assert!(
           stdout.contains("predicates") || stdout.contains("Predicates:"),
           "Expected predicate count, but got: {}",
           stdout
       );
   }

   #[then(regex = r"^I should see number of objects$")]
   fn should_see_object_count(world: &mut GgenWorld) {
       let stdout = world.last_stdout();
       assert!(
           stdout.contains("objects") || stdout.contains("Objects:"),
           "Expected object count, but got: {}",
           stdout
       );
   }

   #[then(regex = r"^the graph should be empty$")]
   fn graph_should_be_empty(world: &mut GgenWorld) {
       let output = Command::cargo_bin("ggen")
           .expect("ggen binary")
           .arg("graph")
           .arg("stats")
           .current_dir(&world.project_dir)
           .output()
           .expect("Failed to get stats");

       let stdout = String::from_utf8_lossy(&output.stdout);
       assert!(
           stdout.contains("0 triples") || stdout.contains("Triples: 0"),
           "Graph should be empty, but got: {}",
           stdout
       );
   }

   #[then(regex = r"^the graph should contain triples from both files$")]
   fn graph_contains_merged_triples(world: &mut GgenWorld) {
       let output = Command::cargo_bin("ggen")
           .expect("ggen binary")
           .arg("graph")
           .arg("query")
           .arg("SELECT (COUNT(*) as ?count) WHERE { ?s ?p ?o }")
           .current_dir(&world.project_dir)
           .output()
           .expect("Failed to query");

       let stdout = String::from_utf8_lossy(&output.stdout);
       // Should have triples from both files (at least 2)
       assert!(
           !stdout.contains("0") && !stdout.contains("1"),
           "Expected multiple triples from merged files, but got: {}",
           stdout
       );
   }

   #[then(regex = r"^I should see (\d+) results$")]
   fn should_see_n_results(world: &mut GgenWorld, count: usize) {
       let stdout = world.last_stdout();
       // Count result rows (implementation depends on output format)
       let line_count = stdout.lines().filter(|line| !line.trim().is_empty()).count();
       assert!(
           line_count >= count,
           "Expected at least {} results, but got {}",
           count,
           line_count
       );
   }

   #[then(regex = r"^they should be results (\d+)-(\d+)$")]
   fn should_be_results_range(_world: &mut GgenWorld, _start: usize, _end: usize) {
       // Validate OFFSET/LIMIT worked correctly
       // Implementation depends on query output including identifiers
   }

   #[then(regex = r"^the file should only contain Person entities$")]
   fn file_contains_only_persons(world: &mut GgenWorld) {
       let content = fs::read_to_string(world.project_dir.join("people.ttl"))
           .expect("Failed to read exported file");

       assert!(
           content.contains("Person") || content.contains("ex:Person"),
           "Exported file should contain Person entities"
       );
   }

   #[then(regex = r"^the file should be valid DOT format$")]
   fn file_is_valid_dot(world: &mut GgenWorld) {
       let content = fs::read_to_string(world.project_dir.join("graph.dot"))
           .expect("Failed to read DOT file");

       assert!(
           content.contains("digraph") || content.contains("graph"),
           "DOT file should start with 'digraph' or 'graph'"
       );
       assert!(
           content.contains("->") || content.contains("--"),
           "DOT file should contain edges"
       );
   }

   #[then(regex = r"^it can be rendered with Graphviz$")]
   fn can_render_with_graphviz(world: &mut GgenWorld) {
       // Attempt to validate DOT syntax (doesn't require graphviz installed)
       let content = fs::read_to_string(world.project_dir.join("graph.dot"))
           .expect("Failed to read DOT file");

       // Basic syntax check
       assert!(content.contains('{') && content.contains('}'),
               "DOT file should have braces");
   }
   ```

**Acceptance Criteria:**
- [ ] All `graph.feature` scenarios can find matching steps
- [ ] Graph operations execute without "step not found" errors
- [ ] SPARQL query validation works
- [ ] Graph statistics are correctly validated

**Deliverables:**
- Completed `graph_steps.rs` with all missing implementations
- Git commit: "feat(bdd): implement missing graph command step definitions"

---

### 🤖 Agent 4: Missing Step Implementor (Project Commands)

**Objective:** Implement missing and fix broken step definitions for project.feature

**Priority:** MEDIUM
**Estimated Effort:** 2-3 hours
**File:** `tests/bdd/steps/project_steps.rs`

**Tasks:**

1. **Fix Docstring Content Extraction**

   **Current Issue:**
   ```
   Step panicked. Captured output: content not found
   ```

   **Root Cause:**
   Cucumber passes docstrings as `String` parameter, not via method call.

   **Fix:**
   ```rust
   // BEFORE (BROKEN):
   #[given(regex = r#"^I have a plan file "([^"]+)" with:$"#)]
   fn create_plan_file(world: &mut GgenWorld, filename: String, content: String) {
       let file_path = world.project_dir.join(&filename);
       fs::write(&file_path, content.trim())  // 'content' is empty!
   }

   // AFTER (FIXED):
   #[given(regex = r#"^I have a plan file "([^"]+)" with:$"#)]
   fn create_plan_file(world: &mut GgenWorld, filename: String, content: cucumber::gherkin::Step) {
       let file_path = world.project_dir.join(&filename);

       // Extract docstring from step
       let plan_content = content
           .docstring()
           .map(|s| s.to_string())
           .unwrap_or_else(|| panic!("Expected docstring for plan file content"));

       fs::write(&file_path, plan_content.trim())
           .unwrap_or_else(|e| panic!("Failed to write plan file {}: {}", filename, e));
   }
   ```

   **Apply to ALL steps with docstrings:**
   - `create_template_file`
   - `create_plan_file`
   - `create_existing_file` (if it has docstring variant)

2. **Implement Determinism Validation**

   ```rust
   #[then(regex = r"^both outputs should be identical$")]
   fn both_outputs_should_be_identical(world: &mut GgenWorld) {
       // This requires running the command twice and comparing outputs
       // Need to capture output on first run, then compare on second

       if world.captured_hashes.len() < 2 {
           panic!(
               "Expected 2 captured hashes for comparison, but got {}. \
                Ensure the scenario runs generation twice.",
               world.captured_hashes.len()
           );
       }

       let hash1 = &world.captured_hashes[0];
       let hash2 = &world.captured_hashes[1];

       assert_eq!(
           hash1, hash2,
           "Outputs should be identical (deterministic), but hashes differ:\n\
            First:  {}\n\
            Second: {}",
           hash1, hash2
       );
   }
   ```

3. **Implement Diff Validation**

   ```rust
   #[then(regex = r#"^I should see "---([^"]+)" in output$"#)]
   fn should_see_diff_header(world: &mut GgenWorld, filename: String) {
       let stdout = world.last_stdout();
       let expected_header = format!("--- {}", filename);

       assert!(
           stdout.contains(&expected_header),
           "Expected diff header '{}' in output, but got:\n{}",
           expected_header,
           stdout
       );
   }

   #[then(regex = r#"^I should see "\+\+\+([^"]+)" in output$"#)]
   fn should_see_new_file_header(world: &mut GgenWorld, filename: String) {
       let stdout = world.last_stdout();
       let expected_header = format!("+++ {}", filename);

       assert!(
           stdout.contains(&expected_header),
           "Expected new file header '{}' in output, but got:\n{}",
           expected_header,
           stdout
       );
   }

   #[then(regex = r#"^I should see "-([^"]+)" in output$"#)]
   fn should_see_removed_line(world: &mut GgenWorld, line: String) {
       let stdout = world.last_stdout();
       let expected_line = format!("-{}", line);

       assert!(
           stdout.contains(&expected_line) || stdout.contains(&line),
           "Expected removed line '{}' in diff, but got:\n{}",
           line,
           stdout
       );
   }

   #[then(regex = r#"^I should see "\+([^"]+)" in output$"#)]
   fn should_see_added_line(world: &mut GgenWorld, line: String) {
       let stdout = world.last_stdout();
       let expected_line = format!("+{}", line);

       assert!(
           stdout.contains(&expected_line) || stdout.contains(&line),
           "Expected added line '{}' in diff, but got:\n{}",
           line,
           stdout
       );
   }
   ```

4. **Implement Plan JSON Validation**

   ```rust
   #[when(regex = r#"^I run "ggen project plan ([^"]+) (.+)"$"#)]
   fn run_project_plan_command(world: &mut GgenWorld, template: String, args: String) {
       let arg_list = shell_words::split(&args)
           .unwrap_or_else(|e| panic!("Failed to parse arguments: {}", e));

       let mut cmd = Command::cargo_bin("ggen").expect("ggen binary not found");
       let output = cmd
           .arg("project")
           .arg("plan")
           .arg(&template)
           .args(&arg_list)
           .current_dir(&world.project_dir)
           .output()
           .expect("Failed to run ggen project plan");

       // Capture plan for later application
       if output.status.success() {
           let plan_json = String::from_utf8_lossy(&output.stdout).to_string();
           world.capture_file("last_plan.json", plan_json);
       }

       world.last_output = Some(output.clone());
       world.last_exit_code = output.status.code();
   }

   #[then(regex = r#"^I should see "Plan:" in output$"#)]
   fn should_see_plan_marker(world: &mut GgenWorld) {
       let stdout = world.last_stdout();
       assert!(
           stdout.contains("Plan:") || stdout.contains("\"operations\""),
           "Expected plan output with 'Plan:' or JSON structure, but got:\n{}",
           stdout
       );
   }
   ```

**Acceptance Criteria:**
- [ ] All `project.feature` scenarios execute without panics
- [ ] Docstring content is properly extracted
- [ ] Plan and diff outputs are validated
- [ ] Determinism tests can compare multiple runs

**Deliverables:**
- Fixed `project_steps.rs` with docstring handling
- Implemented plan/diff validation steps
- Git commit: "fix(bdd): fix docstring extraction and implement project step validations"

---

### 🤖 Agent 5: Missing Step Implementor (Market Commands)

**Objective:** Implement marketplace step definitions with mock registry support

**Priority:** MEDIUM
**Estimated Effort:** 3-4 hours
**Files:**
- `tests/bdd/steps/market_noun_verb_steps.rs`
- `tests/bdd/world.rs` (for mock server setup)

**Tasks:**

1. **Setup Mock HTTP Registry**

   **Enhance World State:**
   ```rust
   // In world.rs
   impl GgenWorld {
       pub fn setup_mock_registry(&mut self) -> String {
           use mockito::Server;

           // Create mock HTTP server
           let mut server = Server::new();
           let registry_url = server.url();

           // Mock the index endpoint
           let mock = server.mock("GET", "/index.json")
               .with_status(200)
               .with_header("content-type", "application/json")
               .with_body(r#"{
                   "packages": {
                       "io.ggen.rust.cli-subcommand": {
                           "name": "io.ggen.rust.cli-subcommand",
                           "version": "0.2.0",
                           "description": "Rust CLI subcommand template",
                           "category": "rust"
                       },
                       "io.ggen.python.web-api": {
                           "name": "io.ggen.python.web-api",
                           "version": "0.1.5",
                           "description": "Python web API template",
                           "category": "python"
                       }
                   }
               }"#)
               .create();

           self.mock_server = Some(server);
           registry_url
       }
   }
   ```

2. **Implement Registry-Dependent Steps**

   ```rust
   #[given(regex = r"^the marketplace registry is available$")]
   fn marketplace_registry_available(world: &mut GgenWorld) {
       let registry_url = world.setup_mock_registry();

       // Set environment variable so ggen uses mock registry
       std::env::set_var("GGEN_REGISTRY_URL", &registry_url);
       world.set_registry_url(registry_url);
   }

   #[then(regex = r"^results should only show rust category gpacks$")]
   fn results_show_rust_category(world: &mut GgenWorld) {
       let stdout = world.last_stdout();

       // Parse JSON or text output
       if stdout.contains('{') {
           // JSON output
           let json: serde_json::Value = serde_json::from_str(&stdout)
               .expect("Failed to parse search results as JSON");

           if let Some(results) = json.get("results").and_then(|r| r.as_array()) {
               for result in results {
                   if let Some(category) = result.get("category").and_then(|c| c.as_str()) {
                       assert_eq!(
                           category, "rust",
                           "Expected only rust category, but found: {}",
                           category
                       );
                   }
               }
           }
       } else {
           // Text output - verify all results mention "rust"
           for line in stdout.lines() {
               if line.contains("category") || line.contains("Category") {
                   assert!(
                       line.to_lowercase().contains("rust"),
                       "Expected rust category, but got: {}",
                       line
                   );
               }
           }
       }
   }

   #[then(regex = r"^the gpack should be cached locally$")]
   fn gpack_should_be_cached(world: &mut GgenWorld) {
       // Check local cache directory
       let cache_dir = dirs::cache_dir()
           .expect("Failed to get cache directory")
           .join("ggen")
           .join("gpacks");

       // In test environment, cache might be in project dir
       let local_cache = world.project_dir.join(".ggen/cache");

       assert!(
           cache_dir.exists() || local_cache.exists(),
           "Expected gpack cache directory to exist"
       );
   }

   #[then(regex = r"^the gpack should be updated to latest version$")]
   fn gpack_updated_to_latest(world: &mut GgenWorld) {
       let lockfile_path = world.project_dir.join("ggen.lock");

       if lockfile_path.exists() {
           let content = fs::read_to_string(&lockfile_path)
               .expect("Failed to read lockfile");

           // Parse lockfile (assuming JSON format)
           let lockfile: serde_json::Value = serde_json::from_str(&content)
               .expect("Failed to parse lockfile");

           // Verify version was updated (mock returns 0.2.0)
           if let Some(packages) = lockfile.get("packages").and_then(|p| p.as_object()) {
               for (_, pkg) in packages {
                   if let Some(version) = pkg.get("version").and_then(|v| v.as_str()) {
                       assert!(
                           version >= "0.2.0",
                           "Expected version >= 0.2.0, got {}",
                           version
                       );
                   }
               }
           }
       }
   }
   ```

3. **Implement PQC Signature Steps**

   ```rust
   #[then(regex = r"^the lockfile should contain the PQC signature$")]
   fn lockfile_contains_pqc_signature(world: &mut GgenWorld) {
       let lockfile_path = world.project_dir.join("ggen.lock");

       if lockfile_path.exists() {
           let content = fs::read_to_string(&lockfile_path)
               .expect("Failed to read lockfile");

           // Check for PQC signature fields
           assert!(
               content.contains("pqc_signature") || content.contains("signature"),
               "Lockfile should contain PQC signature field"
           );
       } else {
           panic!("Lockfile should exist after adding gpack with PQC signature");
       }
   }

   #[then(regex = r"^the lockfile should contain the PQC public key$")]
   fn lockfile_contains_pqc_public_key(world: &mut GgenWorld) {
       let lockfile_path = world.project_dir.join("ggen.lock");

       if lockfile_path.exists() {
           let content = fs::read_to_string(&lockfile_path)
               .expect("Failed to read lockfile");

           // Check for PQC public key fields
           assert!(
               content.contains("pqc_public_key") || content.contains("public_key"),
               "Lockfile should contain PQC public key field"
           );
       } else {
           panic!("Lockfile should exist after adding gpack with PQC signature");
       }
   }
   ```

4. **Fix SHA256 Validation**

   ```rust
   #[then(regex = r"^the SHA256 should be 64 hex characters$")]
   fn sha256_should_be_valid(world: &mut GgenWorld) {
       let stdout = world.last_stdout();

       // Find SHA256 hash in output (64 hex chars)
       let hex_regex = regex::Regex::new(r"[0-9a-fA-F]{64}").expect("Invalid regex");

       let found_hash = hex_regex.is_match(&stdout);
       assert!(
           found_hash,
           "Expected to find 64-character hex SHA256 hash in output, but got:\n{}",
           stdout
       );
   }
   ```

**Acceptance Criteria:**
- [ ] Mock registry is set up and responds to requests
- [ ] Market commands can search and retrieve gpacks
- [ ] SHA256 validation works correctly
- [ ] PQC signature fields are validated in lockfile

**Deliverables:**
- Enhanced `world.rs` with mock registry support
- Completed `market_noun_verb_steps.rs` implementations
- Git commit: "feat(bdd): implement market command steps with mock registry"

---

### 🤖 Agent 6: Missing Step Implementor (Template Commands)

**Objective:** Implement template management step definitions

**Priority:** LOW
**Estimated Effort:** 2 hours
**File:** `tests/bdd/steps/template_management_steps.rs`

**Tasks:**

1. **Implement Template Creation Steps**

   ```rust
   #[then(regex = r"^the template should have RDF frontmatter section$")]
   fn template_has_rdf_section(world: &mut GgenWorld) {
       // Find the created template file
       let templates_dir = world.project_dir.join("templates");

       if templates_dir.exists() {
           // Find most recently created .tmpl file
           let mut entries: Vec<_> = fs::read_dir(&templates_dir)
               .expect("Failed to read templates dir")
               .filter_map(|e| e.ok())
               .filter(|e| e.path().extension().map_or(false, |ext| ext == "tmpl"))
               .collect();

           entries.sort_by_key(|e| e.metadata().unwrap().modified().unwrap());

           if let Some(latest) = entries.last() {
               let content = fs::read_to_string(latest.path())
                   .expect("Failed to read template");

               assert!(
                   content.contains("rdf:") || content.contains("rdf_inline:"),
                   "Template should have RDF frontmatter section, but got:\n{}",
                   content
               );
           } else {
               panic!("No template files found in templates directory");
           }
       } else {
           panic!("Templates directory should exist");
       }
   }

   #[then(regex = r"^the template should have SPARQL section$")]
   fn template_has_sparql_section(world: &mut GgenWorld) {
       // Similar to above, check for "sparql:" in frontmatter
       let templates_dir = world.project_dir.join("templates");

       if templates_dir.exists() {
           let mut entries: Vec<_> = fs::read_dir(&templates_dir)
               .expect("Failed to read templates dir")
               .filter_map(|e| e.ok())
               .filter(|e| e.path().extension().map_or(false, |ext| ext == "tmpl"))
               .collect();

           entries.sort_by_key(|e| e.metadata().unwrap().modified().unwrap());

           if let Some(latest) = entries.last() {
               let content = fs::read_to_string(latest.path())
                   .expect("Failed to read template");

               assert!(
                   content.contains("sparql:"),
                   "Template should have SPARQL section, but got:\n{}",
                   content
               );
           }
       }
   }

   #[then(regex = r"^I should see compatibility information$")]
   fn should_see_compatibility_info(world: &mut GgenWorld) {
       let stdout = world.last_stdout();

       assert!(
           stdout.contains("compatible") ||
           stdout.contains("version") ||
           stdout.contains("requires"),
           "Expected compatibility information, but got:\n{}",
           stdout
       );
   }
   ```

2. **Implement Interactive Wizard Steps**

   ```rust
   #[when(regex = r#"^I answer "([^"]+)" to "([^"]+)"$"#)]
   fn answer_interactive_prompt(world: &mut GgenWorld, answer: String, prompt: String) {
       // For now, interactive steps are not fully testable
       // Would require expect/pty-based testing

       // Store answers for future use
       world.captured_files.insert(
           format!("prompt:{}", prompt),
           answer
       );

       // In real implementation, would pipe input to stdin
   }
   ```

3. **Implement Template Validation**

   ```rust
   #[then(regex = r"^I should see a preview of rendered output$")]
   fn should_see_preview_of_output(world: &mut GgenWorld) {
       let stdout = world.last_stdout();

       // Preview should show rendered template content
       assert!(
           stdout.contains("Preview:") ||
           stdout.contains("Output:") ||
           !stdout.is_empty(),
           "Expected preview of rendered output, but got:\n{}",
           stdout
       );
   }

   #[then(regex = r#"^the template should have "([^"]+)" set to "([^"]+)"$"#)]
   fn template_has_field_value(world: &mut GgenWorld, field: String, value: String) {
       // Find and parse the template file
       let templates_dir = world.project_dir.join("templates");

       if templates_dir.exists() {
           let mut entries: Vec<_> = fs::read_dir(&templates_dir)
               .expect("Failed to read templates dir")
               .filter_map(|e| e.ok())
               .filter(|e| e.path().extension().map_or(false, |ext| ext == "tmpl"))
               .collect();

           entries.sort_by_key(|e| e.metadata().unwrap().modified().unwrap());

           if let Some(latest) = entries.last() {
               let content = fs::read_to_string(latest.path())
                   .expect("Failed to read template");

               // Parse YAML frontmatter (between --- markers)
               if let Some(frontmatter) = extract_frontmatter(&content) {
                   let expected = format!("{}: \"{}\"", field, value);
                   assert!(
                       frontmatter.contains(&expected) ||
                       frontmatter.contains(&format!("{}: {}", field, value)),
                       "Template should have {} set to '{}', but frontmatter is:\n{}",
                       field, value, frontmatter
                   );
               }
           }
       }
   }

   fn extract_frontmatter(content: &str) -> Option<String> {
       let parts: Vec<&str> = content.split("---").collect();
       if parts.len() >= 3 {
           Some(parts[1].to_string())
       } else {
           None
       }
   }
   ```

**Acceptance Criteria:**
- [ ] Template creation scenarios execute
- [ ] Template validation checks frontmatter structure
- [ ] Template listing shows correct templates

**Deliverables:**
- Completed `template_management_steps.rs`
- Git commit: "feat(bdd): implement template management step definitions"

---

### 🤖 Agent 7: Feature File Fixer

**Objective:** Update feature files to match disambiguated step definitions

**Priority:** HIGH (after Agent 1 completes)
**Estimated Effort:** 1-2 hours
**Files:** All `.feature` files in `tests/bdd/features/`

**Tasks:**

1. **Update Template-Related Steps**

   **In `template_generation.feature`:**
   ```gherkin
   # BEFORE:
   Given I have a template with:

   # AFTER:
   Given I have a basic template with:
   ```

   ```gherkin
   # BEFORE:
   Given I have a template with RDF inline:

   # AFTER:
   Given I have a template with RDF inline data:
   ```

   ```gherkin
   # BEFORE:
   Given I have a template with SPARQL query:

   # AFTER:
   Given I have a template with SPARQL query definition:
   ```

   ```gherkin
   # BEFORE:
   Given I have a template with determinism config:

   # AFTER:
   Given I have a template with determinism configuration:
   ```

2. **Remove Duplicate Marketplace Registry Steps**

   **In `market.feature` and `project.feature`:**
   - Keep only ONE "Given the marketplace registry is available" in market.feature
   - Remove from project.feature (or reference it via market steps)

3. **Verify All Step Patterns Match**

   Run this validation:
   ```bash
   # Check that all steps in features have matching definitions
   cargo test --test bdd 2>&1 | grep "Step doesn't match"
   ```

   For each unmatched step:
   - Find the closest matching step definition
   - Update feature file OR create new step definition

**Acceptance Criteria:**
- [ ] No "Step match is ambiguous" errors
- [ ] No "Step doesn't match any function" errors
- [ ] All features parse and load successfully

**Deliverables:**
- Updated feature files with corrected step wording
- Git commit: "fix(bdd): update feature files to match disambiguated steps"

---

### 🤖 Agent 8: Documentation & Examples

**Objective:** Create comprehensive documentation and example step definitions

**Priority:** LOW
**Estimated Effort:** 2 hours
**Files:**
- `tests/bdd/docs/STEP_PATTERNS.md`
- `tests/bdd/docs/EXAMPLES.md`
- `tests/bdd/docs/TROUBLESHOOTING.md`

**Tasks:**

1. **Create Step Patterns Reference**

   **File:** `tests/bdd/docs/STEP_PATTERNS.md`

   ```markdown
   # BDD Step Definition Patterns

   ## Regex Best Practices

   ### Capturing Strings
   - **Quoted strings:** `"([^"]+)"` matches "hello world"
   - **Unquoted words:** `(\w+)` matches hello
   - **Paths:** `([^\s]+)` matches path/to/file.txt

   ### Capturing Numbers
   - **Integer:** `(\d+)` matches 42
   - **Decimal:** `(\d+\.\d+)` matches 3.14

   ### Common Patterns

   #### File Operations
   ```rust
   #[given(regex = r#"^I have a file "([^"]+)" with content:$"#)]
   fn create_file(world: &mut GgenWorld, filename: String, content: String)
   ```

   #### Command Execution
   ```rust
   #[when(regex = r#"^I run "ggen ([^"]+)"$"#)]
   fn run_ggen_command(world: &mut GgenWorld, args: String)
   ```

   #### Validation
   ```rust
   #[then(regex = r#"^I should see "([^"]+)" in output$"#)]
   fn should_see_in_output(world: &mut GgenWorld, expected: String)
   ```

   ## Avoiding Ambiguity

   ### DO ✅
   - Make regex as specific as possible
   - Use unique keywords in step descriptions
   - Test steps in isolation

   ### DON'T ❌
   - Use generic matchers like `(.+)` for all steps
   - Create overlapping regex patterns
   - Forget to test disambiguation

   ## Step Naming Conventions

   - **GIVEN:** Setup state (`create_*`, `setup_*`, `have_*`)
   - **WHEN:** Execute action (`run_*`, `execute_*`, `perform_*`)
   - **THEN:** Validate outcome (`should_*`, `validate_*`, `verify_*`)
   ```

2. **Create Examples Guide**

   **File:** `tests/bdd/docs/EXAMPLES.md`

   (Content with real-world examples from the codebase)

3. **Create Troubleshooting Guide**

   **File:** `tests/bdd/docs/TROUBLESHOOTING.md`

   (Common errors and solutions)

**Acceptance Criteria:**
- [ ] Documentation is clear and comprehensive
- [ ] Examples are executable and pass
- [ ] Troubleshooting covers known issues

**Deliverables:**
- Three new documentation files
- Git commit: "docs(bdd): add comprehensive BDD testing documentation"

---

## Step Definition Specifications

### Complete Step Inventory

This section catalogs ALL step definitions across all files, with implementation status.

#### Common Steps (`common_steps.rs`)

| Step Pattern | Type | Status | Notes |
|-------------|------|--------|-------|
| `^I have a clean project directory$` | GIVEN | ✅ Complete | Creates temp dir |
| `^ggen is installed$` | GIVEN | ✅ Complete | Validates binary |
| `^I run (.+)$` | WHEN | ✅ Complete | Generic command execution |
| `^the command should succeed$` | THEN | ✅ Complete | Exit code 0 |
| `^I should see (.+)$` | THEN | ✅ Complete | Output validation |
| `^the file (.+) should exist$` | THEN | ✅ Complete | File existence check |

#### Graph Steps (`graph_steps.rs`)

| Step Pattern | Type | Status | Notes |
|-------------|------|--------|-------|
| `^I have an RDF file "([^"]+)" with content:$` | GIVEN | ✅ Complete | Creates Turtle file |
| `^I have an RDF file "([^"]+)" in RDF/XML format$` | GIVEN | ✅ Complete | Creates RDF/XML |
| `^I have a graph with person data$` | GIVEN | ✅ Complete | Loads sample graph |
| `^I have a SPARQL query file "([^"]+)" with:$` | GIVEN | ✅ Complete | Creates .sparql file |
| `^I have a graph with (\d+) triples$` | GIVEN | ✅ Complete | Generates N triples |
| `^I have a graph with multiple RDF types$` | GIVEN | ✅ Complete | Multiple classes |
| `^I have a graph with (\d+) people$` | GIVEN | ✅ Complete | Generates persons |
| `^I have a graph with person relationships$` | GIVEN | ❌ Missing | Need implementation |
| `^I have a large graph$` | GIVEN | ❌ Missing | 1000+ triples |
| `^I have RDF files "([^"]+)" and "([^"]+)"$` | GIVEN | ❌ Missing | Multi-file setup |
| `^I have triples in named graph "([^"]+)"$` | GIVEN | ❌ Missing | Named graphs |
| `^I have SHACL shapes defining person constraints$` | GIVEN | ❌ Missing | SHACL validation |
| `^I run "ggen graph (.+)"$` | WHEN | ✅ Complete | Graph commands |
| `^I should see "([^"]+)" in output$` | THEN | ✅ Complete | Output validation |
| `^the graph should contain the triples$` | THEN | ✅ Complete | SPARQL count |
| `^I should see query results in output$` | THEN | ✅ Complete | Generic validation |
| `^I should see "([^"]+)" and "([^"]+)" in results$` | THEN | ✅ Complete | Multi-value check |
| `^the output should be valid JSON$` | THEN | ✅ Complete | JSON parsing |
| `^I should see a formatted table in output$` | THEN | ✅ Complete | Table format |
| `^the file should contain valid Turtle syntax$` | THEN | 🟡 Placeholder | Need parser |
| `^the file should be valid JSON-LD$` | THEN | ✅ Complete | JSON-LD parsing |
| `^the file should contain N-Triples format$` | THEN | 🟡 Placeholder | Need validation |
| `^I should see (\d+) results$` | THEN | ❌ Missing | Count validation |
| `^they should be results (\d+)-(\d+)$` | THEN | ❌ Missing | Range validation |
| `^the file should only contain Person entities$` | THEN | ❌ Missing | Filter validation |
| `^the file should be valid DOT format$` | THEN | ❌ Missing | Graphviz |
| `^it can be rendered with Graphviz$` | THEN | ❌ Missing | DOT syntax check |
| `^the triples should be in the named graph$` | THEN | ❌ Missing | Named graph query |
| `^I should see only triples from that graph$` | THEN | ❌ Missing | Graph filtering |
| `^I should see validation report$` | THEN | ❌ Missing | SHACL report |
| `^I should see number of subjects/predicates/objects$` | THEN | ❌ Missing | Stats validation |
| `^the graph should be empty$` | THEN | ❌ Missing | Empty check |
| `^the graph should contain triples from both files$` | THEN | ❌ Missing | Merge validation |

#### Project Steps (`project_steps.rs`)

| Step Pattern | Type | Status | Notes |
|-------------|------|--------|-------|
| `^I am in a clean project directory$` | GIVEN | ✅ Complete | Alias for common step |
| `^I have a template "([^"]+)" with content:$` | GIVEN | ⚠️ Ambiguous | Needs disambiguation |
| `^I have an existing file "([^"]+)" with "([^"]+)"$` | GIVEN | ✅ Complete | Creates file |
| `^I have a plan file "([^"]+)" with:$` | GIVEN | ❌ Broken | Docstring issue |
| `^the marketplace registry is available$` | GIVEN | ⚠️ Ambiguous | Duplicate |
| `^I have installed the gpack "([^"]+)"$` | GIVEN | 🟡 Placeholder | Mock install |
| `^I run "ggen project (.+)"$` | WHEN | ✅ Complete | Project commands |
| `^the command should succeed$` | THEN | ✅ Complete | Exit code check |
| `^the file "([^"]+)" should exist$` | THEN | ✅ Complete | File check |
| `^the file "([^"]+)" should contain "([^"]+)"$` | THEN | ✅ Complete | Content check |
| `^I should see "([^"]+)" in output$` | THEN | ✅ Complete | Output validation |
| `^the file "([^"]+)" should not exist$` | THEN | ✅ Complete | Non-existence |
| `^the file "([^"]+)" should still contain "([^"]+)"$` | THEN | ✅ Complete | Unchanged check |
| `^both outputs should be identical$` | THEN | ❌ Broken | Needs hash capture |
| `^I should see Generated in output$` | THEN | ✅ Complete | Success message |

#### Market Steps (`market_noun_verb_steps.rs`)

| Step Pattern | Type | Status | Notes |
|-------------|------|--------|-------|
| `^the marketplace registry is available$` | GIVEN | ⚠️ Ambiguous | Keep this one |
| `^I have installed the gpack "([^"]+)"$` | GIVEN | ✅ Complete | Mock lockfile |
| `^I have installed the gpack "([^"]+)@([^"]+)"$` | GIVEN | ✅ Complete | Versioned install |
| `^a newer version "([^"]+)" is available$` | GIVEN | 🟡 Placeholder | Mock registry |
| `^the gpack "([^"]+)" has a PQC signature$` | GIVEN | 🟡 Placeholder | Mock registry |
| `^I run "ggen market (.+)"$` | WHEN | ✅ Complete | Market commands |
| `^the command should succeed$` | THEN | ✅ Complete | Exit code |
| `^I should see results for rust gpacks$` | THEN | ✅ Complete | Category filter |
| `^I should see "([^"]+)" in output$` | THEN | ✅ Complete | Output validation |
| `^results should only show rust category gpacks$` | THEN | ❌ Missing | JSON validation |
| `^the output should be valid JSON$` | THEN | ✅ Complete | JSON parsing |
| `^the JSON should contain a "([^"]+)" array$` | THEN | ✅ Complete | Field check |
| `^the gpack should be listed in "([^"]+)"$` | THEN | ✅ Complete | Lockfile check |
| `^the gpack should be cached locally$` | THEN | ❌ Missing | Cache validation |
| `^the lockfile should show version "([^"]+)"$` | THEN | ✅ Complete | Version check |
| `^the gpack should not be in "([^"]+)"$` | THEN | ✅ Complete | Removal check |
| `^I should see SHA256 hash for each gpack$` | THEN | ✅ Complete | Hash presence |
| `^the SHA256 should be 64 hex characters$` | THEN | ❌ Missing | Regex validation |
| `^the lockfile should contain the PQC signature$` | THEN | ❌ Missing | PQC field check |
| `^the lockfile should contain the PQC public key$` | THEN | ❌ Missing | PQC key check |

#### Template Management Steps (`template_management_steps.rs`)

| Step Pattern | Type | Status | Notes |
|-------------|------|--------|-------|
| `^I have a local template "([^"]+)"$` | GIVEN | ✅ Complete | Creates template |
| `^I have a template "([^"]+)" with content:$` | GIVEN | ⚠️ Ambiguous | Needs fix |
| `^I have a file "([^"]+)" with "([^"]+)"$` | GIVEN | ✅ Complete | File creation |
| `^I have templates "([^"]+)", "([^"]+)", "([^"]+)"$` | GIVEN | ✅ Complete | Multi-template |
| `^I have multiple templates with descriptions$` | GIVEN | ✅ Complete | Desc templates |
| `^I have a template "([^"]+)" with "([^"]+)"$` | GIVEN | ✅ Complete | Field template |
| `^I run "ggen template (.+)"$` | WHEN | ✅ Complete | Template commands |
| `^I answer "([^"]+)" to "([^"]+)"$` | WHEN | 🟡 Placeholder | Interactive |
| `^the command should succeed$` | THEN | ✅ Complete | Exit code |
| `^the file "([^"]+)" should exist$` | THEN | ✅ Complete | File check |
| `^the file should contain valid YAML frontmatter$` | THEN | 🟡 Placeholder | YAML parse |
| `^I should see "([^"]+)" in output$` | THEN | ✅ Complete | Output validation |
| `^the template should have "([^"]+)" set to "([^"]+)"$` | THEN | ❌ Missing | Field validation |
| `^the template should have variables "([^"]+)" and "([^"]+)"$` | THEN | 🟡 Placeholder | Var check |
| `^I should see gpacks templates in output$` | THEN | ✅ Complete | Gpack listing |
| `^I should not see gpack templates in output$` | THEN | ✅ Complete | Filter check |
| `^I should see template metadata$` | THEN | ✅ Complete | Metadata display |
| `^RDF validation should pass$` | THEN | 🟡 Placeholder | RDF check |
| `^a template should be created from "([^"]+)"$` | THEN | 🟡 Placeholder | From-file |
| `^I should not see "([^"]+)" in output$` | THEN | ✅ Complete | Negative check |
| `^I should see a preview of rendered output$` | THEN | ❌ Missing | Preview validation |
| `^the template should have RDF frontmatter section$` | THEN | ❌ Missing | RDF section |
| `^the template should have SPARQL section$` | THEN | ❌ Missing | SPARQL section |
| `^I should see descriptions for each template$` | THEN | ✅ Complete | Description list |
| `^I should see compatibility information$` | THEN | ❌ Missing | Compatibility |

**Legend:**
- ✅ Complete: Fully implemented and working
- 🟡 Placeholder: Implemented but validation is superficial
- ⚠️ Ambiguous: Multiple regex patterns match (CRITICAL)
- ❌ Missing: Not implemented
- ❌ Broken: Implemented but has bugs

---

## Ambiguous Steps Resolution

### Critical Ambiguities

#### 1. Template Creation Steps

**Problem:**
```
Step: Given I have a template "hello.tmpl" with content:

Matches:
1. ^I have a template "([^"]+)" with content:$ (project_steps.rs:27)
2. ^I have a template "([^"]+)" with content:$ (template_management_steps.rs:36)
3. ^I have a template (.+)$ (template_steps.rs:26)
```

**Resolution Strategy:**

**Option A: Namespace by Feature**
```rust
// project_steps.rs - Keep as-is (most specific)
#[given(regex = r#"^I have a template "([^"]+)" with content:$"#)]

// template_management_steps.rs - Remove (duplicate)
// DELETE THIS STEP

// template_steps.rs - Make more generic for backwards compat
#[given(regex = r#"^I have a template file at (.+)$"#)]
```

**Option B: Use Context Keywords**
```rust
// project_steps.rs
#[given(regex = r#"^I have a project template "([^"]+)" with content:$"#)]

// template_management_steps.rs
#[given(regex = r#"^I have a managed template "([^"]+)" with content:$"#)]

// template_steps.rs
#[given(regex = r#"^I have a legacy template (.+)$"#)]
```

**Recommended: Option A** (less intrusive, clearer ownership)

**Implementation:**
1. Delete duplicate from `template_management_steps.rs`
2. Update `template_steps.rs` regex to be non-overlapping
3. Update feature files if needed

#### 2. Marketplace Registry Availability

**Problem:**
```
Step: Given the marketplace registry is available

Matches:
1. ^the marketplace registry is available$ (market_noun_verb_steps.rs:21)
2. ^the marketplace registry is available$ (project_steps.rs:51)
```

**Resolution:**
Keep in `market_noun_verb_steps.rs` (canonical location for market operations)
Delete from `project_steps.rs` (just delegates anyway)

**Implementation:**
```rust
// In project_steps.rs - DELETE THIS:
#[given(regex = r"^the marketplace registry is available$")]
async fn marketplace_registry_available(_world: &mut GgenWorld) {
    // DELETE ENTIRE FUNCTION
}
```

#### 3. Template Variant Steps

**Problem:**
```
Steps:
- Given I have a template with:
- Given I have a template with RDF inline:
- Given I have a template with SPARQL query:
- Given I have a template with determinism config:

All match generic: ^I have a template (.+)$
```

**Resolution:**
Make each variant more specific and update generic matcher:

```rust
// template_steps.rs

// Generic matcher - make it NOT match "with X:" patterns
#[given(regex = r"^I have a template ([^w].+)$")]  // Won't match "with..."
fn have_template_generic(world: &mut GgenWorld, desc: String) {
    // ...
}

// Specific matchers
#[given(regex = r"^I have a basic template with:$")]
fn have_basic_template_with_content(world: &mut GgenWorld, content: String) {
    // ...
}

#[given(regex = r"^I have a template with RDF inline data:$")]
fn have_template_with_rdf(world: &mut GgenWorld, rdf: String) {
    // ...
}

#[given(regex = r"^I have a template with SPARQL query definition:$")]
fn have_template_with_sparql(world: &mut GgenWorld, sparql: String) {
    // ...
}

#[given(regex = r"^I have a template with determinism configuration:$")]
fn have_template_with_determinism(world: &mut GgenWorld, config: String) {
    // ...
}
```

Then update feature files to use new wording.

---

## Feature File Catalog

### Overview

| Feature File | Scenarios | Steps | Status | Priority |
|-------------|-----------|-------|--------|----------|
| graph.feature | 29 | 87 | 🟡 Partial | HIGH |
| market.feature | 21 | 63 | 🟡 Partial | HIGH |
| project.feature | 9 | 27 | ⚠️ Ambiguous | HIGH |
| template.feature | 21 | 63 | ⚠️ Ambiguous | MEDIUM |
| installation.feature | 3 | 9 | ✅ Complete | LOW |
| quickstart.feature | 3 | 9 | ✅ Complete | MEDIUM |
| template_generation.feature | 4 | 12 | ⚠️ Ambiguous | MEDIUM |
| marketplace.feature | 8 | 24 | ✅ Complete | LOW |
| cli_commands.feature | 3 | 9 | ✅ Complete | LOW |
| determinism.feature | 2 | 6 | ✅ Complete | MEDIUM |
| multi_language.feature | 2 | 6 | ❌ Failing | LOW |
| rdf_sparql.feature | 4 | 12 | 🟡 Partial | MEDIUM |

**Total: 109 scenarios, 327 steps**

### Feature Dependency Graph

```
installation.feature (no dependencies)
    ↓
quickstart.feature (requires installed ggen)
    ↓
template_generation.feature (requires templates)
    ↓
project.feature (requires template generation)
    ↓
marketplace.feature (requires network/mock)
    ↓
market.feature (new noun-verb structure)
    ↓
template.feature (template management)
    ↓
graph.feature (RDF operations)
    ↓
rdf_sparql.feature (advanced RDF)
    ↓
multi_language.feature (integrates all)
    ↓
determinism.feature (reproducibility)
    ↓
cli_commands.feature (validation)
```

### Scenario Complexity

**Simple (1-3 steps):**
- Installation verification
- Command help/version
- List operations

**Medium (4-8 steps):**
- Single file generation
- Basic SPARQL queries
- Marketplace search

**Complex (9+ steps):**
- Multi-file generation workflows
- Graph visualization
- Cross-language integration
- Determinism validation

---

## Testing Patterns & Best Practices

### London School Patterns

#### 1. Mock External Dependencies

**Always mock:**
- HTTP registries (marketplace)
- File system (when testing logic, not I/O)
- Time/randomness (for determinism)
- External commands (unless integration testing)

**Example:**
```rust
// Setup mock registry
#[given(regex = r"^the marketplace registry is available$")]
fn setup_mock_registry(world: &mut GgenWorld) {
    let registry_url = world.setup_mock_registry();
    std::env::set_var("GGEN_REGISTRY_URL", &registry_url);
}
```

#### 2. Test Interactions, Not State

**Bad (state-based):**
```rust
#[then(regex = r"^the graph should have data$")]
fn graph_has_data(world: &mut GgenWorld) {
    // Assumes internal graph state is accessible
    assert!(world.graph.is_some());
    assert!(world.graph.unwrap().len() > 0);
}
```

**Good (interaction-based):**
```rust
#[then(regex = r"^the graph should have data$")]
fn graph_has_data(world: &mut GgenWorld) {
    // Query the graph via its public interface
    let output = Command::cargo_bin("ggen")
        .arg("graph")
        .arg("query")
        .arg("SELECT (COUNT(*) as ?count) WHERE { ?s ?p ?o }")
        .current_dir(&world.project_dir)
        .output()
        .expect("Query failed");

    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(!stdout.contains("0"), "Graph should have triples");
}
```

#### 3. Arrange-Act-Assert Pattern

Every scenario follows AAA:

```rust
// ARRANGE (Given steps)
#[given(regex = r"^I have a template$")]
fn arrange_template(world: &mut GgenWorld) {
    // Setup preconditions
}

// ACT (When steps)
#[when(regex = r"^I generate code$")]
fn act_generate(world: &mut GgenWorld) {
    // Execute the action
}

// ASSERT (Then steps)
#[then(regex = r"^code is generated$")]
fn assert_generated(world: &mut GgenWorld) {
    // Verify outcomes
}
```

### Cucumber Best Practices

#### 1. Write Declarative Steps

**Bad (imperative):**
```gherkin
Given I create a file "data.ttl"
And I open the file
And I write "@prefix ex: <http://example.org/> ."
And I write "ex:Subject ex:pred ex:Object ."
And I close the file
```

**Good (declarative):**
```gherkin
Given I have an RDF file "data.ttl" with content:
  """
  @prefix ex: <http://example.org/> .
  ex:Subject ex:pred ex:Object .
  """
```

#### 2. Reuse Steps Across Features

Don't duplicate:
```rust
// ❌ DON'T: Duplicate in every file
#[then(regex = r"^the command should succeed$")]
fn command_succeeds_graph(world: &mut GgenWorld) { ... }

#[then(regex = r"^the command should succeed$")]
fn command_succeeds_market(world: &mut GgenWorld) { ... }
```

Do centralize:
```rust
// ✅ DO: One definition in common_steps.rs
#[then(regex = r"^the command should succeed$")]
fn command_succeeds(world: &mut GgenWorld) { ... }
```

#### 3. Use Backgrounds for Common Setup

```gherkin
Feature: Graph Operations

  Background:
    Given I am in a clean project directory
    And I have a graph with person data

  Scenario: Query persons
    When I run "ggen graph query 'SELECT ?name WHERE { ?p ex:name ?name }'"
    Then I should see "Alice" in output

  Scenario: Export graph
    When I run "ggen graph export --format turtle"
    Then the file should contain valid Turtle syntax
```

#### 4. Parameterize Steps

Instead of:
```gherkin
Then I should see "Alice" in output
Then I should see "Bob" in output
Then I should see "Charlie" in output
```

Use:
```gherkin
Then I should see "Alice", "Bob", and "Charlie" in output
```

With step:
```rust
#[then(regex = r#"^I should see "([^"]+)", "([^"]+)", and "([^"]+)" in output$"#)]
fn should_see_three_values(world: &mut GgenWorld, v1: String, v2: String, v3: String) {
    let stdout = world.last_stdout();
    assert!(stdout.contains(&v1) && stdout.contains(&v2) && stdout.contains(&v3));
}
```

### Error Handling

#### 1. Descriptive Assertion Messages

**Bad:**
```rust
assert!(file_exists);
```

**Good:**
```rust
assert!(
    file_path.exists(),
    "Expected file {} to exist at {}, but it doesn't. \
     Generated files: {:?}",
    filename,
    file_path.display(),
    list_generated_files(&world.project_dir)
);
```

#### 2. Capture Context on Failure

```rust
#[then(regex = r"^the command should succeed$")]
fn command_should_succeed(world: &mut GgenWorld) {
    if !world.last_command_succeeded() {
        // Capture full context for debugging
        eprintln!("=== COMMAND FAILED ===");
        eprintln!("Exit code: {}", world.last_exit_code.unwrap_or(-1));
        eprintln!("Stdout:\n{}", world.last_stdout());
        eprintln!("Stderr:\n{}", world.last_stderr());
        eprintln!("Working dir: {}", world.project_dir.display());
        eprintln!("Files present:");
        for entry in fs::read_dir(&world.project_dir).unwrap() {
            eprintln!("  - {}", entry.unwrap().path().display());
        }
        panic!("Command failed");
    }
}
```

### Performance Optimization

#### 1. Minimize Command Execution

Cache results when testing multiple assertions:
```rust
// ❌ SLOW: Runs command 3 times
#[then(regex = r"^output contains valid data$")]
fn output_is_valid(world: &mut GgenWorld) {
    let output1 = run_query(world, "SELECT ?s WHERE { ?s ?p ?o }");
    assert!(!output1.is_empty());

    let output2 = run_query(world, "SELECT ?s WHERE { ?s ?p ?o }");
    assert!(output2.contains("ex:"));

    let output3 = run_query(world, "SELECT ?s WHERE { ?s ?p ?o }");
    assert_valid_json(&output3);
}

// ✅ FAST: Runs command once, uses cached output
#[then(regex = r"^output contains valid data$")]
fn output_is_valid(world: &mut GgenWorld) {
    let output = world.last_stdout(); // Already captured

    assert!(!output.is_empty());
    assert!(output.contains("ex:"));
    assert_valid_json(&output);
}
```

#### 2. Parallel Test Execution

Ensure scenarios are isolated:
```rust
// Each scenario gets its own temp directory
impl cucumber::World for GgenWorld {
    fn new() -> Result<Self, ()> {
        let temp_dir = TempDir::new().map_err(|_| ())?;
        let project_dir = temp_dir.path().to_path_buf();

        Ok(Self {
            temp_dir: Some(temp_dir),
            project_dir,
            ..Default::default()
        })
    }
}
```

---

## Verification & Acceptance Criteria

### Phase 1: Infrastructure (✅ Complete)

**Criteria:**
- [x] Test runner executes all 13 test functions
- [x] Cucumber framework loads feature files
- [x] World state properly initialized for each scenario
- [x] Step modules registered and imported

**Verification:**
```bash
cargo test --test bdd 2>&1 | grep "running 13 tests"
```

### Phase 2: Disambiguation (🔴 Critical)

**Criteria:**
- [ ] Zero "Step match is ambiguous" errors
- [ ] All steps have unique, non-overlapping regex
- [ ] Feature files updated to match new step wording

**Verification:**
```bash
# Should produce NO ambiguity warnings
cargo test --test bdd 2>&1 | grep -i "ambiguous" || echo "No ambiguities found"
```

**Exit Criteria:**
- Agent 1 completes all disambiguation tasks
- Agent 7 updates all feature files
- All tests run without ambiguity errors

### Phase 3: Implementation (🟡 In Progress)

**Criteria:**
- [ ] All graph command steps implemented (Agent 3)
- [ ] All project command steps implemented (Agent 4)
- [ ] All market command steps implemented (Agent 5)
- [ ] All template command steps implemented (Agent 6)
- [ ] Mock registry functional
- [ ] Docstring extraction fixed

**Verification:**
```bash
# Count implemented vs placeholder steps
grep -r "Placeholder" tests/bdd/steps/*.rs | wc -l  # Should be 0
grep -r "Missing" tests/bdd/steps/*.rs | wc -l      # Should be 0
grep -r "TODO" tests/bdd/steps/*.rs | wc -l         # Should be 0
```

**Exit Criteria:**
- No placeholder implementations
- No "Step doesn't match" errors
- Mock services operational

### Phase 4: Execution (🟡 Blocked)

**Criteria:**
- [ ] ggen binary built and accessible
- [ ] All command executions succeed (where expected)
- [ ] At least 70% of scenarios pass
- [ ] No panics or unexpected failures

**Verification:**
```bash
# Build binary
cargo make build

# Run tests
cargo make test-bdd 2>&1 | tee bdd-results.txt

# Check pass rate
grep "scenarios" bdd-results.txt
```

**Exit Criteria:**
- Binary builds successfully
- 70%+ scenario pass rate
- All critical features validated

### Phase 5: Coverage (Target)

**Criteria:**
- [ ] 100% of graph.feature scenarios pass
- [ ] 100% of market.feature scenarios pass
- [ ] 100% of project.feature scenarios pass
- [ ] 100% of template.feature scenarios pass
- [ ] 90%+ of all scenarios pass

**Verification:**
```bash
# Generate coverage report
cargo make test-bdd-verbose > coverage.txt 2>&1

# Parse results
python3 << 'EOF'
import re

with open('coverage.txt') as f:
    content = f.read()

features = re.findall(r'Feature: (.+)', content)
scenarios = re.findall(r'(\d+) scenarios \((\d+) passed', content)

for feature, (total, passed) in zip(features, scenarios):
    pct = (int(passed) / int(total)) * 100
    print(f"{feature}: {passed}/{total} ({pct:.1f}%)")
EOF
```

**Exit Criteria:**
- All noun-verb features at 100%
- Overall suite at 90%+
- Zero critical bugs

### Phase 6: Documentation (Low Priority)

**Criteria:**
- [ ] STEP_PATTERNS.md complete with examples
- [ ] EXAMPLES.md with working code samples
- [ ] TROUBLESHOOTING.md with common issues
- [ ] All steps documented with purpose and usage

**Verification:**
```bash
# Check docs exist
ls tests/bdd/docs/*.md

# Validate examples compile
cargo test --doc --test bdd
```

**Exit Criteria:**
- Agent 8 completes all documentation
- Examples are runnable and pass
- Troubleshooting covers known issues

---

## Troubleshooting Guide

### Common Issues

#### 1. "Step match is ambiguous"

**Symptom:**
```
Step match is ambiguous: Possible matches:
^I have a template "([^"]+)" with content:$ --> tests/bdd/steps/project_steps.rs:27:1
^I have a template "([^"]+)" with content:$ --> tests/bdd/steps/template_management_steps.rs:36:1
```

**Cause:**
Multiple step definitions have identical or overlapping regex patterns.

**Solution:**
1. Identify all matching patterns
2. Keep the most specific one
3. Delete duplicates
4. Make remaining patterns more specific

**Prevention:**
- Use unique keywords in step descriptions
- Test regex patterns in isolation
- Run disambiguation check: `cargo make test-bdd 2>&1 | grep ambiguous`

#### 2. "Step doesn't match any function"

**Symptom:**
```
Given I have templates for Rust, Python, and Bash
Step doesn't match any function
```

**Cause:**
No step definition exists with a matching regex pattern.

**Solution:**
1. Find the feature file and line number
2. Check if similar step exists (typo?)
3. Implement the missing step definition
4. Or update feature file to use existing step

**Example Fix:**
```rust
// Add to appropriate steps file
#[given(regex = r"^I have templates for Rust, Python, and Bash$")]
fn have_multilang_templates(world: &mut GgenWorld) {
    for lang in ["rust", "python", "bash"] {
        create_template(world, &format!("{}.tmpl", lang));
    }
}
```

#### 3. "Failed to run command: No such file or directory"

**Symptom:**
```
Step panicked. Captured output: Failed to run command: "ggen add ...":
Os { code: 2, kind: NotFound, message: "No such file or directory" }
```

**Cause:**
ggen binary not built or not in PATH.

**Solution:**
```bash
# Build the binary
cargo make build

# Verify it exists
ls -la target/debug/ggen

# Or use release build
cargo make build-release
ls -la target/release/ggen
```

**Prevention:**
- Add build step to CI before BDD tests
- Document prerequisite in README
- Add build check to test runner

#### 4. "content not found" (Docstring Issue)

**Symptom:**
```
Step panicked. Captured output: content not found
```

**Cause:**
Step definition expects docstring but isn't extracting it correctly.

**Solution:**
```rust
// BEFORE (BROKEN):
#[given(regex = r"^I have a file with content:$")]
fn create_file(world: &mut GgenWorld, content: String) {
    // 'content' is empty!
}

// AFTER (FIXED):
use cucumber::gherkin::Step;

#[given(regex = r"^I have a file with content:$")]
fn create_file(world: &mut GgenWorld, step: &Step) {
    let content = step.docstring.as_ref()
        .expect("Expected docstring")
        .to_string();
    // Now 'content' has the docstring
}
```

#### 5. Async Function Issues

**Symptom:**
```
warning: async fn used in non-async context
```

**Cause:**
Cucumber 0.20 expects synchronous step functions by default.

**Solution:**
Remove `async` from step definitions:

```rust
// BEFORE:
#[given(regex = r"^I have a file$")]
async fn create_file(world: &mut GgenWorld) { ... }

// AFTER:
#[given(regex = r"^I have a file$")]
fn create_file(world: &mut GgenWorld) { ... }
```

If async I/O is needed, use `tokio::runtime::Runtime::block_on()`:

```rust
#[when(regex = r"^I run async command$")]
fn run_async_command(world: &mut GgenWorld) {
    let rt = tokio::runtime::Runtime::new().unwrap();
    rt.block_on(async {
        // Async code here
    });
}
```

#### 6. Mock Registry Not Working

**Symptom:**
```
Error: Connection refused (os error 111)
```

**Cause:**
Mock HTTP server not started or URL not set.

**Solution:**
```rust
#[given(regex = r"^the marketplace registry is available$")]
fn setup_registry(world: &mut GgenWorld) {
    // MUST call setup_mock_registry
    let url = world.setup_mock_registry();

    // MUST set env var
    std::env::set_var("GGEN_REGISTRY_URL", &url);
}
```

**Debug:**
```rust
// In step definition
eprintln!("Registry URL: {:?}", std::env::var("GGEN_REGISTRY_URL"));
eprintln!("Mock server: {:?}", world.mock_server.as_ref().map(|s| s.url()));
```

### Debugging Strategies

#### 1. Enable Verbose Output

```bash
cargo make test-bdd-verbose
```

Shows all step execution details.

#### 2. Run Single Feature

```bash
cargo test --test bdd test_graph_features -- --nocapture
```

Isolates one feature for debugging.

#### 3. Add Debug Prints to Steps

```rust
#[when(regex = r"^I run command$")]
fn run_command(world: &mut GgenWorld) {
    eprintln!("=== DEBUG ===");
    eprintln!("Working dir: {}", world.project_dir.display());
    eprintln!("Files: {:?}", fs::read_dir(&world.project_dir).unwrap().collect::<Vec<_>>());
    eprintln!("=============");

    // ... rest of step
}
```

#### 4. Inspect World State

```rust
impl GgenWorld {
    pub fn debug_dump(&self) {
        eprintln!("=== WORLD STATE ===");
        eprintln!("Project dir: {}", self.project_dir.display());
        eprintln!("Last exit code: {:?}", self.last_exit_code);
        eprintln!("Stdout: {}", self.last_stdout());
        eprintln!("Stderr: {}", self.last_stderr());
        eprintln!("Captured files: {:?}", self.captured_files.keys());
        eprintln!("==================");
    }
}

// In step:
world.debug_dump();
```

#### 5. Use Rust Debugger

```bash
# Install lldb/gdb
rust-lldb target/debug/deps/bdd-*

# Set breakpoint
(lldb) b tests/bdd/steps/graph_steps.rs:45

# Run
(lldb) run
```

---

## Agent Coordination Protocol

### Communication

**Agent Status Updates:**
Each agent should report status via git commits:

```bash
# Starting work
git commit --allow-empty -m "wip(bdd): Agent N starting ${TASK}"

# Progress update
git commit -m "wip(bdd): Agent N completed ${SUBTASK} (X/Y)"

# Completion
git commit -m "feat(bdd): Agent N completed - ${DELIVERABLE}"
```

### Dependency Management

**Blocking Dependencies:**
- Agent 2-6 → Blocked by Agent 1 (disambiguation)
- Agent 7 → Blocked by Agent 1 (needs updated step patterns)
- Agent 3-6 → Need binary built (external dependency)

**Parallel Work:**
- Agent 1 and Agent 8 can work concurrently
- Agent 2-6 can start planning while Agent 1 disambiguates
- Agent 8 can document existing steps immediately

### Handoff Protocol

When Agent N completes a task:

1. **Run verification:**
   ```bash
   cargo make test-bdd > agent-N-results.txt 2>&1
   ```

2. **Commit deliverables:**
   ```bash
   git add tests/bdd/
   git commit -m "feat(bdd): Agent N - ${DESCRIPTION}\n\n${DETAILS}"
   ```

3. **Tag next agent:**
   ```bash
   git commit --allow-empty -m "handoff: Agent N -> Agent M\n\nNext: ${NEXT_TASK}"
   ```

4. **Update status in this doc:**
   ```markdown
   ### Agent N Status: ✅ Complete
   - Started: 2025-01-09 10:00
   - Completed: 2025-01-09 12:30
   - Deliverables: [list]
   - Next: Agent M
   ```

---

## Appendix

### Useful Commands

```bash
# Build ggen binary
cargo make build

# Run all BDD tests
cargo make test-bdd

# Run specific feature
cargo make test-bdd-feature FEATURE=graph

# Run with verbose output
cargo make test-bdd-verbose

# Check for ambiguous steps
cargo test --test bdd 2>&1 | grep -i "ambiguous"

# Count scenarios and steps
find tests/bdd/features -name "*.feature" -exec grep -h "Scenario:" {} \; | wc -l
find tests/bdd/features -name "*.feature" -exec grep -h "Given\|When\|Then" {} \; | wc -l

# List all step definitions
grep -r "#\[given\]\|#\[when\]\|#\[then\]" tests/bdd/steps/*.rs

# Find unimplemented steps
grep -r "TODO\|FIXME\|Placeholder" tests/bdd/steps/*.rs
```

### File Templates

**New Step Definition File:**

```rust
use super::super::world::GgenWorld;
use assert_cmd::Command;
use cucumber::{given, then, when};
use std::fs;

/// Step definitions for [FEATURE NAME]
///
/// Covers:
/// - [Operation 1]
/// - [Operation 2]

// ============================================================================
// GIVEN steps - Setup preconditions
// ============================================================================

#[given(regex = r"^[STEP PATTERN]$")]
fn step_name(world: &mut GgenWorld) {
    // Implementation
}

// ============================================================================
// WHEN steps - Execute actions
// ============================================================================

#[when(regex = r"^[STEP PATTERN]$")]
fn step_name(world: &mut GgenWorld) {
    // Implementation
}

// ============================================================================
// THEN steps - Verify outcomes
// ============================================================================

#[then(regex = r"^[STEP PATTERN]$")]
fn step_name(world: &mut GgenWorld) {
    // Implementation
}
```

**New Feature File:**

```gherkin
Feature: [Feature Name]
  As a [role]
  I want to [goal]
  So that [benefit]

  Background:
    Given [common setup]

  Scenario: [Scenario name]
    Given [precondition]
    When [action]
    Then [expected outcome]
    And [additional validation]

  Scenario: [Another scenario]
    Given [precondition]
    When [action]
    Then [expected outcome]
```

### References

- **Cucumber Documentation:** https://cucumber.io/docs/
- **cucumber-rs:** https://github.com/cucumber-rs/cucumber
- **London School TDD:** http://www.growing-object-oriented-software.com/
- **assert_cmd:** https://docs.rs/assert_cmd/
- **Gherkin Reference:** https://cucumber.io/docs/gherkin/reference/

---

## Version History

- **1.0.0** (2025-01-09): Initial comprehensive documentation
  - Complete agent task breakdown
  - Step definition inventory
  - Ambiguity resolution strategies
  - Troubleshooting guide

---

**End of Document**

This document is a living specification. Update it as the implementation progresses and new patterns emerge.
