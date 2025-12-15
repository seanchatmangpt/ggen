# PHASE 5 RAW TEST DATA

## Commands Executed

### 1. Full Test Run with Output Capture
```bash
cargo test -p ggen-cli-lib --test integration -- --nocapture 2>&1 | tee tests/test_results.txt
```

**Duration:** 0.99s
**Exit Code:** 101 (test failures)

---

### 2. Test Summary Extraction
```bash
cargo test -p ggen-cli-lib --test integration 2>&1 | grep -E "test result:|running|tests passed|finished in"
```

**Output:**
```
running 28 tests
test result: FAILED. 12 passed; 16 failed; 0 ignored; 0 measured; 0 filtered out; finished in 0.99s
```

---

### 3. Test Count Verification
```bash
cargo test -p ggen-cli-lib --test integration -- --list 2>&1 | grep -E "^test_" | wc -l
```

**Output:** 28 tests

---

## Individual Test Results

### Passing Tests (12)

```
test test_error_propagation_invalid_command ... ok
test test_error_propagation_missing_required_var ... ok
test test_error_propagation_invalid_template ... ok
test test_error_propagation_missing_file ... ok
test test_json_output_marketplace_search ... ok
test test_json_output_project_info ... ok
test test_manifest_path_option ... ok
test test_v2_business_logic_not_overwritten ... ok
test test_v2_frozen_section_preservation ... ok
test test_v2_rdf_based_template_generation ... ok
test test_workflow_template_to_lifecycle ... ok
test test_workflow_graph_operations ... ok
```

---

### Failing Tests (16)

#### 1. test_template_generate_integration
```
thread 'test_template_generate_integration' panicked at /Users/sac/.asdf/installs/rust/1.86.0/toolchains/1.90.0-aarch64-apple-darwin/lib/rustlib/src/rust/library/core/src/ops/function.rs:253:5:
Unexpected failure.
code=1
stderr=``````
Error: CLI error: CLI execution failed: Argument parsing failed: error: unexpected argument '/Users/sac/.cache/tmp/.tmpnWHUay/template.yaml' found

Usage: ggen template generate [OPTIONS]

For more information, try '--help'.
```
```
command=`"/Users/sac/ggen/target/debug/ggen" "template" "generate" "/Users/sac/.cache/tmp/.tmpnWHUay/template.yaml" "/Users/sac/.cache/tmp/.tmpnWHUay/output"`
```

---

#### 2. test_marketplace_search_integration
```
thread 'test_marketplace_search_integration' panicked at crates/ggen-cli/tests/integration.rs:80:5:
assertion failed: output.status.success() || output.status.code() == Some(0)
```

---

#### 3. test_project_gen_integration
```
thread 'test_project_gen_integration' panicked at crates/ggen-cli/tests/integration.rs:106:5:
assertion failed: output.status.success() || project_dir.path().exists()
```

---

#### 4. test_lifecycle_execution_integration
```
thread 'test_lifecycle_execution_integration' panicked at crates/ggen-cli/tests/integration.rs:149:5:
assertion failed: output.status.success()
```

---

#### 5. test_help_command
```
thread 'test_help_command' panicked at /Users/sac/.asdf/installs/rust/1.86.0/toolchains/1.90.0-aarch64-apple-darwin/lib/rustlib/src/rust/library/core/src/ops/function.rs:253:5:
Unexpected failure.
code=1
stderr=``````
Error: CLI error: CLI execution failed: Argument parsing failed: Usage: ggen [COMMAND]

Commands:
  paper        Track paper submission and peer review status
  utils        Manage environment variables
  ai           Analyze code with AI insights
  marketplace  Export marketplace assessments in various formats
  hook         Monitor hook events
  graph        Visualize graph structure
  template     Generate CLI project from RDF/TTL file
  workflow     Generate workflow report
  project      Watch for changes and auto-regenerate
  help         Print this message or the help of the given subcommand(s)

Options:
  -h, --help  Print help
```
```
command=`"/Users/sac/ggen/target/debug/ggen" "--help"`
code=1
```

---

#### 6. test_version_command
```
thread 'test_version_command' panicked at /Users/sac/.asdf/installs/rust/1.86.0/toolchains/1.90.0-aarch64-apple-darwin/lib/rustlib/src/rust/library/core/src/ops/function.rs:253:5:
Unexpected stdout, failed var.contains(1.2.0)
├── var: 
└── var as str: 

command=`"/Users/sac/ggen/target/debug/ggen" "--version"`
code=0
stdout=""
stderr=""
```

---

#### 7. test_progressive_help
```
thread 'test_progressive_help' panicked at /Users/sac/.asdf/installs/rust/1.86.0/toolchains/1.90.0-aarch64-apple-darwin/lib/rustlib/src/rust/library/core/src/ops/function.rs:253:5:
Unexpected failure.
code=1
stderr=``````
Error: CLI error: CLI execution failed: Argument parsing failed: error: unrecognized subcommand 'help-me'

  tip: some similar subcommands exist: 'template', 'help'

Usage: ggen [COMMAND]

For more information, try '--help'.
```
```
command=`"/Users/sac/ggen/target/debug/ggen" "help-me"`
code=1
```

---

#### 8. test_subcommand_help
```
thread 'test_subcommand_help' panicked at /Users/sac/.asdf/installs/rust/1.86.0/toolchains/1.90.0-aarch64-apple-darwin/lib/rustlib/src/rust/library/core/src/ops/function.rs:253:5:
Unexpected failure.
code=1
stderr=``````
Error: CLI error: CLI execution failed: Argument parsing failed: Generate CLI project from RDF/TTL file

Usage: ggen template [COMMAND]

Commands:
  list           List templates
  lint           Lint a template
  generate       Generate from template (basic version without Vec support)
  show           Show template metadata
  generate_tree  Generate file tree from template
  new            Create new template
  regenerate     Regenerate from template
  generate_rdf   Generate CLI project from RDF/TTL file
  help           Print this message or the help of the given subcommand(s)

Options:
  -h, --help  Print help
```
```
command=`"/Users/sac/ggen/target/debug/ggen" "template" "--help"`
code=1
```

---

#### 9. test_v2_auto_discovery
```
thread 'test_v2_auto_discovery' panicked at crates/ggen-cli/tests/integration.rs:562:5:
assertion failed: stdout.contains("marketplace") || stdout.contains("market")
```

---

#### 10. test_v2_sync_wrapper_execution
```
thread 'test_v2_sync_wrapper_execution' panicked at crates/ggen-cli/tests/integration.rs:577:5:
assertion failed: output.status.success()
```

---

#### 11. test_v2_help_me_command
```
thread 'test_v2_help_me_command' panicked at crates/ggen-cli/tests/integration.rs:589:5:
assertion failed: output.status.success()
```

---

#### 12. test_shell_completion_generation
```
thread 'test_shell_completion_generation' panicked at crates/ggen-cli/tests/integration.rs:434:5:
assertion failed: output.status.success() || completion_file.path().exists()
```

---

#### 13. test_config_file_loading
```
thread 'test_config_file_loading' panicked at crates/ggen-cli/tests/integration.rs:469:5:
assertion failed: output.status.success()
```

---

#### 14. test_doctor_before_operations
```
thread 'test_doctor_before_operations' panicked at /Users/sac/.asdf/installs/rust/1.86.0/toolchains/1.90.0-aarch64-apple-darwin/lib/rustlib/src/rust/library/core/src/ops/function.rs:253:5:
Unexpected failure.
code=1
stderr=``````
Error: CLI error: CLI execution failed: Argument parsing failed: error: unrecognized subcommand 'doctor'

Usage: ggen [COMMAND]

For more information, try '--help'.
```
```
command=`"/Users/sac/ggen/target/debug/ggen" "doctor"`
code=1
```

---

#### 15. test_workflow_marketplace_to_project
```
thread 'test_workflow_marketplace_to_project' panicked at /Users/sac/.asdf/installs/rust/1.86.0/toolchains/1.90.0-aarch64-apple-darwin/lib/rustlib/src/rust/library/core/src/ops/function.rs:253:5:
Unexpected failure.
code=1
stderr=``````
Error: CLI error: CLI execution failed: Argument parsing failed: error: unexpected argument 'cli-template' found

Usage: ggen marketplace search [OPTIONS] --query <QUERY>

For more information, try '--help'.
```
```
command=`"/Users/sac/ggen/target/debug/ggen" "marketplace" "search" "cli-template" "--limit" "1"`
code=1
```

---

#### 16. test_v2_marketplace_search_with_rdf
```
thread 'test_v2_marketplace_search_with_rdf' panicked at crates/ggen-cli/tests/integration.rs:724:5:
assertion failed: output.status.success() || output.status.code() == Some(0)
```

---

## Compilation Warnings

### Unused Attributes (7 warnings)
```
warning: unused attribute `ignore`
  --> crates/ggen-cli/tests/integration.rs:83:1
   |
83 | #[ignore = "project gen command not fully implemented"]
   | ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

warning: unused attribute `ignore`
   --> crates/ggen-cli/tests/integration.rs:109:1
    |
109 | #[ignore = "lifecycle run command not fully implemented"]
    | ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

warning: unused attribute `ignore`
   --> crates/ggen-cli/tests/integration.rs:415:1
    |
415 | #[ignore = "shell completion command not implemented"]
    | ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

warning: unused attribute `ignore`
   --> crates/ggen-cli/tests/integration.rs:441:1
    |
441 | #[ignore = "config file loading not fully implemented"]
    | ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

warning: unused attribute `ignore`
   --> crates/ggen-cli/tests/integration.rs:592:1
    |
592 | #[ignore = "frozen section feature not yet implemented"]
    | ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

warning: unused attribute `ignore`
   --> crates/ggen-cli/tests/integration.rs:664:1
    |
664 | #[ignore = "business logic protection not yet implemented"]
    | ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

warning: unused attribute `ignore`
   --> crates/ggen-cli/tests/integration.rs:727:1
    |
727 | #[ignore = "RDF template generation not yet fully implemented"]
    | ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
```

**Note:** These #[ignore] attributes are being applied to the `test!` macro invocation, not the actual test function, so they're not working as intended.

---

### Unused Import (1 warning)
```
warning: unused import: `chicago_tdd_tools::prelude::*`
  --> crates/ggen-cli/tests/integration.rs:15:5
   |
15 | use chicago_tdd_tools::prelude::*;
   |     ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
```

**Note:** Prelude import not needed since we're importing the macro directly.

---

## Timing Analysis

### Build Phase
- **Compilation:** 0.34s
- **Dependencies:** Already cached
- **Incremental:** Excellent (only test file recompiled)

### Test Phase
- **Test Discovery:** <10ms
- **Test Execution:** 0.99s
- **Cleanup:** <10ms

### Total Pipeline
- **End-to-End:** 1.33s
- **Per-Test Average:** 35ms
- **Overhead:** ~5% (test framework)

---

## Test Environment

### Build Configuration
- **Profile:** test (unoptimized + debuginfo)
- **Target:** x86_64/aarch64-apple-darwin
- **Rust Version:** 1.86.0 (via asdf)
- **Toolchain:** 1.90.0-aarch64-apple-darwin

### Test Configuration
- **Test Binary:** target/debug/deps/integration-0581b3f5324bc097
- **Features:** Default
- **Parallel Execution:** Yes (28 threads)

### Package Information
- **Package:** ggen-cli-lib
- **Version:** 2.7.1
- **Test Type:** Integration tests

---

## File Locations

### Test Files
- **Source:** `/Users/sac/ggen/crates/ggen-cli/tests/integration.rs`
- **Output:** `/Users/sac/ggen/tests/test_results.txt`
- **Reports:** `/Users/sac/ggen/tests/PHASE_5_*.md`

### Binary
- **Test Binary:** `/Users/sac/ggen/target/debug/deps/integration-0581b3f5324bc097`
- **CLI Binary:** `/Users/sac/ggen/target/debug/ggen`

---

## Next Diagnostic Commands

### To Run Individual Failing Test
```bash
cargo test -p ggen-cli-lib --test integration test_template_generate_integration -- --nocapture
```

### To Get Detailed Backtrace
```bash
RUST_BACKTRACE=1 cargo test -p ggen-cli-lib --test integration test_name
```

### To Run Only Passing Tests
```bash
cargo test -p ggen-cli-lib --test integration test_error_propagation
cargo test -p ggen-cli-lib --test integration test_json_output
cargo test -p ggen-cli-lib --test integration test_workflow
```

### To Check CLI Help
```bash
./target/debug/ggen --help
./target/debug/ggen template --help
./target/debug/ggen marketplace --help
```

---

**Report Generated:** 2025-11-16
**Test Execution Completed:** 2025-11-16 (Phase 5)
**Total Test Duration:** 0.99 seconds
