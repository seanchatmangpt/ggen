# Diagnostics Report

- **Date / Time**: 2026-05-27T15:30:15-07:00 (Local), 2026-05-27T22:30:15Z (UTC)
- **Role**: Diagnostic Specialist
- **Target Repository**: `/Users/sac/capability-map`

---

## 1. Compilation Verification (`cargo check --all-targets`)

**Command Executed:**
```bash
cargo check --all-targets
```

**Status:** `SUCCESS` (with warnings, 0 errors)

**Output:**
```
    Checking cpmp v0.1.0 (/Users/sac/capability-map)
warning: unused import: `Context`
 --> src/receipt.rs:2:14
  |
2 | use anyhow::{Context, Result};
  |              ^^^^^^^
  |
  = note: `#[warn(unused_imports)]` (part of `#[warn(unused)]`) on by default

warning: unused import: `bail`
 --> src/scanner.rs:3:14
  |
3 | use anyhow::{bail, Context, Result};
  |              ^^^^

warning: unused import: `blake3::Hasher`
 --> src/scanner.rs:4:5
  |
4 | use blake3::Hasher;
  |     ^^^^^^^^^^^^^^

warning: unused import: `FileEntry`
 --> src/symbol.rs:1:21
  |
1 | use crate::models::{FileEntry, Symbol};
  |                     ^^^^^^^^^

warning: `cpmp` (lib test) generated 4 warnings (4 duplicates)
warning: `cpmp` (lib) generated 4 warnings (run `cargo fix --lib -p cpmp` to apply 4 suggestions)
warning: unused import: `rdf`
 --> tests/integration_tests.rs:1:33
  |
1 | use cpmp::{capability, receipt, rdf, scanner, symbol};
  |                                 ^^^
  |
  = note: `#[warn(unused_imports)]` (part of `#[warn(unused)]`) on by default

warning: `cpmp` (test "integration_tests") generated 1 warning (run `cargo fix --test "integration_tests" -p cpmp` to apply 1 suggestion)
    Finished `dev` profile [unoptimized + debuginfo] target(s) in 0.54s
```

---

## 2. Test Verification (`cargo test`)

**Command Executed:**
```bash
cargo test
```

**Status:** `SUCCESS` (8 passed, 0 failed)

**Output:**
```
   Compiling cpmp v0.1.0 (/Users/sac/capability-map)
warning: unused import: `Context`
 --> src/receipt.rs:2:14
  |
2 | use anyhow::{Context, Result};
  |              ^^^^^^^
  |
  = note: `#[warn(unused_imports)]` (part of `#[warn(unused)]`) on by default

warning: unused import: `FileEntry`
 --> src/symbol.rs:1:21
  |
1 | use crate::models::{FileEntry, Symbol};
  |                     ^^^^^^^^^

warning: `cpmp` (lib test) generated 2 warnings (2 duplicates)
warning: `cpmp` (lib) generated 2 warnings (run `cargo fix --lib -p cpmp` to apply 2 suggestions)
warning: unused import: `rdf`
 --> tests/integration_tests.rs:1:33
  |
1 | use cpmp::{capability, receipt, rdf, scanner, symbol};
  |                                 ^^^
  |
  = note: `#[warn(unused_imports)]` (part of `#[warn(unused)]`) on by default

warning: `cpmp` (test "integration_tests") generated 1 warning (run `cargo fix --test "integration_tests" -p cpmp` to apply 1 suggestion)
    Finished `test` profile [unoptimized + debuginfo] target(s) in 1.65s
     Running unittests src/lib.rs (target/debug/deps/cpmp-9cbc04e80d78a010)

running 0 tests

test result: ok. 0 passed; 0 failed; 0 ignored; 0 measured; 0 filtered out; finished in 0.00s

     Running unittests src/main.rs (target/debug/deps/cpmp-2cd2d3a6cb40fc80)

running 0 tests

test result: ok. 0 passed; 0 failed; 0 ignored; 0 measured; 0 filtered out; finished in 0.00s

     Running tests/integration_tests.rs (target/debug/deps/integration_tests-2db203740074f6bc)

running 8 tests
test test_capabilities_detected_in_readme ... ok
test test_no_deletion_fail_when_file_removed ... ok
test test_no_deletion_pass_when_no_files_removed ... ok
test test_symbols_extracted_from_fixture ... ok
test test_catalog_ttl_contains_required_vocabulary ... ok
test test_policy_checks_pass_after_valid_scan ... ok
test test_scan_produces_files_and_receipt ... ok
{"ok":true,"triples":85}
test test_catalog_ttl_validates_with_open_ontologies ... ok

test result: ok. 8 passed; 0 failed; 0 ignored; 0 measured; 0 filtered out; finished in 0.09s

   Doc-tests cpmp

running 0 tests

test result: ok. 0 passed; 0 failed; 0 ignored; 0 measured; 0 filtered out; finished in 0.00s
```

---

## 3. Command-line Tool Verification (`open-ontologies`)

**Command Executed:**
```bash
which open-ontologies
```

**Status:** `INSTALLED`

**Path:** `/Users/sac/.local/bin/open-ontologies`

**Version Check (`open-ontologies status`):**
```json
{"status":"ok","triples_loaded":0,"version":"0.1.11"}
```
