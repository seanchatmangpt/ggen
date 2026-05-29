# Analysis: Sabotage Suite Exploration for ggen-graph

## Executive Summary
This analysis outlines the design and implementation details for `scripts/gall/external/23_run_sabotage_suite.sh`. The purpose of this script is to apply temporary corrupted worktree mutations to the `ggen-graph` package, run the corresponding verification checks to prove that the validation system correctly refuses the mutations, and restore the worktree to a clean state.

---

## 1. Cargo.toml Features Sabotage
* **Where Features are Defined:** `crates/ggen-graph/Cargo.toml` has no `[features]` section by default (unlike the workspace root `Cargo.toml`).
* **Verifier script:** `scripts/gall/external/03_check_feature_flags.sh`.
* **Verification logic:** 
  ```bash
  if grep -q "^\[features\]" "crates/ggen-graph/Cargo.toml"; then
      echo "FAIL: [features] section found in crates/ggen-graph/Cargo.toml" >&2
      exit 1
  fi
  ```
* **Sabotage Application:** Append `[features]\nsabotage = []` to `crates/ggen-graph/Cargo.toml`.
* **Expected Result:** `03_check_feature_flags.sh` exits with code `1`.

---

## 2. TODO in Source Code Sabotage
* **Where to Insert a TODO:** `crates/ggen-graph/src/lib.rs`.
* **Verifier script:** `scripts/gall/external/07_check_anti_fake.sh` (which invokes `scripts/gall/anti_fake_implementation.sh`).
* **Verification logic:** Looks for `TODO`, `FIXME`, `unimplemented!`, `mockall`, etc. inside `crates/ggen-graph/src` recursively via `grep`.
* **Sabotage Application:** Append a comment `// TODO: sabotage` to `crates/ggen-graph/src/lib.rs`. (Note: Putting it in a comment avoids Clippy errors if Cloppy is configured to deny TODOs as compiler flags).
* **Expected Result:** `07_check_anti_fake.sh` exits with code `1`.

---

## 3. Forbidden Surfaces (std::process::Command) Sabotage
* **Where to Insert:** `crates/ggen-graph/src/lib.rs`.
* **Verifier script:** `scripts/gall/external/06_scan_forbidden_surfaces.sh` (which invokes `scripts/gall/forbidden_surface.sh`).
* **Verification logic:** Checks for forbidden patterns including `std::process::Command`, `Command::new`, `std::net`, `tokio::net`, `reqwest`, and `hyper`.
* **Sabotage Application:** Append code or a comment containing `std::process::Command` to `crates/ggen-graph/src/lib.rs`.
* **Expected Result:** `06_scan_forbidden_surfaces.sh` exits with code `1`.

---

## 4. Receipt Tampering Sabotage
* **Mechanism:** Cryptographic transition receipts are verified at the Rust integration test level (`crates/ggen-graph/tests/receipt_replay.rs`) and the pre-commit CLI hook level (`scripts/ggen-receipt-gate.sh`).
* **Hook Verification Logic:** `scripts/ggen-receipt-gate.sh` scans staged files. For each staged file, it derives the expected receipt path under `.ggen/receipts/` and compares the staged file's hash against the stored `file_hash`.
* **Sabotage Application:**
  1. Append a temporary comment (`// change`) to `crates/ggen-graph/src/lib.rs`.
  2. Stage the file: `git add crates/ggen-graph/src/lib.rs`.
  3. Create a mismatched receipt file `.ggen/receipts/crates_ggen-graph_src_lib_rs.receipt.json` containing `{"file_hash": "mismatched_hash_value"}`.
* **Expected Result:** `scripts/ggen-receipt-gate.sh` exits with code `1` showing:
  `TAMPERED crates/ggen-graph/src/lib.rs`

---

## 5. Missing Requirement Link Sabotage
* **Mechanism:** The coverage matrix `crates/ggen-graph/audit/vision2030.coverage.json` lists 9 required compliance identifiers. The verifier script `scripts/gall/external/09_verify_ocel_self_audit.sh` and the Rust binary `verify_audit` parse the self-audit log `crates/ggen-graph/audit/vision2030.self_audit.ocel.json` to ensure every requirement is linked to at least one lifecycle event.
* **Sabotage Application:** Modify `crates/ggen-graph/src/ocel/self_audit.rs` (using Python to avoid using stream editors like `sed`) to replace references of `req_r1_one_crate` with `req_r1_one_crate_sabotage`, which breaks the mapping on subsequent audit generation.
* **Expected Result:** Running `./scripts/gall/external/09_verify_ocel_self_audit.sh` fails because `req_r1_one_crate` is not linked to any event in the generated log.

---

## 6. File Deletion Sabotage
* **Mechanism:** `09_verify_ocel_self_audit.sh` reads all source and test files declared in the coverage matrix `vision2030.coverage.json` and asserts their physical existence.
* **Sabotage Application:** Temporarily delete `crates/ggen-graph/tests/anti_fake_implementation.rs` (which is declared under `req_r1_one_crate`).
* **Expected Result:** `09_verify_ocel_self_audit.sh` exits with code `1` reporting that the documented file does not exist.

---

## Proposed Implementation Plan for `23_run_sabotage_suite.sh`

The script uses a safe `cleanup` trap to guarantee restoration of the worktree on interrupt or failure. It runs each mutation in isolation and asserts that the corresponding verifier exits with a non-zero status.

```bash
#!/usr/bin/env bash
# ==============================================================================
# 23_run_sabotage_suite.sh
# Applies temporary corrupted worktree mutations to prove verification refusal.
# Path: scripts/gall/external/23_run_sabotage_suite.sh
# Exit code: 0 if all sabotaged scenarios are successfully refused.
# ==============================================================================
set -euo pipefail

# Locate workspace root
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
WORKSPACE_ROOT="$(cd "$SCRIPT_DIR/../../.." && pwd)"
cd "$WORKSPACE_ROOT"

# Ensure we clean up on exit, restoring all modified files
cleanup() {
    echo "=== Cleaning up mutations and restoring worktree ==="
    # Restore Cargo.toml
    if [ -f "crates/ggen-graph/Cargo.toml.bak" ]; then
        mv "crates/ggen-graph/Cargo.toml.bak" "crates/ggen-graph/Cargo.toml"
    fi
    # Restore lib.rs
    if [ -f "crates/ggen-graph/src/lib.rs.bak" ]; then
        mv "crates/ggen-graph/src/lib.rs.bak" "crates/ggen-graph/src/lib.rs"
    fi
    # Restore self_audit.rs
    if [ -f "crates/ggen-graph/src/ocel/self_audit.rs.bak" ]; then
        mv "crates/ggen-graph/src/ocel/self_audit.rs.bak" "crates/ggen-graph/src/ocel/self_audit.rs"
    fi
    # Restore anti_fake_implementation.rs
    if [ -f "crates/ggen-graph/tests/anti_fake_implementation.rs.bak" ]; then
        mv "crates/ggen-graph/tests/anti_fake_implementation.rs.bak" "crates/ggen-graph/tests/anti_fake_implementation.rs"
    fi
    # Clean up staged files and unstaged changes
    git checkout -- crates/ggen-graph/Cargo.toml crates/ggen-graph/src/lib.rs crates/ggen-graph/src/ocel/self_audit.rs crates/ggen-graph/tests/anti_fake_implementation.rs 2>/dev/null || true
    # Clean up receipt file and staged state
    rm -f ".ggen/receipts/crates_ggen-graph_src_lib_rs.receipt.json"
    git reset HEAD crates/ggen-graph/src/lib.rs 2>/dev/null || true
}
trap cleanup EXIT

echo "=== Running Sabotage Suite ==="

# Mutation 1: Features in Cargo.toml
echo "--- Testing Mutation 1: Features in Cargo.toml ---"
cp crates/ggen-graph/Cargo.toml crates/ggen-graph/Cargo.toml.bak
echo -e "\n[features]\nsabotage = []" >> crates/ggen-graph/Cargo.toml
if ./scripts/gall/external/03_check_feature_flags.sh; then
    echo "FAIL: 03_check_feature_flags.sh passed despite Cargo.toml features sabotage!"
    exit 1
else
    echo "PASS: Cargo.toml features sabotage was correctly refused."
fi
# Restore
mv crates/ggen-graph/Cargo.toml.bak crates/ggen-graph/Cargo.toml

# Mutation 2: TODO in source
echo "--- Testing Mutation 2: TODO in source ---"
cp crates/ggen-graph/src/lib.rs crates/ggen-graph/src/lib.rs.bak
echo "// TODO: sabotage comment" >> crates/ggen-graph/src/lib.rs
if ./scripts/gall/external/07_check_anti_fake.sh; then
    echo "FAIL: 07_check_anti_fake.sh passed despite source TODO sabotage!"
    exit 1
else
    echo "PASS: Source TODO sabotage was correctly refused."
fi
# Restore
mv crates/ggen-graph/src/lib.rs.bak crates/ggen-graph/src/lib.rs

# Mutation 3: std::process::Command
echo "--- Testing Mutation 3: std::process::Command ---"
cp crates/ggen-graph/src/lib.rs crates/ggen-graph/src/lib.rs.bak
echo "fn dummy_surface() { let _ = std::process::Command::new(\"ls\"); }" >> crates/ggen-graph/src/lib.rs
if ./scripts/gall/external/06_scan_forbidden_surfaces.sh; then
    echo "FAIL: 06_scan_forbidden_surfaces.sh passed despite std::process::Command sabotage!"
    exit 1
else
    echo "PASS: std::process::Command sabotage was correctly refused."
fi
# Restore
mv crates/ggen-graph/src/lib.rs.bak crates/ggen-graph/src/lib.rs

# Mutation 4: Receipt tampering (via pre-commit receipt gate)
echo "--- Testing Mutation 4: Receipt tampering ---"
cp crates/ggen-graph/src/lib.rs crates/ggen-graph/src/lib.rs.bak
echo "// minor change to stage" >> crates/ggen-graph/src/lib.rs
git add crates/ggen-graph/src/lib.rs
# Create a mismatched receipt file
echo '{"file_hash": "mismatched_hash_value_123456"}' > .ggen/receipts/crates_ggen-graph_src_lib_rs.receipt.json
if ./scripts/ggen-receipt-gate.sh; then
    echo "FAIL: ggen-receipt-gate.sh passed despite receipt tampering sabotage!"
    exit 1
else
    echo "PASS: Receipt tampering sabotage was correctly refused."
fi
# Restore
git reset HEAD crates/ggen-graph/src/lib.rs
rm -f .ggen/receipts/crates_ggen-graph_src_lib_rs.receipt.json
mv crates/ggen-graph/src/lib.rs.bak crates/ggen-graph/src/lib.rs

# Mutation 5: Missing requirement link
echo "--- Testing Mutation 5: Missing requirement link ---"
cp crates/ggen-graph/src/ocel/self_audit.rs crates/ggen-graph/src/ocel/self_audit.rs.bak
# Sabotage self_audit.rs using python replacement (non-stream editor compliant)
python3 -c "content = open('crates/ggen-graph/src/ocel/self_audit.rs').read(); open('crates/ggen-graph/src/ocel/self_audit.rs', 'w').write(content.replace('req_r1_one_crate', 'req_r1_one_crate_sabotage'))"
if ./scripts/gall/external/09_verify_ocel_self_audit.sh; then
    echo "FAIL: 09_verify_ocel_self_audit.sh passed despite requirement link sabotage!"
    exit 1
else
    echo "PASS: Requirement link sabotage was correctly refused."
fi
# Restore
mv crates/ggen-graph/src/ocel/self_audit.rs.bak crates/ggen-graph/src/ocel/self_audit.rs

# Mutation 6: File deletion
echo "--- Testing Mutation 6: File deletion ---"
cp crates/ggen-graph/tests/anti_fake_implementation.rs crates/ggen-graph/tests/anti_fake_implementation.rs.bak
rm crates/ggen-graph/tests/anti_fake_implementation.rs
if ./scripts/gall/external/09_verify_ocel_self_audit.sh; then
    echo "FAIL: 09_verify_ocel_self_audit.sh passed despite file deletion sabotage!"
    exit 1
else
    echo "PASS: File deletion sabotage was correctly refused."
fi
# Restore
mv crates/ggen-graph/tests/anti_fake_implementation.rs.bak crates/ggen-graph/tests/anti_fake_implementation.rs

echo "=== Sabotage Suite Completed Successfully: All Sabotage Scenarios Correctly Refused! ==="
exit 0
```
