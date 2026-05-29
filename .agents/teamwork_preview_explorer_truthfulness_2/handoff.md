# Handoff Report: Sabotage Suite Exploration for ggen-graph

This report outlines the observations, logic chain, and implementation proposal for `scripts/gall/external/23_run_sabotage_suite.sh`.

## 1. Observation
We observed the following configurations, scripts, and structures in the repository:
1. **Cargo.toml Features Configuration:**
   * In `crates/ggen-graph/Cargo.toml`, we observed no `[features]` section defined (lines 1-17).
   * In `scripts/gall/external/03_check_feature_flags.sh` (lines 14-18), the script verifies this via grep:
     ```bash
     if grep -q "^\[features\]" "$CARGO_TOML"; then
         echo "FAIL: [features] section found in $CARGO_TOML" >&2
         exit 1
     fi
     ```
2. **TODO Scanning:**
   * In `scripts/gall/anti_fake_implementation.sh` (lines 9-19), the checker specifies:
     ```bash
     FORBIDDEN_PATTERNS=(
         "mockall"
         "mock!"
         "#[automock]"
         "TODO"
         ...
     )
     ```
   * It scans `crates/ggen-graph/src` and `crates/ggen-graph/tests` for these patterns.
3. **Forbidden Surfaces:**
   * In `scripts/gall/forbidden_surface.sh` (lines 8-15), the script defines:
     ```bash
     FORBIDDEN_PATTERNS=(
         "std::process::Command"
         "Command::new"
         "std::net"
         ...
     )
     ```
   * It scans `crates/ggen-graph/src` recursively via grep.
4. **Receipt Validation Hook:**
   * In `scripts/ggen-receipt-gate.sh` (lines 148-189), the hook checks staged files against their derived receipt names under `.ggen/receipts/`:
     ```bash
     RELATIVE="${file#/}"
     SAFE_NAME=$(echo "$RELATIVE" | sed 's|/|_|g; s|\.|_|g')
     RECEIPT_FILE="${RECEIPT_DIR}/${SAFE_NAME}.receipt.json"
     ```
   * It compares `file_hash` stored in the JSON file against `file_sha256 "$FULL_PATH"`.
5. **Requirement Linkage and Coverage:**
   * In `crates/ggen-graph/audit/vision2030.coverage.json`, the 9 requirements (`req_r1_one_crate` ... `req_r9_proof_report`) are mapped to source and test files.
   * In `scripts/gall/external/09_verify_ocel_self_audit.sh` (lines 74-87), the script queries linkages using jq:
     ```bash
     EVENT_LINK_COUNT=$(jq --arg req "$req" '[.events[].objects[] | select(.id == $req)] | length' "$SELF_AUDIT_JSON")
     if [ "$EVENT_LINK_COUNT" -eq 0 ]; then
         echo "FAIL: Requirement $req is not linked to any event in $SELF_AUDIT_JSON." >&2
         exit 1
     fi
     ```
6. **File Existence Validation:**
   * In `scripts/gall/external/09_verify_ocel_self_audit.sh` (lines 90-105), the script validates existence of all documented files:
     ```bash
     if [ ! -f "$file_path" ]; then
         echo "FAIL: Documented file does not exist: $file_path" >&2
         exit 1
     fi
     ```
7. **Integrity Manifest:**
   * In `scripts/gall/external/manifest.sha256` (lines 14-16), files like `self_audit.rs`, `coverage.rs`, and `verify_audit.rs` are tracked, but `crates/ggen-graph/src/lib.rs` and `crates/ggen-graph/Cargo.toml` are not.

---

## 2. Logic Chain
1. To test **Cargo.toml features refusal**, adding a `[features]` section directly in `crates/ggen-graph/Cargo.toml` is guaranteed to fail `03_check_feature_flags.sh` based on Observation 1.
2. To test **TODO in source refusal**, appending `// TODO` to `crates/ggen-graph/src/lib.rs` triggers `07_check_anti_fake.sh` based on Observation 2.
3. To test **forbidden surfaces refusal**, appending `std::process::Command` to `crates/ggen-graph/src/lib.rs` triggers `06_scan_forbidden_surfaces.sh` based on Observation 3.
4. To test **receipt tampering refusal**, adding a minor change to `crates/ggen-graph/src/lib.rs`, staging it via `git add`, and writing a mismatched receipt to `.ggen/receipts/crates_ggen-graph_src_lib_rs.receipt.json` forces a mismatch check failure in `ggen-receipt-gate.sh` based on Observation 4.
5. To test **missing requirement links refusal**, replacing `req_r1_one_crate` in `crates/ggen-graph/src/ocel/self_audit.rs` (using Python to avoid stream editors) breaks the relationship link mapping. Running `09_verify_ocel_self_audit.sh` (which invokes `emit_audit` and `verify_audit`) will detect the missing requirement ID link and fail based on Observation 5.
6. To test **file deletion refusal**, temporarily deleting `crates/ggen-graph/tests/anti_fake_implementation.rs` causes `09_verify_ocel_self_audit.sh` to report the missing file and exit non-zero based on Observation 6.
7. Using a `trap cleanup` shell mechanism ensures that all temporary mutations are safely restored to keep the worktree clean after the verification tests finish.

---

## 3. Caveats
* If the user runs `13_adjudicate_gall_promotion.sh` during a mutation that affects files listed in `manifest.sha256` (like `self_audit.rs`), the execution will fail at Section 1 (manifest verification). Therefore, the sabotage script executes individual checkers directly to demonstrate specific failure reasons, rather than failing early at the manifest checking stage.
* Receipt gate testing requires staging files (`git add`), which requires restoring the git index during the cleanup step (`git reset HEAD`).

---

## 4. Conclusion
We have identified precise targets and commands for each of the six requested sabotage mutations. The implementation of `scripts/gall/external/23_run_sabotage_suite.sh` outlined in `analysis.md` will successfully demonstrate verification refusal for all of them under clean rollback guarantees.

---

## 5. Verification Method
To verify the exploration and the proposed script design:
1. View the proposed script in `analysis.md`.
2. Inspect the targets:
   * `crates/ggen-graph/Cargo.toml`
   * `crates/ggen-graph/src/lib.rs`
   * `crates/ggen-graph/src/ocel/self_audit.rs`
   * `crates/ggen-graph/tests/anti_fake_implementation.rs`
3. If an implementer is ready to write the script, they can execute `bash scripts/gall/external/23_run_sabotage_suite.sh` and assert that it exits with code `0`, confirming all refusal checks successfully matched the expected failures and the worktree was completely restored.
