# External Lifecycle Evaluation & Observer Script Ring Design

This document details the analysis and technical design for the **External Observer Script Ring (00 to 13)** under `scripts/gall/external/` for the `ggen-graph` substrate. The objective of this script ring is to provide independent, tamper-resistant verification of the crate's lifecycle, requirement coverage, and compliance with the Vision 2030 GALL Checkpoint requirements.

---

## 1. Executive Summary

To prevent self-verification and ensure absolute adherence to the **AGENTS.md Constitution** and the **GEMINI.md Verification Doctrine**, the verification of `ggen-graph` cannot rely solely on the crate's internal tests or binaries. We design an **External Observer Script Ring** consisting of 14 sequential Bash/Python verification scripts (`00_` to `13_`). 

### Core Architectural Decisions:
1. **Self-Auditing Integrity Ring**: Every script in the ring computes its own cryptographic digest (`SHA-256`) and logs it during execution.
2. **Master Adjudicator (`13_adjudicate_gall_promotion.sh`)**: Orchestrates the execution of all validation scripts, computes their current digests, validates them against expected baseline hashes, hashes key source/test/configuration files, and outputs the final promotion status to `crates/ggen-graph/audit/vision2030.external_adjudication.json`.
3. **Traceability and Contradiction Detection**: Explicitly validates that the self-audit log (`vision2030.self_audit.ocel.json`) is free of chronological or status contradictions (e.g., preventing a checkpoint from being both promoted and refused, or being promoted without a prior evaluation).

---

## 2. Current Validation Script Base (`scripts/gall/`)

An analysis of the existing validation scripts in the repository shows the following baseline:

*   `scripts/gall/forbidden_surface.sh`: Recursively scans `crates/ggen-graph/src` for prohibited execution surfaces (such as `std::process::Command`, `Command::new`, `std::net`, `tokio::net`, `reqwest`, and `hyper`).
*   `scripts/gall/anti_fake_implementation.sh`: Scans source files and tests for forbidden mocking frameworks (`mockall`, `mock!`, `#[automock]`), lazy markers (`TODO`, `FIXME`, `unimplemented!`), or placeholder constants (`hash_placeholder`, `uuid_placeholder`, `fake_signature`).
*   `scripts/gall/emit_ocel_self_audit.sh`: Invokes `cargo run -p ggen-graph --bin emit_audit` to generate `vision2030.self_audit.ocel.json` and `vision2030.coverage.json`.
*   `scripts/gall/verify_ocel_self_audit.sh`: Invokes `cargo run -p ggen-graph --bin verify_audit` to enforce the 5 Completeness Rules defined in the PRD.

These scripts run directly at the shell level but are not integrated into an immutable external adjudication flow. The External Observer Script Ring will consume, wrap, and extend these checks.

---

## 3. Cryptographic Digest Capture Research

To prevent verifier bypass or mutation (e.g., an agent modifying `05_run_tests.sh` to return `0` despite cargo test failures), the scripts must dynamically capture digests.

### Available System Utilities
On the target Darwin (macOS) environment, the following command-line utilities are verified as available:
*   `/sbin/sha256sum` (version `sha256sum (Darwin) 1.0`, outputs standard `hash  filename`)
*   `/usr/bin/shasum` (outputs standard `hash  filename` via `shasum -a 256`)
*   `/usr/bin/openssl` (outputs `SHA256(filename)= hash` via `openssl dgst -sha256`)

### Unified Hashing Mechanism
We define a robust shell function to calculate `SHA-256` digests uniformly across environments:

```bash
get_sha256() {
    local file_path="$1"
    if [ ! -f "$file_path" ]; then
        echo "Error: File $file_path does not exist" >&2
        return 1
    fi
    if command -v sha256sum >/dev/null 2>&1; then
        sha256sum "$file_path" | cut -d' ' -f1
    elif command -v shasum >/dev/null 2>&1; then
        shasum -a 256 "$file_path" | cut -d' ' -f1
    elif command -v openssl >/dev/null 2>&1; then
        openssl dgst -sha256 "$file_path" | awk '{print $2}'
    else
        echo "Error: No SHA-256 utility found" >&2
        return 1
    fi
}
```

### Script Self-Verification
Each script under `scripts/gall/external/` will capture its own path and digest upon execution to assert its runtime state:
```bash
SCRIPT_PATH="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)/$(basename "${BASH_SOURCE[0]}")"
SCRIPT_DIGEST=$(get_sha256 "$SCRIPT_PATH")
echo "[INFO] Executing script: $0"
echo "[INFO] Script Runtime SHA-256: $SCRIPT_DIGEST"
```

---

## 4. Contradiction Detection Design (`12_detect_contradictions.sh`)

The contradiction detector must analyze the OCEL self-audit log to ensure that the process trace is mathematically and logically sound. 

### Contradiction Rules
1.  **Status Singularity**: A checkpoint object `obj_cp_verify` must not be associated with both a `CheckpointPromoted` event and a `CheckpointRefused` event.
2.  **Causal Evaluation Precedence**: Any `CheckpointPromoted` or `CheckpointRefused` event for a checkpoint object must be chronologically preceded by a `CheckpointEvaluated` event referencing the same checkpoint object.
3.  **Timestamp Monotonicity**: The timestamp of the `CheckpointEvaluated` event must be strictly earlier than the timestamp of the promotion or refusal event.
4.  **No Missing Evaluations**: A promotion decision must have an evaluation event; a decision cannot be declared in a vacuum.

### Implementation Design
Because datetime comparison and JSON traversing are error-prone in pure Bash, we leverage `/usr/bin/python3` (which is present in the workspace environment) inside `12_detect_contradictions.sh`.

```python
#!/usr/bin/env python3
import json
import sys
from datetime import datetime

OCEL_PATH = "crates/ggen-graph/audit/vision2030.self_audit.ocel.json"

def analyze_contradictions():
    try:
        with open(OCEL_PATH, "r") as f:
            log = json.load(f)
    except Exception as e:
        print(f"FAIL: Failed to read OCEL log: {e}", file=sys.stderr)
        sys.exit(1)

    events = log.get("events", [])
    cp_state = {}

    # Gather events related to GALLCheckpoint objects
    for ev in events:
        activity = ev.get("activity")
        timestamp_str = ev.get("timestamp")
        
        # Parse timestamp safely
        try:
            timestamp = datetime.strptime(timestamp_str, "%Y-%m-%dT%H:%M:%SZ")
        except ValueError:
            try:
                timestamp = datetime.strptime(timestamp_str, "%Y-%m-%dT%H:%M:%S.%fZ")
            except ValueError as ve:
                print(f"FAIL: Invalid timestamp format '{timestamp_str}': {ve}", file=sys.stderr)
                sys.exit(1)

        for obj in ev.get("objects", []):
            if obj.get("type") == "GALLCheckpoint":
                cp_id = obj.get("id")
                if cp_id not in cp_state:
                    cp_state[cp_id] = {"evaluated": [], "promoted": [], "refused": []}
                
                if activity == "CheckpointEvaluated":
                    cp_state[cp_id]["evaluated"].append(timestamp)
                elif activity == "CheckpointPromoted":
                    cp_state[cp_id]["promoted"].append(timestamp)
                elif activity == "CheckpointRefused":
                    cp_state[cp_id]["refused"].append(timestamp)

    violations = []
    for cp_id, data in cp_state.items():
        # Rule 1: Status Singularity (Promoted AND Refused)
        if len(data["promoted"]) > 0 and len(data["refused"]) > 0:
            violations.append(f"Checkpoint '{cp_id}' has contradictory states: both Promoted and Refused events exist.")

        # Rule 2 & 3: Causal Evaluation Precedence & Timestamp Monotonicity
        for promo_time in data["promoted"]:
            prior_evals = [eval_time for eval_time in data["evaluated"] if eval_time < promo_time]
            if not prior_evals:
                violations.append(f"Checkpoint '{cp_id}' was promoted at {promo_time} without a chronologically prior CheckpointEvaluated event.")

        for refuse_time in data["refused"]:
            prior_evals = [eval_time for eval_time in data["evaluated"] if eval_time < refuse_time]
            if not prior_evals:
                violations.append(f"Checkpoint '{cp_id}' was refused at {refuse_time} without a chronologically prior CheckpointEvaluated event.")

        # Rule 4: No empty decisions (Must have at least one evaluation)
        if not data["evaluated"] and (data["promoted"] or data["refused"]):
            violations.append(f"Checkpoint '{cp_id}' has a decision event but zero evaluation events recorded.")

    if violations:
        print("=== Contradictions Detected ===", file=sys.stderr)
        for v in violations:
            print(f"  - {v}", file=sys.stderr)
        sys.exit(1)
    else:
        print("PASS: No process or state contradictions found in self-audit log.")
        sys.exit(0)

if __name__ == "__main__":
    analyze_contradictions()
```

---

## 5. Adjudication Flow Design (`13_adjudicate_gall_promotion.sh`)

The master adjudicator runs all steps, checks script digests to verify they were not bypassed or mutated, and records the results.

### Adjudication Steps:
1.  **Initialize Environment**: Check the workspace root and verify that all script files `00` through `12` exist.
2.  **Capture Digests**: Compute the `SHA-256` of each script file (`00_capture_baseline.sh` through `12_detect_contradictions.sh`) before execution.
3.  **Execute Ring Sequentially**:
    *   Call each script in numerical order.
    *   Capture stdout, stderr, and exit status.
    *   If any script exits with a non-zero code, immediately halt (fail-fast) or collect all failures for reporting.
4.  **Hash Project Files**: Capture the SHA-256 hashes of critical files:
    *   `crates/ggen-graph/Cargo.toml`
    *   `crates/ggen-graph/src/lib.rs`
    *   `crates/ggen-graph/audit/vision2030.self_audit.ocel.json`
    *   `crates/ggen-graph/audit/vision2030.coverage.json`
5.  **Output Report**: Emit `crates/ggen-graph/audit/vision2030.external_adjudication.json`.

### Output JSON Schema (`vision2030.external_adjudication.json`)
```json
{
  "timestamp": "2026-05-26T23:50:00Z",
  "adjudication_status": "Approved",
  "verification_ring_integrity": {
    "00_capture_baseline.sh": "sha256_hash_here",
    "01_extract_requirements.sh": "sha256_hash_here",
    "12_detect_contradictions.sh": "sha256_hash_here"
  },
  "project_file_digests": {
    "Cargo.toml": "sha256_hash_here",
    "src/lib.rs": "sha256_hash_here",
    "audit/vision2030.self_audit.ocel.json": "sha256_hash_here",
    "audit/vision2030.coverage.json": "sha256_hash_here"
  },
  "execution_results": [
    {
      "step": "00_capture_baseline.sh",
      "exit_code": 0,
      "message": "PASS: Baseline captured."
    },
    {
      "step": "12_detect_contradictions.sh",
      "exit_code": 0,
      "message": "PASS: No contradictions found."
    }
  ]
}
```

---

## 6. Script Ring Specifications (00 to 11)

The following specifies the files to be created under `scripts/gall/external/`:

### `00_capture_baseline.sh`
*   **Purpose**: Records workspace state before verification.
*   **Logic**: Run `git rev-parse HEAD` and `git status --porcelain` to verify a clean state or log modified files. Write a list of source/test files and their active SHA-256 hashes to `crates/ggen-graph/audit/baseline.sha256`.

### `01_extract_requirements.sh`
*   **Purpose**: Confirms requirements presence.
*   **Logic**: Parses the target requirement identifiers from `ORIGINAL_REQUEST.md` (e.g., `req_r1_one_crate` to `req_r9_proof_report`) and verifies they are documented.

### `02_verify_package_constraints.sh`
*   **Purpose**: Validates single-crate constraints.
*   **Logic**: Verifies that the codebase consists of a single standalone crate under `crates/ggen-graph/`. Scans for other hidden packages or configurations.

### `03_check_feature_flags.sh`
*   **Purpose**: Verifies that no feature flags are enabled.
*   **Logic**: Parses `crates/ggen-graph/Cargo.toml` and checks that the `[features]` section is either missing or empty.

### `04_compile_substrate.sh`
*   **Purpose**: Builds the substrate target.
*   **Logic**: Runs `cargo check -p ggen-graph --all-targets` and `cargo build -p ggen-graph --all-targets`. Fails if compilation fails.

### `05_run_tests.sh`
*   **Purpose**: Executes the test suite.
*   **Logic**: Runs `cargo test -p ggen-graph`. Parses the output to ensure zero failures and records the total number of passing tests.

### `06_scan_forbidden_surfaces.sh`
*   **Purpose**: Audits for illegal execution layers.
*   **Logic**: Runs code checks (via grep) ensuring no source files import or use forbidden surfaces (such as `std::process::Command`, standard library networking, or networking crates like `reqwest`).

### `07_scan_anti_fake.sh`
*   **Purpose**: Audits against mocks, stubs, and lazy placeholders.
*   **Logic**: Checks for `mockall`, `mock!`, `#[automock]`, `TODO`, `FIXME`, `unimplemented!`, or placeholders in the codebase.

### `08_emit_ocel_self_audit.sh`
*   **Purpose**: Triggers generation of the self-audit log and coverage matrix.
*   **Logic**: Runs `cargo run -p ggen-graph --bin emit_audit`. Asserts that the two target JSON files are generated and are non-empty.

### `09_verify_ocel_self_audit.sh`
*   **Purpose**: Verifies the OCEL self-audit log completeness.
*   **Logic**: Runs `cargo run -p ggen-graph --bin verify_audit`. Asserts that the output contains the success message and exit status is `0`.

### `10_verify_coverage_matrix.sh`
*   **Purpose**: Validates requirement-to-evidence coverage.
*   **Logic**: Parses `crates/ggen-graph/audit/vision2030.coverage.json` and verifies that every requirement has associated source files, test files, and verification commands, and that all referenced files physically exist.

### `11_verify_receipt_replay.sh`
*   **Purpose**: Validates replayability of Blake3 receipts.
*   **Logic**: Runs the receipt replay tests (`cargo test --test receipt_replay`) and parses outputs to confirm that transitions are verified deterministic and replayable.

---

## 7. Crucial Contradiction Finding & Proposed Remediation

### The Contradiction Finding
During the analysis of `crates/ggen-graph/src/ocel/self_audit.rs`, it was observed that the `generate_self_audit_log` function statically appends **both** a `CheckpointPromoted` event (Event 15, timestamp `10:10:00Z`) and a `CheckpointRefused` event (Event 16, timestamp `10:15:00Z`) for the same checkpoint object `obj_cp_verify`.

This static structure represents an inherent logical contradiction: a single checkpoint verification run cannot be both promoted and refused. If `12_detect_contradictions.sh` is run on this log, it will correctly detect this contradiction and fail.

### Proposed Remediation
To satisfy the contradiction checks in the External Observer Script Ring, the self-audit generator must be updated. 
1.  **Parameterization or Flag Control**: The `generate_self_audit_log` function in `self_audit.rs` should accept a parameter (e.g., `success: bool`) or read an environment variable (e.g., `GALL_CHECKPOINT_STATUS`).
2.  **Conditional Event Emission**:
    *   If the status is success, emit the `CheckpointPromoted` event and omit `CheckpointRefused`.
    *   If the status is failure, emit the `CheckpointRefused` event and omit `CheckpointPromoted`.
3.  **Adjudication Integration**: The emitter binary (`emit_audit`) can default to generating a successful promotion log if all baseline checks pass, or it can be driven by the test framework to simulate both states without having both active in the same audit log.
