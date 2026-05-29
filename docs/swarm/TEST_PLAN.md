# Capability Map Test Plan

This document describes the test plan for verifying the Genesis-bearing parts in the `capability-map` project. 
It ensures that the scanning, hashing, projection, and receipt issuance are functioning correctly and that no source code is ever deleted. 
All proofs must cross external boundaries, producing verifiable SQLite catalogs and Markdown projections.

## Target Architecture
*   **Genesis**: The pure Chatman Equation foundation.
*   **ggen**: The foundry and membrane.
*   **AtomVM/WASM**: Runtime bodies for interchangeable parts.
*   **Rust**: Physical discipline.
*   **Receipts**: Composability enablers.
*   **Truex**: Lifecycle for execution trust.

## Required Tests

### 1. Fixture Scanning & Hashing
*   **Action**: Scan a predefined fixture directory containing sample capabilities.
*   **Validation**: Verify that `FileEntry` correctly identifies and hashes each file.
*   **Proof**: SQLite catalog entries must contain accurate BLAKE3 hashes for all fixture files.

### 2. SQLite Catalog Insertion
*   **Action**: Insert file, symbol, and capability records into the SQLite database (`db.rs:insert_catalog`).
*   **Validation**: Query the SQLite database to confirm successful record insertion without data loss.
*   **Proof**: `SELECT COUNT(*)` must match expected counts for files, symbols, and capabilities.

### 3. Capability Detection
*   **Action**: Extract symbols and map them to known capabilities (`capability.rs:detect_capabilities`).
*   **Validation**: Confirm that `genesis`, `ggen`, and `wasm` symbols correctly yield high-confidence capabilities.
*   **Proof**: Check the capability records in SQLite and resulting projections for correctness.

### 4. Test Detection
*   **Action**: Classify files based on symbol extraction (`classification.rs:classify_file`).
*   **Validation**: Verify that symbols containing `test` or `mock` correctly categorize files as `Classification::Partial`.
*   **Proof**: Assert the correct `Classification` output for fixture test files.

### 5. Markdown Report Emission
*   **Action**: Generate `CAPABILITY_INVENTORY.md` and `PATTERN_ATLAS.md` from the database.
*   **Validation**: Confirm files are created in the output directory with appropriate generated content.
*   **Proof**: The files must physically exist and contain non-empty Markdown structures (`projection.rs:generate_projections`).

### 6. Receipt Emission
*   **Action**: Generate an unforgeable cryptographic receipt capturing the scan and projection phase.
*   **Validation**: Verify the receipt structure (BLAKE3 chain, causal dependencies).
*   **Proof**: Receipt hash matches the artifact states, ensuring Truex lifecycle trust.

### 7. Non-Deletion Invariant
*   **Action**: Perform full directory scans and database updates.
*   **Validation**: Ensure no source code or fixtures are removed during the entire lifecycle.
*   **Proof**: Post-scan directory hash matches pre-scan directory hash exactly.

### 8. Added File Verification
*   **Action**: Inject a new file into the fixture directory and run a differential scan.
*   **Validation**: The new file must be explicitly reported as added in the event log and database.
*   **Proof**: A new `INSERT` record and corresponding event boundary.

### 9. Missing File Refusal
*   **Action**: Delete a previously scanned file and rescan.
*   **Validation**: The system must report the missing file as a refusal/missing state, NOT quietly remove it.
*   **Proof**: A missing file refusal receipt is emitted.

### 10. Code Preservation During Scan
*   **Action**: Continuously monitor the directory state during an active scan process.
*   **Validation**: The scanner process must not hold destructive write locks or issue `rm` commands.
*   **Proof**: OS-level tracing (or equivalent test-level simulation) proves zero `unlink` calls are made to source files.

## Definition of Done (DoD)
*   [ ] Tests written and passing using `cargo test`.
*   [ ] No mocked databases; real SQLite files or in-memory SQLite instances are used.
*   [ ] Markdown projections are validated via file existence and pattern matching.
*   [ ] Receipt generation includes causal hashes.
*   [ ] Zero file deletion invariant mathematically proven via tests.

## Notes
*   **IMPLEMENTED**: Database schema, partial capability detection.
*   **MISSING**: Complete receipt emission boundary, OS-level unlink tracing, robust missing file handling.
