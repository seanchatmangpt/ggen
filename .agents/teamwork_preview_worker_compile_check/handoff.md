# Handoff Report — Compilation Check

## 1. Observation
We ran the command `cargo check --all-targets` in the codebase `/Users/sac/capability-map`. The command failed with exit code 101.

The compilation output contained the following errors (22 in total):

```
error[E0425]: cannot find type `Repository` in module `crate::scanner`
   --> src/db.rs:116:77
    |
116 | pub fn insert_repo(conn: &Connection, scan_id: &str, repo: &crate::scanner::Repository) -> Result<()> {
    |                                                                             ^^^^^^^^^^ not found in `crate::scanner`

error[E0425]: cannot find type `FileRecord` in module `crate::scanner`
   --> src/db.rs:125:74
    |
125 | pub fn insert_file(conn: &Connection, scan_id: &str, f: &crate::scanner::FileRecord) -> Result<()> {
    |                                                                          ^^^^^^^^^^ not found in `crate::scanner`

error[E0425]: cannot find type `CapabilityHit` in module `crate::capability`
   --> src/db.rs:149:85
    |
149 | pub fn insert_capability(conn: &Connection, scan_id: &str, hit: &crate::capability::CapabilityHit) -> Result<()> {
    |                                                                                     ^^^^^^^^^^^^^ not found in `crate::capability`

error[E0603]: struct import `ScanReceipt` is private
   --> src/db.rs:168:62
    |
168 | pub fn insert_receipt(conn: &Connection, r: &crate::receipt::ScanReceipt) -> Result<()> {
    |                                                              ^^^^^^^^^^^ private struct import

error[E0609]: no field `scan_run_id` on type `&ScanReceipt`
   --> src/db.rs:173:21
    |
173 |             r.id, r.scan_run_id, r.timestamp, r.roots, r.file_count as i64, r.dir_count as i64,
    |                     ^^^^^^^^^^^ unknown field

error[E0609]: no field `roots` on type `&ScanReceipt`
   --> src/db.rs:173:49
    |
173 |             r.id, r.scan_run_id, r.timestamp, r.roots, r.file_count as i64, r.dir_count as i64,
    |                                                 ^^^^^ unknown field

error[E0609]: no field `dir_count` on type `&ScanReceipt`
   --> src/db.rs:173:79
    |
173 |             r.id, r.scan_run_id, r.timestamp, r.roots, r.file_count as i64, r.dir_count as i64,
    |                                                                               ^^^^^^^^^ unknown field

error[E0609]: no field `hash_algo` on type `&ScanReceipt`
   --> src/db.rs:174:37
    |
174 |             r.total_bytes as i64, r.hash_algo, r.root_hash, r.catalog_version, r.command_run,
    |                                     ^^^^^^^^^ unknown field

error[E0609]: no field `catalog_version` on type `&ScanReceipt`
   --> src/db.rs:174:63
    |
174 |             r.total_bytes as i64, r.hash_algo, r.root_hash, r.catalog_version, r.command_run,
    |                                                               ^^^^^^^^^^^^^^^ unknown field

error[E0609]: no field `command_run` on type `&ScanReceipt`
   --> src/db.rs:174:82
    |
174 |             r.total_bytes as i64, r.hash_algo, r.root_hash, r.catalog_version, r.command_run,
    |                                                                                  ^^^^^^^^^^^ unknown field

error[E0609]: no field `warnings` on type `&ScanReceipt`
   --> src/db.rs:175:15
    |
175 |             r.warnings.join("; "), r.refusals.join("; "), r.receipt_path,
    |               ^^^^^^^^ unknown field

error[E0609]: no field `refusals` on type `&ScanReceipt`
   --> src/db.rs:175:38
    |
175 |             r.warnings.join("; "), r.refusals.join("; "), r.receipt_path,
    |                                      ^^^^^^^^ unknown field

error[E0609]: no field `receipt_path` on type `&ScanReceipt`
   --> src/db.rs:175:61
    |
175 |             r.warnings.join("; "), r.refusals.join("; "), r.receipt_path,
    |                                                             ^^^^^^^^^^^^ unknown field

error[E0599]: no associated item named `Database` found for struct `anyhow::Error` in the current scope
   --> src/db.rs:205:33
    |
205 |         .map_err(|e| CpmpError::Database(e))?;
    |                                 ^^^^^^^^ associated item not found in `anyhow::Error`

error[E0599]: no associated item named `Database` found for struct `anyhow::Error` in the current scope
  --> src/report.rs:30:40
   |
30 |     ).map_err(crate::error::CpmpError::Database)?;
   |                                        ^^^^^^^^ associated item not found in `anyhow::Error`

error[E0599]: no associated item named `Database` found for struct `anyhow::Error` in the current scope
  --> src/report.rs:41:41
   |
41 |     }).map_err(crate::error::CpmpError::Database)?;
   |                                         ^^^^^^^^ associated item not found in `anyhow::Error`

error[E0599]: no associated item named `Database` found for struct `anyhow::Error` in the current scope
  --> src/report.rs:67:40
   |
67 |     ).map_err(crate::error::CpmpError::Database)?;
   |                                        ^^^^^^^^ associated item not found in `anyhow::Error`

error[E0599]: no associated item named `Database` found for struct `anyhow::Error` in the current scope
  --> src/report.rs:71:41
   |
71 |     }).map_err(crate::error::CpmpError::Database)?;
   |                                         ^^^^^^^^ associated item not found in `anyhow::Error`

error[E0599]: no associated item named `Database` found for struct `anyhow::Error` in the current scope
  --> src/report.rs:90:40
   |
90 |     ).map_err(crate::error::CpmpError::Database)?;
   |                                        ^^^^^^^^ associated item not found in `anyhow::Error`

error[E0599]: no associated item named `Database` found for struct `anyhow::Error` in the current scope
  --> src/report.rs:99:41
   |
99 |     }).map_err(crate::error::CpmpError::Database)?;
   |                                         ^^^^^^^^ associated item not found in `anyhow::Error`

error[E0599]: no associated item named `Database` found for struct `anyhow::Error` in the current scope
   --> src/report.rs:119:40
    |
119 |     ).map_err(crate::error::CpmpError::Database)?;
    |                                        ^^^^^^^^ associated item not found in `anyhow::Error`

error[E0599]: no associated item named `Database` found for struct `anyhow::Error` in the current scope
   --> src/report.rs:128:41
    |
128 |     }).map_err(crate::error::CpmpError::Database)?;
    |                                         ^^^^^^^^ associated item not found in `anyhow::Error`
```

## 2. Logic Chain
1. We executed `cargo check --all-targets` in `/Users/sac/capability-map` (Observation 1).
2. The command failed with an exit code of 101, showing that compilation of the library and tests could not succeed (Observation 1).
3. The compiler errors show mismatched/missing type definitions in `src/db.rs` and `src/report.rs`, specifically:
   - Module `crate::scanner` does not export `Repository` or `FileRecord`.
   - Module `crate::capability` does not export `CapabilityHit`.
   - `ScanReceipt` imports and visibility issues, along with missing fields on `ScanReceipt`.
   - Structural mapping error where `CpmpError::Database(e)` fails to map from `anyhow::Error` (associated item `Database` not found for `anyhow::Error` in that scope/type mapping).
4. As a result, the `capability-map` crate cannot compile in its current state.

## 3. Caveats
- We did not modify any source code files to fix the compiler errors.
- We did not run tests or clippy check since cargo compilation failed at checking.
- The `anyhow::Error` mapping issue suggests there might be a mismatch in the definition/handling of `CpmpError` versus `anyhow::Error`.

## 4. Conclusion
The codebase `/Users/sac/capability-map` is in a non-compiling state. There are 22 errors located across `src/db.rs` and `src/report.rs`.

## 5. Verification Method
1. Navigate to `/Users/sac/capability-map`.
2. Run the command `cargo check --all-targets`.
3. Verify that the command returns the compile errors shown above.
