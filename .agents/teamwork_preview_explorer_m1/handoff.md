# Handoff Report — Codebase Audit of `capability-map` (cpmp)

## 1. Observation
Direct observations of the codebase at `/Users/sac/capability-map`:

### Compilation and Test Status
Running `cargo test --all` or `cargo check` fails with several compiler errors:
1. **Missing Function in `capability` module**:
   - File: `/Users/sac/capability-map/src/scanner.rs`, Line 56:
     ```rust
     let caps = capability::detect_capabilities(&file.path, &content, &file_syms);
     ```
   - Error: `cannot find function detect_capabilities in module capability`. 
   - Cause: `/Users/sac/capability-map/src/capability.rs` implements `detect_in_file` (returning `Vec<CapabilityHit>`) but does not implement `detect_capabilities`.
   
2. **Reversed API Arguments in `rdf` module**:
   - File: `/Users/sac/capability-map/src/rdf.rs`, Lines 107-108:
     ```rust
     store
         .dump_to_writer(RdfFormat::NQuads, std::io::BufWriter::new(nq_file))
     ```
     Error: `oxigraph::store::Store::dump_to_writer` expects the writer as the first argument, and the serializer format as the second.
   - File: `/Users/sac/capability-map/src/rdf.rs`, Lines 114-115:
     ```rust
     store
         .dump_graph_to_writer(GraphNameRef::DefaultGraph, RdfFormat::Turtle, std::io::BufWriter::new(ttl_file))
     ```
     Error: `oxigraph::store::Store::dump_graph_to_writer` expects the graph name, then the writer, then the serializer format.
     
3. **Invalid Error Struct Syntax**:
   - File: `/Users/sac/capability-map/src/db.rs`, Line 205:
     ```rust
     .map_err(|e| CpmpError::Database(e))?;
     ```
   - File: `/Users/sac/capability-map/src/report.rs`, Lines 30, 41, 67, 71, 90, 99, 119, 128:
     ```rust
     .map_err(crate::error::CpmpError::Database)?;
     ```
   - Error: `no variant named Database found for struct anyhow::Error`.
   - Cause: `CpmpError` is a re-export of `anyhow::Error` which has no variants.

4. **Integration Test Mismatch**:
   - File: `/Users/sac/capability-map/tests/integration_tests.rs`, Line 166:
     ```rust
     let caps = capability::detect_capabilities(&fixture, &content, &[]);
     ```
   - Error: `cannot find function detect_capabilities in module capability`.

### toolchain Availability
- `open-ontologies` command-line utility is fully available on the path and prints a help menu containing subcommands `validate`, `load`, `save`, `stats`, `query`, `diff`, `version`, `shacl`, `reason`, `lineage`, etc.

### Existing Codebase Structures
- **CLI Command parser (`src/main.rs`)**: Implements Clap subcommands for the aligned architecture: `computer discover`, `graph project`, `graph validate`, `graph load`, `graph query`, `graph version`, `graph drift`, `policy check`, `policy enforce`, `tenant create`, `tenant list`, `audit lineage`, `receipt emit`, `receipt verify-no-deletion`, and `enterprise doctor`.
- **Scan engine (`src/scanner.rs` & `src/capability.rs` & `src/symbol.rs`)**: Walks directories, extracts regex symbols, detects vocabulary terms, and builds JSON reports.
- **RDF generation (`src/rdf.rs`)**: Serializes facts into public ontologies RDF/Turtle and N-Quads using Oxigraph.
- **Verification (`src/receipt.rs` & `src/gates.rs` & `src/policy.rs`)**: Computes BLAKE3 hashes of files and Turtle schemas, implements no-deletion comparison, policy check sets, and verification of shapes via `open-ontologies`.

---

## 2. Logic Chain
1. The compilation fails due to mismatched names in the capability extraction API (calling `detect_capabilities` which is absent instead of `detect_in_file` or implementing `detect_capabilities`), reversed arguments on the Oxigraph dumping API in `src/rdf.rs`, and utilizing an enum constructor `.Database()` on an `anyhow::Error` type alias.
2. Because compilation fails, none of the integration tests in `tests/integration_tests.rs` or the actual binary commands can be executed or tested.
3. The commands mapping in `src/main.rs` are fully aligned with the requirements in R3. However, they call functions that do not compile.
4. The enterprise modules in R4 are only listed as stubs/warnings in `main.rs` and documented in `docs/enterprise/ARCHITECTURE.md` but do not have implemented source code modules.
5. The 8 scan pipeline refusal gates in R5 are partially represented in `policy.rs` and `gates.rs` but lack explicit checks for non-existent scan inputs, empty scan results, non-writable output directories, and optional parser unavailabilities.
6. The docs in R2 are almost entirely missing except for `docs/dev/GALL_CAP_ARCHITECTURE.md` and `docs/enterprise/ARCHITECTURE.md`.

---

## 3. Caveats
- We did not attempt to fix any compilation errors directly since this is a read-only investigation.
- We did not write code files or tests into the target codebase.
- We assume that the user's environment is fully configured for compilation other than these source code bugs.

---

## 4. Conclusion
The `capability-map` codebase implements the core architecture and Open Ontologies integration, but it is currently blocked by five critical compiler errors across `scanner.rs`, `rdf.rs`, `db.rs`, `report.rs`, and the integration tests. Furthermore, all required documents under `docs/` (except two) are empty, the 12 enterprise modules in R4 are only defined as stubs, and the 8 refusal gates in R5 are only partially implemented.

---

## 5. Verification Method
To independently verify the compilation issues:
1. Navigate to `/Users/sac/capability-map`.
2. Run `cargo check` or `cargo test --all`.
3. Verify that the output lists the exact mismatch errors in `scanner.rs`, `rdf.rs`, `db.rs`, `report.rs`, and `tests/integration_tests.rs`.
4. Run `open-ontologies --help` to confirm the utility exists on the path.
