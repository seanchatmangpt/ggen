# Handoff Report: Verbose compilation and resolution check of `capability-map`

## 1. Observation
The following commands were run in the `/Users/sac/capability-map` directory:

### Clean cpmp target
`cargo clean -p cpmp`
Output:
```
     Removed 2709 files, 337.5MiB total
```

### Verbose Cargo Check
`cargo check --verbose`
Output:
```
    Checking cpmp v0.1.0 (/Users/sac/capability-map)
     Running `/Users/sac/.rustup/toolchains/stable-aarch64-apple-darwin/bin/rustc --crate-name cpmp --edition=2021 src/lib.rs --error-format=json --json=diagnostic-rendered-ansi,artifacts,future-incompat --crate-type lib --emit=dep-info,metadata -C embed-bitcode=no -C debuginfo=2 -C split-debuginfo=unpacked --check-cfg 'cfg(docsrs,test)' --check-cfg 'cfg(feature, values())' -C metadata=0e19e9a047601c24 -C extra-filename=-263b7ddc31702e90 --out-dir /Users/sac/capability-map/target/debug/deps -C incremental=/Users/sac/capability-map/target/debug/incremental -L dependency=/Users/sac/capability-map/target/debug/deps --extern anyhow=/Users/sac/capability-map/target/debug/deps/libanyhow-4bab4d01512a9227.rmeta --extern blake3=/Users/sac/capability-map/target/debug/deps/libblake3-41af602a43d1226e.rmeta --extern chrono=/Users/sac/capability-map/target/debug/deps/libchrono-0471893f0424fdf3.rmeta --extern clap=/Users/sac/capability-map/target/debug/deps/libclap-20eaa2602fc01705.rmeta --extern comfy_table=/Users/sac/capability-map/target/debug/deps/libcomfy_table-71b1ae7d4c7dbe37.rmeta --extern hex=/Users/sac/capability-map/target/debug/deps/libhex-cdb2f00c2a22ad7b.rmeta --extern ignore=/Users/sac/capability-map/target/debug/deps/libignore-753c822ee8113cd5.rmeta --extern oxigraph=/Users/sac/capability-map/target/debug/deps/liboxigraph-64ef4dd24e961f8d.rmeta --extern regex=/Users/sac/capability-map/target/debug/deps/libregex-379e28e88099ea12.rmeta --extern rusqlite=/Users/sac/capability-map/target/debug/deps/librusqlite-9a506438f99f57a4.rmeta --extern serde=/Users/sac/capability-map/target/debug/deps/libserde-3f52e0205f41bd97.rmeta --extern serde_json=/Users/sac/capability-map/target/debug/deps/libserde_json-629b9804cebcc9c9.rmeta --extern sha2=/Users/sac/capability-map/target/debug/deps/libsha2-d12d092cf8c114a8.rmeta --extern thiserror=/Users/sac/capability-map/target/debug/deps/libthiserror-77a753abdeaca5f6.rmeta --extern toml=/Users/sac/capability-map/target/debug/deps/libtoml-a7a965fc90bc6007.rmeta --extern uuid=/Users/sac/capability-map/target/debug/deps/libuuid-bddbc8e600b61d12.rmeta --extern walkdir=/Users/sac/capability-map/target/debug/deps/libwalkdir-b4bcc05e720a42a8.rmeta -L native=/Users/sac/capability-map/target/debug/build/blake3-5e88f48a497715be/out -L native=/Users/sac/capability-map/target/debug/build/oxrocksdb-sys-826d1d8c2f44e65d/out -L native=/Users/sac/capability-map/target/debug/build/oxrocksdb-sys-826d1d8c2f44e65d/out -L native=/Users/sac/capability-map/target/debug/build/oxrocksdb-sys-826d1d8c2f44e65d/out -L native=/Users/sac/capability-map/target/debug/build/libsqlite3-sys-126ac4f4f93d0070/out`
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

warning: `cpmp` (lib) generated 4 warnings (run `cargo fix --lib -p cpmp` to apply 4 suggestions)
     Running `/Users/sac/.rustup/toolchains/stable-aarch64-apple-darwin/bin/rustc --crate-name cpmp --edition=2021 src/main.rs --error-format=json --json=diagnostic-rendered-ansi,artifacts,future-incompat --crate-type bin --emit=dep-info,metadata -C embed-bitcode=no -C debuginfo=2 -C split-debuginfo=unpacked --check-cfg 'cfg(docsrs,test)' --check-cfg 'cfg(feature, values())' -C metadata=ada09932084fda32 -C extra-filename=-4df953c5419af2d9 --out-dir /Users/sac/capability-map/target/debug/deps -C incremental=/Users/sac/capability-map/target/debug/incremental -L dependency=/Users/sac/capability-map/target/debug/deps --extern anyhow=/Users/sac/capability-map/target/debug/deps/libanyhow-4bab4d01512a9227.rmeta --extern blake3=/Users/sac/capability-map/target/debug/deps/libblake3-41af602a43d1226e.rmeta --extern chrono=/Users/sac/capability-map/target/debug/deps/libchrono-0471893f0424fdf3.rmeta --extern clap=/Users/sac/capability-map/target/debug/deps/libclap-20eaa2602fc01705.rmeta --extern comfy_table=/Users/sac/capability-map/target/debug/deps/libcomfy_table-71b1ae7d4c7dbe37.rmeta --extern cpmp=/Users/sac/capability-map/target/debug/deps/libcpmp-263b7ddc31702e90.rmeta --extern hex=/Users/sac/capability-map/target/debug/deps/libhex-cdb2f00c2a22ad7b.rmeta --extern ignore=/Users/sac/capability-map/target/debug/deps/libignore-753c822ee8113cd5.rmeta --extern oxigraph=/Users/sac/capability-map/target/debug/deps/liboxigraph-64ef4dd24e961f8d.rmeta --extern regex=/Users/sac/capability-map/target/debug/deps/libregex-379e28e88099ea12.rmeta --extern rusqlite=/Users/sac/capability-map/target/debug/deps/librusqlite-9a506438f99f57a4.rmeta --extern serde=/Users/sac/capability-map/target/debug/deps/libserde-3f52e0205f41bd97.rmeta --extern serde_json=/Users/sac/capability-map/target/debug/deps/libserde_json-629b9804cebcc9c9.rmeta --extern sha2=/Users/sac/capability-map/target/debug/deps/libsha2-d12d092cf8c114a8.rmeta --extern thiserror=/Users/sac/capability-map/target/debug/deps/libthiserror-77a753abdeaca5f6.rmeta --extern toml=/Users/sac/capability-map/target/debug/deps/libtoml-a7a965fc90bc6007.rmeta --extern uuid=/Users/sac/capability-map/target/debug/deps/libuuid-bddbc8e600b61d12.rmeta --extern walkdir=/Users/sac/capability-map/target/debug/deps/libwalkdir-b4bcc05e720a42a8.rmeta -L native=/Users/sac/capability-map/target/debug/build/blake3-5e88f48a497715be/out -L native=/Users/sac/capability-map/target/debug/build/oxrocksdb-sys-826d1d8c2f44e65d/out -L native=/Users/sac/capability-map/target/debug/build/oxrocksdb-sys-826d1d8c2f44e65d/out -L native=/Users/sac/capability-map/target/debug/build/oxrocksdb-sys-826d1d8c2f44e65d/out -L native=/Users/sac/capability-map/target/debug/build/libsqlite3-sys-126ac4f4f93d0070/out`
    Finished `dev` profile [unoptimized + debuginfo] target(s) in 0.52s
```

### Verbose Cargo Test compilation and run
`cargo test --verbose` (ran after clean)
Output (abbreviated):
```
     Running `/Users/sac/.rustup/toolchains/stable-aarch64-apple-darwin/bin/rustc --crate-name cpmp --edition=2021 src/main.rs --error-format=json --json=diagnostic-rendered-ansi,artifacts,future-incompat --crate-type bin --emit=dep-info,link -C embed-bitcode=no -C debuginfo=2 -C split-debuginfo=unpacked --check-cfg 'cfg(docsrs,test)' --check-cfg 'cfg(feature, values())' -C metadata=60ea9c0236538f0d -C extra-filename=-6807001133f55c4d --out-dir /Users/sac/capability-map/target/debug/deps -C incremental=/Users/sac/capability-map/target/debug/incremental -L dependency=/Users/sac/capability-map/target/debug/deps --extern anyhow=/Users/sac/capability-map/target/debug/deps/libanyhow-d967057818ef337f.rlib --extern blake3=/Users/sac/capability-map/target/debug/deps/libblake3-cfafddc501e55f1d.rlib --extern chrono=/Users/sac/capability-map/target/debug/deps/libchrono-0f53b325f5e6734c.rlib --extern clap=/Users/sac/capability-map/target/debug/deps/libclap-6047c1d459dd468d.rlib --extern comfy_table=/Users/sac/capability-map/target/debug/deps/libcomfy_table-f03a749ea8afbd1d.rlib --extern cpmp=/Users/sac/capability-map/target/debug/deps/libcpmp-c3fb234c3d85c76f.rlib --extern hex=/Users/sac/capability-map/target/debug/deps/libhex-a36e081a1c27dff6.rlib --extern ignore=/Users/sac/capability-map/target/debug/deps/libignore-fbdd34738b1d884c.rlib --extern oxigraph=/Users/sac/capability-map/target/debug/deps/liboxigraph-6a265faaac8c7faa.rlib --extern regex=/Users/sac/capability-map/target/debug/deps/libregex-0ef2eeb9f0e58221.rlib --extern rusqlite=/Users/sac/capability-map/target/debug/deps/librusqlite-0f7965661dca8666.rlib --extern serde=/Users/sac/capability-map/target/debug/deps/libserde-38884705b9e06f6c.rlib --extern serde_json=/Users/sac/capability-map/target/debug/deps/libserde_json-d718d21f250428eb.rlib --extern sha2=/Users/sac/capability-map/target/debug/deps/libsha2-16425a9c0c26d8b8.rlib --extern thiserror=/Users/sac/capability-map/target/debug/deps/libthiserror-f33b3f3c10fdcaaf.rlib --extern toml=/Users/sac/capability-map/target/debug/deps/libtoml-241576e99ab4746a.rlib --extern uuid=/Users/sac/capability-map/target/debug/deps/libuuid-136c9621a6326754.rlib --extern walkdir=/Users/sac/capability-map/target/debug/deps/libwalkdir-387cd06384dcff11.rlib -L native=/Users/sac/capability-map/target/debug/build/blake3-1d3c126151c9ccfa/out -L native=/Users/sac/capability-map/target/debug/build/oxrocksdb-sys-8b49d4244a93b0ed/out -L native=/Users/sac/capability-map/target/debug/build/oxrocksdb-sys-8b49d4244a93b0ed/out -L native=/Users/sac/capability-map/target/debug/build/oxrocksdb-sys-8b49d4244a93b0ed/out -L native=/Users/sac/capability-map/target/debug/build/libsqlite3-sys-db631759cd40661d/out`

warning: unused import: `rdf`
 --> tests/integration_tests.rs:1:33
  |
1 | use cpmp::{capability, receipt, rdf, scanner, symbol};
  |                                 ^^^
  |
  = note: `#[warn(unused_imports)]` (part of `#[warn(unused)]`) on by default

warning: `cpmp` (test "integration_tests") generated 1 warning (run `cargo fix --test "integration_tests" -p cpmp` to apply 1 suggestion)
    Finished `test` profile [unoptimized + debuginfo] target(s) in 1.64s
     Running `/Users/sac/capability-map/target/debug/deps/cpmp-9cbc04e80d78a010`
running 0 tests
test result: ok. 0 passed; 0 failed; 0 ignored; 0 measured; 0 filtered out; finished in 0.00s

     Running `/Users/sac/capability-map/target/debug/deps/cpmp-2cd2d3a6cb40fc80`
running 0 tests
test result: ok. 0 passed; 0 failed; 0 ignored; 0 measured; 0 filtered out; finished in 0.00s

     Running `/Users/sac/capability-map/target/debug/deps/integration_tests-2db203740074f6bc`
running 8 tests
test test_capabilities_detected_in_readme ... ok
test test_no_deletion_pass_when_no_files_removed ... ok
test test_no_deletion_fail_when_file_removed ... ok
test test_symbols_extracted_from_fixture ... ok
test test_catalog_ttl_contains_required_vocabulary ... ok
test test_scan_produces_files_and_receipt ... ok
test test_policy_checks_pass_after_valid_scan ... ok
{"ok":true,"triples":85}
test test_catalog_ttl_validates_with_open_ontologies ... ok

test result: ok. 8 passed; 0 failed; 0 ignored; 0 measured; 0 filtered out; finished in 0.10s
```

## 2. Logic Chain
1. The codebase `/Users/sac/capability-map` is structured as a combined cargo binary and library package named `cpmp`, as declared in `Cargo.toml`.
2. The library is rooted at `src/lib.rs` and exports the following submodules: `capability`, `error`, `gates`, `models`, `policy`, `projection`, `rdf`, `receipt`, `scanner`, `symbol`, `db`, `report`.
3. The binary is rooted at `src/main.rs` and imports the `cpmp` library as a dependency via `--extern cpmp=...` in the `rustc` arguments.
4. When `cargo check --verbose` is run, the compiler builds the library target first:
   - Root: `src/lib.rs`
   - Key output: `libcpmp-263b7ddc31702e90.rmeta`
5. The binary target is built second:
   - Root: `src/main.rs`
   - Compiles into the executable target `cpmp` using the library metadata artifact.
6. The test target is built third (during `cargo test`):
   - Root: `tests/integration_tests.rs`
   - References `cpmp` library artifact.
7. Unused import warnings are produced for `Context` (in `src/receipt.rs`), `bail` and `blake3::Hasher` (in `src/scanner.rs`), `FileEntry` (in `src/symbol.rs`), and `rdf` (in `tests/integration_tests.rs`).

## 3. Caveats
- There is a directory `src/cmds/` containing command submodules (`computer.rs`, `graph.rs`, `capability.rs`, etc.). However, these files are not imported anywhere in the `lib.rs` or `main.rs` module trees and do not take part in compilation.
- Pre-built objects were cleaned specifically for the package `cpmp` using `cargo clean -p cpmp` to force recompilation of the local code. External dependencies from Cargo registry were left fresh to optimize performance.

## 4. Conclusion
The compiler compiles the library `src/lib.rs` and exports it as metadata. It then compiles the binary `src/main.rs` and test harness `tests/integration_tests.rs` by linking against the library's metadata/rlib. Unused modules and files (such as files under `src/cmds/`) are ignored by the compiler because they are not declared in `lib.rs` or `main.rs` via `mod` or `pub mod` keywords.

## 5. Verification Method
To reproduce and verify the compilation resolution and outputs:
1. Navigate to `/Users/sac/capability-map`.
2. Clean existing build cache: `cargo clean -p cpmp`
3. Execute verbose compilation: `cargo check --verbose`
4. Inspect the exact compiler invocations to see `src/lib.rs` (lib) and `src/main.rs` (bin) compilation units.
5. Run tests: `cargo test --verbose` to ensure everything is resolved and integrated correctly.
