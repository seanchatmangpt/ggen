# Porting a New Target

1. Confirm the target appears in `rustc --print target-list` or supply a reviewed custom target specification.
2. Generate `rustc --print cfg --target <triple>` facts.
3. Classify Rust support tier separately from this project's standing.
4. Build `tcps-core` as an rlib first.
5. Add only the products the target can represent.
6. Record linker, sysroot, SDK, runner, hardware, and minimum platform version.
7. Add a positive fixture, negative fixture, and target receipt.
8. Execute on native hardware or an approved emulator before claiming runtime support.
9. Add the target to a CI family without weakening other targets.
10. Promote the target only after replay reproduces the original result.

