# Platform Support Contract

The project does not claim that all Rust targets are equivalent. It implements one universal `no_std` core and a capability-indexed product family.

## Source of truth

1. `rustc --print target-list` defines the built-in target set for the active compiler.
2. `targets/official-platform-support-1.97.1.json` records the Rust 1.97.1 Tier 1 and Tier 2 snapshot.
3. A discovered target absent from the snapshot is classified as Tier 3.
4. `rustc --print cfg --target <triple>` supplies architecture, operating system, environment, pointer width, atomics, and other compiler facts.
5. `targets/family-overrides.json` declares external SDK and runner requirements.

## Standing levels

| Result | Meaning |
|---|---|
| `built` | The named product compiled for the named target with the named source digest. |
| `built-nightly-build-std` | Core compiled using nightly and `-Z build-std=core`. |
| `tested` | Tests executed on the named native/emulated runner. |
| `not-applicable` | The target is core-only and requires a target-specific hardware harness. |
| `runtime-unavailable` | Build can exist, but no approved runner was present. |
| `sdk-missing` | The Rust target exists; the required linker, sysroot, or proprietary SDK is absent. |
| `target-unavailable` | The active toolchain cannot supply or construct the target's `core`/`std`. |
| `compiler-refused` | The compiler rejected the source for a reason not classified as missing infrastructure. |
| `timed-out` | The bounded lifecycle station exceeded its time budget. |

A build-only receipt must never be presented as a runtime-test receipt.
